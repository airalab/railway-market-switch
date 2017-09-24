{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Main where

import           Network.Ethereum.Web3
import           Network.Ethereum.Web3.TH

import           Control.Concurrent
import           Control.Concurrent.Chan
import           Control.Logging           (log', warn, withStderrLogging)
import           Control.Monad
import           Control.Monad.IO.Class    (liftIO)
import           Control.Monad.Trans.Class (lift)
import           Data.Monoid               ((<>))
import           Data.Queue
import           Data.String               (fromString)
import           Data.Text                 (pack)
import           Network.Miku
import           Network.Wai.Handler.Warp  (run)
import           System.Environment        (getEnv)
import qualified System.Hardware.Z21       as Z

[abiFrom|abi/Market.json|]

z21 = ("192.168.0.111", 21105)

show' :: Show a => a -> Text
show' = pack . show

data Parity
instance Provider Parity where
    rpcUri = return "http://localhost:8545"

type Markets = Either Integer Integer
type Railway = Either () ()

marketMonitor :: Address
              -> Address
              -> Web3 Parity (Chan Markets)
marketMonitor ml mr = do
    c <- liftIO newChan

    log' "Web3 connected"
    log' . ("Market LEFT: " <>) =<< name ml
    log' . ("Market RIGHT: " <>) =<< name mr

    event ml $ \(OrderClosed o _) -> do
        price <- lift $ priceOf ml o
        log' $ "Order on market LEFT closed, price = " <> show' price
        liftIO $ writeChan c (Left price)
        return ContinueEvent

    event mr $ \(OrderClosed o _) -> do
        price <- lift $ priceOf mr o
        log' $ "Order on market RIGHT closed, price = " <> show' price
        liftIO $ writeChan c (Right price)
        return ContinueEvent

    event ml $ \(OrderPartial o _) -> do
        price <- lift $ priceOf ml o
        log' $ "Order on market LEFT partial, price = " <> show' price
        liftIO $ writeChan c (Left price)
        return ContinueEvent

    event mr $ \(OrderPartial o _) -> do
        price <- lift $ priceOf mr o
        log' $ "Order on market RIGHT partial, price = " <> show' price
        liftIO $ writeChan c (Right price)
        return ContinueEvent

    return c

railwayController :: IO (Chan Railway)
railwayController = do
    forkIO $ forever $ do
        log' "Turn ON locomotive by Z21"
        Z.runZ21 zaddress zport $
            Z.setLocoDrive (Z.Address 0 3) 0x10 0xFA
        threadDelay 3000000000

    cmd <- newChan
    sig <- newFifo

    forkIO $ do
        log' "Start HTTP server for GPIO data"
        run 3000 . miku $ do
            get "/LEFT"  $ liftIO (enqueue (sig :: TChan (Either () ())) (Left ()))  >> text "OK"
            get "/RIGHT" $ liftIO (enqueue sig (Right ())) >> text "OK"

    forkIO $ forever $ do
        c <- readChan cmd
        log' $ "Get CMD: " <> show' c
        q <- dequeueAll sig
        s <- dequeue' sig
        log' $ "Get signal: " <> show' s

        when (s /= c) $ do
            Z.runZ21 zaddress zport $
                Z.setTurnout (Z.Address 0 0) 169

            threadDelay 5000000

            Z.runZ21 zaddress zport $
                Z.setTurnout (Z.Address 0 0) 168

    return cmd

  where (zaddress, zport) = z21
        dequeue' s = do x <- dequeue s
                        case x of
                            Nothing -> dequeue' s
                            Just y  -> return y
        dequeueAll s = do x <- dequeue s
                          case x of
                            Nothing -> return ()
                            Just y  -> dequeueAll s

controller :: Chan (Either Integer Integer)
           -> Chan (Either () ())
           -> (Integer, Integer)
           -> IO ()
controller cmd railcmd (l, r) = readChan cmd >>= switch >>= controller cmd railcmd
  where switch (Right price)
            | price > l = writeChan railcmd (Right ()) >> return (l, price)
            | otherwise = writeChan railcmd (Left ())  >> return (l, price)

        switch (Left price)
            | price > r = writeChan railcmd (Left ())  >> return (price, r)
            | otherwise = writeChan railcmd (Right ()) >> return (price, r)

main :: IO ()
main = withStderrLogging $ do
    ml <- fromString <$> getEnv "MARKET_LEFT_ADDRESS"
    mr <- fromString <$> getEnv "MARKET_RIGHT_ADDRESS"
    market <- runWeb3' (marketMonitor ml mr)
    railway <- railwayController
    case market of
        Left e  -> warn $ "Unable to start market monitor: " <> show' e
        Right m -> controller m railway (0, 0)
