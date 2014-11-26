{-# LANGUAGE OverloadedStrings #-}

-- Module      : Examples
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Examples where

import           Control.Applicative
import           Control.Concurrent
import           Control.Exception.Lifted
import           Control.Lens
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.AWS
import           Data.Monoid
import qualified Data.Text                as Text
import qualified Data.Text.IO             as Text
import           Data.Time.Clock.POSIX
import           Network.AWS.SQS
import           System.IO

sendReceive :: IO (Either Error ())
sendReceive = do
    hSetBuffering stdout LineBuffering
    env  <- getEnv Ireland Discover <&> envLogger .~ Debug Text.putStrLn
    name <- Text.pack . show <$> getTimestamp
    runAWST env $ do
        url <- setup name
        roundtrip url `finally` cleanup url
  where
    setup name = do
        -- Create the queue, ignoring the response so
        -- getQueueUrl can be utilised.
        say "Creating " name
        void $ send (createQueue name)

        feebleWait

        -- Get the SQS URL for the queue name.
        say "Retreving URL for " name
        url <- view gqurQueueUrl <$> send (getQueueUrl name)

        say "Found URL " url
        return url

    roundtrip url = do
        -- Send a message to the newly created queue.
        say "Sending 'hello' to " url
        void $ send (sendMessage url "hello")

        feebleWait

        -- Receive the sent message, waiting a maximum of 2 minutes
        -- until it becomes available.
        say "Receiving messages from " url
        ms <- view rmrMessages <$>
            send (receiveMessage url & rmWaitTimeSeconds ?~ 20)

        -- Ack!
        forM_ ms $ \m -> do
            say "Acking " (m ^. mReceiptHandle)
            m ^!? mReceiptHandle
                . _Just
                . act (send . deleteMessage url)

    cleanup url = do
        say "Deleting " url
        void $ send (deleteQueue url)

    -- Wait 5 seconds. Use of finally, catch, or MonadRetry et al.
    -- should be strongly considered in non-example code.
    feebleWait = liftIO $ do
        putStrLn "Waiting 5 seconds..."
        threadDelay 5000000

    say msg = liftIO . Text.putStrLn . mappend msg . Text.pack . show

getTimestamp :: IO Integer
getTimestamp = truncate <$> getPOSIXTime
