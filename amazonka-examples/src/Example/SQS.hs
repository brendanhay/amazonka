{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Example.SQS
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
--
module Example.SQS where

import           Control.Lens
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.AWS
import           Data.Monoid
import           Data.Text               (Text)
import qualified Data.Text               as Text
import qualified Data.Text.IO            as Text
import           Network.AWS.SQS
import           System.IO

roundTrip :: Region -- ^ Region to operate in.
          -> Text   -- ^ Name of the queue to create.
          -> [Text] -- ^ Contents of the messages to send.
          -> IO ()
roundTrip r name xs = do
    lgr <- newLogger Debug stdout
    env <- newEnv Discover <&> set envLogger lgr

    let say = liftIO . Text.putStrLn

    runResourceT . runAWST env . within r $ do
        void $ send (createQueue name)
        url <- view gqursQueueURL <$> send (getQueueURL name)
        say  $ "Received Queue URL: " <> url

        forM_ xs $ \x -> do
            void $ send (sendMessage url x)
            say  $ "Sent '" <> x <> "' to Queue URL: " <> url

        ms  <- send (receiveMessage url & rmWaitTimeSeconds ?~ 20)
        forM_ (ms ^. rmrsMessages) $ \m ->
            say $ "Received Message: " <> Text.pack (show m)
