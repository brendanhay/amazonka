{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module      : Example.SQS
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
module Example.SQS where

import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Data.Generics.Product
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Network.AWS
import Network.AWS.SQS
import System.IO

roundTrip ::
  -- | Region to operate in.
  Region ->
  -- | Name of the queue to create.
  Text ->
  -- | Contents of the messages to send.
  [Text] ->
  IO ()
roundTrip r name xs = do
  lgr <- newLogger Debug stdout
  env <- newEnv Discover <&> set (field @"envLogger") lgr . within r

  let say = liftIO . Text.putStrLn

  runResourceT $ do
    void $ send env (newCreateQueue name)
    url <- view (field @"queueUrl") <$> send env (newGetQueueUrl name)
    say $ "Received Queue URL: " <> url

    forM_ xs $ \x -> do
      void $ send env (newSendMessage url x)
      say $ "Sent '" <> x <> "' to Queue URL: " <> url

    ms <- send env (newReceiveMessage url & field @"waitTimeSeconds" ?~ 20)
    forM_ (ms ^. field @"messages") $ \m ->
      say $ "Received Message: " <> Text.pack (show m)
