{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module SQS where

import Amazonka
import Amazonka.SQS
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Data.Foldable (for_)
import Data.Generics.Labels ()
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import System.IO

roundTrip ::
  -- | Region to operate in.
  Region ->
  -- | Name of the queue to create.
  Text ->
  -- | Contents of the messages to send.
  [Text] ->
  IO ()
roundTrip reg queue xs = do
  lgr <- newLogger Debug stdout
  env <- newEnv discover <&> set #logger lgr . set #region reg

  let say = liftIO . Text.putStrLn

  runResourceT $ do
    void $ send env (newCreateQueue queue)
    url <- view #queueUrl <$> send env (newGetQueueUrl queue)
    say $ "Received Queue URL: " <> url

    forM_ xs $ \x -> do
      void $ send env (newSendMessage url x)
      say $ "Sent '" <> x <> "' to Queue URL: " <> url

    ms <- send env (newReceiveMessage url & #waitTimeSeconds ?~ 20)
    for_ (ms ^. #messages) $ \m ->
      say $ "Received Message: " <> Text.pack (show m)
