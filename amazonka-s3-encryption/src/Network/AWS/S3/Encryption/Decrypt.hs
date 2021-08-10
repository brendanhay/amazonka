-- |
-- Module      : Network.AWS.S3.Encryption.Decrypt
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Encryption.Decrypt where

import Control.Lens ((%~), (^.))
import qualified Control.Monad.Except as Except
import Data.Coerce (coerce)
import qualified Network.AWS as AWS
import Network.AWS.Core
import Network.AWS.Prelude
import qualified Network.AWS.S3 as S3
import Network.AWS.S3.Encryption.Envelope
import Network.AWS.S3.Encryption.Instructions
import Network.AWS.S3.Encryption.Types
import qualified Network.AWS.S3.Lens as S3
import qualified Network.HTTP.Client as Client

decrypted :: S3.GetObject -> (Decrypt S3.GetObject, GetInstructions)
decrypted x = (Decrypt x, getInstructions x)

newtype Decrypt a = Decrypt a

newtype Decrypted a = Decrypted
  { runDecrypted :: forall m. MonadResource m => Key -> AWS.Env -> Maybe Envelope -> m a
  }

instance AWSRequest (Decrypt S3.GetObject) where
  type AWSResponse (Decrypt S3.GetObject) = Decrypted S3.GetObjectResponse

  request (Decrypt x) = coerce (request x)

  response l s p r =
    Except.runExceptT $ do
      rs <- Except.ExceptT (response l s (proxy p) r)

      let body = Client.responseBody rs
          decrypt =
            Decrypted $ \key env m -> do
              encrypted <-
                case m of
                  Nothing -> fromMetadata key env (body ^. S3.getObjectResponse_metadata)
                  Just e -> pure e

              pure (body & S3.getObjectResponse_body %~ bodyDecrypt encrypted)

      pure (decrypt <$ rs)

proxy :: forall a. Proxy (Decrypt a) -> Proxy a
proxy = const Proxy
