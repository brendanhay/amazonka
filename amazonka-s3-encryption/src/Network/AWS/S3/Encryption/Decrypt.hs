{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- |
-- Module      : Network.AWS.S3.Encryption.Decrypt
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Encryption.Decrypt where

import Control.Lens ((%~), (&), (^.))
import Control.Monad.Except (ExceptT (ExceptT))
import qualified Control.Monad.Except as Except
import Control.Monad.Trans.AWS
import Control.Monad.Trans.Resource (ResourceT)
import Data.Coerce (coerce)
import Data.Proxy (Proxy (Proxy))
import Network.AWS.Prelude
import qualified Network.AWS.S3 as S3
import Network.AWS.S3.Encryption.Envelope
import Network.AWS.S3.Encryption.Instructions
import Network.AWS.S3.Encryption.Types
import qualified Network.AWS.S3.Lens as S3

decrypted :: S3.GetObject -> (Decrypt S3.GetObject, GetInstructions)
decrypted x = (Decrypt x, getInstructions x)

newtype Decrypt a = Decrypt a

newtype Decrypted a = Decrypted
  { runDecrypted ::
      Env ->
      Key ->
      Maybe Envelope ->
      ResourceT IO (Either EncryptionError a)
  }

instance AWSRequest (Decrypt S3.GetObject) where
  type Rs (Decrypt S3.GetObject) = Decrypted S3.GetObjectResponse

  request (Decrypt x) = coerce (request x)

  response l s p r = do
    (status, rs) <- response l s (proxy p) r

    pure
      ( status,
        Decrypted $ \env key m ->
          Except.runExceptT $ do
            encrypted <-
              case m of
                Nothing -> ExceptT (fromMetadata env key (rs ^. S3.getObjectResponse_metadata))
                Just x -> pure x

            pure (rs & S3.getObjectResponse_body %~ bodyDecrypt encrypted)
      )

proxy :: forall a. Proxy (Decrypt a) -> Proxy a
proxy = const Proxy
