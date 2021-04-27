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

import qualified Control.Lens as Lens
import Control.Monad.Trans.AWS
import Data.Coerce (coerce)
import Data.Proxy (Proxy (Proxy))
import qualified Network.AWS.S3 as S3
import qualified Network.AWS.S3.Lens as S3
import Control.Lens ((&), (%~), (^.))
import Network.AWS.Prelude
import Network.AWS.S3.Encryption.Envelope
import Network.AWS.S3.Encryption.Instructions
import Network.AWS.S3.Encryption.Types

decrypted :: S3.GetObject -> (Decrypt S3.GetObject, GetInstructions)
decrypted x = (Decrypt x, getInstructions x)

newtype Decrypt a = Decrypt a

newtype Decrypted a = Decrypted
  { runDecrypted :: forall m r. (AWSConstraint r m, HasKeyEnv r) => Maybe Envelope -> m a
  }

instance AWSRequest (Decrypt S3.GetObject) where
  type Rs (Decrypt S3.GetObject) = Decrypted S3.GetObjectResponse

  request (Decrypt x) = coerce (request x)

  response l s p r = do
    (n, rs) <- response l s (proxy p) r
    
    pure
      ( n,
        Decrypted $ \m -> do
          key <- Lens.view envKey
          env <- Lens.view environment
          
          enc <-
            case m of
              Nothing -> fromMetadata key env (rs ^. S3.getObjectResponse_metadata)
              Just e' -> pure e'
            
          pure (rs & S3.getObjectResponse_body %~ bodyDecrypt enc)
      )

proxy :: forall a. Proxy (Decrypt a) -> Proxy a
proxy = const Proxy
