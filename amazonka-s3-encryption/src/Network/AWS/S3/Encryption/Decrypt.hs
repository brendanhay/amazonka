{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeFamilies      #-}

-- |
-- Module      : Network.AWS.S3.Encryption.Decrypt
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.S3.Encryption.Decrypt where

import           Control.Lens (view, (^.), (&), (%~))
import           Control.Monad.Trans.AWS
import           Data.Coerce
import           Data.Proxy
import           Network.AWS.S3
import           Network.AWS.S3.Encryption.Envelope
import           Network.AWS.S3.Encryption.Instructions
import           Network.AWS.S3.Encryption.Types

decrypted :: GetObject
          -> (Decrypt GetObject, GetInstructions)
decrypted x = (Decrypt x, getInstructions x)

newtype Decrypt   a = Decrypt a
newtype Decrypted a = Decrypted
    (forall m r. (AWSConstraint r m, HasKeyEnv r) => Maybe Envelope -> m a)

instance AWSRequest (Decrypt GetObject) where
    type Rs (Decrypt GetObject) = Decrypted GetObjectResponse

    request (Decrypt x) = coerce (request x)

    response l s p r = do
        (n, rs) <- response l s (proxy p) r
        return ( n
               , Decrypted $ \m -> do
                   key <- view envKey
                   env <- view environment
                   e   <- case m of
                       Nothing -> fromMetadata key env (rs ^. gorsMetadata)
                       Just e' -> pure e'
                   return (rs & gorsBody %~ bodyDecrypt e)
               )

proxy :: forall a. Proxy (Decrypt a) -> Proxy a
proxy = const Proxy
