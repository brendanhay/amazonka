{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE TypeFamilies      #-}

-- |
-- Module      : Network.AWS.S3.Encryption.Decrypt
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.S3.Encryption.Decrypt where

import           Control.Arrow
import           Control.Monad.Catch
import           Control.Monad.Reader
import           Control.Monad.Trans.AWS
import           Control.Monad.Trans.Resource
import qualified Data.Aeson.Types                       as Aeson
import           Data.Coerce
import           Data.Conduit
import           Data.Proxy
import           Network.AWS.Prelude                    hiding (coerce)
import           Network.AWS.Response
import           Network.AWS.S3
import qualified Network.AWS.S3                         as S3
import           Network.AWS.S3.Encryption.Envelope
import           Network.AWS.S3.Encryption.Instructions
import           Network.AWS.S3.Encryption.Types
import           System.IO

decrypted :: GetObject
          -> (Decrypt GetObject, GetInstructions)
decrypted x = (Decrypt x, getInstructions x)

newtype Decrypt   a = Decrypt a
newtype Decrypted a = Decrypted
    (forall m r. (AWSConstraint r m, HasKeyEnv r) => Maybe Envelope -> m a)

instance AWSRequest (Decrypt GetObject) where
    type Rs (Decrypt GetObject) = Decrypted GetObjectResponse

    request (Decrypt x) = coerce (request x)

    response l s (Decrypt x) r = do
        (n, rs) <- response l s x r
        return ( n
               , Decrypted $ \m -> do
                   key <- view envKey
                   env <- view environment
                   e   <- case m of
                       Nothing -> fromMetadata key env (rs ^. gorsMetadata)
                       Just e' -> pure e'
                   return (rs & gorsBody %~ bodyDecrypt e)
               )
