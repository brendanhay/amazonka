{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- |
-- Module      : Network.AWS.S3.Encryption
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.S3.Encryption where

import           Control.Monad.IO.Class
import           Data.Coerce
import           Data.Conduit
import           Data.Proxy
import           Network.AWS.Prelude    hiding (coerce)
import           Network.AWS.S3         hiding (getObject, putObject)
import qualified Network.AWS.S3         as S3
import           System.IO

putObject :: BucketName
          -> ObjectKey
          -> Encrypted RqBody
          -> Encrypted PutObject
putObject b k (Encrypted e x) = Encrypted e (S3.putObject b k x)

instance AWSRequest (Encrypted PutObject) where
    type Rs (Encrypted PutObject) = PutObjectResponse

    request (Encrypted e x) = coerce (request x)
    -- FIXME: add envelope to headers.

    response l s = const (response l s (Proxy :: Proxy PutObject))
