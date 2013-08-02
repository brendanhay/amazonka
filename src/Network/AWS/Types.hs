{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}

-- |
-- Module      : Network.AWS.Types
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.Types where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.ByteString        (ByteString)
import Data.Data
import Data.Map               (Map)
import Network.Http.Client
import System.IO.Streams      (InputStream)

type Endpoint  = ByteString
type Action    = ByteString
type Path      = ByteString
type Version   = ByteString

data Credentials = Credentials
    { accessKey :: ByteString
    , secretKey :: ByteString
    } deriving (Show)

newtype AWS a = AWS { unWrap :: ReaderT Credentials IO a }
    deriving (Functor, Applicative, Monad, MonadIO, MonadPlus, MonadReader Credentials)

data SigningVersion
    = Version2
    | Version3
      deriving (Show)

data RawRequest a where
    RawRequest :: AWSTemplate a
               => { rqMethod  :: !Method
                 , rqHost    :: !ByteString
                 , rqAction  :: !(Maybe ByteString)
                 , rqPath    :: !ByteString
                 , rqHeaders :: !(Map ByteString [ByteString])
                 , rqQuery   :: ![(ByteString, ByteString)]
                 , rqBody    :: !a
                 }
               -> RawRequest a

deriving instance Show a => Show (RawRequest a)

data SignedRequest = SignedRequest
    { rqUrl     :: !ByteString
    , rqRequest :: !Request
    , rqStream  :: !(InputStream ByteString)
    }

class (Data a, Typeable a) => AWSTemplate a where
    template :: a -> ByteString

class AWSRequest a where
    request :: a -> AWS SignedRequest
