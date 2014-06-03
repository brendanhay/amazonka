{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE ViewPatterns      #-}

-- Module      : Network.AWS.Auth
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.Auth
    (
    -- * Loading AWS credentials
      Credentials (..)
    , getAuth

    -- * Defaults
    , accessKey
    , secretKey
    ) where

import           Control.Applicative
import           Control.Error
import           Control.Monad
import           Control.Monad.IO.Class
import qualified Data.Aeson                 as Aeson
import           Data.ByteString            (ByteString)
import qualified Data.ByteString.Char8      as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import           Data.Monoid
import           Data.String
import           Network.AWS.Data
import           Network.AWS.EC2.Metadata
import           Network.AWS.Error
import           Network.AWS.Types
import           System.Environment

-- | Default access key environment variable.
accessKey :: ByteString -- ^ 'AWS_ACCESS_KEY'
accessKey = "AWS_ACCESS_KEY"

-- | Default secret key environment variable.
secretKey :: ByteString -- ^ 'AWS_SECRET_KEY'
secretKey = "AWS_SECRET_KEY"

-- | Determines how authentication information is specified or retrieved.
data Credentials
    = FromKeys AccessKey SecretKey
      -- ^ Explicit access and secret keys.
      --
      -- Note: you can achieve the same result purely by using 'fromKeys'.
    | FromSession AccessKey SecretKey SecurityToken
      -- ^ A session containing the access key, secret key, and a security token.
      --
      -- Note: you can achieve the same result purely by using 'fromSession'.
    | FromProfile ByteString
      -- ^ An IAM Profile name to lookup from the local EC2 instance-data.
    | FromEnv ByteString ByteString
      -- ^ Environment variables to lookup for the access and secret keys.
    | Discover
      -- ^ Attempt to read the default access and secret keys from the environment,
      -- falling back to the first available IAM profile if they are not set.
      --
      -- Note: This attempts to resolve <http://instance-data> rather than directly
      -- retrieving <http://169.254.169.254> for IAM profile information to ensure
      -- the dns lookup terminates promptly if not running on EC2.
      deriving (Eq)

instance ToByteString Credentials where
    toBS (FromKeys    a _)   = "FromKeys "    <> toBS a <> " ****"
    toBS (FromSession a _ _) = "FromSession " <> toBS a <> " **** ****"
    toBS (FromProfile n)     = "FromProfile " <> n
    toBS (FromEnv   a s)     = "FromEnv "     <> a <> " " <> s
    toBS Discover            = "Credentials"

instance Show Credentials where
    show = showBS

getAuth :: MonadIO m => Credentials -> EitherT Error m Auth
getAuth c = case c of
    FromKeys a s      -> return $ fromKeys a s
    FromSession a s t -> return $ fromSession a s t
    FromProfile n     -> fromProfile n
    FromEnv   a s     -> env a s
    Discover          -> env accessKey secretKey `catchT` const defaultProfile
  where
    fromProfile name = do
        !m  <- LBS.fromStrict `liftM` meta (creds $ Just name)
        hoistEither . fmapL fromString $ Aeson.eitherDecode m

    defaultProfile = do
        !ls <- BS.lines <$> meta (creds Nothing)
        !n  <- tryHead "Unable to get default IAM Profile from metadata" ls
        fromProfile n

    creds = IAM . SecurityCredentials

    env a s = Auth
        <$> (AccessKey <$> key a)
        <*> (SecretKey <$> key s)
        <*> pure Nothing
        <*> pure Nothing

    key (BS.unpack -> k) = do
        m <- liftIO $ lookupEnv k
        maybe (throwT . Error $ "Unable to read ENV variable: " ++ k)
              (return . BS.pack)
              m

fromKeys :: AccessKey -> SecretKey -> Auth
fromKeys a s = Auth a s Nothing Nothing

fromSession :: AccessKey -> SecretKey -> SecurityToken -> Auth
fromSession a s t = Auth a s (Just t) Nothing
