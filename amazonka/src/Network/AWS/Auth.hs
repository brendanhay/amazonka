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

-- | Specify or retrieve your Amazon AWS credentials.
module Network.AWS.Auth
    (
    -- * Defaults
      accessKey
    , secretKey

    -- * Specifying credentials
    , fromKeys
    , fromSession

    -- * Retrieving credentials
    , Credentials (..)
    , getAuth
    ) where

import           Control.Applicative
import           Control.Concurrent
import           Control.Error
import           Control.Monad
import           Control.Monad.IO.Class
import qualified Data.Aeson                 as Aeson
import           Data.ByteString            (ByteString)
import qualified Data.ByteString.Char8      as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import           Data.IORef
import           Data.Monoid
import           Data.String
import           Data.Time
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

-- | Explicit access and secret keys.
fromKeys :: AccessKey -> SecretKey -> Auth
fromKeys a s = Auth (AuthEnv a s Nothing Nothing)

-- | A session containing the access key, secret key, and a security token.
fromSession :: AccessKey -> SecretKey -> SecurityToken -> Auth
fromSession a s t = Auth (AuthEnv a s (Just t) Nothing)

-- | Determines how authentication information is retrieved.
data Credentials
    = FromKeys AccessKey SecretKey
      -- ^ Explicit access and secret keys.
      -- Note: you can achieve the same result purely by using 'fromKeys'.
    | FromSession AccessKey SecretKey SecurityToken
      -- ^ A session containing the access key, secret key, and a security token.
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
    toBS Discover            = "Discover"

instance Show Credentials where
    show = showBS

-- | Retrieve authentication information from the environment or instance-data.
getAuth :: MonadIO m => Credentials -> EitherT Error m Auth
getAuth c = case c of
    FromKeys a s      -> return $ fromKeys a s
    FromSession a s t -> return $ fromSession a s t
    FromProfile n     -> fromProfile' n
    FromEnv   a s     -> fromEnv' a s
    Discover          -> fromEnv `catchT` const fromProfile

-- | Retrieve access and secret keys from the default environment variables.
--
-- See: 'accessKey' and 'secretKey'
fromEnv :: MonadIO m => EitherT Error m Auth
fromEnv = fromEnv' accessKey secretKey

-- | Retrieve access and secret keys from specific environment variables.
fromEnv' :: MonadIO m => ByteString -> ByteString -> EitherT Error m Auth
fromEnv' a s = fmap Auth $ AuthEnv
    <$> (AccessKey <$> key a)
    <*> (SecretKey <$> key s)
    <*> pure Nothing
    <*> pure Nothing
  where
    key (BS.unpack -> k) = do
        m <- liftIO $ lookupEnv k
        maybe (throwT . fromString $ "Unable to read ENV variable: " ++ k)
              (return . BS.pack)
              m

-- | Retrieve the default IAM Profile from the local EC2 instance-data.
--
-- This determined by Amazon as the first IAM profile found in the response from:
-- @http://169.254.169.254/latest/meta-data/iam/security-credentials/@
fromProfile :: MonadIO m => EitherT Error m Auth
fromProfile = do
    !ls <- BS.lines <$> meta (IAM $ SecurityCredentials Nothing)
    !n  <- tryHead "Unable to get default IAM Profile from EC2 metadata" ls
    fromProfile' n

-- | Lookup a specific IAM Profile by name from the local EC2 instance-data.
--
-- The resulting IONewRef wrapper + timer is designed so that multiple concurrent
-- accesses of 'AuthEnv' from the 'AWS' environment are not required to calculate
-- expiry and sequentially queue to update it.
--
-- The forked timer ensures a singular owner and pre-emptive refresh of the
-- temporary session credentials.
fromProfile' :: MonadIO m => ByteString -> EitherT Error m Auth
fromProfile' name = auth >>= runIO . start
  where
    auth :: MonadIO m => EitherT Error m AuthEnv
    auth = do
        !m <- LBS.fromStrict `liftM` meta (IAM . SecurityCredentials $ Just name)
        hoistEither . fmapL fromString $ Aeson.eitherDecode m

    start :: AuthEnv -> IO Auth
    start !a = case _authExpiry a of
        Nothing -> return (Auth a)
        Just x  -> do
            r <- newIORef a
            t <- myThreadId
            void . forkIO $ timer r t x
            return (Ref r)

    timer r t x = do
        -- FIXME: guard against a lower expiration than the -60
        n <- truncate . diffUTCTime x <$> getCurrentTime
        threadDelay $ (n - 60) * 1000000
        l <- runEitherT auth
        case l of
            Left   e -> throwTo t e
            Right !a -> do
                 atomicWriteIORef r a
                 maybe (return ())
                       (timer r t)
                       (_authExpiry a)
