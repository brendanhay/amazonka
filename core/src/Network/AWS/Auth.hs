{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
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

-- | Explicitly specify your Amazon AWS security credentials, or retrieve them
-- from the underlying OS.
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
import           Control.Monad.Except
import qualified Data.Aeson                 as Aeson
import qualified Data.ByteString.Char8      as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import           Data.IORef
import           Data.Monoid
import           Data.Text                  (Text)
import qualified Data.Text                  as Text
import qualified Data.Text.Encoding         as Text
import           Data.Time
import           Network.AWS.Data
import           Network.AWS.EC2.Metadata
import           Network.AWS.Types
import           Network.HTTP.Client
import           System.Environment
import           System.Mem.Weak

-- | Default access key environment variable.
accessKey :: Text -- ^ 'AWS_ACCESS_KEY'
accessKey = "AWS_ACCESS_KEY"

-- | Default secret key environment variable.
secretKey :: Text -- ^ 'AWS_SECRET_KEY'
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
      -- /Note:/ you can achieve the same result purely using 'fromKeys' without
      -- having to use the impure 'getAuth'.
    | FromSession AccessKey SecretKey SecurityToken
      -- ^ A session containing the access key, secret key, and a security token.
      -- /Note:/ you can achieve the same result purely using 'fromSession'
      -- without having to use the impure 'getAuth'.
    | FromProfile Text
      -- ^ An IAM Profile name to lookup from the local EC2 instance-data.
    | FromEnv Text Text
      -- ^ Environment variables to lookup for the access and secret keys.
    | Discover
      -- ^ Attempt to read the default access and secret keys from the environment,
      -- falling back to the first available IAM profile if they are not set.
      --
      -- /Note:/ This attempts to resolve <http://instance-data> rather than directly
      -- retrieving <http://169.254.169.254> for IAM profile information to ensure
      -- the dns lookup terminates promptly if not running on EC2.
      deriving (Eq)

instance ToBuilder Credentials where
    build = \case
        FromKeys    a _   -> "FromKeys "    <> build a <> " ****"
        FromSession a _ _ -> "FromSession " <> build a <> " **** ****"
        FromProfile n     -> "FromProfile " <> build n
        FromEnv     a s   -> "FromEnv "     <> build a <> " " <> build s
        Discover          -> "Discover"

instance Show Credentials where
    show = LBS.unpack . buildBS

-- | Retrieve authentication information using the specified 'Credentials' style.
getAuth :: (Functor m, MonadIO m)
        => Manager
        -> Credentials
        -> ExceptT String m Auth
getAuth m = \case
    FromKeys    a s   -> return (fromKeys a s)
    FromSession a s t -> return (fromSession a s t)
    FromProfile n     -> show `withExceptT` fromProfileName m n
    FromEnv     a s   -> fromEnvVars a s
    Discover          -> fromEnv `catchError` const (iam `catchError` const err)
      where
        iam = show `withExceptT` fromProfile m
        err = throwError "Unable to read environment variables or IAM profile."

-- | Retrieve access and secret keys from the default environment variables.
--
-- /See:/ 'accessKey' and 'secretKey'
fromEnv :: (Functor m, MonadIO m) => ExceptT String m Auth
fromEnv = fromEnvVars accessKey secretKey

-- | Retrieve access and secret keys from specific environment variables.
fromEnvVars :: (Functor m, MonadIO m) => Text -> Text -> ExceptT String m Auth
fromEnvVars a s = fmap Auth $ AuthEnv
    <$> (AccessKey <$> key a)
    <*> (SecretKey <$> key s)
    <*> pure Nothing
    <*> pure Nothing
  where
    key (Text.unpack -> k) = ExceptT $ do
        m <- liftIO (lookupEnv k)
        return $
            maybe (Left $ "Unable to read ENV variable: " ++ k)
                  (Right . BS.pack)
                  m

-- | Retrieve the default IAM Profile from the local EC2 instance-data.
--
-- This determined by Amazon as the first IAM profile found in the response from:
-- @http://169.254.169.254/latest/meta-data/iam/security-credentials/@
fromProfile :: MonadIO m => Manager -> ExceptT HttpException m Auth
fromProfile m = do
    !ls <- BS.lines `liftM` metadata m (IAM $ SecurityCredentials Nothing)
    case ls of
        (x:_) -> fromProfileName m (Text.decodeUtf8 x)
        _     -> throwError $
           HttpParserException "Unable to get default IAM Profile from EC2 metadata"

-- | Lookup a specific IAM Profile by name from the local EC2 instance-data.
--
-- The resulting IONewRef wrapper + timer is designed so that multiple concurrent
-- accesses of 'AuthEnv' from the 'AWS' environment are not required to calculate
-- expiry and sequentially queue to update it.
--
-- The forked timer ensures a singular owner and pre-emptive refresh of the
-- temporary session credentials.
--
-- A weak reference is used to ensure that the forked thread will eventually
-- terminate when 'Auth' is no longer referenced.
fromProfileName :: MonadIO m
                => Manager
                -> Text
                -> ExceptT HttpException m Auth
fromProfileName m name = auth >>= start
  where
    auth :: MonadIO m => ExceptT HttpException m AuthEnv
    auth = do
        !lbs <- LBS.fromStrict `liftM` metadata m
            (IAM . SecurityCredentials $ Just name)
        either (throwError . HttpParserException)
               return
               (Aeson.eitherDecode lbs)

    start !a = ExceptT . liftM Right . liftIO $
        case _authExpiry a of
            Nothing -> return (Auth a)
            Just x  -> do
                r <- newIORef a
                p <- myThreadId
                s <- timer r p x
                return (Ref s r)

    timer r p x = forkIO $ do
        s <- myThreadId
        w <- mkWeakIORef r (killThread s)
        loop w p x

    loop w p x = do
        diff x <$> getCurrentTime >>= threadDelay
        ea <- runExceptT auth
        case ea of
            Left   e -> throwTo p e
            Right !a -> do
                 mr <- deRefWeak w
                 case mr of
                     Nothing -> return ()
                     Just  r -> do
                         atomicWriteIORef r a
                         maybe (return ()) (loop w p) (_authExpiry a)

    diff x y = (* 1000000) $
        let n = truncate (diffUTCTime x y) - 60
         in if n > 0 then n else 1
