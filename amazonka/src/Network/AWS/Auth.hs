{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

-- |
-- Module      : Network.AWS.Auth
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Explicitly specify your Amazon AWS security credentials, or retrieve them
-- from the underlying OS.
module Network.AWS.Auth
    (
    -- * Authentication
    -- ** Keys
      AccessKey     (..)
    , SecretKey     (..)
    , SecurityToken (..)
    , accessKey
    , secretKey

    -- ** Retrieving authentication
    , Credentials   (..)
    , Auth
    , getAuth

    -- $credentials

    , fromKeys
    , fromSession
    , fromEnv
    , fromEnvKeys
    , fromProfile
    , fromProfileName

    -- ** Handling Errors
    , AsAuthError (..)
    , AuthError   (..)
    ) where

import           Control.Applicative
import           Control.Concurrent
import           Control.Exception.Lens
import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import qualified Data.ByteString.Char8      as BS8
import qualified Data.ByteString.Lazy.Char8 as LBS8
import           Data.IORef
import           Data.Monoid
import qualified Data.Text                  as Text
import qualified Data.Text.Encoding         as Text
import           Data.Time                  (diffUTCTime, getCurrentTime)
import           Network.AWS.EC2.Metadata
import           Network.AWS.Logger
import           Network.AWS.Prelude
import           Network.AWS.Types
import           Network.HTTP.Conduit
import           System.Environment
import           System.Mem.Weak

-- | Default access key environment variable.
accessKey :: Text -- ^ 'AWS_ACCESS_KEY'
accessKey = "AWS_ACCESS_KEY"

-- | Default secret key environment variable.
secretKey :: Text -- ^ 'AWS_SECRET_KEY'
secretKey = "AWS_SECRET_KEY"

{- $credentials
'getAuth' is implemented using the following @from*@-styled functions below.
Both 'fromKeys' and 'fromSession' can be used directly to avoid the 'MonadIO'
constraint.
-}

-- | Explicit access and secret keys.
fromKeys :: AccessKey -> SecretKey -> Auth
fromKeys a s = Auth (AuthEnv a s Nothing Nothing)

-- | A session containing the access key, secret key, and a security token.
fromSession :: AccessKey -> SecretKey -> SecurityToken -> Auth
fromSession a s t = Auth (AuthEnv a s (Just t) Nothing)

-- | Determines how AuthN/AuthZ information is retrieved.
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
      -- Attempts to resolve <http://instance-data> rather than directly
      -- retrieving <http://169.254.169.254> for IAM profile information.
      -- This assists in ensuring the DNS lookup terminates promptly if not
      -- running on EC2.
      deriving (Eq)

instance ToLog Credentials where
    message = \case
        FromKeys    a _   -> "FromKeys "    <> message a <> " ****"
        FromSession a _ _ -> "FromSession " <> message a <> " **** ****"
        FromProfile n     -> "FromProfile " <> message n
        FromEnv     a s   -> "FromEnv "     <> message a <> " " <> message s
        Discover          -> "Discover"

instance Show Credentials where
    show = BS8.unpack . toBS . message

-- | An error thrown when attempting to read AuthN/AuthZ information.
data AuthError
    = RetrievalError  HttpException
    | MissingEnvError Text
    | InvalidIAMError Text
      deriving (Show, Typeable)

instance Exception AuthError

instance ToLog AuthError where
    message = \case
        RetrievalError  e -> message e
        MissingEnvError e -> "[MissingEnvError] { message = " <> message e <> "}"
        InvalidIAMError e -> "[InvalidIAMError] { message = " <> message e <> "}"

class AsAuthError a where
    _AuthError       :: Prism' a AuthError
    {-# MINIMAL _AuthError #-}

    _RetrievalError  :: Prism' a HttpException
    _MissingEnvError :: Prism' a Text
    _InvalidIAMError :: Prism' a Text

    _RetrievalError  = _AuthError . _RetrievalError
    _MissingEnvError = _AuthError . _MissingEnvError
    _InvalidIAMError = _AuthError . _InvalidIAMError

instance AsAuthError SomeException where
    _AuthError = exception

instance AsAuthError AuthError where
    _AuthError = id

    _RetrievalError = prism RetrievalError $ \case
        RetrievalError e -> Right e
        x                -> Left x

    _MissingEnvError = prism MissingEnvError $ \case
        MissingEnvError e -> Right e
        x               -> Left x

    _InvalidIAMError = prism InvalidIAMError $ \case
        InvalidIAMError e -> Right e
        x               -> Left x

-- | Retrieve authentication information via the specified 'Credentials' mechanism.
--
-- Throws 'AuthError' when environment variables or IAM profiles cannot be read.
getAuth :: (Applicative m, MonadIO m, MonadCatch m)
        => Manager
        -> Credentials
        -> m Auth
getAuth m = \case
    FromKeys    a s   -> return (fromKeys a s)
    FromSession a s t -> return (fromSession a s t)
    FromProfile n     -> fromProfileName m n
    FromEnv     a s   -> fromEnvKeys a s
    Discover          ->
        catching _MissingEnvError fromEnv $ \e -> do
            p <- isEC2 m
            unless p $ throwingM _MissingEnvError e
            fromProfile m

-- | Retrieve access and secret keys from the default environment variables.
--
-- Throws 'MissingEnvError' if either of the default environment variables
-- cannot be read.
--
-- /See:/ 'accessKey' and 'secretKey'
fromEnv :: (Applicative m, MonadIO m, MonadThrow m) => m Auth
fromEnv = fromEnvKeys accessKey secretKey

-- | Retrieve 'Access' and 'Secret' keys from specific environment variables.
--
-- Throws 'MissingEnvError' if either of the specified environment variables
-- cannot be read.
fromEnvKeys :: (Applicative m, MonadIO m, MonadThrow m) => Text -> Text -> m Auth
fromEnvKeys a s = fmap Auth $ AuthEnv
    <$> (AccessKey <$> key a)
    <*> (SecretKey <$> key s)
    <*> pure Nothing
    <*> pure Nothing
  where
    key k = do
        m <- liftIO $ lookupEnv (Text.unpack k)
        maybe (throwM . MissingEnvError $ "Unable to read ENV variable: " <> k)
              (return . BS8.pack)
              m

-- | Retrieve the default IAM Profile from the local EC2 instance-data.
--
-- The default IAM profile is determined by Amazon as the first profile found
-- in the response from:
-- @http://169.254.169.254/latest/meta-data/iam/security-credentials/@
--
-- Throws 'RetrievalError' if the HTTP call fails, or 'InvalidIAMError' if
-- the default IAM profile cannot be read.
fromProfile :: (MonadIO m, MonadCatch m) => Manager -> m Auth
fromProfile m = do
    ls <- try $ metadata m (IAM (SecurityCredentials Nothing))
    case BS8.lines `liftM` ls of
        Right (x:_) -> fromProfileName m (Text.decodeUtf8 x)
        Left  e     -> throwM (RetrievalError e)
        _           -> throwM $
            InvalidIAMError "Unable to get default IAM Profile from EC2 metadata"

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
--
-- Throws 'RetrievalError' if the HTTP call fails, or 'InvalidIAMError' if
-- the specified IAM profile cannot be read.
fromProfileName :: (MonadIO m, MonadCatch m) => Manager -> Text -> m Auth
fromProfileName m name = auth >>= start
  where
    auth :: (MonadIO m, MonadCatch m) => m AuthEnv
    auth = do
        bs <- try $ metadata m (IAM . SecurityCredentials $ Just name)
        case bs of
            Left  e -> throwM (RetrievalError e)
            Right x ->
                either (throwM . invalidErr)
                       return
                       (eitherDecode' (LBS8.fromStrict x))

    invalidErr = InvalidIAMError
        . mappend ("Error parsing IAM profile '" <> name <> "' ")
        . Text.pack

    start :: MonadIO m => AuthEnv -> m Auth
    start !a = liftIO $
        case _authExpiry a of
            Nothing -> return (Auth a)
            Just x  -> do
                r <- newIORef a
                p <- myThreadId
                s <- timer r p x
                return (Ref s r)

    timer :: IORef AuthEnv -> ThreadId -> UTCTime -> IO ThreadId
    timer !r !p !x = forkIO $ do
        s <- myThreadId
        w <- mkWeakIORef r (killThread s)
        loop w p x

    loop :: Weak (IORef AuthEnv) -> ThreadId -> UTCTime -> IO ()
    loop w !p !x = do
        diff x <$> getCurrentTime >>= threadDelay
        ea <- try auth
        case ea of
            Left   e -> throwTo p (RetrievalError e)
            Right !a -> do
                 mr <- deRefWeak w
                 case mr of
                     Nothing -> return ()
                     Just  r -> do
                         atomicWriteIORef r a
                         maybe (return ()) (loop w p) (_authExpiry a)

    diff !x !y = (* 1000000) $ if n > 0 then n else 1
      where
        !n = truncate (diffUTCTime x y) - 60
