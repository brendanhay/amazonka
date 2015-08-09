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
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
--
-- Explicitly specify your Amazon AWS security credentials, or retrieve them
-- from the underlying OS.
--
-- The format of environment variables and the credentials file follows the official
-- <http://blogs.aws.amazon.com/security/post/Tx3D6U6WSFGOK2H/A-New-and-Standardized-Way-to-Manage-Credentials-in-the-AWS-SDKs AWS SDK guidelines>.
module Network.AWS.Auth
    (
    -- * Authentication
    -- ** Retrieving authentication
      getAuth
    , Credentials   (..)
    , Auth

    -- ** Defaults
    -- *** Environment
    , envAccessKey
    , envSecretKey
    , envSessionToken

    -- *** Credentials File
    , credAccessKey
    , credSecretKey
    , credSessionToken
    , credFile

    -- $credentials

    , fromKeys
    , fromSession
    , fromEnv
    , fromEnvKeys
    , fromFile
    , fromFilePath
    , fromProfile
    , fromProfileName

    -- ** Keys
    , AccessKey    (..)
    , SecretKey    (..)
    , SessionToken (..)

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
import qualified Data.Ini                   as INI
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
import           System.Directory           (doesFileExist, getHomeDirectory)
import           System.Environment
import           System.Mem.Weak

import           Prelude

-- | Default access key environment variable.
envAccessKey :: Text -- ^ 'AWS_ACCESS_KEY_ID'
envAccessKey = "AWS_ACCESS_KEY_ID"

-- | Default secret key environment variable.
envSecretKey :: Text -- ^ 'AWS_SECRET_ACCESS_KEY'
envSecretKey = "AWS_SECRET_ACCESS_KEY"

-- | Default session token environment variable.
envSessionToken :: Text -- ^ 'AWS_SESSION_TOKEN'
envSessionToken = "AWS_SESSION_TOKEN"

-- | Credentials file access key INI-key name.
credAccessKey :: Text -- ^ 'aws_access_key_id'
credAccessKey = "aws_access_key_id"

-- | Credentials file secret key INI-key name.
credSecretKey :: Text -- ^ 'aws_secret_access_key'
credSecretKey = "aws_secret_access_key"

-- | Credentials file session token INI-key name.
credSessionToken :: Text -- ^ 'aws_session_token'
credSessionToken = "aws_session_token"

-- | Credentials default profile INI-section name.
credProfile :: Text -- ^ 'default'
credProfile = "default"

-- | Default path for the credentials file. This looks in in the @HOME@ directory
-- as determined by the <http://hackage.haskell.org/package/directory directory>
-- library.
--
-- * UNIX/OSX: @$HOME/.aws/credentials@
--
-- * Windows: @C:\/Users\//\<user\>\.aws\credentials@
--
-- /Note:/ This does not match the default AWS SDK location of
-- @%USERPROFILE%\.aws\credentials@ on Windows. (Sorry.)
credFile :: MonadIO m => m FilePath
credFile = (<> "/.aws/credentials") <$> liftIO getHomeDirectory
-- TODO: probably should be using System.FilePath above.

{- $credentials
'getAuth' is implemented using the following @from*@-styled functions below.
Both 'fromKeys' and 'fromSession' can be used directly to avoid the 'MonadIO'
constraint.
-}

-- | Explicit access and secret keys.
fromKeys :: AccessKey -> SecretKey -> Auth
fromKeys a s = Auth (AuthEnv a s Nothing Nothing)

-- | A session containing the access key, secret key, and a session token.
fromSession :: AccessKey -> SecretKey -> SessionToken -> Auth
fromSession a s t = Auth (AuthEnv a s (Just t) Nothing)

-- | Determines how AuthN/AuthZ information is retrieved.
data Credentials
    = FromKeys AccessKey SecretKey
      -- ^ Explicit access and secret keys.
      -- /Note:/ you can achieve the same result purely using 'fromKeys' without
      -- having to use the impure 'getAuth'.
    | FromSession AccessKey SecretKey SessionToken
      -- ^ A session containing the access key, secret key, and a session token.
      -- /Note:/ you can achieve the same result purely using 'fromSession'
      -- without having to use the impure 'getAuth'.
    | FromProfile Text
      -- ^ An IAM Profile name to lookup from the local EC2 instance-data.
    | FromEnv Text Text Text
      -- ^ Environment variables to lookup for the access key, secret key and
      -- optional session token.
    | FromFile Text FilePath
      -- ^ A credentials profile name and the path to the AWS
      -- <http://blogs.aws.amazon.com/security/post/Tx3D6U6WSFGOK2H/A-New-and-Standardized-Way-to-Manage-Credentials-in-the-AWS-SDKs credentials> file.
    | Discover
      -- ^ Attempt to credentials discovery via the following steps:
      --
      -- * Read the 'accessKey' and 'secretKey' from the environment.
      --
      -- * Read the credentials file from 'credFile'.
      --
      -- * Retrieve the first available IAM profile if running on EC2.
      --
      -- An attempt is made to resolve <http://instance-data> rather than directly
      -- retrieving <http://169.254.169.254> for IAM profile information.
      -- This assists in ensuring the DNS lookup terminates promptly if not
      -- running on EC2.
      deriving (Eq)

instance ToLog Credentials where
    message = \case
        FromKeys    a _   -> "FromKeys "    <> message a <> " ****"
        FromSession a _ _ -> "FromSession " <> message a <> " **** ****"
        FromProfile n     -> "FromProfile " <> message n
        FromEnv     a s t -> "FromEnv "     <> message a <> " " <> message s <> " " <> message t
        FromFile    n f   -> "FromFile "    <> message n <> " " <> message f
        Discover          -> "Discover"

instance Show Credentials where
    show = BS8.unpack . toBS . message

-- | An error thrown when attempting to read AuthN/AuthZ information.
data AuthError
    = RetrievalError   HttpException
    | MissingEnvError  Text
    | MissingFileError FilePath
    | InvalidFileError Text
    | InvalidIAMError  Text
      deriving (Show, Typeable)

instance Exception AuthError

instance ToLog AuthError where
    message = \case
        RetrievalError   e -> message e
        MissingEnvError  e -> "[MissingEnvError]  { message = " <> message e <> "}"
        MissingFileError f -> "[MissingFileError] { path = "    <> message f <> "}"
        InvalidFileError e -> "[InvalidFileError] { message = " <> message e <> "}"
        InvalidIAMError  e -> "[InvalidIAMError]  { message = " <> message e <> "}"

class AsAuthError a where
    -- | A general authentication error.
    _AuthError        :: Prism' a AuthError
    {-# MINIMAL _AuthError #-}

    -- | An error occured while communicating over HTTP with
    -- the local metadata endpoint.
    _RetrievalError   :: Prism' a HttpException

    -- | An error occured looking up a named environment variable.
    _MissingEnvError  :: Prism' a Text

    -- | The specified credentials file could not be found.
    _MissingFileError :: Prism' a FilePath

    -- | An error occured parsing the credentials file.
    _InvalidFileError :: Prism' a Text

    -- | The specified IAM profile could not be found or deserialised.
    _InvalidIAMError  :: Prism' a Text

    _RetrievalError   = _AuthError . _RetrievalError
    _MissingEnvError  = _AuthError . _MissingEnvError
    _MissingFileError = _AuthError . _MissingFileError
    _InvalidFileError = _AuthError . _InvalidFileError
    _InvalidIAMError  = _AuthError . _InvalidIAMError

instance AsAuthError SomeException where
    _AuthError = exception

instance AsAuthError AuthError where
    _AuthError = id

    _RetrievalError = prism RetrievalError $ \case
        RetrievalError   e -> Right e
        x                  -> Left  x

    _MissingEnvError = prism MissingEnvError $ \case
        MissingEnvError  e -> Right e
        x                  -> Left  x

    _MissingFileError = prism MissingFileError $ \case
        MissingFileError f -> Right f
        x                  -> Left  x

    _InvalidFileError = prism InvalidFileError $ \case
        InvalidFileError e -> Right e
        x                  -> Left  x

    _InvalidIAMError = prism InvalidIAMError $ \case
        InvalidIAMError  e -> Right e
        x                  -> Left  x

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
    FromEnv     a s t -> fromEnvKeys a s t
    FromFile    n f   -> fromFilePath n f
    Discover          ->
        -- Don't try and catch InvalidFileError, or InvalidIAMProfile,
        -- let both errors propagate.
        catching_ _MissingEnvError fromEnv $ do
            -- proceed, missing env keys
            catching _MissingFileError fromFile $ \f -> do
                -- proceed, missing credentials file
                p <- isEC2 m
                unless p $ throwingM _MissingFileError f
                fromProfile m

-- | Retrieve access and secret keys from the default environment variables.
--
-- Throws 'MissingEnvError' if either of the default environment variables
-- cannot be read.
--
-- /See:/ 'accessKey' and 'secretKey'
fromEnv :: (Applicative m, MonadIO m, MonadThrow m) => m Auth
fromEnv = fromEnvKeys envAccessKey envSecretKey envSessionToken

-- | Retrieve access and secret keys from specific environment variables.
--
-- Throws 'MissingEnvError' if either of the specified environment variables
-- cannot be read.
fromEnvKeys :: (Applicative m, MonadIO m, MonadThrow m)
            => Text
            -> Text
            -> Text
            -> m Auth
fromEnvKeys a s t = fmap Auth $ AuthEnv
    <$> (req a <&> AccessKey)
    <*> (req s <&> SecretKey)
    <*> (opt t <&> fmap SessionToken)
    <*> pure Nothing
  where
    req k = do
        m <- opt k
        maybe (throwM . MissingEnvError $ "Unable to read ENV variable: " <> k)
              return
              m

    opt k = fmap BS8.pack <$> liftIO (lookupEnv (Text.unpack k))

-- | Loads the default @credentials@ INI file using the default profile name.
--
-- /See:/ 'credProfile' and 'credFile'
fromFile :: (Applicative m, MonadIO m, MonadCatch m) => m Auth
fromFile = credFile >>= fromFilePath credProfile

-- | Retrieve the access, secret and session token from the specified section
-- (profile) in a valid INI @credentials@ file.
fromFilePath :: (Applicative m, MonadIO m, MonadCatch m)
             => Text
             -> FilePath
             -> m Auth
fromFilePath n f = do
    p <- liftIO (doesFileExist f)
    unless p $ throwM (MissingFileError f)
    i <- either (throwM . invalidErr) return =<< liftIO (INI.readIniFile f)
    fmap Auth $ AuthEnv
        <$> (req credAccessKey i    <&> AccessKey)
        <*> (req credSecretKey i    <&> SecretKey)
        <*> (opt credSessionToken i <&> fmap SessionToken)
        <*> pure Nothing
  where
    req k i =
        either (throwM . invalidErr)
               (return . Text.encodeUtf8)
               (INI.lookupValue n k i)

    opt k i = return $
        either (const Nothing)
               (Just . Text.encodeUtf8)
               (INI.lookupValue n k i)

    invalidErr :: String -> AuthError
    invalidErr = InvalidFileError . Text.pack

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
