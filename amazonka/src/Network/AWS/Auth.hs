{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

-- |
-- Module      : Network.AWS.Auth
-- Copyright   : (c) 2013-2016 Brendan Hay
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
    -- ** Retrieving Authentication
      getAuth
    , Credentials  (..)
    , Auth         (..)

    -- ** Defaults
    -- *** Environment
    , envAccessKey
    , envSecretKey
    , envSessionToken

    -- *** Credentials File
    , credAccessKey
    , credSecretKey
    , credSessionToken
    , credProfile
    , credFile

    -- ** Credentials
    -- $credentials

    , fromKeys
    , fromSession
    , fromTemporarySession
    , fromEnv
    , fromEnvKeys
    , fromFile
    , fromFilePath
    , fromProfile
    , fromProfileName
    , fromAuthEnv

    -- ** Keys
    , AccessKey    (..)
    , SecretKey    (..)
    , SessionToken (..)

    -- ** Handling Errors
    , AsAuthError  (..)
    , AuthError    (..)
    ) where

import           Control.Applicative
import           Control.Concurrent
import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Maybe  (MaybeT (..))
import qualified Data.ByteString.Char8      as BS8
import qualified Data.ByteString.Lazy.Char8 as LBS8
import           Data.Char                  (isSpace)
import qualified Data.Ini                   as INI
import           Data.IORef
import           Data.Monoid
import qualified Data.Text                  as Text
import qualified Data.Text.Encoding         as Text
import           Data.Time                  (diffUTCTime, getCurrentTime)
import           Network.AWS.Data.Log
import           Network.AWS.EC2.Metadata
import           Network.AWS.Lens           (catching, catching_, exception,
                                             throwingM, _IOException)
import           Network.AWS.Lens           (Prism', prism, (<&>))
import           Network.AWS.Prelude
import           Network.AWS.Types
import           Network.HTTP.Conduit
import           System.Directory           (doesFileExist, getHomeDirectory)
import           System.Environment
import           System.Mem.Weak

-- | Default access key environment variable.
envAccessKey :: Text -- ^ AWS_ACCESS_KEY_ID
envAccessKey = "AWS_ACCESS_KEY_ID"

-- | Default secret key environment variable.
envSecretKey :: Text -- ^ AWS_SECRET_ACCESS_KEY
envSecretKey = "AWS_SECRET_ACCESS_KEY"

-- | Default session token environment variable.
envSessionToken :: Text -- ^ AWS_SESSION_TOKEN
envSessionToken = "AWS_SESSION_TOKEN"

-- | Default credentials profile environment variable.
envProfile :: Text -- ^ AWS_PROFILE
envProfile = "AWS_PROFILE"

-- | Default region environment variable
envRegion :: Text -- ^ AWS_REGION
envRegion = "AWS_REGION"

-- | Credentials INI file access key variable.
credAccessKey :: Text -- ^ aws_access_key_id
credAccessKey = "aws_access_key_id"

-- | Credentials INI file secret key variable.
credSecretKey :: Text -- ^ aws_secret_access_key
credSecretKey = "aws_secret_access_key"

-- | Credentials INI file session token variable.
credSessionToken :: Text -- ^ aws_session_token
credSessionToken = "aws_session_token"

-- | Credentials INI default profile section variable.
credProfile :: Text -- ^ default
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
credFile :: (MonadCatch m, MonadIO m) => m FilePath
credFile = catching_ _IOException dir err
  where
    dir = (++ p) `liftM` liftIO getHomeDirectory
    err = throwM $ MissingFileError ("$HOME" ++ p)

    -- TODO: probably should be using System.FilePath above.
    p = "/.aws/credentials"

{- $credentials
'getAuth' is implemented using the following @from*@-styled functions below.
Both 'fromKeys' and 'fromSession' can be used directly to avoid the 'MonadIO'
constraint.
-}

-- | Explicit access and secret keys.
fromKeys :: AccessKey -> SecretKey -> Auth
fromKeys a s = Auth (AuthEnv a s Nothing Nothing)

-- | Temporary credentials from a STS session consisting of
-- the access key, secret key, and session token.
--
-- This does not perform any refresh of credentials.
--
-- /See:/ 'fromTemporarySession'
fromSession :: AccessKey -> SecretKey -> SessionToken -> Auth
fromSession a s t = fromAuthEnv (AuthEnv a s (Just t) Nothing)

-- | Temporary credentials from a STS session consisting of
-- the access key, secret key, session token, and expiration time.
--
-- This will pre-emptively refresh the temporary session credentials
-- before expiration, see 'fromAuthEnv' for more information.
--
-- /See:/ 'fromSession', 'fromAuthEnv'
fromTemporarySession :: AccessKey
                     -> SecretKey
                     -> SessionToken
                     -> UTCTime
                     -> Auth
fromTemporarySession a s t = fromAuthEnv (AuthEnv a s (Just t) Nothing)

-- | Determines how AuthN/AuthZ information is retrieved.
data Credentials
    = FromKeys AccessKey SecretKey
      -- ^ Explicit access and secret keys. See 'fromKeys'.

    | FromSession AccessKey SecretKey SessionToken
      -- ^ Explicit access key, secret key and a session token. See 'fromSession'.

    | FromEnv Text Text (Maybe Text) (Maybe Text)
      -- ^ Lookup specific environment variables for access key, secret key,
      -- an optional session token, and an optional region, respectively.

    | FromProfile Text
      -- ^ An IAM Profile name to lookup from the local EC2 instance-data.
      -- Environment variables to lookup for the access key, secret key and
      -- optional session token.

    | FromFile Text FilePath
      -- ^ A credentials profile name (the INI section) and the path to the AWS
      -- <http://blogs.aws.amazon.com/security/post/Tx3D6U6WSFGOK2H/A-New-and-Standardized-Way-to-Manage-Credentials-in-the-AWS-SDKs credentials> file.

    | Discover
      -- ^ Attempt credentials discovery via the following steps:
      --
      -- * Read the 'envAccessKey', 'envSecretKey', and 'envRegion' from the environment if they are set.
      --
      -- * Read the credentials file if 'credFile' exists.
      --
      -- * Retrieve the first available IAM profile and read
      -- the 'Region' from the instance identity document, if running on EC2.
      --
      -- An attempt is made to resolve <http://instance-data> rather than directly
      -- retrieving <http://169.254.169.254> for IAM profile information.
      -- This assists in ensuring the DNS lookup terminates promptly if not
      -- running on EC2.
      deriving (Eq)

instance ToLog Credentials where
    build = \case
        FromKeys    a _ ->
            "FromKeys " <> build a <> " ****"
        FromSession a _ _ ->
            "FromSession " <> build a <> " **** ****"
        FromEnv     a s t r ->
            "FromEnv " <> build a <> " " <> build s <> " " <> m t <> " " <> m r
        FromProfile n ->
            "FromProfile " <> build n
        FromFile    n f ->
            "FromFile " <> build n <> " " <> build f
        Discover ->
            "Discover"
      where
        m (Just x) = "(Just " <> build x <> ")"
        m Nothing  = "Nothing"

instance Show Credentials where
    show = BS8.unpack . toBS . build

-- | An error thrown when attempting to read AuthN/AuthZ information.
data AuthError
    = RetrievalError   HttpException
    | MissingEnvError  Text
    | InvalidEnvError  Text
    | MissingFileError FilePath
    | InvalidFileError Text
    | InvalidIAMError  Text
      deriving (Show, Typeable)

instance Exception AuthError

instance ToLog AuthError where
    build = \case
        RetrievalError   e -> build e
        MissingEnvError  e -> "[MissingEnvError]  { message = " <> build e <> "}"
        InvalidEnvError  e -> "[InvalidEnvError]  { message = " <> build e <> "}"
        MissingFileError f -> "[MissingFileError] { path = "    <> build f <> "}"
        InvalidFileError e -> "[InvalidFileError] { message = " <> build e <> "}"
        InvalidIAMError  e -> "[InvalidIAMError]  { message = " <> build e <> "}"

class AsAuthError a where
    -- | A general authentication error.
    _AuthError        :: Prism' a AuthError
    {-# MINIMAL _AuthError #-}

    -- | An error occured while communicating over HTTP with
    -- the local metadata endpoint.
    _RetrievalError   :: Prism' a HttpException

    -- | The named environment variable was not found.
    _MissingEnvError  :: Prism' a Text

    -- | An error occured parsing named environment variable's value.
    _InvalidEnvError  :: Prism' a Text

    -- | The specified credentials file could not be found.
    _MissingFileError :: Prism' a FilePath

    -- | An error occured parsing the credentials file.
    _InvalidFileError :: Prism' a Text

    -- | The specified IAM profile could not be found or deserialised.
    _InvalidIAMError  :: Prism' a Text

    _RetrievalError   = _AuthError . _RetrievalError
    _MissingEnvError  = _AuthError . _MissingEnvError
    _InvalidEnvError  = _AuthError . _InvalidEnvError
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

    _InvalidEnvError = prism InvalidEnvError $ \case
        InvalidEnvError  e -> Right e
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
-- Throws 'AuthError' when environment variables or IAM profiles cannot be read,
-- and credentials files are invalid or cannot be found.
getAuth :: (Applicative m, MonadIO m, MonadCatch m)
        => Manager
        -> Credentials
        -> m (Auth, Maybe Region)
getAuth m = \case
    FromKeys    a s     -> return (fromKeys a s, Nothing)
    FromSession a s t   -> return (fromSession a s t, Nothing)
    FromEnv     a s t r -> fromEnvKeys a s t r
    FromProfile n       -> fromProfileName m n
    FromFile    n f     -> fromFilePath n f
    Discover            ->
        -- Don't try and catch InvalidFileError, or InvalidIAMProfile,
        -- let both errors propagate.
        catching_ _MissingEnvError fromEnv $
            -- proceed, missing env keys
            catching _MissingFileError fromFile $ \f -> do
                -- proceed, missing credentials file
                p <- isEC2 m
                unless p $
                    -- not an EC2 instance, rethrow the previous error.
                    throwingM _MissingFileError f
                 -- proceed, check EC2 metadata for IAM information.
                fromProfile m

-- | Retrieve access key, secret key, and a session token from the default
-- environment variables.
--
-- Throws 'MissingEnvError' if either of the default environment variables
-- cannot be read, but not if the session token is absent.
--
-- /See:/ 'envAccessKey', 'envSecretKey', 'envSessionToken'
fromEnv :: (Applicative m, MonadIO m, MonadThrow m) => m (Auth, Maybe Region)
fromEnv =
    fromEnvKeys
        envAccessKey
        envSecretKey
        (Just envSessionToken)
        (Just envRegion)

-- | Retrieve access key, secret key and a session token from specific
-- environment variables.
--
-- Throws 'MissingEnvError' if either of the specified key environment variables
-- cannot be read, but not if the session token is absent.
fromEnvKeys :: (Applicative m, MonadIO m, MonadThrow m)
            => Text       -- ^ Access key environment variable.
            -> Text       -- ^ Secret key environment variable.
            -> Maybe Text -- ^ Session token environment variable.
            -> Maybe Text -- ^ Region environment variable.
            -> m (Auth, Maybe Region)
fromEnvKeys access secret session region =
    (,) <$> fmap Auth lookupKeys <*> lookupRegion
  where
    lookupKeys = AuthEnv
        <$> (req access  <&> AccessKey . BS8.pack)
        <*> (req secret  <&> SecretKey . BS8.pack)
        <*> (opt session <&> fmap (SessionToken . BS8.pack))
        <*> return Nothing

    lookupRegion :: (MonadIO m, MonadThrow m) => m (Maybe Region)
    lookupRegion = runMaybeT $ do
        k <- MaybeT (return region)
        r <- MaybeT (opt region)
        case fromText (Text.pack r) of
            Right x -> return x
            Left  e -> throwM . InvalidEnvError $
                "Unable to parse ENV variable: " <> k <> ", " <> Text.pack e

    req k = do
        m <- opt (Just k)
        maybe (throwM . MissingEnvError $ "Unable to read ENV variable: " <> k)
              return
              m

    opt Nothing  = return Nothing
    opt (Just k) = liftIO (lookupEnv (Text.unpack k))

-- | Loads the default @credentials@ INI file using the default profile name.
--
-- Throws 'MissingFileError' if 'credFile' is missing, or 'InvalidFileError'
-- if an error occurs during parsing.
--
-- /See:/ 'credProfile', 'credFile', and 'envProfile'
fromFile :: (Applicative m, MonadIO m, MonadCatch m) => m (Auth, Maybe Region)
fromFile = do
  p <- liftIO (lookupEnv (Text.unpack envProfile))
  fromFilePath (maybe credProfile Text.pack p)
      =<< credFile

-- | Retrieve the access, secret and session token from the specified section
-- (profile) in a valid INI @credentials@ file.
--
-- Throws 'MissingFileError' if the specified file is missing, or 'InvalidFileError'
-- if an error occurs during parsing.
fromFilePath :: (Applicative m, MonadIO m, MonadCatch m)
             => Text
             -> FilePath
             -> m (Auth, Maybe Region)
fromFilePath n f = do
    p <- liftIO (doesFileExist f)
    unless p $
        throwM (MissingFileError f)
    ini <- either (invalidErr Nothing) return =<< liftIO (INI.readIniFile f)
    env <- AuthEnv
        <$> (req credAccessKey    ini <&> AccessKey)
        <*> (req credSecretKey    ini <&> SecretKey)
        <*> (opt credSessionToken ini <&> fmap SessionToken)
        <*> return Nothing
    return (Auth env, Nothing)
  where
    req k i =
        case INI.lookupValue n k i of
            Left  e         -> invalidErr (Just k) e
            Right x
                | blank x   -> invalidErr (Just k) "cannot be a blank string."
                | otherwise -> return (Text.encodeUtf8 x)

    opt k i = return $
        case INI.lookupValue n k i of
            Left  _ -> Nothing
            Right x -> Just (Text.encodeUtf8 x)

    invalidErr Nothing  e = throwM $ InvalidFileError (Text.pack e)
    invalidErr (Just k) e = throwM $ InvalidFileError
        (Text.pack f <> ", key " <> k <> " " <> Text.pack e)

    blank x = Text.null x || Text.all isSpace x

-- | Retrieve the default IAM Profile from the local EC2 instance-data.
--
-- The default IAM profile is determined by Amazon as the first profile found
-- in the response from:
-- @http://169.254.169.254/latest/meta-data/iam/security-credentials/@
--
-- Throws 'RetrievalError' if the HTTP call fails, or 'InvalidIAMError' if
-- the default IAM profile cannot be read.
fromProfile :: (MonadIO m, MonadCatch m) => Manager -> m (Auth, Maybe Region)
fromProfile m = do
    ls <- try $ metadata m (IAM (SecurityCredentials Nothing))
    case BS8.lines `liftM` ls of
        Right (x:_) -> fromProfileName m (Text.decodeUtf8 x)
        Left  e     -> throwM (RetrievalError e)
        _           -> throwM $
            InvalidIAMError "Unable to get default IAM Profile from EC2 metadata"

-- | Lookup a specific IAM Profile by name from the local EC2 instance-data.
--
-- If valid STS security credentials are retrieved, they will be automatically
-- refreshed periodically before expiration.
--
-- /See:/ 'fromAuthEnv'.
fromProfileName :: (MonadIO m, MonadCatch m)
                => Manager
                -> Text
                -> m (Auth, Maybe Region)
fromProfileName m name = do
    auth <- getCredentials >>= fromAuthEnv
    reg  <- getRegion
    return (auth, Just reg)
  where
    getCredentials :: (MonadIO m, MonadCatch m) => m AuthEnv
    getCredentials =
        try (metadata m (IAM . SecurityCredentials $ Just name)) >>=
            handleErr (eitherDecode' . LBS8.fromStrict) invalidIAMErr

    getRegion :: (MonadIO m, MonadCatch m) => m Region
    getRegion =
       try (identity m) >>=
           handleErr (fmap _region) invalidIdentityErr

    handleErr _ _ (Left  e) = throwM (RetrievalError e)
    handleErr f g (Right x) = either (throwM . g) return (f x)

    invalidIAMErr = InvalidIAMError
        . mappend ("Error parsing IAM profile '" <> name <> "' ")
        . Text.pack

    invalidIdentityErr = InvalidIAMError
        . mappend "Error parsing Instance Identity Document "
        . Text.pack

-- | Starts a refresh thread for the given authentication environment.
--
-- The resulting 'IORef' wrapper + timer is designed so that multiple concurrent
-- accesses of 'AuthEnv' from the 'AWS' environment are not required to calculate
-- expiry and sequentially queue to update it.
--
-- The forked timer ensures a singular owner and pre-emptive refresh of the
-- temporary session credentials before expiration.
--
-- A weak reference is used to ensure that the forked thread will eventually
-- terminate when 'Auth' is no longer referenced.
--
-- If no STS token or expiration time is present the credentials will
-- be returned verbatim.
--
fromAuthEnv :: MonadIO m => AuthEnv -> m Auth
fromAuthEnv a
    | Nothing <- _authExpiry a = return (Auth a)
    | otherwise                = do
        r <- newIORef a
        p <- myThreadId
        s <- timer r p x
        return (Ref s r)
  where
    timer :: IORef AuthEnv -> ThreadId -> UTCTime -> IO ThreadId
    timer !r !p !x = forkIO $ do
        s <- myThreadId
        w <- mkWeakIORef r (killThread s)
        loop w p x

    loop :: Weak (IORef AuthEnv) -> ThreadId -> UTCTime -> IO ()
    loop w !p !x = do
        diff x <$> getCurrentTime >>= threadDelay
        env <- try getCredentials
        case env of
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
