{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE ViewPatterns      #-}

-- Module      : Network.AWS.Credentials
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.Credentials where

import           Control.Applicative
import           Control.Concurrent
import           Control.Error
import           Control.Exception          (throwIO)
import           Control.Monad
import           Control.Monad.IO.Class
import qualified Data.Aeson                 as Aeson
import qualified Data.ByteString.Char8      as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import           Data.IORef
import           Data.Monoid
import           Data.String
import           Data.Text                  (Text)
import qualified Data.Text                  as Text
import qualified Data.Text.Encoding         as Text
import           Data.Time
import           Network.AWS.Data
import           Network.AWS.EC2.Metadata
import           Network.AWS.Error
import           Network.AWS.Types
import           System.Environment

-- | Default access key environment variable: 'AWS_ACCESS_KEY'
accessKey :: Text
accessKey = "AWS_ACCESS_KEY"

-- | Default secret key environment variable: 'AWS_SECRET_KEY'
secretKey :: Text
secretKey = "AWS_SECRET_KEY"

data Credentials
    = CredKeys Text Text
      -- ^ Keys credentials containing an access key and a secret key.
    | CredSession Text Text Text
      -- ^ Session credentials containing access key, secret key, and a security token.
    | CredProfile Text
      -- ^ A specific IAM Profile name to query the local instance-data for credentials.
    | CredEnv Text Text
      -- ^ Environment variable names to read for the access and secret keys.
    | CredDiscover
      -- ^ Attempt to read the default access and secret keys from the environment,
      -- falling back to the first available IAM profile if they are not set.
      --
      -- This attempts to resolve <http://instance-data> rather than directly
      -- retrieving <http://169.254.169.254> for IAM profile information to ensure
      -- the dns lookup terminates promptly if not running on EC2.
      deriving (Eq, Ord)

instance ToText Credentials where
    toText (CredKeys    a _)   = Text.concat ["CredKeys ", a, " ****"]
    toText (CredSession a _ _) = Text.concat ["CredSession ", a, " **** ****"]
    toText (CredProfile n)     = "CredProfile " <> n
    toText (CredEnv     a s)   = Text.concat ["CredEnv ", a, " ", s]
    toText CredDiscover        = "CredDiscover"

instance Show Credentials where
    show = showText

credentials :: (Alternative m, MonadIO m)
            => Credentials
            -> EitherT Error m AuthRef
credentials c = case c of
    CredKeys    a s   -> newRef $ Auth a s Nothing Nothing
    CredSession a s t -> newRef $ Auth a s (Just t) Nothing
    CredProfile n     -> fromProfile n
    CredEnv     a s   -> fromKeys a s >>= newRef
    CredDiscover      -> fromDiscover
  where
    fromDiscover = (fromKeys accessKey secretKey >>= newRef)
        `catchT` const (defaultProfile >>= fromProfile)

    fromKeys a s = Auth
        <$> key a
        <*> key s
        <*> pure Nothing
        <*> pure Nothing

    key (Text.unpack -> k) = do
        m <- liftIO $ lookupEnv k
        maybe (throwT . Error $ "Unable to read ENV variable: " ++ k)
              (return . Text.pack)
              m

defaultProfile :: MonadIO m => EitherT Error m Text
defaultProfile = do
    ls <- BS.lines <$> meta (IAM $ SecurityCredentials Nothing)
    p  <- tryHead "Unable to get default IAM Profile from metadata" ls
    return $ Text.decodeUtf8 p

-- | The IONewRef wrapper + timer is designed so that multiple concurrenct
-- accesses of 'Auth' from the 'AWS' environment are not required to calculate
-- expiry and sequentially queue to update it.
--
-- The forked timer ensures a singular owner and pre-emptive refresh of the
-- temporary session credentials.
fromProfile :: MonadIO m => Text -> EitherT Error m AuthRef
fromProfile name = do
    !a@Auth{..} <- auth
    runIO $ do
        r <- newRef a
        start r authExpiry
        return r
  where
    auth :: MonadIO m => EitherT Error m Auth
    auth = do
        m <- LBS.fromStrict `liftM` meta iam
        hoistEither . fmapL fromString $ Aeson.eitherDecode m

    iam = IAM . SecurityCredentials $ Just name

    start r = maybe (return ()) (timer r <=< delay)

    delay n = truncate . diffUTCTime n <$> getCurrentTime

    -- FIXME:
    --  guard against a lower expiration than the -60
    --  remove the error . show shenanigans
    timer r n = void . forkIO $ do
        threadDelay $ (n - 60) * 1000000
        !a@Auth{..} <- eitherT throwIO return auth
        atomicWriteIORef (authRef r) a
        start r authExpiry

newRef :: MonadIO m => Auth -> m AuthRef
newRef = liftM AuthRef . liftIO . newIORef
