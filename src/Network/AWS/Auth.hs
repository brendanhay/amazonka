{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE ViewPatterns      #-}

-- Module      : Network.AWS.Auth
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- |
module Network.AWS.Auth where

import           Control.Applicative
import           Control.Concurrent
import           Control.Error
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
import           Network.AWS.EC2.Metadata
import           Network.AWS.Internal       hiding (Env)
import           System.Environment

data Credentials
    = AuthBasic Text Text
      -- ^ Basic credentials containing an access key and a secret key.
    | AuthSession Text Text Text
      -- ^ Session credentials containing access key, secret key, and a security token.
    | AuthProfile Text
      -- ^ A specific IAM Profile name to query the local instance-data for credentials.
    | AuthEnv Text Text
      -- ^ Environment variable names to read for the access and secret keys.
    | AuthDiscover
      -- ^ Attempt to read the default access and secret keys from the environment,
      -- falling back to the first available IAM profile if they are not set.
      --
      -- This attempts to resolve <http://instance-data> rather than directly
      -- retrieving <http://169.254.169.254> for IAM profile information to ensure
      -- the request terminates promptly if not running on EC2.
      deriving (Eq, Ord)

instance Show Credentials where
    show (AuthBasic   a _)   = Text.unpack $ Text.concat ["Basic ", a, " ****"]
    show (AuthSession a _ _) = Text.unpack $ Text.concat ["Session ", a, " **** ****"]
    show (AuthProfile n)     = Text.unpack $ "Profile " <> n
    show (AuthEnv     a s)   = Text.unpack $ Text.concat ["Env ", a, " ", s]
    show AuthDiscover        = "Discover"

-- | Default access key environment variable: 'AWS_ACCESS_KEY'
accessKey :: Text
accessKey = "AWS_ACCESS_KEY"

-- | Default secret key environment variable: 'AWS_SECRET_KEY'
secretKey :: Text
secretKey = "AWS_SECRET_KEY"

credentials :: (Applicative m, MonadIO m)
            => Credentials
            -> EitherT String m (IORef Auth)
credentials = mk
  where
    mk (AuthBasic   a s)   = ref $ Auth a s Nothing Nothing
    mk (AuthSession a s t) = ref $ Auth a s (Just t) Nothing
    mk (AuthProfile n)     = fromProfile n
    mk (AuthEnv     a s)   = fromKeys a s
    mk AuthDiscover        = fromKeys accessKey secretKey
        <|> (defaultProfile >>= fromProfile)

    fromKeys a s = Auth <$> key a <*> key s <*> pure Nothing <*> pure Nothing
        >>= ref

    key (Text.unpack -> k) = fmapLT (fromString . show) (syncIO $ lookupEnv k)
        >>= failWith (fromString $ "Unable to read ENV variable: " ++ k)
        >>= return . Text.pack

    ref = liftIO . newIORef

defaultProfile :: (Applicative m, MonadIO m) => EitherT String m Text
defaultProfile = do
    ls <- BS.lines <$> meta (IAM $ SecurityCredentials Nothing)
    p  <- tryHead "Unable to get default IAM Profile from metadata" ls
    return $ Text.decodeUtf8 p

-- | The IORef wrapper + timer is designed so that multiple concurrenct
-- accesses of 'Auth' from the 'AWS' environment are not required to calculate
-- expiry and sequentially queue to update it.
--
-- The forked timer ensures a singular owner and pre-emptive refresh of the
-- temporary session credentials.
fromProfile :: (Applicative m, MonadIO m)
            => Text
            -> EitherT String m (IORef Auth)
fromProfile name = do
    !a@Auth{..} <- auth
    fmapLT show . syncIO . liftIO $ do
        ref <- newIORef a
        start ref expiration
        return ref
  where
    auth :: (Applicative m, MonadIO m) => EitherT String m Auth
    auth = do
        m <- LBS.fromStrict <$> meta (IAM . SecurityCredentials $ Just name)
        hoistEither $ Aeson.eitherDecode m

    start ref = maybe (return ()) (timer ref <=< delay)

    delay n = truncate . diffUTCTime n <$> getCurrentTime

    -- FIXME: guard against a lower expiration than the -60
    -- remove the error . show shenanigans
    timer ref n = void . forkIO $ do
        threadDelay $ (n - 60) * 1000000
        !a@Auth{..} <- eitherT (error . show) return auth
        atomicWriteIORef ref a
        start ref expiration
