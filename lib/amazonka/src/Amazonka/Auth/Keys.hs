-- |
-- Module      : Amazonka.Auth.Keys
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
--
-- Authentication via directly-provided access keys, including
-- optional session token and environment variable lookup.
module Amazonka.Auth.Keys where

import Amazonka.Auth.Exception (AuthError (MissingEnvError))
import Amazonka.Data
import Amazonka.Env (Env, Env' (..))
import Amazonka.Prelude
import Amazonka.Types
import Control.Exception (throw)
import Control.Monad.Trans.Maybe (MaybeT (..))
import qualified Data.ByteString.Char8 as BS8
import Data.Foldable (asum)
import qualified System.Environment as Environment

-- | Explicit access and secret keys.
fromKeys :: AccessKey -> SecretKey -> Env' withAuth -> Env
fromKeys a s env =
  env {auth = Identity . Auth $ AuthEnv a (Sensitive s) Nothing Nothing}

-- | Temporary credentials from a STS session consisting of
-- the access key, secret key, and session token.
--
-- /See:/ 'fromTemporarySession'
fromSession ::
  AccessKey -> SecretKey -> SessionToken -> Env' withAuth -> Env
fromSession a s t env =
  env
    { auth =
        Identity
          . Auth
          $ AuthEnv a (Sensitive s) (Just (Sensitive t)) Nothing
    }

-- | Temporary credentials from a STS session consisting of
-- the access key, secret key, session token, and expiration time.
--
-- /See:/ 'fromSession'
fromTemporarySession ::
  AccessKey ->
  SecretKey ->
  SessionToken ->
  UTCTime ->
  Env' withAuth ->
  Env
fromTemporarySession a s t e env =
  env
    { auth =
        Identity
          . Auth
          $ AuthEnv a (Sensitive s) (Just (Sensitive t)) (Just (Time e))
    }

-- | Retrieve access key, secret key and a session token from
-- environment variables. We copy the behaviour of the SDKs and
-- respect the following variables:
--
-- * @AWS_ACCESS_KEY_ID@ (and its alternate name, @AWS_ACCESS_KEY@)
-- * @AWS_SECRET_ACCESS_KEY@ (and its alternate name, @AWS_SECRET_KEY@)
-- * @AWS_SESSION_TOKEN@ (if present)
--
-- Throws 'MissingEnvError' if a required environment variable is
-- empty or unset.
fromKeysEnv :: (MonadIO m) => Env' withAuth -> m Env
fromKeysEnv env = liftIO $ do
  keys <- Auth <$> lookupKeys
  pure $ env {auth = Identity keys}
  where
    lookupKeys =
      AuthEnv
        <$> lookupAccessKey
        <*> lookupSecretKey
        <*> lookupSessionToken
        <*> pure Nothing

    lookupAccessKey :: IO AccessKey
    lookupAccessKey = do
      mVal <-
        runMaybeT
          $ asum
            [ MaybeT (nonEmptyEnv "AWS_ACCESS_KEY_ID"),
              MaybeT (nonEmptyEnv "AWS_ACCESS_KEY")
            ]
      case mVal of
        Nothing ->
          throw
            $ MissingEnvError
              "Unable to read access key from AWS_ACCESS_KEY_ID (or AWS_ACCESS_KEY)"
        Just v -> pure . AccessKey $ BS8.pack v

    lookupSecretKey :: IO (Sensitive SecretKey)
    lookupSecretKey = do
      mVal <-
        runMaybeT
          $ asum
            [ MaybeT (nonEmptyEnv "AWS_SECRET_ACCESS_KEY"),
              MaybeT (nonEmptyEnv "AWS_SECRET_KEY")
            ]
      case mVal of
        Nothing ->
          throw
            $ MissingEnvError
              "Unable to read secret key from AWS_SECRET_ACCESS_KEY (or AWS_SECRET_KEY)"
        Just v -> pure . Sensitive . SecretKey $ BS8.pack v

    lookupSessionToken :: IO (Maybe (Sensitive SessionToken))
    lookupSessionToken =
      nonEmptyEnv "AWS_SESSION_TOKEN"
        <&> fmap (Sensitive . SessionToken . BS8.pack)

    nonEmptyEnv :: String -> IO (Maybe String)
    nonEmptyEnv var =
      Environment.lookupEnv var <&> \case
        Nothing -> Nothing
        Just "" -> Nothing
        Just v -> Just v
