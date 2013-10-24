{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types        #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE ViewPatterns      #-}

-- Module      : Network.AWS.Internal.Monadic
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- |
module Network.AWS.Internal.Monadic where

import           Control.Applicative
import           Control.Error
import           Control.Monad
import           Control.Monad.Error        (MonadError, Error, throwError)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader
import qualified Data.Aeson                 as Aeson
import qualified Data.ByteString.Lazy       as LBS
import           Network.AWS.EC2.Metadata
import           Network.AWS.Internal.Types
import           OpenSSL                    (withOpenSSL)
import           Pipes                      hiding (next)

runAWS :: Credentials -> Bool -> AWS a -> IO (Either AWSError a)
runAWS cred dbg aws =
    eitherT (return . Left) (runEnv aws . Env defaultRegion dbg) $
        credentials cred

runEnv :: AWS a -> Env -> IO (Either AWSError a)
runEnv aws = withOpenSSL . runEitherT . runReaderT (unwrap aws)

credentials :: (Applicative m, MonadIO m)
            => Credentials
            -> EitherT AWSError m Auth
credentials cred = case cred of
    FromKeys acc sec -> right $ Auth acc sec Nothing
    FromRole role    -> do
        m <- LBS.fromStrict <$> metadata (SecurityCredentials role)
        hoistEither . fmapL Err $ Aeson.eitherDecode m

getAuth :: AWS Auth
getAuth = AWS $ awsAuth <$> ask

-- | Run an 'AWS' operation inside a specific 'Region'.
within :: Region -> AWS a -> AWS a
within reg = AWS . local (\e -> e { awsRegion = reg }) . unwrap

region :: Service -> AWS Region
region (Global _ _) = return defaultRegion
region _            = getRegion

defaultRegion :: Region
defaultRegion = NorthVirginia

getRegion :: AWS Region
getRegion = AWS $ awsRegion <$> ask

getDebug :: AWS Bool
getDebug = AWS $ awsDebug <$> ask

whenDebug :: AWS () -> AWS ()
whenDebug action = getDebug >>= \p -> when p action

hoistError :: (MonadError e m, Error e) => Either e a -> m a
hoistError = either throwError return

liftEitherT :: ToError e => EitherT e IO a -> AWS a
liftEitherT = AWS . lift . fmapLT toError
