{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types        #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

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
import qualified Control.Concurrent.Async   as A
import           Control.Error
import           Control.Exception
import           Control.Monad
import           Control.Monad.Error        (MonadError, Error, throwError)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader
import qualified Data.Aeson                 as Aeson
import           Data.ByteString            (ByteString)
import qualified Data.ByteString.Char8      as BS
import qualified Data.ByteString.Lazy       as LBS
import           Network.AWS.EC2.Metadata
import           Network.AWS.Internal.Types
import           Network.Http.Client
import           Network.Http.Internal      (retrieveHeaders)
import           OpenSSL                    (withOpenSSL)
import           Pipes                      hiding (next)
import qualified System.IO.Streams          as Streams

createAuth :: (Applicative m, MonadIO m)
           => Credentials
          -> EitherT AWSError m Auth
createAuth cred = case cred of
    FromKeys acc sec -> right $ Auth acc sec Nothing
    FromRole role    -> do
        m <- LBS.fromStrict <$> metadata (SecurityCredentials role)
        hoistEither . fmapL Err $ Aeson.eitherDecode m

getAuth :: AWS Auth
getAuth = AWS $ awsAuth <$> ask

runAWS :: Env -> AWS a -> IO (Either AWSError a)
runAWS env aws = withOpenSSL . runEitherT $ runReaderT (unwrap aws) env

-- | Run an 'AWS' operation inside a specific 'Region'.
within :: Region -> AWS a -> AWS a
within reg = AWS . local (\e -> e { awsRegion = Just reg }) . unwrap

defaultRegion :: Region
defaultRegion = NorthVirginia

getRegion :: AWS Region
getRegion = AWS $ fromMaybe defaultRegion <$> awsRegion <$> ask

serviceRegion :: Service -> AWS Region
serviceRegion svc
    | svcGlobal svc = return defaultRegion
    | otherwise     = getRegion

getDebug :: AWS Bool
getDebug = AWS $ awsDebug <$> ask

whenDebug :: AWS () -> AWS ()
whenDebug action = getDebug >>= \p -> when p action

hoistError :: (MonadError e m, Error e) => Either e a -> m a
hoistError = either throwError return

liftEitherT :: ToError e => EitherT e IO a -> AWS a
liftEitherT = AWS . lift . fmapLT toError
