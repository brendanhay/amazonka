{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- Module      : Network.AWS.Internal.Monad
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.Internal.Monad where

import Control.Applicative
import Control.Error
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader            hiding (lift)
import Network.AWS.Internal.Types

data Env = Env
    { awsRegion :: Maybe Region
    , awsAuth   :: !Auth
    , awsDebug  :: !Bool
    }

newtype AWS a = AWS { unWrap :: EitherT Error (ReaderT Env IO) a }
    deriving (Functor, Applicative, Monad, MonadIO, MonadReader Env)

type AWSContext = EitherT Error AWS

currentRegion :: AWS Region
currentRegion = fromMaybe NorthVirgnia <$> fmap awsRegion ask

currentAuth :: AWS Auth
currentAuth = awsAuth <$> ask

whenDebug :: IO () -> AWSContext ()
whenDebug io = fmap awsDebug ask >>= \p -> liftIO $ when p io

throwError :: Monad m => String -> EitherT Error m a
throwError = throwT . Error

fmapError :: Monad m => EitherT String m a -> EitherT Error m a
fmapError = fmapLT Error

hoistError :: Monad m => Either String a -> EitherT Error m a
hoistError = hoistEither . fmapL Error

tryIO' :: MonadIO m => IO a -> EitherT Error m a
tryIO' = fmapLT Ex . syncIO
