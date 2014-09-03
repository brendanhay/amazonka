{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE ViewPatterns      #-}

-- Module      : Network.AWS
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS
    (
    -- * Environment
      Env (..)
    -- ** Lenses
    , envAuth
    , envRegion
    , envManager
    , envLogging

    -- ** Creating the environment
    , newEnv
    , module Auth

    -- * Synchronous requests
    -- ** Strict
    , send
    -- ** Pagination
    , paginate

    -- * Signing URLs
    , presign

    -- * Types
    , module Network.AWS.Types
    ) where

import Control.Applicative
import Control.Lens                 ((^.))
import Control.Monad.Catch
import Control.Monad.Except
import Control.Monad.Trans.Resource
import Data.Conduit
import Data.Monoid
import Data.Time
import Network.AWS.Data
import Network.AWS.Internal.Auth    as Auth
import Network.AWS.Internal.Signing
import Network.AWS.Types
import Network.HTTP.Conduit

data Env = Env
    { _envRegion  :: !Region
    , _envLogging :: Logging
    , _envManager :: Manager
    , _envAuth    :: Auth
    }

  :: Functor f
    => (Maybe VpnState
    -> f (Maybe VpnState))
    -> VpnStaticRoute
    -> f VpnStaticRoute
vsrState f x =
    (\y -> x { _vsrState = y })
       <$> f (_vsrState x)
{-# INLINE vsrState #-}

makeLenses ''Env

newEnv :: (Functor m, MonadIO m)
       => Region
       -> Credentials
       -> ExceptT Error m Env
newEnv r c = Env r None
    <$> liftIO (newManager conduitManagerSettings)
    <*> getAuth c

send :: (MonadCatch m, MonadResource m, AWSRequest a)
     => Env
     -> a
     -> m (Either (Er (Sv a)) (Rs a))
send Env{..} x@(request -> rq) = go `catch` er >>= response x
  where
    go = do
        debug _envLogging $
            "[Raw Request]\n" <> toText rq
        t  <- liftIO getCurrentTime
        sg <- sign _envAuth _envRegion rq t
        debug _envLogging $
            "[Signed Request]\n" <> toText sg
        rs <- http (sg ^. sgRequest) _envManager
        debug _envLogging $
            "[Raw Response]\n" <> toText rs
        return (Right rs)

    er ex = return (Left (ex :: HttpException))

paginate :: (MonadCatch m, MonadResource m, AWSPager a)
         => Env
         -> a
         -> ResumableSource m (Either (Er (Sv a)) (Rs a))
paginate e = newResumableSource . go
  where
    go rq = do
        rs <- lift (send e rq)
        yield rs
        either (const $ return ())
               (maybe (return ()) go . next rq)
               rs
