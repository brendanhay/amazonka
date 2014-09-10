{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
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


--
module Network.AWS
    (
    -- * Environment
      Env
    -- ** Creating the environment
    , Credentials (..)
    , newEnv
    -- ** Lenses
    , envAuth
    , envRegion
    , envManager
    , envLogging

    -- * Requests
    -- ** Synchronous
    , send
    -- ** Paginated
    , paginate
    -- ** Pre-signing URLs
    , presign

    -- * Types
    , module Network.AWS.Types
    ) where

import           Control.Lens
import           Control.Monad.Catch
import           Control.Monad.Except
import           Control.Monad.Trans.Resource
import           Data.Conduit
import           Data.Monoid
import           Data.Time
import           Network.AWS.Auth
import           Network.AWS.Data
import qualified Network.AWS.Internal.Signing as Sign
import           Network.AWS.Types
import           Network.HTTP.Conduit

-- | The environment containing the parameters required to make AWS requests.
data Env = Env
    { _envRegion  :: !Region
    , _envLogging :: Logging
    , _envManager :: Manager
    , _envAuth    :: Auth
    }

makeLenses ''Env

-- | This creates a new environment without debug logging and uses 'getAuth'
-- to expand/discover the supplied 'Credentials'.
--
-- Lenses such as 'envLogging' can be used to modify the 'Env' with a debug log.
newEnv :: (MonadIO m, MonadError Error m)
       => Region
       -> Credentials
       -> Manager
       -> m Env
newEnv r c m = Env r None m `liftM` getAuth c

-- | Send a data type which is an instance of 'AWSRequest', returning either the
-- associated 'Rs' response type in the success case, or the related service's
-- 'Er' type in the error case.
--
-- This includes 'HTTPExceptions', serialisation errors, and any service
-- errors returned as part of the 'Response'.
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
        sg <- Sign.sign _envAuth _envRegion rq t
        debug _envLogging $
            "[Signed Request]\n" <> toText sg
        rs <- http (sg ^. sgRequest) _envManager
        debug _envLogging $
            "[Raw Response]\n" <> toText rs
        return (Right rs)

    er ex = return (Left (ex :: HttpException))

-- | Send a data type which is an instance of 'AWSPager' and paginate over
-- the associated 'Rs' response type in the success case, or the related service's
-- 'Er' type in the error case.
--
-- Note: The 'ResumableSource' will close when there are no more results or the
-- 'ResourceT' computation is unwrapped. See: 'runResourceT' for more information.
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

-- | Presign a URL with expiry to be used at a later time.
--
-- Note: Requires the service's signer to be an instance of 'AWSPresigner'.
-- Not all signing process support this.
presign :: (MonadIO m, AWSRequest a, AWSPresigner (Sg (Sv a)))
        => Env
        -> a       -- ^ Request to presign.
        -> UTCTime -- ^ Signing time.
        -> Int     -- ^ Expiry time in seconds.
        -> m (Signed a (Sg (Sv a)))
presign Env{..} (request -> rq) = Sign.presign _envAuth _envRegion rq
