{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}

-- Module      : Network.AWS
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS where
    -- (
    -- -- * Synchronous
    --   send
    -- ) where

import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad.Trans
import Control.Monad.Trans.Resource
import Data.Conduit
import Data.Time
import Network.AWS.Signing.Types
import Network.AWS.Types
import Network.HTTP.Conduit

presign :: MonadIO m, AWSRequest a, Presignable a)
     => Auth
     -> Region
     -> Int -- ^ Expiry time in seconds.
     -> a
     -> m (Signed (Sv a) b)
presign = undefined

send :: (MonadResource m, AWSRequest a, Signable a)
     => Auth
     -> Region
     -> a
     -> Manager
     -> m (Either (Er (Sv a)) (Rs a))
send a r rq m = do
    sg <- sign rq <$> getAuthState a <*> pure r <*> liftIO getCurrentTime
    rs <- http (sgRequest sg) m
    response rq rs

paginate :: (MonadResource m, AWSPager a, Signable a)
         => Auth
         -> Region
         -> a
         -> Manager
         -> Source m (Either (Er (Sv a)) (Rs a))
paginate a r x m = go (Just x)
  where
    go Nothing   = return ()
    go (Just rq) = do
        rs <- lift (send a r rq m)
        yield rs
        either (const $ return ())
               (go . next rq)
               rs
