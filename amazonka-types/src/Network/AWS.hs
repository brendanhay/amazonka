{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}

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
   -- * Sending Requests
   -- ** Synchronous
     send
   -- ** Pagination
   , paginate
   -- ** Signed URLs
   , presign
   ) where

import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad.Trans
import Control.Monad.Trans.Resource
import Data.Conduit
import Data.Time
import Network.AWS.Auth
import Network.AWS.Signing.Types
import Network.AWS.Types
import Network.HTTP.Conduit

send :: (MonadResource m, AWSRequest a, AWSSigner (Sg (Sv a)))
     => Auth    -- ^ AWS authentication credentials.
     -> Region  -- ^ AWS Region.
     -> a       -- ^ Request to send.
     -> Manager -- ^ HTTP Manager.
     -> m (Either (Error' (Sv a)) (Rs a))
send a r rq m = do
    sg <- liftIO getCurrentTime >>= sign a r rq
    rs <- http (_sgRequest sg) m
    response rq rs

paginate :: (MonadResource m, AWSPager a, AWSSigner (Sg (Sv a)))
         => Auth    -- ^ AWS authentication credentials.
         -> Region  -- ^ AWS Region.
         -> a       -- ^ Seed request to send.
         -> Manager -- ^ HTTP Manager.
         -> Source m (Either (Error' (Sv a)) (Rs a))
paginate a r rq m = go (Just rq)
  where
    go Nothing   = return ()
    go (Just rq') = do
        rs <- lift (send a r rq' m)
        yield rs
        either (const $ return ())
               (go . next rq')
               rs
