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

import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Trans
import Control.Monad.Trans.Resource
import Data.Conduit
import Data.Time
import Network.AWS.Signing.Types
import Network.AWS.Types
import Network.HTTP.Conduit

send :: (MonadCatch m, MonadResource m, AWSRequest a, AWSSigner (Sg (Sv a)))
     => Auth    -- ^ AWS authentication credentials.
     -> Region  -- ^ AWS Region.
     -> a       -- ^ Request to send.
     -> Manager -- ^ HTTP Manager.
     -> m (Either (Er (Sv a)) (Rs a))
send a r rq m = (go `catch` er) >>= response rq
  where
    go = liftIO getCurrentTime
        >>= sign a r rq
        >>= (`http` m) . _sgRequest
        >>= return . Right

    er :: MonadResource m => ClientException -> m (Either ClientException b)
    er = return . Left

paginate :: (MonadCatch m, MonadResource m, AWSPager a, AWSSigner (Sg (Sv a)))
         => Auth    -- ^ AWS authentication credentials.
         -> Region  -- ^ AWS Region.
         -> a       -- ^ Seed request to send.
         -> Manager -- ^ HTTP Manager.
         -> Source m (Either (Er (Sv a)) (Rs a))
paginate a r rq m = go (Just rq)
  where
    go Nothing   = return ()
    go (Just rq') = do
        rs <- lift (send a r rq' m)
        yield rs
        either (const $ return ())
               (go . next rq')
               rs
