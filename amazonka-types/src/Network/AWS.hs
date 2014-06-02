{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}
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
import System.Locale

presign :: (AWSRequest a, AWSPresigner (Sg (Sv a)))
        => Auth    -- ^ AWS authentication credentials.
        -> Region  -- ^ AWS Region.
        -> a       -- ^ Request to presign.
        -> Int     -- ^ Expiry time in seconds.
        -> UTCTime -- ^ Signing time.
        -> Signed a (Sg (Sv a))
presign a r e rq t = Network.AWS.Signing.Types.presign a r rq t e

send :: (MonadResource m, AWSRequest a, AWSSigner (Sg (Sv a)))
     => Auth    -- ^ AWS authentication credentials.
     -> Region  -- ^ AWS Region.
     -> a       -- ^ Request to send.
     -> Manager -- ^ HTTP Manager.
     -> m (Either (Er (Sv a)) (Rs a))
send a r rq m = do
    sg <- sign a r rq <$> liftIO getCurrentTime
    rs <- http (_sgRequest sg) m
    response rq rs

-- paginate :: (MonadResource m, AWSPager a, Signable a)
--          => Auth
--          -> Region
--          -> a
--          -> Manager
--          -> Source m (Either (Er (Sv a)) (Rs a))
-- paginate a r x m = go (Just x)
--   where
--     go Nothing   = return ()
--     go (Just rq) = do
--         rs <- lift (send a r rq m)
--         yield rs
--         either (const $ return ())
--                (go . next rq)
--                rs
