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
import Control.Monad.Trans.Resource
import Data.Default
import Data.Time
import Network.AWS.Data
import Network.AWS.Signing.Types
import Network.AWS.Types
import Network.HTTP.Conduit

type Failure a = Er (Sv a)
type Success a = Rs a

-- send :: (MonadResource m, AWSService (Sv a), AWSRequest a)
--      => Auth
--      -> Region
--      -> Manager
--      -> a
--      -> m (Either (Er (Sv a)) (Rs a))

send :: (MonadResource m, Signable a)
     => Auth
     -> Region
     -> Manager
     -> a
     -> m (Either (Failure a) (Success a))
send a r m rq = do
    sg <- sign a r rq <$> liftIO getCurrentTime
    rs <- http (mk sg) m
    response rq rs
  where
    mk Signed{..} = def
        { secure         = True
        , method         = toBS (rqMethod sgRequest)
        , host           = toBS sgHost
        , port           = 443
        , path           = rqPath sgRequest
        , queryString    = renderQuery (rqQuery sgRequest)
        , requestHeaders = rqHeaders sgRequest
        , requestBody    = rqBody sgRequest
        , checkStatus    = \_ _ _ -> Nothing
        }

-- paginate :: (MonadResource m, AWSRequest a, AWSPager a)
--          => Auth
--          -> a
--          -> Source m (Either (Er a) (Rs a))
-- paginate = undefined

-- async :: MonadBaseControl IO m => 
-- async = undefined

-- wait
