{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ELB.V2012_06_01.DeleteLoadBalancerListeners
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes listeners from the load balancer for the specified port.
module Network.AWS.ELB.V2012_06_01.DeleteLoadBalancerListeners
    (
    -- * Request
      DeleteLoadBalancerListeners
    -- ** Request constructor
    , deleteLoadBalancerListeners
    -- ** Request lenses
    , dlbliLoadBalancerName
    , dlbliLoadBalancerPorts

    -- * Response
    , DeleteLoadBalancerListenersResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.ELB.V2012_06_01.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'DeleteLoadBalancerListeners' request.
deleteLoadBalancerListeners :: Text -- ^ 'dlbliLoadBalancerName'
                            -> [Integer] -- ^ 'dlbliLoadBalancerPorts'
                            -> DeleteLoadBalancerListeners
deleteLoadBalancerListeners p1 p2 = DeleteLoadBalancerListeners
    { _dlbliLoadBalancerName = p1
    , _dlbliLoadBalancerPorts = p2
    }

data DeleteLoadBalancerListeners = DeleteLoadBalancerListeners
    { _dlbliLoadBalancerName :: Text
      -- ^ The mnemonic name associated with the load balancer.
    , _dlbliLoadBalancerPorts :: [Integer]
      -- ^ The client port number(s) of the load balancer listener(s) to be
      -- removed.
    } deriving (Show, Generic)

-- | The mnemonic name associated with the load balancer.
dlbliLoadBalancerName
    :: Functor f
    => (Text
    -> f (Text))
    -> DeleteLoadBalancerListeners
    -> f DeleteLoadBalancerListeners
dlbliLoadBalancerName f x =
    (\y -> x { _dlbliLoadBalancerName = y })
       <$> f (_dlbliLoadBalancerName x)
{-# INLINE dlbliLoadBalancerName #-}

-- | The client port number(s) of the load balancer listener(s) to be removed.
dlbliLoadBalancerPorts
    :: Functor f
    => ([Integer]
    -> f ([Integer]))
    -> DeleteLoadBalancerListeners
    -> f DeleteLoadBalancerListeners
dlbliLoadBalancerPorts f x =
    (\y -> x { _dlbliLoadBalancerPorts = y })
       <$> f (_dlbliLoadBalancerPorts x)
{-# INLINE dlbliLoadBalancerPorts #-}

instance ToQuery DeleteLoadBalancerListeners where
    toQuery = genericQuery def

data DeleteLoadBalancerListenersResponse = DeleteLoadBalancerListenersResponse
    deriving (Eq, Show, Generic)

instance AWSRequest DeleteLoadBalancerListeners where
    type Sv DeleteLoadBalancerListeners = ELB
    type Rs DeleteLoadBalancerListeners = DeleteLoadBalancerListenersResponse

    request = post "DeleteLoadBalancerListeners"
    response _ = nullaryResponse DeleteLoadBalancerListenersResponse
