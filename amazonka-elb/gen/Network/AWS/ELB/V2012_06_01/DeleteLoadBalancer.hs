{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ELB.V2012_06_01.DeleteLoadBalancer
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes the specified load balancer. If attempting to recreate the load
-- balancer, you must reconfigure all the settings. The DNS name associated
-- with a deleted load balancer will no longer be usable. Once deleted, the
-- name and associated DNS record of the load balancer no longer exist and
-- traffic sent to any of its IP addresses will no longer be delivered to
-- back-end instances. To successfully call this API, you must provide the
-- same account credentials as were used to create the load balancer. By
-- design, if the load balancer does not exist or has already been deleted, a
-- call to DeleteLoadBalancer action still succeeds.
module Network.AWS.ELB.V2012_06_01.DeleteLoadBalancer
    (
    -- * Request
      DeleteLoadBalancer
    -- ** Request constructor
    , mkDeleteLoadBalancer
    -- ** Request lenses
    , dlbLoadBalancerName

    -- * Response
    , DeleteLoadBalancerResponse
    -- ** Response constructor
    , mkDeleteLoadBalancerResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.ELB.V2012_06_01.Types
import Network.AWS.Prelude

-- | The input for the DeleteLoadBalancer action.
newtype DeleteLoadBalancer = DeleteLoadBalancer
    { _dlbLoadBalancerName :: Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeleteLoadBalancer' request.
mkDeleteLoadBalancer :: Text -- ^ 'dlbLoadBalancerName'
                     -> DeleteLoadBalancer
mkDeleteLoadBalancer p1 = DeleteLoadBalancer
    { _dlbLoadBalancerName = p1
    }

-- | The name associated with the load balancer.
dlbLoadBalancerName :: Lens' DeleteLoadBalancer Text
dlbLoadBalancerName =
    lens _dlbLoadBalancerName (\s a -> s { _dlbLoadBalancerName = a })

instance ToQuery DeleteLoadBalancer where
    toQuery = genericQuery def

-- | The output for the DeleteLoadBalancer action.
data DeleteLoadBalancerResponse = DeleteLoadBalancerResponse
    deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeleteLoadBalancerResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
mkDeleteLoadBalancerResponse :: DeleteLoadBalancerResponse
mkDeleteLoadBalancerResponse = DeleteLoadBalancerResponse

instance AWSRequest DeleteLoadBalancer where
    type Sv DeleteLoadBalancer = ELB
    type Rs DeleteLoadBalancer = DeleteLoadBalancerResponse

    request = post "DeleteLoadBalancer"
    response _ = nullaryResponse DeleteLoadBalancerResponse
