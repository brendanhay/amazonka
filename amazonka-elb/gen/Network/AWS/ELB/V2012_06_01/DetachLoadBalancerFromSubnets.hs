{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ELB.V2012_06_01.DetachLoadBalancerFromSubnets
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Removes subnets from the set of configured subnets in the Amazon Virtual
-- Private Cloud (Amazon VPC) for the load balancer. After a subnet is removed
-- all of the EC2 instances registered with the load balancer that are in the
-- removed subnet will go into the OutOfService state. When a subnet is
-- removed, the load balancer will balance the traffic among the remaining
-- routable subnets for the load balancer.
-- https://elasticloadbalancing.amazonaws.com/?Subnets.member.1=subnet-119f0078
-- &LoadBalancerName=my-test-vpc-loadbalancer &Version=2012-06-01
-- &Action=DetachLoadBalancerFromSubnets &AUTHPARAMS subnet-159f007c
-- subnet-3561b05e 07b1ecbc-1100-11e3-acaf-dd7edEXAMPLE.
module Network.AWS.ELB.V2012_06_01.DetachLoadBalancerFromSubnets
    (
    -- * Request
      DetachLoadBalancerFromSubnets
    -- ** Request constructor
    , detachLoadBalancerFromSubnets
    -- ** Request lenses
    , dlbfsiLoadBalancerName
    , dlbfsiSubnets

    -- * Response
    , DetachLoadBalancerFromSubnetsResponse
    -- ** Response lenses
    , dlbfsoSubnets
    ) where

import Network.AWS.Request.Query
import Network.AWS.ELB.V2012_06_01.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'DetachLoadBalancerFromSubnets' request.
detachLoadBalancerFromSubnets :: Text -- ^ 'dlbfsiLoadBalancerName'
                              -> [Text] -- ^ 'dlbfsiSubnets'
                              -> DetachLoadBalancerFromSubnets
detachLoadBalancerFromSubnets p1 p2 = DetachLoadBalancerFromSubnets
    { _dlbfsiLoadBalancerName = p1
    , _dlbfsiSubnets = p2
    }

data DetachLoadBalancerFromSubnets = DetachLoadBalancerFromSubnets
    { _dlbfsiLoadBalancerName :: Text
      -- ^ The name associated with the load balancer to be detached.
    , _dlbfsiSubnets :: [Text]
      -- ^ A list of subnet IDs to remove from the set of configured subnets
      -- for the load balancer.
    } deriving (Show, Generic)

-- | The name associated with the load balancer to be detached.
dlbfsiLoadBalancerName
    :: Functor f
    => (Text
    -> f (Text))
    -> DetachLoadBalancerFromSubnets
    -> f DetachLoadBalancerFromSubnets
dlbfsiLoadBalancerName f x =
    (\y -> x { _dlbfsiLoadBalancerName = y })
       <$> f (_dlbfsiLoadBalancerName x)
{-# INLINE dlbfsiLoadBalancerName #-}

-- | A list of subnet IDs to remove from the set of configured subnets for the
-- load balancer.
dlbfsiSubnets
    :: Functor f
    => ([Text]
    -> f ([Text]))
    -> DetachLoadBalancerFromSubnets
    -> f DetachLoadBalancerFromSubnets
dlbfsiSubnets f x =
    (\y -> x { _dlbfsiSubnets = y })
       <$> f (_dlbfsiSubnets x)
{-# INLINE dlbfsiSubnets #-}

instance ToQuery DetachLoadBalancerFromSubnets where
    toQuery = genericQuery def

data DetachLoadBalancerFromSubnetsResponse = DetachLoadBalancerFromSubnetsResponse
    { _dlbfsoSubnets :: [Text]
      -- ^ A list of subnet IDs the load balancer is now attached to.
    } deriving (Show, Generic)

-- | A list of subnet IDs the load balancer is now attached to.
dlbfsoSubnets
    :: Functor f
    => ([Text]
    -> f ([Text]))
    -> DetachLoadBalancerFromSubnetsResponse
    -> f DetachLoadBalancerFromSubnetsResponse
dlbfsoSubnets f x =
    (\y -> x { _dlbfsoSubnets = y })
       <$> f (_dlbfsoSubnets x)
{-# INLINE dlbfsoSubnets #-}

instance FromXML DetachLoadBalancerFromSubnetsResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DetachLoadBalancerFromSubnets where
    type Sv DetachLoadBalancerFromSubnets = ELB
    type Rs DetachLoadBalancerFromSubnets = DetachLoadBalancerFromSubnetsResponse

    request = post "DetachLoadBalancerFromSubnets"
    response _ = xmlResponse
