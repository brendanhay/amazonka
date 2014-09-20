{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ELB.DetachLoadBalancerFromSubnets
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
module Network.AWS.ELB.DetachLoadBalancerFromSubnets
    (
    -- * Request
      DetachLoadBalancerFromSubnets
    -- ** Request constructor
    , detachLoadBalancerFromSubnets
    -- ** Request lenses
    , dlbfsLoadBalancerName
    , dlbfsSubnets

    -- * Response
    , DetachLoadBalancerFromSubnetsResponse
    -- ** Response constructor
    , detachLoadBalancerFromSubnetsResponse
    -- ** Response lenses
    , dlbfsrSubnets
    ) where

import Network.AWS.Request.Query
import Network.AWS.ELB.Types
import Network.AWS.Prelude

-- | The input for the DetachLoadBalancerFromSubnets action.
data DetachLoadBalancerFromSubnets = DetachLoadBalancerFromSubnets
    { _dlbfsLoadBalancerName :: Text
    , _dlbfsSubnets :: [Text]
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DetachLoadBalancerFromSubnets' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @LoadBalancerName ::@ @Text@
--
-- * @Subnets ::@ @[Text]@
--
detachLoadBalancerFromSubnets :: Text -- ^ 'dlbfsLoadBalancerName'
                              -> [Text] -- ^ 'dlbfsSubnets'
                              -> DetachLoadBalancerFromSubnets
detachLoadBalancerFromSubnets p1 p2 = DetachLoadBalancerFromSubnets
    { _dlbfsLoadBalancerName = p1
    , _dlbfsSubnets = p2
    }

-- | The name associated with the load balancer to be detached.
dlbfsLoadBalancerName :: Lens' DetachLoadBalancerFromSubnets Text
dlbfsLoadBalancerName =
    lens _dlbfsLoadBalancerName (\s a -> s { _dlbfsLoadBalancerName = a })

-- | A list of subnet IDs to remove from the set of configured subnets for the
-- load balancer.
dlbfsSubnets :: Lens' DetachLoadBalancerFromSubnets [Text]
dlbfsSubnets = lens _dlbfsSubnets (\s a -> s { _dlbfsSubnets = a })

instance ToQuery DetachLoadBalancerFromSubnets where
    toQuery = genericQuery def

-- | The output for the DetachLoadBalancerFromSubnets action.
newtype DetachLoadBalancerFromSubnetsResponse = DetachLoadBalancerFromSubnetsResponse
    { _dlbfsrSubnets :: [Text]
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DetachLoadBalancerFromSubnetsResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Subnets ::@ @[Text]@
--
detachLoadBalancerFromSubnetsResponse :: DetachLoadBalancerFromSubnetsResponse
detachLoadBalancerFromSubnetsResponse = DetachLoadBalancerFromSubnetsResponse
    { _dlbfsrSubnets = mempty
    }

-- | A list of subnet IDs the load balancer is now attached to.
dlbfsrSubnets :: Lens' DetachLoadBalancerFromSubnetsResponse [Text]
dlbfsrSubnets = lens _dlbfsrSubnets (\s a -> s { _dlbfsrSubnets = a })

instance FromXML DetachLoadBalancerFromSubnetsResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DetachLoadBalancerFromSubnets where
    type Sv DetachLoadBalancerFromSubnets = ELB
    type Rs DetachLoadBalancerFromSubnets = DetachLoadBalancerFromSubnetsResponse

    request = post "DetachLoadBalancerFromSubnets"
    response _ = xmlResponse
