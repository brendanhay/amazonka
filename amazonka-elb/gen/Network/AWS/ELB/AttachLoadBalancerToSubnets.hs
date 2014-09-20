{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE StandaloneDeriving          #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ELB.AttachLoadBalancerToSubnets
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Adds one or more subnets to the set of configured subnets in the Amazon
-- Virtual Private Cloud (Amazon VPC) for the load balancer. The load
-- balancers evenly distribute requests across all of the registered subnets.
-- For more information, see Deploy Elastic Load Balancing in Amazon VPC in
-- the Elastic Load Balancing Developer Guide.
-- https://elasticloadbalancing.amazonaws.com/?Subnets.member.1=subnet-3561b05e
-- &LoadBalancerName=my-test-vpc-loadbalancer &Version=2012-06-01
-- &Action=AttachLoadBalancerToSubnets &AUTHPARAMS subnet-119f0078
-- subnet-3561b05e 07b1ecbc-1100-11e3-acaf-dd7edEXAMPLE.
module Network.AWS.ELB.AttachLoadBalancerToSubnets
    (
    -- * Request
      AttachLoadBalancerToSubnets
    -- ** Request constructor
    , attachLoadBalancerToSubnets
    -- ** Request lenses
    , albtsLoadBalancerName
    , albtsSubnets

    -- * Response
    , AttachLoadBalancerToSubnetsResponse
    -- ** Response constructor
    , attachLoadBalancerToSubnetsResponse
    -- ** Response lenses
    , albtsrSubnets
    ) where

import Network.AWS.Request.Query
import Network.AWS.ELB.Types
import Network.AWS.Prelude

-- | The input for the AttachLoadBalancerToSubnets action.
data AttachLoadBalancerToSubnets = AttachLoadBalancerToSubnets
    { _albtsLoadBalancerName :: Text
    , _albtsSubnets :: [Text]
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'AttachLoadBalancerToSubnets' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @LoadBalancerName ::@ @Text@
--
-- * @Subnets ::@ @[Text]@
--
attachLoadBalancerToSubnets :: Text -- ^ 'albtsLoadBalancerName'
                            -> [Text] -- ^ 'albtsSubnets'
                            -> AttachLoadBalancerToSubnets
attachLoadBalancerToSubnets p1 p2 = AttachLoadBalancerToSubnets
    { _albtsLoadBalancerName = p1
    , _albtsSubnets = p2
    }

-- | The name associated with the load balancer. The name must be unique within
-- the set of load balancers associated with your AWS account.
albtsLoadBalancerName :: Lens' AttachLoadBalancerToSubnets Text
albtsLoadBalancerName =
    lens _albtsLoadBalancerName (\s a -> s { _albtsLoadBalancerName = a })

-- | A list of subnet IDs to add for the load balancer. You can add only one
-- subnet per Availability Zone.
albtsSubnets :: Lens' AttachLoadBalancerToSubnets [Text]
albtsSubnets = lens _albtsSubnets (\s a -> s { _albtsSubnets = a })

instance ToQuery AttachLoadBalancerToSubnets where
    toQuery = genericQuery def

-- | The output for the AttachLoadBalancerToSubnets action.
newtype AttachLoadBalancerToSubnetsResponse = AttachLoadBalancerToSubnetsResponse
    { _albtsrSubnets :: [Text]
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'AttachLoadBalancerToSubnetsResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Subnets ::@ @[Text]@
--
attachLoadBalancerToSubnetsResponse :: AttachLoadBalancerToSubnetsResponse
attachLoadBalancerToSubnetsResponse = AttachLoadBalancerToSubnetsResponse
    { _albtsrSubnets = mempty
    }

-- | A list of subnet IDs attached to the load balancer.
albtsrSubnets :: Lens' AttachLoadBalancerToSubnetsResponse [Text]
albtsrSubnets = lens _albtsrSubnets (\s a -> s { _albtsrSubnets = a })

instance FromXML AttachLoadBalancerToSubnetsResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest AttachLoadBalancerToSubnets where
    type Sv AttachLoadBalancerToSubnets = ELB
    type Rs AttachLoadBalancerToSubnets = AttachLoadBalancerToSubnetsResponse

    request = post "AttachLoadBalancerToSubnets"
    response _ = xmlResponse
