{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ELB.ApplySecurityGroupsToLoadBalancer
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Associates one or more security groups with your load balancer in Amazon
-- Virtual Private Cloud (Amazon VPC). The provided security group IDs will
-- override any currently applied security groups. For more information, see
-- Manage Security Groups in Amazon VPC in the Elastic Load Balancing
-- Developer Guide.
-- https://elasticloadbalancing.amazonaws.com/?SecurityGroups.member.1=sg-123456789
-- &LoadBalancerName=my-test-vpc-loadbalancer &Version=2012-06-01
-- &Action=ApplySecurityGroupsToLoadBalancer &AUTHPARAMS sg-123456789
-- 06b5decc-102a-11e3-9ad6-bf3e4EXAMPLE.
module Network.AWS.ELB.ApplySecurityGroupsToLoadBalancer
    (
    -- * Request
      ApplySecurityGroupsToLoadBalancer
    -- ** Request constructor
    , applySecurityGroupsToLoadBalancer
    -- ** Request lenses
    , asgtlbLoadBalancerName
    , asgtlbSecurityGroups

    -- * Response
    , ApplySecurityGroupsToLoadBalancerResponse
    -- ** Response constructor
    , applySecurityGroupsToLoadBalancerResponse
    -- ** Response lenses
    , asgtlbrSecurityGroups
    ) where

import Network.AWS.Request.Query
import Network.AWS.ELB.Types
import Network.AWS.Prelude

-- | The input for the ApplySecurityGroupsToLoadBalancer action.
data ApplySecurityGroupsToLoadBalancer = ApplySecurityGroupsToLoadBalancer
    { _asgtlbLoadBalancerName :: Text
    , _asgtlbSecurityGroups :: [Text]
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ApplySecurityGroupsToLoadBalancer' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @LoadBalancerName ::@ @Text@
--
-- * @SecurityGroups ::@ @[Text]@
--
applySecurityGroupsToLoadBalancer :: Text -- ^ 'asgtlbLoadBalancerName'
                                    -> [Text] -- ^ 'asgtlbSecurityGroups'
                                    -> ApplySecurityGroupsToLoadBalancer
applySecurityGroupsToLoadBalancer p1 p2 = ApplySecurityGroupsToLoadBalancer
    { _asgtlbLoadBalancerName = p1
    , _asgtlbSecurityGroups = p2
    }

-- | The name associated with the load balancer. The name must be unique within
-- the set of load balancers associated with your AWS account.
asgtlbLoadBalancerName :: Lens' ApplySecurityGroupsToLoadBalancer Text
asgtlbLoadBalancerName =
    lens _asgtlbLoadBalancerName (\s a -> s { _asgtlbLoadBalancerName = a })

-- | A list of security group IDs to associate with your load balancer in VPC.
-- The security group IDs must be provided as the ID and not the security
-- group name (For example, sg-1234).
asgtlbSecurityGroups :: Lens' ApplySecurityGroupsToLoadBalancer [Text]
asgtlbSecurityGroups =
    lens _asgtlbSecurityGroups (\s a -> s { _asgtlbSecurityGroups = a })

instance ToQuery ApplySecurityGroupsToLoadBalancer where
    toQuery = genericQuery def

-- | The out for the ApplySecurityGroupsToLoadBalancer action.
newtype ApplySecurityGroupsToLoadBalancerResponse = ApplySecurityGroupsToLoadBalancerResponse
    { _asgtlbrSecurityGroups :: [Text]
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ApplySecurityGroupsToLoadBalancerResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @SecurityGroups ::@ @[Text]@
--
applySecurityGroupsToLoadBalancerResponse :: ApplySecurityGroupsToLoadBalancerResponse
applySecurityGroupsToLoadBalancerResponse = ApplySecurityGroupsToLoadBalancerResponse
    { _asgtlbrSecurityGroups = mempty
    }

-- | A list of security group IDs associated with your load balancer.
asgtlbrSecurityGroups :: Lens' ApplySecurityGroupsToLoadBalancerResponse [Text]
asgtlbrSecurityGroups =
    lens _asgtlbrSecurityGroups (\s a -> s { _asgtlbrSecurityGroups = a })

instance FromXML ApplySecurityGroupsToLoadBalancerResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest ApplySecurityGroupsToLoadBalancer where
    type Sv ApplySecurityGroupsToLoadBalancer = ELB
    type Rs ApplySecurityGroupsToLoadBalancer = ApplySecurityGroupsToLoadBalancerResponse

    request = post "ApplySecurityGroupsToLoadBalancer"
    response _ = xmlResponse
