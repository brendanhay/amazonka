{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ELB.V2012_06_01.ApplySecurityGroupsToLoadBalancer
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
module Network.AWS.ELB.V2012_06_01.ApplySecurityGroupsToLoadBalancer
    (
    -- * Request
      ApplySecurityGroupsToLoadBalancer
    -- ** Request constructor
    , applySecurityGroupsToLoadBalancer
    -- ** Request lenses
    , asgtlbiLoadBalancerName
    , asgtlbiSecurityGroups

    -- * Response
    , ApplySecurityGroupsToLoadBalancerResponse
    -- ** Response lenses
    , asgtlboSecurityGroups
    ) where

import Network.AWS.Request.Query
import Network.AWS.ELB.V2012_06_01.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'ApplySecurityGroupsToLoadBalancer' request.
applySecurityGroupsToLoadBalancer :: Text -- ^ 'asgtlbiLoadBalancerName'
                                  -> [Text] -- ^ 'asgtlbiSecurityGroups'
                                  -> ApplySecurityGroupsToLoadBalancer
applySecurityGroupsToLoadBalancer p1 p2 = ApplySecurityGroupsToLoadBalancer
    { _asgtlbiLoadBalancerName = p1
    , _asgtlbiSecurityGroups = p2
    }

data ApplySecurityGroupsToLoadBalancer = ApplySecurityGroupsToLoadBalancer
    { _asgtlbiLoadBalancerName :: Text
      -- ^ The name associated with the load balancer. The name must be
      -- unique within the set of load balancers associated with your AWS
      -- account.
    , _asgtlbiSecurityGroups :: [Text]
      -- ^ A list of security group IDs to associate with your load balancer
      -- in VPC. The security group IDs must be provided as the ID and not
      -- the security group name (For example, sg-1234).
    } deriving (Show, Generic)

-- | The name associated with the load balancer. The name must be unique within
-- the set of load balancers associated with your AWS account.
asgtlbiLoadBalancerName
    :: Functor f
    => (Text
    -> f (Text))
    -> ApplySecurityGroupsToLoadBalancer
    -> f ApplySecurityGroupsToLoadBalancer
asgtlbiLoadBalancerName f x =
    (\y -> x { _asgtlbiLoadBalancerName = y })
       <$> f (_asgtlbiLoadBalancerName x)
{-# INLINE asgtlbiLoadBalancerName #-}

-- | A list of security group IDs to associate with your load balancer in VPC.
-- The security group IDs must be provided as the ID and not the security
-- group name (For example, sg-1234).
asgtlbiSecurityGroups
    :: Functor f
    => ([Text]
    -> f ([Text]))
    -> ApplySecurityGroupsToLoadBalancer
    -> f ApplySecurityGroupsToLoadBalancer
asgtlbiSecurityGroups f x =
    (\y -> x { _asgtlbiSecurityGroups = y })
       <$> f (_asgtlbiSecurityGroups x)
{-# INLINE asgtlbiSecurityGroups #-}

instance ToQuery ApplySecurityGroupsToLoadBalancer where
    toQuery = genericQuery def

data ApplySecurityGroupsToLoadBalancerResponse = ApplySecurityGroupsToLoadBalancerResponse
    { _asgtlboSecurityGroups :: [Text]
      -- ^ A list of security group IDs associated with your load balancer.
    } deriving (Show, Generic)

-- | A list of security group IDs associated with your load balancer.
asgtlboSecurityGroups
    :: Functor f
    => ([Text]
    -> f ([Text]))
    -> ApplySecurityGroupsToLoadBalancerResponse
    -> f ApplySecurityGroupsToLoadBalancerResponse
asgtlboSecurityGroups f x =
    (\y -> x { _asgtlboSecurityGroups = y })
       <$> f (_asgtlboSecurityGroups x)
{-# INLINE asgtlboSecurityGroups #-}

instance FromXML ApplySecurityGroupsToLoadBalancerResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest ApplySecurityGroupsToLoadBalancer where
    type Sv ApplySecurityGroupsToLoadBalancer = ELB
    type Rs ApplySecurityGroupsToLoadBalancer = ApplySecurityGroupsToLoadBalancerResponse

    request = post "ApplySecurityGroupsToLoadBalancer"
    response _ = xmlResponse
