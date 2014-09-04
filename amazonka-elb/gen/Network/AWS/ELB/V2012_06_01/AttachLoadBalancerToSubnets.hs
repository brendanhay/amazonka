{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ELB.V2012_06_01.AttachLoadBalancerToSubnets
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
module Network.AWS.ELB.V2012_06_01.AttachLoadBalancerToSubnets
    (
    -- * Request
      AttachLoadBalancerToSubnets
    -- ** Request constructor
    , attachLoadBalancerToSubnets
    -- ** Request lenses
    , albtsiLoadBalancerName
    , albtsiSubnets

    -- * Response
    , AttachLoadBalancerToSubnetsResponse
    -- ** Response lenses
    , albtsoSubnets
    ) where

import Network.AWS.Request.Query
import Network.AWS.ELB.V2012_06_01.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'AttachLoadBalancerToSubnets' request.
attachLoadBalancerToSubnets :: Text -- ^ 'albtsiLoadBalancerName'
                            -> [Text] -- ^ 'albtsiSubnets'
                            -> AttachLoadBalancerToSubnets
attachLoadBalancerToSubnets p1 p2 = AttachLoadBalancerToSubnets
    { _albtsiLoadBalancerName = p1
    , _albtsiSubnets = p2
    }
{-# INLINE attachLoadBalancerToSubnets #-}

data AttachLoadBalancerToSubnets = AttachLoadBalancerToSubnets
    { _albtsiLoadBalancerName :: Text
      -- ^ The name associated with the load balancer. The name must be
      -- unique within the set of load balancers associated with your AWS
      -- account.
    , _albtsiSubnets :: [Text]
      -- ^ A list of subnet IDs to add for the load balancer. You can add
      -- only one subnet per Availability Zone.
    } deriving (Show, Generic)

-- | The name associated with the load balancer. The name must be unique within
-- the set of load balancers associated with your AWS account.
albtsiLoadBalancerName :: Lens' AttachLoadBalancerToSubnets (Text)
albtsiLoadBalancerName f x =
    f (_albtsiLoadBalancerName x)
        <&> \y -> x { _albtsiLoadBalancerName = y }
{-# INLINE albtsiLoadBalancerName #-}

-- | A list of subnet IDs to add for the load balancer. You can add only one
-- subnet per Availability Zone.
albtsiSubnets :: Lens' AttachLoadBalancerToSubnets ([Text])
albtsiSubnets f x =
    f (_albtsiSubnets x)
        <&> \y -> x { _albtsiSubnets = y }
{-# INLINE albtsiSubnets #-}

instance ToQuery AttachLoadBalancerToSubnets where
    toQuery = genericQuery def

data AttachLoadBalancerToSubnetsResponse = AttachLoadBalancerToSubnetsResponse
    { _albtsoSubnets :: [Text]
      -- ^ A list of subnet IDs attached to the load balancer.
    } deriving (Show, Generic)

-- | A list of subnet IDs attached to the load balancer.
albtsoSubnets :: Lens' AttachLoadBalancerToSubnetsResponse ([Text])
albtsoSubnets f x =
    f (_albtsoSubnets x)
        <&> \y -> x { _albtsoSubnets = y }
{-# INLINE albtsoSubnets #-}

instance FromXML AttachLoadBalancerToSubnetsResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest AttachLoadBalancerToSubnets where
    type Sv AttachLoadBalancerToSubnets = ELB
    type Rs AttachLoadBalancerToSubnets = AttachLoadBalancerToSubnetsResponse

    request = post "AttachLoadBalancerToSubnets"
    response _ = xmlResponse
