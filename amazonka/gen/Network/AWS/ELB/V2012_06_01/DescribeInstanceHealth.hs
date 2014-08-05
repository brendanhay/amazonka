{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.ELB.V2012_06_01.DescribeInstanceHealth
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns the current state of the specified instances registered with the
-- specified load balancer. If no instances are specified, the state of all
-- the instances registered with the load balancer is returned. You must
-- provide the same account credentials as those that were used to create the
-- load balancer. Description of a healthy (InService) instance
-- https://elasticloadbalancing.amazonaws.com/?LoadBalancerName=my-test-loadbalancer
-- &Version=2012-06-01 &Action=DescribeInstanceHealth &AUTHPARAMS N/A
-- i-90d8c2a5 InService N/A 1549581b-12b7-11e3-895e-1334aEXAMPLE Description
-- of an instance with registration in progress
-- https://elasticloadbalancing.amazonaws.com/?LoadBalancerName=my-test-loadbalancer
-- &Version=2012-06-01 &Action=DescribeInstanceHealth &AUTHPARAMS Instance
-- registration is still in progress. i-315b7e51 OutOfService ELB
-- 1549581b-12b7-11e3-895e-1334aEXAMPLE Description of an unhealthy
-- (OutOfService) instance
-- https://elasticloadbalancing.amazonaws.com/?LoadBalancerName=my-test-loadbalancer
-- &Version=2012-06-01 &Action=DescribeInstanceHealth &AUTHPARAMS Instance has
-- failed at least the UnhealthyThreshold number of health checks
-- consecutively. i-fda142c9 OutOfService Instance
-- 83c88b9d-12b7-11e3-8b82-87b12EXAMPLE Description of an instance in an
-- unknown state
-- https://elasticloadbalancing.amazonaws.com/?LoadBalancerName=my-test-loadbalancer
-- &Version=2012-06-01 &Action=DescribeInstanceHealth &AUTHPARAMS A transient
-- error occurred. Please try again later. i-7f12e649 Unknown ELB
-- 83c88b9d-12b7-11e3-8b82-87b12EXAMPLE.
module Network.AWS.ELB.V2012_06_01.DescribeInstanceHealth where

import Control.Lens.TH (makeLenses)
import Network.AWS.Request.Query
import Network.AWS.ELB.V2012_06_01.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'DescribeInstanceHealth' request.
describeInstanceHealth :: Text -- ^ '_depsiLoadBalancerName'
                       -> DescribeInstanceHealth
describeInstanceHealth p1 = DescribeInstanceHealth
    { _depsiLoadBalancerName = p1
    , _depsiInstances = mempty
    }

data DescribeInstanceHealth = DescribeInstanceHealth
    { _depsiLoadBalancerName :: Text
      -- ^ The name of the load balancer.
    , _depsiInstances :: [Instance]
      -- ^ A list of instance IDs whose states are being queried.
    } deriving (Show, Generic)

makeLenses ''DescribeInstanceHealth

instance ToQuery DescribeInstanceHealth where
    toQuery = genericToQuery def

data DescribeInstanceHealthResponse = DescribeInstanceHealthResponse
    { _depsoInstanceStates :: [InstanceState]
      -- ^ A list containing health information for the specified instances.
    } deriving (Show, Generic)

makeLenses ''DescribeInstanceHealthResponse

instance AWSRequest DescribeInstanceHealth where
    type Sv DescribeInstanceHealth = ELB
    type Rs DescribeInstanceHealth = DescribeInstanceHealthResponse

    request = post "DescribeInstanceHealth"
    response _ = cursorResponse $ \hs xml ->
        pure DescribeInstanceHealthResponse
            <*> xml %| "InstanceStates"
