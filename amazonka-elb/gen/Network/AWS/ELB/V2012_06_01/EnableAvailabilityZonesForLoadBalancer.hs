{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TemplateHaskell             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ELB.V2012_06_01.EnableAvailabilityZonesForLoadBalancer
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Adds one or more EC2 Availability Zones to the load balancer. The load
-- balancer evenly distributes requests across all its registered Availability
-- Zones that contain instances. The new EC2 Availability Zones to be added
-- must be in the same EC2 Region as the Availability Zones for which the load
-- balancer was created. For more information, see Expand a Load Balanced
-- Application to an Additional Availability Zone in the Elastic Load
-- Balancing Developer Guide. Enable Availability Zone us-east-1c for
-- my-test-loadbalancer.
-- https://elasticloadbalancing.amazonaws.com/?AvailabilityZones.member.1=us-east-1c
-- &LoadBalancerName=my-test-loadbalancer &Version=2012-06-01
-- &Action=EnableAvailabilityZonesForLoadBalancer &AUTHPARAMS us-east-1a
-- us-east-1c 83c88b9d-12b7-11e3-8b82-87b12EXAMPLE.
module Network.AWS.ELB.V2012_06_01.EnableAvailabilityZonesForLoadBalancer where

import Control.Lens.TH (makeLenses)
import Network.AWS.Request.Query
import Network.AWS.ELB.V2012_06_01.Types
import Network.AWS.Prelude

data EnableAvailabilityZonesForLoadBalancer = EnableAvailabilityZonesForLoadBalancer
    { _aaziLoadBalancerName :: Text
      -- ^ The name associated with the load balancer.
    , _aaziAvailabilityZones :: [Text]
      -- ^ A list of new Availability Zones for the load balancer. Each
      -- Availability Zone must be in the same region as the load
      -- balancer.
    } deriving (Show, Generic)

makeLenses ''EnableAvailabilityZonesForLoadBalancer

instance ToQuery EnableAvailabilityZonesForLoadBalancer where
    toQuery = genericQuery def

data EnableAvailabilityZonesForLoadBalancerResponse = EnableAvailabilityZonesForLoadBalancerResponse
    { _aazoAvailabilityZones :: [Text]
      -- ^ An updated list of Availability Zones for the load balancer.
    } deriving (Show, Generic)

makeLenses ''EnableAvailabilityZonesForLoadBalancerResponse

instance FromXML EnableAvailabilityZonesForLoadBalancerResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest EnableAvailabilityZonesForLoadBalancer where
    type Sv EnableAvailabilityZonesForLoadBalancer = ELB
    type Rs EnableAvailabilityZonesForLoadBalancer = EnableAvailabilityZonesForLoadBalancerResponse

    request = post "EnableAvailabilityZonesForLoadBalancer"
    response _ = xmlResponse
