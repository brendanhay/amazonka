{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ELB.V2012_06_01.DisableAvailabilityZonesForLoadBalancer
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Removes the specified EC2 Availability Zones from the set of configured
-- Availability Zones for the load balancer. There must be at least one
-- Availability Zone registered with a load balancer at all times. Once an
-- Availability Zone is removed, all the instances registered with the load
-- balancer that are in the removed Availability Zone go into the OutOfService
-- state. Upon Availability Zone removal, the load balancer attempts to
-- equally balance the traffic among its remaining usable Availability Zones.
-- Trying to remove an Availability Zone that was not associated with the load
-- balancer does nothing. For more information, see Disable an Availability
-- Zone from a Load-Balanced Application in the Elastic Load Balancing
-- Developer Guide.
-- https://elasticloadbalancing.amazonaws.com/?AvailabilityZones.member.1=us-east-1a
-- &LoadBalancerName=MyHTTPSLoadBalancer &Version=2012-06-01
-- &Action=DisableAvailabilityZonesForLoadBalancer &AUTHPARAMS us-east-1b
-- ba6267d5-2566-11e3-9c6d-eb728EXAMPLE.
module Network.AWS.ELB.V2012_06_01.DisableAvailabilityZonesForLoadBalancer where

import           Control.Applicative
import           Data.ByteString      (ByteString)
import           Data.Default
import           Data.HashMap.Strict  (HashMap)
import           Data.Monoid
import           Data.Text            (Text)
import qualified Data.Text            as Text
import           GHC.Generics
import           Network.AWS.Data
import           Network.AWS.Response
import           Network.AWS.Types    hiding (Error, Endpoint, Region)
import           Network.AWS.Request.Query
import           Network.AWS.ELB.V2012_06_01.Types
import           Network.HTTP.Client  (RequestBody, Response)
import           Prelude              hiding (head)

data DisableAvailabilityZonesForLoadBalancer = DisableAvailabilityZonesForLoadBalancer
    { _raziLoadBalancerName :: Text
      -- ^ The name associated with the load balancer.
    , _raziAvailabilityZones :: [Text]
      -- ^ A list of Availability Zones to be removed from the load
      -- balancer. There must be at least one Availability Zone registered
      -- with a load balancer at all times. Specified Availability Zones
      -- must be in the same region.
    } deriving (Generic)

instance ToQuery DisableAvailabilityZonesForLoadBalancer where
    toQuery = genericToQuery def

instance AWSRequest DisableAvailabilityZonesForLoadBalancer where
    type Sv DisableAvailabilityZonesForLoadBalancer = ELB
    type Rs DisableAvailabilityZonesForLoadBalancer = DisableAvailabilityZonesForLoadBalancerResponse

    request = post "DisableAvailabilityZonesForLoadBalancer"
    response _ = xmlResponse

data DisableAvailabilityZonesForLoadBalancerResponse = DisableAvailabilityZonesForLoadBalancerResponse
    { _razoAvailabilityZones :: [Text]
      -- ^ A list of updated Availability Zones for the load balancer.
    } deriving (Generic)

instance FromXML DisableAvailabilityZonesForLoadBalancerResponse where
    fromXMLOptions = xmlOptions
