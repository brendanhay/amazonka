{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-unused-binds  #-} doesnt work if wall is used
{-# OPTIONS_GHC -w #-}

-- Module      : Network.AWS.ELB.DisableAvailabilityZonesForLoadBalancer
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
module Network.AWS.ELB.DisableAvailabilityZonesForLoadBalancer
    (
    -- * Request
      RemoveAvailabilityZonesInput
    -- ** Request constructor
    , removeAvailabilityZonesInput
    -- ** Request lenses
    , raziAvailabilityZones
    , raziLoadBalancerName

    -- * Response
    , RemoveAvailabilityZonesOutput
    -- ** Response constructor
    , removeAvailabilityZonesOutput
    -- ** Response lenses
    , razoAvailabilityZones
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.ELB.Types

data RemoveAvailabilityZonesInput = RemoveAvailabilityZonesInput
    { _raziAvailabilityZones :: [Text]
    , _raziLoadBalancerName  :: Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'RemoveAvailabilityZonesInput' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'raziAvailabilityZones' @::@ ['Text']
--
-- * 'raziLoadBalancerName' @::@ 'Text'
--
removeAvailabilityZonesInput :: Text -- ^ 'raziLoadBalancerName'
                             -> RemoveAvailabilityZonesInput
removeAvailabilityZonesInput p1 = RemoveAvailabilityZonesInput
    { _raziLoadBalancerName  = p1
    , _raziAvailabilityZones = mempty
    }

-- | A list of Availability Zones to be removed from the load balancer. There
-- must be at least one Availability Zone registered with a load balancer at
-- all times. Specified Availability Zones must be in the same region.
raziAvailabilityZones :: Lens' RemoveAvailabilityZonesInput [Text]
raziAvailabilityZones =
    lens _raziAvailabilityZones (\s a -> s { _raziAvailabilityZones = a })

-- | The name associated with the load balancer.
raziLoadBalancerName :: Lens' RemoveAvailabilityZonesInput Text
raziLoadBalancerName =
    lens _raziLoadBalancerName (\s a -> s { _raziLoadBalancerName = a })
instance ToQuery RemoveAvailabilityZonesInput

instance ToPath RemoveAvailabilityZonesInput where
    toPath = const "/"

newtype RemoveAvailabilityZonesOutput = RemoveAvailabilityZonesOutput
    { _razoAvailabilityZones :: [Text]
    } deriving (Eq, Ord, Show, Generic, Monoid)

-- | 'RemoveAvailabilityZonesOutput' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'razoAvailabilityZones' @::@ ['Text']
--
removeAvailabilityZonesOutput :: RemoveAvailabilityZonesOutput
removeAvailabilityZonesOutput = RemoveAvailabilityZonesOutput
    { _razoAvailabilityZones = mempty
    }

-- | A list of updated Availability Zones for the load balancer.
razoAvailabilityZones :: Lens' RemoveAvailabilityZonesOutput [Text]
razoAvailabilityZones =
    lens _razoAvailabilityZones (\s a -> s { _razoAvailabilityZones = a })
instance FromXML RemoveAvailabilityZonesOutput where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "RemoveAvailabilityZonesOutput"

instance AWSRequest RemoveAvailabilityZonesInput where
    type Sv RemoveAvailabilityZonesInput = ELB
    type Rs RemoveAvailabilityZonesInput = RemoveAvailabilityZonesOutput

    request  = post "DisableAvailabilityZonesForLoadBalancer"
    response = xmlResponse $ \h x -> RemoveAvailabilityZonesOutput
        <$> x %| "AvailabilityZones"
