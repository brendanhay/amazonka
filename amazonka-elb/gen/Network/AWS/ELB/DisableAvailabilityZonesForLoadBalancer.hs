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
      DisableAvailabilityZonesForLoadBalancer
    -- ** Request constructor
    , disableAvailabilityZonesForLoadBalancer
    -- ** Request lenses
    , dazflbAvailabilityZones
    , dazflbLoadBalancerName

    -- * Response
    , DisableAvailabilityZonesForLoadBalancerResponse
    -- ** Response constructor
    , disableAvailabilityZonesForLoadBalancerResponse
    -- ** Response lenses
    , dazflbrAvailabilityZones
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.ELB.Types

data DisableAvailabilityZonesForLoadBalancer = DisableAvailabilityZonesForLoadBalancer
    { _dazflbAvailabilityZones :: [Text]
    , _dazflbLoadBalancerName  :: Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'DisableAvailabilityZonesForLoadBalancer' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dazflbAvailabilityZones' @::@ ['Text']
--
-- * 'dazflbLoadBalancerName' @::@ 'Text'
--
disableAvailabilityZonesForLoadBalancer :: Text -- ^ 'dazflbLoadBalancerName'
                                        -> DisableAvailabilityZonesForLoadBalancer
disableAvailabilityZonesForLoadBalancer p1 = DisableAvailabilityZonesForLoadBalancer
    { _dazflbLoadBalancerName  = p1
    , _dazflbAvailabilityZones = mempty
    }

-- | A list of Availability Zones to be removed from the load balancer. There
-- must be at least one Availability Zone registered with a load balancer at
-- all times. Specified Availability Zones must be in the same region.
dazflbAvailabilityZones :: Lens' DisableAvailabilityZonesForLoadBalancer [Text]
dazflbAvailabilityZones =
    lens _dazflbAvailabilityZones (\s a -> s { _dazflbAvailabilityZones = a })

-- | The name associated with the load balancer.
dazflbLoadBalancerName :: Lens' DisableAvailabilityZonesForLoadBalancer Text
dazflbLoadBalancerName =
    lens _dazflbLoadBalancerName (\s a -> s { _dazflbLoadBalancerName = a })

instance ToQuery DisableAvailabilityZonesForLoadBalancer

instance ToPath DisableAvailabilityZonesForLoadBalancer where
    toPath = const "/"

newtype DisableAvailabilityZonesForLoadBalancerResponse = DisableAvailabilityZonesForLoadBalancerResponse
    { _dazflbrAvailabilityZones :: [Text]
    } deriving (Eq, Ord, Show, Generic, Monoid, Semigroup)

instance IsList DisableAvailabilityZonesForLoadBalancerResponse where
    type Item DisableAvailabilityZonesForLoadBalancerResponse = Text

    fromList = DisableAvailabilityZonesForLoadBalancerResponse . fromList
    toList   = toList . _dazflbrAvailabilityZones

-- | 'DisableAvailabilityZonesForLoadBalancerResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dazflbrAvailabilityZones' @::@ ['Text']
--
disableAvailabilityZonesForLoadBalancerResponse :: DisableAvailabilityZonesForLoadBalancerResponse
disableAvailabilityZonesForLoadBalancerResponse = DisableAvailabilityZonesForLoadBalancerResponse
    { _dazflbrAvailabilityZones = mempty
    }

-- | A list of updated Availability Zones for the load balancer.
dazflbrAvailabilityZones :: Lens' DisableAvailabilityZonesForLoadBalancerResponse [Text]
dazflbrAvailabilityZones =
    lens _dazflbrAvailabilityZones
        (\s a -> s { _dazflbrAvailabilityZones = a })

instance FromXML DisableAvailabilityZonesForLoadBalancerResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DisableAvailabilityZonesForLoadBalancerResponse"

instance AWSRequest DisableAvailabilityZonesForLoadBalancer where
    type Sv DisableAvailabilityZonesForLoadBalancer = ELB
    type Rs DisableAvailabilityZonesForLoadBalancer = DisableAvailabilityZonesForLoadBalancerResponse

    request  = post "DisableAvailabilityZonesForLoadBalancer"
    response = xmlResponse $ \h x -> DisableAvailabilityZonesForLoadBalancerResponse
        <$> x %| "AvailabilityZones"
