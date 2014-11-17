{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ELB.EnableAvailabilityZonesForLoadBalancer
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
-- Balancing Developer Guide.
module Network.AWS.ELB.EnableAvailabilityZonesForLoadBalancer
    (
    -- * Request
      EnableAvailabilityZonesForLoadBalancer
    -- ** Request constructor
    , enableAvailabilityZonesForLoadBalancer
    -- ** Request lenses
    , eazflbAvailabilityZones
    , eazflbLoadBalancerName

    -- * Response
    , EnableAvailabilityZonesForLoadBalancerResponse
    -- ** Response constructor
    , enableAvailabilityZonesForLoadBalancerResponse
    -- ** Response lenses
    , eazflbrAvailabilityZones
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.ELB.Types
import qualified GHC.Exts

data EnableAvailabilityZonesForLoadBalancer = EnableAvailabilityZonesForLoadBalancer
    { _eazflbAvailabilityZones :: [Text]
    , _eazflbLoadBalancerName  :: Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'EnableAvailabilityZonesForLoadBalancer' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'eazflbAvailabilityZones' @::@ ['Text']
--
-- * 'eazflbLoadBalancerName' @::@ 'Text'
--
enableAvailabilityZonesForLoadBalancer :: Text -- ^ 'eazflbLoadBalancerName'
                                       -> EnableAvailabilityZonesForLoadBalancer
enableAvailabilityZonesForLoadBalancer p1 = EnableAvailabilityZonesForLoadBalancer
    { _eazflbLoadBalancerName  = p1
    , _eazflbAvailabilityZones = mempty
    }

-- | A list of new Availability Zones for the load balancer. Each Availability
-- Zone must be in the same region as the load balancer.
eazflbAvailabilityZones :: Lens' EnableAvailabilityZonesForLoadBalancer [Text]
eazflbAvailabilityZones =
    lens _eazflbAvailabilityZones (\s a -> s { _eazflbAvailabilityZones = a })

-- | The name associated with the load balancer.
eazflbLoadBalancerName :: Lens' EnableAvailabilityZonesForLoadBalancer Text
eazflbLoadBalancerName =
    lens _eazflbLoadBalancerName (\s a -> s { _eazflbLoadBalancerName = a })

newtype EnableAvailabilityZonesForLoadBalancerResponse = EnableAvailabilityZonesForLoadBalancerResponse
    { _eazflbrAvailabilityZones :: [Text]
    } deriving (Eq, Ord, Show, Generic, Monoid, Semigroup)

instance GHC.Exts.IsList EnableAvailabilityZonesForLoadBalancerResponse where
    type Item EnableAvailabilityZonesForLoadBalancerResponse = Text

    fromList = EnableAvailabilityZonesForLoadBalancerResponse . GHC.Exts.fromList
    toList   = GHC.Exts.toList . _eazflbrAvailabilityZones

-- | 'EnableAvailabilityZonesForLoadBalancerResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'eazflbrAvailabilityZones' @::@ ['Text']
--
enableAvailabilityZonesForLoadBalancerResponse :: EnableAvailabilityZonesForLoadBalancerResponse
enableAvailabilityZonesForLoadBalancerResponse = EnableAvailabilityZonesForLoadBalancerResponse
    { _eazflbrAvailabilityZones = mempty
    }

-- | An updated list of Availability Zones for the load balancer.
eazflbrAvailabilityZones :: Lens' EnableAvailabilityZonesForLoadBalancerResponse [Text]
eazflbrAvailabilityZones =
    lens _eazflbrAvailabilityZones
        (\s a -> s { _eazflbrAvailabilityZones = a })

instance AWSRequest EnableAvailabilityZonesForLoadBalancer where
    type Sv EnableAvailabilityZonesForLoadBalancer = ELB
    type Rs EnableAvailabilityZonesForLoadBalancer = EnableAvailabilityZonesForLoadBalancerResponse

    request  = post "EnableAvailabilityZonesForLoadBalancer"
    response = xmlResponse

instance FromXML EnableAvailabilityZonesForLoadBalancerResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "EnableAvailabilityZonesForLoadBalancerResponse"

instance ToPath EnableAvailabilityZonesForLoadBalancer where
    toPath = const "/"

instance ToHeaders EnableAvailabilityZonesForLoadBalancer

instance ToQuery EnableAvailabilityZonesForLoadBalancer
