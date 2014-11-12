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
      AddAvailabilityZonesInput
    -- ** Request constructor
    , addAvailabilityZonesInput
    -- ** Request lenses
    , aaziAvailabilityZones
    , aaziLoadBalancerName

    -- * Response
    , AddAvailabilityZonesOutput
    -- ** Response constructor
    , addAvailabilityZonesOutput
    -- ** Response lenses
    , aazoAvailabilityZones
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.ELB.Types

data AddAvailabilityZonesInput = AddAvailabilityZonesInput
    { _aaziAvailabilityZones :: [Text]
    , _aaziLoadBalancerName  :: Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'AddAvailabilityZonesInput' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'aaziAvailabilityZones' @::@ ['Text']
--
-- * 'aaziLoadBalancerName' @::@ 'Text'
--
addAvailabilityZonesInput :: Text -- ^ 'aaziLoadBalancerName'
                          -> AddAvailabilityZonesInput
addAvailabilityZonesInput p1 = AddAvailabilityZonesInput
    { _aaziLoadBalancerName  = p1
    , _aaziAvailabilityZones = mempty
    }

-- | A list of new Availability Zones for the load balancer. Each Availability
-- Zone must be in the same region as the load balancer.
aaziAvailabilityZones :: Lens' AddAvailabilityZonesInput [Text]
aaziAvailabilityZones =
    lens _aaziAvailabilityZones (\s a -> s { _aaziAvailabilityZones = a })

-- | The name associated with the load balancer.
aaziLoadBalancerName :: Lens' AddAvailabilityZonesInput Text
aaziLoadBalancerName =
    lens _aaziLoadBalancerName (\s a -> s { _aaziLoadBalancerName = a })

instance ToQuery AddAvailabilityZonesInput

instance ToPath AddAvailabilityZonesInput where
    toPath = const "/"

newtype AddAvailabilityZonesOutput = AddAvailabilityZonesOutput
    { _aazoAvailabilityZones :: [Text]
    } deriving (Eq, Ord, Show, Generic, Monoid, Semigroup, IsString)

instance IsList AddAvailabilityZonesOutput where
    type Item AddAvailabilityZonesOutput = Text

    fromList = AddAvailabilityZonesOutput . fromList
    toList   = toList . _aazoAvailabilityZones

-- | 'AddAvailabilityZonesOutput' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'aazoAvailabilityZones' @::@ ['Text']
--
addAvailabilityZonesOutput :: AddAvailabilityZonesOutput
addAvailabilityZonesOutput = AddAvailabilityZonesOutput
    { _aazoAvailabilityZones = mempty
    }

-- | An updated list of Availability Zones for the load balancer.
aazoAvailabilityZones :: Lens' AddAvailabilityZonesOutput [Text]
aazoAvailabilityZones =
    lens _aazoAvailabilityZones (\s a -> s { _aazoAvailabilityZones = a })

instance FromXML AddAvailabilityZonesOutput where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "AddAvailabilityZonesOutput"

instance AWSRequest AddAvailabilityZonesInput where
    type Sv AddAvailabilityZonesInput = ELB
    type Rs AddAvailabilityZonesInput = AddAvailabilityZonesOutput

    request  = post "EnableAvailabilityZonesForLoadBalancer"
    response = xmlResponse $ \h x -> AddAvailabilityZonesOutput
        <$> x %| "AvailabilityZones"
