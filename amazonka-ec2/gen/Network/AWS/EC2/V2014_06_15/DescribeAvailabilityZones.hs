{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TemplateHaskell             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.V2014_06_15.DescribeAvailabilityZones
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Describes one or more of the Availability Zones that are available to you.
-- The results include zones only for the region you're currently using. If
-- there is an event impacting an Availability Zone, you can use this request
-- to view the state and any provided message for that Availability Zone. For
-- more information, see Regions and Availability Zones in the Amazon Elastic
-- Compute Cloud User Guide. Example This example request describes the
-- Availability Zones that are available to you. The response includes
-- Availability Zones only for the current region.
-- https://ec2.amazonaws.com/?Action=DescribeAvailabilityZones &amp;AUTHPARAMS
-- &lt;DescribeAvailabilityZonesResponse
-- xmlns="http://ec2.amazonaws.com/doc/2013-10-01/"&gt;
-- &lt;requestId&gt;59dbff89-35bd-4eac-99ed-be587EXAMPLE&lt;/requestId&gt;
-- &lt;availabilityZoneInfo&gt; &lt;item&gt;
-- &lt;zoneName&gt;us-east-1a&lt;/zoneName&gt;
-- &lt;zoneState&gt;available&lt;/zoneState&gt;
-- &lt;regionName&gt;us-east-1&lt;/regionName&gt; &lt;messageSet/&gt;
-- &lt;/item&gt; &lt;item&gt; &lt;zoneName&gt;us-east-1b&lt;/zoneName&gt;
-- &lt;zoneState&gt;available&lt;/zoneState&gt;
-- &lt;regionName&gt;us-east-1&lt;/regionName&gt; &lt;messageSet/&gt;
-- &lt;/item&gt; &lt;item&gt; &lt;zoneName&gt;us-east-1c&lt;/zoneName&gt;
-- &lt;zoneState&gt;available&lt;/zoneState&gt;
-- &lt;regionName&gt;us-east-1&lt;/regionName&gt; &lt;messageSet/&gt;
-- &lt;/item&gt; &lt;item&gt; &lt;zoneName&gt;us-east-1d&lt;/zoneName&gt;
-- &lt;zoneState&gt;available&lt;/zoneState&gt;
-- &lt;regionName&gt;us-east-1&lt;/regionName&gt; &lt;messageSet/&gt;
-- &lt;/item&gt; &lt;/availabilityZoneInfo&gt;
-- &lt;/DescribeAvailabilityZonesResponse&gt;.
module Network.AWS.EC2.V2014_06_15.DescribeAvailabilityZones
    (
    -- * Request
      DescribeAvailabilityZones
    -- ** Default constructor
    , describeAvailabilityZones
    -- ** Accessors and lenses
    , _dazrFilters
    , dazrFilters
    , _dazrZoneNames
    , dazrZoneNames

    -- * Response
    , DescribeAvailabilityZonesResponse
    -- ** Accessors and lenses
    , _dazsAvailabilityZones
    , dazsAvailabilityZones
    ) where

import Network.AWS.Request.Query
import Network.AWS.EC2.V2014_06_15.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'DescribeAvailabilityZones' request.
describeAvailabilityZones :: DescribeAvailabilityZones
describeAvailabilityZones = DescribeAvailabilityZones
    { _dazrFilters = mempty
    , _dazrZoneNames = mempty
    }

data DescribeAvailabilityZones = DescribeAvailabilityZones

makeSiglessLenses ''DescribeAvailabilityZones

instance ToQuery DescribeAvailabilityZones where
    toQuery = genericQuery def

data DescribeAvailabilityZonesResponse = DescribeAvailabilityZonesResponse
    { _dazsAvailabilityZones :: [AvailabilityZone]
      -- ^ Information about one or more Availability Zones.
    } deriving (Show, Generic)

makeSiglessLenses ''DescribeAvailabilityZonesResponse

instance FromXML DescribeAvailabilityZonesResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeAvailabilityZones where
    type Sv DescribeAvailabilityZones = EC2
    type Rs DescribeAvailabilityZones = DescribeAvailabilityZonesResponse

    request = post "DescribeAvailabilityZones"
    response _ = xmlResponse

-- | One or more filters. message - Information about the Availability Zone.
-- region-name - The name of the region for the Availability Zone (for
-- example, us-east-1). state - The state of the Availability Zone (available
-- | impaired | unavailable). zone-name - The name of the Availability Zone
-- (for example, us-east-1a).
dazrFilters :: Lens' DescribeAvailabilityZones ([Filter])

-- | The names of one or more Availability Zones.
dazrZoneNames :: Lens' DescribeAvailabilityZones ([Text])

-- | Information about one or more Availability Zones.
dazsAvailabilityZones :: Lens' DescribeAvailabilityZonesResponse ([AvailabilityZone])
