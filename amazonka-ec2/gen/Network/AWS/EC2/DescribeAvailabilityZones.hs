{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE StandaloneDeriving          #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.DescribeAvailabilityZones
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
module Network.AWS.EC2.DescribeAvailabilityZones
    (
    -- * Request
      DescribeAvailabilityZones
    -- ** Request constructor
    , describeAvailabilityZones
    -- ** Request lenses
    , dazZoneName
    , dazFilter

    -- * Response
    , DescribeAvailabilityZonesResponse
    -- ** Response constructor
    , describeAvailabilityZonesResponse
    -- ** Response lenses
    , dazrItem
    ) where

import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import Network.AWS.Prelude

data DescribeAvailabilityZones = DescribeAvailabilityZones
    { _dazZoneName :: [Text]
    , _dazFilter :: [Filter]
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeAvailabilityZones' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @ZoneName ::@ @[Text]@
--
-- * @Filter ::@ @[Filter]@
--
describeAvailabilityZones :: DescribeAvailabilityZones
describeAvailabilityZones = DescribeAvailabilityZones
    { _dazZoneName = mempty
    , _dazFilter = mempty
    }

-- | The names of one or more Availability Zones.
dazZoneName :: Lens' DescribeAvailabilityZones [Text]
dazZoneName = lens _dazZoneName (\s a -> s { _dazZoneName = a })

-- | One or more filters. message - Information about the Availability Zone.
-- region-name - The name of the region for the Availability Zone (for
-- example, us-east-1). state - The state of the Availability Zone (available
-- | impaired | unavailable). zone-name - The name of the Availability Zone
-- (for example, us-east-1a).
dazFilter :: Lens' DescribeAvailabilityZones [Filter]
dazFilter = lens _dazFilter (\s a -> s { _dazFilter = a })

instance ToQuery DescribeAvailabilityZones where
    toQuery = genericQuery def

newtype DescribeAvailabilityZonesResponse = DescribeAvailabilityZonesResponse
    { _dazrItem :: [AvailabilityZone]
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeAvailabilityZonesResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Item ::@ @[AvailabilityZone]@
--
describeAvailabilityZonesResponse :: DescribeAvailabilityZonesResponse
describeAvailabilityZonesResponse = DescribeAvailabilityZonesResponse
    { _dazrItem = mempty
    }

-- | Information about one or more Availability Zones.
dazrItem :: Lens' DescribeAvailabilityZonesResponse [AvailabilityZone]
dazrItem = lens _dazrItem (\s a -> s { _dazrItem = a })

instance FromXML DescribeAvailabilityZonesResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeAvailabilityZones where
    type Sv DescribeAvailabilityZones = EC2
    type Rs DescribeAvailabilityZones = DescribeAvailabilityZonesResponse

    request = post "DescribeAvailabilityZones"
    response _ = xmlResponse
