{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.V2014_05_01.DescribeAvailabilityZones
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
module Network.AWS.EC2.V2014_05_01.DescribeAvailabilityZones where

import           Control.Applicative
import           Data.ByteString      (ByteString)
import           Data.Default
import           Data.HashMap.Strict  (HashMap)
import           Data.Maybe
import           Data.Monoid
import           Data.Text            (Text)
import qualified Data.Text            as Text
import           GHC.Generics
import           Network.AWS.Data
import           Network.AWS.Response
import           Network.AWS.Types    hiding (Region, Error)
import           Network.AWS.Request.Query
import           Network.AWS.EC2.V2014_05_01.Types
import           Network.HTTP.Client  (RequestBody, Response)
import           Prelude              hiding (head)

data DescribeAvailabilityZones = DescribeAvailabilityZones
    { _dazrDryRun :: Maybe Bool
      -- ^ 
    , _dazrFilters :: [Filter]
      -- ^ One or more filters. message - Information about the Availability
      -- Zone. region-name - The name of the region for the Availability
      -- Zone (for example, us-east-1). state - The state of the
      -- Availability Zone (available | impaired | unavailable). zone-name
      -- - The name of the Availability Zone (for example, us-east-1a).
    , _dazrZoneNames :: [Text]
      -- ^ The names of one or more Availability Zones.
    } deriving (Generic)

instance ToQuery DescribeAvailabilityZones where
    toQuery = genericToQuery def

instance AWSRequest DescribeAvailabilityZones where
    type Sv DescribeAvailabilityZones = EC2
    type Rs DescribeAvailabilityZones = DescribeAvailabilityZonesResponse

    request = post "DescribeAvailabilityZones"
    response _ = xmlResponse

data DescribeAvailabilityZonesResponse = DescribeAvailabilityZonesResponse
    { _dazsAvailabilityZones :: [AvailabilityZone]
      -- ^ Information about one or more Availability Zones.
    } deriving (Generic)

instance FromXML DescribeAvailabilityZonesResponse where
    fromXMLOptions = xmlOptions
