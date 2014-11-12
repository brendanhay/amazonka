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
-- Compute Cloud User Guide.
module Network.AWS.EC2.DescribeAvailabilityZones
    (
    -- * Request
      DescribeAvailabilityZones
    -- ** Request constructor
    , describeAvailabilityZones
    -- ** Request lenses
    , dazDryRun
    , dazFilters
    , dazZoneNames

    -- * Response
    , DescribeAvailabilityZonesResult
    -- ** Response constructor
    , describeAvailabilityZonesResult
    -- ** Response lenses
    , dazrAvailabilityZones
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.EC2.Types

data DescribeAvailabilityZones = DescribeAvailabilityZones
    { _dazDryRun    :: Maybe Bool
    , _dazFilters   :: [Filter]
    , _dazZoneNames :: [Text]
    } deriving (Eq, Show, Generic)

-- | 'DescribeAvailabilityZones' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dazDryRun' @::@ 'Maybe' 'Bool'
--
-- * 'dazFilters' @::@ ['Filter']
--
-- * 'dazZoneNames' @::@ ['Text']
--
describeAvailabilityZones :: DescribeAvailabilityZones
describeAvailabilityZones = DescribeAvailabilityZones
    { _dazDryRun    = Nothing
    , _dazZoneNames = mempty
    , _dazFilters   = mempty
    }

dazDryRun :: Lens' DescribeAvailabilityZones (Maybe Bool)
dazDryRun = lens _dazDryRun (\s a -> s { _dazDryRun = a })

-- | One or more filters. message - Information about the Availability Zone.
-- region-name - The name of the region for the Availability Zone (for
-- example, us-east-1). state - The state of the Availability Zone
-- (available | impaired | unavailable). zone-name - The name of the
-- Availability Zone (for example, us-east-1a).
dazFilters :: Lens' DescribeAvailabilityZones [Filter]
dazFilters = lens _dazFilters (\s a -> s { _dazFilters = a })

-- | The names of one or more Availability Zones.
dazZoneNames :: Lens' DescribeAvailabilityZones [Text]
dazZoneNames = lens _dazZoneNames (\s a -> s { _dazZoneNames = a })

instance ToQuery DescribeAvailabilityZones

instance ToPath DescribeAvailabilityZones where
    toPath = const "/"

newtype DescribeAvailabilityZonesResult = DescribeAvailabilityZonesResult
    { _dazrAvailabilityZones :: [AvailabilityZone]
    } deriving (Eq, Show, Generic, Monoid, Semigroup)

instance IsList DescribeAvailabilityZonesResult where
    type Item DescribeAvailabilityZonesResult = AvailabilityZone

    fromList = DescribeAvailabilityZonesResult . fromList
    toList   = toList . _dazrAvailabilityZones

-- | 'DescribeAvailabilityZonesResult' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dazrAvailabilityZones' @::@ ['AvailabilityZone']
--
describeAvailabilityZonesResult :: DescribeAvailabilityZonesResult
describeAvailabilityZonesResult = DescribeAvailabilityZonesResult
    { _dazrAvailabilityZones = mempty
    }

-- | Information about one or more Availability Zones.
dazrAvailabilityZones :: Lens' DescribeAvailabilityZonesResult [AvailabilityZone]
dazrAvailabilityZones =
    lens _dazrAvailabilityZones (\s a -> s { _dazrAvailabilityZones = a })

instance FromXML DescribeAvailabilityZonesResult where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DescribeAvailabilityZonesResult"

instance AWSRequest DescribeAvailabilityZones where
    type Sv DescribeAvailabilityZones = EC2
    type Rs DescribeAvailabilityZones = DescribeAvailabilityZonesResult

    request  = post "DescribeAvailabilityZones"
    response = xmlResponse $ \h x -> DescribeAvailabilityZonesResult
        <$> x %| "availabilityZoneInfo"
