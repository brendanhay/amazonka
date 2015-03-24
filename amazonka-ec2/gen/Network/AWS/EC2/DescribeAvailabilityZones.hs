{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
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
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Describes one or more of the Availability Zones that are available to you.
-- The results include zones only for the region you're currently using. If
-- there is an event impacting an Availability Zone, you can use this request to
-- view the state and any provided message for that Availability Zone.
--
-- For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/using-regions-availability-zones.html Regions and Availability Zones> in the /AmazonElastic Compute Cloud User Guide/.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeAvailabilityZones.html>
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
    , DescribeAvailabilityZonesResponse
    -- ** Response constructor
    , describeAvailabilityZonesResponse
    -- ** Response lenses
    , dazrAvailabilityZones
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import qualified GHC.Exts

data DescribeAvailabilityZones = DescribeAvailabilityZones
    { _dazDryRun    :: Maybe Bool
    , _dazFilters   :: List "Filter" Filter
    , _dazZoneNames :: List "ZoneName" Text
    } deriving (Eq, Read, Show)

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

-- | One or more filters.
--
-- 'message' - Information about the Availability Zone.
--
-- 'region-name' - The name of the region for the Availability Zone (for
-- example, 'us-east-1').
--
-- 'state' - The state of the Availability Zone ('available' | 'impaired' | 'unavailable').
--
-- 'zone-name' - The name of the Availability Zone (for example, 'us-east-1a').
--
--
dazFilters :: Lens' DescribeAvailabilityZones [Filter]
dazFilters = lens _dazFilters (\s a -> s { _dazFilters = a }) . _List

-- | The names of one or more Availability Zones.
dazZoneNames :: Lens' DescribeAvailabilityZones [Text]
dazZoneNames = lens _dazZoneNames (\s a -> s { _dazZoneNames = a }) . _List

newtype DescribeAvailabilityZonesResponse = DescribeAvailabilityZonesResponse
    { _dazrAvailabilityZones :: List "item" AvailabilityZone
    } deriving (Eq, Read, Show, Monoid, Semigroup)

-- | 'DescribeAvailabilityZonesResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dazrAvailabilityZones' @::@ ['AvailabilityZone']
--
describeAvailabilityZonesResponse :: DescribeAvailabilityZonesResponse
describeAvailabilityZonesResponse = DescribeAvailabilityZonesResponse
    { _dazrAvailabilityZones = mempty
    }

-- | Information about one or more Availability Zones.
dazrAvailabilityZones :: Lens' DescribeAvailabilityZonesResponse [AvailabilityZone]
dazrAvailabilityZones =
    lens _dazrAvailabilityZones (\s a -> s { _dazrAvailabilityZones = a })
        . _List

instance ToPath DescribeAvailabilityZones where
    toPath = const "/"

instance ToQuery DescribeAvailabilityZones where
    toQuery DescribeAvailabilityZones{..} = mconcat
        [ "DryRun"   =? _dazDryRun
        , "Filter"   `toQueryList` _dazFilters
        , "ZoneName" `toQueryList` _dazZoneNames
        ]

instance ToHeaders DescribeAvailabilityZones

instance AWSRequest DescribeAvailabilityZones where
    type Sv DescribeAvailabilityZones = EC2
    type Rs DescribeAvailabilityZones = DescribeAvailabilityZonesResponse

    request  = post "DescribeAvailabilityZones"
    response = xmlResponse

instance FromXML DescribeAvailabilityZonesResponse where
    parseXML x = DescribeAvailabilityZonesResponse
        <$> x .@? "availabilityZoneInfo" .!@ mempty
