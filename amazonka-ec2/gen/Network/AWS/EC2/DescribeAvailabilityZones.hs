{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.EC2.DescribeAvailabilityZones
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Describes one or more of the Availability Zones that are available to
-- you. The results include zones only for the region you\'re currently
-- using. If there is an event impacting an Availability Zone, you can use
-- this request to view the state and any provided message for that
-- Availability Zone.
--
-- For more information, see
-- <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/using-regions-availability-zones.html Regions and Availability Zones>
-- in the /Amazon Elastic Compute Cloud User Guide/.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeAvailabilityZones.html>
module Network.AWS.EC2.DescribeAvailabilityZones
    (
    -- * Request
      DescribeAvailabilityZones
    -- ** Request constructor
    , describeAvailabilityZones
    -- ** Request lenses
    , dazZoneNames
    , dazFilters
    , dazDryRun

    -- * Response
    , DescribeAvailabilityZonesResponse
    -- ** Response constructor
    , describeAvailabilityZonesResponse
    -- ** Response lenses
    , dazrAvailabilityZones
    , dazrStatus
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'describeAvailabilityZones' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dazZoneNames'
--
-- * 'dazFilters'
--
-- * 'dazDryRun'
data DescribeAvailabilityZones = DescribeAvailabilityZones'
    { _dazZoneNames :: !(Maybe [Text])
    , _dazFilters   :: !(Maybe [Filter])
    , _dazDryRun    :: !(Maybe Bool)
    } deriving (Eq,Read,Show)

-- | 'DescribeAvailabilityZones' smart constructor.
describeAvailabilityZones :: DescribeAvailabilityZones
describeAvailabilityZones =
    DescribeAvailabilityZones'
    { _dazZoneNames = Nothing
    , _dazFilters = Nothing
    , _dazDryRun = Nothing
    }

-- | The names of one or more Availability Zones.
dazZoneNames :: Lens' DescribeAvailabilityZones [Text]
dazZoneNames = lens _dazZoneNames (\ s a -> s{_dazZoneNames = a}) . _Default;

-- | One or more filters.
--
-- -   @message@ - Information about the Availability Zone.
--
-- -   @region-name@ - The name of the region for the Availability Zone
--     (for example, @us-east-1@).
--
-- -   @state@ - The state of the Availability Zone (@available@ |
--     @impaired@ | @unavailable@).
--
-- -   @zone-name@ - The name of the Availability Zone (for example,
--     @us-east-1a@).
--
dazFilters :: Lens' DescribeAvailabilityZones [Filter]
dazFilters = lens _dazFilters (\ s a -> s{_dazFilters = a}) . _Default;

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
dazDryRun :: Lens' DescribeAvailabilityZones (Maybe Bool)
dazDryRun = lens _dazDryRun (\ s a -> s{_dazDryRun = a});

instance AWSRequest DescribeAvailabilityZones where
        type Sv DescribeAvailabilityZones = EC2
        type Rs DescribeAvailabilityZones =
             DescribeAvailabilityZonesResponse
        request = post
        response
          = receiveXML
              (\ s h x ->
                 DescribeAvailabilityZonesResponse' <$>
                   (may (parseXMLList "item") x) <*>
                     (pure (fromEnum s)))

instance ToHeaders DescribeAvailabilityZones where
        toHeaders = const mempty

instance ToPath DescribeAvailabilityZones where
        toPath = const "/"

instance ToQuery DescribeAvailabilityZones where
        toQuery DescribeAvailabilityZones'{..}
          = mconcat
              ["Action" =:
                 ("DescribeAvailabilityZones" :: ByteString),
               "Version" =: ("2015-04-15" :: ByteString),
               toQuery (toQueryList "ZoneName" <$> _dazZoneNames),
               toQuery (toQueryList "Filter" <$> _dazFilters),
               "DryRun" =: _dazDryRun]

-- | /See:/ 'describeAvailabilityZonesResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dazrAvailabilityZones'
--
-- * 'dazrStatus'
data DescribeAvailabilityZonesResponse = DescribeAvailabilityZonesResponse'
    { _dazrAvailabilityZones :: !(Maybe [AvailabilityZone])
    , _dazrStatus            :: !Int
    } deriving (Eq,Read,Show)

-- | 'DescribeAvailabilityZonesResponse' smart constructor.
describeAvailabilityZonesResponse :: Int -> DescribeAvailabilityZonesResponse
describeAvailabilityZonesResponse pStatus =
    DescribeAvailabilityZonesResponse'
    { _dazrAvailabilityZones = Nothing
    , _dazrStatus = pStatus
    }

-- | Information about one or more Availability Zones.
dazrAvailabilityZones :: Lens' DescribeAvailabilityZonesResponse [AvailabilityZone]
dazrAvailabilityZones = lens _dazrAvailabilityZones (\ s a -> s{_dazrAvailabilityZones = a}) . _Default;

-- | FIXME: Undocumented member.
dazrStatus :: Lens' DescribeAvailabilityZonesResponse Int
dazrStatus = lens _dazrStatus (\ s a -> s{_dazrStatus = a});
