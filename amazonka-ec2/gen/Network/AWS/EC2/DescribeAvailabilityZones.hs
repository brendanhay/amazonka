{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeAvailabilityZones
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more of the Availability Zones that are available to
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
    , dazrqZoneNames
    , dazrqFilters
    , dazrqDryRun

    -- * Response
    , DescribeAvailabilityZonesResponse
    -- ** Response constructor
    , describeAvailabilityZonesResponse
    -- ** Response lenses
    , dazrsAvailabilityZones
    , dazrsStatus
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'describeAvailabilityZones' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dazrqZoneNames'
--
-- * 'dazrqFilters'
--
-- * 'dazrqDryRun'
data DescribeAvailabilityZones = DescribeAvailabilityZones'
    { _dazrqZoneNames :: !(Maybe [Text])
    , _dazrqFilters   :: !(Maybe [Filter])
    , _dazrqDryRun    :: !(Maybe Bool)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeAvailabilityZones' smart constructor.
describeAvailabilityZones :: DescribeAvailabilityZones
describeAvailabilityZones =
    DescribeAvailabilityZones'
    { _dazrqZoneNames = Nothing
    , _dazrqFilters = Nothing
    , _dazrqDryRun = Nothing
    }

-- | The names of one or more Availability Zones.
dazrqZoneNames :: Lens' DescribeAvailabilityZones [Text]
dazrqZoneNames = lens _dazrqZoneNames (\ s a -> s{_dazrqZoneNames = a}) . _Default;

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
dazrqFilters :: Lens' DescribeAvailabilityZones [Filter]
dazrqFilters = lens _dazrqFilters (\ s a -> s{_dazrqFilters = a}) . _Default;

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
dazrqDryRun :: Lens' DescribeAvailabilityZones (Maybe Bool)
dazrqDryRun = lens _dazrqDryRun (\ s a -> s{_dazrqDryRun = a});

instance AWSRequest DescribeAvailabilityZones where
        type Sv DescribeAvailabilityZones = EC2
        type Rs DescribeAvailabilityZones =
             DescribeAvailabilityZonesResponse
        request = post
        response
          = receiveXML
              (\ s h x ->
                 DescribeAvailabilityZonesResponse' <$>
                   (x .@? "availabilityZoneInfo" .!@ mempty >>=
                      may (parseXMLList "item"))
                     <*> (pure (fromEnum s)))

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
               toQuery (toQueryList "ZoneName" <$> _dazrqZoneNames),
               toQuery (toQueryList "Filter" <$> _dazrqFilters),
               "DryRun" =: _dazrqDryRun]

-- | /See:/ 'describeAvailabilityZonesResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dazrsAvailabilityZones'
--
-- * 'dazrsStatus'
data DescribeAvailabilityZonesResponse = DescribeAvailabilityZonesResponse'
    { _dazrsAvailabilityZones :: !(Maybe [AvailabilityZone])
    , _dazrsStatus            :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeAvailabilityZonesResponse' smart constructor.
describeAvailabilityZonesResponse :: Int -> DescribeAvailabilityZonesResponse
describeAvailabilityZonesResponse pStatus_ =
    DescribeAvailabilityZonesResponse'
    { _dazrsAvailabilityZones = Nothing
    , _dazrsStatus = pStatus_
    }

-- | Information about one or more Availability Zones.
dazrsAvailabilityZones :: Lens' DescribeAvailabilityZonesResponse [AvailabilityZone]
dazrsAvailabilityZones = lens _dazrsAvailabilityZones (\ s a -> s{_dazrsAvailabilityZones = a}) . _Default;

-- | FIXME: Undocumented member.
dazrsStatus :: Lens' DescribeAvailabilityZonesResponse Int
dazrsStatus = lens _dazrsStatus (\ s a -> s{_dazrsStatus = a});
