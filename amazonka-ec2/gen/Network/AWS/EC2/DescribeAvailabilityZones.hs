{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeAvailabilityZones
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
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
-- /See:/ <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeAvailabilityZones.html AWS API Reference> for DescribeAvailabilityZones.
module Network.AWS.EC2.DescribeAvailabilityZones
    (
    -- * Creating a Request
      describeAvailabilityZones
    , DescribeAvailabilityZones
    -- * Request Lenses
    , dazZoneNames
    , dazFilters
    , dazDryRun

    -- * Destructuring the Response
    , describeAvailabilityZonesResponse
    , DescribeAvailabilityZonesResponse
    -- * Response Lenses
    , dazrsAvailabilityZones
    , dazrsStatus
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.EC2.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'describeAvailabilityZones' smart constructor.
data DescribeAvailabilityZones = DescribeAvailabilityZones'
    { _dazZoneNames :: !(Maybe [Text])
    , _dazFilters   :: !(Maybe [Filter])
    , _dazDryRun    :: !(Maybe Bool)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeAvailabilityZones' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dazZoneNames'
--
-- * 'dazFilters'
--
-- * 'dazDryRun'
describeAvailabilityZones
    :: DescribeAvailabilityZones
describeAvailabilityZones =
    DescribeAvailabilityZones'
    { _dazZoneNames = Nothing
    , _dazFilters = Nothing
    , _dazDryRun = Nothing
    }

-- | The names of one or more Availability Zones.
dazZoneNames :: Lens' DescribeAvailabilityZones [Text]
dazZoneNames = lens _dazZoneNames (\ s a -> s{_dazZoneNames = a}) . _Default . _Coerce;

-- | One or more filters.
--
-- -   'message' - Information about the Availability Zone.
--
-- -   'region-name' - The name of the region for the Availability Zone
--     (for example, 'us-east-1').
--
-- -   'state' - The state of the Availability Zone ('available' |
--     'impaired' | 'unavailable').
--
-- -   'zone-name' - The name of the Availability Zone (for example,
--     'us-east-1a').
--
dazFilters :: Lens' DescribeAvailabilityZones [Filter]
dazFilters = lens _dazFilters (\ s a -> s{_dazFilters = a}) . _Default . _Coerce;

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is 'DryRunOperation'.
-- Otherwise, it is 'UnauthorizedOperation'.
dazDryRun :: Lens' DescribeAvailabilityZones (Maybe Bool)
dazDryRun = lens _dazDryRun (\ s a -> s{_dazDryRun = a});

instance AWSRequest DescribeAvailabilityZones where
        type Rs DescribeAvailabilityZones =
             DescribeAvailabilityZonesResponse
        request = postQuery eC2
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
               toQuery (toQueryList "ZoneName" <$> _dazZoneNames),
               toQuery (toQueryList "Filter" <$> _dazFilters),
               "DryRun" =: _dazDryRun]

-- | /See:/ 'describeAvailabilityZonesResponse' smart constructor.
data DescribeAvailabilityZonesResponse = DescribeAvailabilityZonesResponse'
    { _dazrsAvailabilityZones :: !(Maybe [AvailabilityZone])
    , _dazrsStatus            :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeAvailabilityZonesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dazrsAvailabilityZones'
--
-- * 'dazrsStatus'
describeAvailabilityZonesResponse
    :: Int -- ^ 'dazrsStatus'
    -> DescribeAvailabilityZonesResponse
describeAvailabilityZonesResponse pStatus_ =
    DescribeAvailabilityZonesResponse'
    { _dazrsAvailabilityZones = Nothing
    , _dazrsStatus = pStatus_
    }

-- | Information about one or more Availability Zones.
dazrsAvailabilityZones :: Lens' DescribeAvailabilityZonesResponse [AvailabilityZone]
dazrsAvailabilityZones = lens _dazrsAvailabilityZones (\ s a -> s{_dazrsAvailabilityZones = a}) . _Default . _Coerce;

-- | The response status code.
dazrsStatus :: Lens' DescribeAvailabilityZonesResponse Int
dazrsStatus = lens _dazrsStatus (\ s a -> s{_dazrsStatus = a});
