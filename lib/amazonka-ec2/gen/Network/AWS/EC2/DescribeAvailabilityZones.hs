{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeAvailabilityZones
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the Availability Zones, Local Zones, and Wavelength Zones that are available to you. If there is an event impacting a zone, you can use this request to view the state and any provided messages for that zone.
--
--
-- For more information about Availability Zones, Local Zones, and Wavelength Zones, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/using-regions-availability-zones.html Regions, Zones and Outposts> in the /Amazon Elastic Compute Cloud User Guide/ .
module Network.AWS.EC2.DescribeAvailabilityZones
  ( -- * Creating a Request
    describeAvailabilityZones,
    DescribeAvailabilityZones,

    -- * Request Lenses
    dazZoneNames,
    dazAllAvailabilityZones,
    dazZoneIds,
    dazFilters,
    dazDryRun,

    -- * Destructuring the Response
    describeAvailabilityZonesResponse,
    DescribeAvailabilityZonesResponse,

    -- * Response Lenses
    dazrsAvailabilityZones,
    dazrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeAvailabilityZones' smart constructor.
data DescribeAvailabilityZones = DescribeAvailabilityZones'
  { _dazZoneNames ::
      !(Maybe [Text]),
    _dazAllAvailabilityZones ::
      !(Maybe Bool),
    _dazZoneIds :: !(Maybe [Text]),
    _dazFilters :: !(Maybe [Filter]),
    _dazDryRun :: !(Maybe Bool)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeAvailabilityZones' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dazZoneNames' - The names of the Availability Zones, Local Zones, and Wavelength Zones.
--
-- * 'dazAllAvailabilityZones' - Include all Availability Zones, Local Zones, and Wavelength Zones regardless of your opt-in status. If you do not use this parameter, the results include only the zones for the Regions where you have chosen the option to opt in.
--
-- * 'dazZoneIds' - The IDs of the Availability Zones, Local Zones, and Wavelength Zones.
--
-- * 'dazFilters' - The filters.     * @group-name@ - For Availability Zones, use the Region name. For Local Zones, use the name of the group associated with the Local Zone (for example, @us-west-2-lax-1@ ) For Wavelength Zones, use the name of the group associated with the Wavelength Zone (for example, @us-east-1-wl1-bos-wlz-1@ ).     * @message@ - The Zone message.     * @opt-in-status@ - The opt-in status (@opted-in@ , and @not-opted-in@ | @opt-in-not-required@ ).     * @parent-zoneID@ - The ID of the zone that handles some of the Local Zone and Wavelength Zone control plane operations, such as API calls.     * @parent-zoneName@ - The ID of the zone that handles some of the Local Zone and Wavelength Zone control plane operations, such as API calls.     * @region-name@ - The name of the Region for the Zone (for example, @us-east-1@ ).     * @state@ - The state of the Availability Zone, the Local Zone, or the Wavelength Zone (@available@ | @information@ | @impaired@ | @unavailable@ ).     * @zone-id@ - The ID of the Availability Zone (for example, @use1-az1@ ), the Local Zone (for example, @usw2-lax1-az1@ ), or the Wavelength Zone (for example, @us-east-1-wl1-bos-wlz-1@ ).     * @zone-type@ - The type of zone, for example, @local-zone@ .     * @zone-name@ - The name of the Availability Zone (for example, @us-east-1a@ ), the Local Zone (for example, @us-west-2-lax-1a@ ), or the Wavelength Zone (for example, @us-east-1-wl1-bos-wlz-1@ ).     * @zone-type@ - The type of zone, for example, @local-zone@ .
--
-- * 'dazDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
describeAvailabilityZones ::
  DescribeAvailabilityZones
describeAvailabilityZones =
  DescribeAvailabilityZones'
    { _dazZoneNames = Nothing,
      _dazAllAvailabilityZones = Nothing,
      _dazZoneIds = Nothing,
      _dazFilters = Nothing,
      _dazDryRun = Nothing
    }

-- | The names of the Availability Zones, Local Zones, and Wavelength Zones.
dazZoneNames :: Lens' DescribeAvailabilityZones [Text]
dazZoneNames = lens _dazZoneNames (\s a -> s {_dazZoneNames = a}) . _Default . _Coerce

-- | Include all Availability Zones, Local Zones, and Wavelength Zones regardless of your opt-in status. If you do not use this parameter, the results include only the zones for the Regions where you have chosen the option to opt in.
dazAllAvailabilityZones :: Lens' DescribeAvailabilityZones (Maybe Bool)
dazAllAvailabilityZones = lens _dazAllAvailabilityZones (\s a -> s {_dazAllAvailabilityZones = a})

-- | The IDs of the Availability Zones, Local Zones, and Wavelength Zones.
dazZoneIds :: Lens' DescribeAvailabilityZones [Text]
dazZoneIds = lens _dazZoneIds (\s a -> s {_dazZoneIds = a}) . _Default . _Coerce

-- | The filters.     * @group-name@ - For Availability Zones, use the Region name. For Local Zones, use the name of the group associated with the Local Zone (for example, @us-west-2-lax-1@ ) For Wavelength Zones, use the name of the group associated with the Wavelength Zone (for example, @us-east-1-wl1-bos-wlz-1@ ).     * @message@ - The Zone message.     * @opt-in-status@ - The opt-in status (@opted-in@ , and @not-opted-in@ | @opt-in-not-required@ ).     * @parent-zoneID@ - The ID of the zone that handles some of the Local Zone and Wavelength Zone control plane operations, such as API calls.     * @parent-zoneName@ - The ID of the zone that handles some of the Local Zone and Wavelength Zone control plane operations, such as API calls.     * @region-name@ - The name of the Region for the Zone (for example, @us-east-1@ ).     * @state@ - The state of the Availability Zone, the Local Zone, or the Wavelength Zone (@available@ | @information@ | @impaired@ | @unavailable@ ).     * @zone-id@ - The ID of the Availability Zone (for example, @use1-az1@ ), the Local Zone (for example, @usw2-lax1-az1@ ), or the Wavelength Zone (for example, @us-east-1-wl1-bos-wlz-1@ ).     * @zone-type@ - The type of zone, for example, @local-zone@ .     * @zone-name@ - The name of the Availability Zone (for example, @us-east-1a@ ), the Local Zone (for example, @us-west-2-lax-1a@ ), or the Wavelength Zone (for example, @us-east-1-wl1-bos-wlz-1@ ).     * @zone-type@ - The type of zone, for example, @local-zone@ .
dazFilters :: Lens' DescribeAvailabilityZones [Filter]
dazFilters = lens _dazFilters (\s a -> s {_dazFilters = a}) . _Default . _Coerce

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
dazDryRun :: Lens' DescribeAvailabilityZones (Maybe Bool)
dazDryRun = lens _dazDryRun (\s a -> s {_dazDryRun = a})

instance AWSRequest DescribeAvailabilityZones where
  type
    Rs DescribeAvailabilityZones =
      DescribeAvailabilityZonesResponse
  request = postQuery ec2
  response =
    receiveXML
      ( \s h x ->
          DescribeAvailabilityZonesResponse'
            <$> ( x .@? "availabilityZoneInfo" .!@ mempty
                    >>= may (parseXMLList "item")
                )
            <*> (pure (fromEnum s))
      )

instance Hashable DescribeAvailabilityZones

instance NFData DescribeAvailabilityZones

instance ToHeaders DescribeAvailabilityZones where
  toHeaders = const mempty

instance ToPath DescribeAvailabilityZones where
  toPath = const "/"

instance ToQuery DescribeAvailabilityZones where
  toQuery DescribeAvailabilityZones' {..} =
    mconcat
      [ "Action" =: ("DescribeAvailabilityZones" :: ByteString),
        "Version" =: ("2016-11-15" :: ByteString),
        toQuery (toQueryList "ZoneName" <$> _dazZoneNames),
        "AllAvailabilityZones" =: _dazAllAvailabilityZones,
        toQuery (toQueryList "ZoneId" <$> _dazZoneIds),
        toQuery (toQueryList "Filter" <$> _dazFilters),
        "DryRun" =: _dazDryRun
      ]

-- | /See:/ 'describeAvailabilityZonesResponse' smart constructor.
data DescribeAvailabilityZonesResponse = DescribeAvailabilityZonesResponse'
  { _dazrsAvailabilityZones ::
      !( Maybe
           [AvailabilityZone]
       ),
    _dazrsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeAvailabilityZonesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dazrsAvailabilityZones' - Information about the Availability Zones, Local Zones, and Wavelength Zones.
--
-- * 'dazrsResponseStatus' - -- | The response status code.
describeAvailabilityZonesResponse ::
  -- | 'dazrsResponseStatus'
  Int ->
  DescribeAvailabilityZonesResponse
describeAvailabilityZonesResponse pResponseStatus_ =
  DescribeAvailabilityZonesResponse'
    { _dazrsAvailabilityZones =
        Nothing,
      _dazrsResponseStatus = pResponseStatus_
    }

-- | Information about the Availability Zones, Local Zones, and Wavelength Zones.
dazrsAvailabilityZones :: Lens' DescribeAvailabilityZonesResponse [AvailabilityZone]
dazrsAvailabilityZones = lens _dazrsAvailabilityZones (\s a -> s {_dazrsAvailabilityZones = a}) . _Default . _Coerce

-- | -- | The response status code.
dazrsResponseStatus :: Lens' DescribeAvailabilityZonesResponse Int
dazrsResponseStatus = lens _dazrsResponseStatus (\s a -> s {_dazrsResponseStatus = a})

instance NFData DescribeAvailabilityZonesResponse
