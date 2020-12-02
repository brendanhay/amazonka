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
-- Module      : Network.AWS.RDS.DescribeCustomAvailabilityZones
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about custom Availability Zones (AZs).
--
--
-- A custom AZ is an on-premises AZ that is integrated with a VMware vSphere cluster.
--
-- For more information about RDS on VMware, see the <https://docs.aws.amazon.com/AmazonRDS/latest/RDSonVMwareUserGuide/rds-on-vmware.html /RDS on VMware User Guide./ >
--
--
-- This operation returns paginated results.
module Network.AWS.RDS.DescribeCustomAvailabilityZones
  ( -- * Creating a Request
    describeCustomAvailabilityZones,
    DescribeCustomAvailabilityZones,

    -- * Request Lenses
    dcazFilters,
    dcazCustomAvailabilityZoneId,
    dcazMarker,
    dcazMaxRecords,

    -- * Destructuring the Response
    describeCustomAvailabilityZonesResponse,
    DescribeCustomAvailabilityZonesResponse,

    -- * Response Lenses
    dcazrsCustomAvailabilityZones,
    dcazrsMarker,
    dcazrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.RDS.Types
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeCustomAvailabilityZones' smart constructor.
data DescribeCustomAvailabilityZones = DescribeCustomAvailabilityZones'
  { _dcazFilters ::
      !(Maybe [Filter]),
    _dcazCustomAvailabilityZoneId ::
      !(Maybe Text),
    _dcazMarker ::
      !(Maybe Text),
    _dcazMaxRecords ::
      !(Maybe Int)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeCustomAvailabilityZones' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcazFilters' - A filter that specifies one or more custom AZs to describe.
--
-- * 'dcazCustomAvailabilityZoneId' - The custom AZ identifier. If this parameter is specified, information from only the specific custom AZ is returned.
--
-- * 'dcazMarker' - An optional pagination token provided by a previous @DescribeCustomAvailabilityZones@ request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- * 'dcazMaxRecords' - The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so you can retrieve the remaining results. Default: 100 Constraints: Minimum 20, maximum 100.
describeCustomAvailabilityZones ::
  DescribeCustomAvailabilityZones
describeCustomAvailabilityZones =
  DescribeCustomAvailabilityZones'
    { _dcazFilters = Nothing,
      _dcazCustomAvailabilityZoneId = Nothing,
      _dcazMarker = Nothing,
      _dcazMaxRecords = Nothing
    }

-- | A filter that specifies one or more custom AZs to describe.
dcazFilters :: Lens' DescribeCustomAvailabilityZones [Filter]
dcazFilters = lens _dcazFilters (\s a -> s {_dcazFilters = a}) . _Default . _Coerce

-- | The custom AZ identifier. If this parameter is specified, information from only the specific custom AZ is returned.
dcazCustomAvailabilityZoneId :: Lens' DescribeCustomAvailabilityZones (Maybe Text)
dcazCustomAvailabilityZoneId = lens _dcazCustomAvailabilityZoneId (\s a -> s {_dcazCustomAvailabilityZoneId = a})

-- | An optional pagination token provided by a previous @DescribeCustomAvailabilityZones@ request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
dcazMarker :: Lens' DescribeCustomAvailabilityZones (Maybe Text)
dcazMarker = lens _dcazMarker (\s a -> s {_dcazMarker = a})

-- | The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so you can retrieve the remaining results. Default: 100 Constraints: Minimum 20, maximum 100.
dcazMaxRecords :: Lens' DescribeCustomAvailabilityZones (Maybe Int)
dcazMaxRecords = lens _dcazMaxRecords (\s a -> s {_dcazMaxRecords = a})

instance AWSPager DescribeCustomAvailabilityZones where
  page rq rs
    | stop (rs ^. dcazrsMarker) = Nothing
    | stop (rs ^. dcazrsCustomAvailabilityZones) = Nothing
    | otherwise = Just $ rq & dcazMarker .~ rs ^. dcazrsMarker

instance AWSRequest DescribeCustomAvailabilityZones where
  type
    Rs DescribeCustomAvailabilityZones =
      DescribeCustomAvailabilityZonesResponse
  request = postQuery rds
  response =
    receiveXMLWrapper
      "DescribeCustomAvailabilityZonesResult"
      ( \s h x ->
          DescribeCustomAvailabilityZonesResponse'
            <$> ( x .@? "CustomAvailabilityZones" .!@ mempty
                    >>= may (parseXMLList "CustomAvailabilityZone")
                )
            <*> (x .@? "Marker")
            <*> (pure (fromEnum s))
      )

instance Hashable DescribeCustomAvailabilityZones

instance NFData DescribeCustomAvailabilityZones

instance ToHeaders DescribeCustomAvailabilityZones where
  toHeaders = const mempty

instance ToPath DescribeCustomAvailabilityZones where
  toPath = const "/"

instance ToQuery DescribeCustomAvailabilityZones where
  toQuery DescribeCustomAvailabilityZones' {..} =
    mconcat
      [ "Action" =: ("DescribeCustomAvailabilityZones" :: ByteString),
        "Version" =: ("2014-10-31" :: ByteString),
        "Filters" =: toQuery (toQueryList "Filter" <$> _dcazFilters),
        "CustomAvailabilityZoneId" =: _dcazCustomAvailabilityZoneId,
        "Marker" =: _dcazMarker,
        "MaxRecords" =: _dcazMaxRecords
      ]

-- | /See:/ 'describeCustomAvailabilityZonesResponse' smart constructor.
data DescribeCustomAvailabilityZonesResponse = DescribeCustomAvailabilityZonesResponse'
  { _dcazrsCustomAvailabilityZones ::
      !( Maybe
           [CustomAvailabilityZone]
       ),
    _dcazrsMarker ::
      !( Maybe
           Text
       ),
    _dcazrsResponseStatus ::
      !Int
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeCustomAvailabilityZonesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcazrsCustomAvailabilityZones' - The list of 'CustomAvailabilityZone' objects for the AWS account.
--
-- * 'dcazrsMarker' - An optional pagination token provided by a previous @DescribeCustomAvailabilityZones@ request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- * 'dcazrsResponseStatus' - -- | The response status code.
describeCustomAvailabilityZonesResponse ::
  -- | 'dcazrsResponseStatus'
  Int ->
  DescribeCustomAvailabilityZonesResponse
describeCustomAvailabilityZonesResponse pResponseStatus_ =
  DescribeCustomAvailabilityZonesResponse'
    { _dcazrsCustomAvailabilityZones =
        Nothing,
      _dcazrsMarker = Nothing,
      _dcazrsResponseStatus = pResponseStatus_
    }

-- | The list of 'CustomAvailabilityZone' objects for the AWS account.
dcazrsCustomAvailabilityZones :: Lens' DescribeCustomAvailabilityZonesResponse [CustomAvailabilityZone]
dcazrsCustomAvailabilityZones = lens _dcazrsCustomAvailabilityZones (\s a -> s {_dcazrsCustomAvailabilityZones = a}) . _Default . _Coerce

-- | An optional pagination token provided by a previous @DescribeCustomAvailabilityZones@ request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
dcazrsMarker :: Lens' DescribeCustomAvailabilityZonesResponse (Maybe Text)
dcazrsMarker = lens _dcazrsMarker (\s a -> s {_dcazrsMarker = a})

-- | -- | The response status code.
dcazrsResponseStatus :: Lens' DescribeCustomAvailabilityZonesResponse Int
dcazrsResponseStatus = lens _dcazrsResponseStatus (\s a -> s {_dcazrsResponseStatus = a})

instance NFData DescribeCustomAvailabilityZonesResponse
