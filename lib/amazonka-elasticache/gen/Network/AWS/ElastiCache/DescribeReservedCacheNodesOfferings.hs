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
-- Module      : Network.AWS.ElastiCache.DescribeReservedCacheNodesOfferings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists available reserved cache node offerings.
--
--
--
-- This operation returns paginated results.
module Network.AWS.ElastiCache.DescribeReservedCacheNodesOfferings
  ( -- * Creating a Request
    describeReservedCacheNodesOfferings,
    DescribeReservedCacheNodesOfferings,

    -- * Request Lenses
    drcnoCacheNodeType,
    drcnoProductDescription,
    drcnoMarker,
    drcnoMaxRecords,
    drcnoOfferingType,
    drcnoDuration,
    drcnoReservedCacheNodesOfferingId,

    -- * Destructuring the Response
    describeReservedCacheNodesOfferingsResponse,
    DescribeReservedCacheNodesOfferingsResponse,

    -- * Response Lenses
    drcnorsMarker,
    drcnorsReservedCacheNodesOfferings,
    drcnorsResponseStatus,
  )
where

import Network.AWS.ElastiCache.Types
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents the input of a @DescribeReservedCacheNodesOfferings@ operation.
--
--
--
-- /See:/ 'describeReservedCacheNodesOfferings' smart constructor.
data DescribeReservedCacheNodesOfferings = DescribeReservedCacheNodesOfferings'
  { _drcnoCacheNodeType ::
      !(Maybe Text),
    _drcnoProductDescription ::
      !(Maybe Text),
    _drcnoMarker ::
      !(Maybe Text),
    _drcnoMaxRecords ::
      !(Maybe Int),
    _drcnoOfferingType ::
      !(Maybe Text),
    _drcnoDuration ::
      !(Maybe Text),
    _drcnoReservedCacheNodesOfferingId ::
      !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeReservedCacheNodesOfferings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drcnoCacheNodeType' - The cache node type filter value. Use this parameter to show only the available offerings matching the specified cache node type. The following node types are supported by ElastiCache. Generally speaking, the current generation types provide more memory and computational power at lower cost when compared to their equivalent previous generation counterparts.     * General purpose:     * Current generation:  __M6g node types__ (available only for Redis engine version 5.0.6 onward and for Memcached engine version 1.5.16 onward). @cache.m6g.large@ , @cache.m6g.xlarge@ , @cache.m6g.2xlarge@ , @cache.m6g.4xlarge@ , @cache.m6g.8xlarge@ , @cache.m6g.12xlarge@ , @cache.m6g.16xlarge@  __M5 node types:__ @cache.m5.large@ , @cache.m5.xlarge@ , @cache.m5.2xlarge@ , @cache.m5.4xlarge@ , @cache.m5.12xlarge@ , @cache.m5.24xlarge@  __M4 node types:__ @cache.m4.large@ , @cache.m4.xlarge@ , @cache.m4.2xlarge@ , @cache.m4.4xlarge@ , @cache.m4.10xlarge@  __T3 node types:__ @cache.t3.micro@ , @cache.t3.small@ , @cache.t3.medium@  __T2 node types:__ @cache.t2.micro@ , @cache.t2.small@ , @cache.t2.medium@      * Previous generation: (not recommended) __T1 node types:__ @cache.t1.micro@  __M1 node types:__ @cache.m1.small@ , @cache.m1.medium@ , @cache.m1.large@ , @cache.m1.xlarge@  __M3 node types:__ @cache.m3.medium@ , @cache.m3.large@ , @cache.m3.xlarge@ , @cache.m3.2xlarge@      * Compute optimized:     * Previous generation: (not recommended) __C1 node types:__ @cache.c1.xlarge@      * Memory optimized:     * Current generation:  __R6g node types__ (available only for Redis engine version 5.0.6 onward and for Memcached engine version 1.5.16 onward). @cache.r6g.large@ , @cache.r6g.xlarge@ , @cache.r6g.2xlarge@ , @cache.r6g.4xlarge@ , @cache.r6g.8xlarge@ , @cache.r6g.12xlarge@ , @cache.r6g.16xlarge@  __R5 node types:__ @cache.r5.large@ , @cache.r5.xlarge@ , @cache.r5.2xlarge@ , @cache.r5.4xlarge@ , @cache.r5.12xlarge@ , @cache.r5.24xlarge@  __R4 node types:__ @cache.r4.large@ , @cache.r4.xlarge@ , @cache.r4.2xlarge@ , @cache.r4.4xlarge@ , @cache.r4.8xlarge@ , @cache.r4.16xlarge@      * Previous generation: (not recommended) __M2 node types:__ @cache.m2.xlarge@ , @cache.m2.2xlarge@ , @cache.m2.4xlarge@  __R3 node types:__ @cache.r3.large@ , @cache.r3.xlarge@ , @cache.r3.2xlarge@ , @cache.r3.4xlarge@ , @cache.r3.8xlarge@  __Additional node type info__      * All current generation instance types are created in Amazon VPC by default.     * Redis append-only files (AOF) are not supported for T1 or T2 instances.     * Redis Multi-AZ with automatic failover is not supported on T1 instances.     * Redis configuration variables @appendonly@ and @appendfsync@ are not supported on Redis version 2.8.22 and later.
--
-- * 'drcnoProductDescription' - The product description filter value. Use this parameter to show only the available offerings matching the specified product description.
--
-- * 'drcnoMarker' - An optional marker returned from a prior request. Use this marker for pagination of results from this operation. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- * 'drcnoMaxRecords' - The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a marker is included in the response so that the remaining results can be retrieved. Default: 100 Constraints: minimum 20; maximum 100.
--
-- * 'drcnoOfferingType' - The offering type filter value. Use this parameter to show only the available offerings matching the specified offering type. Valid Values: @"Light Utilization"|"Medium Utilization"|"Heavy Utilization" |"All Upfront"|"Partial Upfront"| "No Upfront"@
--
-- * 'drcnoDuration' - Duration filter value, specified in years or seconds. Use this parameter to show only reservations for a given duration. Valid Values: @1 | 3 | 31536000 | 94608000@
--
-- * 'drcnoReservedCacheNodesOfferingId' - The offering identifier filter value. Use this parameter to show only the available offering that matches the specified reservation identifier. Example: @438012d3-4052-4cc7-b2e3-8d3372e0e706@
describeReservedCacheNodesOfferings ::
  DescribeReservedCacheNodesOfferings
describeReservedCacheNodesOfferings =
  DescribeReservedCacheNodesOfferings'
    { _drcnoCacheNodeType =
        Nothing,
      _drcnoProductDescription = Nothing,
      _drcnoMarker = Nothing,
      _drcnoMaxRecords = Nothing,
      _drcnoOfferingType = Nothing,
      _drcnoDuration = Nothing,
      _drcnoReservedCacheNodesOfferingId = Nothing
    }

-- | The cache node type filter value. Use this parameter to show only the available offerings matching the specified cache node type. The following node types are supported by ElastiCache. Generally speaking, the current generation types provide more memory and computational power at lower cost when compared to their equivalent previous generation counterparts.     * General purpose:     * Current generation:  __M6g node types__ (available only for Redis engine version 5.0.6 onward and for Memcached engine version 1.5.16 onward). @cache.m6g.large@ , @cache.m6g.xlarge@ , @cache.m6g.2xlarge@ , @cache.m6g.4xlarge@ , @cache.m6g.8xlarge@ , @cache.m6g.12xlarge@ , @cache.m6g.16xlarge@  __M5 node types:__ @cache.m5.large@ , @cache.m5.xlarge@ , @cache.m5.2xlarge@ , @cache.m5.4xlarge@ , @cache.m5.12xlarge@ , @cache.m5.24xlarge@  __M4 node types:__ @cache.m4.large@ , @cache.m4.xlarge@ , @cache.m4.2xlarge@ , @cache.m4.4xlarge@ , @cache.m4.10xlarge@  __T3 node types:__ @cache.t3.micro@ , @cache.t3.small@ , @cache.t3.medium@  __T2 node types:__ @cache.t2.micro@ , @cache.t2.small@ , @cache.t2.medium@      * Previous generation: (not recommended) __T1 node types:__ @cache.t1.micro@  __M1 node types:__ @cache.m1.small@ , @cache.m1.medium@ , @cache.m1.large@ , @cache.m1.xlarge@  __M3 node types:__ @cache.m3.medium@ , @cache.m3.large@ , @cache.m3.xlarge@ , @cache.m3.2xlarge@      * Compute optimized:     * Previous generation: (not recommended) __C1 node types:__ @cache.c1.xlarge@      * Memory optimized:     * Current generation:  __R6g node types__ (available only for Redis engine version 5.0.6 onward and for Memcached engine version 1.5.16 onward). @cache.r6g.large@ , @cache.r6g.xlarge@ , @cache.r6g.2xlarge@ , @cache.r6g.4xlarge@ , @cache.r6g.8xlarge@ , @cache.r6g.12xlarge@ , @cache.r6g.16xlarge@  __R5 node types:__ @cache.r5.large@ , @cache.r5.xlarge@ , @cache.r5.2xlarge@ , @cache.r5.4xlarge@ , @cache.r5.12xlarge@ , @cache.r5.24xlarge@  __R4 node types:__ @cache.r4.large@ , @cache.r4.xlarge@ , @cache.r4.2xlarge@ , @cache.r4.4xlarge@ , @cache.r4.8xlarge@ , @cache.r4.16xlarge@      * Previous generation: (not recommended) __M2 node types:__ @cache.m2.xlarge@ , @cache.m2.2xlarge@ , @cache.m2.4xlarge@  __R3 node types:__ @cache.r3.large@ , @cache.r3.xlarge@ , @cache.r3.2xlarge@ , @cache.r3.4xlarge@ , @cache.r3.8xlarge@  __Additional node type info__      * All current generation instance types are created in Amazon VPC by default.     * Redis append-only files (AOF) are not supported for T1 or T2 instances.     * Redis Multi-AZ with automatic failover is not supported on T1 instances.     * Redis configuration variables @appendonly@ and @appendfsync@ are not supported on Redis version 2.8.22 and later.
drcnoCacheNodeType :: Lens' DescribeReservedCacheNodesOfferings (Maybe Text)
drcnoCacheNodeType = lens _drcnoCacheNodeType (\s a -> s {_drcnoCacheNodeType = a})

-- | The product description filter value. Use this parameter to show only the available offerings matching the specified product description.
drcnoProductDescription :: Lens' DescribeReservedCacheNodesOfferings (Maybe Text)
drcnoProductDescription = lens _drcnoProductDescription (\s a -> s {_drcnoProductDescription = a})

-- | An optional marker returned from a prior request. Use this marker for pagination of results from this operation. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
drcnoMarker :: Lens' DescribeReservedCacheNodesOfferings (Maybe Text)
drcnoMarker = lens _drcnoMarker (\s a -> s {_drcnoMarker = a})

-- | The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a marker is included in the response so that the remaining results can be retrieved. Default: 100 Constraints: minimum 20; maximum 100.
drcnoMaxRecords :: Lens' DescribeReservedCacheNodesOfferings (Maybe Int)
drcnoMaxRecords = lens _drcnoMaxRecords (\s a -> s {_drcnoMaxRecords = a})

-- | The offering type filter value. Use this parameter to show only the available offerings matching the specified offering type. Valid Values: @"Light Utilization"|"Medium Utilization"|"Heavy Utilization" |"All Upfront"|"Partial Upfront"| "No Upfront"@
drcnoOfferingType :: Lens' DescribeReservedCacheNodesOfferings (Maybe Text)
drcnoOfferingType = lens _drcnoOfferingType (\s a -> s {_drcnoOfferingType = a})

-- | Duration filter value, specified in years or seconds. Use this parameter to show only reservations for a given duration. Valid Values: @1 | 3 | 31536000 | 94608000@
drcnoDuration :: Lens' DescribeReservedCacheNodesOfferings (Maybe Text)
drcnoDuration = lens _drcnoDuration (\s a -> s {_drcnoDuration = a})

-- | The offering identifier filter value. Use this parameter to show only the available offering that matches the specified reservation identifier. Example: @438012d3-4052-4cc7-b2e3-8d3372e0e706@
drcnoReservedCacheNodesOfferingId :: Lens' DescribeReservedCacheNodesOfferings (Maybe Text)
drcnoReservedCacheNodesOfferingId = lens _drcnoReservedCacheNodesOfferingId (\s a -> s {_drcnoReservedCacheNodesOfferingId = a})

instance AWSPager DescribeReservedCacheNodesOfferings where
  page rq rs
    | stop (rs ^. drcnorsMarker) = Nothing
    | stop (rs ^. drcnorsReservedCacheNodesOfferings) = Nothing
    | otherwise = Just $ rq & drcnoMarker .~ rs ^. drcnorsMarker

instance AWSRequest DescribeReservedCacheNodesOfferings where
  type
    Rs DescribeReservedCacheNodesOfferings =
      DescribeReservedCacheNodesOfferingsResponse
  request = postQuery elastiCache
  response =
    receiveXMLWrapper
      "DescribeReservedCacheNodesOfferingsResult"
      ( \s h x ->
          DescribeReservedCacheNodesOfferingsResponse'
            <$> (x .@? "Marker")
            <*> ( x .@? "ReservedCacheNodesOfferings" .!@ mempty
                    >>= may (parseXMLList "ReservedCacheNodesOffering")
                )
            <*> (pure (fromEnum s))
      )

instance Hashable DescribeReservedCacheNodesOfferings

instance NFData DescribeReservedCacheNodesOfferings

instance ToHeaders DescribeReservedCacheNodesOfferings where
  toHeaders = const mempty

instance ToPath DescribeReservedCacheNodesOfferings where
  toPath = const "/"

instance ToQuery DescribeReservedCacheNodesOfferings where
  toQuery DescribeReservedCacheNodesOfferings' {..} =
    mconcat
      [ "Action" =: ("DescribeReservedCacheNodesOfferings" :: ByteString),
        "Version" =: ("2015-02-02" :: ByteString),
        "CacheNodeType" =: _drcnoCacheNodeType,
        "ProductDescription" =: _drcnoProductDescription,
        "Marker" =: _drcnoMarker,
        "MaxRecords" =: _drcnoMaxRecords,
        "OfferingType" =: _drcnoOfferingType,
        "Duration" =: _drcnoDuration,
        "ReservedCacheNodesOfferingId"
          =: _drcnoReservedCacheNodesOfferingId
      ]

-- | Represents the output of a @DescribeReservedCacheNodesOfferings@ operation.
--
--
--
-- /See:/ 'describeReservedCacheNodesOfferingsResponse' smart constructor.
data DescribeReservedCacheNodesOfferingsResponse = DescribeReservedCacheNodesOfferingsResponse'
  { _drcnorsMarker ::
      !( Maybe
           Text
       ),
    _drcnorsReservedCacheNodesOfferings ::
      !( Maybe
           [ReservedCacheNodesOffering]
       ),
    _drcnorsResponseStatus ::
      !Int
  }
  deriving
    ( Eq,
      Read,
      Show,
      Data,
      Typeable,
      Generic
    )

-- | Creates a value of 'DescribeReservedCacheNodesOfferingsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drcnorsMarker' - Provides an identifier to allow retrieval of paginated results.
--
-- * 'drcnorsReservedCacheNodesOfferings' - A list of reserved cache node offerings. Each element in the list contains detailed information about one offering.
--
-- * 'drcnorsResponseStatus' - -- | The response status code.
describeReservedCacheNodesOfferingsResponse ::
  -- | 'drcnorsResponseStatus'
  Int ->
  DescribeReservedCacheNodesOfferingsResponse
describeReservedCacheNodesOfferingsResponse pResponseStatus_ =
  DescribeReservedCacheNodesOfferingsResponse'
    { _drcnorsMarker =
        Nothing,
      _drcnorsReservedCacheNodesOfferings = Nothing,
      _drcnorsResponseStatus = pResponseStatus_
    }

-- | Provides an identifier to allow retrieval of paginated results.
drcnorsMarker :: Lens' DescribeReservedCacheNodesOfferingsResponse (Maybe Text)
drcnorsMarker = lens _drcnorsMarker (\s a -> s {_drcnorsMarker = a})

-- | A list of reserved cache node offerings. Each element in the list contains detailed information about one offering.
drcnorsReservedCacheNodesOfferings :: Lens' DescribeReservedCacheNodesOfferingsResponse [ReservedCacheNodesOffering]
drcnorsReservedCacheNodesOfferings = lens _drcnorsReservedCacheNodesOfferings (\s a -> s {_drcnorsReservedCacheNodesOfferings = a}) . _Default . _Coerce

-- | -- | The response status code.
drcnorsResponseStatus :: Lens' DescribeReservedCacheNodesOfferingsResponse Int
drcnorsResponseStatus = lens _drcnorsResponseStatus (\s a -> s {_drcnorsResponseStatus = a})

instance NFData DescribeReservedCacheNodesOfferingsResponse
