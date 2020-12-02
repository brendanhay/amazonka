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
-- Module      : Network.AWS.ElastiCache.DescribeReservedCacheNodes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about reserved cache nodes for this account, or about a specified reserved cache node.
--
--
--
-- This operation returns paginated results.
module Network.AWS.ElastiCache.DescribeReservedCacheNodes
  ( -- * Creating a Request
    describeReservedCacheNodes,
    DescribeReservedCacheNodes,

    -- * Request Lenses
    drcnCacheNodeType,
    drcnProductDescription,
    drcnMarker,
    drcnMaxRecords,
    drcnReservedCacheNodeId,
    drcnOfferingType,
    drcnDuration,
    drcnReservedCacheNodesOfferingId,

    -- * Destructuring the Response
    describeReservedCacheNodesResponse,
    DescribeReservedCacheNodesResponse,

    -- * Response Lenses
    drcnrsMarker,
    drcnrsReservedCacheNodes,
    drcnrsResponseStatus,
  )
where

import Network.AWS.ElastiCache.Types
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents the input of a @DescribeReservedCacheNodes@ operation.
--
--
--
-- /See:/ 'describeReservedCacheNodes' smart constructor.
data DescribeReservedCacheNodes = DescribeReservedCacheNodes'
  { _drcnCacheNodeType ::
      !(Maybe Text),
    _drcnProductDescription ::
      !(Maybe Text),
    _drcnMarker :: !(Maybe Text),
    _drcnMaxRecords :: !(Maybe Int),
    _drcnReservedCacheNodeId ::
      !(Maybe Text),
    _drcnOfferingType :: !(Maybe Text),
    _drcnDuration :: !(Maybe Text),
    _drcnReservedCacheNodesOfferingId ::
      !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeReservedCacheNodes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drcnCacheNodeType' - The cache node type filter value. Use this parameter to show only those reservations matching the specified cache node type. The following node types are supported by ElastiCache. Generally speaking, the current generation types provide more memory and computational power at lower cost when compared to their equivalent previous generation counterparts.     * General purpose:     * Current generation:  __M6g node types__ (available only for Redis engine version 5.0.6 onward and for Memcached engine version 1.5.16 onward). @cache.m6g.large@ , @cache.m6g.xlarge@ , @cache.m6g.2xlarge@ , @cache.m6g.4xlarge@ , @cache.m6g.8xlarge@ , @cache.m6g.12xlarge@ , @cache.m6g.16xlarge@  __M5 node types:__ @cache.m5.large@ , @cache.m5.xlarge@ , @cache.m5.2xlarge@ , @cache.m5.4xlarge@ , @cache.m5.12xlarge@ , @cache.m5.24xlarge@  __M4 node types:__ @cache.m4.large@ , @cache.m4.xlarge@ , @cache.m4.2xlarge@ , @cache.m4.4xlarge@ , @cache.m4.10xlarge@  __T3 node types:__ @cache.t3.micro@ , @cache.t3.small@ , @cache.t3.medium@  __T2 node types:__ @cache.t2.micro@ , @cache.t2.small@ , @cache.t2.medium@      * Previous generation: (not recommended) __T1 node types:__ @cache.t1.micro@  __M1 node types:__ @cache.m1.small@ , @cache.m1.medium@ , @cache.m1.large@ , @cache.m1.xlarge@  __M3 node types:__ @cache.m3.medium@ , @cache.m3.large@ , @cache.m3.xlarge@ , @cache.m3.2xlarge@      * Compute optimized:     * Previous generation: (not recommended) __C1 node types:__ @cache.c1.xlarge@      * Memory optimized:     * Current generation:  __R6g node types__ (available only for Redis engine version 5.0.6 onward and for Memcached engine version 1.5.16 onward). @cache.r6g.large@ , @cache.r6g.xlarge@ , @cache.r6g.2xlarge@ , @cache.r6g.4xlarge@ , @cache.r6g.8xlarge@ , @cache.r6g.12xlarge@ , @cache.r6g.16xlarge@  __R5 node types:__ @cache.r5.large@ , @cache.r5.xlarge@ , @cache.r5.2xlarge@ , @cache.r5.4xlarge@ , @cache.r5.12xlarge@ , @cache.r5.24xlarge@  __R4 node types:__ @cache.r4.large@ , @cache.r4.xlarge@ , @cache.r4.2xlarge@ , @cache.r4.4xlarge@ , @cache.r4.8xlarge@ , @cache.r4.16xlarge@      * Previous generation: (not recommended) __M2 node types:__ @cache.m2.xlarge@ , @cache.m2.2xlarge@ , @cache.m2.4xlarge@  __R3 node types:__ @cache.r3.large@ , @cache.r3.xlarge@ , @cache.r3.2xlarge@ , @cache.r3.4xlarge@ , @cache.r3.8xlarge@  __Additional node type info__      * All current generation instance types are created in Amazon VPC by default.     * Redis append-only files (AOF) are not supported for T1 or T2 instances.     * Redis Multi-AZ with automatic failover is not supported on T1 instances.     * Redis configuration variables @appendonly@ and @appendfsync@ are not supported on Redis version 2.8.22 and later.
--
-- * 'drcnProductDescription' - The product description filter value. Use this parameter to show only those reservations matching the specified product description.
--
-- * 'drcnMarker' - An optional marker returned from a prior request. Use this marker for pagination of results from this operation. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- * 'drcnMaxRecords' - The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a marker is included in the response so that the remaining results can be retrieved. Default: 100 Constraints: minimum 20; maximum 100.
--
-- * 'drcnReservedCacheNodeId' - The reserved cache node identifier filter value. Use this parameter to show only the reservation that matches the specified reservation ID.
--
-- * 'drcnOfferingType' - The offering type filter value. Use this parameter to show only the available offerings matching the specified offering type. Valid values: @"Light Utilization"|"Medium Utilization"|"Heavy Utilization"|"All Upfront"|"Partial Upfront"| "No Upfront"@
--
-- * 'drcnDuration' - The duration filter value, specified in years or seconds. Use this parameter to show only reservations for this duration. Valid Values: @1 | 3 | 31536000 | 94608000@
--
-- * 'drcnReservedCacheNodesOfferingId' - The offering identifier filter value. Use this parameter to show only purchased reservations matching the specified offering identifier.
describeReservedCacheNodes ::
  DescribeReservedCacheNodes
describeReservedCacheNodes =
  DescribeReservedCacheNodes'
    { _drcnCacheNodeType = Nothing,
      _drcnProductDescription = Nothing,
      _drcnMarker = Nothing,
      _drcnMaxRecords = Nothing,
      _drcnReservedCacheNodeId = Nothing,
      _drcnOfferingType = Nothing,
      _drcnDuration = Nothing,
      _drcnReservedCacheNodesOfferingId = Nothing
    }

-- | The cache node type filter value. Use this parameter to show only those reservations matching the specified cache node type. The following node types are supported by ElastiCache. Generally speaking, the current generation types provide more memory and computational power at lower cost when compared to their equivalent previous generation counterparts.     * General purpose:     * Current generation:  __M6g node types__ (available only for Redis engine version 5.0.6 onward and for Memcached engine version 1.5.16 onward). @cache.m6g.large@ , @cache.m6g.xlarge@ , @cache.m6g.2xlarge@ , @cache.m6g.4xlarge@ , @cache.m6g.8xlarge@ , @cache.m6g.12xlarge@ , @cache.m6g.16xlarge@  __M5 node types:__ @cache.m5.large@ , @cache.m5.xlarge@ , @cache.m5.2xlarge@ , @cache.m5.4xlarge@ , @cache.m5.12xlarge@ , @cache.m5.24xlarge@  __M4 node types:__ @cache.m4.large@ , @cache.m4.xlarge@ , @cache.m4.2xlarge@ , @cache.m4.4xlarge@ , @cache.m4.10xlarge@  __T3 node types:__ @cache.t3.micro@ , @cache.t3.small@ , @cache.t3.medium@  __T2 node types:__ @cache.t2.micro@ , @cache.t2.small@ , @cache.t2.medium@      * Previous generation: (not recommended) __T1 node types:__ @cache.t1.micro@  __M1 node types:__ @cache.m1.small@ , @cache.m1.medium@ , @cache.m1.large@ , @cache.m1.xlarge@  __M3 node types:__ @cache.m3.medium@ , @cache.m3.large@ , @cache.m3.xlarge@ , @cache.m3.2xlarge@      * Compute optimized:     * Previous generation: (not recommended) __C1 node types:__ @cache.c1.xlarge@      * Memory optimized:     * Current generation:  __R6g node types__ (available only for Redis engine version 5.0.6 onward and for Memcached engine version 1.5.16 onward). @cache.r6g.large@ , @cache.r6g.xlarge@ , @cache.r6g.2xlarge@ , @cache.r6g.4xlarge@ , @cache.r6g.8xlarge@ , @cache.r6g.12xlarge@ , @cache.r6g.16xlarge@  __R5 node types:__ @cache.r5.large@ , @cache.r5.xlarge@ , @cache.r5.2xlarge@ , @cache.r5.4xlarge@ , @cache.r5.12xlarge@ , @cache.r5.24xlarge@  __R4 node types:__ @cache.r4.large@ , @cache.r4.xlarge@ , @cache.r4.2xlarge@ , @cache.r4.4xlarge@ , @cache.r4.8xlarge@ , @cache.r4.16xlarge@      * Previous generation: (not recommended) __M2 node types:__ @cache.m2.xlarge@ , @cache.m2.2xlarge@ , @cache.m2.4xlarge@  __R3 node types:__ @cache.r3.large@ , @cache.r3.xlarge@ , @cache.r3.2xlarge@ , @cache.r3.4xlarge@ , @cache.r3.8xlarge@  __Additional node type info__      * All current generation instance types are created in Amazon VPC by default.     * Redis append-only files (AOF) are not supported for T1 or T2 instances.     * Redis Multi-AZ with automatic failover is not supported on T1 instances.     * Redis configuration variables @appendonly@ and @appendfsync@ are not supported on Redis version 2.8.22 and later.
drcnCacheNodeType :: Lens' DescribeReservedCacheNodes (Maybe Text)
drcnCacheNodeType = lens _drcnCacheNodeType (\s a -> s {_drcnCacheNodeType = a})

-- | The product description filter value. Use this parameter to show only those reservations matching the specified product description.
drcnProductDescription :: Lens' DescribeReservedCacheNodes (Maybe Text)
drcnProductDescription = lens _drcnProductDescription (\s a -> s {_drcnProductDescription = a})

-- | An optional marker returned from a prior request. Use this marker for pagination of results from this operation. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
drcnMarker :: Lens' DescribeReservedCacheNodes (Maybe Text)
drcnMarker = lens _drcnMarker (\s a -> s {_drcnMarker = a})

-- | The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a marker is included in the response so that the remaining results can be retrieved. Default: 100 Constraints: minimum 20; maximum 100.
drcnMaxRecords :: Lens' DescribeReservedCacheNodes (Maybe Int)
drcnMaxRecords = lens _drcnMaxRecords (\s a -> s {_drcnMaxRecords = a})

-- | The reserved cache node identifier filter value. Use this parameter to show only the reservation that matches the specified reservation ID.
drcnReservedCacheNodeId :: Lens' DescribeReservedCacheNodes (Maybe Text)
drcnReservedCacheNodeId = lens _drcnReservedCacheNodeId (\s a -> s {_drcnReservedCacheNodeId = a})

-- | The offering type filter value. Use this parameter to show only the available offerings matching the specified offering type. Valid values: @"Light Utilization"|"Medium Utilization"|"Heavy Utilization"|"All Upfront"|"Partial Upfront"| "No Upfront"@
drcnOfferingType :: Lens' DescribeReservedCacheNodes (Maybe Text)
drcnOfferingType = lens _drcnOfferingType (\s a -> s {_drcnOfferingType = a})

-- | The duration filter value, specified in years or seconds. Use this parameter to show only reservations for this duration. Valid Values: @1 | 3 | 31536000 | 94608000@
drcnDuration :: Lens' DescribeReservedCacheNodes (Maybe Text)
drcnDuration = lens _drcnDuration (\s a -> s {_drcnDuration = a})

-- | The offering identifier filter value. Use this parameter to show only purchased reservations matching the specified offering identifier.
drcnReservedCacheNodesOfferingId :: Lens' DescribeReservedCacheNodes (Maybe Text)
drcnReservedCacheNodesOfferingId = lens _drcnReservedCacheNodesOfferingId (\s a -> s {_drcnReservedCacheNodesOfferingId = a})

instance AWSPager DescribeReservedCacheNodes where
  page rq rs
    | stop (rs ^. drcnrsMarker) = Nothing
    | stop (rs ^. drcnrsReservedCacheNodes) = Nothing
    | otherwise = Just $ rq & drcnMarker .~ rs ^. drcnrsMarker

instance AWSRequest DescribeReservedCacheNodes where
  type
    Rs DescribeReservedCacheNodes =
      DescribeReservedCacheNodesResponse
  request = postQuery elastiCache
  response =
    receiveXMLWrapper
      "DescribeReservedCacheNodesResult"
      ( \s h x ->
          DescribeReservedCacheNodesResponse'
            <$> (x .@? "Marker")
            <*> ( x .@? "ReservedCacheNodes" .!@ mempty
                    >>= may (parseXMLList "ReservedCacheNode")
                )
            <*> (pure (fromEnum s))
      )

instance Hashable DescribeReservedCacheNodes

instance NFData DescribeReservedCacheNodes

instance ToHeaders DescribeReservedCacheNodes where
  toHeaders = const mempty

instance ToPath DescribeReservedCacheNodes where
  toPath = const "/"

instance ToQuery DescribeReservedCacheNodes where
  toQuery DescribeReservedCacheNodes' {..} =
    mconcat
      [ "Action" =: ("DescribeReservedCacheNodes" :: ByteString),
        "Version" =: ("2015-02-02" :: ByteString),
        "CacheNodeType" =: _drcnCacheNodeType,
        "ProductDescription" =: _drcnProductDescription,
        "Marker" =: _drcnMarker,
        "MaxRecords" =: _drcnMaxRecords,
        "ReservedCacheNodeId" =: _drcnReservedCacheNodeId,
        "OfferingType" =: _drcnOfferingType,
        "Duration" =: _drcnDuration,
        "ReservedCacheNodesOfferingId"
          =: _drcnReservedCacheNodesOfferingId
      ]

-- | Represents the output of a @DescribeReservedCacheNodes@ operation.
--
--
--
-- /See:/ 'describeReservedCacheNodesResponse' smart constructor.
data DescribeReservedCacheNodesResponse = DescribeReservedCacheNodesResponse'
  { _drcnrsMarker ::
      !(Maybe Text),
    _drcnrsReservedCacheNodes ::
      !( Maybe
           [ReservedCacheNode]
       ),
    _drcnrsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeReservedCacheNodesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drcnrsMarker' - Provides an identifier to allow retrieval of paginated results.
--
-- * 'drcnrsReservedCacheNodes' - A list of reserved cache nodes. Each element in the list contains detailed information about one node.
--
-- * 'drcnrsResponseStatus' - -- | The response status code.
describeReservedCacheNodesResponse ::
  -- | 'drcnrsResponseStatus'
  Int ->
  DescribeReservedCacheNodesResponse
describeReservedCacheNodesResponse pResponseStatus_ =
  DescribeReservedCacheNodesResponse'
    { _drcnrsMarker = Nothing,
      _drcnrsReservedCacheNodes = Nothing,
      _drcnrsResponseStatus = pResponseStatus_
    }

-- | Provides an identifier to allow retrieval of paginated results.
drcnrsMarker :: Lens' DescribeReservedCacheNodesResponse (Maybe Text)
drcnrsMarker = lens _drcnrsMarker (\s a -> s {_drcnrsMarker = a})

-- | A list of reserved cache nodes. Each element in the list contains detailed information about one node.
drcnrsReservedCacheNodes :: Lens' DescribeReservedCacheNodesResponse [ReservedCacheNode]
drcnrsReservedCacheNodes = lens _drcnrsReservedCacheNodes (\s a -> s {_drcnrsReservedCacheNodes = a}) . _Default . _Coerce

-- | -- | The response status code.
drcnrsResponseStatus :: Lens' DescribeReservedCacheNodesResponse Int
drcnrsResponseStatus = lens _drcnrsResponseStatus (\s a -> s {_drcnrsResponseStatus = a})

instance NFData DescribeReservedCacheNodesResponse
