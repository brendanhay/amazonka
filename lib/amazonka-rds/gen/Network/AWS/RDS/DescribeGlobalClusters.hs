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
-- Module      : Network.AWS.RDS.DescribeGlobalClusters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about Aurora global database clusters. This API supports pagination.
--
--
-- For more information on Amazon Aurora, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/CHAP_AuroraOverview.html What Is Amazon Aurora?> in the /Amazon Aurora User Guide./
--
--
-- This operation returns paginated results.
module Network.AWS.RDS.DescribeGlobalClusters
  ( -- * Creating a Request
    describeGlobalClusters,
    DescribeGlobalClusters,

    -- * Request Lenses
    dgcsGlobalClusterIdentifier,
    dgcsFilters,
    dgcsMarker,
    dgcsMaxRecords,

    -- * Destructuring the Response
    describeGlobalClustersResponse,
    DescribeGlobalClustersResponse,

    -- * Response Lenses
    drsGlobalClusters,
    drsMarker,
    drsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.RDS.Types
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeGlobalClusters' smart constructor.
data DescribeGlobalClusters = DescribeGlobalClusters'
  { _dgcsGlobalClusterIdentifier ::
      !(Maybe Text),
    _dgcsFilters :: !(Maybe [Filter]),
    _dgcsMarker :: !(Maybe Text),
    _dgcsMaxRecords :: !(Maybe Int)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeGlobalClusters' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dgcsGlobalClusterIdentifier' - The user-supplied DB cluster identifier. If this parameter is specified, information from only the specific DB cluster is returned. This parameter isn't case-sensitive.  Constraints:     * If supplied, must match an existing DBClusterIdentifier.
--
-- * 'dgcsFilters' - A filter that specifies one or more global DB clusters to describe. Supported filters:     * @db-cluster-id@ - Accepts DB cluster identifiers and DB cluster Amazon Resource Names (ARNs). The results list will only include information about the DB clusters identified by these ARNs.
--
-- * 'dgcsMarker' - An optional pagination token provided by a previous @DescribeGlobalClusters@ request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- * 'dgcsMaxRecords' - The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that you can retrieve the remaining results.  Default: 100 Constraints: Minimum 20, maximum 100.
describeGlobalClusters ::
  DescribeGlobalClusters
describeGlobalClusters =
  DescribeGlobalClusters'
    { _dgcsGlobalClusterIdentifier = Nothing,
      _dgcsFilters = Nothing,
      _dgcsMarker = Nothing,
      _dgcsMaxRecords = Nothing
    }

-- | The user-supplied DB cluster identifier. If this parameter is specified, information from only the specific DB cluster is returned. This parameter isn't case-sensitive.  Constraints:     * If supplied, must match an existing DBClusterIdentifier.
dgcsGlobalClusterIdentifier :: Lens' DescribeGlobalClusters (Maybe Text)
dgcsGlobalClusterIdentifier = lens _dgcsGlobalClusterIdentifier (\s a -> s {_dgcsGlobalClusterIdentifier = a})

-- | A filter that specifies one or more global DB clusters to describe. Supported filters:     * @db-cluster-id@ - Accepts DB cluster identifiers and DB cluster Amazon Resource Names (ARNs). The results list will only include information about the DB clusters identified by these ARNs.
dgcsFilters :: Lens' DescribeGlobalClusters [Filter]
dgcsFilters = lens _dgcsFilters (\s a -> s {_dgcsFilters = a}) . _Default . _Coerce

-- | An optional pagination token provided by a previous @DescribeGlobalClusters@ request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
dgcsMarker :: Lens' DescribeGlobalClusters (Maybe Text)
dgcsMarker = lens _dgcsMarker (\s a -> s {_dgcsMarker = a})

-- | The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that you can retrieve the remaining results.  Default: 100 Constraints: Minimum 20, maximum 100.
dgcsMaxRecords :: Lens' DescribeGlobalClusters (Maybe Int)
dgcsMaxRecords = lens _dgcsMaxRecords (\s a -> s {_dgcsMaxRecords = a})

instance AWSPager DescribeGlobalClusters where
  page rq rs
    | stop (rs ^. drsMarker) = Nothing
    | stop (rs ^. drsGlobalClusters) = Nothing
    | otherwise = Just $ rq & dgcsMarker .~ rs ^. drsMarker

instance AWSRequest DescribeGlobalClusters where
  type Rs DescribeGlobalClusters = DescribeGlobalClustersResponse
  request = postQuery rds
  response =
    receiveXMLWrapper
      "DescribeGlobalClustersResult"
      ( \s h x ->
          DescribeGlobalClustersResponse'
            <$> ( x .@? "GlobalClusters" .!@ mempty
                    >>= may (parseXMLList "GlobalClusterMember")
                )
            <*> (x .@? "Marker")
            <*> (pure (fromEnum s))
      )

instance Hashable DescribeGlobalClusters

instance NFData DescribeGlobalClusters

instance ToHeaders DescribeGlobalClusters where
  toHeaders = const mempty

instance ToPath DescribeGlobalClusters where
  toPath = const "/"

instance ToQuery DescribeGlobalClusters where
  toQuery DescribeGlobalClusters' {..} =
    mconcat
      [ "Action" =: ("DescribeGlobalClusters" :: ByteString),
        "Version" =: ("2014-10-31" :: ByteString),
        "GlobalClusterIdentifier" =: _dgcsGlobalClusterIdentifier,
        "Filters" =: toQuery (toQueryList "Filter" <$> _dgcsFilters),
        "Marker" =: _dgcsMarker,
        "MaxRecords" =: _dgcsMaxRecords
      ]

-- | /See:/ 'describeGlobalClustersResponse' smart constructor.
data DescribeGlobalClustersResponse = DescribeGlobalClustersResponse'
  { _drsGlobalClusters ::
      !(Maybe [GlobalCluster]),
    _drsMarker :: !(Maybe Text),
    _drsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeGlobalClustersResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drsGlobalClusters' - The list of global clusters returned by this request.
--
-- * 'drsMarker' - An optional pagination token provided by a previous @DescribeGlobalClusters@ request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- * 'drsResponseStatus' - -- | The response status code.
describeGlobalClustersResponse ::
  -- | 'drsResponseStatus'
  Int ->
  DescribeGlobalClustersResponse
describeGlobalClustersResponse pResponseStatus_ =
  DescribeGlobalClustersResponse'
    { _drsGlobalClusters = Nothing,
      _drsMarker = Nothing,
      _drsResponseStatus = pResponseStatus_
    }

-- | The list of global clusters returned by this request.
drsGlobalClusters :: Lens' DescribeGlobalClustersResponse [GlobalCluster]
drsGlobalClusters = lens _drsGlobalClusters (\s a -> s {_drsGlobalClusters = a}) . _Default . _Coerce

-- | An optional pagination token provided by a previous @DescribeGlobalClusters@ request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
drsMarker :: Lens' DescribeGlobalClustersResponse (Maybe Text)
drsMarker = lens _drsMarker (\s a -> s {_drsMarker = a})

-- | -- | The response status code.
drsResponseStatus :: Lens' DescribeGlobalClustersResponse Int
drsResponseStatus = lens _drsResponseStatus (\s a -> s {_drsResponseStatus = a})

instance NFData DescribeGlobalClustersResponse
