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
-- Module      : Network.AWS.RDS.DescribeDBClusterEndpoints
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about endpoints for an Amazon Aurora DB cluster.
--
--
--
-- This operation returns paginated results.
module Network.AWS.RDS.DescribeDBClusterEndpoints
  ( -- * Creating a Request
    describeDBClusterEndpoints,
    DescribeDBClusterEndpoints,

    -- * Request Lenses
    ddbceDBClusterIdentifier,
    ddbceFilters,
    ddbceDBClusterEndpointIdentifier,
    ddbceMarker,
    ddbceMaxRecords,

    -- * Destructuring the Response
    describeDBClusterEndpointsResponse,
    DescribeDBClusterEndpointsResponse,

    -- * Response Lenses
    ddcersDBClusterEndpoints,
    ddcersMarker,
    ddcersResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.RDS.Types
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeDBClusterEndpoints' smart constructor.
data DescribeDBClusterEndpoints = DescribeDBClusterEndpoints'
  { _ddbceDBClusterIdentifier ::
      !(Maybe Text),
    _ddbceFilters :: !(Maybe [Filter]),
    _ddbceDBClusterEndpointIdentifier ::
      !(Maybe Text),
    _ddbceMarker :: !(Maybe Text),
    _ddbceMaxRecords :: !(Maybe Int)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeDBClusterEndpoints' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddbceDBClusterIdentifier' - The DB cluster identifier of the DB cluster associated with the endpoint. This parameter is stored as a lowercase string.
--
-- * 'ddbceFilters' - A set of name-value pairs that define which endpoints to include in the output. The filters are specified as name-value pairs, in the format @Name=/endpoint_type/ ,Values=/endpoint_type1/ ,/endpoint_type2/ ,...@ . @Name@ can be one of: @db-cluster-endpoint-type@ , @db-cluster-endpoint-custom-type@ , @db-cluster-endpoint-id@ , @db-cluster-endpoint-status@ . @Values@ for the @db-cluster-endpoint-type@ filter can be one or more of: @reader@ , @writer@ , @custom@ . @Values@ for the @db-cluster-endpoint-custom-type@ filter can be one or more of: @reader@ , @any@ . @Values@ for the @db-cluster-endpoint-status@ filter can be one or more of: @available@ , @creating@ , @deleting@ , @inactive@ , @modifying@ .
--
-- * 'ddbceDBClusterEndpointIdentifier' - The identifier of the endpoint to describe. This parameter is stored as a lowercase string.
--
-- * 'ddbceMarker' - An optional pagination token provided by a previous @DescribeDBClusterEndpoints@ request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- * 'ddbceMaxRecords' - The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so you can retrieve the remaining results.  Default: 100 Constraints: Minimum 20, maximum 100.
describeDBClusterEndpoints ::
  DescribeDBClusterEndpoints
describeDBClusterEndpoints =
  DescribeDBClusterEndpoints'
    { _ddbceDBClusterIdentifier = Nothing,
      _ddbceFilters = Nothing,
      _ddbceDBClusterEndpointIdentifier = Nothing,
      _ddbceMarker = Nothing,
      _ddbceMaxRecords = Nothing
    }

-- | The DB cluster identifier of the DB cluster associated with the endpoint. This parameter is stored as a lowercase string.
ddbceDBClusterIdentifier :: Lens' DescribeDBClusterEndpoints (Maybe Text)
ddbceDBClusterIdentifier = lens _ddbceDBClusterIdentifier (\s a -> s {_ddbceDBClusterIdentifier = a})

-- | A set of name-value pairs that define which endpoints to include in the output. The filters are specified as name-value pairs, in the format @Name=/endpoint_type/ ,Values=/endpoint_type1/ ,/endpoint_type2/ ,...@ . @Name@ can be one of: @db-cluster-endpoint-type@ , @db-cluster-endpoint-custom-type@ , @db-cluster-endpoint-id@ , @db-cluster-endpoint-status@ . @Values@ for the @db-cluster-endpoint-type@ filter can be one or more of: @reader@ , @writer@ , @custom@ . @Values@ for the @db-cluster-endpoint-custom-type@ filter can be one or more of: @reader@ , @any@ . @Values@ for the @db-cluster-endpoint-status@ filter can be one or more of: @available@ , @creating@ , @deleting@ , @inactive@ , @modifying@ .
ddbceFilters :: Lens' DescribeDBClusterEndpoints [Filter]
ddbceFilters = lens _ddbceFilters (\s a -> s {_ddbceFilters = a}) . _Default . _Coerce

-- | The identifier of the endpoint to describe. This parameter is stored as a lowercase string.
ddbceDBClusterEndpointIdentifier :: Lens' DescribeDBClusterEndpoints (Maybe Text)
ddbceDBClusterEndpointIdentifier = lens _ddbceDBClusterEndpointIdentifier (\s a -> s {_ddbceDBClusterEndpointIdentifier = a})

-- | An optional pagination token provided by a previous @DescribeDBClusterEndpoints@ request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
ddbceMarker :: Lens' DescribeDBClusterEndpoints (Maybe Text)
ddbceMarker = lens _ddbceMarker (\s a -> s {_ddbceMarker = a})

-- | The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so you can retrieve the remaining results.  Default: 100 Constraints: Minimum 20, maximum 100.
ddbceMaxRecords :: Lens' DescribeDBClusterEndpoints (Maybe Int)
ddbceMaxRecords = lens _ddbceMaxRecords (\s a -> s {_ddbceMaxRecords = a})

instance AWSPager DescribeDBClusterEndpoints where
  page rq rs
    | stop (rs ^. ddcersMarker) = Nothing
    | stop (rs ^. ddcersDBClusterEndpoints) = Nothing
    | otherwise = Just $ rq & ddbceMarker .~ rs ^. ddcersMarker

instance AWSRequest DescribeDBClusterEndpoints where
  type
    Rs DescribeDBClusterEndpoints =
      DescribeDBClusterEndpointsResponse
  request = postQuery rds
  response =
    receiveXMLWrapper
      "DescribeDBClusterEndpointsResult"
      ( \s h x ->
          DescribeDBClusterEndpointsResponse'
            <$> ( x .@? "DBClusterEndpoints" .!@ mempty
                    >>= may (parseXMLList "DBClusterEndpointList")
                )
            <*> (x .@? "Marker")
            <*> (pure (fromEnum s))
      )

instance Hashable DescribeDBClusterEndpoints

instance NFData DescribeDBClusterEndpoints

instance ToHeaders DescribeDBClusterEndpoints where
  toHeaders = const mempty

instance ToPath DescribeDBClusterEndpoints where
  toPath = const "/"

instance ToQuery DescribeDBClusterEndpoints where
  toQuery DescribeDBClusterEndpoints' {..} =
    mconcat
      [ "Action" =: ("DescribeDBClusterEndpoints" :: ByteString),
        "Version" =: ("2014-10-31" :: ByteString),
        "DBClusterIdentifier" =: _ddbceDBClusterIdentifier,
        "Filters" =: toQuery (toQueryList "Filter" <$> _ddbceFilters),
        "DBClusterEndpointIdentifier" =: _ddbceDBClusterEndpointIdentifier,
        "Marker" =: _ddbceMarker,
        "MaxRecords" =: _ddbceMaxRecords
      ]

-- | /See:/ 'describeDBClusterEndpointsResponse' smart constructor.
data DescribeDBClusterEndpointsResponse = DescribeDBClusterEndpointsResponse'
  { _ddcersDBClusterEndpoints ::
      !( Maybe
           [DBClusterEndpoint]
       ),
    _ddcersMarker ::
      !(Maybe Text),
    _ddcersResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeDBClusterEndpointsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddcersDBClusterEndpoints' - Contains the details of the endpoints associated with the cluster and matching any filter conditions.
--
-- * 'ddcersMarker' - An optional pagination token provided by a previous @DescribeDBClusterEndpoints@ request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- * 'ddcersResponseStatus' - -- | The response status code.
describeDBClusterEndpointsResponse ::
  -- | 'ddcersResponseStatus'
  Int ->
  DescribeDBClusterEndpointsResponse
describeDBClusterEndpointsResponse pResponseStatus_ =
  DescribeDBClusterEndpointsResponse'
    { _ddcersDBClusterEndpoints =
        Nothing,
      _ddcersMarker = Nothing,
      _ddcersResponseStatus = pResponseStatus_
    }

-- | Contains the details of the endpoints associated with the cluster and matching any filter conditions.
ddcersDBClusterEndpoints :: Lens' DescribeDBClusterEndpointsResponse [DBClusterEndpoint]
ddcersDBClusterEndpoints = lens _ddcersDBClusterEndpoints (\s a -> s {_ddcersDBClusterEndpoints = a}) . _Default . _Coerce

-- | An optional pagination token provided by a previous @DescribeDBClusterEndpoints@ request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
ddcersMarker :: Lens' DescribeDBClusterEndpointsResponse (Maybe Text)
ddcersMarker = lens _ddcersMarker (\s a -> s {_ddcersMarker = a})

-- | -- | The response status code.
ddcersResponseStatus :: Lens' DescribeDBClusterEndpointsResponse Int
ddcersResponseStatus = lens _ddcersResponseStatus (\s a -> s {_ddcersResponseStatus = a})

instance NFData DescribeDBClusterEndpointsResponse
