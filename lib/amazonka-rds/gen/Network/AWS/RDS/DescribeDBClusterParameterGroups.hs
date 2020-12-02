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
-- Module      : Network.AWS.RDS.DescribeDBClusterParameterGroups
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of @DBClusterParameterGroup@ descriptions. If a @DBClusterParameterGroupName@ parameter is specified, the list will contain only the description of the specified DB cluster parameter group.
--
--
-- For more information on Amazon Aurora, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/CHAP_AuroraOverview.html What Is Amazon Aurora?> in the /Amazon Aurora User Guide./
--
--
-- This operation returns paginated results.
module Network.AWS.RDS.DescribeDBClusterParameterGroups
  ( -- * Creating a Request
    describeDBClusterParameterGroups,
    DescribeDBClusterParameterGroups,

    -- * Request Lenses
    ddcpgFilters,
    ddcpgMarker,
    ddcpgMaxRecords,
    ddcpgDBClusterParameterGroupName,

    -- * Destructuring the Response
    describeDBClusterParameterGroupsResponse,
    DescribeDBClusterParameterGroupsResponse,

    -- * Response Lenses
    ddcpgrsMarker,
    ddcpgrsDBClusterParameterGroups,
    ddcpgrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.RDS.Types
import Network.AWS.Request
import Network.AWS.Response

-- |
--
--
--
-- /See:/ 'describeDBClusterParameterGroups' smart constructor.
data DescribeDBClusterParameterGroups = DescribeDBClusterParameterGroups'
  { _ddcpgFilters ::
      !(Maybe [Filter]),
    _ddcpgMarker ::
      !(Maybe Text),
    _ddcpgMaxRecords ::
      !(Maybe Int),
    _ddcpgDBClusterParameterGroupName ::
      !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeDBClusterParameterGroups' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddcpgFilters' - This parameter isn't currently supported.
--
-- * 'ddcpgMarker' - An optional pagination token provided by a previous @DescribeDBClusterParameterGroups@ request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- * 'ddcpgMaxRecords' - The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so you can retrieve the remaining results.  Default: 100 Constraints: Minimum 20, maximum 100.
--
-- * 'ddcpgDBClusterParameterGroupName' - The name of a specific DB cluster parameter group to return details for. Constraints:     * If supplied, must match the name of an existing DBClusterParameterGroup.
describeDBClusterParameterGroups ::
  DescribeDBClusterParameterGroups
describeDBClusterParameterGroups =
  DescribeDBClusterParameterGroups'
    { _ddcpgFilters = Nothing,
      _ddcpgMarker = Nothing,
      _ddcpgMaxRecords = Nothing,
      _ddcpgDBClusterParameterGroupName = Nothing
    }

-- | This parameter isn't currently supported.
ddcpgFilters :: Lens' DescribeDBClusterParameterGroups [Filter]
ddcpgFilters = lens _ddcpgFilters (\s a -> s {_ddcpgFilters = a}) . _Default . _Coerce

-- | An optional pagination token provided by a previous @DescribeDBClusterParameterGroups@ request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
ddcpgMarker :: Lens' DescribeDBClusterParameterGroups (Maybe Text)
ddcpgMarker = lens _ddcpgMarker (\s a -> s {_ddcpgMarker = a})

-- | The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so you can retrieve the remaining results.  Default: 100 Constraints: Minimum 20, maximum 100.
ddcpgMaxRecords :: Lens' DescribeDBClusterParameterGroups (Maybe Int)
ddcpgMaxRecords = lens _ddcpgMaxRecords (\s a -> s {_ddcpgMaxRecords = a})

-- | The name of a specific DB cluster parameter group to return details for. Constraints:     * If supplied, must match the name of an existing DBClusterParameterGroup.
ddcpgDBClusterParameterGroupName :: Lens' DescribeDBClusterParameterGroups (Maybe Text)
ddcpgDBClusterParameterGroupName = lens _ddcpgDBClusterParameterGroupName (\s a -> s {_ddcpgDBClusterParameterGroupName = a})

instance AWSPager DescribeDBClusterParameterGroups where
  page rq rs
    | stop (rs ^. ddcpgrsMarker) = Nothing
    | stop (rs ^. ddcpgrsDBClusterParameterGroups) = Nothing
    | otherwise = Just $ rq & ddcpgMarker .~ rs ^. ddcpgrsMarker

instance AWSRequest DescribeDBClusterParameterGroups where
  type
    Rs DescribeDBClusterParameterGroups =
      DescribeDBClusterParameterGroupsResponse
  request = postQuery rds
  response =
    receiveXMLWrapper
      "DescribeDBClusterParameterGroupsResult"
      ( \s h x ->
          DescribeDBClusterParameterGroupsResponse'
            <$> (x .@? "Marker")
            <*> ( x .@? "DBClusterParameterGroups" .!@ mempty
                    >>= may (parseXMLList "DBClusterParameterGroup")
                )
            <*> (pure (fromEnum s))
      )

instance Hashable DescribeDBClusterParameterGroups

instance NFData DescribeDBClusterParameterGroups

instance ToHeaders DescribeDBClusterParameterGroups where
  toHeaders = const mempty

instance ToPath DescribeDBClusterParameterGroups where
  toPath = const "/"

instance ToQuery DescribeDBClusterParameterGroups where
  toQuery DescribeDBClusterParameterGroups' {..} =
    mconcat
      [ "Action" =: ("DescribeDBClusterParameterGroups" :: ByteString),
        "Version" =: ("2014-10-31" :: ByteString),
        "Filters" =: toQuery (toQueryList "Filter" <$> _ddcpgFilters),
        "Marker" =: _ddcpgMarker,
        "MaxRecords" =: _ddcpgMaxRecords,
        "DBClusterParameterGroupName" =: _ddcpgDBClusterParameterGroupName
      ]

-- |
--
--
--
-- /See:/ 'describeDBClusterParameterGroupsResponse' smart constructor.
data DescribeDBClusterParameterGroupsResponse = DescribeDBClusterParameterGroupsResponse'
  { _ddcpgrsMarker ::
      !( Maybe
           Text
       ),
    _ddcpgrsDBClusterParameterGroups ::
      !( Maybe
           [DBClusterParameterGroup]
       ),
    _ddcpgrsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeDBClusterParameterGroupsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddcpgrsMarker' - An optional pagination token provided by a previous @DescribeDBClusterParameterGroups@ request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- * 'ddcpgrsDBClusterParameterGroups' - A list of DB cluster parameter groups.
--
-- * 'ddcpgrsResponseStatus' - -- | The response status code.
describeDBClusterParameterGroupsResponse ::
  -- | 'ddcpgrsResponseStatus'
  Int ->
  DescribeDBClusterParameterGroupsResponse
describeDBClusterParameterGroupsResponse pResponseStatus_ =
  DescribeDBClusterParameterGroupsResponse'
    { _ddcpgrsMarker =
        Nothing,
      _ddcpgrsDBClusterParameterGroups = Nothing,
      _ddcpgrsResponseStatus = pResponseStatus_
    }

-- | An optional pagination token provided by a previous @DescribeDBClusterParameterGroups@ request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
ddcpgrsMarker :: Lens' DescribeDBClusterParameterGroupsResponse (Maybe Text)
ddcpgrsMarker = lens _ddcpgrsMarker (\s a -> s {_ddcpgrsMarker = a})

-- | A list of DB cluster parameter groups.
ddcpgrsDBClusterParameterGroups :: Lens' DescribeDBClusterParameterGroupsResponse [DBClusterParameterGroup]
ddcpgrsDBClusterParameterGroups = lens _ddcpgrsDBClusterParameterGroups (\s a -> s {_ddcpgrsDBClusterParameterGroups = a}) . _Default . _Coerce

-- | -- | The response status code.
ddcpgrsResponseStatus :: Lens' DescribeDBClusterParameterGroupsResponse Int
ddcpgrsResponseStatus = lens _ddcpgrsResponseStatus (\s a -> s {_ddcpgrsResponseStatus = a})

instance NFData DescribeDBClusterParameterGroupsResponse
