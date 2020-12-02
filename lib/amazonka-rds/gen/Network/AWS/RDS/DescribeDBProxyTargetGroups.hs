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
-- Module      : Network.AWS.RDS.DescribeDBProxyTargetGroups
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about DB proxy target groups, represented by @DBProxyTargetGroup@ data structures.
--
--
--
-- This operation returns paginated results.
module Network.AWS.RDS.DescribeDBProxyTargetGroups
  ( -- * Creating a Request
    describeDBProxyTargetGroups,
    DescribeDBProxyTargetGroups,

    -- * Request Lenses
    ddptgFilters,
    ddptgMarker,
    ddptgMaxRecords,
    ddptgTargetGroupName,
    ddptgDBProxyName,

    -- * Destructuring the Response
    describeDBProxyTargetGroupsResponse,
    DescribeDBProxyTargetGroupsResponse,

    -- * Response Lenses
    ddptgrsMarker,
    ddptgrsTargetGroups,
    ddptgrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.RDS.Types
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeDBProxyTargetGroups' smart constructor.
data DescribeDBProxyTargetGroups = DescribeDBProxyTargetGroups'
  { _ddptgFilters ::
      !(Maybe [Filter]),
    _ddptgMarker :: !(Maybe Text),
    _ddptgMaxRecords :: !(Maybe Nat),
    _ddptgTargetGroupName ::
      !(Maybe Text),
    _ddptgDBProxyName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeDBProxyTargetGroups' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddptgFilters' - This parameter is not currently supported.
--
-- * 'ddptgMarker' - An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- * 'ddptgMaxRecords' - The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that the remaining results can be retrieved.  Default: 100 Constraints: Minimum 20, maximum 100.
--
-- * 'ddptgTargetGroupName' - The identifier of the @DBProxyTargetGroup@ to describe.
--
-- * 'ddptgDBProxyName' - The identifier of the @DBProxy@ associated with the target group.
describeDBProxyTargetGroups ::
  -- | 'ddptgDBProxyName'
  Text ->
  DescribeDBProxyTargetGroups
describeDBProxyTargetGroups pDBProxyName_ =
  DescribeDBProxyTargetGroups'
    { _ddptgFilters = Nothing,
      _ddptgMarker = Nothing,
      _ddptgMaxRecords = Nothing,
      _ddptgTargetGroupName = Nothing,
      _ddptgDBProxyName = pDBProxyName_
    }

-- | This parameter is not currently supported.
ddptgFilters :: Lens' DescribeDBProxyTargetGroups [Filter]
ddptgFilters = lens _ddptgFilters (\s a -> s {_ddptgFilters = a}) . _Default . _Coerce

-- | An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
ddptgMarker :: Lens' DescribeDBProxyTargetGroups (Maybe Text)
ddptgMarker = lens _ddptgMarker (\s a -> s {_ddptgMarker = a})

-- | The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that the remaining results can be retrieved.  Default: 100 Constraints: Minimum 20, maximum 100.
ddptgMaxRecords :: Lens' DescribeDBProxyTargetGroups (Maybe Natural)
ddptgMaxRecords = lens _ddptgMaxRecords (\s a -> s {_ddptgMaxRecords = a}) . mapping _Nat

-- | The identifier of the @DBProxyTargetGroup@ to describe.
ddptgTargetGroupName :: Lens' DescribeDBProxyTargetGroups (Maybe Text)
ddptgTargetGroupName = lens _ddptgTargetGroupName (\s a -> s {_ddptgTargetGroupName = a})

-- | The identifier of the @DBProxy@ associated with the target group.
ddptgDBProxyName :: Lens' DescribeDBProxyTargetGroups Text
ddptgDBProxyName = lens _ddptgDBProxyName (\s a -> s {_ddptgDBProxyName = a})

instance AWSPager DescribeDBProxyTargetGroups where
  page rq rs
    | stop (rs ^. ddptgrsMarker) = Nothing
    | stop (rs ^. ddptgrsTargetGroups) = Nothing
    | otherwise = Just $ rq & ddptgMarker .~ rs ^. ddptgrsMarker

instance AWSRequest DescribeDBProxyTargetGroups where
  type
    Rs DescribeDBProxyTargetGroups =
      DescribeDBProxyTargetGroupsResponse
  request = postQuery rds
  response =
    receiveXMLWrapper
      "DescribeDBProxyTargetGroupsResult"
      ( \s h x ->
          DescribeDBProxyTargetGroupsResponse'
            <$> (x .@? "Marker")
            <*> (x .@? "TargetGroups" .!@ mempty >>= may (parseXMLList "member"))
            <*> (pure (fromEnum s))
      )

instance Hashable DescribeDBProxyTargetGroups

instance NFData DescribeDBProxyTargetGroups

instance ToHeaders DescribeDBProxyTargetGroups where
  toHeaders = const mempty

instance ToPath DescribeDBProxyTargetGroups where
  toPath = const "/"

instance ToQuery DescribeDBProxyTargetGroups where
  toQuery DescribeDBProxyTargetGroups' {..} =
    mconcat
      [ "Action" =: ("DescribeDBProxyTargetGroups" :: ByteString),
        "Version" =: ("2014-10-31" :: ByteString),
        "Filters" =: toQuery (toQueryList "Filter" <$> _ddptgFilters),
        "Marker" =: _ddptgMarker,
        "MaxRecords" =: _ddptgMaxRecords,
        "TargetGroupName" =: _ddptgTargetGroupName,
        "DBProxyName" =: _ddptgDBProxyName
      ]

-- | /See:/ 'describeDBProxyTargetGroupsResponse' smart constructor.
data DescribeDBProxyTargetGroupsResponse = DescribeDBProxyTargetGroupsResponse'
  { _ddptgrsMarker ::
      !(Maybe Text),
    _ddptgrsTargetGroups ::
      !( Maybe
           [DBProxyTargetGroup]
       ),
    _ddptgrsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeDBProxyTargetGroupsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddptgrsMarker' - An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- * 'ddptgrsTargetGroups' - An arbitrary number of @DBProxyTargetGroup@ objects, containing details of the corresponding target groups.
--
-- * 'ddptgrsResponseStatus' - -- | The response status code.
describeDBProxyTargetGroupsResponse ::
  -- | 'ddptgrsResponseStatus'
  Int ->
  DescribeDBProxyTargetGroupsResponse
describeDBProxyTargetGroupsResponse pResponseStatus_ =
  DescribeDBProxyTargetGroupsResponse'
    { _ddptgrsMarker = Nothing,
      _ddptgrsTargetGroups = Nothing,
      _ddptgrsResponseStatus = pResponseStatus_
    }

-- | An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
ddptgrsMarker :: Lens' DescribeDBProxyTargetGroupsResponse (Maybe Text)
ddptgrsMarker = lens _ddptgrsMarker (\s a -> s {_ddptgrsMarker = a})

-- | An arbitrary number of @DBProxyTargetGroup@ objects, containing details of the corresponding target groups.
ddptgrsTargetGroups :: Lens' DescribeDBProxyTargetGroupsResponse [DBProxyTargetGroup]
ddptgrsTargetGroups = lens _ddptgrsTargetGroups (\s a -> s {_ddptgrsTargetGroups = a}) . _Default . _Coerce

-- | -- | The response status code.
ddptgrsResponseStatus :: Lens' DescribeDBProxyTargetGroupsResponse Int
ddptgrsResponseStatus = lens _ddptgrsResponseStatus (\s a -> s {_ddptgrsResponseStatus = a})

instance NFData DescribeDBProxyTargetGroupsResponse
