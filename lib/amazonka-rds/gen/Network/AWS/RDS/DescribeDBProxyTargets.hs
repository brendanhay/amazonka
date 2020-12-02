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
-- Module      : Network.AWS.RDS.DescribeDBProxyTargets
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about @DBProxyTarget@ objects. This API supports pagination.
--
--
--
-- This operation returns paginated results.
module Network.AWS.RDS.DescribeDBProxyTargets
  ( -- * Creating a Request
    describeDBProxyTargets,
    DescribeDBProxyTargets,

    -- * Request Lenses
    ddptFilters,
    ddptMarker,
    ddptMaxRecords,
    ddptTargetGroupName,
    ddptDBProxyName,

    -- * Destructuring the Response
    describeDBProxyTargetsResponse,
    DescribeDBProxyTargetsResponse,

    -- * Response Lenses
    ddbptrsTargets,
    ddbptrsMarker,
    ddbptrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.RDS.Types
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeDBProxyTargets' smart constructor.
data DescribeDBProxyTargets = DescribeDBProxyTargets'
  { _ddptFilters ::
      !(Maybe [Filter]),
    _ddptMarker :: !(Maybe Text),
    _ddptMaxRecords :: !(Maybe Nat),
    _ddptTargetGroupName :: !(Maybe Text),
    _ddptDBProxyName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeDBProxyTargets' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddptFilters' - This parameter is not currently supported.
--
-- * 'ddptMarker' - An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- * 'ddptMaxRecords' - The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that the remaining results can be retrieved.  Default: 100 Constraints: Minimum 20, maximum 100.
--
-- * 'ddptTargetGroupName' - The identifier of the @DBProxyTargetGroup@ to describe.
--
-- * 'ddptDBProxyName' - The identifier of the @DBProxyTarget@ to describe.
describeDBProxyTargets ::
  -- | 'ddptDBProxyName'
  Text ->
  DescribeDBProxyTargets
describeDBProxyTargets pDBProxyName_ =
  DescribeDBProxyTargets'
    { _ddptFilters = Nothing,
      _ddptMarker = Nothing,
      _ddptMaxRecords = Nothing,
      _ddptTargetGroupName = Nothing,
      _ddptDBProxyName = pDBProxyName_
    }

-- | This parameter is not currently supported.
ddptFilters :: Lens' DescribeDBProxyTargets [Filter]
ddptFilters = lens _ddptFilters (\s a -> s {_ddptFilters = a}) . _Default . _Coerce

-- | An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
ddptMarker :: Lens' DescribeDBProxyTargets (Maybe Text)
ddptMarker = lens _ddptMarker (\s a -> s {_ddptMarker = a})

-- | The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that the remaining results can be retrieved.  Default: 100 Constraints: Minimum 20, maximum 100.
ddptMaxRecords :: Lens' DescribeDBProxyTargets (Maybe Natural)
ddptMaxRecords = lens _ddptMaxRecords (\s a -> s {_ddptMaxRecords = a}) . mapping _Nat

-- | The identifier of the @DBProxyTargetGroup@ to describe.
ddptTargetGroupName :: Lens' DescribeDBProxyTargets (Maybe Text)
ddptTargetGroupName = lens _ddptTargetGroupName (\s a -> s {_ddptTargetGroupName = a})

-- | The identifier of the @DBProxyTarget@ to describe.
ddptDBProxyName :: Lens' DescribeDBProxyTargets Text
ddptDBProxyName = lens _ddptDBProxyName (\s a -> s {_ddptDBProxyName = a})

instance AWSPager DescribeDBProxyTargets where
  page rq rs
    | stop (rs ^. ddbptrsMarker) = Nothing
    | stop (rs ^. ddbptrsTargets) = Nothing
    | otherwise = Just $ rq & ddptMarker .~ rs ^. ddbptrsMarker

instance AWSRequest DescribeDBProxyTargets where
  type Rs DescribeDBProxyTargets = DescribeDBProxyTargetsResponse
  request = postQuery rds
  response =
    receiveXMLWrapper
      "DescribeDBProxyTargetsResult"
      ( \s h x ->
          DescribeDBProxyTargetsResponse'
            <$> (x .@? "Targets" .!@ mempty >>= may (parseXMLList "member"))
            <*> (x .@? "Marker")
            <*> (pure (fromEnum s))
      )

instance Hashable DescribeDBProxyTargets

instance NFData DescribeDBProxyTargets

instance ToHeaders DescribeDBProxyTargets where
  toHeaders = const mempty

instance ToPath DescribeDBProxyTargets where
  toPath = const "/"

instance ToQuery DescribeDBProxyTargets where
  toQuery DescribeDBProxyTargets' {..} =
    mconcat
      [ "Action" =: ("DescribeDBProxyTargets" :: ByteString),
        "Version" =: ("2014-10-31" :: ByteString),
        "Filters" =: toQuery (toQueryList "Filter" <$> _ddptFilters),
        "Marker" =: _ddptMarker,
        "MaxRecords" =: _ddptMaxRecords,
        "TargetGroupName" =: _ddptTargetGroupName,
        "DBProxyName" =: _ddptDBProxyName
      ]

-- | /See:/ 'describeDBProxyTargetsResponse' smart constructor.
data DescribeDBProxyTargetsResponse = DescribeDBProxyTargetsResponse'
  { _ddbptrsTargets ::
      !(Maybe [DBProxyTarget]),
    _ddbptrsMarker ::
      !(Maybe Text),
    _ddbptrsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeDBProxyTargetsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddbptrsTargets' - An arbitrary number of @DBProxyTarget@ objects, containing details of the corresponding targets.
--
-- * 'ddbptrsMarker' - An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- * 'ddbptrsResponseStatus' - -- | The response status code.
describeDBProxyTargetsResponse ::
  -- | 'ddbptrsResponseStatus'
  Int ->
  DescribeDBProxyTargetsResponse
describeDBProxyTargetsResponse pResponseStatus_ =
  DescribeDBProxyTargetsResponse'
    { _ddbptrsTargets = Nothing,
      _ddbptrsMarker = Nothing,
      _ddbptrsResponseStatus = pResponseStatus_
    }

-- | An arbitrary number of @DBProxyTarget@ objects, containing details of the corresponding targets.
ddbptrsTargets :: Lens' DescribeDBProxyTargetsResponse [DBProxyTarget]
ddbptrsTargets = lens _ddbptrsTargets (\s a -> s {_ddbptrsTargets = a}) . _Default . _Coerce

-- | An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
ddbptrsMarker :: Lens' DescribeDBProxyTargetsResponse (Maybe Text)
ddbptrsMarker = lens _ddbptrsMarker (\s a -> s {_ddbptrsMarker = a})

-- | -- | The response status code.
ddbptrsResponseStatus :: Lens' DescribeDBProxyTargetsResponse Int
ddbptrsResponseStatus = lens _ddbptrsResponseStatus (\s a -> s {_ddbptrsResponseStatus = a})

instance NFData DescribeDBProxyTargetsResponse
