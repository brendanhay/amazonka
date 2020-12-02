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
-- Module      : Network.AWS.RDS.DescribeDBProxies
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about DB proxies.
--
--
--
-- This operation returns paginated results.
module Network.AWS.RDS.DescribeDBProxies
  ( -- * Creating a Request
    describeDBProxies,
    DescribeDBProxies,

    -- * Request Lenses
    ddbpFilters,
    ddbpMarker,
    ddbpMaxRecords,
    ddbpDBProxyName,

    -- * Destructuring the Response
    describeDBProxiesResponse,
    DescribeDBProxiesResponse,

    -- * Response Lenses
    ddpsrsDBProxies,
    ddpsrsMarker,
    ddpsrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.RDS.Types
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeDBProxies' smart constructor.
data DescribeDBProxies = DescribeDBProxies'
  { _ddbpFilters ::
      !(Maybe [Filter]),
    _ddbpMarker :: !(Maybe Text),
    _ddbpMaxRecords :: !(Maybe Nat),
    _ddbpDBProxyName :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeDBProxies' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddbpFilters' - This parameter is not currently supported.
--
-- * 'ddbpMarker' - An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- * 'ddbpMaxRecords' - The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that the remaining results can be retrieved.  Default: 100 Constraints: Minimum 20, maximum 100.
--
-- * 'ddbpDBProxyName' - The name of the DB proxy.
describeDBProxies ::
  DescribeDBProxies
describeDBProxies =
  DescribeDBProxies'
    { _ddbpFilters = Nothing,
      _ddbpMarker = Nothing,
      _ddbpMaxRecords = Nothing,
      _ddbpDBProxyName = Nothing
    }

-- | This parameter is not currently supported.
ddbpFilters :: Lens' DescribeDBProxies [Filter]
ddbpFilters = lens _ddbpFilters (\s a -> s {_ddbpFilters = a}) . _Default . _Coerce

-- | An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
ddbpMarker :: Lens' DescribeDBProxies (Maybe Text)
ddbpMarker = lens _ddbpMarker (\s a -> s {_ddbpMarker = a})

-- | The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that the remaining results can be retrieved.  Default: 100 Constraints: Minimum 20, maximum 100.
ddbpMaxRecords :: Lens' DescribeDBProxies (Maybe Natural)
ddbpMaxRecords = lens _ddbpMaxRecords (\s a -> s {_ddbpMaxRecords = a}) . mapping _Nat

-- | The name of the DB proxy.
ddbpDBProxyName :: Lens' DescribeDBProxies (Maybe Text)
ddbpDBProxyName = lens _ddbpDBProxyName (\s a -> s {_ddbpDBProxyName = a})

instance AWSPager DescribeDBProxies where
  page rq rs
    | stop (rs ^. ddpsrsMarker) = Nothing
    | stop (rs ^. ddpsrsDBProxies) = Nothing
    | otherwise = Just $ rq & ddbpMarker .~ rs ^. ddpsrsMarker

instance AWSRequest DescribeDBProxies where
  type Rs DescribeDBProxies = DescribeDBProxiesResponse
  request = postQuery rds
  response =
    receiveXMLWrapper
      "DescribeDBProxiesResult"
      ( \s h x ->
          DescribeDBProxiesResponse'
            <$> (x .@? "DBProxies" .!@ mempty >>= may (parseXMLList "member"))
            <*> (x .@? "Marker")
            <*> (pure (fromEnum s))
      )

instance Hashable DescribeDBProxies

instance NFData DescribeDBProxies

instance ToHeaders DescribeDBProxies where
  toHeaders = const mempty

instance ToPath DescribeDBProxies where
  toPath = const "/"

instance ToQuery DescribeDBProxies where
  toQuery DescribeDBProxies' {..} =
    mconcat
      [ "Action" =: ("DescribeDBProxies" :: ByteString),
        "Version" =: ("2014-10-31" :: ByteString),
        "Filters" =: toQuery (toQueryList "Filter" <$> _ddbpFilters),
        "Marker" =: _ddbpMarker,
        "MaxRecords" =: _ddbpMaxRecords,
        "DBProxyName" =: _ddbpDBProxyName
      ]

-- | /See:/ 'describeDBProxiesResponse' smart constructor.
data DescribeDBProxiesResponse = DescribeDBProxiesResponse'
  { _ddpsrsDBProxies ::
      !(Maybe [DBProxy]),
    _ddpsrsMarker :: !(Maybe Text),
    _ddpsrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeDBProxiesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddpsrsDBProxies' - A return value representing an arbitrary number of @DBProxy@ data structures.
--
-- * 'ddpsrsMarker' - An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- * 'ddpsrsResponseStatus' - -- | The response status code.
describeDBProxiesResponse ::
  -- | 'ddpsrsResponseStatus'
  Int ->
  DescribeDBProxiesResponse
describeDBProxiesResponse pResponseStatus_ =
  DescribeDBProxiesResponse'
    { _ddpsrsDBProxies = Nothing,
      _ddpsrsMarker = Nothing,
      _ddpsrsResponseStatus = pResponseStatus_
    }

-- | A return value representing an arbitrary number of @DBProxy@ data structures.
ddpsrsDBProxies :: Lens' DescribeDBProxiesResponse [DBProxy]
ddpsrsDBProxies = lens _ddpsrsDBProxies (\s a -> s {_ddpsrsDBProxies = a}) . _Default . _Coerce

-- | An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
ddpsrsMarker :: Lens' DescribeDBProxiesResponse (Maybe Text)
ddpsrsMarker = lens _ddpsrsMarker (\s a -> s {_ddpsrsMarker = a})

-- | -- | The response status code.
ddpsrsResponseStatus :: Lens' DescribeDBProxiesResponse Int
ddpsrsResponseStatus = lens _ddpsrsResponseStatus (\s a -> s {_ddpsrsResponseStatus = a})

instance NFData DescribeDBProxiesResponse
