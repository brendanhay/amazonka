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
-- Module      : Network.AWS.EC2.DescribeLocalGatewayRouteTables
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more local gateway route tables. By default, all local gateway route tables are described. Alternatively, you can filter the results.
--
--
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeLocalGatewayRouteTables
  ( -- * Creating a Request
    describeLocalGatewayRouteTables,
    DescribeLocalGatewayRouteTables,

    -- * Request Lenses
    dlgrtFilters,
    dlgrtNextToken,
    dlgrtLocalGatewayRouteTableIds,
    dlgrtDryRun,
    dlgrtMaxResults,

    -- * Destructuring the Response
    describeLocalGatewayRouteTablesResponse,
    DescribeLocalGatewayRouteTablesResponse,

    -- * Response Lenses
    dlgrtrsNextToken,
    dlgrtrsLocalGatewayRouteTables,
    dlgrtrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeLocalGatewayRouteTables' smart constructor.
data DescribeLocalGatewayRouteTables = DescribeLocalGatewayRouteTables'
  { _dlgrtFilters ::
      !(Maybe [Filter]),
    _dlgrtNextToken ::
      !(Maybe Text),
    _dlgrtLocalGatewayRouteTableIds ::
      !(Maybe [Text]),
    _dlgrtDryRun ::
      !(Maybe Bool),
    _dlgrtMaxResults ::
      !(Maybe Nat)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeLocalGatewayRouteTables' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dlgrtFilters' - One or more filters.     * @local-gateway-id@ - The ID of a local gateway.     * @local-gateway-route-table-id@ - The ID of a local gateway route table.     * @outpost-arn@ - The Amazon Resource Name (ARN) of the Outpost.     * @state@ - The state of the local gateway route table.
--
-- * 'dlgrtNextToken' - The token for the next page of results.
--
-- * 'dlgrtLocalGatewayRouteTableIds' - The IDs of the local gateway route tables.
--
-- * 'dlgrtDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'dlgrtMaxResults' - The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
describeLocalGatewayRouteTables ::
  DescribeLocalGatewayRouteTables
describeLocalGatewayRouteTables =
  DescribeLocalGatewayRouteTables'
    { _dlgrtFilters = Nothing,
      _dlgrtNextToken = Nothing,
      _dlgrtLocalGatewayRouteTableIds = Nothing,
      _dlgrtDryRun = Nothing,
      _dlgrtMaxResults = Nothing
    }

-- | One or more filters.     * @local-gateway-id@ - The ID of a local gateway.     * @local-gateway-route-table-id@ - The ID of a local gateway route table.     * @outpost-arn@ - The Amazon Resource Name (ARN) of the Outpost.     * @state@ - The state of the local gateway route table.
dlgrtFilters :: Lens' DescribeLocalGatewayRouteTables [Filter]
dlgrtFilters = lens _dlgrtFilters (\s a -> s {_dlgrtFilters = a}) . _Default . _Coerce

-- | The token for the next page of results.
dlgrtNextToken :: Lens' DescribeLocalGatewayRouteTables (Maybe Text)
dlgrtNextToken = lens _dlgrtNextToken (\s a -> s {_dlgrtNextToken = a})

-- | The IDs of the local gateway route tables.
dlgrtLocalGatewayRouteTableIds :: Lens' DescribeLocalGatewayRouteTables [Text]
dlgrtLocalGatewayRouteTableIds = lens _dlgrtLocalGatewayRouteTableIds (\s a -> s {_dlgrtLocalGatewayRouteTableIds = a}) . _Default . _Coerce

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
dlgrtDryRun :: Lens' DescribeLocalGatewayRouteTables (Maybe Bool)
dlgrtDryRun = lens _dlgrtDryRun (\s a -> s {_dlgrtDryRun = a})

-- | The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
dlgrtMaxResults :: Lens' DescribeLocalGatewayRouteTables (Maybe Natural)
dlgrtMaxResults = lens _dlgrtMaxResults (\s a -> s {_dlgrtMaxResults = a}) . mapping _Nat

instance AWSPager DescribeLocalGatewayRouteTables where
  page rq rs
    | stop (rs ^. dlgrtrsNextToken) = Nothing
    | stop (rs ^. dlgrtrsLocalGatewayRouteTables) = Nothing
    | otherwise = Just $ rq & dlgrtNextToken .~ rs ^. dlgrtrsNextToken

instance AWSRequest DescribeLocalGatewayRouteTables where
  type
    Rs DescribeLocalGatewayRouteTables =
      DescribeLocalGatewayRouteTablesResponse
  request = postQuery ec2
  response =
    receiveXML
      ( \s h x ->
          DescribeLocalGatewayRouteTablesResponse'
            <$> (x .@? "nextToken")
            <*> ( x .@? "localGatewayRouteTableSet" .!@ mempty
                    >>= may (parseXMLList "item")
                )
            <*> (pure (fromEnum s))
      )

instance Hashable DescribeLocalGatewayRouteTables

instance NFData DescribeLocalGatewayRouteTables

instance ToHeaders DescribeLocalGatewayRouteTables where
  toHeaders = const mempty

instance ToPath DescribeLocalGatewayRouteTables where
  toPath = const "/"

instance ToQuery DescribeLocalGatewayRouteTables where
  toQuery DescribeLocalGatewayRouteTables' {..} =
    mconcat
      [ "Action" =: ("DescribeLocalGatewayRouteTables" :: ByteString),
        "Version" =: ("2016-11-15" :: ByteString),
        toQuery (toQueryList "Filter" <$> _dlgrtFilters),
        "NextToken" =: _dlgrtNextToken,
        toQuery
          ( toQueryList "LocalGatewayRouteTableId"
              <$> _dlgrtLocalGatewayRouteTableIds
          ),
        "DryRun" =: _dlgrtDryRun,
        "MaxResults" =: _dlgrtMaxResults
      ]

-- | /See:/ 'describeLocalGatewayRouteTablesResponse' smart constructor.
data DescribeLocalGatewayRouteTablesResponse = DescribeLocalGatewayRouteTablesResponse'
  { _dlgrtrsNextToken ::
      !( Maybe
           Text
       ),
    _dlgrtrsLocalGatewayRouteTables ::
      !( Maybe
           [LocalGatewayRouteTable]
       ),
    _dlgrtrsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeLocalGatewayRouteTablesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dlgrtrsNextToken' - The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- * 'dlgrtrsLocalGatewayRouteTables' - Information about the local gateway route tables.
--
-- * 'dlgrtrsResponseStatus' - -- | The response status code.
describeLocalGatewayRouteTablesResponse ::
  -- | 'dlgrtrsResponseStatus'
  Int ->
  DescribeLocalGatewayRouteTablesResponse
describeLocalGatewayRouteTablesResponse pResponseStatus_ =
  DescribeLocalGatewayRouteTablesResponse'
    { _dlgrtrsNextToken =
        Nothing,
      _dlgrtrsLocalGatewayRouteTables = Nothing,
      _dlgrtrsResponseStatus = pResponseStatus_
    }

-- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
dlgrtrsNextToken :: Lens' DescribeLocalGatewayRouteTablesResponse (Maybe Text)
dlgrtrsNextToken = lens _dlgrtrsNextToken (\s a -> s {_dlgrtrsNextToken = a})

-- | Information about the local gateway route tables.
dlgrtrsLocalGatewayRouteTables :: Lens' DescribeLocalGatewayRouteTablesResponse [LocalGatewayRouteTable]
dlgrtrsLocalGatewayRouteTables = lens _dlgrtrsLocalGatewayRouteTables (\s a -> s {_dlgrtrsLocalGatewayRouteTables = a}) . _Default . _Coerce

-- | -- | The response status code.
dlgrtrsResponseStatus :: Lens' DescribeLocalGatewayRouteTablesResponse Int
dlgrtrsResponseStatus = lens _dlgrtrsResponseStatus (\s a -> s {_dlgrtrsResponseStatus = a})

instance NFData DescribeLocalGatewayRouteTablesResponse
