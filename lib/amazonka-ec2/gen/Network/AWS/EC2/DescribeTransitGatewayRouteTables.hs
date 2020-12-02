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
-- Module      : Network.AWS.EC2.DescribeTransitGatewayRouteTables
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more transit gateway route tables. By default, all transit gateway route tables are described. Alternatively, you can filter the results.
--
--
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeTransitGatewayRouteTables
  ( -- * Creating a Request
    describeTransitGatewayRouteTables,
    DescribeTransitGatewayRouteTables,

    -- * Request Lenses
    dtgrtsFilters,
    dtgrtsNextToken,
    dtgrtsDryRun,
    dtgrtsTransitGatewayRouteTableIds,
    dtgrtsMaxResults,

    -- * Destructuring the Response
    describeTransitGatewayRouteTablesResponse,
    DescribeTransitGatewayRouteTablesResponse,

    -- * Response Lenses
    dtgrtsrsTransitGatewayRouteTables,
    dtgrtsrsNextToken,
    dtgrtsrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeTransitGatewayRouteTables' smart constructor.
data DescribeTransitGatewayRouteTables = DescribeTransitGatewayRouteTables'
  { _dtgrtsFilters ::
      !(Maybe [Filter]),
    _dtgrtsNextToken ::
      !(Maybe Text),
    _dtgrtsDryRun ::
      !(Maybe Bool),
    _dtgrtsTransitGatewayRouteTableIds ::
      !(Maybe [Text]),
    _dtgrtsMaxResults ::
      !(Maybe Nat)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeTransitGatewayRouteTables' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dtgrtsFilters' - One or more filters. The possible values are:     * @default-association-route-table@ - Indicates whether this is the default association route table for the transit gateway (@true@ | @false@ ).     * @default-propagation-route-table@ - Indicates whether this is the default propagation route table for the transit gateway (@true@ | @false@ ).     * @state@ - The state of the route table (@available@ | @deleting@ | @deleted@ | @pending@ ).     * @transit-gateway-id@ - The ID of the transit gateway.     * @transit-gateway-route-table-id@ - The ID of the transit gateway route table.
--
-- * 'dtgrtsNextToken' - The token for the next page of results.
--
-- * 'dtgrtsDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'dtgrtsTransitGatewayRouteTableIds' - The IDs of the transit gateway route tables.
--
-- * 'dtgrtsMaxResults' - The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
describeTransitGatewayRouteTables ::
  DescribeTransitGatewayRouteTables
describeTransitGatewayRouteTables =
  DescribeTransitGatewayRouteTables'
    { _dtgrtsFilters = Nothing,
      _dtgrtsNextToken = Nothing,
      _dtgrtsDryRun = Nothing,
      _dtgrtsTransitGatewayRouteTableIds = Nothing,
      _dtgrtsMaxResults = Nothing
    }

-- | One or more filters. The possible values are:     * @default-association-route-table@ - Indicates whether this is the default association route table for the transit gateway (@true@ | @false@ ).     * @default-propagation-route-table@ - Indicates whether this is the default propagation route table for the transit gateway (@true@ | @false@ ).     * @state@ - The state of the route table (@available@ | @deleting@ | @deleted@ | @pending@ ).     * @transit-gateway-id@ - The ID of the transit gateway.     * @transit-gateway-route-table-id@ - The ID of the transit gateway route table.
dtgrtsFilters :: Lens' DescribeTransitGatewayRouteTables [Filter]
dtgrtsFilters = lens _dtgrtsFilters (\s a -> s {_dtgrtsFilters = a}) . _Default . _Coerce

-- | The token for the next page of results.
dtgrtsNextToken :: Lens' DescribeTransitGatewayRouteTables (Maybe Text)
dtgrtsNextToken = lens _dtgrtsNextToken (\s a -> s {_dtgrtsNextToken = a})

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
dtgrtsDryRun :: Lens' DescribeTransitGatewayRouteTables (Maybe Bool)
dtgrtsDryRun = lens _dtgrtsDryRun (\s a -> s {_dtgrtsDryRun = a})

-- | The IDs of the transit gateway route tables.
dtgrtsTransitGatewayRouteTableIds :: Lens' DescribeTransitGatewayRouteTables [Text]
dtgrtsTransitGatewayRouteTableIds = lens _dtgrtsTransitGatewayRouteTableIds (\s a -> s {_dtgrtsTransitGatewayRouteTableIds = a}) . _Default . _Coerce

-- | The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
dtgrtsMaxResults :: Lens' DescribeTransitGatewayRouteTables (Maybe Natural)
dtgrtsMaxResults = lens _dtgrtsMaxResults (\s a -> s {_dtgrtsMaxResults = a}) . mapping _Nat

instance AWSPager DescribeTransitGatewayRouteTables where
  page rq rs
    | stop (rs ^. dtgrtsrsNextToken) = Nothing
    | stop (rs ^. dtgrtsrsTransitGatewayRouteTables) = Nothing
    | otherwise =
      Just $ rq & dtgrtsNextToken .~ rs ^. dtgrtsrsNextToken

instance AWSRequest DescribeTransitGatewayRouteTables where
  type
    Rs DescribeTransitGatewayRouteTables =
      DescribeTransitGatewayRouteTablesResponse
  request = postQuery ec2
  response =
    receiveXML
      ( \s h x ->
          DescribeTransitGatewayRouteTablesResponse'
            <$> ( x .@? "transitGatewayRouteTables" .!@ mempty
                    >>= may (parseXMLList "item")
                )
            <*> (x .@? "nextToken")
            <*> (pure (fromEnum s))
      )

instance Hashable DescribeTransitGatewayRouteTables

instance NFData DescribeTransitGatewayRouteTables

instance ToHeaders DescribeTransitGatewayRouteTables where
  toHeaders = const mempty

instance ToPath DescribeTransitGatewayRouteTables where
  toPath = const "/"

instance ToQuery DescribeTransitGatewayRouteTables where
  toQuery DescribeTransitGatewayRouteTables' {..} =
    mconcat
      [ "Action" =: ("DescribeTransitGatewayRouteTables" :: ByteString),
        "Version" =: ("2016-11-15" :: ByteString),
        toQuery (toQueryList "Filter" <$> _dtgrtsFilters),
        "NextToken" =: _dtgrtsNextToken,
        "DryRun" =: _dtgrtsDryRun,
        toQuery
          ( toQueryList "TransitGatewayRouteTableIds"
              <$> _dtgrtsTransitGatewayRouteTableIds
          ),
        "MaxResults" =: _dtgrtsMaxResults
      ]

-- | /See:/ 'describeTransitGatewayRouteTablesResponse' smart constructor.
data DescribeTransitGatewayRouteTablesResponse = DescribeTransitGatewayRouteTablesResponse'
  { _dtgrtsrsTransitGatewayRouteTables ::
      !( Maybe
           [TransitGatewayRouteTable]
       ),
    _dtgrtsrsNextToken ::
      !( Maybe
           Text
       ),
    _dtgrtsrsResponseStatus ::
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

-- | Creates a value of 'DescribeTransitGatewayRouteTablesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dtgrtsrsTransitGatewayRouteTables' - Information about the transit gateway route tables.
--
-- * 'dtgrtsrsNextToken' - The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- * 'dtgrtsrsResponseStatus' - -- | The response status code.
describeTransitGatewayRouteTablesResponse ::
  -- | 'dtgrtsrsResponseStatus'
  Int ->
  DescribeTransitGatewayRouteTablesResponse
describeTransitGatewayRouteTablesResponse pResponseStatus_ =
  DescribeTransitGatewayRouteTablesResponse'
    { _dtgrtsrsTransitGatewayRouteTables =
        Nothing,
      _dtgrtsrsNextToken = Nothing,
      _dtgrtsrsResponseStatus = pResponseStatus_
    }

-- | Information about the transit gateway route tables.
dtgrtsrsTransitGatewayRouteTables :: Lens' DescribeTransitGatewayRouteTablesResponse [TransitGatewayRouteTable]
dtgrtsrsTransitGatewayRouteTables = lens _dtgrtsrsTransitGatewayRouteTables (\s a -> s {_dtgrtsrsTransitGatewayRouteTables = a}) . _Default . _Coerce

-- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
dtgrtsrsNextToken :: Lens' DescribeTransitGatewayRouteTablesResponse (Maybe Text)
dtgrtsrsNextToken = lens _dtgrtsrsNextToken (\s a -> s {_dtgrtsrsNextToken = a})

-- | -- | The response status code.
dtgrtsrsResponseStatus :: Lens' DescribeTransitGatewayRouteTablesResponse Int
dtgrtsrsResponseStatus = lens _dtgrtsrsResponseStatus (\s a -> s {_dtgrtsrsResponseStatus = a})

instance NFData DescribeTransitGatewayRouteTablesResponse
