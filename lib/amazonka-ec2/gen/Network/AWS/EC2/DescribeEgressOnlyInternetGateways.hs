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
-- Module      : Network.AWS.EC2.DescribeEgressOnlyInternetGateways
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more of your egress-only internet gateways.
--
--
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeEgressOnlyInternetGateways
  ( -- * Creating a Request
    describeEgressOnlyInternetGateways,
    DescribeEgressOnlyInternetGateways,

    -- * Request Lenses
    deoigEgressOnlyInternetGatewayIds,
    deoigFilters,
    deoigNextToken,
    deoigDryRun,
    deoigMaxResults,

    -- * Destructuring the Response
    describeEgressOnlyInternetGatewaysResponse,
    DescribeEgressOnlyInternetGatewaysResponse,

    -- * Response Lenses
    deoigrsEgressOnlyInternetGateways,
    deoigrsNextToken,
    deoigrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeEgressOnlyInternetGateways' smart constructor.
data DescribeEgressOnlyInternetGateways = DescribeEgressOnlyInternetGateways'
  { _deoigEgressOnlyInternetGatewayIds ::
      !(Maybe [Text]),
    _deoigFilters ::
      !(Maybe [Filter]),
    _deoigNextToken ::
      !(Maybe Text),
    _deoigDryRun ::
      !(Maybe Bool),
    _deoigMaxResults ::
      !(Maybe Nat)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeEgressOnlyInternetGateways' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'deoigEgressOnlyInternetGatewayIds' - One or more egress-only internet gateway IDs.
--
-- * 'deoigFilters' - One or more filters.     * @tag@ :<key> - The key/value combination of a tag assigned to the resource. Use the tag key in the filter name and the tag value as the filter value. For example, to find all resources that have a tag with the key @Owner@ and the value @TeamA@ , specify @tag:Owner@ for the filter name and @TeamA@ for the filter value.     * @tag-key@ - The key of a tag assigned to the resource. Use this filter to find all resources assigned a tag with a specific key, regardless of the tag value.
--
-- * 'deoigNextToken' - The token for the next page of results.
--
-- * 'deoigDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'deoigMaxResults' - The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
describeEgressOnlyInternetGateways ::
  DescribeEgressOnlyInternetGateways
describeEgressOnlyInternetGateways =
  DescribeEgressOnlyInternetGateways'
    { _deoigEgressOnlyInternetGatewayIds =
        Nothing,
      _deoigFilters = Nothing,
      _deoigNextToken = Nothing,
      _deoigDryRun = Nothing,
      _deoigMaxResults = Nothing
    }

-- | One or more egress-only internet gateway IDs.
deoigEgressOnlyInternetGatewayIds :: Lens' DescribeEgressOnlyInternetGateways [Text]
deoigEgressOnlyInternetGatewayIds = lens _deoigEgressOnlyInternetGatewayIds (\s a -> s {_deoigEgressOnlyInternetGatewayIds = a}) . _Default . _Coerce

-- | One or more filters.     * @tag@ :<key> - The key/value combination of a tag assigned to the resource. Use the tag key in the filter name and the tag value as the filter value. For example, to find all resources that have a tag with the key @Owner@ and the value @TeamA@ , specify @tag:Owner@ for the filter name and @TeamA@ for the filter value.     * @tag-key@ - The key of a tag assigned to the resource. Use this filter to find all resources assigned a tag with a specific key, regardless of the tag value.
deoigFilters :: Lens' DescribeEgressOnlyInternetGateways [Filter]
deoigFilters = lens _deoigFilters (\s a -> s {_deoigFilters = a}) . _Default . _Coerce

-- | The token for the next page of results.
deoigNextToken :: Lens' DescribeEgressOnlyInternetGateways (Maybe Text)
deoigNextToken = lens _deoigNextToken (\s a -> s {_deoigNextToken = a})

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
deoigDryRun :: Lens' DescribeEgressOnlyInternetGateways (Maybe Bool)
deoigDryRun = lens _deoigDryRun (\s a -> s {_deoigDryRun = a})

-- | The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
deoigMaxResults :: Lens' DescribeEgressOnlyInternetGateways (Maybe Natural)
deoigMaxResults = lens _deoigMaxResults (\s a -> s {_deoigMaxResults = a}) . mapping _Nat

instance AWSPager DescribeEgressOnlyInternetGateways where
  page rq rs
    | stop (rs ^. deoigrsNextToken) = Nothing
    | stop (rs ^. deoigrsEgressOnlyInternetGateways) = Nothing
    | otherwise = Just $ rq & deoigNextToken .~ rs ^. deoigrsNextToken

instance AWSRequest DescribeEgressOnlyInternetGateways where
  type
    Rs DescribeEgressOnlyInternetGateways =
      DescribeEgressOnlyInternetGatewaysResponse
  request = postQuery ec2
  response =
    receiveXML
      ( \s h x ->
          DescribeEgressOnlyInternetGatewaysResponse'
            <$> ( x .@? "egressOnlyInternetGatewaySet" .!@ mempty
                    >>= may (parseXMLList "item")
                )
            <*> (x .@? "nextToken")
            <*> (pure (fromEnum s))
      )

instance Hashable DescribeEgressOnlyInternetGateways

instance NFData DescribeEgressOnlyInternetGateways

instance ToHeaders DescribeEgressOnlyInternetGateways where
  toHeaders = const mempty

instance ToPath DescribeEgressOnlyInternetGateways where
  toPath = const "/"

instance ToQuery DescribeEgressOnlyInternetGateways where
  toQuery DescribeEgressOnlyInternetGateways' {..} =
    mconcat
      [ "Action" =: ("DescribeEgressOnlyInternetGateways" :: ByteString),
        "Version" =: ("2016-11-15" :: ByteString),
        toQuery
          ( toQueryList "EgressOnlyInternetGatewayId"
              <$> _deoigEgressOnlyInternetGatewayIds
          ),
        toQuery (toQueryList "Filter" <$> _deoigFilters),
        "NextToken" =: _deoigNextToken,
        "DryRun" =: _deoigDryRun,
        "MaxResults" =: _deoigMaxResults
      ]

-- | /See:/ 'describeEgressOnlyInternetGatewaysResponse' smart constructor.
data DescribeEgressOnlyInternetGatewaysResponse = DescribeEgressOnlyInternetGatewaysResponse'
  { _deoigrsEgressOnlyInternetGateways ::
      !( Maybe
           [EgressOnlyInternetGateway]
       ),
    _deoigrsNextToken ::
      !( Maybe
           Text
       ),
    _deoigrsResponseStatus ::
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

-- | Creates a value of 'DescribeEgressOnlyInternetGatewaysResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'deoigrsEgressOnlyInternetGateways' - Information about the egress-only internet gateways.
--
-- * 'deoigrsNextToken' - The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- * 'deoigrsResponseStatus' - -- | The response status code.
describeEgressOnlyInternetGatewaysResponse ::
  -- | 'deoigrsResponseStatus'
  Int ->
  DescribeEgressOnlyInternetGatewaysResponse
describeEgressOnlyInternetGatewaysResponse pResponseStatus_ =
  DescribeEgressOnlyInternetGatewaysResponse'
    { _deoigrsEgressOnlyInternetGateways =
        Nothing,
      _deoigrsNextToken = Nothing,
      _deoigrsResponseStatus = pResponseStatus_
    }

-- | Information about the egress-only internet gateways.
deoigrsEgressOnlyInternetGateways :: Lens' DescribeEgressOnlyInternetGatewaysResponse [EgressOnlyInternetGateway]
deoigrsEgressOnlyInternetGateways = lens _deoigrsEgressOnlyInternetGateways (\s a -> s {_deoigrsEgressOnlyInternetGateways = a}) . _Default . _Coerce

-- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
deoigrsNextToken :: Lens' DescribeEgressOnlyInternetGatewaysResponse (Maybe Text)
deoigrsNextToken = lens _deoigrsNextToken (\s a -> s {_deoigrsNextToken = a})

-- | -- | The response status code.
deoigrsResponseStatus :: Lens' DescribeEgressOnlyInternetGatewaysResponse Int
deoigrsResponseStatus = lens _deoigrsResponseStatus (\s a -> s {_deoigrsResponseStatus = a})

instance NFData DescribeEgressOnlyInternetGatewaysResponse
