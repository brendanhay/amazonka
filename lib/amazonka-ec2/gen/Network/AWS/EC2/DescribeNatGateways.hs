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
-- Module      : Network.AWS.EC2.DescribeNatGateways
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more of your NAT gateways.
--
--
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeNatGateways
  ( -- * Creating a Request
    describeNatGateways,
    DescribeNatGateways,

    -- * Request Lenses
    dngNatGatewayIds,
    dngNextToken,
    dngFilter,
    dngDryRun,
    dngMaxResults,

    -- * Destructuring the Response
    describeNatGatewaysResponse,
    DescribeNatGatewaysResponse,

    -- * Response Lenses
    dngrsNatGateways,
    dngrsNextToken,
    dngrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeNatGateways' smart constructor.
data DescribeNatGateways = DescribeNatGateways'
  { _dngNatGatewayIds ::
      !(Maybe [Text]),
    _dngNextToken :: !(Maybe Text),
    _dngFilter :: !(Maybe [Filter]),
    _dngDryRun :: !(Maybe Bool),
    _dngMaxResults :: !(Maybe Nat)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeNatGateways' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dngNatGatewayIds' - One or more NAT gateway IDs.
--
-- * 'dngNextToken' - The token for the next page of results.
--
-- * 'dngFilter' - One or more filters.     * @nat-gateway-id@ - The ID of the NAT gateway.     * @state@ - The state of the NAT gateway (@pending@ | @failed@ | @available@ | @deleting@ | @deleted@ ).     * @subnet-id@ - The ID of the subnet in which the NAT gateway resides.     * @tag@ :<key> - The key/value combination of a tag assigned to the resource. Use the tag key in the filter name and the tag value as the filter value. For example, to find all resources that have a tag with the key @Owner@ and the value @TeamA@ , specify @tag:Owner@ for the filter name and @TeamA@ for the filter value.     * @tag-key@ - The key of a tag assigned to the resource. Use this filter to find all resources assigned a tag with a specific key, regardless of the tag value.     * @vpc-id@ - The ID of the VPC in which the NAT gateway resides.
--
-- * 'dngDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'dngMaxResults' - The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
describeNatGateways ::
  DescribeNatGateways
describeNatGateways =
  DescribeNatGateways'
    { _dngNatGatewayIds = Nothing,
      _dngNextToken = Nothing,
      _dngFilter = Nothing,
      _dngDryRun = Nothing,
      _dngMaxResults = Nothing
    }

-- | One or more NAT gateway IDs.
dngNatGatewayIds :: Lens' DescribeNatGateways [Text]
dngNatGatewayIds = lens _dngNatGatewayIds (\s a -> s {_dngNatGatewayIds = a}) . _Default . _Coerce

-- | The token for the next page of results.
dngNextToken :: Lens' DescribeNatGateways (Maybe Text)
dngNextToken = lens _dngNextToken (\s a -> s {_dngNextToken = a})

-- | One or more filters.     * @nat-gateway-id@ - The ID of the NAT gateway.     * @state@ - The state of the NAT gateway (@pending@ | @failed@ | @available@ | @deleting@ | @deleted@ ).     * @subnet-id@ - The ID of the subnet in which the NAT gateway resides.     * @tag@ :<key> - The key/value combination of a tag assigned to the resource. Use the tag key in the filter name and the tag value as the filter value. For example, to find all resources that have a tag with the key @Owner@ and the value @TeamA@ , specify @tag:Owner@ for the filter name and @TeamA@ for the filter value.     * @tag-key@ - The key of a tag assigned to the resource. Use this filter to find all resources assigned a tag with a specific key, regardless of the tag value.     * @vpc-id@ - The ID of the VPC in which the NAT gateway resides.
dngFilter :: Lens' DescribeNatGateways [Filter]
dngFilter = lens _dngFilter (\s a -> s {_dngFilter = a}) . _Default . _Coerce

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
dngDryRun :: Lens' DescribeNatGateways (Maybe Bool)
dngDryRun = lens _dngDryRun (\s a -> s {_dngDryRun = a})

-- | The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
dngMaxResults :: Lens' DescribeNatGateways (Maybe Natural)
dngMaxResults = lens _dngMaxResults (\s a -> s {_dngMaxResults = a}) . mapping _Nat

instance AWSPager DescribeNatGateways where
  page rq rs
    | stop (rs ^. dngrsNextToken) = Nothing
    | stop (rs ^. dngrsNatGateways) = Nothing
    | otherwise = Just $ rq & dngNextToken .~ rs ^. dngrsNextToken

instance AWSRequest DescribeNatGateways where
  type Rs DescribeNatGateways = DescribeNatGatewaysResponse
  request = postQuery ec2
  response =
    receiveXML
      ( \s h x ->
          DescribeNatGatewaysResponse'
            <$> (x .@? "natGatewaySet" .!@ mempty >>= may (parseXMLList "item"))
            <*> (x .@? "nextToken")
            <*> (pure (fromEnum s))
      )

instance Hashable DescribeNatGateways

instance NFData DescribeNatGateways

instance ToHeaders DescribeNatGateways where
  toHeaders = const mempty

instance ToPath DescribeNatGateways where
  toPath = const "/"

instance ToQuery DescribeNatGateways where
  toQuery DescribeNatGateways' {..} =
    mconcat
      [ "Action" =: ("DescribeNatGateways" :: ByteString),
        "Version" =: ("2016-11-15" :: ByteString),
        toQuery (toQueryList "NatGatewayId" <$> _dngNatGatewayIds),
        "NextToken" =: _dngNextToken,
        toQuery (toQueryList "Filter" <$> _dngFilter),
        "DryRun" =: _dngDryRun,
        "MaxResults" =: _dngMaxResults
      ]

-- | /See:/ 'describeNatGatewaysResponse' smart constructor.
data DescribeNatGatewaysResponse = DescribeNatGatewaysResponse'
  { _dngrsNatGateways ::
      !(Maybe [NatGateway]),
    _dngrsNextToken :: !(Maybe Text),
    _dngrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeNatGatewaysResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dngrsNatGateways' - Information about the NAT gateways.
--
-- * 'dngrsNextToken' - The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- * 'dngrsResponseStatus' - -- | The response status code.
describeNatGatewaysResponse ::
  -- | 'dngrsResponseStatus'
  Int ->
  DescribeNatGatewaysResponse
describeNatGatewaysResponse pResponseStatus_ =
  DescribeNatGatewaysResponse'
    { _dngrsNatGateways = Nothing,
      _dngrsNextToken = Nothing,
      _dngrsResponseStatus = pResponseStatus_
    }

-- | Information about the NAT gateways.
dngrsNatGateways :: Lens' DescribeNatGatewaysResponse [NatGateway]
dngrsNatGateways = lens _dngrsNatGateways (\s a -> s {_dngrsNatGateways = a}) . _Default . _Coerce

-- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
dngrsNextToken :: Lens' DescribeNatGatewaysResponse (Maybe Text)
dngrsNextToken = lens _dngrsNextToken (\s a -> s {_dngrsNextToken = a})

-- | -- | The response status code.
dngrsResponseStatus :: Lens' DescribeNatGatewaysResponse Int
dngrsResponseStatus = lens _dngrsResponseStatus (\s a -> s {_dngrsResponseStatus = a})

instance NFData DescribeNatGatewaysResponse
