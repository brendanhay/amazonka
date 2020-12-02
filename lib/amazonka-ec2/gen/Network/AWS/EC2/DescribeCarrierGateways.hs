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
-- Module      : Network.AWS.EC2.DescribeCarrierGateways
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more of your carrier gateways.
--
--
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeCarrierGateways
  ( -- * Creating a Request
    describeCarrierGateways,
    DescribeCarrierGateways,

    -- * Request Lenses
    dcgsFilters,
    dcgsNextToken,
    dcgsCarrierGatewayIds,
    dcgsDryRun,
    dcgsMaxResults,

    -- * Destructuring the Response
    describeCarrierGatewaysResponse,
    DescribeCarrierGatewaysResponse,

    -- * Response Lenses
    dcgsrsNextToken,
    dcgsrsCarrierGateways,
    dcgsrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeCarrierGateways' smart constructor.
data DescribeCarrierGateways = DescribeCarrierGateways'
  { _dcgsFilters ::
      !(Maybe [Filter]),
    _dcgsNextToken :: !(Maybe Text),
    _dcgsCarrierGatewayIds :: !(Maybe [Text]),
    _dcgsDryRun :: !(Maybe Bool),
    _dcgsMaxResults :: !(Maybe Nat)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeCarrierGateways' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcgsFilters' - One or more filters.     * @carrier-gateway-id@ - The ID of the carrier gateway.     * @state@ - The state of the carrier gateway (@pending@ | @failed@ | @available@ | @deleting@ | @deleted@ ).     * @owner-id@ - The AWS account ID of the owner of the carrier gateway.     * @tag@ :<key> - The key/value combination of a tag assigned to the resource. Use the tag key in the filter name and the tag value as the filter value. For example, to find all resources that have a tag with the key @Owner@ and the value @TeamA@ , specify @tag:Owner@ for the filter name and @TeamA@ for the filter value.     * @tag-key@ - The key of a tag assigned to the resource. Use this filter to find all resources assigned a tag with a specific key, regardless of the tag value.     * @vpc-id@ - The ID of the VPC associated with the carrier gateway.
--
-- * 'dcgsNextToken' - The token for the next page of results.
--
-- * 'dcgsCarrierGatewayIds' - One or more carrier gateway IDs.
--
-- * 'dcgsDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'dcgsMaxResults' - The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
describeCarrierGateways ::
  DescribeCarrierGateways
describeCarrierGateways =
  DescribeCarrierGateways'
    { _dcgsFilters = Nothing,
      _dcgsNextToken = Nothing,
      _dcgsCarrierGatewayIds = Nothing,
      _dcgsDryRun = Nothing,
      _dcgsMaxResults = Nothing
    }

-- | One or more filters.     * @carrier-gateway-id@ - The ID of the carrier gateway.     * @state@ - The state of the carrier gateway (@pending@ | @failed@ | @available@ | @deleting@ | @deleted@ ).     * @owner-id@ - The AWS account ID of the owner of the carrier gateway.     * @tag@ :<key> - The key/value combination of a tag assigned to the resource. Use the tag key in the filter name and the tag value as the filter value. For example, to find all resources that have a tag with the key @Owner@ and the value @TeamA@ , specify @tag:Owner@ for the filter name and @TeamA@ for the filter value.     * @tag-key@ - The key of a tag assigned to the resource. Use this filter to find all resources assigned a tag with a specific key, regardless of the tag value.     * @vpc-id@ - The ID of the VPC associated with the carrier gateway.
dcgsFilters :: Lens' DescribeCarrierGateways [Filter]
dcgsFilters = lens _dcgsFilters (\s a -> s {_dcgsFilters = a}) . _Default . _Coerce

-- | The token for the next page of results.
dcgsNextToken :: Lens' DescribeCarrierGateways (Maybe Text)
dcgsNextToken = lens _dcgsNextToken (\s a -> s {_dcgsNextToken = a})

-- | One or more carrier gateway IDs.
dcgsCarrierGatewayIds :: Lens' DescribeCarrierGateways [Text]
dcgsCarrierGatewayIds = lens _dcgsCarrierGatewayIds (\s a -> s {_dcgsCarrierGatewayIds = a}) . _Default . _Coerce

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
dcgsDryRun :: Lens' DescribeCarrierGateways (Maybe Bool)
dcgsDryRun = lens _dcgsDryRun (\s a -> s {_dcgsDryRun = a})

-- | The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
dcgsMaxResults :: Lens' DescribeCarrierGateways (Maybe Natural)
dcgsMaxResults = lens _dcgsMaxResults (\s a -> s {_dcgsMaxResults = a}) . mapping _Nat

instance AWSPager DescribeCarrierGateways where
  page rq rs
    | stop (rs ^. dcgsrsNextToken) = Nothing
    | stop (rs ^. dcgsrsCarrierGateways) = Nothing
    | otherwise = Just $ rq & dcgsNextToken .~ rs ^. dcgsrsNextToken

instance AWSRequest DescribeCarrierGateways where
  type Rs DescribeCarrierGateways = DescribeCarrierGatewaysResponse
  request = postQuery ec2
  response =
    receiveXML
      ( \s h x ->
          DescribeCarrierGatewaysResponse'
            <$> (x .@? "nextToken")
            <*> ( x .@? "carrierGatewaySet" .!@ mempty
                    >>= may (parseXMLList "item")
                )
            <*> (pure (fromEnum s))
      )

instance Hashable DescribeCarrierGateways

instance NFData DescribeCarrierGateways

instance ToHeaders DescribeCarrierGateways where
  toHeaders = const mempty

instance ToPath DescribeCarrierGateways where
  toPath = const "/"

instance ToQuery DescribeCarrierGateways where
  toQuery DescribeCarrierGateways' {..} =
    mconcat
      [ "Action" =: ("DescribeCarrierGateways" :: ByteString),
        "Version" =: ("2016-11-15" :: ByteString),
        toQuery (toQueryList "Filter" <$> _dcgsFilters),
        "NextToken" =: _dcgsNextToken,
        toQuery
          (toQueryList "CarrierGatewayId" <$> _dcgsCarrierGatewayIds),
        "DryRun" =: _dcgsDryRun,
        "MaxResults" =: _dcgsMaxResults
      ]

-- | /See:/ 'describeCarrierGatewaysResponse' smart constructor.
data DescribeCarrierGatewaysResponse = DescribeCarrierGatewaysResponse'
  { _dcgsrsNextToken ::
      !(Maybe Text),
    _dcgsrsCarrierGateways ::
      !(Maybe [CarrierGateway]),
    _dcgsrsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeCarrierGatewaysResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcgsrsNextToken' - The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- * 'dcgsrsCarrierGateways' - Information about the carrier gateway.
--
-- * 'dcgsrsResponseStatus' - -- | The response status code.
describeCarrierGatewaysResponse ::
  -- | 'dcgsrsResponseStatus'
  Int ->
  DescribeCarrierGatewaysResponse
describeCarrierGatewaysResponse pResponseStatus_ =
  DescribeCarrierGatewaysResponse'
    { _dcgsrsNextToken = Nothing,
      _dcgsrsCarrierGateways = Nothing,
      _dcgsrsResponseStatus = pResponseStatus_
    }

-- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
dcgsrsNextToken :: Lens' DescribeCarrierGatewaysResponse (Maybe Text)
dcgsrsNextToken = lens _dcgsrsNextToken (\s a -> s {_dcgsrsNextToken = a})

-- | Information about the carrier gateway.
dcgsrsCarrierGateways :: Lens' DescribeCarrierGatewaysResponse [CarrierGateway]
dcgsrsCarrierGateways = lens _dcgsrsCarrierGateways (\s a -> s {_dcgsrsCarrierGateways = a}) . _Default . _Coerce

-- | -- | The response status code.
dcgsrsResponseStatus :: Lens' DescribeCarrierGatewaysResponse Int
dcgsrsResponseStatus = lens _dcgsrsResponseStatus (\s a -> s {_dcgsrsResponseStatus = a})

instance NFData DescribeCarrierGatewaysResponse
