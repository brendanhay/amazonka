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
-- Module      : Network.AWS.EC2.DescribeInternetGateways
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more of your internet gateways.
--
--
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeInternetGateways
  ( -- * Creating a Request
    describeInternetGateways,
    DescribeInternetGateways,

    -- * Request Lenses
    dFilters,
    dNextToken,
    dInternetGatewayIds,
    dDryRun,
    dMaxResults,

    -- * Destructuring the Response
    describeInternetGatewaysResponse,
    DescribeInternetGatewaysResponse,

    -- * Response Lenses
    digrsNextToken,
    digrsInternetGateways,
    digrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeInternetGateways' smart constructor.
data DescribeInternetGateways = DescribeInternetGateways'
  { _dFilters ::
      !(Maybe [Filter]),
    _dNextToken :: !(Maybe Text),
    _dInternetGatewayIds :: !(Maybe [Text]),
    _dDryRun :: !(Maybe Bool),
    _dMaxResults :: !(Maybe Nat)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeInternetGateways' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dFilters' - One or more filters.     * @attachment.state@ - The current state of the attachment between the gateway and the VPC (@available@ ). Present only if a VPC is attached.     * @attachment.vpc-id@ - The ID of an attached VPC.     * @internet-gateway-id@ - The ID of the Internet gateway.     * @owner-id@ - The ID of the AWS account that owns the internet gateway.     * @tag@ :<key> - The key/value combination of a tag assigned to the resource. Use the tag key in the filter name and the tag value as the filter value. For example, to find all resources that have a tag with the key @Owner@ and the value @TeamA@ , specify @tag:Owner@ for the filter name and @TeamA@ for the filter value.     * @tag-key@ - The key of a tag assigned to the resource. Use this filter to find all resources assigned a tag with a specific key, regardless of the tag value.
--
-- * 'dNextToken' - The token for the next page of results.
--
-- * 'dInternetGatewayIds' - One or more internet gateway IDs. Default: Describes all your internet gateways.
--
-- * 'dDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'dMaxResults' - The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
describeInternetGateways ::
  DescribeInternetGateways
describeInternetGateways =
  DescribeInternetGateways'
    { _dFilters = Nothing,
      _dNextToken = Nothing,
      _dInternetGatewayIds = Nothing,
      _dDryRun = Nothing,
      _dMaxResults = Nothing
    }

-- | One or more filters.     * @attachment.state@ - The current state of the attachment between the gateway and the VPC (@available@ ). Present only if a VPC is attached.     * @attachment.vpc-id@ - The ID of an attached VPC.     * @internet-gateway-id@ - The ID of the Internet gateway.     * @owner-id@ - The ID of the AWS account that owns the internet gateway.     * @tag@ :<key> - The key/value combination of a tag assigned to the resource. Use the tag key in the filter name and the tag value as the filter value. For example, to find all resources that have a tag with the key @Owner@ and the value @TeamA@ , specify @tag:Owner@ for the filter name and @TeamA@ for the filter value.     * @tag-key@ - The key of a tag assigned to the resource. Use this filter to find all resources assigned a tag with a specific key, regardless of the tag value.
dFilters :: Lens' DescribeInternetGateways [Filter]
dFilters = lens _dFilters (\s a -> s {_dFilters = a}) . _Default . _Coerce

-- | The token for the next page of results.
dNextToken :: Lens' DescribeInternetGateways (Maybe Text)
dNextToken = lens _dNextToken (\s a -> s {_dNextToken = a})

-- | One or more internet gateway IDs. Default: Describes all your internet gateways.
dInternetGatewayIds :: Lens' DescribeInternetGateways [Text]
dInternetGatewayIds = lens _dInternetGatewayIds (\s a -> s {_dInternetGatewayIds = a}) . _Default . _Coerce

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
dDryRun :: Lens' DescribeInternetGateways (Maybe Bool)
dDryRun = lens _dDryRun (\s a -> s {_dDryRun = a})

-- | The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
dMaxResults :: Lens' DescribeInternetGateways (Maybe Natural)
dMaxResults = lens _dMaxResults (\s a -> s {_dMaxResults = a}) . mapping _Nat

instance AWSPager DescribeInternetGateways where
  page rq rs
    | stop (rs ^. digrsNextToken) = Nothing
    | stop (rs ^. digrsInternetGateways) = Nothing
    | otherwise = Just $ rq & dNextToken .~ rs ^. digrsNextToken

instance AWSRequest DescribeInternetGateways where
  type Rs DescribeInternetGateways = DescribeInternetGatewaysResponse
  request = postQuery ec2
  response =
    receiveXML
      ( \s h x ->
          DescribeInternetGatewaysResponse'
            <$> (x .@? "nextToken")
            <*> ( x .@? "internetGatewaySet" .!@ mempty
                    >>= may (parseXMLList "item")
                )
            <*> (pure (fromEnum s))
      )

instance Hashable DescribeInternetGateways

instance NFData DescribeInternetGateways

instance ToHeaders DescribeInternetGateways where
  toHeaders = const mempty

instance ToPath DescribeInternetGateways where
  toPath = const "/"

instance ToQuery DescribeInternetGateways where
  toQuery DescribeInternetGateways' {..} =
    mconcat
      [ "Action" =: ("DescribeInternetGateways" :: ByteString),
        "Version" =: ("2016-11-15" :: ByteString),
        toQuery (toQueryList "Filter" <$> _dFilters),
        "NextToken" =: _dNextToken,
        toQuery (toQueryList "InternetGatewayId" <$> _dInternetGatewayIds),
        "DryRun" =: _dDryRun,
        "MaxResults" =: _dMaxResults
      ]

-- | /See:/ 'describeInternetGatewaysResponse' smart constructor.
data DescribeInternetGatewaysResponse = DescribeInternetGatewaysResponse'
  { _digrsNextToken ::
      !(Maybe Text),
    _digrsInternetGateways ::
      !( Maybe
           [InternetGateway]
       ),
    _digrsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeInternetGatewaysResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'digrsNextToken' - The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- * 'digrsInternetGateways' - Information about one or more internet gateways.
--
-- * 'digrsResponseStatus' - -- | The response status code.
describeInternetGatewaysResponse ::
  -- | 'digrsResponseStatus'
  Int ->
  DescribeInternetGatewaysResponse
describeInternetGatewaysResponse pResponseStatus_ =
  DescribeInternetGatewaysResponse'
    { _digrsNextToken = Nothing,
      _digrsInternetGateways = Nothing,
      _digrsResponseStatus = pResponseStatus_
    }

-- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
digrsNextToken :: Lens' DescribeInternetGatewaysResponse (Maybe Text)
digrsNextToken = lens _digrsNextToken (\s a -> s {_digrsNextToken = a})

-- | Information about one or more internet gateways.
digrsInternetGateways :: Lens' DescribeInternetGatewaysResponse [InternetGateway]
digrsInternetGateways = lens _digrsInternetGateways (\s a -> s {_digrsInternetGateways = a}) . _Default . _Coerce

-- | -- | The response status code.
digrsResponseStatus :: Lens' DescribeInternetGatewaysResponse Int
digrsResponseStatus = lens _digrsResponseStatus (\s a -> s {_digrsResponseStatus = a})

instance NFData DescribeInternetGatewaysResponse
