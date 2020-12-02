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
-- Module      : Network.AWS.EC2.DescribeVPCs
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more of your VPCs.
--
--
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeVPCs
  ( -- * Creating a Request
    describeVPCs,
    DescribeVPCs,

    -- * Request Lenses
    dvsFilters,
    dvsNextToken,
    dvsVPCIds,
    dvsDryRun,
    dvsMaxResults,

    -- * Destructuring the Response
    describeVPCsResponse,
    DescribeVPCsResponse,

    -- * Response Lenses
    dvrsVPCs,
    dvrsNextToken,
    dvrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeVPCs' smart constructor.
data DescribeVPCs = DescribeVPCs'
  { _dvsFilters :: !(Maybe [Filter]),
    _dvsNextToken :: !(Maybe Text),
    _dvsVPCIds :: !(Maybe [Text]),
    _dvsDryRun :: !(Maybe Bool),
    _dvsMaxResults :: !(Maybe Nat)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeVPCs' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dvsFilters' - One or more filters.     * @cidr@ - The primary IPv4 CIDR block of the VPC. The CIDR block you specify must exactly match the VPC's CIDR block for information to be returned for the VPC. Must contain the slash followed by one or two digits (for example, @/28@ ).     * @cidr-block-association.cidr-block@ - An IPv4 CIDR block associated with the VPC.     * @cidr-block-association.association-id@ - The association ID for an IPv4 CIDR block associated with the VPC.     * @cidr-block-association.state@ - The state of an IPv4 CIDR block associated with the VPC.     * @dhcp-options-id@ - The ID of a set of DHCP options.     * @ipv6-cidr-block-association.ipv6-cidr-block@ - An IPv6 CIDR block associated with the VPC.     * @ipv6-cidr-block-association.ipv6-pool@ - The ID of the IPv6 address pool from which the IPv6 CIDR block is allocated.     * @ipv6-cidr-block-association.association-id@ - The association ID for an IPv6 CIDR block associated with the VPC.     * @ipv6-cidr-block-association.state@ - The state of an IPv6 CIDR block associated with the VPC.     * @isDefault@ - Indicates whether the VPC is the default VPC.     * @owner-id@ - The ID of the AWS account that owns the VPC.     * @state@ - The state of the VPC (@pending@ | @available@ ).     * @tag@ :<key> - The key/value combination of a tag assigned to the resource. Use the tag key in the filter name and the tag value as the filter value. For example, to find all resources that have a tag with the key @Owner@ and the value @TeamA@ , specify @tag:Owner@ for the filter name and @TeamA@ for the filter value.     * @tag-key@ - The key of a tag assigned to the resource. Use this filter to find all resources assigned a tag with a specific key, regardless of the tag value.     * @vpc-id@ - The ID of the VPC.
--
-- * 'dvsNextToken' - The token for the next page of results.
--
-- * 'dvsVPCIds' - One or more VPC IDs. Default: Describes all your VPCs.
--
-- * 'dvsDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'dvsMaxResults' - The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
describeVPCs ::
  DescribeVPCs
describeVPCs =
  DescribeVPCs'
    { _dvsFilters = Nothing,
      _dvsNextToken = Nothing,
      _dvsVPCIds = Nothing,
      _dvsDryRun = Nothing,
      _dvsMaxResults = Nothing
    }

-- | One or more filters.     * @cidr@ - The primary IPv4 CIDR block of the VPC. The CIDR block you specify must exactly match the VPC's CIDR block for information to be returned for the VPC. Must contain the slash followed by one or two digits (for example, @/28@ ).     * @cidr-block-association.cidr-block@ - An IPv4 CIDR block associated with the VPC.     * @cidr-block-association.association-id@ - The association ID for an IPv4 CIDR block associated with the VPC.     * @cidr-block-association.state@ - The state of an IPv4 CIDR block associated with the VPC.     * @dhcp-options-id@ - The ID of a set of DHCP options.     * @ipv6-cidr-block-association.ipv6-cidr-block@ - An IPv6 CIDR block associated with the VPC.     * @ipv6-cidr-block-association.ipv6-pool@ - The ID of the IPv6 address pool from which the IPv6 CIDR block is allocated.     * @ipv6-cidr-block-association.association-id@ - The association ID for an IPv6 CIDR block associated with the VPC.     * @ipv6-cidr-block-association.state@ - The state of an IPv6 CIDR block associated with the VPC.     * @isDefault@ - Indicates whether the VPC is the default VPC.     * @owner-id@ - The ID of the AWS account that owns the VPC.     * @state@ - The state of the VPC (@pending@ | @available@ ).     * @tag@ :<key> - The key/value combination of a tag assigned to the resource. Use the tag key in the filter name and the tag value as the filter value. For example, to find all resources that have a tag with the key @Owner@ and the value @TeamA@ , specify @tag:Owner@ for the filter name and @TeamA@ for the filter value.     * @tag-key@ - The key of a tag assigned to the resource. Use this filter to find all resources assigned a tag with a specific key, regardless of the tag value.     * @vpc-id@ - The ID of the VPC.
dvsFilters :: Lens' DescribeVPCs [Filter]
dvsFilters = lens _dvsFilters (\s a -> s {_dvsFilters = a}) . _Default . _Coerce

-- | The token for the next page of results.
dvsNextToken :: Lens' DescribeVPCs (Maybe Text)
dvsNextToken = lens _dvsNextToken (\s a -> s {_dvsNextToken = a})

-- | One or more VPC IDs. Default: Describes all your VPCs.
dvsVPCIds :: Lens' DescribeVPCs [Text]
dvsVPCIds = lens _dvsVPCIds (\s a -> s {_dvsVPCIds = a}) . _Default . _Coerce

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
dvsDryRun :: Lens' DescribeVPCs (Maybe Bool)
dvsDryRun = lens _dvsDryRun (\s a -> s {_dvsDryRun = a})

-- | The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
dvsMaxResults :: Lens' DescribeVPCs (Maybe Natural)
dvsMaxResults = lens _dvsMaxResults (\s a -> s {_dvsMaxResults = a}) . mapping _Nat

instance AWSPager DescribeVPCs where
  page rq rs
    | stop (rs ^. dvrsNextToken) = Nothing
    | stop (rs ^. dvrsVPCs) = Nothing
    | otherwise = Just $ rq & dvsNextToken .~ rs ^. dvrsNextToken

instance AWSRequest DescribeVPCs where
  type Rs DescribeVPCs = DescribeVPCsResponse
  request = postQuery ec2
  response =
    receiveXML
      ( \s h x ->
          DescribeVPCsResponse'
            <$> (x .@? "vpcSet" .!@ mempty >>= may (parseXMLList "item"))
            <*> (x .@? "nextToken")
            <*> (pure (fromEnum s))
      )

instance Hashable DescribeVPCs

instance NFData DescribeVPCs

instance ToHeaders DescribeVPCs where
  toHeaders = const mempty

instance ToPath DescribeVPCs where
  toPath = const "/"

instance ToQuery DescribeVPCs where
  toQuery DescribeVPCs' {..} =
    mconcat
      [ "Action" =: ("DescribeVpcs" :: ByteString),
        "Version" =: ("2016-11-15" :: ByteString),
        toQuery (toQueryList "Filter" <$> _dvsFilters),
        "NextToken" =: _dvsNextToken,
        toQuery (toQueryList "VpcId" <$> _dvsVPCIds),
        "DryRun" =: _dvsDryRun,
        "MaxResults" =: _dvsMaxResults
      ]

-- | /See:/ 'describeVPCsResponse' smart constructor.
data DescribeVPCsResponse = DescribeVPCsResponse'
  { _dvrsVPCs ::
      !(Maybe [VPC]),
    _dvrsNextToken :: !(Maybe Text),
    _dvrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeVPCsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dvrsVPCs' - Information about one or more VPCs.
--
-- * 'dvrsNextToken' - The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- * 'dvrsResponseStatus' - -- | The response status code.
describeVPCsResponse ::
  -- | 'dvrsResponseStatus'
  Int ->
  DescribeVPCsResponse
describeVPCsResponse pResponseStatus_ =
  DescribeVPCsResponse'
    { _dvrsVPCs = Nothing,
      _dvrsNextToken = Nothing,
      _dvrsResponseStatus = pResponseStatus_
    }

-- | Information about one or more VPCs.
dvrsVPCs :: Lens' DescribeVPCsResponse [VPC]
dvrsVPCs = lens _dvrsVPCs (\s a -> s {_dvrsVPCs = a}) . _Default . _Coerce

-- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
dvrsNextToken :: Lens' DescribeVPCsResponse (Maybe Text)
dvrsNextToken = lens _dvrsNextToken (\s a -> s {_dvrsNextToken = a})

-- | -- | The response status code.
dvrsResponseStatus :: Lens' DescribeVPCsResponse Int
dvrsResponseStatus = lens _dvrsResponseStatus (\s a -> s {_dvrsResponseStatus = a})

instance NFData DescribeVPCsResponse
