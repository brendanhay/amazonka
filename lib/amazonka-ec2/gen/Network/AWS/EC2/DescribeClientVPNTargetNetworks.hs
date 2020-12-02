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
-- Module      : Network.AWS.EC2.DescribeClientVPNTargetNetworks
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the target networks associated with the specified Client VPN endpoint.
--
--
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeClientVPNTargetNetworks
  ( -- * Creating a Request
    describeClientVPNTargetNetworks,
    DescribeClientVPNTargetNetworks,

    -- * Request Lenses
    dcvtnFilters,
    dcvtnNextToken,
    dcvtnAssociationIds,
    dcvtnDryRun,
    dcvtnMaxResults,
    dcvtnClientVPNEndpointId,

    -- * Destructuring the Response
    describeClientVPNTargetNetworksResponse,
    DescribeClientVPNTargetNetworksResponse,

    -- * Response Lenses
    dcvtnrsClientVPNTargetNetworks,
    dcvtnrsNextToken,
    dcvtnrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeClientVPNTargetNetworks' smart constructor.
data DescribeClientVPNTargetNetworks = DescribeClientVPNTargetNetworks'
  { _dcvtnFilters ::
      !(Maybe [Filter]),
    _dcvtnNextToken ::
      !(Maybe Text),
    _dcvtnAssociationIds ::
      !(Maybe [Text]),
    _dcvtnDryRun ::
      !(Maybe Bool),
    _dcvtnMaxResults ::
      !(Maybe Nat),
    _dcvtnClientVPNEndpointId ::
      !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeClientVPNTargetNetworks' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcvtnFilters' - One or more filters. Filter names and values are case-sensitive.     * @association-id@ - The ID of the association.     * @target-network-id@ - The ID of the subnet specified as the target network.     * @vpc-id@ - The ID of the VPC in which the target network is located.
--
-- * 'dcvtnNextToken' - The token to retrieve the next page of results.
--
-- * 'dcvtnAssociationIds' - The IDs of the target network associations.
--
-- * 'dcvtnDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'dcvtnMaxResults' - The maximum number of results to return for the request in a single page. The remaining results can be seen by sending another request with the nextToken value.
--
-- * 'dcvtnClientVPNEndpointId' - The ID of the Client VPN endpoint.
describeClientVPNTargetNetworks ::
  -- | 'dcvtnClientVPNEndpointId'
  Text ->
  DescribeClientVPNTargetNetworks
describeClientVPNTargetNetworks pClientVPNEndpointId_ =
  DescribeClientVPNTargetNetworks'
    { _dcvtnFilters = Nothing,
      _dcvtnNextToken = Nothing,
      _dcvtnAssociationIds = Nothing,
      _dcvtnDryRun = Nothing,
      _dcvtnMaxResults = Nothing,
      _dcvtnClientVPNEndpointId = pClientVPNEndpointId_
    }

-- | One or more filters. Filter names and values are case-sensitive.     * @association-id@ - The ID of the association.     * @target-network-id@ - The ID of the subnet specified as the target network.     * @vpc-id@ - The ID of the VPC in which the target network is located.
dcvtnFilters :: Lens' DescribeClientVPNTargetNetworks [Filter]
dcvtnFilters = lens _dcvtnFilters (\s a -> s {_dcvtnFilters = a}) . _Default . _Coerce

-- | The token to retrieve the next page of results.
dcvtnNextToken :: Lens' DescribeClientVPNTargetNetworks (Maybe Text)
dcvtnNextToken = lens _dcvtnNextToken (\s a -> s {_dcvtnNextToken = a})

-- | The IDs of the target network associations.
dcvtnAssociationIds :: Lens' DescribeClientVPNTargetNetworks [Text]
dcvtnAssociationIds = lens _dcvtnAssociationIds (\s a -> s {_dcvtnAssociationIds = a}) . _Default . _Coerce

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
dcvtnDryRun :: Lens' DescribeClientVPNTargetNetworks (Maybe Bool)
dcvtnDryRun = lens _dcvtnDryRun (\s a -> s {_dcvtnDryRun = a})

-- | The maximum number of results to return for the request in a single page. The remaining results can be seen by sending another request with the nextToken value.
dcvtnMaxResults :: Lens' DescribeClientVPNTargetNetworks (Maybe Natural)
dcvtnMaxResults = lens _dcvtnMaxResults (\s a -> s {_dcvtnMaxResults = a}) . mapping _Nat

-- | The ID of the Client VPN endpoint.
dcvtnClientVPNEndpointId :: Lens' DescribeClientVPNTargetNetworks Text
dcvtnClientVPNEndpointId = lens _dcvtnClientVPNEndpointId (\s a -> s {_dcvtnClientVPNEndpointId = a})

instance AWSPager DescribeClientVPNTargetNetworks where
  page rq rs
    | stop (rs ^. dcvtnrsNextToken) = Nothing
    | stop (rs ^. dcvtnrsClientVPNTargetNetworks) = Nothing
    | otherwise = Just $ rq & dcvtnNextToken .~ rs ^. dcvtnrsNextToken

instance AWSRequest DescribeClientVPNTargetNetworks where
  type
    Rs DescribeClientVPNTargetNetworks =
      DescribeClientVPNTargetNetworksResponse
  request = postQuery ec2
  response =
    receiveXML
      ( \s h x ->
          DescribeClientVPNTargetNetworksResponse'
            <$> ( x .@? "clientVpnTargetNetworks" .!@ mempty
                    >>= may (parseXMLList "item")
                )
            <*> (x .@? "nextToken")
            <*> (pure (fromEnum s))
      )

instance Hashable DescribeClientVPNTargetNetworks

instance NFData DescribeClientVPNTargetNetworks

instance ToHeaders DescribeClientVPNTargetNetworks where
  toHeaders = const mempty

instance ToPath DescribeClientVPNTargetNetworks where
  toPath = const "/"

instance ToQuery DescribeClientVPNTargetNetworks where
  toQuery DescribeClientVPNTargetNetworks' {..} =
    mconcat
      [ "Action" =: ("DescribeClientVpnTargetNetworks" :: ByteString),
        "Version" =: ("2016-11-15" :: ByteString),
        toQuery (toQueryList "Filter" <$> _dcvtnFilters),
        "NextToken" =: _dcvtnNextToken,
        toQuery (toQueryList "AssociationIds" <$> _dcvtnAssociationIds),
        "DryRun" =: _dcvtnDryRun,
        "MaxResults" =: _dcvtnMaxResults,
        "ClientVpnEndpointId" =: _dcvtnClientVPNEndpointId
      ]

-- | /See:/ 'describeClientVPNTargetNetworksResponse' smart constructor.
data DescribeClientVPNTargetNetworksResponse = DescribeClientVPNTargetNetworksResponse'
  { _dcvtnrsClientVPNTargetNetworks ::
      !( Maybe
           [TargetNetwork]
       ),
    _dcvtnrsNextToken ::
      !( Maybe
           Text
       ),
    _dcvtnrsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeClientVPNTargetNetworksResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcvtnrsClientVPNTargetNetworks' - Information about the associated target networks.
--
-- * 'dcvtnrsNextToken' - The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- * 'dcvtnrsResponseStatus' - -- | The response status code.
describeClientVPNTargetNetworksResponse ::
  -- | 'dcvtnrsResponseStatus'
  Int ->
  DescribeClientVPNTargetNetworksResponse
describeClientVPNTargetNetworksResponse pResponseStatus_ =
  DescribeClientVPNTargetNetworksResponse'
    { _dcvtnrsClientVPNTargetNetworks =
        Nothing,
      _dcvtnrsNextToken = Nothing,
      _dcvtnrsResponseStatus = pResponseStatus_
    }

-- | Information about the associated target networks.
dcvtnrsClientVPNTargetNetworks :: Lens' DescribeClientVPNTargetNetworksResponse [TargetNetwork]
dcvtnrsClientVPNTargetNetworks = lens _dcvtnrsClientVPNTargetNetworks (\s a -> s {_dcvtnrsClientVPNTargetNetworks = a}) . _Default . _Coerce

-- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
dcvtnrsNextToken :: Lens' DescribeClientVPNTargetNetworksResponse (Maybe Text)
dcvtnrsNextToken = lens _dcvtnrsNextToken (\s a -> s {_dcvtnrsNextToken = a})

-- | -- | The response status code.
dcvtnrsResponseStatus :: Lens' DescribeClientVPNTargetNetworksResponse Int
dcvtnrsResponseStatus = lens _dcvtnrsResponseStatus (\s a -> s {_dcvtnrsResponseStatus = a})

instance NFData DescribeClientVPNTargetNetworksResponse
