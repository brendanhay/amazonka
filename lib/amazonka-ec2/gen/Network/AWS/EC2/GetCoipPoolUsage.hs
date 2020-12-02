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
-- Module      : Network.AWS.EC2.GetCoipPoolUsage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the allocations from the specified customer-owned address pool.
module Network.AWS.EC2.GetCoipPoolUsage
  ( -- * Creating a Request
    getCoipPoolUsage,
    GetCoipPoolUsage,

    -- * Request Lenses
    gcpuFilters,
    gcpuNextToken,
    gcpuDryRun,
    gcpuMaxResults,
    gcpuPoolId,

    -- * Destructuring the Response
    getCoipPoolUsageResponse,
    GetCoipPoolUsageResponse,

    -- * Response Lenses
    gcpursCoipAddressUsages,
    gcpursCoipPoolId,
    gcpursLocalGatewayRouteTableId,
    gcpursResponseStatus,
  )
where

import Network.AWS.EC2.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getCoipPoolUsage' smart constructor.
data GetCoipPoolUsage = GetCoipPoolUsage'
  { _gcpuFilters ::
      !(Maybe [Filter]),
    _gcpuNextToken :: !(Maybe Text),
    _gcpuDryRun :: !(Maybe Bool),
    _gcpuMaxResults :: !(Maybe Nat),
    _gcpuPoolId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetCoipPoolUsage' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gcpuFilters' - The filters. The following are the possible values:     * @coip-address-usage.allocation-id@      * @coip-address-usage.aws-account-id@      * @coip-address-usage.aws-service@      * @coip-address-usage.co-ip@
--
-- * 'gcpuNextToken' - The token for the next page of results.
--
-- * 'gcpuDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'gcpuMaxResults' - The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
--
-- * 'gcpuPoolId' - The ID of the address pool.
getCoipPoolUsage ::
  -- | 'gcpuPoolId'
  Text ->
  GetCoipPoolUsage
getCoipPoolUsage pPoolId_ =
  GetCoipPoolUsage'
    { _gcpuFilters = Nothing,
      _gcpuNextToken = Nothing,
      _gcpuDryRun = Nothing,
      _gcpuMaxResults = Nothing,
      _gcpuPoolId = pPoolId_
    }

-- | The filters. The following are the possible values:     * @coip-address-usage.allocation-id@      * @coip-address-usage.aws-account-id@      * @coip-address-usage.aws-service@      * @coip-address-usage.co-ip@
gcpuFilters :: Lens' GetCoipPoolUsage [Filter]
gcpuFilters = lens _gcpuFilters (\s a -> s {_gcpuFilters = a}) . _Default . _Coerce

-- | The token for the next page of results.
gcpuNextToken :: Lens' GetCoipPoolUsage (Maybe Text)
gcpuNextToken = lens _gcpuNextToken (\s a -> s {_gcpuNextToken = a})

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
gcpuDryRun :: Lens' GetCoipPoolUsage (Maybe Bool)
gcpuDryRun = lens _gcpuDryRun (\s a -> s {_gcpuDryRun = a})

-- | The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
gcpuMaxResults :: Lens' GetCoipPoolUsage (Maybe Natural)
gcpuMaxResults = lens _gcpuMaxResults (\s a -> s {_gcpuMaxResults = a}) . mapping _Nat

-- | The ID of the address pool.
gcpuPoolId :: Lens' GetCoipPoolUsage Text
gcpuPoolId = lens _gcpuPoolId (\s a -> s {_gcpuPoolId = a})

instance AWSRequest GetCoipPoolUsage where
  type Rs GetCoipPoolUsage = GetCoipPoolUsageResponse
  request = postQuery ec2
  response =
    receiveXML
      ( \s h x ->
          GetCoipPoolUsageResponse'
            <$> ( x .@? "coipAddressUsageSet" .!@ mempty
                    >>= may (parseXMLList "item")
                )
            <*> (x .@? "coipPoolId")
            <*> (x .@? "localGatewayRouteTableId")
            <*> (pure (fromEnum s))
      )

instance Hashable GetCoipPoolUsage

instance NFData GetCoipPoolUsage

instance ToHeaders GetCoipPoolUsage where
  toHeaders = const mempty

instance ToPath GetCoipPoolUsage where
  toPath = const "/"

instance ToQuery GetCoipPoolUsage where
  toQuery GetCoipPoolUsage' {..} =
    mconcat
      [ "Action" =: ("GetCoipPoolUsage" :: ByteString),
        "Version" =: ("2016-11-15" :: ByteString),
        toQuery (toQueryList "Filter" <$> _gcpuFilters),
        "NextToken" =: _gcpuNextToken,
        "DryRun" =: _gcpuDryRun,
        "MaxResults" =: _gcpuMaxResults,
        "PoolId" =: _gcpuPoolId
      ]

-- | /See:/ 'getCoipPoolUsageResponse' smart constructor.
data GetCoipPoolUsageResponse = GetCoipPoolUsageResponse'
  { _gcpursCoipAddressUsages ::
      !(Maybe [CoipAddressUsage]),
    _gcpursCoipPoolId :: !(Maybe Text),
    _gcpursLocalGatewayRouteTableId ::
      !(Maybe Text),
    _gcpursResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetCoipPoolUsageResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gcpursCoipAddressUsages' - Information about the address usage.
--
-- * 'gcpursCoipPoolId' - The ID of the customer-owned address pool.
--
-- * 'gcpursLocalGatewayRouteTableId' - The ID of the local gateway route table.
--
-- * 'gcpursResponseStatus' - -- | The response status code.
getCoipPoolUsageResponse ::
  -- | 'gcpursResponseStatus'
  Int ->
  GetCoipPoolUsageResponse
getCoipPoolUsageResponse pResponseStatus_ =
  GetCoipPoolUsageResponse'
    { _gcpursCoipAddressUsages = Nothing,
      _gcpursCoipPoolId = Nothing,
      _gcpursLocalGatewayRouteTableId = Nothing,
      _gcpursResponseStatus = pResponseStatus_
    }

-- | Information about the address usage.
gcpursCoipAddressUsages :: Lens' GetCoipPoolUsageResponse [CoipAddressUsage]
gcpursCoipAddressUsages = lens _gcpursCoipAddressUsages (\s a -> s {_gcpursCoipAddressUsages = a}) . _Default . _Coerce

-- | The ID of the customer-owned address pool.
gcpursCoipPoolId :: Lens' GetCoipPoolUsageResponse (Maybe Text)
gcpursCoipPoolId = lens _gcpursCoipPoolId (\s a -> s {_gcpursCoipPoolId = a})

-- | The ID of the local gateway route table.
gcpursLocalGatewayRouteTableId :: Lens' GetCoipPoolUsageResponse (Maybe Text)
gcpursLocalGatewayRouteTableId = lens _gcpursLocalGatewayRouteTableId (\s a -> s {_gcpursLocalGatewayRouteTableId = a})

-- | -- | The response status code.
gcpursResponseStatus :: Lens' GetCoipPoolUsageResponse Int
gcpursResponseStatus = lens _gcpursResponseStatus (\s a -> s {_gcpursResponseStatus = a})

instance NFData GetCoipPoolUsageResponse
