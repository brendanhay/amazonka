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
-- Module      : Network.AWS.EC2.GetCapacityReservationUsage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets usage information about a Capacity Reservation. If the Capacity Reservation is shared, it shows usage information for the Capacity Reservation owner and each AWS account that is currently using the shared capacity. If the Capacity Reservation is not shared, it shows only the Capacity Reservation owner's usage.
module Network.AWS.EC2.GetCapacityReservationUsage
  ( -- * Creating a Request
    getCapacityReservationUsage,
    GetCapacityReservationUsage,

    -- * Request Lenses
    gcruNextToken,
    gcruDryRun,
    gcruMaxResults,
    gcruCapacityReservationId,

    -- * Destructuring the Response
    getCapacityReservationUsageResponse,
    GetCapacityReservationUsageResponse,

    -- * Response Lenses
    gcrursState,
    gcrursInstanceUsages,
    gcrursAvailableInstanceCount,
    gcrursCapacityReservationId,
    gcrursInstanceType,
    gcrursNextToken,
    gcrursTotalInstanceCount,
    gcrursResponseStatus,
  )
where

import Network.AWS.EC2.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getCapacityReservationUsage' smart constructor.
data GetCapacityReservationUsage = GetCapacityReservationUsage'
  { _gcruNextToken ::
      !(Maybe Text),
    _gcruDryRun :: !(Maybe Bool),
    _gcruMaxResults :: !(Maybe Nat),
    _gcruCapacityReservationId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetCapacityReservationUsage' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gcruNextToken' - The token to use to retrieve the next page of results.
--
-- * 'gcruDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'gcruMaxResults' - The maximum number of results to return for the request in a single page. The remaining results can be seen by sending another request with the returned @nextToken@ value. This value can be between 5 and 500. If @maxResults@ is given a larger value than 500, you receive an error. Valid range: Minimum value of 1. Maximum value of 1000.
--
-- * 'gcruCapacityReservationId' - The ID of the Capacity Reservation.
getCapacityReservationUsage ::
  -- | 'gcruCapacityReservationId'
  Text ->
  GetCapacityReservationUsage
getCapacityReservationUsage pCapacityReservationId_ =
  GetCapacityReservationUsage'
    { _gcruNextToken = Nothing,
      _gcruDryRun = Nothing,
      _gcruMaxResults = Nothing,
      _gcruCapacityReservationId = pCapacityReservationId_
    }

-- | The token to use to retrieve the next page of results.
gcruNextToken :: Lens' GetCapacityReservationUsage (Maybe Text)
gcruNextToken = lens _gcruNextToken (\s a -> s {_gcruNextToken = a})

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
gcruDryRun :: Lens' GetCapacityReservationUsage (Maybe Bool)
gcruDryRun = lens _gcruDryRun (\s a -> s {_gcruDryRun = a})

-- | The maximum number of results to return for the request in a single page. The remaining results can be seen by sending another request with the returned @nextToken@ value. This value can be between 5 and 500. If @maxResults@ is given a larger value than 500, you receive an error. Valid range: Minimum value of 1. Maximum value of 1000.
gcruMaxResults :: Lens' GetCapacityReservationUsage (Maybe Natural)
gcruMaxResults = lens _gcruMaxResults (\s a -> s {_gcruMaxResults = a}) . mapping _Nat

-- | The ID of the Capacity Reservation.
gcruCapacityReservationId :: Lens' GetCapacityReservationUsage Text
gcruCapacityReservationId = lens _gcruCapacityReservationId (\s a -> s {_gcruCapacityReservationId = a})

instance AWSRequest GetCapacityReservationUsage where
  type
    Rs GetCapacityReservationUsage =
      GetCapacityReservationUsageResponse
  request = postQuery ec2
  response =
    receiveXML
      ( \s h x ->
          GetCapacityReservationUsageResponse'
            <$> (x .@? "state")
            <*> (x .@? "instanceUsageSet" .!@ mempty >>= may (parseXMLList "item"))
            <*> (x .@? "availableInstanceCount")
            <*> (x .@? "capacityReservationId")
            <*> (x .@? "instanceType")
            <*> (x .@? "nextToken")
            <*> (x .@? "totalInstanceCount")
            <*> (pure (fromEnum s))
      )

instance Hashable GetCapacityReservationUsage

instance NFData GetCapacityReservationUsage

instance ToHeaders GetCapacityReservationUsage where
  toHeaders = const mempty

instance ToPath GetCapacityReservationUsage where
  toPath = const "/"

instance ToQuery GetCapacityReservationUsage where
  toQuery GetCapacityReservationUsage' {..} =
    mconcat
      [ "Action" =: ("GetCapacityReservationUsage" :: ByteString),
        "Version" =: ("2016-11-15" :: ByteString),
        "NextToken" =: _gcruNextToken,
        "DryRun" =: _gcruDryRun,
        "MaxResults" =: _gcruMaxResults,
        "CapacityReservationId" =: _gcruCapacityReservationId
      ]

-- | /See:/ 'getCapacityReservationUsageResponse' smart constructor.
data GetCapacityReservationUsageResponse = GetCapacityReservationUsageResponse'
  { _gcrursState ::
      !( Maybe
           CapacityReservationState
       ),
    _gcrursInstanceUsages ::
      !( Maybe
           [InstanceUsage]
       ),
    _gcrursAvailableInstanceCount ::
      !(Maybe Int),
    _gcrursCapacityReservationId ::
      !(Maybe Text),
    _gcrursInstanceType ::
      !(Maybe Text),
    _gcrursNextToken ::
      !(Maybe Text),
    _gcrursTotalInstanceCount ::
      !(Maybe Int),
    _gcrursResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetCapacityReservationUsageResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gcrursState' - The current state of the Capacity Reservation. A Capacity Reservation can be in one of the following states:     * @active@ - The Capacity Reservation is active and the capacity is available for your use.     * @expired@ - The Capacity Reservation expired automatically at the date and time specified in your request. The reserved capacity is no longer available for your use.     * @cancelled@ - The Capacity Reservation was manually cancelled. The reserved capacity is no longer available for your use.     * @pending@ - The Capacity Reservation request was successful but the capacity provisioning is still pending.     * @failed@ - The Capacity Reservation request has failed. A request might fail due to invalid request parameters, capacity constraints, or instance limit constraints. Failed requests are retained for 60 minutes.
--
-- * 'gcrursInstanceUsages' - Information about the Capacity Reservation usage.
--
-- * 'gcrursAvailableInstanceCount' - The remaining capacity. Indicates the number of instances that can be launched in the Capacity Reservation.
--
-- * 'gcrursCapacityReservationId' - The ID of the Capacity Reservation.
--
-- * 'gcrursInstanceType' - The type of instance for which the Capacity Reservation reserves capacity.
--
-- * 'gcrursNextToken' - The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- * 'gcrursTotalInstanceCount' - The number of instances for which the Capacity Reservation reserves capacity.
--
-- * 'gcrursResponseStatus' - -- | The response status code.
getCapacityReservationUsageResponse ::
  -- | 'gcrursResponseStatus'
  Int ->
  GetCapacityReservationUsageResponse
getCapacityReservationUsageResponse pResponseStatus_ =
  GetCapacityReservationUsageResponse'
    { _gcrursState = Nothing,
      _gcrursInstanceUsages = Nothing,
      _gcrursAvailableInstanceCount = Nothing,
      _gcrursCapacityReservationId = Nothing,
      _gcrursInstanceType = Nothing,
      _gcrursNextToken = Nothing,
      _gcrursTotalInstanceCount = Nothing,
      _gcrursResponseStatus = pResponseStatus_
    }

-- | The current state of the Capacity Reservation. A Capacity Reservation can be in one of the following states:     * @active@ - The Capacity Reservation is active and the capacity is available for your use.     * @expired@ - The Capacity Reservation expired automatically at the date and time specified in your request. The reserved capacity is no longer available for your use.     * @cancelled@ - The Capacity Reservation was manually cancelled. The reserved capacity is no longer available for your use.     * @pending@ - The Capacity Reservation request was successful but the capacity provisioning is still pending.     * @failed@ - The Capacity Reservation request has failed. A request might fail due to invalid request parameters, capacity constraints, or instance limit constraints. Failed requests are retained for 60 minutes.
gcrursState :: Lens' GetCapacityReservationUsageResponse (Maybe CapacityReservationState)
gcrursState = lens _gcrursState (\s a -> s {_gcrursState = a})

-- | Information about the Capacity Reservation usage.
gcrursInstanceUsages :: Lens' GetCapacityReservationUsageResponse [InstanceUsage]
gcrursInstanceUsages = lens _gcrursInstanceUsages (\s a -> s {_gcrursInstanceUsages = a}) . _Default . _Coerce

-- | The remaining capacity. Indicates the number of instances that can be launched in the Capacity Reservation.
gcrursAvailableInstanceCount :: Lens' GetCapacityReservationUsageResponse (Maybe Int)
gcrursAvailableInstanceCount = lens _gcrursAvailableInstanceCount (\s a -> s {_gcrursAvailableInstanceCount = a})

-- | The ID of the Capacity Reservation.
gcrursCapacityReservationId :: Lens' GetCapacityReservationUsageResponse (Maybe Text)
gcrursCapacityReservationId = lens _gcrursCapacityReservationId (\s a -> s {_gcrursCapacityReservationId = a})

-- | The type of instance for which the Capacity Reservation reserves capacity.
gcrursInstanceType :: Lens' GetCapacityReservationUsageResponse (Maybe Text)
gcrursInstanceType = lens _gcrursInstanceType (\s a -> s {_gcrursInstanceType = a})

-- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
gcrursNextToken :: Lens' GetCapacityReservationUsageResponse (Maybe Text)
gcrursNextToken = lens _gcrursNextToken (\s a -> s {_gcrursNextToken = a})

-- | The number of instances for which the Capacity Reservation reserves capacity.
gcrursTotalInstanceCount :: Lens' GetCapacityReservationUsageResponse (Maybe Int)
gcrursTotalInstanceCount = lens _gcrursTotalInstanceCount (\s a -> s {_gcrursTotalInstanceCount = a})

-- | -- | The response status code.
gcrursResponseStatus :: Lens' GetCapacityReservationUsageResponse Int
gcrursResponseStatus = lens _gcrursResponseStatus (\s a -> s {_gcrursResponseStatus = a})

instance NFData GetCapacityReservationUsageResponse
