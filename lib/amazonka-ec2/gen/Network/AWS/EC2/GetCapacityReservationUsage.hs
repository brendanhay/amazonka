{-# OPTIONS_GHC -fno-warn-deprecations #-}
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
  ( -- * Creating a request
    GetCapacityReservationUsage (..),
    mkGetCapacityReservationUsage,

    -- ** Request lenses
    gcruCapacityReservationId,
    gcruNextToken,
    gcruDryRun,
    gcruMaxResults,

    -- * Destructuring the response
    GetCapacityReservationUsageResponse (..),
    mkGetCapacityReservationUsageResponse,

    -- ** Response lenses
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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetCapacityReservationUsage' smart constructor.
data GetCapacityReservationUsage = GetCapacityReservationUsage'
  { -- | The ID of the Capacity Reservation.
    capacityReservationId :: Lude.Text,
    -- | The token to use to retrieve the next page of results.
    nextToken :: Lude.Maybe Lude.Text,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Lude.Maybe Lude.Bool,
    -- | The maximum number of results to return for the request in a single page. The remaining results can be seen by sending another request with the returned @nextToken@ value. This value can be between 5 and 500. If @maxResults@ is given a larger value than 500, you receive an error.
    --
    -- Valid range: Minimum value of 1. Maximum value of 1000.
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetCapacityReservationUsage' with the minimum fields required to make a request.
--
-- * 'capacityReservationId' - The ID of the Capacity Reservation.
-- * 'nextToken' - The token to use to retrieve the next page of results.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'maxResults' - The maximum number of results to return for the request in a single page. The remaining results can be seen by sending another request with the returned @nextToken@ value. This value can be between 5 and 500. If @maxResults@ is given a larger value than 500, you receive an error.
--
-- Valid range: Minimum value of 1. Maximum value of 1000.
mkGetCapacityReservationUsage ::
  -- | 'capacityReservationId'
  Lude.Text ->
  GetCapacityReservationUsage
mkGetCapacityReservationUsage pCapacityReservationId_ =
  GetCapacityReservationUsage'
    { capacityReservationId =
        pCapacityReservationId_,
      nextToken = Lude.Nothing,
      dryRun = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | The ID of the Capacity Reservation.
--
-- /Note:/ Consider using 'capacityReservationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcruCapacityReservationId :: Lens.Lens' GetCapacityReservationUsage Lude.Text
gcruCapacityReservationId = Lens.lens (capacityReservationId :: GetCapacityReservationUsage -> Lude.Text) (\s a -> s {capacityReservationId = a} :: GetCapacityReservationUsage)
{-# DEPRECATED gcruCapacityReservationId "Use generic-lens or generic-optics with 'capacityReservationId' instead." #-}

-- | The token to use to retrieve the next page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcruNextToken :: Lens.Lens' GetCapacityReservationUsage (Lude.Maybe Lude.Text)
gcruNextToken = Lens.lens (nextToken :: GetCapacityReservationUsage -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: GetCapacityReservationUsage)
{-# DEPRECATED gcruNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcruDryRun :: Lens.Lens' GetCapacityReservationUsage (Lude.Maybe Lude.Bool)
gcruDryRun = Lens.lens (dryRun :: GetCapacityReservationUsage -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: GetCapacityReservationUsage)
{-# DEPRECATED gcruDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The maximum number of results to return for the request in a single page. The remaining results can be seen by sending another request with the returned @nextToken@ value. This value can be between 5 and 500. If @maxResults@ is given a larger value than 500, you receive an error.
--
-- Valid range: Minimum value of 1. Maximum value of 1000.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcruMaxResults :: Lens.Lens' GetCapacityReservationUsage (Lude.Maybe Lude.Natural)
gcruMaxResults = Lens.lens (maxResults :: GetCapacityReservationUsage -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: GetCapacityReservationUsage)
{-# DEPRECATED gcruMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Lude.AWSRequest GetCapacityReservationUsage where
  type
    Rs GetCapacityReservationUsage =
      GetCapacityReservationUsageResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          GetCapacityReservationUsageResponse'
            Lude.<$> (x Lude..@? "state")
            Lude.<*> ( x Lude..@? "instanceUsageSet" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "item")
                     )
            Lude.<*> (x Lude..@? "availableInstanceCount")
            Lude.<*> (x Lude..@? "capacityReservationId")
            Lude.<*> (x Lude..@? "instanceType")
            Lude.<*> (x Lude..@? "nextToken")
            Lude.<*> (x Lude..@? "totalInstanceCount")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetCapacityReservationUsage where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath GetCapacityReservationUsage where
  toPath = Lude.const "/"

instance Lude.ToQuery GetCapacityReservationUsage where
  toQuery GetCapacityReservationUsage' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("GetCapacityReservationUsage" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "CapacityReservationId" Lude.=: capacityReservationId,
        "NextToken" Lude.=: nextToken,
        "DryRun" Lude.=: dryRun,
        "MaxResults" Lude.=: maxResults
      ]

-- | /See:/ 'mkGetCapacityReservationUsageResponse' smart constructor.
data GetCapacityReservationUsageResponse = GetCapacityReservationUsageResponse'
  { -- | The current state of the Capacity Reservation. A Capacity Reservation can be in one of the following states:
    --
    --
    --     * @active@ - The Capacity Reservation is active and the capacity is available for your use.
    --
    --
    --     * @expired@ - The Capacity Reservation expired automatically at the date and time specified in your request. The reserved capacity is no longer available for your use.
    --
    --
    --     * @cancelled@ - The Capacity Reservation was manually cancelled. The reserved capacity is no longer available for your use.
    --
    --
    --     * @pending@ - The Capacity Reservation request was successful but the capacity provisioning is still pending.
    --
    --
    --     * @failed@ - The Capacity Reservation request has failed. A request might fail due to invalid request parameters, capacity constraints, or instance limit constraints. Failed requests are retained for 60 minutes.
    state :: Lude.Maybe CapacityReservationState,
    -- | Information about the Capacity Reservation usage.
    instanceUsages :: Lude.Maybe [InstanceUsage],
    -- | The remaining capacity. Indicates the number of instances that can be launched in the Capacity Reservation.
    availableInstanceCount :: Lude.Maybe Lude.Int,
    -- | The ID of the Capacity Reservation.
    capacityReservationId :: Lude.Maybe Lude.Text,
    -- | The type of instance for which the Capacity Reservation reserves capacity.
    instanceType :: Lude.Maybe Lude.Text,
    -- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The number of instances for which the Capacity Reservation reserves capacity.
    totalInstanceCount :: Lude.Maybe Lude.Int,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetCapacityReservationUsageResponse' with the minimum fields required to make a request.
--
-- * 'state' - The current state of the Capacity Reservation. A Capacity Reservation can be in one of the following states:
--
--
--     * @active@ - The Capacity Reservation is active and the capacity is available for your use.
--
--
--     * @expired@ - The Capacity Reservation expired automatically at the date and time specified in your request. The reserved capacity is no longer available for your use.
--
--
--     * @cancelled@ - The Capacity Reservation was manually cancelled. The reserved capacity is no longer available for your use.
--
--
--     * @pending@ - The Capacity Reservation request was successful but the capacity provisioning is still pending.
--
--
--     * @failed@ - The Capacity Reservation request has failed. A request might fail due to invalid request parameters, capacity constraints, or instance limit constraints. Failed requests are retained for 60 minutes.
--
--
-- * 'instanceUsages' - Information about the Capacity Reservation usage.
-- * 'availableInstanceCount' - The remaining capacity. Indicates the number of instances that can be launched in the Capacity Reservation.
-- * 'capacityReservationId' - The ID of the Capacity Reservation.
-- * 'instanceType' - The type of instance for which the Capacity Reservation reserves capacity.
-- * 'nextToken' - The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
-- * 'totalInstanceCount' - The number of instances for which the Capacity Reservation reserves capacity.
-- * 'responseStatus' - The response status code.
mkGetCapacityReservationUsageResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetCapacityReservationUsageResponse
mkGetCapacityReservationUsageResponse pResponseStatus_ =
  GetCapacityReservationUsageResponse'
    { state = Lude.Nothing,
      instanceUsages = Lude.Nothing,
      availableInstanceCount = Lude.Nothing,
      capacityReservationId = Lude.Nothing,
      instanceType = Lude.Nothing,
      nextToken = Lude.Nothing,
      totalInstanceCount = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The current state of the Capacity Reservation. A Capacity Reservation can be in one of the following states:
--
--
--     * @active@ - The Capacity Reservation is active and the capacity is available for your use.
--
--
--     * @expired@ - The Capacity Reservation expired automatically at the date and time specified in your request. The reserved capacity is no longer available for your use.
--
--
--     * @cancelled@ - The Capacity Reservation was manually cancelled. The reserved capacity is no longer available for your use.
--
--
--     * @pending@ - The Capacity Reservation request was successful but the capacity provisioning is still pending.
--
--
--     * @failed@ - The Capacity Reservation request has failed. A request might fail due to invalid request parameters, capacity constraints, or instance limit constraints. Failed requests are retained for 60 minutes.
--
--
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcrursState :: Lens.Lens' GetCapacityReservationUsageResponse (Lude.Maybe CapacityReservationState)
gcrursState = Lens.lens (state :: GetCapacityReservationUsageResponse -> Lude.Maybe CapacityReservationState) (\s a -> s {state = a} :: GetCapacityReservationUsageResponse)
{-# DEPRECATED gcrursState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | Information about the Capacity Reservation usage.
--
-- /Note:/ Consider using 'instanceUsages' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcrursInstanceUsages :: Lens.Lens' GetCapacityReservationUsageResponse (Lude.Maybe [InstanceUsage])
gcrursInstanceUsages = Lens.lens (instanceUsages :: GetCapacityReservationUsageResponse -> Lude.Maybe [InstanceUsage]) (\s a -> s {instanceUsages = a} :: GetCapacityReservationUsageResponse)
{-# DEPRECATED gcrursInstanceUsages "Use generic-lens or generic-optics with 'instanceUsages' instead." #-}

-- | The remaining capacity. Indicates the number of instances that can be launched in the Capacity Reservation.
--
-- /Note:/ Consider using 'availableInstanceCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcrursAvailableInstanceCount :: Lens.Lens' GetCapacityReservationUsageResponse (Lude.Maybe Lude.Int)
gcrursAvailableInstanceCount = Lens.lens (availableInstanceCount :: GetCapacityReservationUsageResponse -> Lude.Maybe Lude.Int) (\s a -> s {availableInstanceCount = a} :: GetCapacityReservationUsageResponse)
{-# DEPRECATED gcrursAvailableInstanceCount "Use generic-lens or generic-optics with 'availableInstanceCount' instead." #-}

-- | The ID of the Capacity Reservation.
--
-- /Note:/ Consider using 'capacityReservationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcrursCapacityReservationId :: Lens.Lens' GetCapacityReservationUsageResponse (Lude.Maybe Lude.Text)
gcrursCapacityReservationId = Lens.lens (capacityReservationId :: GetCapacityReservationUsageResponse -> Lude.Maybe Lude.Text) (\s a -> s {capacityReservationId = a} :: GetCapacityReservationUsageResponse)
{-# DEPRECATED gcrursCapacityReservationId "Use generic-lens or generic-optics with 'capacityReservationId' instead." #-}

-- | The type of instance for which the Capacity Reservation reserves capacity.
--
-- /Note:/ Consider using 'instanceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcrursInstanceType :: Lens.Lens' GetCapacityReservationUsageResponse (Lude.Maybe Lude.Text)
gcrursInstanceType = Lens.lens (instanceType :: GetCapacityReservationUsageResponse -> Lude.Maybe Lude.Text) (\s a -> s {instanceType = a} :: GetCapacityReservationUsageResponse)
{-# DEPRECATED gcrursInstanceType "Use generic-lens or generic-optics with 'instanceType' instead." #-}

-- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcrursNextToken :: Lens.Lens' GetCapacityReservationUsageResponse (Lude.Maybe Lude.Text)
gcrursNextToken = Lens.lens (nextToken :: GetCapacityReservationUsageResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: GetCapacityReservationUsageResponse)
{-# DEPRECATED gcrursNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The number of instances for which the Capacity Reservation reserves capacity.
--
-- /Note:/ Consider using 'totalInstanceCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcrursTotalInstanceCount :: Lens.Lens' GetCapacityReservationUsageResponse (Lude.Maybe Lude.Int)
gcrursTotalInstanceCount = Lens.lens (totalInstanceCount :: GetCapacityReservationUsageResponse -> Lude.Maybe Lude.Int) (\s a -> s {totalInstanceCount = a} :: GetCapacityReservationUsageResponse)
{-# DEPRECATED gcrursTotalInstanceCount "Use generic-lens or generic-optics with 'totalInstanceCount' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcrursResponseStatus :: Lens.Lens' GetCapacityReservationUsageResponse Lude.Int
gcrursResponseStatus = Lens.lens (responseStatus :: GetCapacityReservationUsageResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetCapacityReservationUsageResponse)
{-# DEPRECATED gcrursResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
