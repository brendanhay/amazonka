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
    gcruDryRun,
    gcruMaxResults,
    gcruNextToken,

    -- * Destructuring the response
    GetCapacityReservationUsageResponse (..),
    mkGetCapacityReservationUsageResponse,

    -- ** Response lenses
    gcrurrsAvailableInstanceCount,
    gcrurrsCapacityReservationId,
    gcrurrsInstanceType,
    gcrurrsInstanceUsages,
    gcrurrsNextToken,
    gcrurrsState,
    gcrurrsTotalInstanceCount,
    gcrurrsResponseStatus,
  )
where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetCapacityReservationUsage' smart constructor.
data GetCapacityReservationUsage = GetCapacityReservationUsage'
  { -- | The ID of the Capacity Reservation.
    capacityReservationId :: Types.CapacityReservationId,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Core.Maybe Core.Bool,
    -- | The maximum number of results to return for the request in a single page. The remaining results can be seen by sending another request with the returned @nextToken@ value. This value can be between 5 and 500. If @maxResults@ is given a larger value than 500, you receive an error.
    --
    -- Valid range: Minimum value of 1. Maximum value of 1000.
    maxResults :: Core.Maybe Core.Natural,
    -- | The token to use to retrieve the next page of results.
    nextToken :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetCapacityReservationUsage' value with any optional fields omitted.
mkGetCapacityReservationUsage ::
  -- | 'capacityReservationId'
  Types.CapacityReservationId ->
  GetCapacityReservationUsage
mkGetCapacityReservationUsage capacityReservationId =
  GetCapacityReservationUsage'
    { capacityReservationId,
      dryRun = Core.Nothing,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | The ID of the Capacity Reservation.
--
-- /Note:/ Consider using 'capacityReservationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcruCapacityReservationId :: Lens.Lens' GetCapacityReservationUsage Types.CapacityReservationId
gcruCapacityReservationId = Lens.field @"capacityReservationId"
{-# DEPRECATED gcruCapacityReservationId "Use generic-lens or generic-optics with 'capacityReservationId' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcruDryRun :: Lens.Lens' GetCapacityReservationUsage (Core.Maybe Core.Bool)
gcruDryRun = Lens.field @"dryRun"
{-# DEPRECATED gcruDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The maximum number of results to return for the request in a single page. The remaining results can be seen by sending another request with the returned @nextToken@ value. This value can be between 5 and 500. If @maxResults@ is given a larger value than 500, you receive an error.
--
-- Valid range: Minimum value of 1. Maximum value of 1000.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcruMaxResults :: Lens.Lens' GetCapacityReservationUsage (Core.Maybe Core.Natural)
gcruMaxResults = Lens.field @"maxResults"
{-# DEPRECATED gcruMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The token to use to retrieve the next page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcruNextToken :: Lens.Lens' GetCapacityReservationUsage (Core.Maybe Types.String)
gcruNextToken = Lens.field @"nextToken"
{-# DEPRECATED gcruNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.AWSRequest GetCapacityReservationUsage where
  type
    Rs GetCapacityReservationUsage =
      GetCapacityReservationUsageResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Action", "GetCapacityReservationUsage")
                Core.<> (Core.pure ("Version", "2016-11-15"))
                Core.<> (Core.toQueryValue "CapacityReservationId" capacityReservationId)
                Core.<> (Core.toQueryValue "DryRun" Core.<$> dryRun)
                Core.<> (Core.toQueryValue "MaxResults" Core.<$> maxResults)
                Core.<> (Core.toQueryValue "NextToken" Core.<$> nextToken)
            )
      }
  response =
    Response.receiveXML
      ( \s h x ->
          GetCapacityReservationUsageResponse'
            Core.<$> (x Core..@? "availableInstanceCount")
            Core.<*> (x Core..@? "capacityReservationId")
            Core.<*> (x Core..@? "instanceType")
            Core.<*> (x Core..@? "instanceUsageSet" Core..<@> Core.parseXMLList "item")
            Core.<*> (x Core..@? "nextToken")
            Core.<*> (x Core..@? "state")
            Core.<*> (x Core..@? "totalInstanceCount")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetCapacityReservationUsageResponse' smart constructor.
data GetCapacityReservationUsageResponse = GetCapacityReservationUsageResponse'
  { -- | The remaining capacity. Indicates the number of instances that can be launched in the Capacity Reservation.
    availableInstanceCount :: Core.Maybe Core.Int,
    -- | The ID of the Capacity Reservation.
    capacityReservationId :: Core.Maybe Types.String,
    -- | The type of instance for which the Capacity Reservation reserves capacity.
    instanceType :: Core.Maybe Types.String,
    -- | Information about the Capacity Reservation usage.
    instanceUsages :: Core.Maybe [Types.InstanceUsage],
    -- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
    nextToken :: Core.Maybe Types.String,
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
    state :: Core.Maybe Types.CapacityReservationState,
    -- | The number of instances for which the Capacity Reservation reserves capacity.
    totalInstanceCount :: Core.Maybe Core.Int,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetCapacityReservationUsageResponse' value with any optional fields omitted.
mkGetCapacityReservationUsageResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetCapacityReservationUsageResponse
mkGetCapacityReservationUsageResponse responseStatus =
  GetCapacityReservationUsageResponse'
    { availableInstanceCount =
        Core.Nothing,
      capacityReservationId = Core.Nothing,
      instanceType = Core.Nothing,
      instanceUsages = Core.Nothing,
      nextToken = Core.Nothing,
      state = Core.Nothing,
      totalInstanceCount = Core.Nothing,
      responseStatus
    }

-- | The remaining capacity. Indicates the number of instances that can be launched in the Capacity Reservation.
--
-- /Note:/ Consider using 'availableInstanceCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcrurrsAvailableInstanceCount :: Lens.Lens' GetCapacityReservationUsageResponse (Core.Maybe Core.Int)
gcrurrsAvailableInstanceCount = Lens.field @"availableInstanceCount"
{-# DEPRECATED gcrurrsAvailableInstanceCount "Use generic-lens or generic-optics with 'availableInstanceCount' instead." #-}

-- | The ID of the Capacity Reservation.
--
-- /Note:/ Consider using 'capacityReservationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcrurrsCapacityReservationId :: Lens.Lens' GetCapacityReservationUsageResponse (Core.Maybe Types.String)
gcrurrsCapacityReservationId = Lens.field @"capacityReservationId"
{-# DEPRECATED gcrurrsCapacityReservationId "Use generic-lens or generic-optics with 'capacityReservationId' instead." #-}

-- | The type of instance for which the Capacity Reservation reserves capacity.
--
-- /Note:/ Consider using 'instanceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcrurrsInstanceType :: Lens.Lens' GetCapacityReservationUsageResponse (Core.Maybe Types.String)
gcrurrsInstanceType = Lens.field @"instanceType"
{-# DEPRECATED gcrurrsInstanceType "Use generic-lens or generic-optics with 'instanceType' instead." #-}

-- | Information about the Capacity Reservation usage.
--
-- /Note:/ Consider using 'instanceUsages' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcrurrsInstanceUsages :: Lens.Lens' GetCapacityReservationUsageResponse (Core.Maybe [Types.InstanceUsage])
gcrurrsInstanceUsages = Lens.field @"instanceUsages"
{-# DEPRECATED gcrurrsInstanceUsages "Use generic-lens or generic-optics with 'instanceUsages' instead." #-}

-- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcrurrsNextToken :: Lens.Lens' GetCapacityReservationUsageResponse (Core.Maybe Types.String)
gcrurrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED gcrurrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

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
gcrurrsState :: Lens.Lens' GetCapacityReservationUsageResponse (Core.Maybe Types.CapacityReservationState)
gcrurrsState = Lens.field @"state"
{-# DEPRECATED gcrurrsState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | The number of instances for which the Capacity Reservation reserves capacity.
--
-- /Note:/ Consider using 'totalInstanceCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcrurrsTotalInstanceCount :: Lens.Lens' GetCapacityReservationUsageResponse (Core.Maybe Core.Int)
gcrurrsTotalInstanceCount = Lens.field @"totalInstanceCount"
{-# DEPRECATED gcrurrsTotalInstanceCount "Use generic-lens or generic-optics with 'totalInstanceCount' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcrurrsResponseStatus :: Lens.Lens' GetCapacityReservationUsageResponse Core.Int
gcrurrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gcrurrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
