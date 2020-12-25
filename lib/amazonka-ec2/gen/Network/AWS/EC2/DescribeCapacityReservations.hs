{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeCapacityReservations
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more of your Capacity Reservations. The results describe only the Capacity Reservations in the AWS Region that you're currently using.
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeCapacityReservations
  ( -- * Creating a request
    DescribeCapacityReservations (..),
    mkDescribeCapacityReservations,

    -- ** Request lenses
    dcrCapacityReservationIds,
    dcrDryRun,
    dcrFilters,
    dcrMaxResults,
    dcrNextToken,

    -- * Destructuring the response
    DescribeCapacityReservationsResponse (..),
    mkDescribeCapacityReservationsResponse,

    -- ** Response lenses
    dcrrrsCapacityReservations,
    dcrrrsNextToken,
    dcrrrsResponseStatus,
  )
where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeCapacityReservations' smart constructor.
data DescribeCapacityReservations = DescribeCapacityReservations'
  { -- | The ID of the Capacity Reservation.
    capacityReservationIds :: Core.Maybe [Types.CapacityReservationId],
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Core.Maybe Core.Bool,
    -- | One or more filters.
    --
    --
    --     * @instance-type@ - The type of instance for which the Capacity Reservation reserves capacity.
    --
    --
    --     * @owner-id@ - The ID of the AWS account that owns the Capacity Reservation.
    --
    --
    --     * @availability-zone-id@ - The Availability Zone ID of the Capacity Reservation.
    --
    --
    --     * @instance-platform@ - The type of operating system for which the Capacity Reservation reserves capacity.
    --
    --
    --     * @availability-zone@ - The Availability Zone ID of the Capacity Reservation.
    --
    --
    --     * @tenancy@ - Indicates the tenancy of the Capacity Reservation. A Capacity Reservation can have one of the following tenancy settings:
    --
    --     * @default@ - The Capacity Reservation is created on hardware that is shared with other AWS accounts.
    --
    --
    --     * @dedicated@ - The Capacity Reservation is created on single-tenant hardware that is dedicated to a single AWS account.
    --
    --
    --
    --
    --     * @state@ - The current state of the Capacity Reservation. A Capacity Reservation can be in one of the following states:
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
    --
    --     * @end-date@ - The date and time at which the Capacity Reservation expires. When a Capacity Reservation expires, the reserved capacity is released and you can no longer launch instances into it. The Capacity Reservation's state changes to expired when it reaches its end date and time.
    --
    --
    --     * @end-date-type@ - Indicates the way in which the Capacity Reservation ends. A Capacity Reservation can have one of the following end types:
    --
    --     * @unlimited@ - The Capacity Reservation remains active until you explicitly cancel it.
    --
    --
    --     * @limited@ - The Capacity Reservation expires automatically at a specified date and time.
    --
    --
    --
    --
    --     * @instance-match-criteria@ - Indicates the type of instance launches that the Capacity Reservation accepts. The options include:
    --
    --     * @open@ - The Capacity Reservation accepts all instances that have matching attributes (instance type, platform, and Availability Zone). Instances that have matching attributes launch into the Capacity Reservation automatically without specifying any additional parameters.
    --
    --
    --     * @targeted@ - The Capacity Reservation only accepts instances that have matching attributes (instance type, platform, and Availability Zone), and explicitly target the Capacity Reservation. This ensures that only permitted instances can use the reserved capacity.
    filters :: Core.Maybe [Types.Filter],
    -- | The maximum number of results to return for the request in a single page. The remaining results can be seen by sending another request with the returned @nextToken@ value. This value can be between 5 and 500. If @maxResults@ is given a larger value than 500, you receive an error.
    maxResults :: Core.Maybe Core.Natural,
    -- | The token to use to retrieve the next page of results.
    nextToken :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeCapacityReservations' value with any optional fields omitted.
mkDescribeCapacityReservations ::
  DescribeCapacityReservations
mkDescribeCapacityReservations =
  DescribeCapacityReservations'
    { capacityReservationIds =
        Core.Nothing,
      dryRun = Core.Nothing,
      filters = Core.Nothing,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | The ID of the Capacity Reservation.
--
-- /Note:/ Consider using 'capacityReservationIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrCapacityReservationIds :: Lens.Lens' DescribeCapacityReservations (Core.Maybe [Types.CapacityReservationId])
dcrCapacityReservationIds = Lens.field @"capacityReservationIds"
{-# DEPRECATED dcrCapacityReservationIds "Use generic-lens or generic-optics with 'capacityReservationIds' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrDryRun :: Lens.Lens' DescribeCapacityReservations (Core.Maybe Core.Bool)
dcrDryRun = Lens.field @"dryRun"
{-# DEPRECATED dcrDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | One or more filters.
--
--
--     * @instance-type@ - The type of instance for which the Capacity Reservation reserves capacity.
--
--
--     * @owner-id@ - The ID of the AWS account that owns the Capacity Reservation.
--
--
--     * @availability-zone-id@ - The Availability Zone ID of the Capacity Reservation.
--
--
--     * @instance-platform@ - The type of operating system for which the Capacity Reservation reserves capacity.
--
--
--     * @availability-zone@ - The Availability Zone ID of the Capacity Reservation.
--
--
--     * @tenancy@ - Indicates the tenancy of the Capacity Reservation. A Capacity Reservation can have one of the following tenancy settings:
--
--     * @default@ - The Capacity Reservation is created on hardware that is shared with other AWS accounts.
--
--
--     * @dedicated@ - The Capacity Reservation is created on single-tenant hardware that is dedicated to a single AWS account.
--
--
--
--
--     * @state@ - The current state of the Capacity Reservation. A Capacity Reservation can be in one of the following states:
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
--
--     * @end-date@ - The date and time at which the Capacity Reservation expires. When a Capacity Reservation expires, the reserved capacity is released and you can no longer launch instances into it. The Capacity Reservation's state changes to expired when it reaches its end date and time.
--
--
--     * @end-date-type@ - Indicates the way in which the Capacity Reservation ends. A Capacity Reservation can have one of the following end types:
--
--     * @unlimited@ - The Capacity Reservation remains active until you explicitly cancel it.
--
--
--     * @limited@ - The Capacity Reservation expires automatically at a specified date and time.
--
--
--
--
--     * @instance-match-criteria@ - Indicates the type of instance launches that the Capacity Reservation accepts. The options include:
--
--     * @open@ - The Capacity Reservation accepts all instances that have matching attributes (instance type, platform, and Availability Zone). Instances that have matching attributes launch into the Capacity Reservation automatically without specifying any additional parameters.
--
--
--     * @targeted@ - The Capacity Reservation only accepts instances that have matching attributes (instance type, platform, and Availability Zone), and explicitly target the Capacity Reservation. This ensures that only permitted instances can use the reserved capacity.
--
--
--
--
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrFilters :: Lens.Lens' DescribeCapacityReservations (Core.Maybe [Types.Filter])
dcrFilters = Lens.field @"filters"
{-# DEPRECATED dcrFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | The maximum number of results to return for the request in a single page. The remaining results can be seen by sending another request with the returned @nextToken@ value. This value can be between 5 and 500. If @maxResults@ is given a larger value than 500, you receive an error.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrMaxResults :: Lens.Lens' DescribeCapacityReservations (Core.Maybe Core.Natural)
dcrMaxResults = Lens.field @"maxResults"
{-# DEPRECATED dcrMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The token to use to retrieve the next page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrNextToken :: Lens.Lens' DescribeCapacityReservations (Core.Maybe Types.String)
dcrNextToken = Lens.field @"nextToken"
{-# DEPRECATED dcrNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.AWSRequest DescribeCapacityReservations where
  type
    Rs DescribeCapacityReservations =
      DescribeCapacityReservationsResponse
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
            ( Core.pure ("Action", "DescribeCapacityReservations")
                Core.<> (Core.pure ("Version", "2016-11-15"))
                Core.<> ( Core.toQueryList "CapacityReservationId"
                            Core.<$> capacityReservationIds
                        )
                Core.<> (Core.toQueryValue "DryRun" Core.<$> dryRun)
                Core.<> (Core.toQueryList "Filter" Core.<$> filters)
                Core.<> (Core.toQueryValue "MaxResults" Core.<$> maxResults)
                Core.<> (Core.toQueryValue "NextToken" Core.<$> nextToken)
            )
      }
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeCapacityReservationsResponse'
            Core.<$> ( x Core..@? "capacityReservationSet"
                         Core..<@> Core.parseXMLList "item"
                     )
            Core.<*> (x Core..@? "nextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager DescribeCapacityReservations where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"capacityReservations" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkDescribeCapacityReservationsResponse' smart constructor.
data DescribeCapacityReservationsResponse = DescribeCapacityReservationsResponse'
  { -- | Information about the Capacity Reservations.
    capacityReservations :: Core.Maybe [Types.CapacityReservation],
    -- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
    nextToken :: Core.Maybe Types.String,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeCapacityReservationsResponse' value with any optional fields omitted.
mkDescribeCapacityReservationsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeCapacityReservationsResponse
mkDescribeCapacityReservationsResponse responseStatus =
  DescribeCapacityReservationsResponse'
    { capacityReservations =
        Core.Nothing,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | Information about the Capacity Reservations.
--
-- /Note:/ Consider using 'capacityReservations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrrrsCapacityReservations :: Lens.Lens' DescribeCapacityReservationsResponse (Core.Maybe [Types.CapacityReservation])
dcrrrsCapacityReservations = Lens.field @"capacityReservations"
{-# DEPRECATED dcrrrsCapacityReservations "Use generic-lens or generic-optics with 'capacityReservations' instead." #-}

-- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrrrsNextToken :: Lens.Lens' DescribeCapacityReservationsResponse (Core.Maybe Types.String)
dcrrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED dcrrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrrrsResponseStatus :: Lens.Lens' DescribeCapacityReservationsResponse Core.Int
dcrrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dcrrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
