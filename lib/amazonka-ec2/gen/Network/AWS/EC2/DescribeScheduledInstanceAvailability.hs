{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeScheduledInstanceAvailability
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Finds available schedules that meet the specified criteria.
--
-- You can search for an available schedule no more than 3 months in advance. You must meet the minimum required duration of 1,200 hours per year. For example, the minimum daily schedule is 4 hours, the minimum weekly schedule is 24 hours, and the minimum monthly schedule is 100 hours.
-- After you find a schedule that meets your needs, call 'PurchaseScheduledInstances' to purchase Scheduled Instances with that schedule.
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeScheduledInstanceAvailability
  ( -- * Creating a request
    DescribeScheduledInstanceAvailability (..),
    mkDescribeScheduledInstanceAvailability,

    -- ** Request lenses
    dsiaFirstSlotStartTimeRange,
    dsiaRecurrence,
    dsiaDryRun,
    dsiaFilters,
    dsiaMaxResults,
    dsiaMaxSlotDurationInHours,
    dsiaMinSlotDurationInHours,
    dsiaNextToken,

    -- * Destructuring the response
    DescribeScheduledInstanceAvailabilityResponse (..),
    mkDescribeScheduledInstanceAvailabilityResponse,

    -- ** Response lenses
    dsiarrsNextToken,
    dsiarrsScheduledInstanceAvailabilitySet,
    dsiarrsResponseStatus,
  )
where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for DescribeScheduledInstanceAvailability.
--
-- /See:/ 'mkDescribeScheduledInstanceAvailability' smart constructor.
data DescribeScheduledInstanceAvailability = DescribeScheduledInstanceAvailability'
  { -- | The time period for the first schedule to start.
    firstSlotStartTimeRange :: Types.SlotDateTimeRangeRequest,
    -- | The schedule recurrence.
    recurrence :: Types.ScheduledInstanceRecurrenceRequest,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Core.Maybe Core.Bool,
    -- | The filters.
    --
    --
    --     * @availability-zone@ - The Availability Zone (for example, @us-west-2a@ ).
    --
    --
    --     * @instance-type@ - The instance type (for example, @c4.large@ ).
    --
    --
    --     * @network-platform@ - The network platform (@EC2-Classic@ or @EC2-VPC@ ).
    --
    --
    --     * @platform@ - The platform (@Linux/UNIX@ or @Windows@ ).
    filters :: Core.Maybe [Types.Filter],
    -- | The maximum number of results to return in a single call. This value can be between 5 and 300. The default value is 300. To retrieve the remaining results, make another call with the returned @NextToken@ value.
    maxResults :: Core.Maybe Core.Natural,
    -- | The maximum available duration, in hours. This value must be greater than @MinSlotDurationInHours@ and less than 1,720.
    maxSlotDurationInHours :: Core.Maybe Core.Int,
    -- | The minimum available duration, in hours. The minimum required duration is 1,200 hours per year. For example, the minimum daily schedule is 4 hours, the minimum weekly schedule is 24 hours, and the minimum monthly schedule is 100 hours.
    minSlotDurationInHours :: Core.Maybe Core.Int,
    -- | The token for the next set of results.
    nextToken :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeScheduledInstanceAvailability' value with any optional fields omitted.
mkDescribeScheduledInstanceAvailability ::
  -- | 'firstSlotStartTimeRange'
  Types.SlotDateTimeRangeRequest ->
  -- | 'recurrence'
  Types.ScheduledInstanceRecurrenceRequest ->
  DescribeScheduledInstanceAvailability
mkDescribeScheduledInstanceAvailability
  firstSlotStartTimeRange
  recurrence =
    DescribeScheduledInstanceAvailability'
      { firstSlotStartTimeRange,
        recurrence,
        dryRun = Core.Nothing,
        filters = Core.Nothing,
        maxResults = Core.Nothing,
        maxSlotDurationInHours = Core.Nothing,
        minSlotDurationInHours = Core.Nothing,
        nextToken = Core.Nothing
      }

-- | The time period for the first schedule to start.
--
-- /Note:/ Consider using 'firstSlotStartTimeRange' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsiaFirstSlotStartTimeRange :: Lens.Lens' DescribeScheduledInstanceAvailability Types.SlotDateTimeRangeRequest
dsiaFirstSlotStartTimeRange = Lens.field @"firstSlotStartTimeRange"
{-# DEPRECATED dsiaFirstSlotStartTimeRange "Use generic-lens or generic-optics with 'firstSlotStartTimeRange' instead." #-}

-- | The schedule recurrence.
--
-- /Note:/ Consider using 'recurrence' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsiaRecurrence :: Lens.Lens' DescribeScheduledInstanceAvailability Types.ScheduledInstanceRecurrenceRequest
dsiaRecurrence = Lens.field @"recurrence"
{-# DEPRECATED dsiaRecurrence "Use generic-lens or generic-optics with 'recurrence' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsiaDryRun :: Lens.Lens' DescribeScheduledInstanceAvailability (Core.Maybe Core.Bool)
dsiaDryRun = Lens.field @"dryRun"
{-# DEPRECATED dsiaDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The filters.
--
--
--     * @availability-zone@ - The Availability Zone (for example, @us-west-2a@ ).
--
--
--     * @instance-type@ - The instance type (for example, @c4.large@ ).
--
--
--     * @network-platform@ - The network platform (@EC2-Classic@ or @EC2-VPC@ ).
--
--
--     * @platform@ - The platform (@Linux/UNIX@ or @Windows@ ).
--
--
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsiaFilters :: Lens.Lens' DescribeScheduledInstanceAvailability (Core.Maybe [Types.Filter])
dsiaFilters = Lens.field @"filters"
{-# DEPRECATED dsiaFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | The maximum number of results to return in a single call. This value can be between 5 and 300. The default value is 300. To retrieve the remaining results, make another call with the returned @NextToken@ value.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsiaMaxResults :: Lens.Lens' DescribeScheduledInstanceAvailability (Core.Maybe Core.Natural)
dsiaMaxResults = Lens.field @"maxResults"
{-# DEPRECATED dsiaMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The maximum available duration, in hours. This value must be greater than @MinSlotDurationInHours@ and less than 1,720.
--
-- /Note:/ Consider using 'maxSlotDurationInHours' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsiaMaxSlotDurationInHours :: Lens.Lens' DescribeScheduledInstanceAvailability (Core.Maybe Core.Int)
dsiaMaxSlotDurationInHours = Lens.field @"maxSlotDurationInHours"
{-# DEPRECATED dsiaMaxSlotDurationInHours "Use generic-lens or generic-optics with 'maxSlotDurationInHours' instead." #-}

-- | The minimum available duration, in hours. The minimum required duration is 1,200 hours per year. For example, the minimum daily schedule is 4 hours, the minimum weekly schedule is 24 hours, and the minimum monthly schedule is 100 hours.
--
-- /Note:/ Consider using 'minSlotDurationInHours' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsiaMinSlotDurationInHours :: Lens.Lens' DescribeScheduledInstanceAvailability (Core.Maybe Core.Int)
dsiaMinSlotDurationInHours = Lens.field @"minSlotDurationInHours"
{-# DEPRECATED dsiaMinSlotDurationInHours "Use generic-lens or generic-optics with 'minSlotDurationInHours' instead." #-}

-- | The token for the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsiaNextToken :: Lens.Lens' DescribeScheduledInstanceAvailability (Core.Maybe Types.String)
dsiaNextToken = Lens.field @"nextToken"
{-# DEPRECATED dsiaNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.AWSRequest DescribeScheduledInstanceAvailability where
  type
    Rs DescribeScheduledInstanceAvailability =
      DescribeScheduledInstanceAvailabilityResponse
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
            ( Core.pure ("Action", "DescribeScheduledInstanceAvailability")
                Core.<> (Core.pure ("Version", "2016-11-15"))
                Core.<> ( Core.toQueryValue
                            "FirstSlotStartTimeRange"
                            firstSlotStartTimeRange
                        )
                Core.<> (Core.toQueryValue "Recurrence" recurrence)
                Core.<> (Core.toQueryValue "DryRun" Core.<$> dryRun)
                Core.<> (Core.toQueryList "Filter" Core.<$> filters)
                Core.<> (Core.toQueryValue "MaxResults" Core.<$> maxResults)
                Core.<> ( Core.toQueryValue "MaxSlotDurationInHours"
                            Core.<$> maxSlotDurationInHours
                        )
                Core.<> ( Core.toQueryValue "MinSlotDurationInHours"
                            Core.<$> minSlotDurationInHours
                        )
                Core.<> (Core.toQueryValue "NextToken" Core.<$> nextToken)
            )
      }
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeScheduledInstanceAvailabilityResponse'
            Core.<$> (x Core..@? "nextToken")
            Core.<*> ( x Core..@? "scheduledInstanceAvailabilitySet"
                         Core..<@> Core.parseXMLList "item"
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager DescribeScheduledInstanceAvailability where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop
        ( rs
            Lens.^? Lens.field @"scheduledInstanceAvailabilitySet" Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | Contains the output of DescribeScheduledInstanceAvailability.
--
-- /See:/ 'mkDescribeScheduledInstanceAvailabilityResponse' smart constructor.
data DescribeScheduledInstanceAvailabilityResponse = DescribeScheduledInstanceAvailabilityResponse'
  { -- | The token required to retrieve the next set of results. This value is @null@ when there are no more results to return.
    nextToken :: Core.Maybe Types.String,
    -- | Information about the available Scheduled Instances.
    scheduledInstanceAvailabilitySet :: Core.Maybe [Types.ScheduledInstanceAvailability],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeScheduledInstanceAvailabilityResponse' value with any optional fields omitted.
mkDescribeScheduledInstanceAvailabilityResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeScheduledInstanceAvailabilityResponse
mkDescribeScheduledInstanceAvailabilityResponse responseStatus =
  DescribeScheduledInstanceAvailabilityResponse'
    { nextToken =
        Core.Nothing,
      scheduledInstanceAvailabilitySet = Core.Nothing,
      responseStatus
    }

-- | The token required to retrieve the next set of results. This value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsiarrsNextToken :: Lens.Lens' DescribeScheduledInstanceAvailabilityResponse (Core.Maybe Types.String)
dsiarrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED dsiarrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Information about the available Scheduled Instances.
--
-- /Note:/ Consider using 'scheduledInstanceAvailabilitySet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsiarrsScheduledInstanceAvailabilitySet :: Lens.Lens' DescribeScheduledInstanceAvailabilityResponse (Core.Maybe [Types.ScheduledInstanceAvailability])
dsiarrsScheduledInstanceAvailabilitySet = Lens.field @"scheduledInstanceAvailabilitySet"
{-# DEPRECATED dsiarrsScheduledInstanceAvailabilitySet "Use generic-lens or generic-optics with 'scheduledInstanceAvailabilitySet' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsiarrsResponseStatus :: Lens.Lens' DescribeScheduledInstanceAvailabilityResponse Core.Int
dsiarrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dsiarrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
