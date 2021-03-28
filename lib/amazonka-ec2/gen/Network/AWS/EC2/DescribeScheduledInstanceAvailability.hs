{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      DescribeScheduledInstanceAvailability (..)
    , mkDescribeScheduledInstanceAvailability
    -- ** Request lenses
    , dsiaFirstSlotStartTimeRange
    , dsiaRecurrence
    , dsiaDryRun
    , dsiaFilters
    , dsiaMaxResults
    , dsiaMaxSlotDurationInHours
    , dsiaMinSlotDurationInHours
    , dsiaNextToken

    -- * Destructuring the response
    , DescribeScheduledInstanceAvailabilityResponse (..)
    , mkDescribeScheduledInstanceAvailabilityResponse
    -- ** Response lenses
    , dsiarrsNextToken
    , dsiarrsScheduledInstanceAvailabilitySet
    , dsiarrsResponseStatus
    ) where

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
  { firstSlotStartTimeRange :: Types.SlotDateTimeRangeRequest
    -- ^ The time period for the first schedule to start.
  , recurrence :: Types.ScheduledInstanceRecurrenceRequest
    -- ^ The schedule recurrence.
  , dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  , filters :: Core.Maybe [Types.Filter]
    -- ^ The filters.
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
  , maxResults :: Core.Maybe Core.Natural
    -- ^ The maximum number of results to return in a single call. This value can be between 5 and 300. The default value is 300. To retrieve the remaining results, make another call with the returned @NextToken@ value.
  , maxSlotDurationInHours :: Core.Maybe Core.Int
    -- ^ The maximum available duration, in hours. This value must be greater than @MinSlotDurationInHours@ and less than 1,720.
  , minSlotDurationInHours :: Core.Maybe Core.Int
    -- ^ The minimum available duration, in hours. The minimum required duration is 1,200 hours per year. For example, the minimum daily schedule is 4 hours, the minimum weekly schedule is 24 hours, and the minimum monthly schedule is 100 hours.
  , nextToken :: Core.Maybe Core.Text
    -- ^ The token for the next set of results.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeScheduledInstanceAvailability' value with any optional fields omitted.
mkDescribeScheduledInstanceAvailability
    :: Types.SlotDateTimeRangeRequest -- ^ 'firstSlotStartTimeRange'
    -> Types.ScheduledInstanceRecurrenceRequest -- ^ 'recurrence'
    -> DescribeScheduledInstanceAvailability
mkDescribeScheduledInstanceAvailability firstSlotStartTimeRange
  recurrence
  = DescribeScheduledInstanceAvailability'{firstSlotStartTimeRange,
                                           recurrence, dryRun = Core.Nothing,
                                           filters = Core.Nothing, maxResults = Core.Nothing,
                                           maxSlotDurationInHours = Core.Nothing,
                                           minSlotDurationInHours = Core.Nothing,
                                           nextToken = Core.Nothing}

-- | The time period for the first schedule to start.
--
-- /Note:/ Consider using 'firstSlotStartTimeRange' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsiaFirstSlotStartTimeRange :: Lens.Lens' DescribeScheduledInstanceAvailability Types.SlotDateTimeRangeRequest
dsiaFirstSlotStartTimeRange = Lens.field @"firstSlotStartTimeRange"
{-# INLINEABLE dsiaFirstSlotStartTimeRange #-}
{-# DEPRECATED firstSlotStartTimeRange "Use generic-lens or generic-optics with 'firstSlotStartTimeRange' instead"  #-}

-- | The schedule recurrence.
--
-- /Note:/ Consider using 'recurrence' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsiaRecurrence :: Lens.Lens' DescribeScheduledInstanceAvailability Types.ScheduledInstanceRecurrenceRequest
dsiaRecurrence = Lens.field @"recurrence"
{-# INLINEABLE dsiaRecurrence #-}
{-# DEPRECATED recurrence "Use generic-lens or generic-optics with 'recurrence' instead"  #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsiaDryRun :: Lens.Lens' DescribeScheduledInstanceAvailability (Core.Maybe Core.Bool)
dsiaDryRun = Lens.field @"dryRun"
{-# INLINEABLE dsiaDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

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
{-# INLINEABLE dsiaFilters #-}
{-# DEPRECATED filters "Use generic-lens or generic-optics with 'filters' instead"  #-}

-- | The maximum number of results to return in a single call. This value can be between 5 and 300. The default value is 300. To retrieve the remaining results, make another call with the returned @NextToken@ value.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsiaMaxResults :: Lens.Lens' DescribeScheduledInstanceAvailability (Core.Maybe Core.Natural)
dsiaMaxResults = Lens.field @"maxResults"
{-# INLINEABLE dsiaMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | The maximum available duration, in hours. This value must be greater than @MinSlotDurationInHours@ and less than 1,720.
--
-- /Note:/ Consider using 'maxSlotDurationInHours' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsiaMaxSlotDurationInHours :: Lens.Lens' DescribeScheduledInstanceAvailability (Core.Maybe Core.Int)
dsiaMaxSlotDurationInHours = Lens.field @"maxSlotDurationInHours"
{-# INLINEABLE dsiaMaxSlotDurationInHours #-}
{-# DEPRECATED maxSlotDurationInHours "Use generic-lens or generic-optics with 'maxSlotDurationInHours' instead"  #-}

-- | The minimum available duration, in hours. The minimum required duration is 1,200 hours per year. For example, the minimum daily schedule is 4 hours, the minimum weekly schedule is 24 hours, and the minimum monthly schedule is 100 hours.
--
-- /Note:/ Consider using 'minSlotDurationInHours' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsiaMinSlotDurationInHours :: Lens.Lens' DescribeScheduledInstanceAvailability (Core.Maybe Core.Int)
dsiaMinSlotDurationInHours = Lens.field @"minSlotDurationInHours"
{-# INLINEABLE dsiaMinSlotDurationInHours #-}
{-# DEPRECATED minSlotDurationInHours "Use generic-lens or generic-optics with 'minSlotDurationInHours' instead"  #-}

-- | The token for the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsiaNextToken :: Lens.Lens' DescribeScheduledInstanceAvailability (Core.Maybe Core.Text)
dsiaNextToken = Lens.field @"nextToken"
{-# INLINEABLE dsiaNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery DescribeScheduledInstanceAvailability where
        toQuery DescribeScheduledInstanceAvailability{..}
          = Core.toQueryPair "Action"
              ("DescribeScheduledInstanceAvailability" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<>
              Core.toQueryPair "FirstSlotStartTimeRange" firstSlotStartTimeRange
              Core.<> Core.toQueryPair "Recurrence" recurrence
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun
              Core.<> Core.maybe Core.mempty (Core.toQueryList "Filter") filters
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "MaxResults") maxResults
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "MaxSlotDurationInHours")
                maxSlotDurationInHours
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "MinSlotDurationInHours")
                minSlotDurationInHours
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "NextToken") nextToken

instance Core.ToHeaders DescribeScheduledInstanceAvailability where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DescribeScheduledInstanceAvailability
         where
        type Rs DescribeScheduledInstanceAvailability =
             DescribeScheduledInstanceAvailabilityResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.mempty,
                         Core._rqHeaders =
                           Core.pure
                             ("Content-Type",
                              "application/x-www-form-urlencoded; charset=utf-8")
                             Core.<> Core.toHeaders x,
                         Core._rqBody = Core.toFormBody (Core.toQuery x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXML
              (\ s h x ->
                 DescribeScheduledInstanceAvailabilityResponse' Core.<$>
                   (x Core..@? "nextToken") Core.<*>
                     x Core..@? "scheduledInstanceAvailabilitySet" Core..<@>
                       Core.parseXMLList "item"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager DescribeScheduledInstanceAvailability where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop
              (rs Lens.^?
                 Lens.field @"scheduledInstanceAvailabilitySet" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | Contains the output of DescribeScheduledInstanceAvailability.
--
-- /See:/ 'mkDescribeScheduledInstanceAvailabilityResponse' smart constructor.
data DescribeScheduledInstanceAvailabilityResponse = DescribeScheduledInstanceAvailabilityResponse'
  { nextToken :: Core.Maybe Core.Text
    -- ^ The token required to retrieve the next set of results. This value is @null@ when there are no more results to return.
  , scheduledInstanceAvailabilitySet :: Core.Maybe [Types.ScheduledInstanceAvailability]
    -- ^ Information about the available Scheduled Instances.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeScheduledInstanceAvailabilityResponse' value with any optional fields omitted.
mkDescribeScheduledInstanceAvailabilityResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeScheduledInstanceAvailabilityResponse
mkDescribeScheduledInstanceAvailabilityResponse responseStatus
  = DescribeScheduledInstanceAvailabilityResponse'{nextToken =
                                                     Core.Nothing,
                                                   scheduledInstanceAvailabilitySet = Core.Nothing,
                                                   responseStatus}

-- | The token required to retrieve the next set of results. This value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsiarrsNextToken :: Lens.Lens' DescribeScheduledInstanceAvailabilityResponse (Core.Maybe Core.Text)
dsiarrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE dsiarrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | Information about the available Scheduled Instances.
--
-- /Note:/ Consider using 'scheduledInstanceAvailabilitySet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsiarrsScheduledInstanceAvailabilitySet :: Lens.Lens' DescribeScheduledInstanceAvailabilityResponse (Core.Maybe [Types.ScheduledInstanceAvailability])
dsiarrsScheduledInstanceAvailabilitySet = Lens.field @"scheduledInstanceAvailabilitySet"
{-# INLINEABLE dsiarrsScheduledInstanceAvailabilitySet #-}
{-# DEPRECATED scheduledInstanceAvailabilitySet "Use generic-lens or generic-optics with 'scheduledInstanceAvailabilitySet' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsiarrsResponseStatus :: Lens.Lens' DescribeScheduledInstanceAvailabilityResponse Core.Int
dsiarrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dsiarrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
