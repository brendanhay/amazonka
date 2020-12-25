{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeScheduledInstances
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified Scheduled Instances or all your Scheduled Instances.
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeScheduledInstances
  ( -- * Creating a request
    DescribeScheduledInstances (..),
    mkDescribeScheduledInstances,

    -- ** Request lenses
    dsiDryRun,
    dsiFilters,
    dsiMaxResults,
    dsiNextToken,
    dsiScheduledInstanceIds,
    dsiSlotStartTimeRange,

    -- * Destructuring the response
    DescribeScheduledInstancesResponse (..),
    mkDescribeScheduledInstancesResponse,

    -- ** Response lenses
    dsirrsNextToken,
    dsirrsScheduledInstanceSet,
    dsirrsResponseStatus,
  )
where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for DescribeScheduledInstances.
--
-- /See:/ 'mkDescribeScheduledInstances' smart constructor.
data DescribeScheduledInstances = DescribeScheduledInstances'
  { -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
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
    -- | The maximum number of results to return in a single call. This value can be between 5 and 300. The default value is 100. To retrieve the remaining results, make another call with the returned @NextToken@ value.
    maxResults :: Core.Maybe Core.Int,
    -- | The token for the next set of results.
    nextToken :: Core.Maybe Types.NextToken,
    -- | The Scheduled Instance IDs.
    scheduledInstanceIds :: Core.Maybe [Types.ScheduledInstanceId],
    -- | The time period for the first schedule to start.
    slotStartTimeRange :: Core.Maybe Types.SlotStartTimeRangeRequest
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeScheduledInstances' value with any optional fields omitted.
mkDescribeScheduledInstances ::
  DescribeScheduledInstances
mkDescribeScheduledInstances =
  DescribeScheduledInstances'
    { dryRun = Core.Nothing,
      filters = Core.Nothing,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing,
      scheduledInstanceIds = Core.Nothing,
      slotStartTimeRange = Core.Nothing
    }

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsiDryRun :: Lens.Lens' DescribeScheduledInstances (Core.Maybe Core.Bool)
dsiDryRun = Lens.field @"dryRun"
{-# DEPRECATED dsiDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

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
dsiFilters :: Lens.Lens' DescribeScheduledInstances (Core.Maybe [Types.Filter])
dsiFilters = Lens.field @"filters"
{-# DEPRECATED dsiFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | The maximum number of results to return in a single call. This value can be between 5 and 300. The default value is 100. To retrieve the remaining results, make another call with the returned @NextToken@ value.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsiMaxResults :: Lens.Lens' DescribeScheduledInstances (Core.Maybe Core.Int)
dsiMaxResults = Lens.field @"maxResults"
{-# DEPRECATED dsiMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The token for the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsiNextToken :: Lens.Lens' DescribeScheduledInstances (Core.Maybe Types.NextToken)
dsiNextToken = Lens.field @"nextToken"
{-# DEPRECATED dsiNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The Scheduled Instance IDs.
--
-- /Note:/ Consider using 'scheduledInstanceIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsiScheduledInstanceIds :: Lens.Lens' DescribeScheduledInstances (Core.Maybe [Types.ScheduledInstanceId])
dsiScheduledInstanceIds = Lens.field @"scheduledInstanceIds"
{-# DEPRECATED dsiScheduledInstanceIds "Use generic-lens or generic-optics with 'scheduledInstanceIds' instead." #-}

-- | The time period for the first schedule to start.
--
-- /Note:/ Consider using 'slotStartTimeRange' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsiSlotStartTimeRange :: Lens.Lens' DescribeScheduledInstances (Core.Maybe Types.SlotStartTimeRangeRequest)
dsiSlotStartTimeRange = Lens.field @"slotStartTimeRange"
{-# DEPRECATED dsiSlotStartTimeRange "Use generic-lens or generic-optics with 'slotStartTimeRange' instead." #-}

instance Core.AWSRequest DescribeScheduledInstances where
  type
    Rs DescribeScheduledInstances =
      DescribeScheduledInstancesResponse
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
            ( Core.pure ("Action", "DescribeScheduledInstances")
                Core.<> (Core.pure ("Version", "2016-11-15"))
                Core.<> (Core.toQueryValue "DryRun" Core.<$> dryRun)
                Core.<> (Core.toQueryList "Filter" Core.<$> filters)
                Core.<> (Core.toQueryValue "MaxResults" Core.<$> maxResults)
                Core.<> (Core.toQueryValue "NextToken" Core.<$> nextToken)
                Core.<> ( Core.toQueryList "ScheduledInstanceId"
                            Core.<$> scheduledInstanceIds
                        )
                Core.<> ( Core.toQueryValue "SlotStartTimeRange"
                            Core.<$> slotStartTimeRange
                        )
            )
      }
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeScheduledInstancesResponse'
            Core.<$> (x Core..@? "nextToken")
            Core.<*> ( x Core..@? "scheduledInstanceSet"
                         Core..<@> Core.parseXMLList "item"
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager DescribeScheduledInstances where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"scheduledInstanceSet" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | Contains the output of DescribeScheduledInstances.
--
-- /See:/ 'mkDescribeScheduledInstancesResponse' smart constructor.
data DescribeScheduledInstancesResponse = DescribeScheduledInstancesResponse'
  { -- | The token required to retrieve the next set of results. This value is @null@ when there are no more results to return.
    nextToken :: Core.Maybe Types.String,
    -- | Information about the Scheduled Instances.
    scheduledInstanceSet :: Core.Maybe [Types.ScheduledInstance],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeScheduledInstancesResponse' value with any optional fields omitted.
mkDescribeScheduledInstancesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeScheduledInstancesResponse
mkDescribeScheduledInstancesResponse responseStatus =
  DescribeScheduledInstancesResponse'
    { nextToken = Core.Nothing,
      scheduledInstanceSet = Core.Nothing,
      responseStatus
    }

-- | The token required to retrieve the next set of results. This value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsirrsNextToken :: Lens.Lens' DescribeScheduledInstancesResponse (Core.Maybe Types.String)
dsirrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED dsirrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Information about the Scheduled Instances.
--
-- /Note:/ Consider using 'scheduledInstanceSet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsirrsScheduledInstanceSet :: Lens.Lens' DescribeScheduledInstancesResponse (Core.Maybe [Types.ScheduledInstance])
dsirrsScheduledInstanceSet = Lens.field @"scheduledInstanceSet"
{-# DEPRECATED dsirrsScheduledInstanceSet "Use generic-lens or generic-optics with 'scheduledInstanceSet' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsirrsResponseStatus :: Lens.Lens' DescribeScheduledInstancesResponse Core.Int
dsirrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dsirrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
