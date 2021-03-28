{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      DescribeScheduledInstances (..)
    , mkDescribeScheduledInstances
    -- ** Request lenses
    , dsiDryRun
    , dsiFilters
    , dsiMaxResults
    , dsiNextToken
    , dsiScheduledInstanceIds
    , dsiSlotStartTimeRange

    -- * Destructuring the response
    , DescribeScheduledInstancesResponse (..)
    , mkDescribeScheduledInstancesResponse
    -- ** Response lenses
    , dsirrsNextToken
    , dsirrsScheduledInstanceSet
    , dsirrsResponseStatus
    ) where

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
  { dryRun :: Core.Maybe Core.Bool
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
  , maxResults :: Core.Maybe Core.Int
    -- ^ The maximum number of results to return in a single call. This value can be between 5 and 300. The default value is 100. To retrieve the remaining results, make another call with the returned @NextToken@ value.
  , nextToken :: Core.Maybe Core.Text
    -- ^ The token for the next set of results.
  , scheduledInstanceIds :: Core.Maybe [Types.ScheduledInstanceId]
    -- ^ The Scheduled Instance IDs.
  , slotStartTimeRange :: Core.Maybe Types.SlotStartTimeRangeRequest
    -- ^ The time period for the first schedule to start.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeScheduledInstances' value with any optional fields omitted.
mkDescribeScheduledInstances
    :: DescribeScheduledInstances
mkDescribeScheduledInstances
  = DescribeScheduledInstances'{dryRun = Core.Nothing,
                                filters = Core.Nothing, maxResults = Core.Nothing,
                                nextToken = Core.Nothing, scheduledInstanceIds = Core.Nothing,
                                slotStartTimeRange = Core.Nothing}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsiDryRun :: Lens.Lens' DescribeScheduledInstances (Core.Maybe Core.Bool)
dsiDryRun = Lens.field @"dryRun"
{-# INLINEABLE dsiDryRun #-}
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
dsiFilters :: Lens.Lens' DescribeScheduledInstances (Core.Maybe [Types.Filter])
dsiFilters = Lens.field @"filters"
{-# INLINEABLE dsiFilters #-}
{-# DEPRECATED filters "Use generic-lens or generic-optics with 'filters' instead"  #-}

-- | The maximum number of results to return in a single call. This value can be between 5 and 300. The default value is 100. To retrieve the remaining results, make another call with the returned @NextToken@ value.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsiMaxResults :: Lens.Lens' DescribeScheduledInstances (Core.Maybe Core.Int)
dsiMaxResults = Lens.field @"maxResults"
{-# INLINEABLE dsiMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | The token for the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsiNextToken :: Lens.Lens' DescribeScheduledInstances (Core.Maybe Core.Text)
dsiNextToken = Lens.field @"nextToken"
{-# INLINEABLE dsiNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The Scheduled Instance IDs.
--
-- /Note:/ Consider using 'scheduledInstanceIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsiScheduledInstanceIds :: Lens.Lens' DescribeScheduledInstances (Core.Maybe [Types.ScheduledInstanceId])
dsiScheduledInstanceIds = Lens.field @"scheduledInstanceIds"
{-# INLINEABLE dsiScheduledInstanceIds #-}
{-# DEPRECATED scheduledInstanceIds "Use generic-lens or generic-optics with 'scheduledInstanceIds' instead"  #-}

-- | The time period for the first schedule to start.
--
-- /Note:/ Consider using 'slotStartTimeRange' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsiSlotStartTimeRange :: Lens.Lens' DescribeScheduledInstances (Core.Maybe Types.SlotStartTimeRangeRequest)
dsiSlotStartTimeRange = Lens.field @"slotStartTimeRange"
{-# INLINEABLE dsiSlotStartTimeRange #-}
{-# DEPRECATED slotStartTimeRange "Use generic-lens or generic-optics with 'slotStartTimeRange' instead"  #-}

instance Core.ToQuery DescribeScheduledInstances where
        toQuery DescribeScheduledInstances{..}
          = Core.toQueryPair "Action"
              ("DescribeScheduledInstances" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun
              Core.<> Core.maybe Core.mempty (Core.toQueryList "Filter") filters
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "MaxResults") maxResults
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "NextToken") nextToken
              Core.<>
              Core.maybe Core.mempty (Core.toQueryList "ScheduledInstanceId")
                scheduledInstanceIds
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "SlotStartTimeRange")
                slotStartTimeRange

instance Core.ToHeaders DescribeScheduledInstances where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DescribeScheduledInstances where
        type Rs DescribeScheduledInstances =
             DescribeScheduledInstancesResponse
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
                 DescribeScheduledInstancesResponse' Core.<$>
                   (x Core..@? "nextToken") Core.<*>
                     x Core..@? "scheduledInstanceSet" Core..<@>
                       Core.parseXMLList "item"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager DescribeScheduledInstances where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"scheduledInstanceSet" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | Contains the output of DescribeScheduledInstances.
--
-- /See:/ 'mkDescribeScheduledInstancesResponse' smart constructor.
data DescribeScheduledInstancesResponse = DescribeScheduledInstancesResponse'
  { nextToken :: Core.Maybe Core.Text
    -- ^ The token required to retrieve the next set of results. This value is @null@ when there are no more results to return.
  , scheduledInstanceSet :: Core.Maybe [Types.ScheduledInstance]
    -- ^ Information about the Scheduled Instances.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeScheduledInstancesResponse' value with any optional fields omitted.
mkDescribeScheduledInstancesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeScheduledInstancesResponse
mkDescribeScheduledInstancesResponse responseStatus
  = DescribeScheduledInstancesResponse'{nextToken = Core.Nothing,
                                        scheduledInstanceSet = Core.Nothing, responseStatus}

-- | The token required to retrieve the next set of results. This value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsirrsNextToken :: Lens.Lens' DescribeScheduledInstancesResponse (Core.Maybe Core.Text)
dsirrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE dsirrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | Information about the Scheduled Instances.
--
-- /Note:/ Consider using 'scheduledInstanceSet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsirrsScheduledInstanceSet :: Lens.Lens' DescribeScheduledInstancesResponse (Core.Maybe [Types.ScheduledInstance])
dsirrsScheduledInstanceSet = Lens.field @"scheduledInstanceSet"
{-# INLINEABLE dsirrsScheduledInstanceSet #-}
{-# DEPRECATED scheduledInstanceSet "Use generic-lens or generic-optics with 'scheduledInstanceSet' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsirrsResponseStatus :: Lens.Lens' DescribeScheduledInstancesResponse Core.Int
dsirrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dsirrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
