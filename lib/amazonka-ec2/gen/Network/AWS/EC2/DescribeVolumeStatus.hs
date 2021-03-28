{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeVolumeStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the status of the specified volumes. Volume status provides the result of the checks performed on your volumes to determine events that can impair the performance of your volumes. The performance of a volume can be affected if an issue occurs on the volume's underlying host. If the volume's underlying host experiences a power outage or system issue, after the system is restored, there could be data inconsistencies on the volume. Volume events notify you if this occurs. Volume actions notify you if any action needs to be taken in response to the event.
--
-- The @DescribeVolumeStatus@ operation provides the following information about the specified volumes:
-- /Status/ : Reflects the current status of the volume. The possible values are @ok@ , @impaired@ , @warning@ , or @insufficient-data@ . If all checks pass, the overall status of the volume is @ok@ . If the check fails, the overall status is @impaired@ . If the status is @insufficient-data@ , then the checks may still be taking place on your volume at the time. We recommend that you retry the request. For more information about volume status, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/monitoring-volume-status.html Monitoring the status of your volumes> in the /Amazon Elastic Compute Cloud User Guide/ .
-- /Events/ : Reflect the cause of a volume status and may require you to take action. For example, if your volume returns an @impaired@ status, then the volume event might be @potential-data-inconsistency@ . This means that your volume has been affected by an issue with the underlying host, has all I/O operations disabled, and may have inconsistent data.
-- /Actions/ : Reflect the actions you may have to take in response to an event. For example, if the status of the volume is @impaired@ and the volume event shows @potential-data-inconsistency@ , then the action shows @enable-volume-io@ . This means that you may want to enable the I/O operations for the volume by calling the 'EnableVolumeIO' action and then check the volume for data consistency.
-- Volume status is based on the volume status checks, and does not reflect the volume state. Therefore, volume status does not indicate volumes in the @error@ state (for example, when a volume is incapable of accepting I/O.)
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeVolumeStatus
    (
    -- * Creating a request
      DescribeVolumeStatus (..)
    , mkDescribeVolumeStatus
    -- ** Request lenses
    , dvssDryRun
    , dvssFilters
    , dvssMaxResults
    , dvssNextToken
    , dvssVolumeIds

    -- * Destructuring the response
    , DescribeVolumeStatusResponse (..)
    , mkDescribeVolumeStatusResponse
    -- ** Response lenses
    , dvsrrsNextToken
    , dvsrrsVolumeStatuses
    , dvsrrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeVolumeStatus' smart constructor.
data DescribeVolumeStatus = DescribeVolumeStatus'
  { dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  , filters :: Core.Maybe [Types.Filter]
    -- ^ The filters.
--
--
--     * @action.code@ - The action code for the event (for example, @enable-volume-io@ ).
--
--
--     * @action.description@ - A description of the action.
--
--
--     * @action.event-id@ - The event ID associated with the action.
--
--
--     * @availability-zone@ - The Availability Zone of the instance.
--
--
--     * @event.description@ - A description of the event.
--
--
--     * @event.event-id@ - The event ID.
--
--
--     * @event.event-type@ - The event type (for @io-enabled@ : @passed@ | @failed@ ; for @io-performance@ : @io-performance:degraded@ | @io-performance:severely-degraded@ | @io-performance:stalled@ ).
--
--
--     * @event.not-after@ - The latest end time for the event.
--
--
--     * @event.not-before@ - The earliest start time for the event.
--
--
--     * @volume-status.details-name@ - The cause for @volume-status.status@ (@io-enabled@ | @io-performance@ ).
--
--
--     * @volume-status.details-status@ - The status of @volume-status.details-name@ (for @io-enabled@ : @passed@ | @failed@ ; for @io-performance@ : @normal@ | @degraded@ | @severely-degraded@ | @stalled@ ).
--
--
--     * @volume-status.status@ - The status of the volume (@ok@ | @impaired@ | @warning@ | @insufficient-data@ ).
--
--
  , maxResults :: Core.Maybe Core.Int
    -- ^ The maximum number of volume results returned by @DescribeVolumeStatus@ in paginated output. When this parameter is used, the request only returns @MaxResults@ results in a single page along with a @NextToken@ response element. The remaining results of the initial request can be seen by sending another request with the returned @NextToken@ value. This value can be between 5 and 1000; if @MaxResults@ is given a value larger than 1000, only 1000 results are returned. If this parameter is not used, then @DescribeVolumeStatus@ returns all results. You cannot specify this parameter and the volume IDs parameter in the same request.
  , nextToken :: Core.Maybe Core.Text
    -- ^ The @NextToken@ value to include in a future @DescribeVolumeStatus@ request. When the results of the request exceed @MaxResults@ , this value can be used to retrieve the next page of results. This value is @null@ when there are no more results to return.
  , volumeIds :: Core.Maybe [Types.VolumeId]
    -- ^ The IDs of the volumes.
--
-- Default: Describes all your volumes.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeVolumeStatus' value with any optional fields omitted.
mkDescribeVolumeStatus
    :: DescribeVolumeStatus
mkDescribeVolumeStatus
  = DescribeVolumeStatus'{dryRun = Core.Nothing,
                          filters = Core.Nothing, maxResults = Core.Nothing,
                          nextToken = Core.Nothing, volumeIds = Core.Nothing}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvssDryRun :: Lens.Lens' DescribeVolumeStatus (Core.Maybe Core.Bool)
dvssDryRun = Lens.field @"dryRun"
{-# INLINEABLE dvssDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

-- | The filters.
--
--
--     * @action.code@ - The action code for the event (for example, @enable-volume-io@ ).
--
--
--     * @action.description@ - A description of the action.
--
--
--     * @action.event-id@ - The event ID associated with the action.
--
--
--     * @availability-zone@ - The Availability Zone of the instance.
--
--
--     * @event.description@ - A description of the event.
--
--
--     * @event.event-id@ - The event ID.
--
--
--     * @event.event-type@ - The event type (for @io-enabled@ : @passed@ | @failed@ ; for @io-performance@ : @io-performance:degraded@ | @io-performance:severely-degraded@ | @io-performance:stalled@ ).
--
--
--     * @event.not-after@ - The latest end time for the event.
--
--
--     * @event.not-before@ - The earliest start time for the event.
--
--
--     * @volume-status.details-name@ - The cause for @volume-status.status@ (@io-enabled@ | @io-performance@ ).
--
--
--     * @volume-status.details-status@ - The status of @volume-status.details-name@ (for @io-enabled@ : @passed@ | @failed@ ; for @io-performance@ : @normal@ | @degraded@ | @severely-degraded@ | @stalled@ ).
--
--
--     * @volume-status.status@ - The status of the volume (@ok@ | @impaired@ | @warning@ | @insufficient-data@ ).
--
--
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvssFilters :: Lens.Lens' DescribeVolumeStatus (Core.Maybe [Types.Filter])
dvssFilters = Lens.field @"filters"
{-# INLINEABLE dvssFilters #-}
{-# DEPRECATED filters "Use generic-lens or generic-optics with 'filters' instead"  #-}

-- | The maximum number of volume results returned by @DescribeVolumeStatus@ in paginated output. When this parameter is used, the request only returns @MaxResults@ results in a single page along with a @NextToken@ response element. The remaining results of the initial request can be seen by sending another request with the returned @NextToken@ value. This value can be between 5 and 1000; if @MaxResults@ is given a value larger than 1000, only 1000 results are returned. If this parameter is not used, then @DescribeVolumeStatus@ returns all results. You cannot specify this parameter and the volume IDs parameter in the same request.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvssMaxResults :: Lens.Lens' DescribeVolumeStatus (Core.Maybe Core.Int)
dvssMaxResults = Lens.field @"maxResults"
{-# INLINEABLE dvssMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | The @NextToken@ value to include in a future @DescribeVolumeStatus@ request. When the results of the request exceed @MaxResults@ , this value can be used to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvssNextToken :: Lens.Lens' DescribeVolumeStatus (Core.Maybe Core.Text)
dvssNextToken = Lens.field @"nextToken"
{-# INLINEABLE dvssNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The IDs of the volumes.
--
-- Default: Describes all your volumes.
--
-- /Note:/ Consider using 'volumeIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvssVolumeIds :: Lens.Lens' DescribeVolumeStatus (Core.Maybe [Types.VolumeId])
dvssVolumeIds = Lens.field @"volumeIds"
{-# INLINEABLE dvssVolumeIds #-}
{-# DEPRECATED volumeIds "Use generic-lens or generic-optics with 'volumeIds' instead"  #-}

instance Core.ToQuery DescribeVolumeStatus where
        toQuery DescribeVolumeStatus{..}
          = Core.toQueryPair "Action" ("DescribeVolumeStatus" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun
              Core.<> Core.maybe Core.mempty (Core.toQueryList "Filter") filters
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "MaxResults") maxResults
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "NextToken") nextToken
              Core.<>
              Core.maybe Core.mempty (Core.toQueryList "VolumeId") volumeIds

instance Core.ToHeaders DescribeVolumeStatus where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DescribeVolumeStatus where
        type Rs DescribeVolumeStatus = DescribeVolumeStatusResponse
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
                 DescribeVolumeStatusResponse' Core.<$>
                   (x Core..@? "nextToken") Core.<*>
                     x Core..@? "volumeStatusSet" Core..<@> Core.parseXMLList "item"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager DescribeVolumeStatus where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"volumeStatuses" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkDescribeVolumeStatusResponse' smart constructor.
data DescribeVolumeStatusResponse = DescribeVolumeStatusResponse'
  { nextToken :: Core.Maybe Core.Text
    -- ^ The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
  , volumeStatuses :: Core.Maybe [Types.VolumeStatusItem]
    -- ^ Information about the status of the volumes.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeVolumeStatusResponse' value with any optional fields omitted.
mkDescribeVolumeStatusResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeVolumeStatusResponse
mkDescribeVolumeStatusResponse responseStatus
  = DescribeVolumeStatusResponse'{nextToken = Core.Nothing,
                                  volumeStatuses = Core.Nothing, responseStatus}

-- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvsrrsNextToken :: Lens.Lens' DescribeVolumeStatusResponse (Core.Maybe Core.Text)
dvsrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE dvsrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | Information about the status of the volumes.
--
-- /Note:/ Consider using 'volumeStatuses' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvsrrsVolumeStatuses :: Lens.Lens' DescribeVolumeStatusResponse (Core.Maybe [Types.VolumeStatusItem])
dvsrrsVolumeStatuses = Lens.field @"volumeStatuses"
{-# INLINEABLE dvsrrsVolumeStatuses #-}
{-# DEPRECATED volumeStatuses "Use generic-lens or generic-optics with 'volumeStatuses' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvsrrsResponseStatus :: Lens.Lens' DescribeVolumeStatusResponse Core.Int
dvsrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dvsrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
