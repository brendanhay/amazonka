{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.DescribeInstanceRefreshes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more instance refreshes.
--
-- You can determine the status of a request by looking at the @Status@ parameter. The following are the possible statuses: 
--
--     * @Pending@ - The request was created, but the operation has not started.
--
--
--     * @InProgress@ - The operation is in progress.
--
--
--     * @Successful@ - The operation completed successfully.
--
--
--     * @Failed@ - The operation failed to complete. You can troubleshoot using the status reason and the scaling activities. 
--
--
--     * @Cancelling@ - An ongoing operation is being cancelled. Cancellation does not roll back any replacements that have already been completed, but it prevents new replacements from being started. 
--
--
--     * @Cancelled@ - The operation is cancelled. 
--
--
-- For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/asg-instance-refresh.html Replacing Auto Scaling Instances Based on an Instance Refresh> .
module Network.AWS.AutoScaling.DescribeInstanceRefreshes
    (
    -- * Creating a request
      DescribeInstanceRefreshes (..)
    , mkDescribeInstanceRefreshes
    -- ** Request lenses
    , dirAutoScalingGroupName
    , dirInstanceRefreshIds
    , dirMaxRecords
    , dirNextToken

    -- * Destructuring the response
    , DescribeInstanceRefreshesResponse (..)
    , mkDescribeInstanceRefreshesResponse
    -- ** Response lenses
    , dirrrsInstanceRefreshes
    , dirrrsNextToken
    , dirrrsResponseStatus
    ) where

import qualified Network.AWS.AutoScaling.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeInstanceRefreshes' smart constructor.
data DescribeInstanceRefreshes = DescribeInstanceRefreshes'
  { autoScalingGroupName :: Types.AutoScalingGroupName
    -- ^ The name of the Auto Scaling group.
  , instanceRefreshIds :: Core.Maybe [Types.XmlStringMaxLen255]
    -- ^ One or more instance refresh IDs.
  , maxRecords :: Core.Maybe Core.Int
    -- ^ The maximum number of items to return with this call. The default value is @50@ and the maximum value is @100@ .
  , nextToken :: Core.Maybe Types.XmlString
    -- ^ The token for the next set of items to return. (You received this token from a previous call.)
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeInstanceRefreshes' value with any optional fields omitted.
mkDescribeInstanceRefreshes
    :: Types.AutoScalingGroupName -- ^ 'autoScalingGroupName'
    -> DescribeInstanceRefreshes
mkDescribeInstanceRefreshes autoScalingGroupName
  = DescribeInstanceRefreshes'{autoScalingGroupName,
                               instanceRefreshIds = Core.Nothing, maxRecords = Core.Nothing,
                               nextToken = Core.Nothing}

-- | The name of the Auto Scaling group.
--
-- /Note:/ Consider using 'autoScalingGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dirAutoScalingGroupName :: Lens.Lens' DescribeInstanceRefreshes Types.AutoScalingGroupName
dirAutoScalingGroupName = Lens.field @"autoScalingGroupName"
{-# INLINEABLE dirAutoScalingGroupName #-}
{-# DEPRECATED autoScalingGroupName "Use generic-lens or generic-optics with 'autoScalingGroupName' instead"  #-}

-- | One or more instance refresh IDs.
--
-- /Note:/ Consider using 'instanceRefreshIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dirInstanceRefreshIds :: Lens.Lens' DescribeInstanceRefreshes (Core.Maybe [Types.XmlStringMaxLen255])
dirInstanceRefreshIds = Lens.field @"instanceRefreshIds"
{-# INLINEABLE dirInstanceRefreshIds #-}
{-# DEPRECATED instanceRefreshIds "Use generic-lens or generic-optics with 'instanceRefreshIds' instead"  #-}

-- | The maximum number of items to return with this call. The default value is @50@ and the maximum value is @100@ .
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dirMaxRecords :: Lens.Lens' DescribeInstanceRefreshes (Core.Maybe Core.Int)
dirMaxRecords = Lens.field @"maxRecords"
{-# INLINEABLE dirMaxRecords #-}
{-# DEPRECATED maxRecords "Use generic-lens or generic-optics with 'maxRecords' instead"  #-}

-- | The token for the next set of items to return. (You received this token from a previous call.)
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dirNextToken :: Lens.Lens' DescribeInstanceRefreshes (Core.Maybe Types.XmlString)
dirNextToken = Lens.field @"nextToken"
{-# INLINEABLE dirNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery DescribeInstanceRefreshes where
        toQuery DescribeInstanceRefreshes{..}
          = Core.toQueryPair "Action"
              ("DescribeInstanceRefreshes" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2011-01-01" :: Core.Text)
              Core.<>
              Core.toQueryPair "AutoScalingGroupName" autoScalingGroupName
              Core.<>
              Core.toQueryPair "InstanceRefreshIds"
                (Core.maybe Core.mempty (Core.toQueryList "member")
                   instanceRefreshIds)
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "MaxRecords") maxRecords
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "NextToken") nextToken

instance Core.ToHeaders DescribeInstanceRefreshes where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DescribeInstanceRefreshes where
        type Rs DescribeInstanceRefreshes =
             DescribeInstanceRefreshesResponse
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
          = Response.receiveXMLWrapper "DescribeInstanceRefreshesResult"
              (\ s h x ->
                 DescribeInstanceRefreshesResponse' Core.<$>
                   (x Core..@? "InstanceRefreshes" Core..<@>
                      Core.parseXMLList "member")
                     Core.<*> x Core..@? "NextToken"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDescribeInstanceRefreshesResponse' smart constructor.
data DescribeInstanceRefreshesResponse = DescribeInstanceRefreshesResponse'
  { instanceRefreshes :: Core.Maybe [Types.InstanceRefresh]
    -- ^ The instance refreshes for the specified group.
  , nextToken :: Core.Maybe Types.XmlString
    -- ^ A string that indicates that the response contains more items than can be returned in a single response. To receive additional items, specify this string for the @NextToken@ value when requesting the next set of items. This value is null when there are no more items to return.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeInstanceRefreshesResponse' value with any optional fields omitted.
mkDescribeInstanceRefreshesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeInstanceRefreshesResponse
mkDescribeInstanceRefreshesResponse responseStatus
  = DescribeInstanceRefreshesResponse'{instanceRefreshes =
                                         Core.Nothing,
                                       nextToken = Core.Nothing, responseStatus}

-- | The instance refreshes for the specified group.
--
-- /Note:/ Consider using 'instanceRefreshes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dirrrsInstanceRefreshes :: Lens.Lens' DescribeInstanceRefreshesResponse (Core.Maybe [Types.InstanceRefresh])
dirrrsInstanceRefreshes = Lens.field @"instanceRefreshes"
{-# INLINEABLE dirrrsInstanceRefreshes #-}
{-# DEPRECATED instanceRefreshes "Use generic-lens or generic-optics with 'instanceRefreshes' instead"  #-}

-- | A string that indicates that the response contains more items than can be returned in a single response. To receive additional items, specify this string for the @NextToken@ value when requesting the next set of items. This value is null when there are no more items to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dirrrsNextToken :: Lens.Lens' DescribeInstanceRefreshesResponse (Core.Maybe Types.XmlString)
dirrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE dirrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dirrrsResponseStatus :: Lens.Lens' DescribeInstanceRefreshesResponse Core.Int
dirrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dirrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
