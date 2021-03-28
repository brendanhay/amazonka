{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.DescribeScheduledActions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the actions scheduled for your Auto Scaling group that haven't run or that have not reached their end time. To describe the actions that have already run, call the 'DescribeScalingActivities' API.
--
-- This operation returns paginated results.
module Network.AWS.AutoScaling.DescribeScheduledActions
    (
    -- * Creating a request
      DescribeScheduledActions (..)
    , mkDescribeScheduledActions
    -- ** Request lenses
    , dsasAutoScalingGroupName
    , dsasEndTime
    , dsasMaxRecords
    , dsasNextToken
    , dsasScheduledActionNames
    , dsasStartTime

    -- * Destructuring the response
    , DescribeScheduledActionsResponse (..)
    , mkDescribeScheduledActionsResponse
    -- ** Response lenses
    , dsarrsNextToken
    , dsarrsScheduledUpdateGroupActions
    , dsarrsResponseStatus
    ) where

import qualified Network.AWS.AutoScaling.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeScheduledActions' smart constructor.
data DescribeScheduledActions = DescribeScheduledActions'
  { autoScalingGroupName :: Core.Maybe Types.ResourceName
    -- ^ The name of the Auto Scaling group.
  , endTime :: Core.Maybe Core.UTCTime
    -- ^ The latest scheduled start time to return. If scheduled action names are provided, this parameter is ignored.
  , maxRecords :: Core.Maybe Core.Int
    -- ^ The maximum number of items to return with this call. The default value is @50@ and the maximum value is @100@ .
  , nextToken :: Core.Maybe Types.XmlString
    -- ^ The token for the next set of items to return. (You received this token from a previous call.)
  , scheduledActionNames :: Core.Maybe [Types.ResourceName]
    -- ^ The names of one or more scheduled actions. You can specify up to 50 actions. If you omit this parameter, all scheduled actions are described. If you specify an unknown scheduled action, it is ignored with no error.
  , startTime :: Core.Maybe Core.UTCTime
    -- ^ The earliest scheduled start time to return. If scheduled action names are provided, this parameter is ignored.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeScheduledActions' value with any optional fields omitted.
mkDescribeScheduledActions
    :: DescribeScheduledActions
mkDescribeScheduledActions
  = DescribeScheduledActions'{autoScalingGroupName = Core.Nothing,
                              endTime = Core.Nothing, maxRecords = Core.Nothing,
                              nextToken = Core.Nothing, scheduledActionNames = Core.Nothing,
                              startTime = Core.Nothing}

-- | The name of the Auto Scaling group.
--
-- /Note:/ Consider using 'autoScalingGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsasAutoScalingGroupName :: Lens.Lens' DescribeScheduledActions (Core.Maybe Types.ResourceName)
dsasAutoScalingGroupName = Lens.field @"autoScalingGroupName"
{-# INLINEABLE dsasAutoScalingGroupName #-}
{-# DEPRECATED autoScalingGroupName "Use generic-lens or generic-optics with 'autoScalingGroupName' instead"  #-}

-- | The latest scheduled start time to return. If scheduled action names are provided, this parameter is ignored.
--
-- /Note:/ Consider using 'endTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsasEndTime :: Lens.Lens' DescribeScheduledActions (Core.Maybe Core.UTCTime)
dsasEndTime = Lens.field @"endTime"
{-# INLINEABLE dsasEndTime #-}
{-# DEPRECATED endTime "Use generic-lens or generic-optics with 'endTime' instead"  #-}

-- | The maximum number of items to return with this call. The default value is @50@ and the maximum value is @100@ .
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsasMaxRecords :: Lens.Lens' DescribeScheduledActions (Core.Maybe Core.Int)
dsasMaxRecords = Lens.field @"maxRecords"
{-# INLINEABLE dsasMaxRecords #-}
{-# DEPRECATED maxRecords "Use generic-lens or generic-optics with 'maxRecords' instead"  #-}

-- | The token for the next set of items to return. (You received this token from a previous call.)
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsasNextToken :: Lens.Lens' DescribeScheduledActions (Core.Maybe Types.XmlString)
dsasNextToken = Lens.field @"nextToken"
{-# INLINEABLE dsasNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The names of one or more scheduled actions. You can specify up to 50 actions. If you omit this parameter, all scheduled actions are described. If you specify an unknown scheduled action, it is ignored with no error.
--
-- /Note:/ Consider using 'scheduledActionNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsasScheduledActionNames :: Lens.Lens' DescribeScheduledActions (Core.Maybe [Types.ResourceName])
dsasScheduledActionNames = Lens.field @"scheduledActionNames"
{-# INLINEABLE dsasScheduledActionNames #-}
{-# DEPRECATED scheduledActionNames "Use generic-lens or generic-optics with 'scheduledActionNames' instead"  #-}

-- | The earliest scheduled start time to return. If scheduled action names are provided, this parameter is ignored.
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsasStartTime :: Lens.Lens' DescribeScheduledActions (Core.Maybe Core.UTCTime)
dsasStartTime = Lens.field @"startTime"
{-# INLINEABLE dsasStartTime #-}
{-# DEPRECATED startTime "Use generic-lens or generic-optics with 'startTime' instead"  #-}

instance Core.ToQuery DescribeScheduledActions where
        toQuery DescribeScheduledActions{..}
          = Core.toQueryPair "Action"
              ("DescribeScheduledActions" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2011-01-01" :: Core.Text)
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "AutoScalingGroupName")
                autoScalingGroupName
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "EndTime") endTime
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "MaxRecords") maxRecords
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "NextToken") nextToken
              Core.<>
              Core.toQueryPair "ScheduledActionNames"
                (Core.maybe Core.mempty (Core.toQueryList "member")
                   scheduledActionNames)
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "StartTime") startTime

instance Core.ToHeaders DescribeScheduledActions where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DescribeScheduledActions where
        type Rs DescribeScheduledActions = DescribeScheduledActionsResponse
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
          = Response.receiveXMLWrapper "DescribeScheduledActionsResult"
              (\ s h x ->
                 DescribeScheduledActionsResponse' Core.<$>
                   (x Core..@? "NextToken") Core.<*>
                     x Core..@? "ScheduledUpdateGroupActions" Core..<@>
                       Core.parseXMLList "member"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager DescribeScheduledActions where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop
              (rs Lens.^?
                 Lens.field @"scheduledUpdateGroupActions" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkDescribeScheduledActionsResponse' smart constructor.
data DescribeScheduledActionsResponse = DescribeScheduledActionsResponse'
  { nextToken :: Core.Maybe Types.XmlString
    -- ^ A string that indicates that the response contains more items than can be returned in a single response. To receive additional items, specify this string for the @NextToken@ value when requesting the next set of items. This value is null when there are no more items to return.
  , scheduledUpdateGroupActions :: Core.Maybe [Types.ScheduledUpdateGroupAction]
    -- ^ The scheduled actions.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeScheduledActionsResponse' value with any optional fields omitted.
mkDescribeScheduledActionsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeScheduledActionsResponse
mkDescribeScheduledActionsResponse responseStatus
  = DescribeScheduledActionsResponse'{nextToken = Core.Nothing,
                                      scheduledUpdateGroupActions = Core.Nothing, responseStatus}

-- | A string that indicates that the response contains more items than can be returned in a single response. To receive additional items, specify this string for the @NextToken@ value when requesting the next set of items. This value is null when there are no more items to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsarrsNextToken :: Lens.Lens' DescribeScheduledActionsResponse (Core.Maybe Types.XmlString)
dsarrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE dsarrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The scheduled actions.
--
-- /Note:/ Consider using 'scheduledUpdateGroupActions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsarrsScheduledUpdateGroupActions :: Lens.Lens' DescribeScheduledActionsResponse (Core.Maybe [Types.ScheduledUpdateGroupAction])
dsarrsScheduledUpdateGroupActions = Lens.field @"scheduledUpdateGroupActions"
{-# INLINEABLE dsarrsScheduledUpdateGroupActions #-}
{-# DEPRECATED scheduledUpdateGroupActions "Use generic-lens or generic-optics with 'scheduledUpdateGroupActions' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsarrsResponseStatus :: Lens.Lens' DescribeScheduledActionsResponse Core.Int
dsarrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dsarrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
