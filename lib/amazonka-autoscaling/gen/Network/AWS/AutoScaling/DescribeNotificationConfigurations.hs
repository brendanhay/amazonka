{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.DescribeNotificationConfigurations
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the notification actions associated with the specified Auto Scaling group.
--
-- This operation returns paginated results.
module Network.AWS.AutoScaling.DescribeNotificationConfigurations
    (
    -- * Creating a request
      DescribeNotificationConfigurations (..)
    , mkDescribeNotificationConfigurations
    -- ** Request lenses
    , dncAutoScalingGroupNames
    , dncMaxRecords
    , dncNextToken

    -- * Destructuring the response
    , DescribeNotificationConfigurationsResponse (..)
    , mkDescribeNotificationConfigurationsResponse
    -- ** Response lenses
    , dncrrsNotificationConfigurations
    , dncrrsNextToken
    , dncrrsResponseStatus
    ) where

import qualified Network.AWS.AutoScaling.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeNotificationConfigurations' smart constructor.
data DescribeNotificationConfigurations = DescribeNotificationConfigurations'
  { autoScalingGroupNames :: Core.Maybe [Types.ResourceName]
    -- ^ The name of the Auto Scaling group.
  , maxRecords :: Core.Maybe Core.Int
    -- ^ The maximum number of items to return with this call. The default value is @50@ and the maximum value is @100@ .
  , nextToken :: Core.Maybe Types.XmlString
    -- ^ The token for the next set of items to return. (You received this token from a previous call.)
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeNotificationConfigurations' value with any optional fields omitted.
mkDescribeNotificationConfigurations
    :: DescribeNotificationConfigurations
mkDescribeNotificationConfigurations
  = DescribeNotificationConfigurations'{autoScalingGroupNames =
                                          Core.Nothing,
                                        maxRecords = Core.Nothing, nextToken = Core.Nothing}

-- | The name of the Auto Scaling group.
--
-- /Note:/ Consider using 'autoScalingGroupNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dncAutoScalingGroupNames :: Lens.Lens' DescribeNotificationConfigurations (Core.Maybe [Types.ResourceName])
dncAutoScalingGroupNames = Lens.field @"autoScalingGroupNames"
{-# INLINEABLE dncAutoScalingGroupNames #-}
{-# DEPRECATED autoScalingGroupNames "Use generic-lens or generic-optics with 'autoScalingGroupNames' instead"  #-}

-- | The maximum number of items to return with this call. The default value is @50@ and the maximum value is @100@ .
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dncMaxRecords :: Lens.Lens' DescribeNotificationConfigurations (Core.Maybe Core.Int)
dncMaxRecords = Lens.field @"maxRecords"
{-# INLINEABLE dncMaxRecords #-}
{-# DEPRECATED maxRecords "Use generic-lens or generic-optics with 'maxRecords' instead"  #-}

-- | The token for the next set of items to return. (You received this token from a previous call.)
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dncNextToken :: Lens.Lens' DescribeNotificationConfigurations (Core.Maybe Types.XmlString)
dncNextToken = Lens.field @"nextToken"
{-# INLINEABLE dncNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery DescribeNotificationConfigurations where
        toQuery DescribeNotificationConfigurations{..}
          = Core.toQueryPair "Action"
              ("DescribeNotificationConfigurations" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2011-01-01" :: Core.Text)
              Core.<>
              Core.toQueryPair "AutoScalingGroupNames"
                (Core.maybe Core.mempty (Core.toQueryList "member")
                   autoScalingGroupNames)
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "MaxRecords") maxRecords
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "NextToken") nextToken

instance Core.ToHeaders DescribeNotificationConfigurations where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DescribeNotificationConfigurations where
        type Rs DescribeNotificationConfigurations =
             DescribeNotificationConfigurationsResponse
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
          = Response.receiveXMLWrapper
              "DescribeNotificationConfigurationsResult"
              (\ s h x ->
                 DescribeNotificationConfigurationsResponse' Core.<$>
                   (x Core..@ "NotificationConfigurations" Core..@! Core.mempty
                      Core..<@> Core.parseXMLList "member")
                     Core.<*> x Core..@? "NextToken"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager DescribeNotificationConfigurations where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop (rs Lens.^. Lens.field @"notificationConfigurations")
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkDescribeNotificationConfigurationsResponse' smart constructor.
data DescribeNotificationConfigurationsResponse = DescribeNotificationConfigurationsResponse'
  { notificationConfigurations :: [Types.NotificationConfiguration]
    -- ^ The notification configurations.
  , nextToken :: Core.Maybe Types.XmlString
    -- ^ A string that indicates that the response contains more items than can be returned in a single response. To receive additional items, specify this string for the @NextToken@ value when requesting the next set of items. This value is null when there are no more items to return.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeNotificationConfigurationsResponse' value with any optional fields omitted.
mkDescribeNotificationConfigurationsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeNotificationConfigurationsResponse
mkDescribeNotificationConfigurationsResponse responseStatus
  = DescribeNotificationConfigurationsResponse'{notificationConfigurations
                                                  = Core.mempty,
                                                nextToken = Core.Nothing, responseStatus}

-- | The notification configurations.
--
-- /Note:/ Consider using 'notificationConfigurations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dncrrsNotificationConfigurations :: Lens.Lens' DescribeNotificationConfigurationsResponse [Types.NotificationConfiguration]
dncrrsNotificationConfigurations = Lens.field @"notificationConfigurations"
{-# INLINEABLE dncrrsNotificationConfigurations #-}
{-# DEPRECATED notificationConfigurations "Use generic-lens or generic-optics with 'notificationConfigurations' instead"  #-}

-- | A string that indicates that the response contains more items than can be returned in a single response. To receive additional items, specify this string for the @NextToken@ value when requesting the next set of items. This value is null when there are no more items to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dncrrsNextToken :: Lens.Lens' DescribeNotificationConfigurationsResponse (Core.Maybe Types.XmlString)
dncrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE dncrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dncrrsResponseStatus :: Lens.Lens' DescribeNotificationConfigurationsResponse Core.Int
dncrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dncrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
