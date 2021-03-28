{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeVpcEndpointConnectionNotifications
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the connection notifications for VPC endpoints and VPC endpoint services.
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeVpcEndpointConnectionNotifications
    (
    -- * Creating a request
      DescribeVpcEndpointConnectionNotifications (..)
    , mkDescribeVpcEndpointConnectionNotifications
    -- ** Request lenses
    , dvecnsConnectionNotificationId
    , dvecnsDryRun
    , dvecnsFilters
    , dvecnsMaxResults
    , dvecnsNextToken

    -- * Destructuring the response
    , DescribeVpcEndpointConnectionNotificationsResponse (..)
    , mkDescribeVpcEndpointConnectionNotificationsResponse
    -- ** Response lenses
    , dvecnrfrsConnectionNotificationSet
    , dvecnrfrsNextToken
    , dvecnrfrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeVpcEndpointConnectionNotifications' smart constructor.
data DescribeVpcEndpointConnectionNotifications = DescribeVpcEndpointConnectionNotifications'
  { connectionNotificationId :: Core.Maybe Types.ConnectionNotificationId
    -- ^ The ID of the notification.
  , dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  , filters :: Core.Maybe [Types.Filter]
    -- ^ One or more filters.
--
--
--     * @connection-notification-arn@ - The ARN of the SNS topic for the notification.
--
--
--     * @connection-notification-id@ - The ID of the notification.
--
--
--     * @connection-notification-state@ - The state of the notification (@Enabled@ | @Disabled@ ).
--
--
--     * @connection-notification-type@ - The type of notification (@Topic@ ).
--
--
--     * @service-id@ - The ID of the endpoint service.
--
--
--     * @vpc-endpoint-id@ - The ID of the VPC endpoint.
--
--
  , maxResults :: Core.Maybe Core.Int
    -- ^ The maximum number of results to return in a single call. To retrieve the remaining results, make another request with the returned @NextToken@ value.
  , nextToken :: Core.Maybe Core.Text
    -- ^ The token to request the next page of results.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeVpcEndpointConnectionNotifications' value with any optional fields omitted.
mkDescribeVpcEndpointConnectionNotifications
    :: DescribeVpcEndpointConnectionNotifications
mkDescribeVpcEndpointConnectionNotifications
  = DescribeVpcEndpointConnectionNotifications'{connectionNotificationId
                                                  = Core.Nothing,
                                                dryRun = Core.Nothing, filters = Core.Nothing,
                                                maxResults = Core.Nothing, nextToken = Core.Nothing}

-- | The ID of the notification.
--
-- /Note:/ Consider using 'connectionNotificationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvecnsConnectionNotificationId :: Lens.Lens' DescribeVpcEndpointConnectionNotifications (Core.Maybe Types.ConnectionNotificationId)
dvecnsConnectionNotificationId = Lens.field @"connectionNotificationId"
{-# INLINEABLE dvecnsConnectionNotificationId #-}
{-# DEPRECATED connectionNotificationId "Use generic-lens or generic-optics with 'connectionNotificationId' instead"  #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvecnsDryRun :: Lens.Lens' DescribeVpcEndpointConnectionNotifications (Core.Maybe Core.Bool)
dvecnsDryRun = Lens.field @"dryRun"
{-# INLINEABLE dvecnsDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

-- | One or more filters.
--
--
--     * @connection-notification-arn@ - The ARN of the SNS topic for the notification.
--
--
--     * @connection-notification-id@ - The ID of the notification.
--
--
--     * @connection-notification-state@ - The state of the notification (@Enabled@ | @Disabled@ ).
--
--
--     * @connection-notification-type@ - The type of notification (@Topic@ ).
--
--
--     * @service-id@ - The ID of the endpoint service.
--
--
--     * @vpc-endpoint-id@ - The ID of the VPC endpoint.
--
--
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvecnsFilters :: Lens.Lens' DescribeVpcEndpointConnectionNotifications (Core.Maybe [Types.Filter])
dvecnsFilters = Lens.field @"filters"
{-# INLINEABLE dvecnsFilters #-}
{-# DEPRECATED filters "Use generic-lens or generic-optics with 'filters' instead"  #-}

-- | The maximum number of results to return in a single call. To retrieve the remaining results, make another request with the returned @NextToken@ value.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvecnsMaxResults :: Lens.Lens' DescribeVpcEndpointConnectionNotifications (Core.Maybe Core.Int)
dvecnsMaxResults = Lens.field @"maxResults"
{-# INLINEABLE dvecnsMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | The token to request the next page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvecnsNextToken :: Lens.Lens' DescribeVpcEndpointConnectionNotifications (Core.Maybe Core.Text)
dvecnsNextToken = Lens.field @"nextToken"
{-# INLINEABLE dvecnsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery DescribeVpcEndpointConnectionNotifications
         where
        toQuery DescribeVpcEndpointConnectionNotifications{..}
          = Core.toQueryPair "Action"
              ("DescribeVpcEndpointConnectionNotifications" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<>
              Core.maybe Core.mempty
                (Core.toQueryPair "ConnectionNotificationId")
                connectionNotificationId
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun
              Core.<> Core.maybe Core.mempty (Core.toQueryList "Filter") filters
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "MaxResults") maxResults
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "NextToken") nextToken

instance Core.ToHeaders DescribeVpcEndpointConnectionNotifications
         where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DescribeVpcEndpointConnectionNotifications
         where
        type Rs DescribeVpcEndpointConnectionNotifications =
             DescribeVpcEndpointConnectionNotificationsResponse
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
                 DescribeVpcEndpointConnectionNotificationsResponse' Core.<$>
                   (x Core..@? "connectionNotificationSet" Core..<@>
                      Core.parseXMLList "item")
                     Core.<*> x Core..@? "nextToken"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager DescribeVpcEndpointConnectionNotifications
         where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop
              (rs Lens.^?
                 Lens.field @"connectionNotificationSet" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkDescribeVpcEndpointConnectionNotificationsResponse' smart constructor.
data DescribeVpcEndpointConnectionNotificationsResponse = DescribeVpcEndpointConnectionNotificationsResponse'
  { connectionNotificationSet :: Core.Maybe [Types.ConnectionNotification]
    -- ^ One or more notifications.
  , nextToken :: Core.Maybe Core.Text
    -- ^ The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeVpcEndpointConnectionNotificationsResponse' value with any optional fields omitted.
mkDescribeVpcEndpointConnectionNotificationsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeVpcEndpointConnectionNotificationsResponse
mkDescribeVpcEndpointConnectionNotificationsResponse responseStatus
  = DescribeVpcEndpointConnectionNotificationsResponse'{connectionNotificationSet
                                                          = Core.Nothing,
                                                        nextToken = Core.Nothing, responseStatus}

-- | One or more notifications.
--
-- /Note:/ Consider using 'connectionNotificationSet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvecnrfrsConnectionNotificationSet :: Lens.Lens' DescribeVpcEndpointConnectionNotificationsResponse (Core.Maybe [Types.ConnectionNotification])
dvecnrfrsConnectionNotificationSet = Lens.field @"connectionNotificationSet"
{-# INLINEABLE dvecnrfrsConnectionNotificationSet #-}
{-# DEPRECATED connectionNotificationSet "Use generic-lens or generic-optics with 'connectionNotificationSet' instead"  #-}

-- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvecnrfrsNextToken :: Lens.Lens' DescribeVpcEndpointConnectionNotificationsResponse (Core.Maybe Core.Text)
dvecnrfrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE dvecnrfrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvecnrfrsResponseStatus :: Lens.Lens' DescribeVpcEndpointConnectionNotificationsResponse Core.Int
dvecnrfrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dvecnrfrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
