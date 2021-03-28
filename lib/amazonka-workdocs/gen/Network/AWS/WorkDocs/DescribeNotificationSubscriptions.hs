{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkDocs.DescribeNotificationSubscriptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the specified notification subscriptions.
--
-- This operation returns paginated results.
module Network.AWS.WorkDocs.DescribeNotificationSubscriptions
    (
    -- * Creating a request
      DescribeNotificationSubscriptions (..)
    , mkDescribeNotificationSubscriptions
    -- ** Request lenses
    , dOrganizationId
    , dLimit
    , dMarker

    -- * Destructuring the response
    , DescribeNotificationSubscriptionsResponse (..)
    , mkDescribeNotificationSubscriptionsResponse
    -- ** Response lenses
    , dnsrrsMarker
    , dnsrrsSubscriptions
    , dnsrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WorkDocs.Types as Types

-- | /See:/ 'mkDescribeNotificationSubscriptions' smart constructor.
data DescribeNotificationSubscriptions = DescribeNotificationSubscriptions'
  { organizationId :: Types.IdType
    -- ^ The ID of the organization.
  , limit :: Core.Maybe Core.Natural
    -- ^ The maximum number of items to return with this call.
  , marker :: Core.Maybe Types.PageMarkerType
    -- ^ The marker for the next set of results. (You received this marker from a previous call.)
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeNotificationSubscriptions' value with any optional fields omitted.
mkDescribeNotificationSubscriptions
    :: Types.IdType -- ^ 'organizationId'
    -> DescribeNotificationSubscriptions
mkDescribeNotificationSubscriptions organizationId
  = DescribeNotificationSubscriptions'{organizationId,
                                       limit = Core.Nothing, marker = Core.Nothing}

-- | The ID of the organization.
--
-- /Note:/ Consider using 'organizationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dOrganizationId :: Lens.Lens' DescribeNotificationSubscriptions Types.IdType
dOrganizationId = Lens.field @"organizationId"
{-# INLINEABLE dOrganizationId #-}
{-# DEPRECATED organizationId "Use generic-lens or generic-optics with 'organizationId' instead"  #-}

-- | The maximum number of items to return with this call.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dLimit :: Lens.Lens' DescribeNotificationSubscriptions (Core.Maybe Core.Natural)
dLimit = Lens.field @"limit"
{-# INLINEABLE dLimit #-}
{-# DEPRECATED limit "Use generic-lens or generic-optics with 'limit' instead"  #-}

-- | The marker for the next set of results. (You received this marker from a previous call.)
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dMarker :: Lens.Lens' DescribeNotificationSubscriptions (Core.Maybe Types.PageMarkerType)
dMarker = Lens.field @"marker"
{-# INLINEABLE dMarker #-}
{-# DEPRECATED marker "Use generic-lens or generic-optics with 'marker' instead"  #-}

instance Core.ToQuery DescribeNotificationSubscriptions where
        toQuery DescribeNotificationSubscriptions{..}
          = Core.maybe Core.mempty (Core.toQueryPair "limit") limit Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "marker") marker

instance Core.ToHeaders DescribeNotificationSubscriptions where
        toHeaders DescribeNotificationSubscriptions{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.AWSRequest DescribeNotificationSubscriptions where
        type Rs DescribeNotificationSubscriptions =
             DescribeNotificationSubscriptionsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath =
                           "/api/v1/organizations/" Core.<> Core.toText organizationId Core.<>
                             "/subscriptions",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeNotificationSubscriptionsResponse' Core.<$>
                   (x Core..:? "Marker") Core.<*> x Core..:? "Subscriptions" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager DescribeNotificationSubscriptions where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"marker") = Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"subscriptions" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"marker" Lens..~ rs Lens.^. Lens.field @"marker")

-- | /See:/ 'mkDescribeNotificationSubscriptionsResponse' smart constructor.
data DescribeNotificationSubscriptionsResponse = DescribeNotificationSubscriptionsResponse'
  { marker :: Core.Maybe Types.Marker
    -- ^ The marker to use when requesting the next set of results. If there are no additional results, the string is empty.
  , subscriptions :: Core.Maybe [Types.Subscription]
    -- ^ The subscriptions.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeNotificationSubscriptionsResponse' value with any optional fields omitted.
mkDescribeNotificationSubscriptionsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeNotificationSubscriptionsResponse
mkDescribeNotificationSubscriptionsResponse responseStatus
  = DescribeNotificationSubscriptionsResponse'{marker = Core.Nothing,
                                               subscriptions = Core.Nothing, responseStatus}

-- | The marker to use when requesting the next set of results. If there are no additional results, the string is empty.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnsrrsMarker :: Lens.Lens' DescribeNotificationSubscriptionsResponse (Core.Maybe Types.Marker)
dnsrrsMarker = Lens.field @"marker"
{-# INLINEABLE dnsrrsMarker #-}
{-# DEPRECATED marker "Use generic-lens or generic-optics with 'marker' instead"  #-}

-- | The subscriptions.
--
-- /Note:/ Consider using 'subscriptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnsrrsSubscriptions :: Lens.Lens' DescribeNotificationSubscriptionsResponse (Core.Maybe [Types.Subscription])
dnsrrsSubscriptions = Lens.field @"subscriptions"
{-# INLINEABLE dnsrrsSubscriptions #-}
{-# DEPRECATED subscriptions "Use generic-lens or generic-optics with 'subscriptions' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnsrrsResponseStatus :: Lens.Lens' DescribeNotificationSubscriptionsResponse Core.Int
dnsrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dnsrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
