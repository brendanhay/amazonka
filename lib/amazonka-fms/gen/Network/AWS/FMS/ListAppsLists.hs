{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.FMS.ListAppsLists
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns an array of @AppsListDataSummary@ objects.
module Network.AWS.FMS.ListAppsLists
    (
    -- * Creating a request
      ListAppsLists (..)
    , mkListAppsLists
    -- ** Request lenses
    , lalMaxResults
    , lalDefaultLists
    , lalNextToken

    -- * Destructuring the response
    , ListAppsListsResponse (..)
    , mkListAppsListsResponse
    -- ** Response lenses
    , lalrrsAppsLists
    , lalrrsNextToken
    , lalrrsResponseStatus
    ) where

import qualified Network.AWS.FMS.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListAppsLists' smart constructor.
data ListAppsLists = ListAppsLists'
  { maxResults :: Core.Natural
    -- ^ The maximum number of objects that you want AWS Firewall Manager to return for this request. If more objects are available, in the response, AWS Firewall Manager provides a @NextToken@ value that you can use in a subsequent call to get the next batch of objects.
--
-- If you don't specify this, AWS Firewall Manager returns all available objects.
  , defaultLists :: Core.Maybe Core.Bool
    -- ^ Specifies whether the lists to retrieve are default lists owned by AWS Firewall Manager.
  , nextToken :: Core.Maybe Types.PaginationToken
    -- ^ If you specify a value for @MaxResults@ in your list request, and you have more objects than the maximum, AWS Firewall Manager returns this token in the response. For all but the first request, you provide the token returned by the prior request in the request parameters, to retrieve the next batch of objects.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListAppsLists' value with any optional fields omitted.
mkListAppsLists
    :: Core.Natural -- ^ 'maxResults'
    -> ListAppsLists
mkListAppsLists maxResults
  = ListAppsLists'{maxResults, defaultLists = Core.Nothing,
                   nextToken = Core.Nothing}

-- | The maximum number of objects that you want AWS Firewall Manager to return for this request. If more objects are available, in the response, AWS Firewall Manager provides a @NextToken@ value that you can use in a subsequent call to get the next batch of objects.
--
-- If you don't specify this, AWS Firewall Manager returns all available objects.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lalMaxResults :: Lens.Lens' ListAppsLists Core.Natural
lalMaxResults = Lens.field @"maxResults"
{-# INLINEABLE lalMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | Specifies whether the lists to retrieve are default lists owned by AWS Firewall Manager.
--
-- /Note:/ Consider using 'defaultLists' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lalDefaultLists :: Lens.Lens' ListAppsLists (Core.Maybe Core.Bool)
lalDefaultLists = Lens.field @"defaultLists"
{-# INLINEABLE lalDefaultLists #-}
{-# DEPRECATED defaultLists "Use generic-lens or generic-optics with 'defaultLists' instead"  #-}

-- | If you specify a value for @MaxResults@ in your list request, and you have more objects than the maximum, AWS Firewall Manager returns this token in the response. For all but the first request, you provide the token returned by the prior request in the request parameters, to retrieve the next batch of objects.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lalNextToken :: Lens.Lens' ListAppsLists (Core.Maybe Types.PaginationToken)
lalNextToken = Lens.field @"nextToken"
{-# INLINEABLE lalNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery ListAppsLists where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ListAppsLists where
        toHeaders ListAppsLists{..}
          = Core.pure ("X-Amz-Target", "AWSFMS_20180101.ListAppsLists")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ListAppsLists where
        toJSON ListAppsLists{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("MaxResults" Core..= maxResults),
                  ("DefaultLists" Core..=) Core.<$> defaultLists,
                  ("NextToken" Core..=) Core.<$> nextToken])

instance Core.AWSRequest ListAppsLists where
        type Rs ListAppsLists = ListAppsListsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListAppsListsResponse' Core.<$>
                   (x Core..:? "AppsLists") Core.<*> x Core..:? "NextToken" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkListAppsListsResponse' smart constructor.
data ListAppsListsResponse = ListAppsListsResponse'
  { appsLists :: Core.Maybe [Types.AppsListDataSummary]
    -- ^ An array of @AppsListDataSummary@ objects.
  , nextToken :: Core.Maybe Types.PaginationToken
    -- ^ If you specify a value for @MaxResults@ in your list request, and you have more objects than the maximum, AWS Firewall Manager returns this token in the response. You can use this token in subsequent requests to retrieve the next batch of objects.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListAppsListsResponse' value with any optional fields omitted.
mkListAppsListsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListAppsListsResponse
mkListAppsListsResponse responseStatus
  = ListAppsListsResponse'{appsLists = Core.Nothing,
                           nextToken = Core.Nothing, responseStatus}

-- | An array of @AppsListDataSummary@ objects.
--
-- /Note:/ Consider using 'appsLists' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lalrrsAppsLists :: Lens.Lens' ListAppsListsResponse (Core.Maybe [Types.AppsListDataSummary])
lalrrsAppsLists = Lens.field @"appsLists"
{-# INLINEABLE lalrrsAppsLists #-}
{-# DEPRECATED appsLists "Use generic-lens or generic-optics with 'appsLists' instead"  #-}

-- | If you specify a value for @MaxResults@ in your list request, and you have more objects than the maximum, AWS Firewall Manager returns this token in the response. You can use this token in subsequent requests to retrieve the next batch of objects.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lalrrsNextToken :: Lens.Lens' ListAppsListsResponse (Core.Maybe Types.PaginationToken)
lalrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE lalrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lalrrsResponseStatus :: Lens.Lens' ListAppsListsResponse Core.Int
lalrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lalrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
