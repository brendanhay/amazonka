{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.ListComplianceItems
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- For a specified resource ID, this API action returns a list of compliance statuses for different resource types. Currently, you can only specify one resource ID per call. List results depend on the criteria specified in the filter.
--
-- This operation returns paginated results.
module Network.AWS.SSM.ListComplianceItems
    (
    -- * Creating a request
      ListComplianceItems (..)
    , mkListComplianceItems
    -- ** Request lenses
    , lFilters
    , lMaxResults
    , lNextToken
    , lResourceIds
    , lResourceTypes

    -- * Destructuring the response
    , ListComplianceItemsResponse (..)
    , mkListComplianceItemsResponse
    -- ** Response lenses
    , lrsComplianceItems
    , lrsNextToken
    , lrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SSM.Types as Types

-- | /See:/ 'mkListComplianceItems' smart constructor.
data ListComplianceItems = ListComplianceItems'
  { filters :: Core.Maybe [Types.ComplianceStringFilter]
    -- ^ One or more compliance filters. Use a filter to return a more specific list of results.
  , maxResults :: Core.Maybe Core.Natural
    -- ^ The maximum number of items to return for this call. The call also returns a token that you can specify in a subsequent call to get the next set of results.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ A token to start the list. Use this token to get the next set of results. 
  , resourceIds :: Core.Maybe (Core.NonEmpty Types.ComplianceResourceId)
    -- ^ The ID for the resources from which to get compliance information. Currently, you can only specify one resource ID.
  , resourceTypes :: Core.Maybe (Core.NonEmpty Types.ComplianceResourceType)
    -- ^ The type of resource from which to get compliance information. Currently, the only supported resource type is @ManagedInstance@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListComplianceItems' value with any optional fields omitted.
mkListComplianceItems
    :: ListComplianceItems
mkListComplianceItems
  = ListComplianceItems'{filters = Core.Nothing,
                         maxResults = Core.Nothing, nextToken = Core.Nothing,
                         resourceIds = Core.Nothing, resourceTypes = Core.Nothing}

-- | One or more compliance filters. Use a filter to return a more specific list of results.
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lFilters :: Lens.Lens' ListComplianceItems (Core.Maybe [Types.ComplianceStringFilter])
lFilters = Lens.field @"filters"
{-# INLINEABLE lFilters #-}
{-# DEPRECATED filters "Use generic-lens or generic-optics with 'filters' instead"  #-}

-- | The maximum number of items to return for this call. The call also returns a token that you can specify in a subsequent call to get the next set of results.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lMaxResults :: Lens.Lens' ListComplianceItems (Core.Maybe Core.Natural)
lMaxResults = Lens.field @"maxResults"
{-# INLINEABLE lMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | A token to start the list. Use this token to get the next set of results. 
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lNextToken :: Lens.Lens' ListComplianceItems (Core.Maybe Types.NextToken)
lNextToken = Lens.field @"nextToken"
{-# INLINEABLE lNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The ID for the resources from which to get compliance information. Currently, you can only specify one resource ID.
--
-- /Note:/ Consider using 'resourceIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lResourceIds :: Lens.Lens' ListComplianceItems (Core.Maybe (Core.NonEmpty Types.ComplianceResourceId))
lResourceIds = Lens.field @"resourceIds"
{-# INLINEABLE lResourceIds #-}
{-# DEPRECATED resourceIds "Use generic-lens or generic-optics with 'resourceIds' instead"  #-}

-- | The type of resource from which to get compliance information. Currently, the only supported resource type is @ManagedInstance@ .
--
-- /Note:/ Consider using 'resourceTypes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lResourceTypes :: Lens.Lens' ListComplianceItems (Core.Maybe (Core.NonEmpty Types.ComplianceResourceType))
lResourceTypes = Lens.field @"resourceTypes"
{-# INLINEABLE lResourceTypes #-}
{-# DEPRECATED resourceTypes "Use generic-lens or generic-optics with 'resourceTypes' instead"  #-}

instance Core.ToQuery ListComplianceItems where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ListComplianceItems where
        toHeaders ListComplianceItems{..}
          = Core.pure ("X-Amz-Target", "AmazonSSM.ListComplianceItems")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ListComplianceItems where
        toJSON ListComplianceItems{..}
          = Core.object
              (Core.catMaybes
                 [("Filters" Core..=) Core.<$> filters,
                  ("MaxResults" Core..=) Core.<$> maxResults,
                  ("NextToken" Core..=) Core.<$> nextToken,
                  ("ResourceIds" Core..=) Core.<$> resourceIds,
                  ("ResourceTypes" Core..=) Core.<$> resourceTypes])

instance Core.AWSRequest ListComplianceItems where
        type Rs ListComplianceItems = ListComplianceItemsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListComplianceItemsResponse' Core.<$>
                   (x Core..:? "ComplianceItems") Core.<*> x Core..:? "NextToken"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListComplianceItems where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"complianceItems" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkListComplianceItemsResponse' smart constructor.
data ListComplianceItemsResponse = ListComplianceItemsResponse'
  { complianceItems :: Core.Maybe [Types.ComplianceItem]
    -- ^ A list of compliance information for the specified resource ID. 
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ The token for the next set of items to return. Use this token to get the next set of results.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ListComplianceItemsResponse' value with any optional fields omitted.
mkListComplianceItemsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListComplianceItemsResponse
mkListComplianceItemsResponse responseStatus
  = ListComplianceItemsResponse'{complianceItems = Core.Nothing,
                                 nextToken = Core.Nothing, responseStatus}

-- | A list of compliance information for the specified resource ID. 
--
-- /Note:/ Consider using 'complianceItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrsComplianceItems :: Lens.Lens' ListComplianceItemsResponse (Core.Maybe [Types.ComplianceItem])
lrsComplianceItems = Lens.field @"complianceItems"
{-# INLINEABLE lrsComplianceItems #-}
{-# DEPRECATED complianceItems "Use generic-lens or generic-optics with 'complianceItems' instead"  #-}

-- | The token for the next set of items to return. Use this token to get the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrsNextToken :: Lens.Lens' ListComplianceItemsResponse (Core.Maybe Types.NextToken)
lrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE lrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrsResponseStatus :: Lens.Lens' ListComplianceItemsResponse Core.Int
lrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
