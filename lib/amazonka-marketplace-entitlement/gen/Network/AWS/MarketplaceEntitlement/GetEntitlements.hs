{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MarketplaceEntitlement.GetEntitlements
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- GetEntitlements retrieves entitlement values for a given product. The results can be filtered based on customer identifier or product dimensions.
--
-- This operation returns paginated results.
module Network.AWS.MarketplaceEntitlement.GetEntitlements
    (
    -- * Creating a request
      GetEntitlements (..)
    , mkGetEntitlements
    -- ** Request lenses
    , geProductCode
    , geFilter
    , geMaxResults
    , geNextToken

    -- * Destructuring the response
    , GetEntitlementsResponse (..)
    , mkGetEntitlementsResponse
    -- ** Response lenses
    , gerrsEntitlements
    , gerrsNextToken
    , gerrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MarketplaceEntitlement.Types as Types
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The GetEntitlementsRequest contains parameters for the GetEntitlements operation.
--
-- /See:/ 'mkGetEntitlements' smart constructor.
data GetEntitlements = GetEntitlements'
  { productCode :: Types.ProductCode
    -- ^ Product code is used to uniquely identify a product in AWS Marketplace. The product code will be provided by AWS Marketplace when the product listing is created.
  , filter :: Core.Maybe (Core.HashMap Types.GetEntitlementFilterName (Core.NonEmpty Types.FilterValue))
    -- ^ Filter is used to return entitlements for a specific customer or for a specific dimension. Filters are described as keys mapped to a lists of values. Filtered requests are /unioned/ for each value in the value list, and then /intersected/ for each filter key.
  , maxResults :: Core.Maybe Core.Int
    -- ^ The maximum number of items to retrieve from the GetEntitlements operation. For pagination, use the NextToken field in subsequent calls to GetEntitlements.
  , nextToken :: Core.Maybe Types.NonEmptyString
    -- ^ For paginated calls to GetEntitlements, pass the NextToken from the previous GetEntitlementsResult.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetEntitlements' value with any optional fields omitted.
mkGetEntitlements
    :: Types.ProductCode -- ^ 'productCode'
    -> GetEntitlements
mkGetEntitlements productCode
  = GetEntitlements'{productCode, filter = Core.Nothing,
                     maxResults = Core.Nothing, nextToken = Core.Nothing}

-- | Product code is used to uniquely identify a product in AWS Marketplace. The product code will be provided by AWS Marketplace when the product listing is created.
--
-- /Note:/ Consider using 'productCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
geProductCode :: Lens.Lens' GetEntitlements Types.ProductCode
geProductCode = Lens.field @"productCode"
{-# INLINEABLE geProductCode #-}
{-# DEPRECATED productCode "Use generic-lens or generic-optics with 'productCode' instead"  #-}

-- | Filter is used to return entitlements for a specific customer or for a specific dimension. Filters are described as keys mapped to a lists of values. Filtered requests are /unioned/ for each value in the value list, and then /intersected/ for each filter key.
--
-- /Note:/ Consider using 'filter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
geFilter :: Lens.Lens' GetEntitlements (Core.Maybe (Core.HashMap Types.GetEntitlementFilterName (Core.NonEmpty Types.FilterValue)))
geFilter = Lens.field @"filter"
{-# INLINEABLE geFilter #-}
{-# DEPRECATED filter "Use generic-lens or generic-optics with 'filter' instead"  #-}

-- | The maximum number of items to retrieve from the GetEntitlements operation. For pagination, use the NextToken field in subsequent calls to GetEntitlements.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
geMaxResults :: Lens.Lens' GetEntitlements (Core.Maybe Core.Int)
geMaxResults = Lens.field @"maxResults"
{-# INLINEABLE geMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | For paginated calls to GetEntitlements, pass the NextToken from the previous GetEntitlementsResult.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
geNextToken :: Lens.Lens' GetEntitlements (Core.Maybe Types.NonEmptyString)
geNextToken = Lens.field @"nextToken"
{-# INLINEABLE geNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery GetEntitlements where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetEntitlements where
        toHeaders GetEntitlements{..}
          = Core.pure
              ("X-Amz-Target", "AWSMPEntitlementService.GetEntitlements")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GetEntitlements where
        toJSON GetEntitlements{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("ProductCode" Core..= productCode),
                  ("Filter" Core..=) Core.<$> filter,
                  ("MaxResults" Core..=) Core.<$> maxResults,
                  ("NextToken" Core..=) Core.<$> nextToken])

instance Core.AWSRequest GetEntitlements where
        type Rs GetEntitlements = GetEntitlementsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetEntitlementsResponse' Core.<$>
                   (x Core..:? "Entitlements") Core.<*> x Core..:? "NextToken"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager GetEntitlements where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"entitlements" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | The GetEntitlementsRequest contains results from the GetEntitlements operation.
--
-- /See:/ 'mkGetEntitlementsResponse' smart constructor.
data GetEntitlementsResponse = GetEntitlementsResponse'
  { entitlements :: Core.Maybe [Types.Entitlement]
    -- ^ The set of entitlements found through the GetEntitlements operation. If the result contains an empty set of entitlements, NextToken might still be present and should be used.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ For paginated results, use NextToken in subsequent calls to GetEntitlements. If the result contains an empty set of entitlements, NextToken might still be present and should be used.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'GetEntitlementsResponse' value with any optional fields omitted.
mkGetEntitlementsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetEntitlementsResponse
mkGetEntitlementsResponse responseStatus
  = GetEntitlementsResponse'{entitlements = Core.Nothing,
                             nextToken = Core.Nothing, responseStatus}

-- | The set of entitlements found through the GetEntitlements operation. If the result contains an empty set of entitlements, NextToken might still be present and should be used.
--
-- /Note:/ Consider using 'entitlements' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gerrsEntitlements :: Lens.Lens' GetEntitlementsResponse (Core.Maybe [Types.Entitlement])
gerrsEntitlements = Lens.field @"entitlements"
{-# INLINEABLE gerrsEntitlements #-}
{-# DEPRECATED entitlements "Use generic-lens or generic-optics with 'entitlements' instead"  #-}

-- | For paginated results, use NextToken in subsequent calls to GetEntitlements. If the result contains an empty set of entitlements, NextToken might still be present and should be used.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gerrsNextToken :: Lens.Lens' GetEntitlementsResponse (Core.Maybe Types.NextToken)
gerrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE gerrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gerrsResponseStatus :: Lens.Lens' GetEntitlementsResponse Core.Int
gerrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gerrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
