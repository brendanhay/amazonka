{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.ListStackInstancesForProvisionedProduct
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns summary information about stack instances that are associated with the specified @CFN_STACKSET@ type provisioned product. You can filter for stack instances that are associated with a specific AWS account name or region. 
module Network.AWS.ServiceCatalog.ListStackInstancesForProvisionedProduct
    (
    -- * Creating a request
      ListStackInstancesForProvisionedProduct (..)
    , mkListStackInstancesForProvisionedProduct
    -- ** Request lenses
    , lsifppProvisionedProductId
    , lsifppAcceptLanguage
    , lsifppPageSize
    , lsifppPageToken

    -- * Destructuring the response
    , ListStackInstancesForProvisionedProductResponse (..)
    , mkListStackInstancesForProvisionedProductResponse
    -- ** Response lenses
    , lsifpprrsNextPageToken
    , lsifpprrsStackInstances
    , lsifpprrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.ServiceCatalog.Types as Types

-- | /See:/ 'mkListStackInstancesForProvisionedProduct' smart constructor.
data ListStackInstancesForProvisionedProduct = ListStackInstancesForProvisionedProduct'
  { provisionedProductId :: Types.ProvisionedProductId
    -- ^ The identifier of the provisioned product.
  , acceptLanguage :: Core.Maybe Types.AcceptLanguage
    -- ^ The language code.
--
--
--     * @en@ - English (default)
--
--
--     * @jp@ - Japanese
--
--
--     * @zh@ - Chinese
--
--
  , pageSize :: Core.Maybe Core.Natural
    -- ^ The maximum number of items to return with this call.
  , pageToken :: Core.Maybe Types.PageToken
    -- ^ The page token for the next set of results. To retrieve the first set of results, use null.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListStackInstancesForProvisionedProduct' value with any optional fields omitted.
mkListStackInstancesForProvisionedProduct
    :: Types.ProvisionedProductId -- ^ 'provisionedProductId'
    -> ListStackInstancesForProvisionedProduct
mkListStackInstancesForProvisionedProduct provisionedProductId
  = ListStackInstancesForProvisionedProduct'{provisionedProductId,
                                             acceptLanguage = Core.Nothing, pageSize = Core.Nothing,
                                             pageToken = Core.Nothing}

-- | The identifier of the provisioned product.
--
-- /Note:/ Consider using 'provisionedProductId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsifppProvisionedProductId :: Lens.Lens' ListStackInstancesForProvisionedProduct Types.ProvisionedProductId
lsifppProvisionedProductId = Lens.field @"provisionedProductId"
{-# INLINEABLE lsifppProvisionedProductId #-}
{-# DEPRECATED provisionedProductId "Use generic-lens or generic-optics with 'provisionedProductId' instead"  #-}

-- | The language code.
--
--
--     * @en@ - English (default)
--
--
--     * @jp@ - Japanese
--
--
--     * @zh@ - Chinese
--
--
--
-- /Note:/ Consider using 'acceptLanguage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsifppAcceptLanguage :: Lens.Lens' ListStackInstancesForProvisionedProduct (Core.Maybe Types.AcceptLanguage)
lsifppAcceptLanguage = Lens.field @"acceptLanguage"
{-# INLINEABLE lsifppAcceptLanguage #-}
{-# DEPRECATED acceptLanguage "Use generic-lens or generic-optics with 'acceptLanguage' instead"  #-}

-- | The maximum number of items to return with this call.
--
-- /Note:/ Consider using 'pageSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsifppPageSize :: Lens.Lens' ListStackInstancesForProvisionedProduct (Core.Maybe Core.Natural)
lsifppPageSize = Lens.field @"pageSize"
{-# INLINEABLE lsifppPageSize #-}
{-# DEPRECATED pageSize "Use generic-lens or generic-optics with 'pageSize' instead"  #-}

-- | The page token for the next set of results. To retrieve the first set of results, use null.
--
-- /Note:/ Consider using 'pageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsifppPageToken :: Lens.Lens' ListStackInstancesForProvisionedProduct (Core.Maybe Types.PageToken)
lsifppPageToken = Lens.field @"pageToken"
{-# INLINEABLE lsifppPageToken #-}
{-# DEPRECATED pageToken "Use generic-lens or generic-optics with 'pageToken' instead"  #-}

instance Core.ToQuery ListStackInstancesForProvisionedProduct where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ListStackInstancesForProvisionedProduct
         where
        toHeaders ListStackInstancesForProvisionedProduct{..}
          = Core.pure
              ("X-Amz-Target",
               "AWS242ServiceCatalogService.ListStackInstancesForProvisionedProduct")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ListStackInstancesForProvisionedProduct
         where
        toJSON ListStackInstancesForProvisionedProduct{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("ProvisionedProductId" Core..= provisionedProductId),
                  ("AcceptLanguage" Core..=) Core.<$> acceptLanguage,
                  ("PageSize" Core..=) Core.<$> pageSize,
                  ("PageToken" Core..=) Core.<$> pageToken])

instance Core.AWSRequest ListStackInstancesForProvisionedProduct
         where
        type Rs ListStackInstancesForProvisionedProduct =
             ListStackInstancesForProvisionedProductResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListStackInstancesForProvisionedProductResponse' Core.<$>
                   (x Core..:? "NextPageToken") Core.<*> x Core..:? "StackInstances"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkListStackInstancesForProvisionedProductResponse' smart constructor.
data ListStackInstancesForProvisionedProductResponse = ListStackInstancesForProvisionedProductResponse'
  { nextPageToken :: Core.Maybe Types.NextPageToken
    -- ^ The page token to use to retrieve the next set of results. If there are no additional results, this value is null.
  , stackInstances :: Core.Maybe [Types.StackInstance]
    -- ^ List of stack instances.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListStackInstancesForProvisionedProductResponse' value with any optional fields omitted.
mkListStackInstancesForProvisionedProductResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListStackInstancesForProvisionedProductResponse
mkListStackInstancesForProvisionedProductResponse responseStatus
  = ListStackInstancesForProvisionedProductResponse'{nextPageToken =
                                                       Core.Nothing,
                                                     stackInstances = Core.Nothing, responseStatus}

-- | The page token to use to retrieve the next set of results. If there are no additional results, this value is null.
--
-- /Note:/ Consider using 'nextPageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsifpprrsNextPageToken :: Lens.Lens' ListStackInstancesForProvisionedProductResponse (Core.Maybe Types.NextPageToken)
lsifpprrsNextPageToken = Lens.field @"nextPageToken"
{-# INLINEABLE lsifpprrsNextPageToken #-}
{-# DEPRECATED nextPageToken "Use generic-lens or generic-optics with 'nextPageToken' instead"  #-}

-- | List of stack instances.
--
-- /Note:/ Consider using 'stackInstances' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsifpprrsStackInstances :: Lens.Lens' ListStackInstancesForProvisionedProductResponse (Core.Maybe [Types.StackInstance])
lsifpprrsStackInstances = Lens.field @"stackInstances"
{-# INLINEABLE lsifpprrsStackInstances #-}
{-# DEPRECATED stackInstances "Use generic-lens or generic-optics with 'stackInstances' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsifpprrsResponseStatus :: Lens.Lens' ListStackInstancesForProvisionedProductResponse Core.Int
lsifpprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lsifpprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
