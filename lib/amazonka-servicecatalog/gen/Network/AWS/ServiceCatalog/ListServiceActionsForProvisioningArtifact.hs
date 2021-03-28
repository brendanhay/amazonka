{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.ListServiceActionsForProvisioningArtifact
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a paginated list of self-service actions associated with the specified Product ID and Provisioning Artifact ID.
--
-- This operation returns paginated results.
module Network.AWS.ServiceCatalog.ListServiceActionsForProvisioningArtifact
    (
    -- * Creating a request
      ListServiceActionsForProvisioningArtifact (..)
    , mkListServiceActionsForProvisioningArtifact
    -- ** Request lenses
    , lsafpaProductId
    , lsafpaProvisioningArtifactId
    , lsafpaAcceptLanguage
    , lsafpaPageSize
    , lsafpaPageToken

    -- * Destructuring the response
    , ListServiceActionsForProvisioningArtifactResponse (..)
    , mkListServiceActionsForProvisioningArtifactResponse
    -- ** Response lenses
    , lsafparrsNextPageToken
    , lsafparrsServiceActionSummaries
    , lsafparrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.ServiceCatalog.Types as Types

-- | /See:/ 'mkListServiceActionsForProvisioningArtifact' smart constructor.
data ListServiceActionsForProvisioningArtifact = ListServiceActionsForProvisioningArtifact'
  { productId :: Types.ProductId
    -- ^ The product identifier. For example, @prod-abcdzk7xy33qa@ .
  , provisioningArtifactId :: Types.ProvisioningArtifactId
    -- ^ The identifier of the provisioning artifact. For example, @pa-4abcdjnxjj6ne@ .
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

-- | Creates a 'ListServiceActionsForProvisioningArtifact' value with any optional fields omitted.
mkListServiceActionsForProvisioningArtifact
    :: Types.ProductId -- ^ 'productId'
    -> Types.ProvisioningArtifactId -- ^ 'provisioningArtifactId'
    -> ListServiceActionsForProvisioningArtifact
mkListServiceActionsForProvisioningArtifact productId
  provisioningArtifactId
  = ListServiceActionsForProvisioningArtifact'{productId,
                                               provisioningArtifactId,
                                               acceptLanguage = Core.Nothing,
                                               pageSize = Core.Nothing, pageToken = Core.Nothing}

-- | The product identifier. For example, @prod-abcdzk7xy33qa@ .
--
-- /Note:/ Consider using 'productId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsafpaProductId :: Lens.Lens' ListServiceActionsForProvisioningArtifact Types.ProductId
lsafpaProductId = Lens.field @"productId"
{-# INLINEABLE lsafpaProductId #-}
{-# DEPRECATED productId "Use generic-lens or generic-optics with 'productId' instead"  #-}

-- | The identifier of the provisioning artifact. For example, @pa-4abcdjnxjj6ne@ .
--
-- /Note:/ Consider using 'provisioningArtifactId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsafpaProvisioningArtifactId :: Lens.Lens' ListServiceActionsForProvisioningArtifact Types.ProvisioningArtifactId
lsafpaProvisioningArtifactId = Lens.field @"provisioningArtifactId"
{-# INLINEABLE lsafpaProvisioningArtifactId #-}
{-# DEPRECATED provisioningArtifactId "Use generic-lens or generic-optics with 'provisioningArtifactId' instead"  #-}

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
lsafpaAcceptLanguage :: Lens.Lens' ListServiceActionsForProvisioningArtifact (Core.Maybe Types.AcceptLanguage)
lsafpaAcceptLanguage = Lens.field @"acceptLanguage"
{-# INLINEABLE lsafpaAcceptLanguage #-}
{-# DEPRECATED acceptLanguage "Use generic-lens or generic-optics with 'acceptLanguage' instead"  #-}

-- | The maximum number of items to return with this call.
--
-- /Note:/ Consider using 'pageSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsafpaPageSize :: Lens.Lens' ListServiceActionsForProvisioningArtifact (Core.Maybe Core.Natural)
lsafpaPageSize = Lens.field @"pageSize"
{-# INLINEABLE lsafpaPageSize #-}
{-# DEPRECATED pageSize "Use generic-lens or generic-optics with 'pageSize' instead"  #-}

-- | The page token for the next set of results. To retrieve the first set of results, use null.
--
-- /Note:/ Consider using 'pageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsafpaPageToken :: Lens.Lens' ListServiceActionsForProvisioningArtifact (Core.Maybe Types.PageToken)
lsafpaPageToken = Lens.field @"pageToken"
{-# INLINEABLE lsafpaPageToken #-}
{-# DEPRECATED pageToken "Use generic-lens or generic-optics with 'pageToken' instead"  #-}

instance Core.ToQuery ListServiceActionsForProvisioningArtifact
         where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ListServiceActionsForProvisioningArtifact
         where
        toHeaders ListServiceActionsForProvisioningArtifact{..}
          = Core.pure
              ("X-Amz-Target",
               "AWS242ServiceCatalogService.ListServiceActionsForProvisioningArtifact")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ListServiceActionsForProvisioningArtifact
         where
        toJSON ListServiceActionsForProvisioningArtifact{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("ProductId" Core..= productId),
                  Core.Just
                    ("ProvisioningArtifactId" Core..= provisioningArtifactId),
                  ("AcceptLanguage" Core..=) Core.<$> acceptLanguage,
                  ("PageSize" Core..=) Core.<$> pageSize,
                  ("PageToken" Core..=) Core.<$> pageToken])

instance Core.AWSRequest ListServiceActionsForProvisioningArtifact
         where
        type Rs ListServiceActionsForProvisioningArtifact =
             ListServiceActionsForProvisioningArtifactResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListServiceActionsForProvisioningArtifactResponse' Core.<$>
                   (x Core..:? "NextPageToken") Core.<*>
                     x Core..:? "ServiceActionSummaries"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListServiceActionsForProvisioningArtifact
         where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextPageToken") =
            Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"serviceActionSummaries" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"pageToken" Lens..~
                   rs Lens.^. Lens.field @"nextPageToken")

-- | /See:/ 'mkListServiceActionsForProvisioningArtifactResponse' smart constructor.
data ListServiceActionsForProvisioningArtifactResponse = ListServiceActionsForProvisioningArtifactResponse'
  { nextPageToken :: Core.Maybe Types.NextPageToken
    -- ^ The page token to use to retrieve the next set of results. If there are no additional results, this value is null.
  , serviceActionSummaries :: Core.Maybe [Types.ServiceActionSummary]
    -- ^ An object containing information about the self-service actions associated with the provisioning artifact.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListServiceActionsForProvisioningArtifactResponse' value with any optional fields omitted.
mkListServiceActionsForProvisioningArtifactResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListServiceActionsForProvisioningArtifactResponse
mkListServiceActionsForProvisioningArtifactResponse responseStatus
  = ListServiceActionsForProvisioningArtifactResponse'{nextPageToken
                                                         = Core.Nothing,
                                                       serviceActionSummaries = Core.Nothing,
                                                       responseStatus}

-- | The page token to use to retrieve the next set of results. If there are no additional results, this value is null.
--
-- /Note:/ Consider using 'nextPageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsafparrsNextPageToken :: Lens.Lens' ListServiceActionsForProvisioningArtifactResponse (Core.Maybe Types.NextPageToken)
lsafparrsNextPageToken = Lens.field @"nextPageToken"
{-# INLINEABLE lsafparrsNextPageToken #-}
{-# DEPRECATED nextPageToken "Use generic-lens or generic-optics with 'nextPageToken' instead"  #-}

-- | An object containing information about the self-service actions associated with the provisioning artifact.
--
-- /Note:/ Consider using 'serviceActionSummaries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsafparrsServiceActionSummaries :: Lens.Lens' ListServiceActionsForProvisioningArtifactResponse (Core.Maybe [Types.ServiceActionSummary])
lsafparrsServiceActionSummaries = Lens.field @"serviceActionSummaries"
{-# INLINEABLE lsafparrsServiceActionSummaries #-}
{-# DEPRECATED serviceActionSummaries "Use generic-lens or generic-optics with 'serviceActionSummaries' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsafparrsResponseStatus :: Lens.Lens' ListServiceActionsForProvisioningArtifactResponse Core.Int
lsafparrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lsafparrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
