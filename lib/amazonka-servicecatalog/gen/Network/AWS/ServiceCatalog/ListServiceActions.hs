{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.ListServiceActions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all self-service actions.
--
-- This operation returns paginated results.
module Network.AWS.ServiceCatalog.ListServiceActions
  ( -- * Creating a request
    ListServiceActions (..),
    mkListServiceActions,

    -- ** Request lenses
    lsaAcceptLanguage,
    lsaPageSize,
    lsaPageToken,

    -- * Destructuring the response
    ListServiceActionsResponse (..),
    mkListServiceActionsResponse,

    -- ** Response lenses
    lsarrsNextPageToken,
    lsarrsServiceActionSummaries,
    lsarrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.ServiceCatalog.Types as Types

-- | /See:/ 'mkListServiceActions' smart constructor.
data ListServiceActions = ListServiceActions'
  { -- | The language code.
    --
    --
    --     * @en@ - English (default)
    --
    --
    --     * @jp@ - Japanese
    --
    --
    --     * @zh@ - Chinese
    acceptLanguage :: Core.Maybe Types.AcceptLanguage,
    -- | The maximum number of items to return with this call.
    pageSize :: Core.Maybe Core.Natural,
    -- | The page token for the next set of results. To retrieve the first set of results, use null.
    pageToken :: Core.Maybe Types.PageToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListServiceActions' value with any optional fields omitted.
mkListServiceActions ::
  ListServiceActions
mkListServiceActions =
  ListServiceActions'
    { acceptLanguage = Core.Nothing,
      pageSize = Core.Nothing,
      pageToken = Core.Nothing
    }

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
lsaAcceptLanguage :: Lens.Lens' ListServiceActions (Core.Maybe Types.AcceptLanguage)
lsaAcceptLanguage = Lens.field @"acceptLanguage"
{-# DEPRECATED lsaAcceptLanguage "Use generic-lens or generic-optics with 'acceptLanguage' instead." #-}

-- | The maximum number of items to return with this call.
--
-- /Note:/ Consider using 'pageSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsaPageSize :: Lens.Lens' ListServiceActions (Core.Maybe Core.Natural)
lsaPageSize = Lens.field @"pageSize"
{-# DEPRECATED lsaPageSize "Use generic-lens or generic-optics with 'pageSize' instead." #-}

-- | The page token for the next set of results. To retrieve the first set of results, use null.
--
-- /Note:/ Consider using 'pageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsaPageToken :: Lens.Lens' ListServiceActions (Core.Maybe Types.PageToken)
lsaPageToken = Lens.field @"pageToken"
{-# DEPRECATED lsaPageToken "Use generic-lens or generic-optics with 'pageToken' instead." #-}

instance Core.FromJSON ListServiceActions where
  toJSON ListServiceActions {..} =
    Core.object
      ( Core.catMaybes
          [ ("AcceptLanguage" Core..=) Core.<$> acceptLanguage,
            ("PageSize" Core..=) Core.<$> pageSize,
            ("PageToken" Core..=) Core.<$> pageToken
          ]
      )

instance Core.AWSRequest ListServiceActions where
  type Rs ListServiceActions = ListServiceActionsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "AWS242ServiceCatalogService.ListServiceActions")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListServiceActionsResponse'
            Core.<$> (x Core..:? "NextPageToken")
            Core.<*> (x Core..:? "ServiceActionSummaries")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListServiceActions where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextPageToken") =
      Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"serviceActionSummaries" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"pageToken"
            Lens..~ rs Lens.^. Lens.field @"nextPageToken"
        )

-- | /See:/ 'mkListServiceActionsResponse' smart constructor.
data ListServiceActionsResponse = ListServiceActionsResponse'
  { -- | The page token to use to retrieve the next set of results. If there are no additional results, this value is null.
    nextPageToken :: Core.Maybe Types.NextPageToken,
    -- | An object containing information about the service actions associated with the provisioning artifact.
    serviceActionSummaries :: Core.Maybe [Types.ServiceActionSummary],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListServiceActionsResponse' value with any optional fields omitted.
mkListServiceActionsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListServiceActionsResponse
mkListServiceActionsResponse responseStatus =
  ListServiceActionsResponse'
    { nextPageToken = Core.Nothing,
      serviceActionSummaries = Core.Nothing,
      responseStatus
    }

-- | The page token to use to retrieve the next set of results. If there are no additional results, this value is null.
--
-- /Note:/ Consider using 'nextPageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsarrsNextPageToken :: Lens.Lens' ListServiceActionsResponse (Core.Maybe Types.NextPageToken)
lsarrsNextPageToken = Lens.field @"nextPageToken"
{-# DEPRECATED lsarrsNextPageToken "Use generic-lens or generic-optics with 'nextPageToken' instead." #-}

-- | An object containing information about the service actions associated with the provisioning artifact.
--
-- /Note:/ Consider using 'serviceActionSummaries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsarrsServiceActionSummaries :: Lens.Lens' ListServiceActionsResponse (Core.Maybe [Types.ServiceActionSummary])
lsarrsServiceActionSummaries = Lens.field @"serviceActionSummaries"
{-# DEPRECATED lsarrsServiceActionSummaries "Use generic-lens or generic-optics with 'serviceActionSummaries' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsarrsResponseStatus :: Lens.Lens' ListServiceActionsResponse Core.Int
lsarrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED lsarrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
