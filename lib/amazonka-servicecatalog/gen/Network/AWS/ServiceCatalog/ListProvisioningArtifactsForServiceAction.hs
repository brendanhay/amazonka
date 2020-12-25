{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.ListProvisioningArtifactsForServiceAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all provisioning artifacts (also known as versions) for the specified self-service action.
--
-- This operation returns paginated results.
module Network.AWS.ServiceCatalog.ListProvisioningArtifactsForServiceAction
  ( -- * Creating a request
    ListProvisioningArtifactsForServiceAction (..),
    mkListProvisioningArtifactsForServiceAction,

    -- ** Request lenses
    lpafsaServiceActionId,
    lpafsaAcceptLanguage,
    lpafsaPageSize,
    lpafsaPageToken,

    -- * Destructuring the response
    ListProvisioningArtifactsForServiceActionResponse (..),
    mkListProvisioningArtifactsForServiceActionResponse,

    -- ** Response lenses
    lpafsarrsNextPageToken,
    lpafsarrsProvisioningArtifactViews,
    lpafsarrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.ServiceCatalog.Types as Types

-- | /See:/ 'mkListProvisioningArtifactsForServiceAction' smart constructor.
data ListProvisioningArtifactsForServiceAction = ListProvisioningArtifactsForServiceAction'
  { -- | The self-service action identifier. For example, @act-fs7abcd89wxyz@ .
    serviceActionId :: Types.Id,
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
    acceptLanguage :: Core.Maybe Types.AcceptLanguage,
    -- | The maximum number of items to return with this call.
    pageSize :: Core.Maybe Core.Natural,
    -- | The page token for the next set of results. To retrieve the first set of results, use null.
    pageToken :: Core.Maybe Types.PageToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListProvisioningArtifactsForServiceAction' value with any optional fields omitted.
mkListProvisioningArtifactsForServiceAction ::
  -- | 'serviceActionId'
  Types.Id ->
  ListProvisioningArtifactsForServiceAction
mkListProvisioningArtifactsForServiceAction serviceActionId =
  ListProvisioningArtifactsForServiceAction'
    { serviceActionId,
      acceptLanguage = Core.Nothing,
      pageSize = Core.Nothing,
      pageToken = Core.Nothing
    }

-- | The self-service action identifier. For example, @act-fs7abcd89wxyz@ .
--
-- /Note:/ Consider using 'serviceActionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpafsaServiceActionId :: Lens.Lens' ListProvisioningArtifactsForServiceAction Types.Id
lpafsaServiceActionId = Lens.field @"serviceActionId"
{-# DEPRECATED lpafsaServiceActionId "Use generic-lens or generic-optics with 'serviceActionId' instead." #-}

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
lpafsaAcceptLanguage :: Lens.Lens' ListProvisioningArtifactsForServiceAction (Core.Maybe Types.AcceptLanguage)
lpafsaAcceptLanguage = Lens.field @"acceptLanguage"
{-# DEPRECATED lpafsaAcceptLanguage "Use generic-lens or generic-optics with 'acceptLanguage' instead." #-}

-- | The maximum number of items to return with this call.
--
-- /Note:/ Consider using 'pageSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpafsaPageSize :: Lens.Lens' ListProvisioningArtifactsForServiceAction (Core.Maybe Core.Natural)
lpafsaPageSize = Lens.field @"pageSize"
{-# DEPRECATED lpafsaPageSize "Use generic-lens or generic-optics with 'pageSize' instead." #-}

-- | The page token for the next set of results. To retrieve the first set of results, use null.
--
-- /Note:/ Consider using 'pageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpafsaPageToken :: Lens.Lens' ListProvisioningArtifactsForServiceAction (Core.Maybe Types.PageToken)
lpafsaPageToken = Lens.field @"pageToken"
{-# DEPRECATED lpafsaPageToken "Use generic-lens or generic-optics with 'pageToken' instead." #-}

instance Core.FromJSON ListProvisioningArtifactsForServiceAction where
  toJSON ListProvisioningArtifactsForServiceAction {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("ServiceActionId" Core..= serviceActionId),
            ("AcceptLanguage" Core..=) Core.<$> acceptLanguage,
            ("PageSize" Core..=) Core.<$> pageSize,
            ("PageToken" Core..=) Core.<$> pageToken
          ]
      )

instance Core.AWSRequest ListProvisioningArtifactsForServiceAction where
  type
    Rs ListProvisioningArtifactsForServiceAction =
      ListProvisioningArtifactsForServiceActionResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "AWS242ServiceCatalogService.ListProvisioningArtifactsForServiceAction"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListProvisioningArtifactsForServiceActionResponse'
            Core.<$> (x Core..:? "NextPageToken")
            Core.<*> (x Core..:? "ProvisioningArtifactViews")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListProvisioningArtifactsForServiceAction where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextPageToken") =
      Core.Nothing
    | Pager.stop
        ( rs
            Lens.^? Lens.field @"provisioningArtifactViews" Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"pageToken"
            Lens..~ rs Lens.^. Lens.field @"nextPageToken"
        )

-- | /See:/ 'mkListProvisioningArtifactsForServiceActionResponse' smart constructor.
data ListProvisioningArtifactsForServiceActionResponse = ListProvisioningArtifactsForServiceActionResponse'
  { -- | The page token to use to retrieve the next set of results. If there are no additional results, this value is null.
    nextPageToken :: Core.Maybe Types.PageToken,
    -- | An array of objects with information about product views and provisioning artifacts.
    provisioningArtifactViews :: Core.Maybe [Types.ProvisioningArtifactView],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ListProvisioningArtifactsForServiceActionResponse' value with any optional fields omitted.
mkListProvisioningArtifactsForServiceActionResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListProvisioningArtifactsForServiceActionResponse
mkListProvisioningArtifactsForServiceActionResponse responseStatus =
  ListProvisioningArtifactsForServiceActionResponse'
    { nextPageToken =
        Core.Nothing,
      provisioningArtifactViews = Core.Nothing,
      responseStatus
    }

-- | The page token to use to retrieve the next set of results. If there are no additional results, this value is null.
--
-- /Note:/ Consider using 'nextPageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpafsarrsNextPageToken :: Lens.Lens' ListProvisioningArtifactsForServiceActionResponse (Core.Maybe Types.PageToken)
lpafsarrsNextPageToken = Lens.field @"nextPageToken"
{-# DEPRECATED lpafsarrsNextPageToken "Use generic-lens or generic-optics with 'nextPageToken' instead." #-}

-- | An array of objects with information about product views and provisioning artifacts.
--
-- /Note:/ Consider using 'provisioningArtifactViews' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpafsarrsProvisioningArtifactViews :: Lens.Lens' ListProvisioningArtifactsForServiceActionResponse (Core.Maybe [Types.ProvisioningArtifactView])
lpafsarrsProvisioningArtifactViews = Lens.field @"provisioningArtifactViews"
{-# DEPRECATED lpafsarrsProvisioningArtifactViews "Use generic-lens or generic-optics with 'provisioningArtifactViews' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpafsarrsResponseStatus :: Lens.Lens' ListProvisioningArtifactsForServiceActionResponse Core.Int
lpafsarrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED lpafsarrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
