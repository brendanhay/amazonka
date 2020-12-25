{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.ListPortfolioAccess
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the account IDs that have access to the specified portfolio.
--
-- A delegated admin can list the accounts that have access to the shared portfolio. Note that if a delegated admin is de-registered, they can no longer perform this operation.
module Network.AWS.ServiceCatalog.ListPortfolioAccess
  ( -- * Creating a request
    ListPortfolioAccess (..),
    mkListPortfolioAccess,

    -- ** Request lenses
    lPortfolioId,
    lAcceptLanguage,
    lOrganizationParentId,
    lPageSize,
    lPageToken,

    -- * Destructuring the response
    ListPortfolioAccessResponse (..),
    mkListPortfolioAccessResponse,

    -- ** Response lenses
    lrsAccountIds,
    lrsNextPageToken,
    lrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.ServiceCatalog.Types as Types

-- | /See:/ 'mkListPortfolioAccess' smart constructor.
data ListPortfolioAccess = ListPortfolioAccess'
  { -- | The portfolio identifier.
    portfolioId :: Types.PortfolioId,
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
    -- | The ID of an organization node the portfolio is shared with. All children of this node with an inherited portfolio share will be returned.
    organizationParentId :: Core.Maybe Types.OrganizationParentId,
    -- | The maximum number of items to return with this call.
    pageSize :: Core.Maybe Core.Natural,
    -- | The page token for the next set of results. To retrieve the first set of results, use null.
    pageToken :: Core.Maybe Types.PageToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListPortfolioAccess' value with any optional fields omitted.
mkListPortfolioAccess ::
  -- | 'portfolioId'
  Types.PortfolioId ->
  ListPortfolioAccess
mkListPortfolioAccess portfolioId =
  ListPortfolioAccess'
    { portfolioId,
      acceptLanguage = Core.Nothing,
      organizationParentId = Core.Nothing,
      pageSize = Core.Nothing,
      pageToken = Core.Nothing
    }

-- | The portfolio identifier.
--
-- /Note:/ Consider using 'portfolioId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lPortfolioId :: Lens.Lens' ListPortfolioAccess Types.PortfolioId
lPortfolioId = Lens.field @"portfolioId"
{-# DEPRECATED lPortfolioId "Use generic-lens or generic-optics with 'portfolioId' instead." #-}

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
lAcceptLanguage :: Lens.Lens' ListPortfolioAccess (Core.Maybe Types.AcceptLanguage)
lAcceptLanguage = Lens.field @"acceptLanguage"
{-# DEPRECATED lAcceptLanguage "Use generic-lens or generic-optics with 'acceptLanguage' instead." #-}

-- | The ID of an organization node the portfolio is shared with. All children of this node with an inherited portfolio share will be returned.
--
-- /Note:/ Consider using 'organizationParentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lOrganizationParentId :: Lens.Lens' ListPortfolioAccess (Core.Maybe Types.OrganizationParentId)
lOrganizationParentId = Lens.field @"organizationParentId"
{-# DEPRECATED lOrganizationParentId "Use generic-lens or generic-optics with 'organizationParentId' instead." #-}

-- | The maximum number of items to return with this call.
--
-- /Note:/ Consider using 'pageSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lPageSize :: Lens.Lens' ListPortfolioAccess (Core.Maybe Core.Natural)
lPageSize = Lens.field @"pageSize"
{-# DEPRECATED lPageSize "Use generic-lens or generic-optics with 'pageSize' instead." #-}

-- | The page token for the next set of results. To retrieve the first set of results, use null.
--
-- /Note:/ Consider using 'pageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lPageToken :: Lens.Lens' ListPortfolioAccess (Core.Maybe Types.PageToken)
lPageToken = Lens.field @"pageToken"
{-# DEPRECATED lPageToken "Use generic-lens or generic-optics with 'pageToken' instead." #-}

instance Core.FromJSON ListPortfolioAccess where
  toJSON ListPortfolioAccess {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("PortfolioId" Core..= portfolioId),
            ("AcceptLanguage" Core..=) Core.<$> acceptLanguage,
            ("OrganizationParentId" Core..=) Core.<$> organizationParentId,
            ("PageSize" Core..=) Core.<$> pageSize,
            ("PageToken" Core..=) Core.<$> pageToken
          ]
      )

instance Core.AWSRequest ListPortfolioAccess where
  type Rs ListPortfolioAccess = ListPortfolioAccessResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "AWS242ServiceCatalogService.ListPortfolioAccess")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListPortfolioAccessResponse'
            Core.<$> (x Core..:? "AccountIds")
            Core.<*> (x Core..:? "NextPageToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkListPortfolioAccessResponse' smart constructor.
data ListPortfolioAccessResponse = ListPortfolioAccessResponse'
  { -- | Information about the AWS accounts with access to the portfolio.
    accountIds :: Core.Maybe [Types.AccountId],
    -- | The page token to use to retrieve the next set of results. If there are no additional results, this value is null.
    nextPageToken :: Core.Maybe Types.NextPageToken,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListPortfolioAccessResponse' value with any optional fields omitted.
mkListPortfolioAccessResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListPortfolioAccessResponse
mkListPortfolioAccessResponse responseStatus =
  ListPortfolioAccessResponse'
    { accountIds = Core.Nothing,
      nextPageToken = Core.Nothing,
      responseStatus
    }

-- | Information about the AWS accounts with access to the portfolio.
--
-- /Note:/ Consider using 'accountIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrsAccountIds :: Lens.Lens' ListPortfolioAccessResponse (Core.Maybe [Types.AccountId])
lrsAccountIds = Lens.field @"accountIds"
{-# DEPRECATED lrsAccountIds "Use generic-lens or generic-optics with 'accountIds' instead." #-}

-- | The page token to use to retrieve the next set of results. If there are no additional results, this value is null.
--
-- /Note:/ Consider using 'nextPageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrsNextPageToken :: Lens.Lens' ListPortfolioAccessResponse (Core.Maybe Types.NextPageToken)
lrsNextPageToken = Lens.field @"nextPageToken"
{-# DEPRECATED lrsNextPageToken "Use generic-lens or generic-optics with 'nextPageToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrsResponseStatus :: Lens.Lens' ListPortfolioAccessResponse Core.Int
lrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED lrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
