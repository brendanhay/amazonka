{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.DeletePortfolioShare
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops sharing the specified portfolio with the specified account or organization node. Shares to an organization node can only be deleted by the management account of an organization or by a delegated administrator.
--
-- Note that if a delegated admin is de-registered, portfolio shares created from that account are removed.
module Network.AWS.ServiceCatalog.DeletePortfolioShare
  ( -- * Creating a request
    DeletePortfolioShare (..),
    mkDeletePortfolioShare,

    -- ** Request lenses
    dpsPortfolioId,
    dpsAcceptLanguage,
    dpsAccountId,
    dpsOrganizationNode,

    -- * Destructuring the response
    DeletePortfolioShareResponse (..),
    mkDeletePortfolioShareResponse,

    -- ** Response lenses
    dpsrrsPortfolioShareToken,
    dpsrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.ServiceCatalog.Types as Types

-- | /See:/ 'mkDeletePortfolioShare' smart constructor.
data DeletePortfolioShare = DeletePortfolioShare'
  { -- | The portfolio identifier.
    portfolioId :: Types.Id,
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
    -- | The AWS account ID.
    accountId :: Core.Maybe Types.AccountId,
    -- | The organization node to whom you are going to stop sharing.
    organizationNode :: Core.Maybe Types.OrganizationNode
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeletePortfolioShare' value with any optional fields omitted.
mkDeletePortfolioShare ::
  -- | 'portfolioId'
  Types.Id ->
  DeletePortfolioShare
mkDeletePortfolioShare portfolioId =
  DeletePortfolioShare'
    { portfolioId,
      acceptLanguage = Core.Nothing,
      accountId = Core.Nothing,
      organizationNode = Core.Nothing
    }

-- | The portfolio identifier.
--
-- /Note:/ Consider using 'portfolioId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpsPortfolioId :: Lens.Lens' DeletePortfolioShare Types.Id
dpsPortfolioId = Lens.field @"portfolioId"
{-# DEPRECATED dpsPortfolioId "Use generic-lens or generic-optics with 'portfolioId' instead." #-}

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
dpsAcceptLanguage :: Lens.Lens' DeletePortfolioShare (Core.Maybe Types.AcceptLanguage)
dpsAcceptLanguage = Lens.field @"acceptLanguage"
{-# DEPRECATED dpsAcceptLanguage "Use generic-lens or generic-optics with 'acceptLanguage' instead." #-}

-- | The AWS account ID.
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpsAccountId :: Lens.Lens' DeletePortfolioShare (Core.Maybe Types.AccountId)
dpsAccountId = Lens.field @"accountId"
{-# DEPRECATED dpsAccountId "Use generic-lens or generic-optics with 'accountId' instead." #-}

-- | The organization node to whom you are going to stop sharing.
--
-- /Note:/ Consider using 'organizationNode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpsOrganizationNode :: Lens.Lens' DeletePortfolioShare (Core.Maybe Types.OrganizationNode)
dpsOrganizationNode = Lens.field @"organizationNode"
{-# DEPRECATED dpsOrganizationNode "Use generic-lens or generic-optics with 'organizationNode' instead." #-}

instance Core.FromJSON DeletePortfolioShare where
  toJSON DeletePortfolioShare {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("PortfolioId" Core..= portfolioId),
            ("AcceptLanguage" Core..=) Core.<$> acceptLanguage,
            ("AccountId" Core..=) Core.<$> accountId,
            ("OrganizationNode" Core..=) Core.<$> organizationNode
          ]
      )

instance Core.AWSRequest DeletePortfolioShare where
  type Rs DeletePortfolioShare = DeletePortfolioShareResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "AWS242ServiceCatalogService.DeletePortfolioShare"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DeletePortfolioShareResponse'
            Core.<$> (x Core..:? "PortfolioShareToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDeletePortfolioShareResponse' smart constructor.
data DeletePortfolioShareResponse = DeletePortfolioShareResponse'
  { -- | The portfolio share unique identifier. This will only be returned if delete is made to an organization node.
    portfolioShareToken :: Core.Maybe Types.Id,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeletePortfolioShareResponse' value with any optional fields omitted.
mkDeletePortfolioShareResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeletePortfolioShareResponse
mkDeletePortfolioShareResponse responseStatus =
  DeletePortfolioShareResponse'
    { portfolioShareToken = Core.Nothing,
      responseStatus
    }

-- | The portfolio share unique identifier. This will only be returned if delete is made to an organization node.
--
-- /Note:/ Consider using 'portfolioShareToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpsrrsPortfolioShareToken :: Lens.Lens' DeletePortfolioShareResponse (Core.Maybe Types.Id)
dpsrrsPortfolioShareToken = Lens.field @"portfolioShareToken"
{-# DEPRECATED dpsrrsPortfolioShareToken "Use generic-lens or generic-optics with 'portfolioShareToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpsrrsResponseStatus :: Lens.Lens' DeletePortfolioShareResponse Core.Int
dpsrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dpsrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
