{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.CreatePortfolioShare
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Shares the specified portfolio with the specified account or organization node. Shares to an organization node can only be created by the management account of an organization or by a delegated administrator. You can share portfolios to an organization, an organizational unit, or a specific account.
--
-- Note that if a delegated admin is de-registered, they can no longer create portfolio shares.
-- @AWSOrganizationsAccess@ must be enabled in order to create a portfolio share to an organization node.
-- You can't share a shared resource. This includes portfolios that contain a shared product.
module Network.AWS.ServiceCatalog.CreatePortfolioShare
  ( -- * Creating a request
    CreatePortfolioShare (..),
    mkCreatePortfolioShare,

    -- ** Request lenses
    cpsPortfolioId,
    cpsAcceptLanguage,
    cpsAccountId,
    cpsOrganizationNode,

    -- * Destructuring the response
    CreatePortfolioShareResponse (..),
    mkCreatePortfolioShareResponse,

    -- ** Response lenses
    cpsrrsPortfolioShareToken,
    cpsrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.ServiceCatalog.Types as Types

-- | /See:/ 'mkCreatePortfolioShare' smart constructor.
data CreatePortfolioShare = CreatePortfolioShare'
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
    -- | The AWS account ID. For example, @123456789012@ .
    accountId :: Core.Maybe Types.AccountId,
    -- | The organization node to whom you are going to share. If @OrganizationNode@ is passed in, @PortfolioShare@ will be created for the node an ListOrganizationPortfolioAccessd its children (when applies), and a @PortfolioShareToken@ will be returned in the output in order for the administrator to monitor the status of the @PortfolioShare@ creation process.
    organizationNode :: Core.Maybe Types.OrganizationNode
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreatePortfolioShare' value with any optional fields omitted.
mkCreatePortfolioShare ::
  -- | 'portfolioId'
  Types.PortfolioId ->
  CreatePortfolioShare
mkCreatePortfolioShare portfolioId =
  CreatePortfolioShare'
    { portfolioId,
      acceptLanguage = Core.Nothing,
      accountId = Core.Nothing,
      organizationNode = Core.Nothing
    }

-- | The portfolio identifier.
--
-- /Note:/ Consider using 'portfolioId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpsPortfolioId :: Lens.Lens' CreatePortfolioShare Types.PortfolioId
cpsPortfolioId = Lens.field @"portfolioId"
{-# DEPRECATED cpsPortfolioId "Use generic-lens or generic-optics with 'portfolioId' instead." #-}

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
cpsAcceptLanguage :: Lens.Lens' CreatePortfolioShare (Core.Maybe Types.AcceptLanguage)
cpsAcceptLanguage = Lens.field @"acceptLanguage"
{-# DEPRECATED cpsAcceptLanguage "Use generic-lens or generic-optics with 'acceptLanguage' instead." #-}

-- | The AWS account ID. For example, @123456789012@ .
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpsAccountId :: Lens.Lens' CreatePortfolioShare (Core.Maybe Types.AccountId)
cpsAccountId = Lens.field @"accountId"
{-# DEPRECATED cpsAccountId "Use generic-lens or generic-optics with 'accountId' instead." #-}

-- | The organization node to whom you are going to share. If @OrganizationNode@ is passed in, @PortfolioShare@ will be created for the node an ListOrganizationPortfolioAccessd its children (when applies), and a @PortfolioShareToken@ will be returned in the output in order for the administrator to monitor the status of the @PortfolioShare@ creation process.
--
-- /Note:/ Consider using 'organizationNode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpsOrganizationNode :: Lens.Lens' CreatePortfolioShare (Core.Maybe Types.OrganizationNode)
cpsOrganizationNode = Lens.field @"organizationNode"
{-# DEPRECATED cpsOrganizationNode "Use generic-lens or generic-optics with 'organizationNode' instead." #-}

instance Core.FromJSON CreatePortfolioShare where
  toJSON CreatePortfolioShare {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("PortfolioId" Core..= portfolioId),
            ("AcceptLanguage" Core..=) Core.<$> acceptLanguage,
            ("AccountId" Core..=) Core.<$> accountId,
            ("OrganizationNode" Core..=) Core.<$> organizationNode
          ]
      )

instance Core.AWSRequest CreatePortfolioShare where
  type Rs CreatePortfolioShare = CreatePortfolioShareResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "AWS242ServiceCatalogService.CreatePortfolioShare"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CreatePortfolioShareResponse'
            Core.<$> (x Core..:? "PortfolioShareToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreatePortfolioShareResponse' smart constructor.
data CreatePortfolioShareResponse = CreatePortfolioShareResponse'
  { -- | The portfolio shares a unique identifier that only returns if the portfolio is shared to an organization node.
    portfolioShareToken :: Core.Maybe Types.PortfolioShareToken,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreatePortfolioShareResponse' value with any optional fields omitted.
mkCreatePortfolioShareResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreatePortfolioShareResponse
mkCreatePortfolioShareResponse responseStatus =
  CreatePortfolioShareResponse'
    { portfolioShareToken = Core.Nothing,
      responseStatus
    }

-- | The portfolio shares a unique identifier that only returns if the portfolio is shared to an organization node.
--
-- /Note:/ Consider using 'portfolioShareToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpsrrsPortfolioShareToken :: Lens.Lens' CreatePortfolioShareResponse (Core.Maybe Types.PortfolioShareToken)
cpsrrsPortfolioShareToken = Lens.field @"portfolioShareToken"
{-# DEPRECATED cpsrrsPortfolioShareToken "Use generic-lens or generic-optics with 'portfolioShareToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpsrrsResponseStatus :: Lens.Lens' CreatePortfolioShareResponse Core.Int
cpsrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED cpsrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
