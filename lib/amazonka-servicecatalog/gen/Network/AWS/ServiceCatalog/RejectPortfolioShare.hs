{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.RejectPortfolioShare
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Rejects an offer to share the specified portfolio.
module Network.AWS.ServiceCatalog.RejectPortfolioShare
  ( -- * Creating a request
    RejectPortfolioShare (..),
    mkRejectPortfolioShare,

    -- ** Request lenses
    rpsPortfolioId,
    rpsAcceptLanguage,
    rpsPortfolioShareType,

    -- * Destructuring the response
    RejectPortfolioShareResponse (..),
    mkRejectPortfolioShareResponse,

    -- ** Response lenses
    rpsrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.ServiceCatalog.Types as Types

-- | /See:/ 'mkRejectPortfolioShare' smart constructor.
data RejectPortfolioShare = RejectPortfolioShare'
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
    -- | The type of shared portfolios to reject. The default is to reject imported portfolios.
    --
    --
    --     * @AWS_ORGANIZATIONS@ - Reject portfolios shared by the management account of your organization.
    --
    --
    --     * @IMPORTED@ - Reject imported portfolios.
    --
    --
    --     * @AWS_SERVICECATALOG@ - Not supported. (Throws ResourceNotFoundException.)
    --
    --
    -- For example, @aws servicecatalog reject-portfolio-share --portfolio-id "port-2qwzkwxt3y5fk" --portfolio-share-type AWS_ORGANIZATIONS@
    portfolioShareType :: Core.Maybe Types.PortfolioShareType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RejectPortfolioShare' value with any optional fields omitted.
mkRejectPortfolioShare ::
  -- | 'portfolioId'
  Types.Id ->
  RejectPortfolioShare
mkRejectPortfolioShare portfolioId =
  RejectPortfolioShare'
    { portfolioId,
      acceptLanguage = Core.Nothing,
      portfolioShareType = Core.Nothing
    }

-- | The portfolio identifier.
--
-- /Note:/ Consider using 'portfolioId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpsPortfolioId :: Lens.Lens' RejectPortfolioShare Types.Id
rpsPortfolioId = Lens.field @"portfolioId"
{-# DEPRECATED rpsPortfolioId "Use generic-lens or generic-optics with 'portfolioId' instead." #-}

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
rpsAcceptLanguage :: Lens.Lens' RejectPortfolioShare (Core.Maybe Types.AcceptLanguage)
rpsAcceptLanguage = Lens.field @"acceptLanguage"
{-# DEPRECATED rpsAcceptLanguage "Use generic-lens or generic-optics with 'acceptLanguage' instead." #-}

-- | The type of shared portfolios to reject. The default is to reject imported portfolios.
--
--
--     * @AWS_ORGANIZATIONS@ - Reject portfolios shared by the management account of your organization.
--
--
--     * @IMPORTED@ - Reject imported portfolios.
--
--
--     * @AWS_SERVICECATALOG@ - Not supported. (Throws ResourceNotFoundException.)
--
--
-- For example, @aws servicecatalog reject-portfolio-share --portfolio-id "port-2qwzkwxt3y5fk" --portfolio-share-type AWS_ORGANIZATIONS@
--
-- /Note:/ Consider using 'portfolioShareType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpsPortfolioShareType :: Lens.Lens' RejectPortfolioShare (Core.Maybe Types.PortfolioShareType)
rpsPortfolioShareType = Lens.field @"portfolioShareType"
{-# DEPRECATED rpsPortfolioShareType "Use generic-lens or generic-optics with 'portfolioShareType' instead." #-}

instance Core.FromJSON RejectPortfolioShare where
  toJSON RejectPortfolioShare {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("PortfolioId" Core..= portfolioId),
            ("AcceptLanguage" Core..=) Core.<$> acceptLanguage,
            ("PortfolioShareType" Core..=) Core.<$> portfolioShareType
          ]
      )

instance Core.AWSRequest RejectPortfolioShare where
  type Rs RejectPortfolioShare = RejectPortfolioShareResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "AWS242ServiceCatalogService.RejectPortfolioShare"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          RejectPortfolioShareResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkRejectPortfolioShareResponse' smart constructor.
newtype RejectPortfolioShareResponse = RejectPortfolioShareResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'RejectPortfolioShareResponse' value with any optional fields omitted.
mkRejectPortfolioShareResponse ::
  -- | 'responseStatus'
  Core.Int ->
  RejectPortfolioShareResponse
mkRejectPortfolioShareResponse responseStatus =
  RejectPortfolioShareResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpsrrsResponseStatus :: Lens.Lens' RejectPortfolioShareResponse Core.Int
rpsrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED rpsrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
