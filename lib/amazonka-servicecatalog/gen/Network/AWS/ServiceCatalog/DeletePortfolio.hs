{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.DeletePortfolio
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified portfolio.
--
-- You cannot delete a portfolio if it was shared with you or if it has associated products, users, constraints, or shared accounts.
-- A delegated admin is authorized to invoke this command.
module Network.AWS.ServiceCatalog.DeletePortfolio
  ( -- * Creating a request
    DeletePortfolio (..),
    mkDeletePortfolio,

    -- ** Request lenses
    dphId,
    dphAcceptLanguage,

    -- * Destructuring the response
    DeletePortfolioResponse (..),
    mkDeletePortfolioResponse,

    -- ** Response lenses
    dprhrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.ServiceCatalog.Types as Types

-- | /See:/ 'mkDeletePortfolio' smart constructor.
data DeletePortfolio = DeletePortfolio'
  { -- | The portfolio identifier.
    id :: Types.Id,
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
    acceptLanguage :: Core.Maybe Types.AcceptLanguage
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeletePortfolio' value with any optional fields omitted.
mkDeletePortfolio ::
  -- | 'id'
  Types.Id ->
  DeletePortfolio
mkDeletePortfolio id =
  DeletePortfolio' {id, acceptLanguage = Core.Nothing}

-- | The portfolio identifier.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dphId :: Lens.Lens' DeletePortfolio Types.Id
dphId = Lens.field @"id"
{-# DEPRECATED dphId "Use generic-lens or generic-optics with 'id' instead." #-}

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
dphAcceptLanguage :: Lens.Lens' DeletePortfolio (Core.Maybe Types.AcceptLanguage)
dphAcceptLanguage = Lens.field @"acceptLanguage"
{-# DEPRECATED dphAcceptLanguage "Use generic-lens or generic-optics with 'acceptLanguage' instead." #-}

instance Core.FromJSON DeletePortfolio where
  toJSON DeletePortfolio {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Id" Core..= id),
            ("AcceptLanguage" Core..=) Core.<$> acceptLanguage
          ]
      )

instance Core.AWSRequest DeletePortfolio where
  type Rs DeletePortfolio = DeletePortfolioResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "AWS242ServiceCatalogService.DeletePortfolio")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeletePortfolioResponse' Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDeletePortfolioResponse' smart constructor.
newtype DeletePortfolioResponse = DeletePortfolioResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeletePortfolioResponse' value with any optional fields omitted.
mkDeletePortfolioResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeletePortfolioResponse
mkDeletePortfolioResponse responseStatus =
  DeletePortfolioResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dprhrsResponseStatus :: Lens.Lens' DeletePortfolioResponse Core.Int
dprhrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dprhrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
