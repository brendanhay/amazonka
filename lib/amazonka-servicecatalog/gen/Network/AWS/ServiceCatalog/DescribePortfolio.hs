{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.DescribePortfolio
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about the specified portfolio.
--
-- A delegated admin is authorized to invoke this command.
module Network.AWS.ServiceCatalog.DescribePortfolio
  ( -- * Creating a request
    DescribePortfolio (..),
    mkDescribePortfolio,

    -- ** Request lenses
    dpfId,
    dpfAcceptLanguage,

    -- * Destructuring the response
    DescribePortfolioResponse (..),
    mkDescribePortfolioResponse,

    -- ** Response lenses
    dprfrsBudgets,
    dprfrsPortfolioDetail,
    dprfrsTagOptions,
    dprfrsTags,
    dprfrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.ServiceCatalog.Types as Types

-- | /See:/ 'mkDescribePortfolio' smart constructor.
data DescribePortfolio = DescribePortfolio'
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

-- | Creates a 'DescribePortfolio' value with any optional fields omitted.
mkDescribePortfolio ::
  -- | 'id'
  Types.Id ->
  DescribePortfolio
mkDescribePortfolio id =
  DescribePortfolio' {id, acceptLanguage = Core.Nothing}

-- | The portfolio identifier.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpfId :: Lens.Lens' DescribePortfolio Types.Id
dpfId = Lens.field @"id"
{-# DEPRECATED dpfId "Use generic-lens or generic-optics with 'id' instead." #-}

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
dpfAcceptLanguage :: Lens.Lens' DescribePortfolio (Core.Maybe Types.AcceptLanguage)
dpfAcceptLanguage = Lens.field @"acceptLanguage"
{-# DEPRECATED dpfAcceptLanguage "Use generic-lens or generic-optics with 'acceptLanguage' instead." #-}

instance Core.FromJSON DescribePortfolio where
  toJSON DescribePortfolio {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Id" Core..= id),
            ("AcceptLanguage" Core..=) Core.<$> acceptLanguage
          ]
      )

instance Core.AWSRequest DescribePortfolio where
  type Rs DescribePortfolio = DescribePortfolioResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "AWS242ServiceCatalogService.DescribePortfolio")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribePortfolioResponse'
            Core.<$> (x Core..:? "Budgets")
            Core.<*> (x Core..:? "PortfolioDetail")
            Core.<*> (x Core..:? "TagOptions")
            Core.<*> (x Core..:? "Tags")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDescribePortfolioResponse' smart constructor.
data DescribePortfolioResponse = DescribePortfolioResponse'
  { -- | Information about the associated budgets.
    budgets :: Core.Maybe [Types.BudgetDetail],
    -- | Information about the portfolio.
    portfolioDetail :: Core.Maybe Types.PortfolioDetail,
    -- | Information about the TagOptions associated with the portfolio.
    tagOptions :: Core.Maybe [Types.TagOptionDetail],
    -- | Information about the tags associated with the portfolio.
    tags :: Core.Maybe [Types.Tag],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribePortfolioResponse' value with any optional fields omitted.
mkDescribePortfolioResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribePortfolioResponse
mkDescribePortfolioResponse responseStatus =
  DescribePortfolioResponse'
    { budgets = Core.Nothing,
      portfolioDetail = Core.Nothing,
      tagOptions = Core.Nothing,
      tags = Core.Nothing,
      responseStatus
    }

-- | Information about the associated budgets.
--
-- /Note:/ Consider using 'budgets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dprfrsBudgets :: Lens.Lens' DescribePortfolioResponse (Core.Maybe [Types.BudgetDetail])
dprfrsBudgets = Lens.field @"budgets"
{-# DEPRECATED dprfrsBudgets "Use generic-lens or generic-optics with 'budgets' instead." #-}

-- | Information about the portfolio.
--
-- /Note:/ Consider using 'portfolioDetail' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dprfrsPortfolioDetail :: Lens.Lens' DescribePortfolioResponse (Core.Maybe Types.PortfolioDetail)
dprfrsPortfolioDetail = Lens.field @"portfolioDetail"
{-# DEPRECATED dprfrsPortfolioDetail "Use generic-lens or generic-optics with 'portfolioDetail' instead." #-}

-- | Information about the TagOptions associated with the portfolio.
--
-- /Note:/ Consider using 'tagOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dprfrsTagOptions :: Lens.Lens' DescribePortfolioResponse (Core.Maybe [Types.TagOptionDetail])
dprfrsTagOptions = Lens.field @"tagOptions"
{-# DEPRECATED dprfrsTagOptions "Use generic-lens or generic-optics with 'tagOptions' instead." #-}

-- | Information about the tags associated with the portfolio.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dprfrsTags :: Lens.Lens' DescribePortfolioResponse (Core.Maybe [Types.Tag])
dprfrsTags = Lens.field @"tags"
{-# DEPRECATED dprfrsTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dprfrsResponseStatus :: Lens.Lens' DescribePortfolioResponse Core.Int
dprfrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dprfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
