{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      DescribePortfolio (..)
    , mkDescribePortfolio
    -- ** Request lenses
    , dpfId
    , dpfAcceptLanguage

    -- * Destructuring the response
    , DescribePortfolioResponse (..)
    , mkDescribePortfolioResponse
    -- ** Response lenses
    , dprfrsBudgets
    , dprfrsPortfolioDetail
    , dprfrsTagOptions
    , dprfrsTags
    , dprfrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.ServiceCatalog.Types as Types

-- | /See:/ 'mkDescribePortfolio' smart constructor.
data DescribePortfolio = DescribePortfolio'
  { id :: Types.Id
    -- ^ The portfolio identifier.
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
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribePortfolio' value with any optional fields omitted.
mkDescribePortfolio
    :: Types.Id -- ^ 'id'
    -> DescribePortfolio
mkDescribePortfolio id
  = DescribePortfolio'{id, acceptLanguage = Core.Nothing}

-- | The portfolio identifier.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpfId :: Lens.Lens' DescribePortfolio Types.Id
dpfId = Lens.field @"id"
{-# INLINEABLE dpfId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

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
{-# INLINEABLE dpfAcceptLanguage #-}
{-# DEPRECATED acceptLanguage "Use generic-lens or generic-optics with 'acceptLanguage' instead"  #-}

instance Core.ToQuery DescribePortfolio where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribePortfolio where
        toHeaders DescribePortfolio{..}
          = Core.pure
              ("X-Amz-Target", "AWS242ServiceCatalogService.DescribePortfolio")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribePortfolio where
        toJSON DescribePortfolio{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("Id" Core..= id),
                  ("AcceptLanguage" Core..=) Core.<$> acceptLanguage])

instance Core.AWSRequest DescribePortfolio where
        type Rs DescribePortfolio = DescribePortfolioResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribePortfolioResponse' Core.<$>
                   (x Core..:? "Budgets") Core.<*> x Core..:? "PortfolioDetail"
                     Core.<*> x Core..:? "TagOptions"
                     Core.<*> x Core..:? "Tags"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDescribePortfolioResponse' smart constructor.
data DescribePortfolioResponse = DescribePortfolioResponse'
  { budgets :: Core.Maybe [Types.BudgetDetail]
    -- ^ Information about the associated budgets.
  , portfolioDetail :: Core.Maybe Types.PortfolioDetail
    -- ^ Information about the portfolio.
  , tagOptions :: Core.Maybe [Types.TagOptionDetail]
    -- ^ Information about the TagOptions associated with the portfolio.
  , tags :: Core.Maybe [Types.Tag]
    -- ^ Information about the tags associated with the portfolio.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribePortfolioResponse' value with any optional fields omitted.
mkDescribePortfolioResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribePortfolioResponse
mkDescribePortfolioResponse responseStatus
  = DescribePortfolioResponse'{budgets = Core.Nothing,
                               portfolioDetail = Core.Nothing, tagOptions = Core.Nothing,
                               tags = Core.Nothing, responseStatus}

-- | Information about the associated budgets.
--
-- /Note:/ Consider using 'budgets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dprfrsBudgets :: Lens.Lens' DescribePortfolioResponse (Core.Maybe [Types.BudgetDetail])
dprfrsBudgets = Lens.field @"budgets"
{-# INLINEABLE dprfrsBudgets #-}
{-# DEPRECATED budgets "Use generic-lens or generic-optics with 'budgets' instead"  #-}

-- | Information about the portfolio.
--
-- /Note:/ Consider using 'portfolioDetail' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dprfrsPortfolioDetail :: Lens.Lens' DescribePortfolioResponse (Core.Maybe Types.PortfolioDetail)
dprfrsPortfolioDetail = Lens.field @"portfolioDetail"
{-# INLINEABLE dprfrsPortfolioDetail #-}
{-# DEPRECATED portfolioDetail "Use generic-lens or generic-optics with 'portfolioDetail' instead"  #-}

-- | Information about the TagOptions associated with the portfolio.
--
-- /Note:/ Consider using 'tagOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dprfrsTagOptions :: Lens.Lens' DescribePortfolioResponse (Core.Maybe [Types.TagOptionDetail])
dprfrsTagOptions = Lens.field @"tagOptions"
{-# INLINEABLE dprfrsTagOptions #-}
{-# DEPRECATED tagOptions "Use generic-lens or generic-optics with 'tagOptions' instead"  #-}

-- | Information about the tags associated with the portfolio.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dprfrsTags :: Lens.Lens' DescribePortfolioResponse (Core.Maybe [Types.Tag])
dprfrsTags = Lens.field @"tags"
{-# INLINEABLE dprfrsTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dprfrsResponseStatus :: Lens.Lens' DescribePortfolioResponse Core.Int
dprfrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dprfrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
