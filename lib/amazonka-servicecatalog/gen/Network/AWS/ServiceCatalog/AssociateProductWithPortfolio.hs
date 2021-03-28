{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.AssociateProductWithPortfolio
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates the specified product with the specified portfolio.
--
-- A delegated admin is authorized to invoke this command.
module Network.AWS.ServiceCatalog.AssociateProductWithPortfolio
    (
    -- * Creating a request
      AssociateProductWithPortfolio (..)
    , mkAssociateProductWithPortfolio
    -- ** Request lenses
    , apwpProductId
    , apwpPortfolioId
    , apwpAcceptLanguage
    , apwpSourcePortfolioId

    -- * Destructuring the response
    , AssociateProductWithPortfolioResponse (..)
    , mkAssociateProductWithPortfolioResponse
    -- ** Response lenses
    , apwprrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.ServiceCatalog.Types as Types

-- | /See:/ 'mkAssociateProductWithPortfolio' smart constructor.
data AssociateProductWithPortfolio = AssociateProductWithPortfolio'
  { productId :: Types.Id
    -- ^ The product identifier.
  , portfolioId :: Types.Id
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
  , sourcePortfolioId :: Core.Maybe Types.Id
    -- ^ The identifier of the source portfolio.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AssociateProductWithPortfolio' value with any optional fields omitted.
mkAssociateProductWithPortfolio
    :: Types.Id -- ^ 'productId'
    -> Types.Id -- ^ 'portfolioId'
    -> AssociateProductWithPortfolio
mkAssociateProductWithPortfolio productId portfolioId
  = AssociateProductWithPortfolio'{productId, portfolioId,
                                   acceptLanguage = Core.Nothing, sourcePortfolioId = Core.Nothing}

-- | The product identifier.
--
-- /Note:/ Consider using 'productId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apwpProductId :: Lens.Lens' AssociateProductWithPortfolio Types.Id
apwpProductId = Lens.field @"productId"
{-# INLINEABLE apwpProductId #-}
{-# DEPRECATED productId "Use generic-lens or generic-optics with 'productId' instead"  #-}

-- | The portfolio identifier.
--
-- /Note:/ Consider using 'portfolioId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apwpPortfolioId :: Lens.Lens' AssociateProductWithPortfolio Types.Id
apwpPortfolioId = Lens.field @"portfolioId"
{-# INLINEABLE apwpPortfolioId #-}
{-# DEPRECATED portfolioId "Use generic-lens or generic-optics with 'portfolioId' instead"  #-}

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
apwpAcceptLanguage :: Lens.Lens' AssociateProductWithPortfolio (Core.Maybe Types.AcceptLanguage)
apwpAcceptLanguage = Lens.field @"acceptLanguage"
{-# INLINEABLE apwpAcceptLanguage #-}
{-# DEPRECATED acceptLanguage "Use generic-lens or generic-optics with 'acceptLanguage' instead"  #-}

-- | The identifier of the source portfolio.
--
-- /Note:/ Consider using 'sourcePortfolioId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apwpSourcePortfolioId :: Lens.Lens' AssociateProductWithPortfolio (Core.Maybe Types.Id)
apwpSourcePortfolioId = Lens.field @"sourcePortfolioId"
{-# INLINEABLE apwpSourcePortfolioId #-}
{-# DEPRECATED sourcePortfolioId "Use generic-lens or generic-optics with 'sourcePortfolioId' instead"  #-}

instance Core.ToQuery AssociateProductWithPortfolio where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders AssociateProductWithPortfolio where
        toHeaders AssociateProductWithPortfolio{..}
          = Core.pure
              ("X-Amz-Target",
               "AWS242ServiceCatalogService.AssociateProductWithPortfolio")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON AssociateProductWithPortfolio where
        toJSON AssociateProductWithPortfolio{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("ProductId" Core..= productId),
                  Core.Just ("PortfolioId" Core..= portfolioId),
                  ("AcceptLanguage" Core..=) Core.<$> acceptLanguage,
                  ("SourcePortfolioId" Core..=) Core.<$> sourcePortfolioId])

instance Core.AWSRequest AssociateProductWithPortfolio where
        type Rs AssociateProductWithPortfolio =
             AssociateProductWithPortfolioResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 AssociateProductWithPortfolioResponse' Core.<$>
                   (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkAssociateProductWithPortfolioResponse' smart constructor.
newtype AssociateProductWithPortfolioResponse = AssociateProductWithPortfolioResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'AssociateProductWithPortfolioResponse' value with any optional fields omitted.
mkAssociateProductWithPortfolioResponse
    :: Core.Int -- ^ 'responseStatus'
    -> AssociateProductWithPortfolioResponse
mkAssociateProductWithPortfolioResponse responseStatus
  = AssociateProductWithPortfolioResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apwprrsResponseStatus :: Lens.Lens' AssociateProductWithPortfolioResponse Core.Int
apwprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE apwprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
