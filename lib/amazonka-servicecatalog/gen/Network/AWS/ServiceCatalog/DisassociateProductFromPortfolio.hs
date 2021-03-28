{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.DisassociateProductFromPortfolio
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates the specified product from the specified portfolio. 
--
-- A delegated admin is authorized to invoke this command.
module Network.AWS.ServiceCatalog.DisassociateProductFromPortfolio
    (
    -- * Creating a request
      DisassociateProductFromPortfolio (..)
    , mkDisassociateProductFromPortfolio
    -- ** Request lenses
    , dpfpProductId
    , dpfpPortfolioId
    , dpfpAcceptLanguage

    -- * Destructuring the response
    , DisassociateProductFromPortfolioResponse (..)
    , mkDisassociateProductFromPortfolioResponse
    -- ** Response lenses
    , dpfprrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.ServiceCatalog.Types as Types

-- | /See:/ 'mkDisassociateProductFromPortfolio' smart constructor.
data DisassociateProductFromPortfolio = DisassociateProductFromPortfolio'
  { productId :: Types.ProductId
    -- ^ The product identifier.
  , portfolioId :: Types.PortfolioId
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

-- | Creates a 'DisassociateProductFromPortfolio' value with any optional fields omitted.
mkDisassociateProductFromPortfolio
    :: Types.ProductId -- ^ 'productId'
    -> Types.PortfolioId -- ^ 'portfolioId'
    -> DisassociateProductFromPortfolio
mkDisassociateProductFromPortfolio productId portfolioId
  = DisassociateProductFromPortfolio'{productId, portfolioId,
                                      acceptLanguage = Core.Nothing}

-- | The product identifier.
--
-- /Note:/ Consider using 'productId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpfpProductId :: Lens.Lens' DisassociateProductFromPortfolio Types.ProductId
dpfpProductId = Lens.field @"productId"
{-# INLINEABLE dpfpProductId #-}
{-# DEPRECATED productId "Use generic-lens or generic-optics with 'productId' instead"  #-}

-- | The portfolio identifier.
--
-- /Note:/ Consider using 'portfolioId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpfpPortfolioId :: Lens.Lens' DisassociateProductFromPortfolio Types.PortfolioId
dpfpPortfolioId = Lens.field @"portfolioId"
{-# INLINEABLE dpfpPortfolioId #-}
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
dpfpAcceptLanguage :: Lens.Lens' DisassociateProductFromPortfolio (Core.Maybe Types.AcceptLanguage)
dpfpAcceptLanguage = Lens.field @"acceptLanguage"
{-# INLINEABLE dpfpAcceptLanguage #-}
{-# DEPRECATED acceptLanguage "Use generic-lens or generic-optics with 'acceptLanguage' instead"  #-}

instance Core.ToQuery DisassociateProductFromPortfolio where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DisassociateProductFromPortfolio where
        toHeaders DisassociateProductFromPortfolio{..}
          = Core.pure
              ("X-Amz-Target",
               "AWS242ServiceCatalogService.DisassociateProductFromPortfolio")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DisassociateProductFromPortfolio where
        toJSON DisassociateProductFromPortfolio{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("ProductId" Core..= productId),
                  Core.Just ("PortfolioId" Core..= portfolioId),
                  ("AcceptLanguage" Core..=) Core.<$> acceptLanguage])

instance Core.AWSRequest DisassociateProductFromPortfolio where
        type Rs DisassociateProductFromPortfolio =
             DisassociateProductFromPortfolioResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 DisassociateProductFromPortfolioResponse' Core.<$>
                   (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDisassociateProductFromPortfolioResponse' smart constructor.
newtype DisassociateProductFromPortfolioResponse = DisassociateProductFromPortfolioResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DisassociateProductFromPortfolioResponse' value with any optional fields omitted.
mkDisassociateProductFromPortfolioResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DisassociateProductFromPortfolioResponse
mkDisassociateProductFromPortfolioResponse responseStatus
  = DisassociateProductFromPortfolioResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpfprrsResponseStatus :: Lens.Lens' DisassociateProductFromPortfolioResponse Core.Int
dpfprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dpfprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
