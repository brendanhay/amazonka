{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      RejectPortfolioShare (..)
    , mkRejectPortfolioShare
    -- ** Request lenses
    , rpsPortfolioId
    , rpsAcceptLanguage
    , rpsPortfolioShareType

    -- * Destructuring the response
    , RejectPortfolioShareResponse (..)
    , mkRejectPortfolioShareResponse
    -- ** Response lenses
    , rpsrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.ServiceCatalog.Types as Types

-- | /See:/ 'mkRejectPortfolioShare' smart constructor.
data RejectPortfolioShare = RejectPortfolioShare'
  { portfolioId :: Types.Id
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
  , portfolioShareType :: Core.Maybe Types.PortfolioShareType
    -- ^ The type of shared portfolios to reject. The default is to reject imported portfolios.
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
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RejectPortfolioShare' value with any optional fields omitted.
mkRejectPortfolioShare
    :: Types.Id -- ^ 'portfolioId'
    -> RejectPortfolioShare
mkRejectPortfolioShare portfolioId
  = RejectPortfolioShare'{portfolioId, acceptLanguage = Core.Nothing,
                          portfolioShareType = Core.Nothing}

-- | The portfolio identifier.
--
-- /Note:/ Consider using 'portfolioId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpsPortfolioId :: Lens.Lens' RejectPortfolioShare Types.Id
rpsPortfolioId = Lens.field @"portfolioId"
{-# INLINEABLE rpsPortfolioId #-}
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
rpsAcceptLanguage :: Lens.Lens' RejectPortfolioShare (Core.Maybe Types.AcceptLanguage)
rpsAcceptLanguage = Lens.field @"acceptLanguage"
{-# INLINEABLE rpsAcceptLanguage #-}
{-# DEPRECATED acceptLanguage "Use generic-lens or generic-optics with 'acceptLanguage' instead"  #-}

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
{-# INLINEABLE rpsPortfolioShareType #-}
{-# DEPRECATED portfolioShareType "Use generic-lens or generic-optics with 'portfolioShareType' instead"  #-}

instance Core.ToQuery RejectPortfolioShare where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders RejectPortfolioShare where
        toHeaders RejectPortfolioShare{..}
          = Core.pure
              ("X-Amz-Target",
               "AWS242ServiceCatalogService.RejectPortfolioShare")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON RejectPortfolioShare where
        toJSON RejectPortfolioShare{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("PortfolioId" Core..= portfolioId),
                  ("AcceptLanguage" Core..=) Core.<$> acceptLanguage,
                  ("PortfolioShareType" Core..=) Core.<$> portfolioShareType])

instance Core.AWSRequest RejectPortfolioShare where
        type Rs RejectPortfolioShare = RejectPortfolioShareResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 RejectPortfolioShareResponse' Core.<$>
                   (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkRejectPortfolioShareResponse' smart constructor.
newtype RejectPortfolioShareResponse = RejectPortfolioShareResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'RejectPortfolioShareResponse' value with any optional fields omitted.
mkRejectPortfolioShareResponse
    :: Core.Int -- ^ 'responseStatus'
    -> RejectPortfolioShareResponse
mkRejectPortfolioShareResponse responseStatus
  = RejectPortfolioShareResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpsrrsResponseStatus :: Lens.Lens' RejectPortfolioShareResponse Core.Int
rpsrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE rpsrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
