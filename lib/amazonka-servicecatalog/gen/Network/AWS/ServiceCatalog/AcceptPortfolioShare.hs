{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.AcceptPortfolioShare
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Accepts an offer to share the specified portfolio.
module Network.AWS.ServiceCatalog.AcceptPortfolioShare
    (
    -- * Creating a request
      AcceptPortfolioShare (..)
    , mkAcceptPortfolioShare
    -- ** Request lenses
    , apsPortfolioId
    , apsAcceptLanguage
    , apsPortfolioShareType

    -- * Destructuring the response
    , AcceptPortfolioShareResponse (..)
    , mkAcceptPortfolioShareResponse
    -- ** Response lenses
    , apsrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.ServiceCatalog.Types as Types

-- | /See:/ 'mkAcceptPortfolioShare' smart constructor.
data AcceptPortfolioShare = AcceptPortfolioShare'
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
    -- ^ The type of shared portfolios to accept. The default is to accept imported portfolios.
--
--
--     * @AWS_ORGANIZATIONS@ - Accept portfolios shared by the management account of your organization.
--
--
--     * @IMPORTED@ - Accept imported portfolios.
--
--
--     * @AWS_SERVICECATALOG@ - Not supported. (Throws ResourceNotFoundException.)
--
--
-- For example, @aws servicecatalog accept-portfolio-share --portfolio-id "port-2qwzkwxt3y5fk" --portfolio-share-type AWS_ORGANIZATIONS@ 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AcceptPortfolioShare' value with any optional fields omitted.
mkAcceptPortfolioShare
    :: Types.Id -- ^ 'portfolioId'
    -> AcceptPortfolioShare
mkAcceptPortfolioShare portfolioId
  = AcceptPortfolioShare'{portfolioId, acceptLanguage = Core.Nothing,
                          portfolioShareType = Core.Nothing}

-- | The portfolio identifier.
--
-- /Note:/ Consider using 'portfolioId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apsPortfolioId :: Lens.Lens' AcceptPortfolioShare Types.Id
apsPortfolioId = Lens.field @"portfolioId"
{-# INLINEABLE apsPortfolioId #-}
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
apsAcceptLanguage :: Lens.Lens' AcceptPortfolioShare (Core.Maybe Types.AcceptLanguage)
apsAcceptLanguage = Lens.field @"acceptLanguage"
{-# INLINEABLE apsAcceptLanguage #-}
{-# DEPRECATED acceptLanguage "Use generic-lens or generic-optics with 'acceptLanguage' instead"  #-}

-- | The type of shared portfolios to accept. The default is to accept imported portfolios.
--
--
--     * @AWS_ORGANIZATIONS@ - Accept portfolios shared by the management account of your organization.
--
--
--     * @IMPORTED@ - Accept imported portfolios.
--
--
--     * @AWS_SERVICECATALOG@ - Not supported. (Throws ResourceNotFoundException.)
--
--
-- For example, @aws servicecatalog accept-portfolio-share --portfolio-id "port-2qwzkwxt3y5fk" --portfolio-share-type AWS_ORGANIZATIONS@ 
--
-- /Note:/ Consider using 'portfolioShareType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apsPortfolioShareType :: Lens.Lens' AcceptPortfolioShare (Core.Maybe Types.PortfolioShareType)
apsPortfolioShareType = Lens.field @"portfolioShareType"
{-# INLINEABLE apsPortfolioShareType #-}
{-# DEPRECATED portfolioShareType "Use generic-lens or generic-optics with 'portfolioShareType' instead"  #-}

instance Core.ToQuery AcceptPortfolioShare where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders AcceptPortfolioShare where
        toHeaders AcceptPortfolioShare{..}
          = Core.pure
              ("X-Amz-Target",
               "AWS242ServiceCatalogService.AcceptPortfolioShare")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON AcceptPortfolioShare where
        toJSON AcceptPortfolioShare{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("PortfolioId" Core..= portfolioId),
                  ("AcceptLanguage" Core..=) Core.<$> acceptLanguage,
                  ("PortfolioShareType" Core..=) Core.<$> portfolioShareType])

instance Core.AWSRequest AcceptPortfolioShare where
        type Rs AcceptPortfolioShare = AcceptPortfolioShareResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 AcceptPortfolioShareResponse' Core.<$>
                   (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkAcceptPortfolioShareResponse' smart constructor.
newtype AcceptPortfolioShareResponse = AcceptPortfolioShareResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'AcceptPortfolioShareResponse' value with any optional fields omitted.
mkAcceptPortfolioShareResponse
    :: Core.Int -- ^ 'responseStatus'
    -> AcceptPortfolioShareResponse
mkAcceptPortfolioShareResponse responseStatus
  = AcceptPortfolioShareResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apsrrsResponseStatus :: Lens.Lens' AcceptPortfolioShareResponse Core.Int
apsrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE apsrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
