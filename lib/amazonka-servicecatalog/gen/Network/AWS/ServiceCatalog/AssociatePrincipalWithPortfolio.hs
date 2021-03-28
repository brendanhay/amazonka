{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.AssociatePrincipalWithPortfolio
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates the specified principal ARN with the specified portfolio.
module Network.AWS.ServiceCatalog.AssociatePrincipalWithPortfolio
    (
    -- * Creating a request
      AssociatePrincipalWithPortfolio (..)
    , mkAssociatePrincipalWithPortfolio
    -- ** Request lenses
    , aPortfolioId
    , aPrincipalARN
    , aPrincipalType
    , aAcceptLanguage

    -- * Destructuring the response
    , AssociatePrincipalWithPortfolioResponse (..)
    , mkAssociatePrincipalWithPortfolioResponse
    -- ** Response lenses
    , arsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.ServiceCatalog.Types as Types

-- | /See:/ 'mkAssociatePrincipalWithPortfolio' smart constructor.
data AssociatePrincipalWithPortfolio = AssociatePrincipalWithPortfolio'
  { portfolioId :: Types.Id
    -- ^ The portfolio identifier.
  , principalARN :: Types.PrincipalARN
    -- ^ The ARN of the principal (IAM user, role, or group).
  , principalType :: Types.PrincipalType
    -- ^ The principal type. The supported value is @IAM@ .
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

-- | Creates a 'AssociatePrincipalWithPortfolio' value with any optional fields omitted.
mkAssociatePrincipalWithPortfolio
    :: Types.Id -- ^ 'portfolioId'
    -> Types.PrincipalARN -- ^ 'principalARN'
    -> Types.PrincipalType -- ^ 'principalType'
    -> AssociatePrincipalWithPortfolio
mkAssociatePrincipalWithPortfolio portfolioId principalARN
  principalType
  = AssociatePrincipalWithPortfolio'{portfolioId, principalARN,
                                     principalType, acceptLanguage = Core.Nothing}

-- | The portfolio identifier.
--
-- /Note:/ Consider using 'portfolioId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aPortfolioId :: Lens.Lens' AssociatePrincipalWithPortfolio Types.Id
aPortfolioId = Lens.field @"portfolioId"
{-# INLINEABLE aPortfolioId #-}
{-# DEPRECATED portfolioId "Use generic-lens or generic-optics with 'portfolioId' instead"  #-}

-- | The ARN of the principal (IAM user, role, or group).
--
-- /Note:/ Consider using 'principalARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aPrincipalARN :: Lens.Lens' AssociatePrincipalWithPortfolio Types.PrincipalARN
aPrincipalARN = Lens.field @"principalARN"
{-# INLINEABLE aPrincipalARN #-}
{-# DEPRECATED principalARN "Use generic-lens or generic-optics with 'principalARN' instead"  #-}

-- | The principal type. The supported value is @IAM@ .
--
-- /Note:/ Consider using 'principalType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aPrincipalType :: Lens.Lens' AssociatePrincipalWithPortfolio Types.PrincipalType
aPrincipalType = Lens.field @"principalType"
{-# INLINEABLE aPrincipalType #-}
{-# DEPRECATED principalType "Use generic-lens or generic-optics with 'principalType' instead"  #-}

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
aAcceptLanguage :: Lens.Lens' AssociatePrincipalWithPortfolio (Core.Maybe Types.AcceptLanguage)
aAcceptLanguage = Lens.field @"acceptLanguage"
{-# INLINEABLE aAcceptLanguage #-}
{-# DEPRECATED acceptLanguage "Use generic-lens or generic-optics with 'acceptLanguage' instead"  #-}

instance Core.ToQuery AssociatePrincipalWithPortfolio where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders AssociatePrincipalWithPortfolio where
        toHeaders AssociatePrincipalWithPortfolio{..}
          = Core.pure
              ("X-Amz-Target",
               "AWS242ServiceCatalogService.AssociatePrincipalWithPortfolio")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON AssociatePrincipalWithPortfolio where
        toJSON AssociatePrincipalWithPortfolio{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("PortfolioId" Core..= portfolioId),
                  Core.Just ("PrincipalARN" Core..= principalARN),
                  Core.Just ("PrincipalType" Core..= principalType),
                  ("AcceptLanguage" Core..=) Core.<$> acceptLanguage])

instance Core.AWSRequest AssociatePrincipalWithPortfolio where
        type Rs AssociatePrincipalWithPortfolio =
             AssociatePrincipalWithPortfolioResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 AssociatePrincipalWithPortfolioResponse' Core.<$>
                   (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkAssociatePrincipalWithPortfolioResponse' smart constructor.
newtype AssociatePrincipalWithPortfolioResponse = AssociatePrincipalWithPortfolioResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'AssociatePrincipalWithPortfolioResponse' value with any optional fields omitted.
mkAssociatePrincipalWithPortfolioResponse
    :: Core.Int -- ^ 'responseStatus'
    -> AssociatePrincipalWithPortfolioResponse
mkAssociatePrincipalWithPortfolioResponse responseStatus
  = AssociatePrincipalWithPortfolioResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arsResponseStatus :: Lens.Lens' AssociatePrincipalWithPortfolioResponse Core.Int
arsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE arsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
