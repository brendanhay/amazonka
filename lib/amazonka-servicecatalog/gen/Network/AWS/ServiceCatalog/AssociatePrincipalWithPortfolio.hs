{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    AssociatePrincipalWithPortfolio (..),
    mkAssociatePrincipalWithPortfolio,

    -- ** Request lenses
    aPortfolioId,
    aPrincipalARN,
    aPrincipalType,
    aAcceptLanguage,

    -- * Destructuring the response
    AssociatePrincipalWithPortfolioResponse (..),
    mkAssociatePrincipalWithPortfolioResponse,

    -- ** Response lenses
    arsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.ServiceCatalog.Types as Types

-- | /See:/ 'mkAssociatePrincipalWithPortfolio' smart constructor.
data AssociatePrincipalWithPortfolio = AssociatePrincipalWithPortfolio'
  { -- | The portfolio identifier.
    portfolioId :: Types.Id,
    -- | The ARN of the principal (IAM user, role, or group).
    principalARN :: Types.PrincipalARN,
    -- | The principal type. The supported value is @IAM@ .
    principalType :: Types.PrincipalType,
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

-- | Creates a 'AssociatePrincipalWithPortfolio' value with any optional fields omitted.
mkAssociatePrincipalWithPortfolio ::
  -- | 'portfolioId'
  Types.Id ->
  -- | 'principalARN'
  Types.PrincipalARN ->
  -- | 'principalType'
  Types.PrincipalType ->
  AssociatePrincipalWithPortfolio
mkAssociatePrincipalWithPortfolio
  portfolioId
  principalARN
  principalType =
    AssociatePrincipalWithPortfolio'
      { portfolioId,
        principalARN,
        principalType,
        acceptLanguage = Core.Nothing
      }

-- | The portfolio identifier.
--
-- /Note:/ Consider using 'portfolioId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aPortfolioId :: Lens.Lens' AssociatePrincipalWithPortfolio Types.Id
aPortfolioId = Lens.field @"portfolioId"
{-# DEPRECATED aPortfolioId "Use generic-lens or generic-optics with 'portfolioId' instead." #-}

-- | The ARN of the principal (IAM user, role, or group).
--
-- /Note:/ Consider using 'principalARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aPrincipalARN :: Lens.Lens' AssociatePrincipalWithPortfolio Types.PrincipalARN
aPrincipalARN = Lens.field @"principalARN"
{-# DEPRECATED aPrincipalARN "Use generic-lens or generic-optics with 'principalARN' instead." #-}

-- | The principal type. The supported value is @IAM@ .
--
-- /Note:/ Consider using 'principalType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aPrincipalType :: Lens.Lens' AssociatePrincipalWithPortfolio Types.PrincipalType
aPrincipalType = Lens.field @"principalType"
{-# DEPRECATED aPrincipalType "Use generic-lens or generic-optics with 'principalType' instead." #-}

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
{-# DEPRECATED aAcceptLanguage "Use generic-lens or generic-optics with 'acceptLanguage' instead." #-}

instance Core.FromJSON AssociatePrincipalWithPortfolio where
  toJSON AssociatePrincipalWithPortfolio {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("PortfolioId" Core..= portfolioId),
            Core.Just ("PrincipalARN" Core..= principalARN),
            Core.Just ("PrincipalType" Core..= principalType),
            ("AcceptLanguage" Core..=) Core.<$> acceptLanguage
          ]
      )

instance Core.AWSRequest AssociatePrincipalWithPortfolio where
  type
    Rs AssociatePrincipalWithPortfolio =
      AssociatePrincipalWithPortfolioResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "AWS242ServiceCatalogService.AssociatePrincipalWithPortfolio"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          AssociatePrincipalWithPortfolioResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkAssociatePrincipalWithPortfolioResponse' smart constructor.
newtype AssociatePrincipalWithPortfolioResponse = AssociatePrincipalWithPortfolioResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'AssociatePrincipalWithPortfolioResponse' value with any optional fields omitted.
mkAssociatePrincipalWithPortfolioResponse ::
  -- | 'responseStatus'
  Core.Int ->
  AssociatePrincipalWithPortfolioResponse
mkAssociatePrincipalWithPortfolioResponse responseStatus =
  AssociatePrincipalWithPortfolioResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arsResponseStatus :: Lens.Lens' AssociatePrincipalWithPortfolioResponse Core.Int
arsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED arsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
