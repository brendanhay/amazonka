{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.CreatePortfolio
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a portfolio.
--
-- A delegated admin is authorized to invoke this command.
module Network.AWS.ServiceCatalog.CreatePortfolio
  ( -- * Creating a request
    CreatePortfolio (..),
    mkCreatePortfolio,

    -- ** Request lenses
    cpfDisplayName,
    cpfProviderName,
    cpfIdempotencyToken,
    cpfAcceptLanguage,
    cpfDescription,
    cpfTags,

    -- * Destructuring the response
    CreatePortfolioResponse (..),
    mkCreatePortfolioResponse,

    -- ** Response lenses
    cprfrsPortfolioDetail,
    cprfrsTags,
    cprfrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.ServiceCatalog.Types as Types

-- | /See:/ 'mkCreatePortfolio' smart constructor.
data CreatePortfolio = CreatePortfolio'
  { -- | The name to use for display purposes.
    displayName :: Types.PortfolioDisplayName,
    -- | The name of the portfolio provider.
    providerName :: Types.ProviderName,
    -- | A unique identifier that you provide to ensure idempotency. If multiple requests differ only by the idempotency token, the same response is returned for each repeated request.
    idempotencyToken :: Types.IdempotencyToken,
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
    -- | The description of the portfolio.
    description :: Core.Maybe Types.PortfolioDescription,
    -- | One or more tags.
    tags :: Core.Maybe [Types.Tag]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreatePortfolio' value with any optional fields omitted.
mkCreatePortfolio ::
  -- | 'displayName'
  Types.PortfolioDisplayName ->
  -- | 'providerName'
  Types.ProviderName ->
  -- | 'idempotencyToken'
  Types.IdempotencyToken ->
  CreatePortfolio
mkCreatePortfolio displayName providerName idempotencyToken =
  CreatePortfolio'
    { displayName,
      providerName,
      idempotencyToken,
      acceptLanguage = Core.Nothing,
      description = Core.Nothing,
      tags = Core.Nothing
    }

-- | The name to use for display purposes.
--
-- /Note:/ Consider using 'displayName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpfDisplayName :: Lens.Lens' CreatePortfolio Types.PortfolioDisplayName
cpfDisplayName = Lens.field @"displayName"
{-# DEPRECATED cpfDisplayName "Use generic-lens or generic-optics with 'displayName' instead." #-}

-- | The name of the portfolio provider.
--
-- /Note:/ Consider using 'providerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpfProviderName :: Lens.Lens' CreatePortfolio Types.ProviderName
cpfProviderName = Lens.field @"providerName"
{-# DEPRECATED cpfProviderName "Use generic-lens or generic-optics with 'providerName' instead." #-}

-- | A unique identifier that you provide to ensure idempotency. If multiple requests differ only by the idempotency token, the same response is returned for each repeated request.
--
-- /Note:/ Consider using 'idempotencyToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpfIdempotencyToken :: Lens.Lens' CreatePortfolio Types.IdempotencyToken
cpfIdempotencyToken = Lens.field @"idempotencyToken"
{-# DEPRECATED cpfIdempotencyToken "Use generic-lens or generic-optics with 'idempotencyToken' instead." #-}

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
cpfAcceptLanguage :: Lens.Lens' CreatePortfolio (Core.Maybe Types.AcceptLanguage)
cpfAcceptLanguage = Lens.field @"acceptLanguage"
{-# DEPRECATED cpfAcceptLanguage "Use generic-lens or generic-optics with 'acceptLanguage' instead." #-}

-- | The description of the portfolio.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpfDescription :: Lens.Lens' CreatePortfolio (Core.Maybe Types.PortfolioDescription)
cpfDescription = Lens.field @"description"
{-# DEPRECATED cpfDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | One or more tags.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpfTags :: Lens.Lens' CreatePortfolio (Core.Maybe [Types.Tag])
cpfTags = Lens.field @"tags"
{-# DEPRECATED cpfTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Core.FromJSON CreatePortfolio where
  toJSON CreatePortfolio {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("DisplayName" Core..= displayName),
            Core.Just ("ProviderName" Core..= providerName),
            Core.Just ("IdempotencyToken" Core..= idempotencyToken),
            ("AcceptLanguage" Core..=) Core.<$> acceptLanguage,
            ("Description" Core..=) Core.<$> description,
            ("Tags" Core..=) Core.<$> tags
          ]
      )

instance Core.AWSRequest CreatePortfolio where
  type Rs CreatePortfolio = CreatePortfolioResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "AWS242ServiceCatalogService.CreatePortfolio")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CreatePortfolioResponse'
            Core.<$> (x Core..:? "PortfolioDetail")
            Core.<*> (x Core..:? "Tags")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreatePortfolioResponse' smart constructor.
data CreatePortfolioResponse = CreatePortfolioResponse'
  { -- | Information about the portfolio.
    portfolioDetail :: Core.Maybe Types.PortfolioDetail,
    -- | Information about the tags associated with the portfolio.
    tags :: Core.Maybe [Types.Tag],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'CreatePortfolioResponse' value with any optional fields omitted.
mkCreatePortfolioResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreatePortfolioResponse
mkCreatePortfolioResponse responseStatus =
  CreatePortfolioResponse'
    { portfolioDetail = Core.Nothing,
      tags = Core.Nothing,
      responseStatus
    }

-- | Information about the portfolio.
--
-- /Note:/ Consider using 'portfolioDetail' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cprfrsPortfolioDetail :: Lens.Lens' CreatePortfolioResponse (Core.Maybe Types.PortfolioDetail)
cprfrsPortfolioDetail = Lens.field @"portfolioDetail"
{-# DEPRECATED cprfrsPortfolioDetail "Use generic-lens or generic-optics with 'portfolioDetail' instead." #-}

-- | Information about the tags associated with the portfolio.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cprfrsTags :: Lens.Lens' CreatePortfolioResponse (Core.Maybe [Types.Tag])
cprfrsTags = Lens.field @"tags"
{-# DEPRECATED cprfrsTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cprfrsResponseStatus :: Lens.Lens' CreatePortfolioResponse Core.Int
cprfrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED cprfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
