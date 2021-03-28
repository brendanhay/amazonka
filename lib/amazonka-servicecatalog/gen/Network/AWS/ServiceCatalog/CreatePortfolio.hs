{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      CreatePortfolio (..)
    , mkCreatePortfolio
    -- ** Request lenses
    , cpfDisplayName
    , cpfProviderName
    , cpfIdempotencyToken
    , cpfAcceptLanguage
    , cpfDescription
    , cpfTags

    -- * Destructuring the response
    , CreatePortfolioResponse (..)
    , mkCreatePortfolioResponse
    -- ** Response lenses
    , cprfrsPortfolioDetail
    , cprfrsTags
    , cprfrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.ServiceCatalog.Types as Types

-- | /See:/ 'mkCreatePortfolio' smart constructor.
data CreatePortfolio = CreatePortfolio'
  { displayName :: Types.PortfolioDisplayName
    -- ^ The name to use for display purposes.
  , providerName :: Types.ProviderName
    -- ^ The name of the portfolio provider.
  , idempotencyToken :: Types.IdempotencyToken
    -- ^ A unique identifier that you provide to ensure idempotency. If multiple requests differ only by the idempotency token, the same response is returned for each repeated request.
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
  , description :: Core.Maybe Types.PortfolioDescription
    -- ^ The description of the portfolio.
  , tags :: Core.Maybe [Types.Tag]
    -- ^ One or more tags.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreatePortfolio' value with any optional fields omitted.
mkCreatePortfolio
    :: Types.PortfolioDisplayName -- ^ 'displayName'
    -> Types.ProviderName -- ^ 'providerName'
    -> Types.IdempotencyToken -- ^ 'idempotencyToken'
    -> CreatePortfolio
mkCreatePortfolio displayName providerName idempotencyToken
  = CreatePortfolio'{displayName, providerName, idempotencyToken,
                     acceptLanguage = Core.Nothing, description = Core.Nothing,
                     tags = Core.Nothing}

-- | The name to use for display purposes.
--
-- /Note:/ Consider using 'displayName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpfDisplayName :: Lens.Lens' CreatePortfolio Types.PortfolioDisplayName
cpfDisplayName = Lens.field @"displayName"
{-# INLINEABLE cpfDisplayName #-}
{-# DEPRECATED displayName "Use generic-lens or generic-optics with 'displayName' instead"  #-}

-- | The name of the portfolio provider.
--
-- /Note:/ Consider using 'providerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpfProviderName :: Lens.Lens' CreatePortfolio Types.ProviderName
cpfProviderName = Lens.field @"providerName"
{-# INLINEABLE cpfProviderName #-}
{-# DEPRECATED providerName "Use generic-lens or generic-optics with 'providerName' instead"  #-}

-- | A unique identifier that you provide to ensure idempotency. If multiple requests differ only by the idempotency token, the same response is returned for each repeated request.
--
-- /Note:/ Consider using 'idempotencyToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpfIdempotencyToken :: Lens.Lens' CreatePortfolio Types.IdempotencyToken
cpfIdempotencyToken = Lens.field @"idempotencyToken"
{-# INLINEABLE cpfIdempotencyToken #-}
{-# DEPRECATED idempotencyToken "Use generic-lens or generic-optics with 'idempotencyToken' instead"  #-}

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
{-# INLINEABLE cpfAcceptLanguage #-}
{-# DEPRECATED acceptLanguage "Use generic-lens or generic-optics with 'acceptLanguage' instead"  #-}

-- | The description of the portfolio.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpfDescription :: Lens.Lens' CreatePortfolio (Core.Maybe Types.PortfolioDescription)
cpfDescription = Lens.field @"description"
{-# INLINEABLE cpfDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | One or more tags.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpfTags :: Lens.Lens' CreatePortfolio (Core.Maybe [Types.Tag])
cpfTags = Lens.field @"tags"
{-# INLINEABLE cpfTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

instance Core.ToQuery CreatePortfolio where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreatePortfolio where
        toHeaders CreatePortfolio{..}
          = Core.pure
              ("X-Amz-Target", "AWS242ServiceCatalogService.CreatePortfolio")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CreatePortfolio where
        toJSON CreatePortfolio{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("DisplayName" Core..= displayName),
                  Core.Just ("ProviderName" Core..= providerName),
                  Core.Just ("IdempotencyToken" Core..= idempotencyToken),
                  ("AcceptLanguage" Core..=) Core.<$> acceptLanguage,
                  ("Description" Core..=) Core.<$> description,
                  ("Tags" Core..=) Core.<$> tags])

instance Core.AWSRequest CreatePortfolio where
        type Rs CreatePortfolio = CreatePortfolioResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CreatePortfolioResponse' Core.<$>
                   (x Core..:? "PortfolioDetail") Core.<*> x Core..:? "Tags" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreatePortfolioResponse' smart constructor.
data CreatePortfolioResponse = CreatePortfolioResponse'
  { portfolioDetail :: Core.Maybe Types.PortfolioDetail
    -- ^ Information about the portfolio.
  , tags :: Core.Maybe [Types.Tag]
    -- ^ Information about the tags associated with the portfolio.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'CreatePortfolioResponse' value with any optional fields omitted.
mkCreatePortfolioResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreatePortfolioResponse
mkCreatePortfolioResponse responseStatus
  = CreatePortfolioResponse'{portfolioDetail = Core.Nothing,
                             tags = Core.Nothing, responseStatus}

-- | Information about the portfolio.
--
-- /Note:/ Consider using 'portfolioDetail' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cprfrsPortfolioDetail :: Lens.Lens' CreatePortfolioResponse (Core.Maybe Types.PortfolioDetail)
cprfrsPortfolioDetail = Lens.field @"portfolioDetail"
{-# INLINEABLE cprfrsPortfolioDetail #-}
{-# DEPRECATED portfolioDetail "Use generic-lens or generic-optics with 'portfolioDetail' instead"  #-}

-- | Information about the tags associated with the portfolio.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cprfrsTags :: Lens.Lens' CreatePortfolioResponse (Core.Maybe [Types.Tag])
cprfrsTags = Lens.field @"tags"
{-# INLINEABLE cprfrsTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cprfrsResponseStatus :: Lens.Lens' CreatePortfolioResponse Core.Int
cprfrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE cprfrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
