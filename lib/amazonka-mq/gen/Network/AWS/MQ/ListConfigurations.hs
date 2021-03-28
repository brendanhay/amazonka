{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MQ.ListConfigurations
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of all configurations.
module Network.AWS.MQ.ListConfigurations
    (
    -- * Creating a request
      ListConfigurations (..)
    , mkListConfigurations
    -- ** Request lenses
    , lcMaxResults
    , lcNextToken

    -- * Destructuring the response
    , ListConfigurationsResponse (..)
    , mkListConfigurationsResponse
    -- ** Response lenses
    , lcrrsConfigurations
    , lcrrsMaxResults
    , lcrrsNextToken
    , lcrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MQ.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListConfigurations' smart constructor.
data ListConfigurations = ListConfigurations'
  { maxResults :: Core.Maybe Core.Natural
    -- ^ The maximum number of configurations that Amazon MQ can return per page (20 by default). This value must be an integer from 5 to 100.
  , nextToken :: Core.Maybe Core.Text
    -- ^ The token that specifies the next page of results Amazon MQ should return. To request the first page, leave nextToken empty.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListConfigurations' value with any optional fields omitted.
mkListConfigurations
    :: ListConfigurations
mkListConfigurations
  = ListConfigurations'{maxResults = Core.Nothing,
                        nextToken = Core.Nothing}

-- | The maximum number of configurations that Amazon MQ can return per page (20 by default). This value must be an integer from 5 to 100.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcMaxResults :: Lens.Lens' ListConfigurations (Core.Maybe Core.Natural)
lcMaxResults = Lens.field @"maxResults"
{-# INLINEABLE lcMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | The token that specifies the next page of results Amazon MQ should return. To request the first page, leave nextToken empty.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcNextToken :: Lens.Lens' ListConfigurations (Core.Maybe Core.Text)
lcNextToken = Lens.field @"nextToken"
{-# INLINEABLE lcNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery ListConfigurations where
        toQuery ListConfigurations{..}
          = Core.maybe Core.mempty (Core.toQueryPair "maxResults") maxResults
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "nextToken") nextToken

instance Core.ToHeaders ListConfigurations where
        toHeaders ListConfigurations{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.AWSRequest ListConfigurations where
        type Rs ListConfigurations = ListConfigurationsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET, Core._rqPath = "/v1/configurations",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListConfigurationsResponse' Core.<$>
                   (x Core..:? "configurations") Core.<*> x Core..:? "maxResults"
                     Core.<*> x Core..:? "nextToken"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkListConfigurationsResponse' smart constructor.
data ListConfigurationsResponse = ListConfigurationsResponse'
  { configurations :: Core.Maybe [Types.Configuration]
    -- ^ The list of all revisions for the specified configuration.
  , maxResults :: Core.Maybe Core.Int
    -- ^ The maximum number of configurations that Amazon MQ can return per page (20 by default). This value must be an integer from 5 to 100.
  , nextToken :: Core.Maybe Core.Text
    -- ^ The token that specifies the next page of results Amazon MQ should return. To request the first page, leave nextToken empty.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ListConfigurationsResponse' value with any optional fields omitted.
mkListConfigurationsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListConfigurationsResponse
mkListConfigurationsResponse responseStatus
  = ListConfigurationsResponse'{configurations = Core.Nothing,
                                maxResults = Core.Nothing, nextToken = Core.Nothing,
                                responseStatus}

-- | The list of all revisions for the specified configuration.
--
-- /Note:/ Consider using 'configurations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcrrsConfigurations :: Lens.Lens' ListConfigurationsResponse (Core.Maybe [Types.Configuration])
lcrrsConfigurations = Lens.field @"configurations"
{-# INLINEABLE lcrrsConfigurations #-}
{-# DEPRECATED configurations "Use generic-lens or generic-optics with 'configurations' instead"  #-}

-- | The maximum number of configurations that Amazon MQ can return per page (20 by default). This value must be an integer from 5 to 100.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcrrsMaxResults :: Lens.Lens' ListConfigurationsResponse (Core.Maybe Core.Int)
lcrrsMaxResults = Lens.field @"maxResults"
{-# INLINEABLE lcrrsMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | The token that specifies the next page of results Amazon MQ should return. To request the first page, leave nextToken empty.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcrrsNextToken :: Lens.Lens' ListConfigurationsResponse (Core.Maybe Core.Text)
lcrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE lcrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcrrsResponseStatus :: Lens.Lens' ListConfigurationsResponse Core.Int
lcrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lcrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
