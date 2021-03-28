{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.ListCodeSigningConfigs
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of <https://docs.aws.amazon.com/lambda/latest/dg/configuring-codesigning.html code signing configurations> for the specified function. A request returns up to 10,000 configurations per call. You can use the @MaxItems@ parameter to return fewer configurations per call. 
--
-- This operation returns paginated results.
module Network.AWS.Lambda.ListCodeSigningConfigs
    (
    -- * Creating a request
      ListCodeSigningConfigs (..)
    , mkListCodeSigningConfigs
    -- ** Request lenses
    , lcscMarker
    , lcscMaxItems

    -- * Destructuring the response
    , ListCodeSigningConfigsResponse (..)
    , mkListCodeSigningConfigsResponse
    -- ** Response lenses
    , lcscrrsCodeSigningConfigs
    , lcscrrsNextMarker
    , lcscrrsResponseStatus
    ) where

import qualified Network.AWS.Lambda.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListCodeSigningConfigs' smart constructor.
data ListCodeSigningConfigs = ListCodeSigningConfigs'
  { marker :: Core.Maybe Core.Text
    -- ^ Specify the pagination token that's returned by a previous request to retrieve the next page of results.
  , maxItems :: Core.Maybe Core.Natural
    -- ^ Maximum number of items to return.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListCodeSigningConfigs' value with any optional fields omitted.
mkListCodeSigningConfigs
    :: ListCodeSigningConfigs
mkListCodeSigningConfigs
  = ListCodeSigningConfigs'{marker = Core.Nothing,
                            maxItems = Core.Nothing}

-- | Specify the pagination token that's returned by a previous request to retrieve the next page of results.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcscMarker :: Lens.Lens' ListCodeSigningConfigs (Core.Maybe Core.Text)
lcscMarker = Lens.field @"marker"
{-# INLINEABLE lcscMarker #-}
{-# DEPRECATED marker "Use generic-lens or generic-optics with 'marker' instead"  #-}

-- | Maximum number of items to return.
--
-- /Note:/ Consider using 'maxItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcscMaxItems :: Lens.Lens' ListCodeSigningConfigs (Core.Maybe Core.Natural)
lcscMaxItems = Lens.field @"maxItems"
{-# INLINEABLE lcscMaxItems #-}
{-# DEPRECATED maxItems "Use generic-lens or generic-optics with 'maxItems' instead"  #-}

instance Core.ToQuery ListCodeSigningConfigs where
        toQuery ListCodeSigningConfigs{..}
          = Core.maybe Core.mempty (Core.toQueryPair "Marker") marker Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "MaxItems") maxItems

instance Core.ToHeaders ListCodeSigningConfigs where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest ListCodeSigningConfigs where
        type Rs ListCodeSigningConfigs = ListCodeSigningConfigsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath = "/2020-04-22/code-signing-configs/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListCodeSigningConfigsResponse' Core.<$>
                   (x Core..:? "CodeSigningConfigs") Core.<*> x Core..:? "NextMarker"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListCodeSigningConfigs where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextMarker") = Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"codeSigningConfigs" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"marker" Lens..~ rs Lens.^. Lens.field @"nextMarker")

-- | /See:/ 'mkListCodeSigningConfigsResponse' smart constructor.
data ListCodeSigningConfigsResponse = ListCodeSigningConfigsResponse'
  { codeSigningConfigs :: Core.Maybe [Types.CodeSigningConfig]
    -- ^ The code signing configurations
  , nextMarker :: Core.Maybe Core.Text
    -- ^ The pagination token that's included if more results are available.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListCodeSigningConfigsResponse' value with any optional fields omitted.
mkListCodeSigningConfigsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListCodeSigningConfigsResponse
mkListCodeSigningConfigsResponse responseStatus
  = ListCodeSigningConfigsResponse'{codeSigningConfigs =
                                      Core.Nothing,
                                    nextMarker = Core.Nothing, responseStatus}

-- | The code signing configurations
--
-- /Note:/ Consider using 'codeSigningConfigs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcscrrsCodeSigningConfigs :: Lens.Lens' ListCodeSigningConfigsResponse (Core.Maybe [Types.CodeSigningConfig])
lcscrrsCodeSigningConfigs = Lens.field @"codeSigningConfigs"
{-# INLINEABLE lcscrrsCodeSigningConfigs #-}
{-# DEPRECATED codeSigningConfigs "Use generic-lens or generic-optics with 'codeSigningConfigs' instead"  #-}

-- | The pagination token that's included if more results are available.
--
-- /Note:/ Consider using 'nextMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcscrrsNextMarker :: Lens.Lens' ListCodeSigningConfigsResponse (Core.Maybe Core.Text)
lcscrrsNextMarker = Lens.field @"nextMarker"
{-# INLINEABLE lcscrrsNextMarker #-}
{-# DEPRECATED nextMarker "Use generic-lens or generic-optics with 'nextMarker' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcscrrsResponseStatus :: Lens.Lens' ListCodeSigningConfigsResponse Core.Int
lcscrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lcscrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
