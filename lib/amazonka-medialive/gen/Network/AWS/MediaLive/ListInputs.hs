{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.ListInputs
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Produces list of inputs that have been created
--
-- This operation returns paginated results.
module Network.AWS.MediaLive.ListInputs
    (
    -- * Creating a request
      ListInputs (..)
    , mkListInputs
    -- ** Request lenses
    , liMaxResults
    , liNextToken

    -- * Destructuring the response
    , ListInputsResponse (..)
    , mkListInputsResponse
    -- ** Response lenses
    , lirrsInputs
    , lirrsNextToken
    , lirrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaLive.Types as Types
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Placeholder documentation for ListInputsRequest
--
-- /See:/ 'mkListInputs' smart constructor.
data ListInputs = ListInputs'
  { maxResults :: Core.Maybe Core.Natural
  , nextToken :: Core.Maybe Core.Text
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListInputs' value with any optional fields omitted.
mkListInputs
    :: ListInputs
mkListInputs
  = ListInputs'{maxResults = Core.Nothing, nextToken = Core.Nothing}

-- | Undocumented field.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
liMaxResults :: Lens.Lens' ListInputs (Core.Maybe Core.Natural)
liMaxResults = Lens.field @"maxResults"
{-# INLINEABLE liMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
liNextToken :: Lens.Lens' ListInputs (Core.Maybe Core.Text)
liNextToken = Lens.field @"nextToken"
{-# INLINEABLE liNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery ListInputs where
        toQuery ListInputs{..}
          = Core.maybe Core.mempty (Core.toQueryPair "maxResults") maxResults
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "nextToken") nextToken

instance Core.ToHeaders ListInputs where
        toHeaders ListInputs{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.AWSRequest ListInputs where
        type Rs ListInputs = ListInputsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET, Core._rqPath = "/prod/inputs",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListInputsResponse' Core.<$>
                   (x Core..:? "inputs") Core.<*> x Core..:? "nextToken" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListInputs where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop (rs Lens.^? Lens.field @"inputs" Core.. Lens._Just) =
            Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | Placeholder documentation for ListInputsResponse
--
-- /See:/ 'mkListInputsResponse' smart constructor.
data ListInputsResponse = ListInputsResponse'
  { inputs :: Core.Maybe [Types.Input]
  , nextToken :: Core.Maybe Core.Text
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListInputsResponse' value with any optional fields omitted.
mkListInputsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListInputsResponse
mkListInputsResponse responseStatus
  = ListInputsResponse'{inputs = Core.Nothing,
                        nextToken = Core.Nothing, responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'inputs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lirrsInputs :: Lens.Lens' ListInputsResponse (Core.Maybe [Types.Input])
lirrsInputs = Lens.field @"inputs"
{-# INLINEABLE lirrsInputs #-}
{-# DEPRECATED inputs "Use generic-lens or generic-optics with 'inputs' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lirrsNextToken :: Lens.Lens' ListInputsResponse (Core.Maybe Core.Text)
lirrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE lirrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lirrsResponseStatus :: Lens.Lens' ListInputsResponse Core.Int
lirrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lirrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
