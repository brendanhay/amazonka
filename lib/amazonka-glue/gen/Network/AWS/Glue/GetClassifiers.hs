{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.GetClassifiers
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all classifier objects in the Data Catalog.
--
-- This operation returns paginated results.
module Network.AWS.Glue.GetClassifiers
    (
    -- * Creating a request
      GetClassifiers (..)
    , mkGetClassifiers
    -- ** Request lenses
    , gcMaxResults
    , gcNextToken

    -- * Destructuring the response
    , GetClassifiersResponse (..)
    , mkGetClassifiersResponse
    -- ** Response lenses
    , gcrgrsClassifiers
    , gcrgrsNextToken
    , gcrgrsResponseStatus
    ) where

import qualified Network.AWS.Glue.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetClassifiers' smart constructor.
data GetClassifiers = GetClassifiers'
  { maxResults :: Core.Maybe Core.Natural
    -- ^ The size of the list to return (optional).
  , nextToken :: Core.Maybe Types.Token
    -- ^ An optional continuation token.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetClassifiers' value with any optional fields omitted.
mkGetClassifiers
    :: GetClassifiers
mkGetClassifiers
  = GetClassifiers'{maxResults = Core.Nothing,
                    nextToken = Core.Nothing}

-- | The size of the list to return (optional).
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcMaxResults :: Lens.Lens' GetClassifiers (Core.Maybe Core.Natural)
gcMaxResults = Lens.field @"maxResults"
{-# INLINEABLE gcMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | An optional continuation token.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcNextToken :: Lens.Lens' GetClassifiers (Core.Maybe Types.Token)
gcNextToken = Lens.field @"nextToken"
{-# INLINEABLE gcNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery GetClassifiers where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetClassifiers where
        toHeaders GetClassifiers{..}
          = Core.pure ("X-Amz-Target", "AWSGlue.GetClassifiers") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GetClassifiers where
        toJSON GetClassifiers{..}
          = Core.object
              (Core.catMaybes
                 [("MaxResults" Core..=) Core.<$> maxResults,
                  ("NextToken" Core..=) Core.<$> nextToken])

instance Core.AWSRequest GetClassifiers where
        type Rs GetClassifiers = GetClassifiersResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetClassifiersResponse' Core.<$>
                   (x Core..:? "Classifiers") Core.<*> x Core..:? "NextToken" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager GetClassifiers where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"classifiers" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkGetClassifiersResponse' smart constructor.
data GetClassifiersResponse = GetClassifiersResponse'
  { classifiers :: Core.Maybe [Types.Classifier]
    -- ^ The requested list of classifier objects.
  , nextToken :: Core.Maybe Types.Token
    -- ^ A continuation token.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'GetClassifiersResponse' value with any optional fields omitted.
mkGetClassifiersResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetClassifiersResponse
mkGetClassifiersResponse responseStatus
  = GetClassifiersResponse'{classifiers = Core.Nothing,
                            nextToken = Core.Nothing, responseStatus}

-- | The requested list of classifier objects.
--
-- /Note:/ Consider using 'classifiers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcrgrsClassifiers :: Lens.Lens' GetClassifiersResponse (Core.Maybe [Types.Classifier])
gcrgrsClassifiers = Lens.field @"classifiers"
{-# INLINEABLE gcrgrsClassifiers #-}
{-# DEPRECATED classifiers "Use generic-lens or generic-optics with 'classifiers' instead"  #-}

-- | A continuation token.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcrgrsNextToken :: Lens.Lens' GetClassifiersResponse (Core.Maybe Types.Token)
gcrgrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE gcrgrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcrgrsResponseStatus :: Lens.Lens' GetClassifiersResponse Core.Int
gcrgrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gcrgrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
