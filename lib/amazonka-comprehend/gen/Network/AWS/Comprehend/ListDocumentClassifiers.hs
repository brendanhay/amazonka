{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.ListDocumentClassifiers
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of the document classifiers that you have created.
--
-- This operation returns paginated results.
module Network.AWS.Comprehend.ListDocumentClassifiers
    (
    -- * Creating a request
      ListDocumentClassifiers (..)
    , mkListDocumentClassifiers
    -- ** Request lenses
    , ldcFilter
    , ldcMaxResults
    , ldcNextToken

    -- * Destructuring the response
    , ListDocumentClassifiersResponse (..)
    , mkListDocumentClassifiersResponse
    -- ** Response lenses
    , ldcrrsDocumentClassifierPropertiesList
    , ldcrrsNextToken
    , ldcrrsResponseStatus
    ) where

import qualified Network.AWS.Comprehend.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListDocumentClassifiers' smart constructor.
data ListDocumentClassifiers = ListDocumentClassifiers'
  { filter :: Core.Maybe Types.DocumentClassifierFilter
    -- ^ Filters the jobs that are returned. You can filter jobs on their name, status, or the date and time that they were submitted. You can only set one filter at a time.
  , maxResults :: Core.Maybe Core.Natural
    -- ^ The maximum number of results to return in each page. The default is 100.
  , nextToken :: Core.Maybe Core.Text
    -- ^ Identifies the next page of results to return.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ListDocumentClassifiers' value with any optional fields omitted.
mkListDocumentClassifiers
    :: ListDocumentClassifiers
mkListDocumentClassifiers
  = ListDocumentClassifiers'{filter = Core.Nothing,
                             maxResults = Core.Nothing, nextToken = Core.Nothing}

-- | Filters the jobs that are returned. You can filter jobs on their name, status, or the date and time that they were submitted. You can only set one filter at a time.
--
-- /Note:/ Consider using 'filter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldcFilter :: Lens.Lens' ListDocumentClassifiers (Core.Maybe Types.DocumentClassifierFilter)
ldcFilter = Lens.field @"filter"
{-# INLINEABLE ldcFilter #-}
{-# DEPRECATED filter "Use generic-lens or generic-optics with 'filter' instead"  #-}

-- | The maximum number of results to return in each page. The default is 100.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldcMaxResults :: Lens.Lens' ListDocumentClassifiers (Core.Maybe Core.Natural)
ldcMaxResults = Lens.field @"maxResults"
{-# INLINEABLE ldcMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | Identifies the next page of results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldcNextToken :: Lens.Lens' ListDocumentClassifiers (Core.Maybe Core.Text)
ldcNextToken = Lens.field @"nextToken"
{-# INLINEABLE ldcNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery ListDocumentClassifiers where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ListDocumentClassifiers where
        toHeaders ListDocumentClassifiers{..}
          = Core.pure
              ("X-Amz-Target", "Comprehend_20171127.ListDocumentClassifiers")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ListDocumentClassifiers where
        toJSON ListDocumentClassifiers{..}
          = Core.object
              (Core.catMaybes
                 [("Filter" Core..=) Core.<$> filter,
                  ("MaxResults" Core..=) Core.<$> maxResults,
                  ("NextToken" Core..=) Core.<$> nextToken])

instance Core.AWSRequest ListDocumentClassifiers where
        type Rs ListDocumentClassifiers = ListDocumentClassifiersResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListDocumentClassifiersResponse' Core.<$>
                   (x Core..:? "DocumentClassifierPropertiesList") Core.<*>
                     x Core..:? "NextToken"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListDocumentClassifiers where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop
              (rs Lens.^?
                 Lens.field @"documentClassifierPropertiesList" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkListDocumentClassifiersResponse' smart constructor.
data ListDocumentClassifiersResponse = ListDocumentClassifiersResponse'
  { documentClassifierPropertiesList :: Core.Maybe [Types.DocumentClassifierProperties]
    -- ^ A list containing the properties of each job returned.
  , nextToken :: Core.Maybe Core.Text
    -- ^ Identifies the next page of results to return.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ListDocumentClassifiersResponse' value with any optional fields omitted.
mkListDocumentClassifiersResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListDocumentClassifiersResponse
mkListDocumentClassifiersResponse responseStatus
  = ListDocumentClassifiersResponse'{documentClassifierPropertiesList
                                       = Core.Nothing,
                                     nextToken = Core.Nothing, responseStatus}

-- | A list containing the properties of each job returned.
--
-- /Note:/ Consider using 'documentClassifierPropertiesList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldcrrsDocumentClassifierPropertiesList :: Lens.Lens' ListDocumentClassifiersResponse (Core.Maybe [Types.DocumentClassifierProperties])
ldcrrsDocumentClassifierPropertiesList = Lens.field @"documentClassifierPropertiesList"
{-# INLINEABLE ldcrrsDocumentClassifierPropertiesList #-}
{-# DEPRECATED documentClassifierPropertiesList "Use generic-lens or generic-optics with 'documentClassifierPropertiesList' instead"  #-}

-- | Identifies the next page of results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldcrrsNextToken :: Lens.Lens' ListDocumentClassifiersResponse (Core.Maybe Core.Text)
ldcrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE ldcrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldcrrsResponseStatus :: Lens.Lens' ListDocumentClassifiersResponse Core.Int
ldcrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ldcrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
