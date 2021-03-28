{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Transcribe.ListVocabularyFilters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about vocabulary filters.
module Network.AWS.Transcribe.ListVocabularyFilters
    (
    -- * Creating a request
      ListVocabularyFilters (..)
    , mkListVocabularyFilters
    -- ** Request lenses
    , lvfMaxResults
    , lvfNameContains
    , lvfNextToken

    -- * Destructuring the response
    , ListVocabularyFiltersResponse (..)
    , mkListVocabularyFiltersResponse
    -- ** Response lenses
    , lvfrrsNextToken
    , lvfrrsVocabularyFilters
    , lvfrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.Transcribe.Types as Types

-- | /See:/ 'mkListVocabularyFilters' smart constructor.
data ListVocabularyFilters = ListVocabularyFilters'
  { maxResults :: Core.Maybe Core.Natural
    -- ^ The maximum number of filters to return in the response. If there are fewer results in the list, this response contains only the actual results.
  , nameContains :: Core.Maybe Types.NameContains
    -- ^ Filters the response so that it only contains vocabulary filters whose name contains the specified string.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ If the result of the previous request to @ListVocabularyFilters@ was truncated, include the @NextToken@ to fetch the next set of collections.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListVocabularyFilters' value with any optional fields omitted.
mkListVocabularyFilters
    :: ListVocabularyFilters
mkListVocabularyFilters
  = ListVocabularyFilters'{maxResults = Core.Nothing,
                           nameContains = Core.Nothing, nextToken = Core.Nothing}

-- | The maximum number of filters to return in the response. If there are fewer results in the list, this response contains only the actual results.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lvfMaxResults :: Lens.Lens' ListVocabularyFilters (Core.Maybe Core.Natural)
lvfMaxResults = Lens.field @"maxResults"
{-# INLINEABLE lvfMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | Filters the response so that it only contains vocabulary filters whose name contains the specified string.
--
-- /Note:/ Consider using 'nameContains' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lvfNameContains :: Lens.Lens' ListVocabularyFilters (Core.Maybe Types.NameContains)
lvfNameContains = Lens.field @"nameContains"
{-# INLINEABLE lvfNameContains #-}
{-# DEPRECATED nameContains "Use generic-lens or generic-optics with 'nameContains' instead"  #-}

-- | If the result of the previous request to @ListVocabularyFilters@ was truncated, include the @NextToken@ to fetch the next set of collections.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lvfNextToken :: Lens.Lens' ListVocabularyFilters (Core.Maybe Types.NextToken)
lvfNextToken = Lens.field @"nextToken"
{-# INLINEABLE lvfNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery ListVocabularyFilters where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ListVocabularyFilters where
        toHeaders ListVocabularyFilters{..}
          = Core.pure ("X-Amz-Target", "Transcribe.ListVocabularyFilters")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ListVocabularyFilters where
        toJSON ListVocabularyFilters{..}
          = Core.object
              (Core.catMaybes
                 [("MaxResults" Core..=) Core.<$> maxResults,
                  ("NameContains" Core..=) Core.<$> nameContains,
                  ("NextToken" Core..=) Core.<$> nextToken])

instance Core.AWSRequest ListVocabularyFilters where
        type Rs ListVocabularyFilters = ListVocabularyFiltersResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListVocabularyFiltersResponse' Core.<$>
                   (x Core..:? "NextToken") Core.<*> x Core..:? "VocabularyFilters"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkListVocabularyFiltersResponse' smart constructor.
data ListVocabularyFiltersResponse = ListVocabularyFiltersResponse'
  { nextToken :: Core.Maybe Types.NextToken
    -- ^ The @ListVocabularyFilters@ operation returns a page of collections at a time. The maximum size of the page is set by the @MaxResults@ parameter. If there are more jobs in the list than the page size, Amazon Transcribe returns the @NextPage@ token. Include the token in the next request to the @ListVocabularyFilters@ operation to return in the next page of jobs.
  , vocabularyFilters :: Core.Maybe [Types.VocabularyFilterInfo]
    -- ^ The list of vocabulary filters. It contains at most @MaxResults@ number of filters. If there are more filters, call the @ListVocabularyFilters@ operation again with the @NextToken@ parameter in the request set to the value of the @NextToken@ field in the response.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ListVocabularyFiltersResponse' value with any optional fields omitted.
mkListVocabularyFiltersResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListVocabularyFiltersResponse
mkListVocabularyFiltersResponse responseStatus
  = ListVocabularyFiltersResponse'{nextToken = Core.Nothing,
                                   vocabularyFilters = Core.Nothing, responseStatus}

-- | The @ListVocabularyFilters@ operation returns a page of collections at a time. The maximum size of the page is set by the @MaxResults@ parameter. If there are more jobs in the list than the page size, Amazon Transcribe returns the @NextPage@ token. Include the token in the next request to the @ListVocabularyFilters@ operation to return in the next page of jobs.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lvfrrsNextToken :: Lens.Lens' ListVocabularyFiltersResponse (Core.Maybe Types.NextToken)
lvfrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE lvfrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The list of vocabulary filters. It contains at most @MaxResults@ number of filters. If there are more filters, call the @ListVocabularyFilters@ operation again with the @NextToken@ parameter in the request set to the value of the @NextToken@ field in the response.
--
-- /Note:/ Consider using 'vocabularyFilters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lvfrrsVocabularyFilters :: Lens.Lens' ListVocabularyFiltersResponse (Core.Maybe [Types.VocabularyFilterInfo])
lvfrrsVocabularyFilters = Lens.field @"vocabularyFilters"
{-# INLINEABLE lvfrrsVocabularyFilters #-}
{-# DEPRECATED vocabularyFilters "Use generic-lens or generic-optics with 'vocabularyFilters' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lvfrrsResponseStatus :: Lens.Lens' ListVocabularyFiltersResponse Core.Int
lvfrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lvfrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
