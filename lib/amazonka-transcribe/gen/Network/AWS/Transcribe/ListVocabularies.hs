{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Transcribe.ListVocabularies
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of vocabularies that match the specified criteria. If no criteria are specified, returns the entire list of vocabularies.
module Network.AWS.Transcribe.ListVocabularies
    (
    -- * Creating a request
      ListVocabularies (..)
    , mkListVocabularies
    -- ** Request lenses
    , lvMaxResults
    , lvNameContains
    , lvNextToken
    , lvStateEquals

    -- * Destructuring the response
    , ListVocabulariesResponse (..)
    , mkListVocabulariesResponse
    -- ** Response lenses
    , lvrrsNextToken
    , lvrrsStatus
    , lvrrsVocabularies
    , lvrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.Transcribe.Types as Types

-- | /See:/ 'mkListVocabularies' smart constructor.
data ListVocabularies = ListVocabularies'
  { maxResults :: Core.Maybe Core.Natural
    -- ^ The maximum number of vocabularies to return in the response. If there are fewer results in the list, this response contains only the actual results.
  , nameContains :: Core.Maybe Types.VocabularyName
    -- ^ When specified, the vocabularies returned in the list are limited to vocabularies whose name contains the specified string. The search is not case sensitive, @ListVocabularies@ returns both "vocabularyname" and "VocabularyName" in the response list.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ If the result of the previous request to @ListVocabularies@ was truncated, include the @NextToken@ to fetch the next set of jobs.
  , stateEquals :: Core.Maybe Types.VocabularyState
    -- ^ When specified, only returns vocabularies with the @VocabularyState@ field equal to the specified state.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListVocabularies' value with any optional fields omitted.
mkListVocabularies
    :: ListVocabularies
mkListVocabularies
  = ListVocabularies'{maxResults = Core.Nothing,
                      nameContains = Core.Nothing, nextToken = Core.Nothing,
                      stateEquals = Core.Nothing}

-- | The maximum number of vocabularies to return in the response. If there are fewer results in the list, this response contains only the actual results.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lvMaxResults :: Lens.Lens' ListVocabularies (Core.Maybe Core.Natural)
lvMaxResults = Lens.field @"maxResults"
{-# INLINEABLE lvMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | When specified, the vocabularies returned in the list are limited to vocabularies whose name contains the specified string. The search is not case sensitive, @ListVocabularies@ returns both "vocabularyname" and "VocabularyName" in the response list.
--
-- /Note:/ Consider using 'nameContains' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lvNameContains :: Lens.Lens' ListVocabularies (Core.Maybe Types.VocabularyName)
lvNameContains = Lens.field @"nameContains"
{-# INLINEABLE lvNameContains #-}
{-# DEPRECATED nameContains "Use generic-lens or generic-optics with 'nameContains' instead"  #-}

-- | If the result of the previous request to @ListVocabularies@ was truncated, include the @NextToken@ to fetch the next set of jobs.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lvNextToken :: Lens.Lens' ListVocabularies (Core.Maybe Types.NextToken)
lvNextToken = Lens.field @"nextToken"
{-# INLINEABLE lvNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | When specified, only returns vocabularies with the @VocabularyState@ field equal to the specified state.
--
-- /Note:/ Consider using 'stateEquals' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lvStateEquals :: Lens.Lens' ListVocabularies (Core.Maybe Types.VocabularyState)
lvStateEquals = Lens.field @"stateEquals"
{-# INLINEABLE lvStateEquals #-}
{-# DEPRECATED stateEquals "Use generic-lens or generic-optics with 'stateEquals' instead"  #-}

instance Core.ToQuery ListVocabularies where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ListVocabularies where
        toHeaders ListVocabularies{..}
          = Core.pure ("X-Amz-Target", "Transcribe.ListVocabularies") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ListVocabularies where
        toJSON ListVocabularies{..}
          = Core.object
              (Core.catMaybes
                 [("MaxResults" Core..=) Core.<$> maxResults,
                  ("NameContains" Core..=) Core.<$> nameContains,
                  ("NextToken" Core..=) Core.<$> nextToken,
                  ("StateEquals" Core..=) Core.<$> stateEquals])

instance Core.AWSRequest ListVocabularies where
        type Rs ListVocabularies = ListVocabulariesResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListVocabulariesResponse' Core.<$>
                   (x Core..:? "NextToken") Core.<*> x Core..:? "Status" Core.<*>
                     x Core..:? "Vocabularies"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkListVocabulariesResponse' smart constructor.
data ListVocabulariesResponse = ListVocabulariesResponse'
  { nextToken :: Core.Maybe Types.NextToken
    -- ^ The @ListVocabularies@ operation returns a page of vocabularies at a time. The maximum size of the page is set in the @MaxResults@ parameter. If there are more jobs in the list than will fit on the page, Amazon Transcribe returns the @NextPage@ token. To return in the next page of jobs, include the token in the next request to the @ListVocabularies@ operation.
  , status :: Core.Maybe Types.VocabularyState
    -- ^ The requested vocabulary state.
  , vocabularies :: Core.Maybe [Types.VocabularyInfo]
    -- ^ A list of objects that describe the vocabularies that match the search criteria in the request.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ListVocabulariesResponse' value with any optional fields omitted.
mkListVocabulariesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListVocabulariesResponse
mkListVocabulariesResponse responseStatus
  = ListVocabulariesResponse'{nextToken = Core.Nothing,
                              status = Core.Nothing, vocabularies = Core.Nothing, responseStatus}

-- | The @ListVocabularies@ operation returns a page of vocabularies at a time. The maximum size of the page is set in the @MaxResults@ parameter. If there are more jobs in the list than will fit on the page, Amazon Transcribe returns the @NextPage@ token. To return in the next page of jobs, include the token in the next request to the @ListVocabularies@ operation.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lvrrsNextToken :: Lens.Lens' ListVocabulariesResponse (Core.Maybe Types.NextToken)
lvrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE lvrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The requested vocabulary state.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lvrrsStatus :: Lens.Lens' ListVocabulariesResponse (Core.Maybe Types.VocabularyState)
lvrrsStatus = Lens.field @"status"
{-# INLINEABLE lvrrsStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

-- | A list of objects that describe the vocabularies that match the search criteria in the request.
--
-- /Note:/ Consider using 'vocabularies' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lvrrsVocabularies :: Lens.Lens' ListVocabulariesResponse (Core.Maybe [Types.VocabularyInfo])
lvrrsVocabularies = Lens.field @"vocabularies"
{-# INLINEABLE lvrrsVocabularies #-}
{-# DEPRECATED vocabularies "Use generic-lens or generic-optics with 'vocabularies' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lvrrsResponseStatus :: Lens.Lens' ListVocabulariesResponse Core.Int
lvrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lvrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
