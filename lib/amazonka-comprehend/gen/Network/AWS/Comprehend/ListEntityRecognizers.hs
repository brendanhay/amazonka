{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.ListEntityRecognizers
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of the properties of all entity recognizers that you created, including recognizers currently in training. Allows you to filter the list of recognizers based on criteria such as status and submission time. This call returns up to 500 entity recognizers in the list, with a default number of 100 recognizers in the list.
--
-- The results of this list are not in any particular order. Please get the list and sort locally if needed.
--
-- This operation returns paginated results.
module Network.AWS.Comprehend.ListEntityRecognizers
    (
    -- * Creating a request
      ListEntityRecognizers (..)
    , mkListEntityRecognizers
    -- ** Request lenses
    , lerFilter
    , lerMaxResults
    , lerNextToken

    -- * Destructuring the response
    , ListEntityRecognizersResponse (..)
    , mkListEntityRecognizersResponse
    -- ** Response lenses
    , lerrrsEntityRecognizerPropertiesList
    , lerrrsNextToken
    , lerrrsResponseStatus
    ) where

import qualified Network.AWS.Comprehend.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListEntityRecognizers' smart constructor.
data ListEntityRecognizers = ListEntityRecognizers'
  { filter :: Core.Maybe Types.EntityRecognizerFilter
    -- ^ Filters the list of entities returned. You can filter on @Status@ , @SubmitTimeBefore@ , or @SubmitTimeAfter@ . You can only set one filter at a time.
  , maxResults :: Core.Maybe Core.Natural
    -- ^ The maximum number of results to return on each page. The default is 100.
  , nextToken :: Core.Maybe Core.Text
    -- ^ Identifies the next page of results to return.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ListEntityRecognizers' value with any optional fields omitted.
mkListEntityRecognizers
    :: ListEntityRecognizers
mkListEntityRecognizers
  = ListEntityRecognizers'{filter = Core.Nothing,
                           maxResults = Core.Nothing, nextToken = Core.Nothing}

-- | Filters the list of entities returned. You can filter on @Status@ , @SubmitTimeBefore@ , or @SubmitTimeAfter@ . You can only set one filter at a time.
--
-- /Note:/ Consider using 'filter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lerFilter :: Lens.Lens' ListEntityRecognizers (Core.Maybe Types.EntityRecognizerFilter)
lerFilter = Lens.field @"filter"
{-# INLINEABLE lerFilter #-}
{-# DEPRECATED filter "Use generic-lens or generic-optics with 'filter' instead"  #-}

-- | The maximum number of results to return on each page. The default is 100.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lerMaxResults :: Lens.Lens' ListEntityRecognizers (Core.Maybe Core.Natural)
lerMaxResults = Lens.field @"maxResults"
{-# INLINEABLE lerMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | Identifies the next page of results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lerNextToken :: Lens.Lens' ListEntityRecognizers (Core.Maybe Core.Text)
lerNextToken = Lens.field @"nextToken"
{-# INLINEABLE lerNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery ListEntityRecognizers where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ListEntityRecognizers where
        toHeaders ListEntityRecognizers{..}
          = Core.pure
              ("X-Amz-Target", "Comprehend_20171127.ListEntityRecognizers")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ListEntityRecognizers where
        toJSON ListEntityRecognizers{..}
          = Core.object
              (Core.catMaybes
                 [("Filter" Core..=) Core.<$> filter,
                  ("MaxResults" Core..=) Core.<$> maxResults,
                  ("NextToken" Core..=) Core.<$> nextToken])

instance Core.AWSRequest ListEntityRecognizers where
        type Rs ListEntityRecognizers = ListEntityRecognizersResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListEntityRecognizersResponse' Core.<$>
                   (x Core..:? "EntityRecognizerPropertiesList") Core.<*>
                     x Core..:? "NextToken"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListEntityRecognizers where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop
              (rs Lens.^?
                 Lens.field @"entityRecognizerPropertiesList" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkListEntityRecognizersResponse' smart constructor.
data ListEntityRecognizersResponse = ListEntityRecognizersResponse'
  { entityRecognizerPropertiesList :: Core.Maybe [Types.EntityRecognizerProperties]
    -- ^ The list of properties of an entity recognizer.
  , nextToken :: Core.Maybe Core.Text
    -- ^ Identifies the next page of results to return.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ListEntityRecognizersResponse' value with any optional fields omitted.
mkListEntityRecognizersResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListEntityRecognizersResponse
mkListEntityRecognizersResponse responseStatus
  = ListEntityRecognizersResponse'{entityRecognizerPropertiesList =
                                     Core.Nothing,
                                   nextToken = Core.Nothing, responseStatus}

-- | The list of properties of an entity recognizer.
--
-- /Note:/ Consider using 'entityRecognizerPropertiesList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lerrrsEntityRecognizerPropertiesList :: Lens.Lens' ListEntityRecognizersResponse (Core.Maybe [Types.EntityRecognizerProperties])
lerrrsEntityRecognizerPropertiesList = Lens.field @"entityRecognizerPropertiesList"
{-# INLINEABLE lerrrsEntityRecognizerPropertiesList #-}
{-# DEPRECATED entityRecognizerPropertiesList "Use generic-lens or generic-optics with 'entityRecognizerPropertiesList' instead"  #-}

-- | Identifies the next page of results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lerrrsNextToken :: Lens.Lens' ListEntityRecognizersResponse (Core.Maybe Core.Text)
lerrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE lerrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lerrrsResponseStatus :: Lens.Lens' ListEntityRecognizersResponse Core.Int
lerrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lerrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
