{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Inspector.ListAssessmentRuns
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the assessment runs that correspond to the assessment templates that are specified by the ARNs of the assessment templates.
--
-- This operation returns paginated results.
module Network.AWS.Inspector.ListAssessmentRuns
    (
    -- * Creating a request
      ListAssessmentRuns (..)
    , mkListAssessmentRuns
    -- ** Request lenses
    , larAssessmentTemplateArns
    , larFilter
    , larMaxResults
    , larNextToken

    -- * Destructuring the response
    , ListAssessmentRunsResponse (..)
    , mkListAssessmentRunsResponse
    -- ** Response lenses
    , larrrsAssessmentRunArns
    , larrrsNextToken
    , larrrsResponseStatus
    ) where

import qualified Network.AWS.Inspector.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListAssessmentRuns' smart constructor.
data ListAssessmentRuns = ListAssessmentRuns'
  { assessmentTemplateArns :: Core.Maybe [Types.Arn]
    -- ^ The ARNs that specify the assessment templates whose assessment runs you want to list.
  , filter :: Core.Maybe Types.AssessmentRunFilter
    -- ^ You can use this parameter to specify a subset of data to be included in the action's response.
--
-- For a record to match a filter, all specified filter attributes must match. When multiple values are specified for a filter attribute, any of the values can match.
  , maxResults :: Core.Maybe Core.Int
    -- ^ You can use this parameter to indicate the maximum number of items that you want in the response. The default value is 10. The maximum value is 500.
  , nextToken :: Core.Maybe Types.PaginationToken
    -- ^ You can use this parameter when paginating results. Set the value of this parameter to null on your first call to the __ListAssessmentRuns__ action. Subsequent calls to the action fill __nextToken__ in the request with the value of __NextToken__ from the previous response to continue listing data.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ListAssessmentRuns' value with any optional fields omitted.
mkListAssessmentRuns
    :: ListAssessmentRuns
mkListAssessmentRuns
  = ListAssessmentRuns'{assessmentTemplateArns = Core.Nothing,
                        filter = Core.Nothing, maxResults = Core.Nothing,
                        nextToken = Core.Nothing}

-- | The ARNs that specify the assessment templates whose assessment runs you want to list.
--
-- /Note:/ Consider using 'assessmentTemplateArns' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
larAssessmentTemplateArns :: Lens.Lens' ListAssessmentRuns (Core.Maybe [Types.Arn])
larAssessmentTemplateArns = Lens.field @"assessmentTemplateArns"
{-# INLINEABLE larAssessmentTemplateArns #-}
{-# DEPRECATED assessmentTemplateArns "Use generic-lens or generic-optics with 'assessmentTemplateArns' instead"  #-}

-- | You can use this parameter to specify a subset of data to be included in the action's response.
--
-- For a record to match a filter, all specified filter attributes must match. When multiple values are specified for a filter attribute, any of the values can match.
--
-- /Note:/ Consider using 'filter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
larFilter :: Lens.Lens' ListAssessmentRuns (Core.Maybe Types.AssessmentRunFilter)
larFilter = Lens.field @"filter"
{-# INLINEABLE larFilter #-}
{-# DEPRECATED filter "Use generic-lens or generic-optics with 'filter' instead"  #-}

-- | You can use this parameter to indicate the maximum number of items that you want in the response. The default value is 10. The maximum value is 500.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
larMaxResults :: Lens.Lens' ListAssessmentRuns (Core.Maybe Core.Int)
larMaxResults = Lens.field @"maxResults"
{-# INLINEABLE larMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | You can use this parameter when paginating results. Set the value of this parameter to null on your first call to the __ListAssessmentRuns__ action. Subsequent calls to the action fill __nextToken__ in the request with the value of __NextToken__ from the previous response to continue listing data.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
larNextToken :: Lens.Lens' ListAssessmentRuns (Core.Maybe Types.PaginationToken)
larNextToken = Lens.field @"nextToken"
{-# INLINEABLE larNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery ListAssessmentRuns where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ListAssessmentRuns where
        toHeaders ListAssessmentRuns{..}
          = Core.pure ("X-Amz-Target", "InspectorService.ListAssessmentRuns")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ListAssessmentRuns where
        toJSON ListAssessmentRuns{..}
          = Core.object
              (Core.catMaybes
                 [("assessmentTemplateArns" Core..=) Core.<$>
                    assessmentTemplateArns,
                  ("filter" Core..=) Core.<$> filter,
                  ("maxResults" Core..=) Core.<$> maxResults,
                  ("nextToken" Core..=) Core.<$> nextToken])

instance Core.AWSRequest ListAssessmentRuns where
        type Rs ListAssessmentRuns = ListAssessmentRunsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListAssessmentRunsResponse' Core.<$>
                   (x Core..:? "assessmentRunArns" Core..!= Core.mempty) Core.<*>
                     x Core..:? "nextToken"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListAssessmentRuns where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop (rs Lens.^. Lens.field @"assessmentRunArns") =
            Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkListAssessmentRunsResponse' smart constructor.
data ListAssessmentRunsResponse = ListAssessmentRunsResponse'
  { assessmentRunArns :: [Types.Arn]
    -- ^ A list of ARNs that specifies the assessment runs that are returned by the action.
  , nextToken :: Core.Maybe Types.PaginationToken
    -- ^ When a response is generated, if there is more data to be listed, this parameter is present in the response and contains the value to use for the __nextToken__ parameter in a subsequent pagination request. If there is no more data to be listed, this parameter is set to null.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListAssessmentRunsResponse' value with any optional fields omitted.
mkListAssessmentRunsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListAssessmentRunsResponse
mkListAssessmentRunsResponse responseStatus
  = ListAssessmentRunsResponse'{assessmentRunArns = Core.mempty,
                                nextToken = Core.Nothing, responseStatus}

-- | A list of ARNs that specifies the assessment runs that are returned by the action.
--
-- /Note:/ Consider using 'assessmentRunArns' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
larrrsAssessmentRunArns :: Lens.Lens' ListAssessmentRunsResponse [Types.Arn]
larrrsAssessmentRunArns = Lens.field @"assessmentRunArns"
{-# INLINEABLE larrrsAssessmentRunArns #-}
{-# DEPRECATED assessmentRunArns "Use generic-lens or generic-optics with 'assessmentRunArns' instead"  #-}

-- | When a response is generated, if there is more data to be listed, this parameter is present in the response and contains the value to use for the __nextToken__ parameter in a subsequent pagination request. If there is no more data to be listed, this parameter is set to null.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
larrrsNextToken :: Lens.Lens' ListAssessmentRunsResponse (Core.Maybe Types.PaginationToken)
larrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE larrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
larrrsResponseStatus :: Lens.Lens' ListAssessmentRunsResponse Core.Int
larrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE larrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
