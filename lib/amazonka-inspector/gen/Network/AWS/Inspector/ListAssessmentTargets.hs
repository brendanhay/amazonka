{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Inspector.ListAssessmentTargets
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the ARNs of the assessment targets within this AWS account. For more information about assessment targets, see <https://docs.aws.amazon.com/inspector/latest/userguide/inspector_applications.html Amazon Inspector Assessment Targets> .
--
-- This operation returns paginated results.
module Network.AWS.Inspector.ListAssessmentTargets
    (
    -- * Creating a request
      ListAssessmentTargets (..)
    , mkListAssessmentTargets
    -- ** Request lenses
    , lFilter
    , lMaxResults
    , lNextToken

    -- * Destructuring the response
    , ListAssessmentTargetsResponse (..)
    , mkListAssessmentTargetsResponse
    -- ** Response lenses
    , lrsAssessmentTargetArns
    , lrsNextToken
    , lrsResponseStatus
    ) where

import qualified Network.AWS.Inspector.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListAssessmentTargets' smart constructor.
data ListAssessmentTargets = ListAssessmentTargets'
  { filter :: Core.Maybe Types.AssessmentTargetFilter
    -- ^ You can use this parameter to specify a subset of data to be included in the action's response.
--
-- For a record to match a filter, all specified filter attributes must match. When multiple values are specified for a filter attribute, any of the values can match.
  , maxResults :: Core.Maybe Core.Int
    -- ^ You can use this parameter to indicate the maximum number of items you want in the response. The default value is 10. The maximum value is 500.
  , nextToken :: Core.Maybe Types.PaginationToken
    -- ^ You can use this parameter when paginating results. Set the value of this parameter to null on your first call to the __ListAssessmentTargets__ action. Subsequent calls to the action fill __nextToken__ in the request with the value of __NextToken__ from the previous response to continue listing data.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListAssessmentTargets' value with any optional fields omitted.
mkListAssessmentTargets
    :: ListAssessmentTargets
mkListAssessmentTargets
  = ListAssessmentTargets'{filter = Core.Nothing,
                           maxResults = Core.Nothing, nextToken = Core.Nothing}

-- | You can use this parameter to specify a subset of data to be included in the action's response.
--
-- For a record to match a filter, all specified filter attributes must match. When multiple values are specified for a filter attribute, any of the values can match.
--
-- /Note:/ Consider using 'filter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lFilter :: Lens.Lens' ListAssessmentTargets (Core.Maybe Types.AssessmentTargetFilter)
lFilter = Lens.field @"filter"
{-# INLINEABLE lFilter #-}
{-# DEPRECATED filter "Use generic-lens or generic-optics with 'filter' instead"  #-}

-- | You can use this parameter to indicate the maximum number of items you want in the response. The default value is 10. The maximum value is 500.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lMaxResults :: Lens.Lens' ListAssessmentTargets (Core.Maybe Core.Int)
lMaxResults = Lens.field @"maxResults"
{-# INLINEABLE lMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | You can use this parameter when paginating results. Set the value of this parameter to null on your first call to the __ListAssessmentTargets__ action. Subsequent calls to the action fill __nextToken__ in the request with the value of __NextToken__ from the previous response to continue listing data.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lNextToken :: Lens.Lens' ListAssessmentTargets (Core.Maybe Types.PaginationToken)
lNextToken = Lens.field @"nextToken"
{-# INLINEABLE lNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery ListAssessmentTargets where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ListAssessmentTargets where
        toHeaders ListAssessmentTargets{..}
          = Core.pure
              ("X-Amz-Target", "InspectorService.ListAssessmentTargets")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ListAssessmentTargets where
        toJSON ListAssessmentTargets{..}
          = Core.object
              (Core.catMaybes
                 [("filter" Core..=) Core.<$> filter,
                  ("maxResults" Core..=) Core.<$> maxResults,
                  ("nextToken" Core..=) Core.<$> nextToken])

instance Core.AWSRequest ListAssessmentTargets where
        type Rs ListAssessmentTargets = ListAssessmentTargetsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListAssessmentTargetsResponse' Core.<$>
                   (x Core..:? "assessmentTargetArns" Core..!= Core.mempty) Core.<*>
                     x Core..:? "nextToken"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListAssessmentTargets where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop (rs Lens.^. Lens.field @"assessmentTargetArns") =
            Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkListAssessmentTargetsResponse' smart constructor.
data ListAssessmentTargetsResponse = ListAssessmentTargetsResponse'
  { assessmentTargetArns :: [Types.Arn]
    -- ^ A list of ARNs that specifies the assessment targets that are returned by the action.
  , nextToken :: Core.Maybe Types.PaginationToken
    -- ^ When a response is generated, if there is more data to be listed, this parameter is present in the response and contains the value to use for the __nextToken__ parameter in a subsequent pagination request. If there is no more data to be listed, this parameter is set to null.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListAssessmentTargetsResponse' value with any optional fields omitted.
mkListAssessmentTargetsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListAssessmentTargetsResponse
mkListAssessmentTargetsResponse responseStatus
  = ListAssessmentTargetsResponse'{assessmentTargetArns =
                                     Core.mempty,
                                   nextToken = Core.Nothing, responseStatus}

-- | A list of ARNs that specifies the assessment targets that are returned by the action.
--
-- /Note:/ Consider using 'assessmentTargetArns' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrsAssessmentTargetArns :: Lens.Lens' ListAssessmentTargetsResponse [Types.Arn]
lrsAssessmentTargetArns = Lens.field @"assessmentTargetArns"
{-# INLINEABLE lrsAssessmentTargetArns #-}
{-# DEPRECATED assessmentTargetArns "Use generic-lens or generic-optics with 'assessmentTargetArns' instead"  #-}

-- | When a response is generated, if there is more data to be listed, this parameter is present in the response and contains the value to use for the __nextToken__ parameter in a subsequent pagination request. If there is no more data to be listed, this parameter is set to null.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrsNextToken :: Lens.Lens' ListAssessmentTargetsResponse (Core.Maybe Types.PaginationToken)
lrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE lrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrsResponseStatus :: Lens.Lens' ListAssessmentTargetsResponse Core.Int
lrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
