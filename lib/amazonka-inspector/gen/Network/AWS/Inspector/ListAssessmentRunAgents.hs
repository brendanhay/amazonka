{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Inspector.ListAssessmentRunAgents
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the agents of the assessment runs that are specified by the ARNs of the assessment runs.
--
-- This operation returns paginated results.
module Network.AWS.Inspector.ListAssessmentRunAgents
    (
    -- * Creating a request
      ListAssessmentRunAgents (..)
    , mkListAssessmentRunAgents
    -- ** Request lenses
    , laraAssessmentRunArn
    , laraFilter
    , laraMaxResults
    , laraNextToken

    -- * Destructuring the response
    , ListAssessmentRunAgentsResponse (..)
    , mkListAssessmentRunAgentsResponse
    -- ** Response lenses
    , lararrsAssessmentRunAgents
    , lararrsNextToken
    , lararrsResponseStatus
    ) where

import qualified Network.AWS.Inspector.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListAssessmentRunAgents' smart constructor.
data ListAssessmentRunAgents = ListAssessmentRunAgents'
  { assessmentRunArn :: Types.Arn
    -- ^ The ARN that specifies the assessment run whose agents you want to list.
  , filter :: Core.Maybe Types.AgentFilter
    -- ^ You can use this parameter to specify a subset of data to be included in the action's response.
--
-- For a record to match a filter, all specified filter attributes must match. When multiple values are specified for a filter attribute, any of the values can match.
  , maxResults :: Core.Maybe Core.Int
    -- ^ You can use this parameter to indicate the maximum number of items that you want in the response. The default value is 10. The maximum value is 500.
  , nextToken :: Core.Maybe Types.PaginationToken
    -- ^ You can use this parameter when paginating results. Set the value of this parameter to null on your first call to the __ListAssessmentRunAgents__ action. Subsequent calls to the action fill __nextToken__ in the request with the value of __NextToken__ from the previous response to continue listing data.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListAssessmentRunAgents' value with any optional fields omitted.
mkListAssessmentRunAgents
    :: Types.Arn -- ^ 'assessmentRunArn'
    -> ListAssessmentRunAgents
mkListAssessmentRunAgents assessmentRunArn
  = ListAssessmentRunAgents'{assessmentRunArn, filter = Core.Nothing,
                             maxResults = Core.Nothing, nextToken = Core.Nothing}

-- | The ARN that specifies the assessment run whose agents you want to list.
--
-- /Note:/ Consider using 'assessmentRunArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laraAssessmentRunArn :: Lens.Lens' ListAssessmentRunAgents Types.Arn
laraAssessmentRunArn = Lens.field @"assessmentRunArn"
{-# INLINEABLE laraAssessmentRunArn #-}
{-# DEPRECATED assessmentRunArn "Use generic-lens or generic-optics with 'assessmentRunArn' instead"  #-}

-- | You can use this parameter to specify a subset of data to be included in the action's response.
--
-- For a record to match a filter, all specified filter attributes must match. When multiple values are specified for a filter attribute, any of the values can match.
--
-- /Note:/ Consider using 'filter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laraFilter :: Lens.Lens' ListAssessmentRunAgents (Core.Maybe Types.AgentFilter)
laraFilter = Lens.field @"filter"
{-# INLINEABLE laraFilter #-}
{-# DEPRECATED filter "Use generic-lens or generic-optics with 'filter' instead"  #-}

-- | You can use this parameter to indicate the maximum number of items that you want in the response. The default value is 10. The maximum value is 500.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laraMaxResults :: Lens.Lens' ListAssessmentRunAgents (Core.Maybe Core.Int)
laraMaxResults = Lens.field @"maxResults"
{-# INLINEABLE laraMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | You can use this parameter when paginating results. Set the value of this parameter to null on your first call to the __ListAssessmentRunAgents__ action. Subsequent calls to the action fill __nextToken__ in the request with the value of __NextToken__ from the previous response to continue listing data.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laraNextToken :: Lens.Lens' ListAssessmentRunAgents (Core.Maybe Types.PaginationToken)
laraNextToken = Lens.field @"nextToken"
{-# INLINEABLE laraNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery ListAssessmentRunAgents where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ListAssessmentRunAgents where
        toHeaders ListAssessmentRunAgents{..}
          = Core.pure
              ("X-Amz-Target", "InspectorService.ListAssessmentRunAgents")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ListAssessmentRunAgents where
        toJSON ListAssessmentRunAgents{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("assessmentRunArn" Core..= assessmentRunArn),
                  ("filter" Core..=) Core.<$> filter,
                  ("maxResults" Core..=) Core.<$> maxResults,
                  ("nextToken" Core..=) Core.<$> nextToken])

instance Core.AWSRequest ListAssessmentRunAgents where
        type Rs ListAssessmentRunAgents = ListAssessmentRunAgentsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListAssessmentRunAgentsResponse' Core.<$>
                   (x Core..:? "assessmentRunAgents" Core..!= Core.mempty) Core.<*>
                     x Core..:? "nextToken"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListAssessmentRunAgents where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop (rs Lens.^. Lens.field @"assessmentRunAgents") =
            Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkListAssessmentRunAgentsResponse' smart constructor.
data ListAssessmentRunAgentsResponse = ListAssessmentRunAgentsResponse'
  { assessmentRunAgents :: [Types.AssessmentRunAgent]
    -- ^ A list of ARNs that specifies the agents returned by the action.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ When a response is generated, if there is more data to be listed, this parameter is present in the response and contains the value to use for the __nextToken__ parameter in a subsequent pagination request. If there is no more data to be listed, this parameter is set to null.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListAssessmentRunAgentsResponse' value with any optional fields omitted.
mkListAssessmentRunAgentsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListAssessmentRunAgentsResponse
mkListAssessmentRunAgentsResponse responseStatus
  = ListAssessmentRunAgentsResponse'{assessmentRunAgents =
                                       Core.mempty,
                                     nextToken = Core.Nothing, responseStatus}

-- | A list of ARNs that specifies the agents returned by the action.
--
-- /Note:/ Consider using 'assessmentRunAgents' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lararrsAssessmentRunAgents :: Lens.Lens' ListAssessmentRunAgentsResponse [Types.AssessmentRunAgent]
lararrsAssessmentRunAgents = Lens.field @"assessmentRunAgents"
{-# INLINEABLE lararrsAssessmentRunAgents #-}
{-# DEPRECATED assessmentRunAgents "Use generic-lens or generic-optics with 'assessmentRunAgents' instead"  #-}

-- | When a response is generated, if there is more data to be listed, this parameter is present in the response and contains the value to use for the __nextToken__ parameter in a subsequent pagination request. If there is no more data to be listed, this parameter is set to null.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lararrsNextToken :: Lens.Lens' ListAssessmentRunAgentsResponse (Core.Maybe Types.NextToken)
lararrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE lararrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lararrsResponseStatus :: Lens.Lens' ListAssessmentRunAgentsResponse Core.Int
lararrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lararrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
