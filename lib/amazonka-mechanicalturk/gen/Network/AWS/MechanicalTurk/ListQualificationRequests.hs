{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MechanicalTurk.ListQualificationRequests
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The @ListQualificationRequests@ operation retrieves requests for Qualifications of a particular Qualification type. The owner of the Qualification type calls this operation to poll for pending requests, and accepts them using the AcceptQualification operation. 
--
-- This operation returns paginated results.
module Network.AWS.MechanicalTurk.ListQualificationRequests
    (
    -- * Creating a request
      ListQualificationRequests (..)
    , mkListQualificationRequests
    -- ** Request lenses
    , lqrMaxResults
    , lqrNextToken
    , lqrQualificationTypeId

    -- * Destructuring the response
    , ListQualificationRequestsResponse (..)
    , mkListQualificationRequestsResponse
    -- ** Response lenses
    , lqrrrsNextToken
    , lqrrrsNumResults
    , lqrrrsQualificationRequests
    , lqrrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MechanicalTurk.Types as Types
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListQualificationRequests' smart constructor.
data ListQualificationRequests = ListQualificationRequests'
  { maxResults :: Core.Maybe Core.Natural
    -- ^ The maximum number of results to return in a single call. 
  , nextToken :: Core.Maybe Types.PaginationToken
  , qualificationTypeId :: Core.Maybe Types.QualificationTypeId
    -- ^ The ID of the QualificationType.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListQualificationRequests' value with any optional fields omitted.
mkListQualificationRequests
    :: ListQualificationRequests
mkListQualificationRequests
  = ListQualificationRequests'{maxResults = Core.Nothing,
                               nextToken = Core.Nothing, qualificationTypeId = Core.Nothing}

-- | The maximum number of results to return in a single call. 
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lqrMaxResults :: Lens.Lens' ListQualificationRequests (Core.Maybe Core.Natural)
lqrMaxResults = Lens.field @"maxResults"
{-# INLINEABLE lqrMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lqrNextToken :: Lens.Lens' ListQualificationRequests (Core.Maybe Types.PaginationToken)
lqrNextToken = Lens.field @"nextToken"
{-# INLINEABLE lqrNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The ID of the QualificationType.
--
-- /Note:/ Consider using 'qualificationTypeId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lqrQualificationTypeId :: Lens.Lens' ListQualificationRequests (Core.Maybe Types.QualificationTypeId)
lqrQualificationTypeId = Lens.field @"qualificationTypeId"
{-# INLINEABLE lqrQualificationTypeId #-}
{-# DEPRECATED qualificationTypeId "Use generic-lens or generic-optics with 'qualificationTypeId' instead"  #-}

instance Core.ToQuery ListQualificationRequests where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ListQualificationRequests where
        toHeaders ListQualificationRequests{..}
          = Core.pure
              ("X-Amz-Target",
               "MTurkRequesterServiceV20170117.ListQualificationRequests")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ListQualificationRequests where
        toJSON ListQualificationRequests{..}
          = Core.object
              (Core.catMaybes
                 [("MaxResults" Core..=) Core.<$> maxResults,
                  ("NextToken" Core..=) Core.<$> nextToken,
                  ("QualificationTypeId" Core..=) Core.<$> qualificationTypeId])

instance Core.AWSRequest ListQualificationRequests where
        type Rs ListQualificationRequests =
             ListQualificationRequestsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListQualificationRequestsResponse' Core.<$>
                   (x Core..:? "NextToken") Core.<*> x Core..:? "NumResults" Core.<*>
                     x Core..:? "QualificationRequests"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListQualificationRequests where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"qualificationRequests" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkListQualificationRequestsResponse' smart constructor.
data ListQualificationRequestsResponse = ListQualificationRequestsResponse'
  { nextToken :: Core.Maybe Types.PaginationToken
  , numResults :: Core.Maybe Core.Int
    -- ^ The number of Qualification requests on this page in the filtered results list, equivalent to the number of Qualification requests being returned by this call.
  , qualificationRequests :: Core.Maybe [Types.QualificationRequest]
    -- ^ The Qualification request. The response includes one QualificationRequest element for each Qualification request returned by the query.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ListQualificationRequestsResponse' value with any optional fields omitted.
mkListQualificationRequestsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListQualificationRequestsResponse
mkListQualificationRequestsResponse responseStatus
  = ListQualificationRequestsResponse'{nextToken = Core.Nothing,
                                       numResults = Core.Nothing,
                                       qualificationRequests = Core.Nothing, responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lqrrrsNextToken :: Lens.Lens' ListQualificationRequestsResponse (Core.Maybe Types.PaginationToken)
lqrrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE lqrrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The number of Qualification requests on this page in the filtered results list, equivalent to the number of Qualification requests being returned by this call.
--
-- /Note:/ Consider using 'numResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lqrrrsNumResults :: Lens.Lens' ListQualificationRequestsResponse (Core.Maybe Core.Int)
lqrrrsNumResults = Lens.field @"numResults"
{-# INLINEABLE lqrrrsNumResults #-}
{-# DEPRECATED numResults "Use generic-lens or generic-optics with 'numResults' instead"  #-}

-- | The Qualification request. The response includes one QualificationRequest element for each Qualification request returned by the query.
--
-- /Note:/ Consider using 'qualificationRequests' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lqrrrsQualificationRequests :: Lens.Lens' ListQualificationRequestsResponse (Core.Maybe [Types.QualificationRequest])
lqrrrsQualificationRequests = Lens.field @"qualificationRequests"
{-# INLINEABLE lqrrrsQualificationRequests #-}
{-# DEPRECATED qualificationRequests "Use generic-lens or generic-optics with 'qualificationRequests' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lqrrrsResponseStatus :: Lens.Lens' ListQualificationRequestsResponse Core.Int
lqrrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lqrrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
