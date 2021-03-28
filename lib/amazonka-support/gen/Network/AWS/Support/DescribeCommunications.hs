{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Support.DescribeCommunications
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns communications and attachments for one or more support cases. Use the @afterTime@ and @beforeTime@ parameters to filter by date. You can use the @caseId@ parameter to restrict the results to a specific case.
--
-- Case data is available for 12 months after creation. If a case was created more than 12 months ago, a request for data might cause an error.
-- You can use the @maxResults@ and @nextToken@ parameters to control the pagination of the results. Set @maxResults@ to the number of cases that you want to display on each page, and use @nextToken@ to specify the resumption of pagination.
--
-- This operation returns paginated results.
module Network.AWS.Support.DescribeCommunications
    (
    -- * Creating a request
      DescribeCommunications (..)
    , mkDescribeCommunications
    -- ** Request lenses
    , dCaseId
    , dAfterTime
    , dBeforeTime
    , dMaxResults
    , dNextToken

    -- * Destructuring the response
    , DescribeCommunicationsResponse (..)
    , mkDescribeCommunicationsResponse
    -- ** Response lenses
    , dcrrsCommunications
    , dcrrsNextToken
    , dcrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.Support.Types as Types

-- | /See:/ 'mkDescribeCommunications' smart constructor.
data DescribeCommunications = DescribeCommunications'
  { caseId :: Types.CaseId
    -- ^ The AWS Support case ID requested or returned in the call. The case ID is an alphanumeric string formatted as shown in this example: case-/12345678910-2013-c4c1d2bf33c5cf47/ 
  , afterTime :: Core.Maybe Types.AfterTime
    -- ^ The start date for a filtered date search on support case communications. Case communications are available for 12 months after creation.
  , beforeTime :: Core.Maybe Types.BeforeTime
    -- ^ The end date for a filtered date search on support case communications. Case communications are available for 12 months after creation.
  , maxResults :: Core.Maybe Core.Natural
    -- ^ The maximum number of results to return before paginating.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ A resumption point for pagination.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeCommunications' value with any optional fields omitted.
mkDescribeCommunications
    :: Types.CaseId -- ^ 'caseId'
    -> DescribeCommunications
mkDescribeCommunications caseId
  = DescribeCommunications'{caseId, afterTime = Core.Nothing,
                            beforeTime = Core.Nothing, maxResults = Core.Nothing,
                            nextToken = Core.Nothing}

-- | The AWS Support case ID requested or returned in the call. The case ID is an alphanumeric string formatted as shown in this example: case-/12345678910-2013-c4c1d2bf33c5cf47/ 
--
-- /Note:/ Consider using 'caseId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dCaseId :: Lens.Lens' DescribeCommunications Types.CaseId
dCaseId = Lens.field @"caseId"
{-# INLINEABLE dCaseId #-}
{-# DEPRECATED caseId "Use generic-lens or generic-optics with 'caseId' instead"  #-}

-- | The start date for a filtered date search on support case communications. Case communications are available for 12 months after creation.
--
-- /Note:/ Consider using 'afterTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dAfterTime :: Lens.Lens' DescribeCommunications (Core.Maybe Types.AfterTime)
dAfterTime = Lens.field @"afterTime"
{-# INLINEABLE dAfterTime #-}
{-# DEPRECATED afterTime "Use generic-lens or generic-optics with 'afterTime' instead"  #-}

-- | The end date for a filtered date search on support case communications. Case communications are available for 12 months after creation.
--
-- /Note:/ Consider using 'beforeTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dBeforeTime :: Lens.Lens' DescribeCommunications (Core.Maybe Types.BeforeTime)
dBeforeTime = Lens.field @"beforeTime"
{-# INLINEABLE dBeforeTime #-}
{-# DEPRECATED beforeTime "Use generic-lens or generic-optics with 'beforeTime' instead"  #-}

-- | The maximum number of results to return before paginating.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dMaxResults :: Lens.Lens' DescribeCommunications (Core.Maybe Core.Natural)
dMaxResults = Lens.field @"maxResults"
{-# INLINEABLE dMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | A resumption point for pagination.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dNextToken :: Lens.Lens' DescribeCommunications (Core.Maybe Types.NextToken)
dNextToken = Lens.field @"nextToken"
{-# INLINEABLE dNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery DescribeCommunications where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeCommunications where
        toHeaders DescribeCommunications{..}
          = Core.pure
              ("X-Amz-Target", "AWSSupport_20130415.DescribeCommunications")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribeCommunications where
        toJSON DescribeCommunications{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("caseId" Core..= caseId),
                  ("afterTime" Core..=) Core.<$> afterTime,
                  ("beforeTime" Core..=) Core.<$> beforeTime,
                  ("maxResults" Core..=) Core.<$> maxResults,
                  ("nextToken" Core..=) Core.<$> nextToken])

instance Core.AWSRequest DescribeCommunications where
        type Rs DescribeCommunications = DescribeCommunicationsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeCommunicationsResponse' Core.<$>
                   (x Core..:? "communications") Core.<*> x Core..:? "nextToken"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager DescribeCommunications where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"communications" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | The communications returned by the 'DescribeCommunications' operation.
--
-- /See:/ 'mkDescribeCommunicationsResponse' smart constructor.
data DescribeCommunicationsResponse = DescribeCommunicationsResponse'
  { communications :: Core.Maybe [Types.Communication]
    -- ^ The communications for the case.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ A resumption point for pagination.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeCommunicationsResponse' value with any optional fields omitted.
mkDescribeCommunicationsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeCommunicationsResponse
mkDescribeCommunicationsResponse responseStatus
  = DescribeCommunicationsResponse'{communications = Core.Nothing,
                                    nextToken = Core.Nothing, responseStatus}

-- | The communications for the case.
--
-- /Note:/ Consider using 'communications' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrrsCommunications :: Lens.Lens' DescribeCommunicationsResponse (Core.Maybe [Types.Communication])
dcrrsCommunications = Lens.field @"communications"
{-# INLINEABLE dcrrsCommunications #-}
{-# DEPRECATED communications "Use generic-lens or generic-optics with 'communications' instead"  #-}

-- | A resumption point for pagination.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrrsNextToken :: Lens.Lens' DescribeCommunicationsResponse (Core.Maybe Types.NextToken)
dcrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE dcrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrrsResponseStatus :: Lens.Lens' DescribeCommunicationsResponse Core.Int
dcrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dcrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
