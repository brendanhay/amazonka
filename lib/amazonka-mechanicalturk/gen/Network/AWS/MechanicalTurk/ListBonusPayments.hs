{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MechanicalTurk.ListBonusPayments
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The @ListBonusPayments@ operation retrieves the amounts of bonuses you have paid to Workers for a given HIT or assignment. 
--
-- This operation returns paginated results.
module Network.AWS.MechanicalTurk.ListBonusPayments
    (
    -- * Creating a request
      ListBonusPayments (..)
    , mkListBonusPayments
    -- ** Request lenses
    , lbpAssignmentId
    , lbpHITId
    , lbpMaxResults
    , lbpNextToken

    -- * Destructuring the response
    , ListBonusPaymentsResponse (..)
    , mkListBonusPaymentsResponse
    -- ** Response lenses
    , lbprrsBonusPayments
    , lbprrsNextToken
    , lbprrsNumResults
    , lbprrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MechanicalTurk.Types as Types
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListBonusPayments' smart constructor.
data ListBonusPayments = ListBonusPayments'
  { assignmentId :: Core.Maybe Types.AssignmentId
    -- ^ The ID of the assignment associated with the bonus payments to retrieve. If specified, only bonus payments for the given assignment are returned. Either the HITId parameter or the AssignmentId parameter must be specified
  , hITId :: Core.Maybe Types.HITId
    -- ^ The ID of the HIT associated with the bonus payments to retrieve. If not specified, all bonus payments for all assignments for the given HIT are returned. Either the HITId parameter or the AssignmentId parameter must be specified
  , maxResults :: Core.Maybe Core.Natural
  , nextToken :: Core.Maybe Types.PaginationToken
    -- ^ Pagination token
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListBonusPayments' value with any optional fields omitted.
mkListBonusPayments
    :: ListBonusPayments
mkListBonusPayments
  = ListBonusPayments'{assignmentId = Core.Nothing,
                       hITId = Core.Nothing, maxResults = Core.Nothing,
                       nextToken = Core.Nothing}

-- | The ID of the assignment associated with the bonus payments to retrieve. If specified, only bonus payments for the given assignment are returned. Either the HITId parameter or the AssignmentId parameter must be specified
--
-- /Note:/ Consider using 'assignmentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbpAssignmentId :: Lens.Lens' ListBonusPayments (Core.Maybe Types.AssignmentId)
lbpAssignmentId = Lens.field @"assignmentId"
{-# INLINEABLE lbpAssignmentId #-}
{-# DEPRECATED assignmentId "Use generic-lens or generic-optics with 'assignmentId' instead"  #-}

-- | The ID of the HIT associated with the bonus payments to retrieve. If not specified, all bonus payments for all assignments for the given HIT are returned. Either the HITId parameter or the AssignmentId parameter must be specified
--
-- /Note:/ Consider using 'hITId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbpHITId :: Lens.Lens' ListBonusPayments (Core.Maybe Types.HITId)
lbpHITId = Lens.field @"hITId"
{-# INLINEABLE lbpHITId #-}
{-# DEPRECATED hITId "Use generic-lens or generic-optics with 'hITId' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbpMaxResults :: Lens.Lens' ListBonusPayments (Core.Maybe Core.Natural)
lbpMaxResults = Lens.field @"maxResults"
{-# INLINEABLE lbpMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | Pagination token
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbpNextToken :: Lens.Lens' ListBonusPayments (Core.Maybe Types.PaginationToken)
lbpNextToken = Lens.field @"nextToken"
{-# INLINEABLE lbpNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery ListBonusPayments where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ListBonusPayments where
        toHeaders ListBonusPayments{..}
          = Core.pure
              ("X-Amz-Target",
               "MTurkRequesterServiceV20170117.ListBonusPayments")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ListBonusPayments where
        toJSON ListBonusPayments{..}
          = Core.object
              (Core.catMaybes
                 [("AssignmentId" Core..=) Core.<$> assignmentId,
                  ("HITId" Core..=) Core.<$> hITId,
                  ("MaxResults" Core..=) Core.<$> maxResults,
                  ("NextToken" Core..=) Core.<$> nextToken])

instance Core.AWSRequest ListBonusPayments where
        type Rs ListBonusPayments = ListBonusPaymentsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListBonusPaymentsResponse' Core.<$>
                   (x Core..:? "BonusPayments") Core.<*> x Core..:? "NextToken"
                     Core.<*> x Core..:? "NumResults"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListBonusPayments where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"bonusPayments" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkListBonusPaymentsResponse' smart constructor.
data ListBonusPaymentsResponse = ListBonusPaymentsResponse'
  { bonusPayments :: Core.Maybe [Types.BonusPayment]
    -- ^ A successful request to the ListBonusPayments operation returns a list of BonusPayment objects. 
  , nextToken :: Core.Maybe Types.PaginationToken
  , numResults :: Core.Maybe Core.Int
    -- ^ The number of bonus payments on this page in the filtered results list, equivalent to the number of bonus payments being returned by this call. 
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ListBonusPaymentsResponse' value with any optional fields omitted.
mkListBonusPaymentsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListBonusPaymentsResponse
mkListBonusPaymentsResponse responseStatus
  = ListBonusPaymentsResponse'{bonusPayments = Core.Nothing,
                               nextToken = Core.Nothing, numResults = Core.Nothing,
                               responseStatus}

-- | A successful request to the ListBonusPayments operation returns a list of BonusPayment objects. 
--
-- /Note:/ Consider using 'bonusPayments' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbprrsBonusPayments :: Lens.Lens' ListBonusPaymentsResponse (Core.Maybe [Types.BonusPayment])
lbprrsBonusPayments = Lens.field @"bonusPayments"
{-# INLINEABLE lbprrsBonusPayments #-}
{-# DEPRECATED bonusPayments "Use generic-lens or generic-optics with 'bonusPayments' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbprrsNextToken :: Lens.Lens' ListBonusPaymentsResponse (Core.Maybe Types.PaginationToken)
lbprrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE lbprrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The number of bonus payments on this page in the filtered results list, equivalent to the number of bonus payments being returned by this call. 
--
-- /Note:/ Consider using 'numResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbprrsNumResults :: Lens.Lens' ListBonusPaymentsResponse (Core.Maybe Core.Int)
lbprrsNumResults = Lens.field @"numResults"
{-# INLINEABLE lbprrsNumResults #-}
{-# DEPRECATED numResults "Use generic-lens or generic-optics with 'numResults' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbprrsResponseStatus :: Lens.Lens' ListBonusPaymentsResponse Core.Int
lbprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lbprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
