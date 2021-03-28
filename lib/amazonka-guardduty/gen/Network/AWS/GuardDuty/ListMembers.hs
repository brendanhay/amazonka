{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.ListMembers
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists details about all member accounts for the current GuardDuty master account.
--
-- This operation returns paginated results.
module Network.AWS.GuardDuty.ListMembers
    (
    -- * Creating a request
      ListMembers (..)
    , mkListMembers
    -- ** Request lenses
    , lmDetectorId
    , lmMaxResults
    , lmNextToken
    , lmOnlyAssociated

    -- * Destructuring the response
    , ListMembersResponse (..)
    , mkListMembersResponse
    -- ** Response lenses
    , lmrrsMembers
    , lmrrsNextToken
    , lmrrsResponseStatus
    ) where

import qualified Network.AWS.GuardDuty.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListMembers' smart constructor.
data ListMembers = ListMembers'
  { detectorId :: Types.DetectorId
    -- ^ The unique ID of the detector the member is associated with.
  , maxResults :: Core.Maybe Core.Natural
    -- ^ You can use this parameter to indicate the maximum number of items you want in the response. The default value is 50. The maximum value is 50.
  , nextToken :: Core.Maybe Core.Text
    -- ^ You can use this parameter when paginating results. Set the value of this parameter to null on your first call to the list action. For subsequent calls to the action, fill nextToken in the request with the value of NextToken from the previous response to continue listing data.
  , onlyAssociated :: Core.Maybe Core.Text
    -- ^ Specifies whether to only return associated members or to return all members (including members who haven't been invited yet or have been disassociated).
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListMembers' value with any optional fields omitted.
mkListMembers
    :: Types.DetectorId -- ^ 'detectorId'
    -> ListMembers
mkListMembers detectorId
  = ListMembers'{detectorId, maxResults = Core.Nothing,
                 nextToken = Core.Nothing, onlyAssociated = Core.Nothing}

-- | The unique ID of the detector the member is associated with.
--
-- /Note:/ Consider using 'detectorId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmDetectorId :: Lens.Lens' ListMembers Types.DetectorId
lmDetectorId = Lens.field @"detectorId"
{-# INLINEABLE lmDetectorId #-}
{-# DEPRECATED detectorId "Use generic-lens or generic-optics with 'detectorId' instead"  #-}

-- | You can use this parameter to indicate the maximum number of items you want in the response. The default value is 50. The maximum value is 50.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmMaxResults :: Lens.Lens' ListMembers (Core.Maybe Core.Natural)
lmMaxResults = Lens.field @"maxResults"
{-# INLINEABLE lmMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | You can use this parameter when paginating results. Set the value of this parameter to null on your first call to the list action. For subsequent calls to the action, fill nextToken in the request with the value of NextToken from the previous response to continue listing data.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmNextToken :: Lens.Lens' ListMembers (Core.Maybe Core.Text)
lmNextToken = Lens.field @"nextToken"
{-# INLINEABLE lmNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | Specifies whether to only return associated members or to return all members (including members who haven't been invited yet or have been disassociated).
--
-- /Note:/ Consider using 'onlyAssociated' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmOnlyAssociated :: Lens.Lens' ListMembers (Core.Maybe Core.Text)
lmOnlyAssociated = Lens.field @"onlyAssociated"
{-# INLINEABLE lmOnlyAssociated #-}
{-# DEPRECATED onlyAssociated "Use generic-lens or generic-optics with 'onlyAssociated' instead"  #-}

instance Core.ToQuery ListMembers where
        toQuery ListMembers{..}
          = Core.maybe Core.mempty (Core.toQueryPair "maxResults") maxResults
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "nextToken") nextToken
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "onlyAssociated")
                onlyAssociated

instance Core.ToHeaders ListMembers where
        toHeaders ListMembers{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.AWSRequest ListMembers where
        type Rs ListMembers = ListMembersResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath =
                           "/detector/" Core.<> Core.toText detectorId Core.<> "/member",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListMembersResponse' Core.<$>
                   (x Core..:? "members") Core.<*> x Core..:? "nextToken" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListMembers where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop (rs Lens.^? Lens.field @"members" Core.. Lens._Just) =
            Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkListMembersResponse' smart constructor.
data ListMembersResponse = ListMembersResponse'
  { members :: Core.Maybe [Types.Member]
    -- ^ A list of members.
  , nextToken :: Core.Maybe Core.Text
    -- ^ The pagination parameter to be used on the next list operation to retrieve more items.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListMembersResponse' value with any optional fields omitted.
mkListMembersResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListMembersResponse
mkListMembersResponse responseStatus
  = ListMembersResponse'{members = Core.Nothing,
                         nextToken = Core.Nothing, responseStatus}

-- | A list of members.
--
-- /Note:/ Consider using 'members' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmrrsMembers :: Lens.Lens' ListMembersResponse (Core.Maybe [Types.Member])
lmrrsMembers = Lens.field @"members"
{-# INLINEABLE lmrrsMembers #-}
{-# DEPRECATED members "Use generic-lens or generic-optics with 'members' instead"  #-}

-- | The pagination parameter to be used on the next list operation to retrieve more items.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmrrsNextToken :: Lens.Lens' ListMembersResponse (Core.Maybe Core.Text)
lmrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE lmrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmrrsResponseStatus :: Lens.Lens' ListMembersResponse Core.Int
lmrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lmrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
