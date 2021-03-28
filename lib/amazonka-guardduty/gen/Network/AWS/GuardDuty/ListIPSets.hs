{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.ListIPSets
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the IPSets of the GuardDuty service specified by the detector ID. If you use this operation from a member account, the IPSets returned are the IPSets from the associated master account.
--
-- This operation returns paginated results.
module Network.AWS.GuardDuty.ListIPSets
    (
    -- * Creating a request
      ListIPSets (..)
    , mkListIPSets
    -- ** Request lenses
    , lipsDetectorId
    , lipsMaxResults
    , lipsNextToken

    -- * Destructuring the response
    , ListIPSetsResponse (..)
    , mkListIPSetsResponse
    -- ** Response lenses
    , lipsrrsIpSetIds
    , lipsrrsNextToken
    , lipsrrsResponseStatus
    ) where

import qualified Network.AWS.GuardDuty.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListIPSets' smart constructor.
data ListIPSets = ListIPSets'
  { detectorId :: Types.DetectorId
    -- ^ The unique ID of the detector that the IPSet is associated with.
  , maxResults :: Core.Maybe Core.Natural
    -- ^ You can use this parameter to indicate the maximum number of items you want in the response. The default value is 50. The maximum value is 50.
  , nextToken :: Core.Maybe Core.Text
    -- ^ You can use this parameter when paginating results. Set the value of this parameter to null on your first call to the list action. For subsequent calls to the action, fill nextToken in the request with the value of NextToken from the previous response to continue listing data.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListIPSets' value with any optional fields omitted.
mkListIPSets
    :: Types.DetectorId -- ^ 'detectorId'
    -> ListIPSets
mkListIPSets detectorId
  = ListIPSets'{detectorId, maxResults = Core.Nothing,
                nextToken = Core.Nothing}

-- | The unique ID of the detector that the IPSet is associated with.
--
-- /Note:/ Consider using 'detectorId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lipsDetectorId :: Lens.Lens' ListIPSets Types.DetectorId
lipsDetectorId = Lens.field @"detectorId"
{-# INLINEABLE lipsDetectorId #-}
{-# DEPRECATED detectorId "Use generic-lens or generic-optics with 'detectorId' instead"  #-}

-- | You can use this parameter to indicate the maximum number of items you want in the response. The default value is 50. The maximum value is 50.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lipsMaxResults :: Lens.Lens' ListIPSets (Core.Maybe Core.Natural)
lipsMaxResults = Lens.field @"maxResults"
{-# INLINEABLE lipsMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | You can use this parameter when paginating results. Set the value of this parameter to null on your first call to the list action. For subsequent calls to the action, fill nextToken in the request with the value of NextToken from the previous response to continue listing data.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lipsNextToken :: Lens.Lens' ListIPSets (Core.Maybe Core.Text)
lipsNextToken = Lens.field @"nextToken"
{-# INLINEABLE lipsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery ListIPSets where
        toQuery ListIPSets{..}
          = Core.maybe Core.mempty (Core.toQueryPair "maxResults") maxResults
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "nextToken") nextToken

instance Core.ToHeaders ListIPSets where
        toHeaders ListIPSets{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.AWSRequest ListIPSets where
        type Rs ListIPSets = ListIPSetsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath =
                           "/detector/" Core.<> Core.toText detectorId Core.<> "/ipset",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListIPSetsResponse' Core.<$>
                   (x Core..:? "ipSetIds" Core..!= Core.mempty) Core.<*>
                     x Core..:? "nextToken"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListIPSets where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop (rs Lens.^. Lens.field @"ipSetIds") = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkListIPSetsResponse' smart constructor.
data ListIPSetsResponse = ListIPSetsResponse'
  { ipSetIds :: [Core.Text]
    -- ^ The IDs of the IPSet resources.
  , nextToken :: Core.Maybe Core.Text
    -- ^ The pagination parameter to be used on the next list operation to retrieve more items.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListIPSetsResponse' value with any optional fields omitted.
mkListIPSetsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListIPSetsResponse
mkListIPSetsResponse responseStatus
  = ListIPSetsResponse'{ipSetIds = Core.mempty,
                        nextToken = Core.Nothing, responseStatus}

-- | The IDs of the IPSet resources.
--
-- /Note:/ Consider using 'ipSetIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lipsrrsIpSetIds :: Lens.Lens' ListIPSetsResponse [Core.Text]
lipsrrsIpSetIds = Lens.field @"ipSetIds"
{-# INLINEABLE lipsrrsIpSetIds #-}
{-# DEPRECATED ipSetIds "Use generic-lens or generic-optics with 'ipSetIds' instead"  #-}

-- | The pagination parameter to be used on the next list operation to retrieve more items.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lipsrrsNextToken :: Lens.Lens' ListIPSetsResponse (Core.Maybe Core.Text)
lipsrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE lipsrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lipsrrsResponseStatus :: Lens.Lens' ListIPSetsResponse Core.Int
lipsrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lipsrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
