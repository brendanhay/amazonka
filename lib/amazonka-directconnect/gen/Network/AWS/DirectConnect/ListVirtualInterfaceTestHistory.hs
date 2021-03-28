{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectConnect.ListVirtualInterfaceTestHistory
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the virtual interface failover test history.
module Network.AWS.DirectConnect.ListVirtualInterfaceTestHistory
    (
    -- * Creating a request
      ListVirtualInterfaceTestHistory (..)
    , mkListVirtualInterfaceTestHistory
    -- ** Request lenses
    , lvithBgpPeers
    , lvithMaxResults
    , lvithNextToken
    , lvithStatus
    , lvithTestId
    , lvithVirtualInterfaceId

    -- * Destructuring the response
    , ListVirtualInterfaceTestHistoryResponse (..)
    , mkListVirtualInterfaceTestHistoryResponse
    -- ** Response lenses
    , lvithrrsNextToken
    , lvithrrsVirtualInterfaceTestHistory
    , lvithrrsResponseStatus
    ) where

import qualified Network.AWS.DirectConnect.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListVirtualInterfaceTestHistory' smart constructor.
data ListVirtualInterfaceTestHistory = ListVirtualInterfaceTestHistory'
  { bgpPeers :: Core.Maybe [Types.BGPPeerId]
    -- ^ The BGP peers that were placed in the DOWN state during the virtual interface failover test.
  , maxResults :: Core.Maybe Core.Int
    -- ^ The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
--
-- If @MaxResults@ is given a value larger than 100, only 100 results are returned.
  , nextToken :: Core.Maybe Types.PaginationToken
    -- ^ The token for the next page of results.
  , status :: Core.Maybe Types.FailureTestHistoryStatus
    -- ^ The status of the virtual interface failover test.
  , testId :: Core.Maybe Types.TestId
    -- ^ The ID of the virtual interface failover test.
  , virtualInterfaceId :: Core.Maybe Types.VirtualInterfaceId
    -- ^ The ID of the virtual interface that was tested.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListVirtualInterfaceTestHistory' value with any optional fields omitted.
mkListVirtualInterfaceTestHistory
    :: ListVirtualInterfaceTestHistory
mkListVirtualInterfaceTestHistory
  = ListVirtualInterfaceTestHistory'{bgpPeers = Core.Nothing,
                                     maxResults = Core.Nothing, nextToken = Core.Nothing,
                                     status = Core.Nothing, testId = Core.Nothing,
                                     virtualInterfaceId = Core.Nothing}

-- | The BGP peers that were placed in the DOWN state during the virtual interface failover test.
--
-- /Note:/ Consider using 'bgpPeers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lvithBgpPeers :: Lens.Lens' ListVirtualInterfaceTestHistory (Core.Maybe [Types.BGPPeerId])
lvithBgpPeers = Lens.field @"bgpPeers"
{-# INLINEABLE lvithBgpPeers #-}
{-# DEPRECATED bgpPeers "Use generic-lens or generic-optics with 'bgpPeers' instead"  #-}

-- | The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
--
-- If @MaxResults@ is given a value larger than 100, only 100 results are returned.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lvithMaxResults :: Lens.Lens' ListVirtualInterfaceTestHistory (Core.Maybe Core.Int)
lvithMaxResults = Lens.field @"maxResults"
{-# INLINEABLE lvithMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | The token for the next page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lvithNextToken :: Lens.Lens' ListVirtualInterfaceTestHistory (Core.Maybe Types.PaginationToken)
lvithNextToken = Lens.field @"nextToken"
{-# INLINEABLE lvithNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The status of the virtual interface failover test.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lvithStatus :: Lens.Lens' ListVirtualInterfaceTestHistory (Core.Maybe Types.FailureTestHistoryStatus)
lvithStatus = Lens.field @"status"
{-# INLINEABLE lvithStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

-- | The ID of the virtual interface failover test.
--
-- /Note:/ Consider using 'testId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lvithTestId :: Lens.Lens' ListVirtualInterfaceTestHistory (Core.Maybe Types.TestId)
lvithTestId = Lens.field @"testId"
{-# INLINEABLE lvithTestId #-}
{-# DEPRECATED testId "Use generic-lens or generic-optics with 'testId' instead"  #-}

-- | The ID of the virtual interface that was tested.
--
-- /Note:/ Consider using 'virtualInterfaceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lvithVirtualInterfaceId :: Lens.Lens' ListVirtualInterfaceTestHistory (Core.Maybe Types.VirtualInterfaceId)
lvithVirtualInterfaceId = Lens.field @"virtualInterfaceId"
{-# INLINEABLE lvithVirtualInterfaceId #-}
{-# DEPRECATED virtualInterfaceId "Use generic-lens or generic-optics with 'virtualInterfaceId' instead"  #-}

instance Core.ToQuery ListVirtualInterfaceTestHistory where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ListVirtualInterfaceTestHistory where
        toHeaders ListVirtualInterfaceTestHistory{..}
          = Core.pure
              ("X-Amz-Target", "OvertureService.ListVirtualInterfaceTestHistory")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ListVirtualInterfaceTestHistory where
        toJSON ListVirtualInterfaceTestHistory{..}
          = Core.object
              (Core.catMaybes
                 [("bgpPeers" Core..=) Core.<$> bgpPeers,
                  ("maxResults" Core..=) Core.<$> maxResults,
                  ("nextToken" Core..=) Core.<$> nextToken,
                  ("status" Core..=) Core.<$> status,
                  ("testId" Core..=) Core.<$> testId,
                  ("virtualInterfaceId" Core..=) Core.<$> virtualInterfaceId])

instance Core.AWSRequest ListVirtualInterfaceTestHistory where
        type Rs ListVirtualInterfaceTestHistory =
             ListVirtualInterfaceTestHistoryResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListVirtualInterfaceTestHistoryResponse' Core.<$>
                   (x Core..:? "nextToken") Core.<*>
                     x Core..:? "virtualInterfaceTestHistory"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkListVirtualInterfaceTestHistoryResponse' smart constructor.
data ListVirtualInterfaceTestHistoryResponse = ListVirtualInterfaceTestHistoryResponse'
  { nextToken :: Core.Maybe Types.PaginationToken
    -- ^ The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
  , virtualInterfaceTestHistory :: Core.Maybe [Types.VirtualInterfaceTestHistory]
    -- ^ The ID of the tested virtual interface.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ListVirtualInterfaceTestHistoryResponse' value with any optional fields omitted.
mkListVirtualInterfaceTestHistoryResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListVirtualInterfaceTestHistoryResponse
mkListVirtualInterfaceTestHistoryResponse responseStatus
  = ListVirtualInterfaceTestHistoryResponse'{nextToken =
                                               Core.Nothing,
                                             virtualInterfaceTestHistory = Core.Nothing,
                                             responseStatus}

-- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lvithrrsNextToken :: Lens.Lens' ListVirtualInterfaceTestHistoryResponse (Core.Maybe Types.PaginationToken)
lvithrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE lvithrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The ID of the tested virtual interface.
--
-- /Note:/ Consider using 'virtualInterfaceTestHistory' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lvithrrsVirtualInterfaceTestHistory :: Lens.Lens' ListVirtualInterfaceTestHistoryResponse (Core.Maybe [Types.VirtualInterfaceTestHistory])
lvithrrsVirtualInterfaceTestHistory = Lens.field @"virtualInterfaceTestHistory"
{-# INLINEABLE lvithrrsVirtualInterfaceTestHistory #-}
{-# DEPRECATED virtualInterfaceTestHistory "Use generic-lens or generic-optics with 'virtualInterfaceTestHistory' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lvithrrsResponseStatus :: Lens.Lens' ListVirtualInterfaceTestHistoryResponse Core.Int
lvithrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lvithrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
