{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    ListVirtualInterfaceTestHistory (..),
    mkListVirtualInterfaceTestHistory,

    -- ** Request lenses
    lvithBgpPeers,
    lvithMaxResults,
    lvithNextToken,
    lvithStatus,
    lvithTestId,
    lvithVirtualInterfaceId,

    -- * Destructuring the response
    ListVirtualInterfaceTestHistoryResponse (..),
    mkListVirtualInterfaceTestHistoryResponse,

    -- ** Response lenses
    lvithrrsNextToken,
    lvithrrsVirtualInterfaceTestHistory,
    lvithrrsResponseStatus,
  )
where

import qualified Network.AWS.DirectConnect.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListVirtualInterfaceTestHistory' smart constructor.
data ListVirtualInterfaceTestHistory = ListVirtualInterfaceTestHistory'
  { -- | The BGP peers that were placed in the DOWN state during the virtual interface failover test.
    bgpPeers :: Core.Maybe [Types.BGPPeerId],
    -- | The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
    --
    -- If @MaxResults@ is given a value larger than 100, only 100 results are returned.
    maxResults :: Core.Maybe Core.Int,
    -- | The token for the next page of results.
    nextToken :: Core.Maybe Types.PaginationToken,
    -- | The status of the virtual interface failover test.
    status :: Core.Maybe Types.FailureTestHistoryStatus,
    -- | The ID of the virtual interface failover test.
    testId :: Core.Maybe Types.TestId,
    -- | The ID of the virtual interface that was tested.
    virtualInterfaceId :: Core.Maybe Types.VirtualInterfaceId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListVirtualInterfaceTestHistory' value with any optional fields omitted.
mkListVirtualInterfaceTestHistory ::
  ListVirtualInterfaceTestHistory
mkListVirtualInterfaceTestHistory =
  ListVirtualInterfaceTestHistory'
    { bgpPeers = Core.Nothing,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing,
      status = Core.Nothing,
      testId = Core.Nothing,
      virtualInterfaceId = Core.Nothing
    }

-- | The BGP peers that were placed in the DOWN state during the virtual interface failover test.
--
-- /Note:/ Consider using 'bgpPeers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lvithBgpPeers :: Lens.Lens' ListVirtualInterfaceTestHistory (Core.Maybe [Types.BGPPeerId])
lvithBgpPeers = Lens.field @"bgpPeers"
{-# DEPRECATED lvithBgpPeers "Use generic-lens or generic-optics with 'bgpPeers' instead." #-}

-- | The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
--
-- If @MaxResults@ is given a value larger than 100, only 100 results are returned.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lvithMaxResults :: Lens.Lens' ListVirtualInterfaceTestHistory (Core.Maybe Core.Int)
lvithMaxResults = Lens.field @"maxResults"
{-# DEPRECATED lvithMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The token for the next page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lvithNextToken :: Lens.Lens' ListVirtualInterfaceTestHistory (Core.Maybe Types.PaginationToken)
lvithNextToken = Lens.field @"nextToken"
{-# DEPRECATED lvithNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The status of the virtual interface failover test.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lvithStatus :: Lens.Lens' ListVirtualInterfaceTestHistory (Core.Maybe Types.FailureTestHistoryStatus)
lvithStatus = Lens.field @"status"
{-# DEPRECATED lvithStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The ID of the virtual interface failover test.
--
-- /Note:/ Consider using 'testId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lvithTestId :: Lens.Lens' ListVirtualInterfaceTestHistory (Core.Maybe Types.TestId)
lvithTestId = Lens.field @"testId"
{-# DEPRECATED lvithTestId "Use generic-lens or generic-optics with 'testId' instead." #-}

-- | The ID of the virtual interface that was tested.
--
-- /Note:/ Consider using 'virtualInterfaceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lvithVirtualInterfaceId :: Lens.Lens' ListVirtualInterfaceTestHistory (Core.Maybe Types.VirtualInterfaceId)
lvithVirtualInterfaceId = Lens.field @"virtualInterfaceId"
{-# DEPRECATED lvithVirtualInterfaceId "Use generic-lens or generic-optics with 'virtualInterfaceId' instead." #-}

instance Core.FromJSON ListVirtualInterfaceTestHistory where
  toJSON ListVirtualInterfaceTestHistory {..} =
    Core.object
      ( Core.catMaybes
          [ ("bgpPeers" Core..=) Core.<$> bgpPeers,
            ("maxResults" Core..=) Core.<$> maxResults,
            ("nextToken" Core..=) Core.<$> nextToken,
            ("status" Core..=) Core.<$> status,
            ("testId" Core..=) Core.<$> testId,
            ("virtualInterfaceId" Core..=) Core.<$> virtualInterfaceId
          ]
      )

instance Core.AWSRequest ListVirtualInterfaceTestHistory where
  type
    Rs ListVirtualInterfaceTestHistory =
      ListVirtualInterfaceTestHistoryResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "OvertureService.ListVirtualInterfaceTestHistory")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListVirtualInterfaceTestHistoryResponse'
            Core.<$> (x Core..:? "nextToken")
            Core.<*> (x Core..:? "virtualInterfaceTestHistory")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkListVirtualInterfaceTestHistoryResponse' smart constructor.
data ListVirtualInterfaceTestHistoryResponse = ListVirtualInterfaceTestHistoryResponse'
  { -- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
    nextToken :: Core.Maybe Types.PaginationToken,
    -- | The ID of the tested virtual interface.
    virtualInterfaceTestHistory :: Core.Maybe [Types.VirtualInterfaceTestHistory],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ListVirtualInterfaceTestHistoryResponse' value with any optional fields omitted.
mkListVirtualInterfaceTestHistoryResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListVirtualInterfaceTestHistoryResponse
mkListVirtualInterfaceTestHistoryResponse responseStatus =
  ListVirtualInterfaceTestHistoryResponse'
    { nextToken =
        Core.Nothing,
      virtualInterfaceTestHistory = Core.Nothing,
      responseStatus
    }

-- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lvithrrsNextToken :: Lens.Lens' ListVirtualInterfaceTestHistoryResponse (Core.Maybe Types.PaginationToken)
lvithrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED lvithrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The ID of the tested virtual interface.
--
-- /Note:/ Consider using 'virtualInterfaceTestHistory' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lvithrrsVirtualInterfaceTestHistory :: Lens.Lens' ListVirtualInterfaceTestHistoryResponse (Core.Maybe [Types.VirtualInterfaceTestHistory])
lvithrrsVirtualInterfaceTestHistory = Lens.field @"virtualInterfaceTestHistory"
{-# DEPRECATED lvithrrsVirtualInterfaceTestHistory "Use generic-lens or generic-optics with 'virtualInterfaceTestHistory' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lvithrrsResponseStatus :: Lens.Lens' ListVirtualInterfaceTestHistoryResponse Core.Int
lvithrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED lvithrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
