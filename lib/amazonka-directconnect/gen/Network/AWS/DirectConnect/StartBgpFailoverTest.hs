{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectConnect.StartBgpFailoverTest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts the virtual interface failover test that verifies your configuration meets your resiliency requirements by placing the BGP peering session in the DOWN state. You can then send traffic to verify that there are no outages.
--
-- You can run the test on public, private, transit, and hosted virtual interfaces.
-- You can use <https://docs.aws.amazon.com/directconnect/latest/APIReference/API_ListVirtualInterfaceTestHistory.html ListVirtualInterfaceTestHistory> to view the virtual interface test history.
-- If you need to stop the test before the test interval completes, use <https://docs.aws.amazon.com/directconnect/latest/APIReference/API_StopBgpFailoverTest.html StopBgpFailoverTest> .
module Network.AWS.DirectConnect.StartBgpFailoverTest
    (
    -- * Creating a request
      StartBgpFailoverTest (..)
    , mkStartBgpFailoverTest
    -- ** Request lenses
    , sbftVirtualInterfaceId
    , sbftBgpPeers
    , sbftTestDurationInMinutes

    -- * Destructuring the response
    , StartBgpFailoverTestResponse (..)
    , mkStartBgpFailoverTestResponse
    -- ** Response lenses
    , sbftrrsVirtualInterfaceTest
    , sbftrrsResponseStatus
    ) where

import qualified Network.AWS.DirectConnect.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkStartBgpFailoverTest' smart constructor.
data StartBgpFailoverTest = StartBgpFailoverTest'
  { virtualInterfaceId :: Types.VirtualInterfaceId
    -- ^ The ID of the virtual interface you want to test.
  , bgpPeers :: Core.Maybe [Types.BGPPeerId]
    -- ^ The BGP peers to place in the DOWN state.
  , testDurationInMinutes :: Core.Maybe Core.Int
    -- ^ The time in minutes that the virtual interface failover test will last.
--
-- Maximum value: 180 minutes (3 hours).
-- Default: 180 minutes (3 hours).
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StartBgpFailoverTest' value with any optional fields omitted.
mkStartBgpFailoverTest
    :: Types.VirtualInterfaceId -- ^ 'virtualInterfaceId'
    -> StartBgpFailoverTest
mkStartBgpFailoverTest virtualInterfaceId
  = StartBgpFailoverTest'{virtualInterfaceId,
                          bgpPeers = Core.Nothing, testDurationInMinutes = Core.Nothing}

-- | The ID of the virtual interface you want to test.
--
-- /Note:/ Consider using 'virtualInterfaceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbftVirtualInterfaceId :: Lens.Lens' StartBgpFailoverTest Types.VirtualInterfaceId
sbftVirtualInterfaceId = Lens.field @"virtualInterfaceId"
{-# INLINEABLE sbftVirtualInterfaceId #-}
{-# DEPRECATED virtualInterfaceId "Use generic-lens or generic-optics with 'virtualInterfaceId' instead"  #-}

-- | The BGP peers to place in the DOWN state.
--
-- /Note:/ Consider using 'bgpPeers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbftBgpPeers :: Lens.Lens' StartBgpFailoverTest (Core.Maybe [Types.BGPPeerId])
sbftBgpPeers = Lens.field @"bgpPeers"
{-# INLINEABLE sbftBgpPeers #-}
{-# DEPRECATED bgpPeers "Use generic-lens or generic-optics with 'bgpPeers' instead"  #-}

-- | The time in minutes that the virtual interface failover test will last.
--
-- Maximum value: 180 minutes (3 hours).
-- Default: 180 minutes (3 hours).
--
-- /Note:/ Consider using 'testDurationInMinutes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbftTestDurationInMinutes :: Lens.Lens' StartBgpFailoverTest (Core.Maybe Core.Int)
sbftTestDurationInMinutes = Lens.field @"testDurationInMinutes"
{-# INLINEABLE sbftTestDurationInMinutes #-}
{-# DEPRECATED testDurationInMinutes "Use generic-lens or generic-optics with 'testDurationInMinutes' instead"  #-}

instance Core.ToQuery StartBgpFailoverTest where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders StartBgpFailoverTest where
        toHeaders StartBgpFailoverTest{..}
          = Core.pure
              ("X-Amz-Target", "OvertureService.StartBgpFailoverTest")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON StartBgpFailoverTest where
        toJSON StartBgpFailoverTest{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("virtualInterfaceId" Core..= virtualInterfaceId),
                  ("bgpPeers" Core..=) Core.<$> bgpPeers,
                  ("testDurationInMinutes" Core..=) Core.<$> testDurationInMinutes])

instance Core.AWSRequest StartBgpFailoverTest where
        type Rs StartBgpFailoverTest = StartBgpFailoverTestResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 StartBgpFailoverTestResponse' Core.<$>
                   (x Core..:? "virtualInterfaceTest") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkStartBgpFailoverTestResponse' smart constructor.
data StartBgpFailoverTestResponse = StartBgpFailoverTestResponse'
  { virtualInterfaceTest :: Core.Maybe Types.VirtualInterfaceTestHistory
    -- ^ Information about the virtual interface failover test.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'StartBgpFailoverTestResponse' value with any optional fields omitted.
mkStartBgpFailoverTestResponse
    :: Core.Int -- ^ 'responseStatus'
    -> StartBgpFailoverTestResponse
mkStartBgpFailoverTestResponse responseStatus
  = StartBgpFailoverTestResponse'{virtualInterfaceTest =
                                    Core.Nothing,
                                  responseStatus}

-- | Information about the virtual interface failover test.
--
-- /Note:/ Consider using 'virtualInterfaceTest' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbftrrsVirtualInterfaceTest :: Lens.Lens' StartBgpFailoverTestResponse (Core.Maybe Types.VirtualInterfaceTestHistory)
sbftrrsVirtualInterfaceTest = Lens.field @"virtualInterfaceTest"
{-# INLINEABLE sbftrrsVirtualInterfaceTest #-}
{-# DEPRECATED virtualInterfaceTest "Use generic-lens or generic-optics with 'virtualInterfaceTest' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbftrrsResponseStatus :: Lens.Lens' StartBgpFailoverTestResponse Core.Int
sbftrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE sbftrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
