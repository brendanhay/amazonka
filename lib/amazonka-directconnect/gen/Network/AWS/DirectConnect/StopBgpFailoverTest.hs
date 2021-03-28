{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectConnect.StopBgpFailoverTest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops the virtual interface failover test.
module Network.AWS.DirectConnect.StopBgpFailoverTest
    (
    -- * Creating a request
      StopBgpFailoverTest (..)
    , mkStopBgpFailoverTest
    -- ** Request lenses
    , sVirtualInterfaceId

    -- * Destructuring the response
    , StopBgpFailoverTestResponse (..)
    , mkStopBgpFailoverTestResponse
    -- ** Response lenses
    , srsVirtualInterfaceTest
    , srsResponseStatus
    ) where

import qualified Network.AWS.DirectConnect.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkStopBgpFailoverTest' smart constructor.
newtype StopBgpFailoverTest = StopBgpFailoverTest'
  { virtualInterfaceId :: Types.VirtualInterfaceId
    -- ^ The ID of the virtual interface you no longer want to test.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'StopBgpFailoverTest' value with any optional fields omitted.
mkStopBgpFailoverTest
    :: Types.VirtualInterfaceId -- ^ 'virtualInterfaceId'
    -> StopBgpFailoverTest
mkStopBgpFailoverTest virtualInterfaceId
  = StopBgpFailoverTest'{virtualInterfaceId}

-- | The ID of the virtual interface you no longer want to test.
--
-- /Note:/ Consider using 'virtualInterfaceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sVirtualInterfaceId :: Lens.Lens' StopBgpFailoverTest Types.VirtualInterfaceId
sVirtualInterfaceId = Lens.field @"virtualInterfaceId"
{-# INLINEABLE sVirtualInterfaceId #-}
{-# DEPRECATED virtualInterfaceId "Use generic-lens or generic-optics with 'virtualInterfaceId' instead"  #-}

instance Core.ToQuery StopBgpFailoverTest where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders StopBgpFailoverTest where
        toHeaders StopBgpFailoverTest{..}
          = Core.pure ("X-Amz-Target", "OvertureService.StopBgpFailoverTest")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON StopBgpFailoverTest where
        toJSON StopBgpFailoverTest{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("virtualInterfaceId" Core..= virtualInterfaceId)])

instance Core.AWSRequest StopBgpFailoverTest where
        type Rs StopBgpFailoverTest = StopBgpFailoverTestResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 StopBgpFailoverTestResponse' Core.<$>
                   (x Core..:? "virtualInterfaceTest") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkStopBgpFailoverTestResponse' smart constructor.
data StopBgpFailoverTestResponse = StopBgpFailoverTestResponse'
  { virtualInterfaceTest :: Core.Maybe Types.VirtualInterfaceTestHistory
    -- ^ Information about the virtual interface failover test.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'StopBgpFailoverTestResponse' value with any optional fields omitted.
mkStopBgpFailoverTestResponse
    :: Core.Int -- ^ 'responseStatus'
    -> StopBgpFailoverTestResponse
mkStopBgpFailoverTestResponse responseStatus
  = StopBgpFailoverTestResponse'{virtualInterfaceTest = Core.Nothing,
                                 responseStatus}

-- | Information about the virtual interface failover test.
--
-- /Note:/ Consider using 'virtualInterfaceTest' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srsVirtualInterfaceTest :: Lens.Lens' StopBgpFailoverTestResponse (Core.Maybe Types.VirtualInterfaceTestHistory)
srsVirtualInterfaceTest = Lens.field @"virtualInterfaceTest"
{-# INLINEABLE srsVirtualInterfaceTest #-}
{-# DEPRECATED virtualInterfaceTest "Use generic-lens or generic-optics with 'virtualInterfaceTest' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srsResponseStatus :: Lens.Lens' StopBgpFailoverTestResponse Core.Int
srsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE srsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
