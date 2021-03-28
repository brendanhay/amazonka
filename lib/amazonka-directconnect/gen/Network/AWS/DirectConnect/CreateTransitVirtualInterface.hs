{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectConnect.CreateTransitVirtualInterface
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a transit virtual interface. A transit virtual interface should be used to access one or more transit gateways associated with Direct Connect gateways. A transit virtual interface enables the connection of multiple VPCs attached to a transit gateway to a Direct Connect gateway.
--
-- /Important:/ If you associate your transit gateway with one or more Direct Connect gateways, the Autonomous System Number (ASN) used by the transit gateway and the Direct Connect gateway must be different. For example, if you use the default ASN 64512 for both your the transit gateway and Direct Connect gateway, the association request fails.
-- Setting the MTU of a virtual interface to 8500 (jumbo frames) can cause an update to the underlying physical connection if it wasn't updated to support jumbo frames. Updating the connection disrupts network connectivity for all virtual interfaces associated with the connection for up to 30 seconds. To check whether your connection supports jumbo frames, call 'DescribeConnections' . To check whether your virtual interface supports jumbo frames, call 'DescribeVirtualInterfaces' .
module Network.AWS.DirectConnect.CreateTransitVirtualInterface
    (
    -- * Creating a request
      CreateTransitVirtualInterface (..)
    , mkCreateTransitVirtualInterface
    -- ** Request lenses
    , ctviConnectionId
    , ctviNewTransitVirtualInterface

    -- * Destructuring the response
    , CreateTransitVirtualInterfaceResponse (..)
    , mkCreateTransitVirtualInterfaceResponse
    -- ** Response lenses
    , ctvirfrsVirtualInterface
    , ctvirfrsResponseStatus
    ) where

import qualified Network.AWS.DirectConnect.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateTransitVirtualInterface' smart constructor.
data CreateTransitVirtualInterface = CreateTransitVirtualInterface'
  { connectionId :: Types.ConnectionId
    -- ^ The ID of the connection.
  , newTransitVirtualInterface :: Types.NewTransitVirtualInterface
    -- ^ Information about the transit virtual interface.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateTransitVirtualInterface' value with any optional fields omitted.
mkCreateTransitVirtualInterface
    :: Types.ConnectionId -- ^ 'connectionId'
    -> Types.NewTransitVirtualInterface -- ^ 'newTransitVirtualInterface'
    -> CreateTransitVirtualInterface
mkCreateTransitVirtualInterface connectionId
  newTransitVirtualInterface
  = CreateTransitVirtualInterface'{connectionId,
                                   newTransitVirtualInterface}

-- | The ID of the connection.
--
-- /Note:/ Consider using 'connectionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctviConnectionId :: Lens.Lens' CreateTransitVirtualInterface Types.ConnectionId
ctviConnectionId = Lens.field @"connectionId"
{-# INLINEABLE ctviConnectionId #-}
{-# DEPRECATED connectionId "Use generic-lens or generic-optics with 'connectionId' instead"  #-}

-- | Information about the transit virtual interface.
--
-- /Note:/ Consider using 'newTransitVirtualInterface' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctviNewTransitVirtualInterface :: Lens.Lens' CreateTransitVirtualInterface Types.NewTransitVirtualInterface
ctviNewTransitVirtualInterface = Lens.field @"newTransitVirtualInterface"
{-# INLINEABLE ctviNewTransitVirtualInterface #-}
{-# DEPRECATED newTransitVirtualInterface "Use generic-lens or generic-optics with 'newTransitVirtualInterface' instead"  #-}

instance Core.ToQuery CreateTransitVirtualInterface where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateTransitVirtualInterface where
        toHeaders CreateTransitVirtualInterface{..}
          = Core.pure
              ("X-Amz-Target", "OvertureService.CreateTransitVirtualInterface")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CreateTransitVirtualInterface where
        toJSON CreateTransitVirtualInterface{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("connectionId" Core..= connectionId),
                  Core.Just
                    ("newTransitVirtualInterface" Core..= newTransitVirtualInterface)])

instance Core.AWSRequest CreateTransitVirtualInterface where
        type Rs CreateTransitVirtualInterface =
             CreateTransitVirtualInterfaceResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CreateTransitVirtualInterfaceResponse' Core.<$>
                   (x Core..:? "virtualInterface") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateTransitVirtualInterfaceResponse' smart constructor.
data CreateTransitVirtualInterfaceResponse = CreateTransitVirtualInterfaceResponse'
  { virtualInterface :: Core.Maybe Types.VirtualInterface
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateTransitVirtualInterfaceResponse' value with any optional fields omitted.
mkCreateTransitVirtualInterfaceResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateTransitVirtualInterfaceResponse
mkCreateTransitVirtualInterfaceResponse responseStatus
  = CreateTransitVirtualInterfaceResponse'{virtualInterface =
                                             Core.Nothing,
                                           responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'virtualInterface' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctvirfrsVirtualInterface :: Lens.Lens' CreateTransitVirtualInterfaceResponse (Core.Maybe Types.VirtualInterface)
ctvirfrsVirtualInterface = Lens.field @"virtualInterface"
{-# INLINEABLE ctvirfrsVirtualInterface #-}
{-# DEPRECATED virtualInterface "Use generic-lens or generic-optics with 'virtualInterface' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctvirfrsResponseStatus :: Lens.Lens' CreateTransitVirtualInterfaceResponse Core.Int
ctvirfrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ctvirfrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
