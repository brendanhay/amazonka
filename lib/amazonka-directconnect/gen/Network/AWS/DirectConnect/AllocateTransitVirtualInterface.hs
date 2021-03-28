{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectConnect.AllocateTransitVirtualInterface
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provisions a transit virtual interface to be owned by the specified AWS account. Use this type of interface to connect a transit gateway to your Direct Connect gateway.
--
-- The owner of a connection provisions a transit virtual interface to be owned by the specified AWS account.
-- After you create a transit virtual interface, it must be confirmed by the owner using 'ConfirmTransitVirtualInterface' . Until this step has been completed, the transit virtual interface is in the @requested@ state and is not available to handle traffic.
module Network.AWS.DirectConnect.AllocateTransitVirtualInterface
    (
    -- * Creating a request
      AllocateTransitVirtualInterface (..)
    , mkAllocateTransitVirtualInterface
    -- ** Request lenses
    , atviConnectionId
    , atviOwnerAccount
    , atviNewTransitVirtualInterfaceAllocation

    -- * Destructuring the response
    , AllocateTransitVirtualInterfaceResponse (..)
    , mkAllocateTransitVirtualInterfaceResponse
    -- ** Response lenses
    , atvirrsVirtualInterface
    , atvirrsResponseStatus
    ) where

import qualified Network.AWS.DirectConnect.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkAllocateTransitVirtualInterface' smart constructor.
data AllocateTransitVirtualInterface = AllocateTransitVirtualInterface'
  { connectionId :: Types.ConnectionId
    -- ^ The ID of the connection on which the transit virtual interface is provisioned.
  , ownerAccount :: Types.OwnerAccount
    -- ^ The ID of the AWS account that owns the transit virtual interface.
  , newTransitVirtualInterfaceAllocation :: Types.NewTransitVirtualInterfaceAllocation
    -- ^ Information about the transit virtual interface.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AllocateTransitVirtualInterface' value with any optional fields omitted.
mkAllocateTransitVirtualInterface
    :: Types.ConnectionId -- ^ 'connectionId'
    -> Types.OwnerAccount -- ^ 'ownerAccount'
    -> Types.NewTransitVirtualInterfaceAllocation -- ^ 'newTransitVirtualInterfaceAllocation'
    -> AllocateTransitVirtualInterface
mkAllocateTransitVirtualInterface connectionId ownerAccount
  newTransitVirtualInterfaceAllocation
  = AllocateTransitVirtualInterface'{connectionId, ownerAccount,
                                     newTransitVirtualInterfaceAllocation}

-- | The ID of the connection on which the transit virtual interface is provisioned.
--
-- /Note:/ Consider using 'connectionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atviConnectionId :: Lens.Lens' AllocateTransitVirtualInterface Types.ConnectionId
atviConnectionId = Lens.field @"connectionId"
{-# INLINEABLE atviConnectionId #-}
{-# DEPRECATED connectionId "Use generic-lens or generic-optics with 'connectionId' instead"  #-}

-- | The ID of the AWS account that owns the transit virtual interface.
--
-- /Note:/ Consider using 'ownerAccount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atviOwnerAccount :: Lens.Lens' AllocateTransitVirtualInterface Types.OwnerAccount
atviOwnerAccount = Lens.field @"ownerAccount"
{-# INLINEABLE atviOwnerAccount #-}
{-# DEPRECATED ownerAccount "Use generic-lens or generic-optics with 'ownerAccount' instead"  #-}

-- | Information about the transit virtual interface.
--
-- /Note:/ Consider using 'newTransitVirtualInterfaceAllocation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atviNewTransitVirtualInterfaceAllocation :: Lens.Lens' AllocateTransitVirtualInterface Types.NewTransitVirtualInterfaceAllocation
atviNewTransitVirtualInterfaceAllocation = Lens.field @"newTransitVirtualInterfaceAllocation"
{-# INLINEABLE atviNewTransitVirtualInterfaceAllocation #-}
{-# DEPRECATED newTransitVirtualInterfaceAllocation "Use generic-lens or generic-optics with 'newTransitVirtualInterfaceAllocation' instead"  #-}

instance Core.ToQuery AllocateTransitVirtualInterface where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders AllocateTransitVirtualInterface where
        toHeaders AllocateTransitVirtualInterface{..}
          = Core.pure
              ("X-Amz-Target", "OvertureService.AllocateTransitVirtualInterface")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON AllocateTransitVirtualInterface where
        toJSON AllocateTransitVirtualInterface{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("connectionId" Core..= connectionId),
                  Core.Just ("ownerAccount" Core..= ownerAccount),
                  Core.Just
                    ("newTransitVirtualInterfaceAllocation" Core..=
                       newTransitVirtualInterfaceAllocation)])

instance Core.AWSRequest AllocateTransitVirtualInterface where
        type Rs AllocateTransitVirtualInterface =
             AllocateTransitVirtualInterfaceResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 AllocateTransitVirtualInterfaceResponse' Core.<$>
                   (x Core..:? "virtualInterface") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkAllocateTransitVirtualInterfaceResponse' smart constructor.
data AllocateTransitVirtualInterfaceResponse = AllocateTransitVirtualInterfaceResponse'
  { virtualInterface :: Core.Maybe Types.VirtualInterface
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AllocateTransitVirtualInterfaceResponse' value with any optional fields omitted.
mkAllocateTransitVirtualInterfaceResponse
    :: Core.Int -- ^ 'responseStatus'
    -> AllocateTransitVirtualInterfaceResponse
mkAllocateTransitVirtualInterfaceResponse responseStatus
  = AllocateTransitVirtualInterfaceResponse'{virtualInterface =
                                               Core.Nothing,
                                             responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'virtualInterface' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atvirrsVirtualInterface :: Lens.Lens' AllocateTransitVirtualInterfaceResponse (Core.Maybe Types.VirtualInterface)
atvirrsVirtualInterface = Lens.field @"virtualInterface"
{-# INLINEABLE atvirrsVirtualInterface #-}
{-# DEPRECATED virtualInterface "Use generic-lens or generic-optics with 'virtualInterface' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atvirrsResponseStatus :: Lens.Lens' AllocateTransitVirtualInterfaceResponse Core.Int
atvirrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE atvirrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
