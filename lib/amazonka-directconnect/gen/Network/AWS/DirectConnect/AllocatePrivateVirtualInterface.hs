{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectConnect.AllocatePrivateVirtualInterface
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provisions a private virtual interface to be owned by the specified AWS account.
--
-- Virtual interfaces created using this action must be confirmed by the owner using 'ConfirmPrivateVirtualInterface' . Until then, the virtual interface is in the @Confirming@ state and is not available to handle traffic.
module Network.AWS.DirectConnect.AllocatePrivateVirtualInterface
    (
    -- * Creating a request
      AllocatePrivateVirtualInterface (..)
    , mkAllocatePrivateVirtualInterface
    -- ** Request lenses
    , apviConnectionId
    , apviOwnerAccount
    , apviNewPrivateVirtualInterfaceAllocation

     -- * Destructuring the response
    , Types.VirtualInterface (..)
    , Types.mkVirtualInterface
    -- ** Response lenses
    , Types.viAddressFamily
    , Types.viAmazonAddress
    , Types.viAmazonSideAsn
    , Types.viAsn
    , Types.viAuthKey
    , Types.viAwsDeviceV2
    , Types.viBgpPeers
    , Types.viConnectionId
    , Types.viCustomerAddress
    , Types.viCustomerRouterConfig
    , Types.viDirectConnectGatewayId
    , Types.viJumboFrameCapable
    , Types.viLocation
    , Types.viMtu
    , Types.viOwnerAccount
    , Types.viRegion
    , Types.viRouteFilterPrefixes
    , Types.viTags
    , Types.viVirtualGatewayId
    , Types.viVirtualInterfaceId
    , Types.viVirtualInterfaceName
    , Types.viVirtualInterfaceState
    , Types.viVirtualInterfaceType
    , Types.viVlan
    ) where

import qualified Network.AWS.DirectConnect.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkAllocatePrivateVirtualInterface' smart constructor.
data AllocatePrivateVirtualInterface = AllocatePrivateVirtualInterface'
  { connectionId :: Types.ConnectionId
    -- ^ The ID of the connection on which the private virtual interface is provisioned.
  , ownerAccount :: Types.OwnerAccount
    -- ^ The ID of the AWS account that owns the virtual private interface.
  , newPrivateVirtualInterfaceAllocation :: Types.NewPrivateVirtualInterfaceAllocation
    -- ^ Information about the private virtual interface.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AllocatePrivateVirtualInterface' value with any optional fields omitted.
mkAllocatePrivateVirtualInterface
    :: Types.ConnectionId -- ^ 'connectionId'
    -> Types.OwnerAccount -- ^ 'ownerAccount'
    -> Types.NewPrivateVirtualInterfaceAllocation -- ^ 'newPrivateVirtualInterfaceAllocation'
    -> AllocatePrivateVirtualInterface
mkAllocatePrivateVirtualInterface connectionId ownerAccount
  newPrivateVirtualInterfaceAllocation
  = AllocatePrivateVirtualInterface'{connectionId, ownerAccount,
                                     newPrivateVirtualInterfaceAllocation}

-- | The ID of the connection on which the private virtual interface is provisioned.
--
-- /Note:/ Consider using 'connectionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apviConnectionId :: Lens.Lens' AllocatePrivateVirtualInterface Types.ConnectionId
apviConnectionId = Lens.field @"connectionId"
{-# INLINEABLE apviConnectionId #-}
{-# DEPRECATED connectionId "Use generic-lens or generic-optics with 'connectionId' instead"  #-}

-- | The ID of the AWS account that owns the virtual private interface.
--
-- /Note:/ Consider using 'ownerAccount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apviOwnerAccount :: Lens.Lens' AllocatePrivateVirtualInterface Types.OwnerAccount
apviOwnerAccount = Lens.field @"ownerAccount"
{-# INLINEABLE apviOwnerAccount #-}
{-# DEPRECATED ownerAccount "Use generic-lens or generic-optics with 'ownerAccount' instead"  #-}

-- | Information about the private virtual interface.
--
-- /Note:/ Consider using 'newPrivateVirtualInterfaceAllocation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apviNewPrivateVirtualInterfaceAllocation :: Lens.Lens' AllocatePrivateVirtualInterface Types.NewPrivateVirtualInterfaceAllocation
apviNewPrivateVirtualInterfaceAllocation = Lens.field @"newPrivateVirtualInterfaceAllocation"
{-# INLINEABLE apviNewPrivateVirtualInterfaceAllocation #-}
{-# DEPRECATED newPrivateVirtualInterfaceAllocation "Use generic-lens or generic-optics with 'newPrivateVirtualInterfaceAllocation' instead"  #-}

instance Core.ToQuery AllocatePrivateVirtualInterface where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders AllocatePrivateVirtualInterface where
        toHeaders AllocatePrivateVirtualInterface{..}
          = Core.pure
              ("X-Amz-Target", "OvertureService.AllocatePrivateVirtualInterface")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON AllocatePrivateVirtualInterface where
        toJSON AllocatePrivateVirtualInterface{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("connectionId" Core..= connectionId),
                  Core.Just ("ownerAccount" Core..= ownerAccount),
                  Core.Just
                    ("newPrivateVirtualInterfaceAllocation" Core..=
                       newPrivateVirtualInterfaceAllocation)])

instance Core.AWSRequest AllocatePrivateVirtualInterface where
        type Rs AllocatePrivateVirtualInterface = Types.VirtualInterface
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON (\ s h x -> Core.eitherParseJSON x)
        
        {-# INLINE parseResponse #-}
