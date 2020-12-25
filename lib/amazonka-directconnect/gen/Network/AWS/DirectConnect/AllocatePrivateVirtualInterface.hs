{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    AllocatePrivateVirtualInterface (..),
    mkAllocatePrivateVirtualInterface,

    -- ** Request lenses
    apviConnectionId,
    apviOwnerAccount,
    apviNewPrivateVirtualInterfaceAllocation,

    -- * Destructuring the response
    Types.VirtualInterface (..),
    Types.mkVirtualInterface,

    -- ** Response lenses
    Types.viAddressFamily,
    Types.viAmazonAddress,
    Types.viAmazonSideAsn,
    Types.viAsn,
    Types.viAuthKey,
    Types.viAwsDeviceV2,
    Types.viBgpPeers,
    Types.viConnectionId,
    Types.viCustomerAddress,
    Types.viCustomerRouterConfig,
    Types.viDirectConnectGatewayId,
    Types.viJumboFrameCapable,
    Types.viLocation,
    Types.viMtu,
    Types.viOwnerAccount,
    Types.viRegion,
    Types.viRouteFilterPrefixes,
    Types.viTags,
    Types.viVirtualGatewayId,
    Types.viVirtualInterfaceId,
    Types.viVirtualInterfaceName,
    Types.viVirtualInterfaceState,
    Types.viVirtualInterfaceType,
    Types.viVlan,
  )
where

import qualified Network.AWS.DirectConnect.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkAllocatePrivateVirtualInterface' smart constructor.
data AllocatePrivateVirtualInterface = AllocatePrivateVirtualInterface'
  { -- | The ID of the connection on which the private virtual interface is provisioned.
    connectionId :: Types.ConnectionId,
    -- | The ID of the AWS account that owns the virtual private interface.
    ownerAccount :: Types.OwnerAccount,
    -- | Information about the private virtual interface.
    newPrivateVirtualInterfaceAllocation :: Types.NewPrivateVirtualInterfaceAllocation
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AllocatePrivateVirtualInterface' value with any optional fields omitted.
mkAllocatePrivateVirtualInterface ::
  -- | 'connectionId'
  Types.ConnectionId ->
  -- | 'ownerAccount'
  Types.OwnerAccount ->
  -- | 'newPrivateVirtualInterfaceAllocation'
  Types.NewPrivateVirtualInterfaceAllocation ->
  AllocatePrivateVirtualInterface
mkAllocatePrivateVirtualInterface
  connectionId
  ownerAccount
  newPrivateVirtualInterfaceAllocation =
    AllocatePrivateVirtualInterface'
      { connectionId,
        ownerAccount,
        newPrivateVirtualInterfaceAllocation
      }

-- | The ID of the connection on which the private virtual interface is provisioned.
--
-- /Note:/ Consider using 'connectionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apviConnectionId :: Lens.Lens' AllocatePrivateVirtualInterface Types.ConnectionId
apviConnectionId = Lens.field @"connectionId"
{-# DEPRECATED apviConnectionId "Use generic-lens or generic-optics with 'connectionId' instead." #-}

-- | The ID of the AWS account that owns the virtual private interface.
--
-- /Note:/ Consider using 'ownerAccount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apviOwnerAccount :: Lens.Lens' AllocatePrivateVirtualInterface Types.OwnerAccount
apviOwnerAccount = Lens.field @"ownerAccount"
{-# DEPRECATED apviOwnerAccount "Use generic-lens or generic-optics with 'ownerAccount' instead." #-}

-- | Information about the private virtual interface.
--
-- /Note:/ Consider using 'newPrivateVirtualInterfaceAllocation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apviNewPrivateVirtualInterfaceAllocation :: Lens.Lens' AllocatePrivateVirtualInterface Types.NewPrivateVirtualInterfaceAllocation
apviNewPrivateVirtualInterfaceAllocation = Lens.field @"newPrivateVirtualInterfaceAllocation"
{-# DEPRECATED apviNewPrivateVirtualInterfaceAllocation "Use generic-lens or generic-optics with 'newPrivateVirtualInterfaceAllocation' instead." #-}

instance Core.FromJSON AllocatePrivateVirtualInterface where
  toJSON AllocatePrivateVirtualInterface {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("connectionId" Core..= connectionId),
            Core.Just ("ownerAccount" Core..= ownerAccount),
            Core.Just
              ( "newPrivateVirtualInterfaceAllocation"
                  Core..= newPrivateVirtualInterfaceAllocation
              )
          ]
      )

instance Core.AWSRequest AllocatePrivateVirtualInterface where
  type Rs AllocatePrivateVirtualInterface = Types.VirtualInterface
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "OvertureService.AllocatePrivateVirtualInterface")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response = Response.receiveJSON (\s h x -> Core.eitherParseJSON x)
