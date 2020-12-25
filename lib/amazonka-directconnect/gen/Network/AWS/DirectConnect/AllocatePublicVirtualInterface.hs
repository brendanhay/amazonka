{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectConnect.AllocatePublicVirtualInterface
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provisions a public virtual interface to be owned by the specified AWS account.
--
-- The owner of a connection calls this function to provision a public virtual interface to be owned by the specified AWS account.
-- Virtual interfaces created using this function must be confirmed by the owner using 'ConfirmPublicVirtualInterface' . Until this step has been completed, the virtual interface is in the @confirming@ state and is not available to handle traffic.
-- When creating an IPv6 public virtual interface, omit the Amazon address and customer address. IPv6 addresses are automatically assigned from the Amazon pool of IPv6 addresses; you cannot specify custom IPv6 addresses.
module Network.AWS.DirectConnect.AllocatePublicVirtualInterface
  ( -- * Creating a request
    AllocatePublicVirtualInterface (..),
    mkAllocatePublicVirtualInterface,

    -- ** Request lenses
    aConnectionId,
    aOwnerAccount,
    aNewPublicVirtualInterfaceAllocation,

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

-- | /See:/ 'mkAllocatePublicVirtualInterface' smart constructor.
data AllocatePublicVirtualInterface = AllocatePublicVirtualInterface'
  { -- | The ID of the connection on which the public virtual interface is provisioned.
    connectionId :: Types.ConnectionId,
    -- | The ID of the AWS account that owns the public virtual interface.
    ownerAccount :: Types.OwnerAccount,
    -- | Information about the public virtual interface.
    newPublicVirtualInterfaceAllocation :: Types.NewPublicVirtualInterfaceAllocation
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AllocatePublicVirtualInterface' value with any optional fields omitted.
mkAllocatePublicVirtualInterface ::
  -- | 'connectionId'
  Types.ConnectionId ->
  -- | 'ownerAccount'
  Types.OwnerAccount ->
  -- | 'newPublicVirtualInterfaceAllocation'
  Types.NewPublicVirtualInterfaceAllocation ->
  AllocatePublicVirtualInterface
mkAllocatePublicVirtualInterface
  connectionId
  ownerAccount
  newPublicVirtualInterfaceAllocation =
    AllocatePublicVirtualInterface'
      { connectionId,
        ownerAccount,
        newPublicVirtualInterfaceAllocation
      }

-- | The ID of the connection on which the public virtual interface is provisioned.
--
-- /Note:/ Consider using 'connectionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aConnectionId :: Lens.Lens' AllocatePublicVirtualInterface Types.ConnectionId
aConnectionId = Lens.field @"connectionId"
{-# DEPRECATED aConnectionId "Use generic-lens or generic-optics with 'connectionId' instead." #-}

-- | The ID of the AWS account that owns the public virtual interface.
--
-- /Note:/ Consider using 'ownerAccount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aOwnerAccount :: Lens.Lens' AllocatePublicVirtualInterface Types.OwnerAccount
aOwnerAccount = Lens.field @"ownerAccount"
{-# DEPRECATED aOwnerAccount "Use generic-lens or generic-optics with 'ownerAccount' instead." #-}

-- | Information about the public virtual interface.
--
-- /Note:/ Consider using 'newPublicVirtualInterfaceAllocation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aNewPublicVirtualInterfaceAllocation :: Lens.Lens' AllocatePublicVirtualInterface Types.NewPublicVirtualInterfaceAllocation
aNewPublicVirtualInterfaceAllocation = Lens.field @"newPublicVirtualInterfaceAllocation"
{-# DEPRECATED aNewPublicVirtualInterfaceAllocation "Use generic-lens or generic-optics with 'newPublicVirtualInterfaceAllocation' instead." #-}

instance Core.FromJSON AllocatePublicVirtualInterface where
  toJSON AllocatePublicVirtualInterface {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("connectionId" Core..= connectionId),
            Core.Just ("ownerAccount" Core..= ownerAccount),
            Core.Just
              ( "newPublicVirtualInterfaceAllocation"
                  Core..= newPublicVirtualInterfaceAllocation
              )
          ]
      )

instance Core.AWSRequest AllocatePublicVirtualInterface where
  type Rs AllocatePublicVirtualInterface = Types.VirtualInterface
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "OvertureService.AllocatePublicVirtualInterface")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response = Response.receiveJSON (\s h x -> Core.eitherParseJSON x)
