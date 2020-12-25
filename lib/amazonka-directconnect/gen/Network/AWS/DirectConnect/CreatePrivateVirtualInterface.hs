{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectConnect.CreatePrivateVirtualInterface
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a private virtual interface. A virtual interface is the VLAN that transports AWS Direct Connect traffic. A private virtual interface can be connected to either a Direct Connect gateway or a Virtual Private Gateway (VGW). Connecting the private virtual interface to a Direct Connect gateway enables the possibility for connecting to multiple VPCs, including VPCs in different AWS Regions. Connecting the private virtual interface to a VGW only provides access to a single VPC within the same Region.
--
-- Setting the MTU of a virtual interface to 9001 (jumbo frames) can cause an update to the underlying physical connection if it wasn't updated to support jumbo frames. Updating the connection disrupts network connectivity for all virtual interfaces associated with the connection for up to 30 seconds. To check whether your connection supports jumbo frames, call 'DescribeConnections' . To check whether your virtual interface supports jumbo frames, call 'DescribeVirtualInterfaces' .
module Network.AWS.DirectConnect.CreatePrivateVirtualInterface
  ( -- * Creating a request
    CreatePrivateVirtualInterface (..),
    mkCreatePrivateVirtualInterface,

    -- ** Request lenses
    cpvifConnectionId,
    cpvifNewPrivateVirtualInterface,

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

-- | /See:/ 'mkCreatePrivateVirtualInterface' smart constructor.
data CreatePrivateVirtualInterface = CreatePrivateVirtualInterface'
  { -- | The ID of the connection.
    connectionId :: Types.ConnectionId,
    -- | Information about the private virtual interface.
    newPrivateVirtualInterface :: Types.NewPrivateVirtualInterface
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreatePrivateVirtualInterface' value with any optional fields omitted.
mkCreatePrivateVirtualInterface ::
  -- | 'connectionId'
  Types.ConnectionId ->
  -- | 'newPrivateVirtualInterface'
  Types.NewPrivateVirtualInterface ->
  CreatePrivateVirtualInterface
mkCreatePrivateVirtualInterface
  connectionId
  newPrivateVirtualInterface =
    CreatePrivateVirtualInterface'
      { connectionId,
        newPrivateVirtualInterface
      }

-- | The ID of the connection.
--
-- /Note:/ Consider using 'connectionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpvifConnectionId :: Lens.Lens' CreatePrivateVirtualInterface Types.ConnectionId
cpvifConnectionId = Lens.field @"connectionId"
{-# DEPRECATED cpvifConnectionId "Use generic-lens or generic-optics with 'connectionId' instead." #-}

-- | Information about the private virtual interface.
--
-- /Note:/ Consider using 'newPrivateVirtualInterface' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpvifNewPrivateVirtualInterface :: Lens.Lens' CreatePrivateVirtualInterface Types.NewPrivateVirtualInterface
cpvifNewPrivateVirtualInterface = Lens.field @"newPrivateVirtualInterface"
{-# DEPRECATED cpvifNewPrivateVirtualInterface "Use generic-lens or generic-optics with 'newPrivateVirtualInterface' instead." #-}

instance Core.FromJSON CreatePrivateVirtualInterface where
  toJSON CreatePrivateVirtualInterface {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("connectionId" Core..= connectionId),
            Core.Just
              ("newPrivateVirtualInterface" Core..= newPrivateVirtualInterface)
          ]
      )

instance Core.AWSRequest CreatePrivateVirtualInterface where
  type Rs CreatePrivateVirtualInterface = Types.VirtualInterface
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "OvertureService.CreatePrivateVirtualInterface")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response = Response.receiveJSON (\s h x -> Core.eitherParseJSON x)
