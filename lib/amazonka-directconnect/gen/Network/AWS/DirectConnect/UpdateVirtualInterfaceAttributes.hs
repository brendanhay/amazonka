{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectConnect.UpdateVirtualInterfaceAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the specified attributes of the specified virtual private interface.
--
-- Setting the MTU of a virtual interface to 9001 (jumbo frames) can cause an update to the underlying physical connection if it wasn't updated to support jumbo frames. Updating the connection disrupts network connectivity for all virtual interfaces associated with the connection for up to 30 seconds. To check whether your connection supports jumbo frames, call 'DescribeConnections' . To check whether your virtual q interface supports jumbo frames, call 'DescribeVirtualInterfaces' .
module Network.AWS.DirectConnect.UpdateVirtualInterfaceAttributes
  ( -- * Creating a request
    UpdateVirtualInterfaceAttributes (..),
    mkUpdateVirtualInterfaceAttributes,

    -- ** Request lenses
    uviaVirtualInterfaceId,
    uviaMtu,

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

-- | /See:/ 'mkUpdateVirtualInterfaceAttributes' smart constructor.
data UpdateVirtualInterfaceAttributes = UpdateVirtualInterfaceAttributes'
  { -- | The ID of the virtual private interface.
    virtualInterfaceId :: Types.VirtualInterfaceId,
    -- | The maximum transmission unit (MTU), in bytes. The supported values are 1500 and 9001. The default value is 1500.
    mtu :: Core.Maybe Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateVirtualInterfaceAttributes' value with any optional fields omitted.
mkUpdateVirtualInterfaceAttributes ::
  -- | 'virtualInterfaceId'
  Types.VirtualInterfaceId ->
  UpdateVirtualInterfaceAttributes
mkUpdateVirtualInterfaceAttributes virtualInterfaceId =
  UpdateVirtualInterfaceAttributes'
    { virtualInterfaceId,
      mtu = Core.Nothing
    }

-- | The ID of the virtual private interface.
--
-- /Note:/ Consider using 'virtualInterfaceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uviaVirtualInterfaceId :: Lens.Lens' UpdateVirtualInterfaceAttributes Types.VirtualInterfaceId
uviaVirtualInterfaceId = Lens.field @"virtualInterfaceId"
{-# DEPRECATED uviaVirtualInterfaceId "Use generic-lens or generic-optics with 'virtualInterfaceId' instead." #-}

-- | The maximum transmission unit (MTU), in bytes. The supported values are 1500 and 9001. The default value is 1500.
--
-- /Note:/ Consider using 'mtu' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uviaMtu :: Lens.Lens' UpdateVirtualInterfaceAttributes (Core.Maybe Core.Int)
uviaMtu = Lens.field @"mtu"
{-# DEPRECATED uviaMtu "Use generic-lens or generic-optics with 'mtu' instead." #-}

instance Core.FromJSON UpdateVirtualInterfaceAttributes where
  toJSON UpdateVirtualInterfaceAttributes {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("virtualInterfaceId" Core..= virtualInterfaceId),
            ("mtu" Core..=) Core.<$> mtu
          ]
      )

instance Core.AWSRequest UpdateVirtualInterfaceAttributes where
  type Rs UpdateVirtualInterfaceAttributes = Types.VirtualInterface
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "OvertureService.UpdateVirtualInterfaceAttributes"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response = Response.receiveJSON (\s h x -> Core.eitherParseJSON x)
