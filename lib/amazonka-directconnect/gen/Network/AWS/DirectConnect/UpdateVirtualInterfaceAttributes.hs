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
    uviaMtu,
    uviaVirtualInterfaceId,

    -- * Destructuring the response
    VirtualInterface (..),
    mkVirtualInterface,

    -- ** Response lenses
    viBgpPeers,
    viVirtualGatewayId,
    viMtu,
    viRouteFilterPrefixes,
    viCustomerAddress,
    viVlan,
    viLocation,
    viAmazonAddress,
    viAddressFamily,
    viVirtualInterfaceState,
    viConnectionId,
    viDirectConnectGatewayId,
    viAmazonSideASN,
    viVirtualInterfaceType,
    viAsn,
    viAuthKey,
    viJumboFrameCapable,
    viCustomerRouterConfig,
    viOwnerAccount,
    viRegion,
    viVirtualInterfaceName,
    viAwsDeviceV2,
    viVirtualInterfaceId,
    viTags,
  )
where

import Network.AWS.DirectConnect.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateVirtualInterfaceAttributes' smart constructor.
data UpdateVirtualInterfaceAttributes = UpdateVirtualInterfaceAttributes'
  { -- | The maximum transmission unit (MTU), in bytes. The supported values are 1500 and 9001. The default value is 1500.
    mtu :: Lude.Maybe Lude.Int,
    -- | The ID of the virtual private interface.
    virtualInterfaceId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateVirtualInterfaceAttributes' with the minimum fields required to make a request.
--
-- * 'mtu' - The maximum transmission unit (MTU), in bytes. The supported values are 1500 and 9001. The default value is 1500.
-- * 'virtualInterfaceId' - The ID of the virtual private interface.
mkUpdateVirtualInterfaceAttributes ::
  -- | 'virtualInterfaceId'
  Lude.Text ->
  UpdateVirtualInterfaceAttributes
mkUpdateVirtualInterfaceAttributes pVirtualInterfaceId_ =
  UpdateVirtualInterfaceAttributes'
    { mtu = Lude.Nothing,
      virtualInterfaceId = pVirtualInterfaceId_
    }

-- | The maximum transmission unit (MTU), in bytes. The supported values are 1500 and 9001. The default value is 1500.
--
-- /Note:/ Consider using 'mtu' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uviaMtu :: Lens.Lens' UpdateVirtualInterfaceAttributes (Lude.Maybe Lude.Int)
uviaMtu = Lens.lens (mtu :: UpdateVirtualInterfaceAttributes -> Lude.Maybe Lude.Int) (\s a -> s {mtu = a} :: UpdateVirtualInterfaceAttributes)
{-# DEPRECATED uviaMtu "Use generic-lens or generic-optics with 'mtu' instead." #-}

-- | The ID of the virtual private interface.
--
-- /Note:/ Consider using 'virtualInterfaceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uviaVirtualInterfaceId :: Lens.Lens' UpdateVirtualInterfaceAttributes Lude.Text
uviaVirtualInterfaceId = Lens.lens (virtualInterfaceId :: UpdateVirtualInterfaceAttributes -> Lude.Text) (\s a -> s {virtualInterfaceId = a} :: UpdateVirtualInterfaceAttributes)
{-# DEPRECATED uviaVirtualInterfaceId "Use generic-lens or generic-optics with 'virtualInterfaceId' instead." #-}

instance Lude.AWSRequest UpdateVirtualInterfaceAttributes where
  type Rs UpdateVirtualInterfaceAttributes = VirtualInterface
  request = Req.postJSON directConnectService
  response = Res.receiveJSON (\s h x -> Lude.eitherParseJSON x)

instance Lude.ToHeaders UpdateVirtualInterfaceAttributes where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "OvertureService.UpdateVirtualInterfaceAttributes" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateVirtualInterfaceAttributes where
  toJSON UpdateVirtualInterfaceAttributes' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("mtu" Lude..=) Lude.<$> mtu,
            Lude.Just ("virtualInterfaceId" Lude..= virtualInterfaceId)
          ]
      )

instance Lude.ToPath UpdateVirtualInterfaceAttributes where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateVirtualInterfaceAttributes where
  toQuery = Lude.const Lude.mempty
