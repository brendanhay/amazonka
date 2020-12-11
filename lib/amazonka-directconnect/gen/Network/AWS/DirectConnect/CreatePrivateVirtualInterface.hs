{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    creConnectionId,
    creNewPrivateVirtualInterface,

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

-- | /See:/ 'mkCreatePrivateVirtualInterface' smart constructor.
data CreatePrivateVirtualInterface = CreatePrivateVirtualInterface'
  { connectionId ::
      Lude.Text,
    newPrivateVirtualInterface ::
      NewPrivateVirtualInterface
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreatePrivateVirtualInterface' with the minimum fields required to make a request.
--
-- * 'connectionId' - The ID of the connection.
-- * 'newPrivateVirtualInterface' - Information about the private virtual interface.
mkCreatePrivateVirtualInterface ::
  -- | 'connectionId'
  Lude.Text ->
  -- | 'newPrivateVirtualInterface'
  NewPrivateVirtualInterface ->
  CreatePrivateVirtualInterface
mkCreatePrivateVirtualInterface
  pConnectionId_
  pNewPrivateVirtualInterface_ =
    CreatePrivateVirtualInterface'
      { connectionId = pConnectionId_,
        newPrivateVirtualInterface = pNewPrivateVirtualInterface_
      }

-- | The ID of the connection.
--
-- /Note:/ Consider using 'connectionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
creConnectionId :: Lens.Lens' CreatePrivateVirtualInterface Lude.Text
creConnectionId = Lens.lens (connectionId :: CreatePrivateVirtualInterface -> Lude.Text) (\s a -> s {connectionId = a} :: CreatePrivateVirtualInterface)
{-# DEPRECATED creConnectionId "Use generic-lens or generic-optics with 'connectionId' instead." #-}

-- | Information about the private virtual interface.
--
-- /Note:/ Consider using 'newPrivateVirtualInterface' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
creNewPrivateVirtualInterface :: Lens.Lens' CreatePrivateVirtualInterface NewPrivateVirtualInterface
creNewPrivateVirtualInterface = Lens.lens (newPrivateVirtualInterface :: CreatePrivateVirtualInterface -> NewPrivateVirtualInterface) (\s a -> s {newPrivateVirtualInterface = a} :: CreatePrivateVirtualInterface)
{-# DEPRECATED creNewPrivateVirtualInterface "Use generic-lens or generic-optics with 'newPrivateVirtualInterface' instead." #-}

instance Lude.AWSRequest CreatePrivateVirtualInterface where
  type Rs CreatePrivateVirtualInterface = VirtualInterface
  request = Req.postJSON directConnectService
  response = Res.receiveJSON (\s h x -> Lude.eitherParseJSON x)

instance Lude.ToHeaders CreatePrivateVirtualInterface where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "OvertureService.CreatePrivateVirtualInterface" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreatePrivateVirtualInterface where
  toJSON CreatePrivateVirtualInterface' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("connectionId" Lude..= connectionId),
            Lude.Just
              ("newPrivateVirtualInterface" Lude..= newPrivateVirtualInterface)
          ]
      )

instance Lude.ToPath CreatePrivateVirtualInterface where
  toPath = Lude.const "/"

instance Lude.ToQuery CreatePrivateVirtualInterface where
  toQuery = Lude.const Lude.mempty
