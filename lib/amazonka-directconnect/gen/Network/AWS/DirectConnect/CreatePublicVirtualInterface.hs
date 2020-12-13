{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectConnect.CreatePublicVirtualInterface
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a public virtual interface. A virtual interface is the VLAN that transports AWS Direct Connect traffic. A public virtual interface supports sending traffic to public services of AWS such as Amazon S3.
--
-- When creating an IPv6 public virtual interface (@addressFamily@ is @ipv6@ ), leave the @customer@ and @amazon@ address fields blank to use auto-assigned IPv6 space. Custom IPv6 addresses are not supported.
module Network.AWS.DirectConnect.CreatePublicVirtualInterface
  ( -- * Creating a request
    CreatePublicVirtualInterface (..),
    mkCreatePublicVirtualInterface,

    -- ** Request lenses
    cpviConnectionId,
    cpviNewPublicVirtualInterface,

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

-- | /See:/ 'mkCreatePublicVirtualInterface' smart constructor.
data CreatePublicVirtualInterface = CreatePublicVirtualInterface'
  { -- | The ID of the connection.
    connectionId :: Lude.Text,
    -- | Information about the public virtual interface.
    newPublicVirtualInterface :: NewPublicVirtualInterface
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreatePublicVirtualInterface' with the minimum fields required to make a request.
--
-- * 'connectionId' - The ID of the connection.
-- * 'newPublicVirtualInterface' - Information about the public virtual interface.
mkCreatePublicVirtualInterface ::
  -- | 'connectionId'
  Lude.Text ->
  -- | 'newPublicVirtualInterface'
  NewPublicVirtualInterface ->
  CreatePublicVirtualInterface
mkCreatePublicVirtualInterface
  pConnectionId_
  pNewPublicVirtualInterface_ =
    CreatePublicVirtualInterface'
      { connectionId = pConnectionId_,
        newPublicVirtualInterface = pNewPublicVirtualInterface_
      }

-- | The ID of the connection.
--
-- /Note:/ Consider using 'connectionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpviConnectionId :: Lens.Lens' CreatePublicVirtualInterface Lude.Text
cpviConnectionId = Lens.lens (connectionId :: CreatePublicVirtualInterface -> Lude.Text) (\s a -> s {connectionId = a} :: CreatePublicVirtualInterface)
{-# DEPRECATED cpviConnectionId "Use generic-lens or generic-optics with 'connectionId' instead." #-}

-- | Information about the public virtual interface.
--
-- /Note:/ Consider using 'newPublicVirtualInterface' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpviNewPublicVirtualInterface :: Lens.Lens' CreatePublicVirtualInterface NewPublicVirtualInterface
cpviNewPublicVirtualInterface = Lens.lens (newPublicVirtualInterface :: CreatePublicVirtualInterface -> NewPublicVirtualInterface) (\s a -> s {newPublicVirtualInterface = a} :: CreatePublicVirtualInterface)
{-# DEPRECATED cpviNewPublicVirtualInterface "Use generic-lens or generic-optics with 'newPublicVirtualInterface' instead." #-}

instance Lude.AWSRequest CreatePublicVirtualInterface where
  type Rs CreatePublicVirtualInterface = VirtualInterface
  request = Req.postJSON directConnectService
  response = Res.receiveJSON (\s h x -> Lude.eitherParseJSON x)

instance Lude.ToHeaders CreatePublicVirtualInterface where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "OvertureService.CreatePublicVirtualInterface" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreatePublicVirtualInterface where
  toJSON CreatePublicVirtualInterface' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("connectionId" Lude..= connectionId),
            Lude.Just
              ("newPublicVirtualInterface" Lude..= newPublicVirtualInterface)
          ]
      )

instance Lude.ToPath CreatePublicVirtualInterface where
  toPath = Lude.const "/"

instance Lude.ToQuery CreatePublicVirtualInterface where
  toQuery = Lude.const Lude.mempty
