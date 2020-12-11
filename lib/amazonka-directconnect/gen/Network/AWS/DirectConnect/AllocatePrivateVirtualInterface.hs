{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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

-- | /See:/ 'mkAllocatePrivateVirtualInterface' smart constructor.
data AllocatePrivateVirtualInterface = AllocatePrivateVirtualInterface'
  { connectionId ::
      Lude.Text,
    ownerAccount :: Lude.Text,
    newPrivateVirtualInterfaceAllocation ::
      NewPrivateVirtualInterfaceAllocation
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AllocatePrivateVirtualInterface' with the minimum fields required to make a request.
--
-- * 'connectionId' - The ID of the connection on which the private virtual interface is provisioned.
-- * 'newPrivateVirtualInterfaceAllocation' - Information about the private virtual interface.
-- * 'ownerAccount' - The ID of the AWS account that owns the virtual private interface.
mkAllocatePrivateVirtualInterface ::
  -- | 'connectionId'
  Lude.Text ->
  -- | 'ownerAccount'
  Lude.Text ->
  -- | 'newPrivateVirtualInterfaceAllocation'
  NewPrivateVirtualInterfaceAllocation ->
  AllocatePrivateVirtualInterface
mkAllocatePrivateVirtualInterface
  pConnectionId_
  pOwnerAccount_
  pNewPrivateVirtualInterfaceAllocation_ =
    AllocatePrivateVirtualInterface'
      { connectionId = pConnectionId_,
        ownerAccount = pOwnerAccount_,
        newPrivateVirtualInterfaceAllocation =
          pNewPrivateVirtualInterfaceAllocation_
      }

-- | The ID of the connection on which the private virtual interface is provisioned.
--
-- /Note:/ Consider using 'connectionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apviConnectionId :: Lens.Lens' AllocatePrivateVirtualInterface Lude.Text
apviConnectionId = Lens.lens (connectionId :: AllocatePrivateVirtualInterface -> Lude.Text) (\s a -> s {connectionId = a} :: AllocatePrivateVirtualInterface)
{-# DEPRECATED apviConnectionId "Use generic-lens or generic-optics with 'connectionId' instead." #-}

-- | The ID of the AWS account that owns the virtual private interface.
--
-- /Note:/ Consider using 'ownerAccount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apviOwnerAccount :: Lens.Lens' AllocatePrivateVirtualInterface Lude.Text
apviOwnerAccount = Lens.lens (ownerAccount :: AllocatePrivateVirtualInterface -> Lude.Text) (\s a -> s {ownerAccount = a} :: AllocatePrivateVirtualInterface)
{-# DEPRECATED apviOwnerAccount "Use generic-lens or generic-optics with 'ownerAccount' instead." #-}

-- | Information about the private virtual interface.
--
-- /Note:/ Consider using 'newPrivateVirtualInterfaceAllocation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apviNewPrivateVirtualInterfaceAllocation :: Lens.Lens' AllocatePrivateVirtualInterface NewPrivateVirtualInterfaceAllocation
apviNewPrivateVirtualInterfaceAllocation = Lens.lens (newPrivateVirtualInterfaceAllocation :: AllocatePrivateVirtualInterface -> NewPrivateVirtualInterfaceAllocation) (\s a -> s {newPrivateVirtualInterfaceAllocation = a} :: AllocatePrivateVirtualInterface)
{-# DEPRECATED apviNewPrivateVirtualInterfaceAllocation "Use generic-lens or generic-optics with 'newPrivateVirtualInterfaceAllocation' instead." #-}

instance Lude.AWSRequest AllocatePrivateVirtualInterface where
  type Rs AllocatePrivateVirtualInterface = VirtualInterface
  request = Req.postJSON directConnectService
  response = Res.receiveJSON (\s h x -> Lude.eitherParseJSON x)

instance Lude.ToHeaders AllocatePrivateVirtualInterface where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "OvertureService.AllocatePrivateVirtualInterface" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON AllocatePrivateVirtualInterface where
  toJSON AllocatePrivateVirtualInterface' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("connectionId" Lude..= connectionId),
            Lude.Just ("ownerAccount" Lude..= ownerAccount),
            Lude.Just
              ( "newPrivateVirtualInterfaceAllocation"
                  Lude..= newPrivateVirtualInterfaceAllocation
              )
          ]
      )

instance Lude.ToPath AllocatePrivateVirtualInterface where
  toPath = Lude.const "/"

instance Lude.ToQuery AllocatePrivateVirtualInterface where
  toQuery = Lude.const Lude.mempty
