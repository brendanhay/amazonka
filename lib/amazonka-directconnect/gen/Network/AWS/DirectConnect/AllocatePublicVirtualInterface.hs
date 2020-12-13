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

-- | /See:/ 'mkAllocatePublicVirtualInterface' smart constructor.
data AllocatePublicVirtualInterface = AllocatePublicVirtualInterface'
  { -- | The ID of the connection on which the public virtual interface is provisioned.
    connectionId :: Lude.Text,
    -- | The ID of the AWS account that owns the public virtual interface.
    ownerAccount :: Lude.Text,
    -- | Information about the public virtual interface.
    newPublicVirtualInterfaceAllocation :: NewPublicVirtualInterfaceAllocation
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AllocatePublicVirtualInterface' with the minimum fields required to make a request.
--
-- * 'connectionId' - The ID of the connection on which the public virtual interface is provisioned.
-- * 'ownerAccount' - The ID of the AWS account that owns the public virtual interface.
-- * 'newPublicVirtualInterfaceAllocation' - Information about the public virtual interface.
mkAllocatePublicVirtualInterface ::
  -- | 'connectionId'
  Lude.Text ->
  -- | 'ownerAccount'
  Lude.Text ->
  -- | 'newPublicVirtualInterfaceAllocation'
  NewPublicVirtualInterfaceAllocation ->
  AllocatePublicVirtualInterface
mkAllocatePublicVirtualInterface
  pConnectionId_
  pOwnerAccount_
  pNewPublicVirtualInterfaceAllocation_ =
    AllocatePublicVirtualInterface'
      { connectionId = pConnectionId_,
        ownerAccount = pOwnerAccount_,
        newPublicVirtualInterfaceAllocation =
          pNewPublicVirtualInterfaceAllocation_
      }

-- | The ID of the connection on which the public virtual interface is provisioned.
--
-- /Note:/ Consider using 'connectionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aConnectionId :: Lens.Lens' AllocatePublicVirtualInterface Lude.Text
aConnectionId = Lens.lens (connectionId :: AllocatePublicVirtualInterface -> Lude.Text) (\s a -> s {connectionId = a} :: AllocatePublicVirtualInterface)
{-# DEPRECATED aConnectionId "Use generic-lens or generic-optics with 'connectionId' instead." #-}

-- | The ID of the AWS account that owns the public virtual interface.
--
-- /Note:/ Consider using 'ownerAccount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aOwnerAccount :: Lens.Lens' AllocatePublicVirtualInterface Lude.Text
aOwnerAccount = Lens.lens (ownerAccount :: AllocatePublicVirtualInterface -> Lude.Text) (\s a -> s {ownerAccount = a} :: AllocatePublicVirtualInterface)
{-# DEPRECATED aOwnerAccount "Use generic-lens or generic-optics with 'ownerAccount' instead." #-}

-- | Information about the public virtual interface.
--
-- /Note:/ Consider using 'newPublicVirtualInterfaceAllocation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aNewPublicVirtualInterfaceAllocation :: Lens.Lens' AllocatePublicVirtualInterface NewPublicVirtualInterfaceAllocation
aNewPublicVirtualInterfaceAllocation = Lens.lens (newPublicVirtualInterfaceAllocation :: AllocatePublicVirtualInterface -> NewPublicVirtualInterfaceAllocation) (\s a -> s {newPublicVirtualInterfaceAllocation = a} :: AllocatePublicVirtualInterface)
{-# DEPRECATED aNewPublicVirtualInterfaceAllocation "Use generic-lens or generic-optics with 'newPublicVirtualInterfaceAllocation' instead." #-}

instance Lude.AWSRequest AllocatePublicVirtualInterface where
  type Rs AllocatePublicVirtualInterface = VirtualInterface
  request = Req.postJSON directConnectService
  response = Res.receiveJSON (\s h x -> Lude.eitherParseJSON x)

instance Lude.ToHeaders AllocatePublicVirtualInterface where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "OvertureService.AllocatePublicVirtualInterface" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON AllocatePublicVirtualInterface where
  toJSON AllocatePublicVirtualInterface' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("connectionId" Lude..= connectionId),
            Lude.Just ("ownerAccount" Lude..= ownerAccount),
            Lude.Just
              ( "newPublicVirtualInterfaceAllocation"
                  Lude..= newPublicVirtualInterfaceAllocation
              )
          ]
      )

instance Lude.ToPath AllocatePublicVirtualInterface where
  toPath = Lude.const "/"

instance Lude.ToQuery AllocatePublicVirtualInterface where
  toQuery = Lude.const Lude.mempty
