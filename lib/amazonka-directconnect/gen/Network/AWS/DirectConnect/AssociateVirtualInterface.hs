{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectConnect.AssociateVirtualInterface
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates a virtual interface with a specified link aggregation group (LAG) or connection. Connectivity to AWS is temporarily interrupted as the virtual interface is being migrated. If the target connection or LAG has an associated virtual interface with a conflicting VLAN number or a conflicting IP address, the operation fails.
--
-- Virtual interfaces associated with a hosted connection cannot be associated with a LAG; hosted connections must be migrated along with their virtual interfaces using 'AssociateHostedConnection' .
-- To reassociate a virtual interface to a new connection or LAG, the requester must own either the virtual interface itself or the connection to which the virtual interface is currently associated. Additionally, the requester must own the connection or LAG for the association.
module Network.AWS.DirectConnect.AssociateVirtualInterface
  ( -- * Creating a request
    AssociateVirtualInterface (..),
    mkAssociateVirtualInterface,

    -- ** Request lenses
    aviVirtualInterfaceId,
    aviConnectionId,

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

-- | /See:/ 'mkAssociateVirtualInterface' smart constructor.
data AssociateVirtualInterface = AssociateVirtualInterface'
  { virtualInterfaceId ::
      Lude.Text,
    connectionId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AssociateVirtualInterface' with the minimum fields required to make a request.
--
-- * 'connectionId' - The ID of the LAG or connection.
-- * 'virtualInterfaceId' - The ID of the virtual interface.
mkAssociateVirtualInterface ::
  -- | 'virtualInterfaceId'
  Lude.Text ->
  -- | 'connectionId'
  Lude.Text ->
  AssociateVirtualInterface
mkAssociateVirtualInterface pVirtualInterfaceId_ pConnectionId_ =
  AssociateVirtualInterface'
    { virtualInterfaceId =
        pVirtualInterfaceId_,
      connectionId = pConnectionId_
    }

-- | The ID of the virtual interface.
--
-- /Note:/ Consider using 'virtualInterfaceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aviVirtualInterfaceId :: Lens.Lens' AssociateVirtualInterface Lude.Text
aviVirtualInterfaceId = Lens.lens (virtualInterfaceId :: AssociateVirtualInterface -> Lude.Text) (\s a -> s {virtualInterfaceId = a} :: AssociateVirtualInterface)
{-# DEPRECATED aviVirtualInterfaceId "Use generic-lens or generic-optics with 'virtualInterfaceId' instead." #-}

-- | The ID of the LAG or connection.
--
-- /Note:/ Consider using 'connectionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aviConnectionId :: Lens.Lens' AssociateVirtualInterface Lude.Text
aviConnectionId = Lens.lens (connectionId :: AssociateVirtualInterface -> Lude.Text) (\s a -> s {connectionId = a} :: AssociateVirtualInterface)
{-# DEPRECATED aviConnectionId "Use generic-lens or generic-optics with 'connectionId' instead." #-}

instance Lude.AWSRequest AssociateVirtualInterface where
  type Rs AssociateVirtualInterface = VirtualInterface
  request = Req.postJSON directConnectService
  response = Res.receiveJSON (\s h x -> Lude.eitherParseJSON x)

instance Lude.ToHeaders AssociateVirtualInterface where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("OvertureService.AssociateVirtualInterface" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON AssociateVirtualInterface where
  toJSON AssociateVirtualInterface' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("virtualInterfaceId" Lude..= virtualInterfaceId),
            Lude.Just ("connectionId" Lude..= connectionId)
          ]
      )

instance Lude.ToPath AssociateVirtualInterface where
  toPath = Lude.const "/"

instance Lude.ToQuery AssociateVirtualInterface where
  toQuery = Lude.const Lude.mempty
