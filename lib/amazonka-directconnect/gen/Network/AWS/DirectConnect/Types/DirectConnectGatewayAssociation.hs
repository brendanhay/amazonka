{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectConnect.Types.DirectConnectGatewayAssociation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DirectConnect.Types.DirectConnectGatewayAssociation
  ( DirectConnectGatewayAssociation (..),

    -- * Smart constructor
    mkDirectConnectGatewayAssociation,

    -- * Lenses
    dcgaVirtualGatewayId,
    dcgaAssociationId,
    dcgaDirectConnectGatewayId,
    dcgaVirtualGatewayOwnerAccount,
    dcgaStateChangeError,
    dcgaVirtualGatewayRegion,
    dcgaAssociatedGateway,
    dcgaDirectConnectGatewayOwnerAccount,
    dcgaAllowedPrefixesToDirectConnectGateway,
    dcgaAssociationState,
  )
where

import Network.AWS.DirectConnect.Types.AssociatedGateway
import Network.AWS.DirectConnect.Types.DirectConnectGatewayAssociationState
import Network.AWS.DirectConnect.Types.RouteFilterPrefix
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about an association between a Direct Connect gateway and a virtual private gateway or transit gateway.
--
-- /See:/ 'mkDirectConnectGatewayAssociation' smart constructor.
data DirectConnectGatewayAssociation = DirectConnectGatewayAssociation'
  { -- | The ID of the virtual private gateway. Applies only to private virtual interfaces.
    virtualGatewayId :: Lude.Maybe Lude.Text,
    -- | The ID of the Direct Connect gateway association.
    associationId :: Lude.Maybe Lude.Text,
    -- | The ID of the Direct Connect gateway.
    directConnectGatewayId :: Lude.Maybe Lude.Text,
    -- | The ID of the AWS account that owns the virtual private gateway.
    virtualGatewayOwnerAccount :: Lude.Maybe Lude.Text,
    -- | The error message if the state of an object failed to advance.
    stateChangeError :: Lude.Maybe Lude.Text,
    -- | The AWS Region where the virtual private gateway is located.
    virtualGatewayRegion :: Lude.Maybe Lude.Text,
    -- | Information about the associated gateway.
    associatedGateway :: Lude.Maybe AssociatedGateway,
    -- | The ID of the AWS account that owns the associated gateway.
    directConnectGatewayOwnerAccount :: Lude.Maybe Lude.Text,
    -- | The Amazon VPC prefixes to advertise to the Direct Connect gateway.
    allowedPrefixesToDirectConnectGateway :: Lude.Maybe [RouteFilterPrefix],
    -- | The state of the association. The following are the possible values:
    --
    --
    --     * @associating@ : The initial state after calling 'CreateDirectConnectGatewayAssociation' .
    --
    --
    --     * @associated@ : The Direct Connect gateway and virtual private gateway or transit gateway are successfully associated and ready to pass traffic.
    --
    --
    --     * @disassociating@ : The initial state after calling 'DeleteDirectConnectGatewayAssociation' .
    --
    --
    --     * @disassociated@ : The virtual private gateway or transit gateway is disassociated from the Direct Connect gateway. Traffic flow between the Direct Connect gateway and virtual private gateway or transit gateway is stopped.
    associationState :: Lude.Maybe DirectConnectGatewayAssociationState
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DirectConnectGatewayAssociation' with the minimum fields required to make a request.
--
-- * 'virtualGatewayId' - The ID of the virtual private gateway. Applies only to private virtual interfaces.
-- * 'associationId' - The ID of the Direct Connect gateway association.
-- * 'directConnectGatewayId' - The ID of the Direct Connect gateway.
-- * 'virtualGatewayOwnerAccount' - The ID of the AWS account that owns the virtual private gateway.
-- * 'stateChangeError' - The error message if the state of an object failed to advance.
-- * 'virtualGatewayRegion' - The AWS Region where the virtual private gateway is located.
-- * 'associatedGateway' - Information about the associated gateway.
-- * 'directConnectGatewayOwnerAccount' - The ID of the AWS account that owns the associated gateway.
-- * 'allowedPrefixesToDirectConnectGateway' - The Amazon VPC prefixes to advertise to the Direct Connect gateway.
-- * 'associationState' - The state of the association. The following are the possible values:
--
--
--     * @associating@ : The initial state after calling 'CreateDirectConnectGatewayAssociation' .
--
--
--     * @associated@ : The Direct Connect gateway and virtual private gateway or transit gateway are successfully associated and ready to pass traffic.
--
--
--     * @disassociating@ : The initial state after calling 'DeleteDirectConnectGatewayAssociation' .
--
--
--     * @disassociated@ : The virtual private gateway or transit gateway is disassociated from the Direct Connect gateway. Traffic flow between the Direct Connect gateway and virtual private gateway or transit gateway is stopped.
mkDirectConnectGatewayAssociation ::
  DirectConnectGatewayAssociation
mkDirectConnectGatewayAssociation =
  DirectConnectGatewayAssociation'
    { virtualGatewayId = Lude.Nothing,
      associationId = Lude.Nothing,
      directConnectGatewayId = Lude.Nothing,
      virtualGatewayOwnerAccount = Lude.Nothing,
      stateChangeError = Lude.Nothing,
      virtualGatewayRegion = Lude.Nothing,
      associatedGateway = Lude.Nothing,
      directConnectGatewayOwnerAccount = Lude.Nothing,
      allowedPrefixesToDirectConnectGateway = Lude.Nothing,
      associationState = Lude.Nothing
    }

-- | The ID of the virtual private gateway. Applies only to private virtual interfaces.
--
-- /Note:/ Consider using 'virtualGatewayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcgaVirtualGatewayId :: Lens.Lens' DirectConnectGatewayAssociation (Lude.Maybe Lude.Text)
dcgaVirtualGatewayId = Lens.lens (virtualGatewayId :: DirectConnectGatewayAssociation -> Lude.Maybe Lude.Text) (\s a -> s {virtualGatewayId = a} :: DirectConnectGatewayAssociation)
{-# DEPRECATED dcgaVirtualGatewayId "Use generic-lens or generic-optics with 'virtualGatewayId' instead." #-}

-- | The ID of the Direct Connect gateway association.
--
-- /Note:/ Consider using 'associationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcgaAssociationId :: Lens.Lens' DirectConnectGatewayAssociation (Lude.Maybe Lude.Text)
dcgaAssociationId = Lens.lens (associationId :: DirectConnectGatewayAssociation -> Lude.Maybe Lude.Text) (\s a -> s {associationId = a} :: DirectConnectGatewayAssociation)
{-# DEPRECATED dcgaAssociationId "Use generic-lens or generic-optics with 'associationId' instead." #-}

-- | The ID of the Direct Connect gateway.
--
-- /Note:/ Consider using 'directConnectGatewayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcgaDirectConnectGatewayId :: Lens.Lens' DirectConnectGatewayAssociation (Lude.Maybe Lude.Text)
dcgaDirectConnectGatewayId = Lens.lens (directConnectGatewayId :: DirectConnectGatewayAssociation -> Lude.Maybe Lude.Text) (\s a -> s {directConnectGatewayId = a} :: DirectConnectGatewayAssociation)
{-# DEPRECATED dcgaDirectConnectGatewayId "Use generic-lens or generic-optics with 'directConnectGatewayId' instead." #-}

-- | The ID of the AWS account that owns the virtual private gateway.
--
-- /Note:/ Consider using 'virtualGatewayOwnerAccount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcgaVirtualGatewayOwnerAccount :: Lens.Lens' DirectConnectGatewayAssociation (Lude.Maybe Lude.Text)
dcgaVirtualGatewayOwnerAccount = Lens.lens (virtualGatewayOwnerAccount :: DirectConnectGatewayAssociation -> Lude.Maybe Lude.Text) (\s a -> s {virtualGatewayOwnerAccount = a} :: DirectConnectGatewayAssociation)
{-# DEPRECATED dcgaVirtualGatewayOwnerAccount "Use generic-lens or generic-optics with 'virtualGatewayOwnerAccount' instead." #-}

-- | The error message if the state of an object failed to advance.
--
-- /Note:/ Consider using 'stateChangeError' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcgaStateChangeError :: Lens.Lens' DirectConnectGatewayAssociation (Lude.Maybe Lude.Text)
dcgaStateChangeError = Lens.lens (stateChangeError :: DirectConnectGatewayAssociation -> Lude.Maybe Lude.Text) (\s a -> s {stateChangeError = a} :: DirectConnectGatewayAssociation)
{-# DEPRECATED dcgaStateChangeError "Use generic-lens or generic-optics with 'stateChangeError' instead." #-}

-- | The AWS Region where the virtual private gateway is located.
--
-- /Note:/ Consider using 'virtualGatewayRegion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcgaVirtualGatewayRegion :: Lens.Lens' DirectConnectGatewayAssociation (Lude.Maybe Lude.Text)
dcgaVirtualGatewayRegion = Lens.lens (virtualGatewayRegion :: DirectConnectGatewayAssociation -> Lude.Maybe Lude.Text) (\s a -> s {virtualGatewayRegion = a} :: DirectConnectGatewayAssociation)
{-# DEPRECATED dcgaVirtualGatewayRegion "Use generic-lens or generic-optics with 'virtualGatewayRegion' instead." #-}

-- | Information about the associated gateway.
--
-- /Note:/ Consider using 'associatedGateway' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcgaAssociatedGateway :: Lens.Lens' DirectConnectGatewayAssociation (Lude.Maybe AssociatedGateway)
dcgaAssociatedGateway = Lens.lens (associatedGateway :: DirectConnectGatewayAssociation -> Lude.Maybe AssociatedGateway) (\s a -> s {associatedGateway = a} :: DirectConnectGatewayAssociation)
{-# DEPRECATED dcgaAssociatedGateway "Use generic-lens or generic-optics with 'associatedGateway' instead." #-}

-- | The ID of the AWS account that owns the associated gateway.
--
-- /Note:/ Consider using 'directConnectGatewayOwnerAccount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcgaDirectConnectGatewayOwnerAccount :: Lens.Lens' DirectConnectGatewayAssociation (Lude.Maybe Lude.Text)
dcgaDirectConnectGatewayOwnerAccount = Lens.lens (directConnectGatewayOwnerAccount :: DirectConnectGatewayAssociation -> Lude.Maybe Lude.Text) (\s a -> s {directConnectGatewayOwnerAccount = a} :: DirectConnectGatewayAssociation)
{-# DEPRECATED dcgaDirectConnectGatewayOwnerAccount "Use generic-lens or generic-optics with 'directConnectGatewayOwnerAccount' instead." #-}

-- | The Amazon VPC prefixes to advertise to the Direct Connect gateway.
--
-- /Note:/ Consider using 'allowedPrefixesToDirectConnectGateway' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcgaAllowedPrefixesToDirectConnectGateway :: Lens.Lens' DirectConnectGatewayAssociation (Lude.Maybe [RouteFilterPrefix])
dcgaAllowedPrefixesToDirectConnectGateway = Lens.lens (allowedPrefixesToDirectConnectGateway :: DirectConnectGatewayAssociation -> Lude.Maybe [RouteFilterPrefix]) (\s a -> s {allowedPrefixesToDirectConnectGateway = a} :: DirectConnectGatewayAssociation)
{-# DEPRECATED dcgaAllowedPrefixesToDirectConnectGateway "Use generic-lens or generic-optics with 'allowedPrefixesToDirectConnectGateway' instead." #-}

-- | The state of the association. The following are the possible values:
--
--
--     * @associating@ : The initial state after calling 'CreateDirectConnectGatewayAssociation' .
--
--
--     * @associated@ : The Direct Connect gateway and virtual private gateway or transit gateway are successfully associated and ready to pass traffic.
--
--
--     * @disassociating@ : The initial state after calling 'DeleteDirectConnectGatewayAssociation' .
--
--
--     * @disassociated@ : The virtual private gateway or transit gateway is disassociated from the Direct Connect gateway. Traffic flow between the Direct Connect gateway and virtual private gateway or transit gateway is stopped.
--
--
--
-- /Note:/ Consider using 'associationState' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcgaAssociationState :: Lens.Lens' DirectConnectGatewayAssociation (Lude.Maybe DirectConnectGatewayAssociationState)
dcgaAssociationState = Lens.lens (associationState :: DirectConnectGatewayAssociation -> Lude.Maybe DirectConnectGatewayAssociationState) (\s a -> s {associationState = a} :: DirectConnectGatewayAssociation)
{-# DEPRECATED dcgaAssociationState "Use generic-lens or generic-optics with 'associationState' instead." #-}

instance Lude.FromJSON DirectConnectGatewayAssociation where
  parseJSON =
    Lude.withObject
      "DirectConnectGatewayAssociation"
      ( \x ->
          DirectConnectGatewayAssociation'
            Lude.<$> (x Lude..:? "virtualGatewayId")
            Lude.<*> (x Lude..:? "associationId")
            Lude.<*> (x Lude..:? "directConnectGatewayId")
            Lude.<*> (x Lude..:? "virtualGatewayOwnerAccount")
            Lude.<*> (x Lude..:? "stateChangeError")
            Lude.<*> (x Lude..:? "virtualGatewayRegion")
            Lude.<*> (x Lude..:? "associatedGateway")
            Lude.<*> (x Lude..:? "directConnectGatewayOwnerAccount")
            Lude.<*> ( x Lude..:? "allowedPrefixesToDirectConnectGateway"
                         Lude..!= Lude.mempty
                     )
            Lude.<*> (x Lude..:? "associationState")
      )
