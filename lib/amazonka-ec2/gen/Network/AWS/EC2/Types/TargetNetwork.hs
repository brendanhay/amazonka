{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.TargetNetwork
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.TargetNetwork
  ( TargetNetwork (..),

    -- * Smart constructor
    mkTargetNetwork,

    -- * Lenses
    tnAssociationId,
    tnStatus,
    tnSecurityGroups,
    tnTargetNetworkId,
    tnVPCId,
    tnClientVPNEndpointId,
  )
where

import Network.AWS.EC2.Types.AssociationStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a target network associated with a Client VPN endpoint.
--
-- /See:/ 'mkTargetNetwork' smart constructor.
data TargetNetwork = TargetNetwork'
  { -- | The ID of the association.
    associationId :: Lude.Maybe Lude.Text,
    -- | The current state of the target network association.
    status :: Lude.Maybe AssociationStatus,
    -- | The IDs of the security groups applied to the target network association.
    securityGroups :: Lude.Maybe [Lude.Text],
    -- | The ID of the subnet specified as the target network.
    targetNetworkId :: Lude.Maybe Lude.Text,
    -- | The ID of the VPC in which the target network (subnet) is located.
    vpcId :: Lude.Maybe Lude.Text,
    -- | The ID of the Client VPN endpoint with which the target network is associated.
    clientVPNEndpointId :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TargetNetwork' with the minimum fields required to make a request.
--
-- * 'associationId' - The ID of the association.
-- * 'status' - The current state of the target network association.
-- * 'securityGroups' - The IDs of the security groups applied to the target network association.
-- * 'targetNetworkId' - The ID of the subnet specified as the target network.
-- * 'vpcId' - The ID of the VPC in which the target network (subnet) is located.
-- * 'clientVPNEndpointId' - The ID of the Client VPN endpoint with which the target network is associated.
mkTargetNetwork ::
  TargetNetwork
mkTargetNetwork =
  TargetNetwork'
    { associationId = Lude.Nothing,
      status = Lude.Nothing,
      securityGroups = Lude.Nothing,
      targetNetworkId = Lude.Nothing,
      vpcId = Lude.Nothing,
      clientVPNEndpointId = Lude.Nothing
    }

-- | The ID of the association.
--
-- /Note:/ Consider using 'associationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tnAssociationId :: Lens.Lens' TargetNetwork (Lude.Maybe Lude.Text)
tnAssociationId = Lens.lens (associationId :: TargetNetwork -> Lude.Maybe Lude.Text) (\s a -> s {associationId = a} :: TargetNetwork)
{-# DEPRECATED tnAssociationId "Use generic-lens or generic-optics with 'associationId' instead." #-}

-- | The current state of the target network association.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tnStatus :: Lens.Lens' TargetNetwork (Lude.Maybe AssociationStatus)
tnStatus = Lens.lens (status :: TargetNetwork -> Lude.Maybe AssociationStatus) (\s a -> s {status = a} :: TargetNetwork)
{-# DEPRECATED tnStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The IDs of the security groups applied to the target network association.
--
-- /Note:/ Consider using 'securityGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tnSecurityGroups :: Lens.Lens' TargetNetwork (Lude.Maybe [Lude.Text])
tnSecurityGroups = Lens.lens (securityGroups :: TargetNetwork -> Lude.Maybe [Lude.Text]) (\s a -> s {securityGroups = a} :: TargetNetwork)
{-# DEPRECATED tnSecurityGroups "Use generic-lens or generic-optics with 'securityGroups' instead." #-}

-- | The ID of the subnet specified as the target network.
--
-- /Note:/ Consider using 'targetNetworkId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tnTargetNetworkId :: Lens.Lens' TargetNetwork (Lude.Maybe Lude.Text)
tnTargetNetworkId = Lens.lens (targetNetworkId :: TargetNetwork -> Lude.Maybe Lude.Text) (\s a -> s {targetNetworkId = a} :: TargetNetwork)
{-# DEPRECATED tnTargetNetworkId "Use generic-lens or generic-optics with 'targetNetworkId' instead." #-}

-- | The ID of the VPC in which the target network (subnet) is located.
--
-- /Note:/ Consider using 'vpcId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tnVPCId :: Lens.Lens' TargetNetwork (Lude.Maybe Lude.Text)
tnVPCId = Lens.lens (vpcId :: TargetNetwork -> Lude.Maybe Lude.Text) (\s a -> s {vpcId = a} :: TargetNetwork)
{-# DEPRECATED tnVPCId "Use generic-lens or generic-optics with 'vpcId' instead." #-}

-- | The ID of the Client VPN endpoint with which the target network is associated.
--
-- /Note:/ Consider using 'clientVPNEndpointId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tnClientVPNEndpointId :: Lens.Lens' TargetNetwork (Lude.Maybe Lude.Text)
tnClientVPNEndpointId = Lens.lens (clientVPNEndpointId :: TargetNetwork -> Lude.Maybe Lude.Text) (\s a -> s {clientVPNEndpointId = a} :: TargetNetwork)
{-# DEPRECATED tnClientVPNEndpointId "Use generic-lens or generic-optics with 'clientVPNEndpointId' instead." #-}

instance Lude.FromXML TargetNetwork where
  parseXML x =
    TargetNetwork'
      Lude.<$> (x Lude..@? "associationId")
      Lude.<*> (x Lude..@? "status")
      Lude.<*> ( x Lude..@? "securityGroups" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "item")
               )
      Lude.<*> (x Lude..@? "targetNetworkId")
      Lude.<*> (x Lude..@? "vpcId")
      Lude.<*> (x Lude..@? "clientVpnEndpointId")
