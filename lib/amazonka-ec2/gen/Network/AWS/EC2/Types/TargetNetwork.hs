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
    tnClientVpnEndpointId,
    tnSecurityGroups,
    tnStatus,
    tnTargetNetworkId,
    tnVpcId,
  )
where

import qualified Network.AWS.EC2.Types.AssociationStatus as Types
import qualified Network.AWS.EC2.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a target network associated with a Client VPN endpoint.
--
-- /See:/ 'mkTargetNetwork' smart constructor.
data TargetNetwork = TargetNetwork'
  { -- | The ID of the association.
    associationId :: Core.Maybe Types.String,
    -- | The ID of the Client VPN endpoint with which the target network is associated.
    clientVpnEndpointId :: Core.Maybe Types.String,
    -- | The IDs of the security groups applied to the target network association.
    securityGroups :: Core.Maybe [Types.String],
    -- | The current state of the target network association.
    status :: Core.Maybe Types.AssociationStatus,
    -- | The ID of the subnet specified as the target network.
    targetNetworkId :: Core.Maybe Types.String,
    -- | The ID of the VPC in which the target network (subnet) is located.
    vpcId :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TargetNetwork' value with any optional fields omitted.
mkTargetNetwork ::
  TargetNetwork
mkTargetNetwork =
  TargetNetwork'
    { associationId = Core.Nothing,
      clientVpnEndpointId = Core.Nothing,
      securityGroups = Core.Nothing,
      status = Core.Nothing,
      targetNetworkId = Core.Nothing,
      vpcId = Core.Nothing
    }

-- | The ID of the association.
--
-- /Note:/ Consider using 'associationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tnAssociationId :: Lens.Lens' TargetNetwork (Core.Maybe Types.String)
tnAssociationId = Lens.field @"associationId"
{-# DEPRECATED tnAssociationId "Use generic-lens or generic-optics with 'associationId' instead." #-}

-- | The ID of the Client VPN endpoint with which the target network is associated.
--
-- /Note:/ Consider using 'clientVpnEndpointId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tnClientVpnEndpointId :: Lens.Lens' TargetNetwork (Core.Maybe Types.String)
tnClientVpnEndpointId = Lens.field @"clientVpnEndpointId"
{-# DEPRECATED tnClientVpnEndpointId "Use generic-lens or generic-optics with 'clientVpnEndpointId' instead." #-}

-- | The IDs of the security groups applied to the target network association.
--
-- /Note:/ Consider using 'securityGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tnSecurityGroups :: Lens.Lens' TargetNetwork (Core.Maybe [Types.String])
tnSecurityGroups = Lens.field @"securityGroups"
{-# DEPRECATED tnSecurityGroups "Use generic-lens or generic-optics with 'securityGroups' instead." #-}

-- | The current state of the target network association.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tnStatus :: Lens.Lens' TargetNetwork (Core.Maybe Types.AssociationStatus)
tnStatus = Lens.field @"status"
{-# DEPRECATED tnStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The ID of the subnet specified as the target network.
--
-- /Note:/ Consider using 'targetNetworkId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tnTargetNetworkId :: Lens.Lens' TargetNetwork (Core.Maybe Types.String)
tnTargetNetworkId = Lens.field @"targetNetworkId"
{-# DEPRECATED tnTargetNetworkId "Use generic-lens or generic-optics with 'targetNetworkId' instead." #-}

-- | The ID of the VPC in which the target network (subnet) is located.
--
-- /Note:/ Consider using 'vpcId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tnVpcId :: Lens.Lens' TargetNetwork (Core.Maybe Types.String)
tnVpcId = Lens.field @"vpcId"
{-# DEPRECATED tnVpcId "Use generic-lens or generic-optics with 'vpcId' instead." #-}

instance Core.FromXML TargetNetwork where
  parseXML x =
    TargetNetwork'
      Core.<$> (x Core..@? "associationId")
      Core.<*> (x Core..@? "clientVpnEndpointId")
      Core.<*> (x Core..@? "securityGroups" Core..<@> Core.parseXMLList "item")
      Core.<*> (x Core..@? "status")
      Core.<*> (x Core..@? "targetNetworkId")
      Core.<*> (x Core..@? "vpcId")
