{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.InstanceNetworkInterfaceAssociation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.InstanceNetworkInterfaceAssociation
  ( InstanceNetworkInterfaceAssociation (..),

    -- * Smart constructor
    mkInstanceNetworkInterfaceAssociation,

    -- * Lenses
    iniaCarrierIp,
    iniaIpOwnerId,
    iniaPublicDnsName,
    iniaPublicIp,
  )
where

import qualified Network.AWS.EC2.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes association information for an Elastic IP address (IPv4).
--
-- /See:/ 'mkInstanceNetworkInterfaceAssociation' smart constructor.
data InstanceNetworkInterfaceAssociation = InstanceNetworkInterfaceAssociation'
  { -- | The carrier IP address associated with the network interface.
    carrierIp :: Core.Maybe Types.String,
    -- | The ID of the owner of the Elastic IP address.
    ipOwnerId :: Core.Maybe Types.String,
    -- | The public DNS name.
    publicDnsName :: Core.Maybe Types.String,
    -- | The public IP address or Elastic IP address bound to the network interface.
    publicIp :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'InstanceNetworkInterfaceAssociation' value with any optional fields omitted.
mkInstanceNetworkInterfaceAssociation ::
  InstanceNetworkInterfaceAssociation
mkInstanceNetworkInterfaceAssociation =
  InstanceNetworkInterfaceAssociation'
    { carrierIp = Core.Nothing,
      ipOwnerId = Core.Nothing,
      publicDnsName = Core.Nothing,
      publicIp = Core.Nothing
    }

-- | The carrier IP address associated with the network interface.
--
-- /Note:/ Consider using 'carrierIp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iniaCarrierIp :: Lens.Lens' InstanceNetworkInterfaceAssociation (Core.Maybe Types.String)
iniaCarrierIp = Lens.field @"carrierIp"
{-# DEPRECATED iniaCarrierIp "Use generic-lens or generic-optics with 'carrierIp' instead." #-}

-- | The ID of the owner of the Elastic IP address.
--
-- /Note:/ Consider using 'ipOwnerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iniaIpOwnerId :: Lens.Lens' InstanceNetworkInterfaceAssociation (Core.Maybe Types.String)
iniaIpOwnerId = Lens.field @"ipOwnerId"
{-# DEPRECATED iniaIpOwnerId "Use generic-lens or generic-optics with 'ipOwnerId' instead." #-}

-- | The public DNS name.
--
-- /Note:/ Consider using 'publicDnsName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iniaPublicDnsName :: Lens.Lens' InstanceNetworkInterfaceAssociation (Core.Maybe Types.String)
iniaPublicDnsName = Lens.field @"publicDnsName"
{-# DEPRECATED iniaPublicDnsName "Use generic-lens or generic-optics with 'publicDnsName' instead." #-}

-- | The public IP address or Elastic IP address bound to the network interface.
--
-- /Note:/ Consider using 'publicIp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iniaPublicIp :: Lens.Lens' InstanceNetworkInterfaceAssociation (Core.Maybe Types.String)
iniaPublicIp = Lens.field @"publicIp"
{-# DEPRECATED iniaPublicIp "Use generic-lens or generic-optics with 'publicIp' instead." #-}

instance Core.FromXML InstanceNetworkInterfaceAssociation where
  parseXML x =
    InstanceNetworkInterfaceAssociation'
      Core.<$> (x Core..@? "carrierIp")
      Core.<*> (x Core..@? "ipOwnerId")
      Core.<*> (x Core..@? "publicDnsName")
      Core.<*> (x Core..@? "publicIp")
