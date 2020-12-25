{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Inspector.Types.NetworkInterface
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Inspector.Types.NetworkInterface
  ( NetworkInterface (..),

    -- * Smart constructor
    mkNetworkInterface,

    -- * Lenses
    niIpv6Addresses,
    niNetworkInterfaceId,
    niPrivateDnsName,
    niPrivateIpAddress,
    niPrivateIpAddresses,
    niPublicDnsName,
    niPublicIp,
    niSecurityGroups,
    niSubnetId,
    niVpcId,
  )
where

import qualified Network.AWS.Inspector.Types.PrivateIp as Types
import qualified Network.AWS.Inspector.Types.SecurityGroup as Types
import qualified Network.AWS.Inspector.Types.Text as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains information about the network interfaces interacting with an EC2 instance. This data type is used as one of the elements of the 'AssetAttributes' data type.
--
-- /See:/ 'mkNetworkInterface' smart constructor.
data NetworkInterface = NetworkInterface'
  { -- | The IP addresses associated with the network interface.
    ipv6Addresses :: Core.Maybe [Types.Text],
    -- | The ID of the network interface.
    networkInterfaceId :: Core.Maybe Types.Text,
    -- | The name of a private DNS associated with the network interface.
    privateDnsName :: Core.Maybe Types.Text,
    -- | The private IP address associated with the network interface.
    privateIpAddress :: Core.Maybe Types.Text,
    -- | A list of the private IP addresses associated with the network interface. Includes the privateDnsName and privateIpAddress.
    privateIpAddresses :: Core.Maybe [Types.PrivateIp],
    -- | The name of a public DNS associated with the network interface.
    publicDnsName :: Core.Maybe Types.Text,
    -- | The public IP address from which the network interface is reachable.
    publicIp :: Core.Maybe Types.Text,
    -- | A list of the security groups associated with the network interface. Includes the groupId and groupName.
    securityGroups :: Core.Maybe [Types.SecurityGroup],
    -- | The ID of a subnet associated with the network interface.
    subnetId :: Core.Maybe Types.Text,
    -- | The ID of a VPC associated with the network interface.
    vpcId :: Core.Maybe Types.Text
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'NetworkInterface' value with any optional fields omitted.
mkNetworkInterface ::
  NetworkInterface
mkNetworkInterface =
  NetworkInterface'
    { ipv6Addresses = Core.Nothing,
      networkInterfaceId = Core.Nothing,
      privateDnsName = Core.Nothing,
      privateIpAddress = Core.Nothing,
      privateIpAddresses = Core.Nothing,
      publicDnsName = Core.Nothing,
      publicIp = Core.Nothing,
      securityGroups = Core.Nothing,
      subnetId = Core.Nothing,
      vpcId = Core.Nothing
    }

-- | The IP addresses associated with the network interface.
--
-- /Note:/ Consider using 'ipv6Addresses' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
niIpv6Addresses :: Lens.Lens' NetworkInterface (Core.Maybe [Types.Text])
niIpv6Addresses = Lens.field @"ipv6Addresses"
{-# DEPRECATED niIpv6Addresses "Use generic-lens or generic-optics with 'ipv6Addresses' instead." #-}

-- | The ID of the network interface.
--
-- /Note:/ Consider using 'networkInterfaceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
niNetworkInterfaceId :: Lens.Lens' NetworkInterface (Core.Maybe Types.Text)
niNetworkInterfaceId = Lens.field @"networkInterfaceId"
{-# DEPRECATED niNetworkInterfaceId "Use generic-lens or generic-optics with 'networkInterfaceId' instead." #-}

-- | The name of a private DNS associated with the network interface.
--
-- /Note:/ Consider using 'privateDnsName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
niPrivateDnsName :: Lens.Lens' NetworkInterface (Core.Maybe Types.Text)
niPrivateDnsName = Lens.field @"privateDnsName"
{-# DEPRECATED niPrivateDnsName "Use generic-lens or generic-optics with 'privateDnsName' instead." #-}

-- | The private IP address associated with the network interface.
--
-- /Note:/ Consider using 'privateIpAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
niPrivateIpAddress :: Lens.Lens' NetworkInterface (Core.Maybe Types.Text)
niPrivateIpAddress = Lens.field @"privateIpAddress"
{-# DEPRECATED niPrivateIpAddress "Use generic-lens or generic-optics with 'privateIpAddress' instead." #-}

-- | A list of the private IP addresses associated with the network interface. Includes the privateDnsName and privateIpAddress.
--
-- /Note:/ Consider using 'privateIpAddresses' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
niPrivateIpAddresses :: Lens.Lens' NetworkInterface (Core.Maybe [Types.PrivateIp])
niPrivateIpAddresses = Lens.field @"privateIpAddresses"
{-# DEPRECATED niPrivateIpAddresses "Use generic-lens or generic-optics with 'privateIpAddresses' instead." #-}

-- | The name of a public DNS associated with the network interface.
--
-- /Note:/ Consider using 'publicDnsName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
niPublicDnsName :: Lens.Lens' NetworkInterface (Core.Maybe Types.Text)
niPublicDnsName = Lens.field @"publicDnsName"
{-# DEPRECATED niPublicDnsName "Use generic-lens or generic-optics with 'publicDnsName' instead." #-}

-- | The public IP address from which the network interface is reachable.
--
-- /Note:/ Consider using 'publicIp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
niPublicIp :: Lens.Lens' NetworkInterface (Core.Maybe Types.Text)
niPublicIp = Lens.field @"publicIp"
{-# DEPRECATED niPublicIp "Use generic-lens or generic-optics with 'publicIp' instead." #-}

-- | A list of the security groups associated with the network interface. Includes the groupId and groupName.
--
-- /Note:/ Consider using 'securityGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
niSecurityGroups :: Lens.Lens' NetworkInterface (Core.Maybe [Types.SecurityGroup])
niSecurityGroups = Lens.field @"securityGroups"
{-# DEPRECATED niSecurityGroups "Use generic-lens or generic-optics with 'securityGroups' instead." #-}

-- | The ID of a subnet associated with the network interface.
--
-- /Note:/ Consider using 'subnetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
niSubnetId :: Lens.Lens' NetworkInterface (Core.Maybe Types.Text)
niSubnetId = Lens.field @"subnetId"
{-# DEPRECATED niSubnetId "Use generic-lens or generic-optics with 'subnetId' instead." #-}

-- | The ID of a VPC associated with the network interface.
--
-- /Note:/ Consider using 'vpcId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
niVpcId :: Lens.Lens' NetworkInterface (Core.Maybe Types.Text)
niVpcId = Lens.field @"vpcId"
{-# DEPRECATED niVpcId "Use generic-lens or generic-optics with 'vpcId' instead." #-}

instance Core.FromJSON NetworkInterface where
  parseJSON =
    Core.withObject "NetworkInterface" Core.$
      \x ->
        NetworkInterface'
          Core.<$> (x Core..:? "ipv6Addresses")
          Core.<*> (x Core..:? "networkInterfaceId")
          Core.<*> (x Core..:? "privateDnsName")
          Core.<*> (x Core..:? "privateIpAddress")
          Core.<*> (x Core..:? "privateIpAddresses")
          Core.<*> (x Core..:? "publicDnsName")
          Core.<*> (x Core..:? "publicIp")
          Core.<*> (x Core..:? "securityGroups")
          Core.<*> (x Core..:? "subnetId")
          Core.<*> (x Core..:? "vpcId")
