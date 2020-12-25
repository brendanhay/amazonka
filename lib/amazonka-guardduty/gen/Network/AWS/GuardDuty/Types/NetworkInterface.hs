{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.Types.NetworkInterface
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GuardDuty.Types.NetworkInterface
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

import qualified Network.AWS.GuardDuty.Types.PrivateIpAddressDetails as Types
import qualified Network.AWS.GuardDuty.Types.SecurityGroup as Types
import qualified Network.AWS.GuardDuty.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains information about the elastic network interface of the EC2 instance.
--
-- /See:/ 'mkNetworkInterface' smart constructor.
data NetworkInterface = NetworkInterface'
  { -- | A list of IPv6 addresses for the EC2 instance.
    ipv6Addresses :: Core.Maybe [Types.String],
    -- | The ID of the network interface.
    networkInterfaceId :: Core.Maybe Types.String,
    -- | The private DNS name of the EC2 instance.
    privateDnsName :: Core.Maybe Types.String,
    -- | The private IP address of the EC2 instance.
    privateIpAddress :: Core.Maybe Types.String,
    -- | Other private IP address information of the EC2 instance.
    privateIpAddresses :: Core.Maybe [Types.PrivateIpAddressDetails],
    -- | The public DNS name of the EC2 instance.
    publicDnsName :: Core.Maybe Types.String,
    -- | The public IP address of the EC2 instance.
    publicIp :: Core.Maybe Types.String,
    -- | The security groups associated with the EC2 instance.
    securityGroups :: Core.Maybe [Types.SecurityGroup],
    -- | The subnet ID of the EC2 instance.
    subnetId :: Core.Maybe Types.String,
    -- | The VPC ID of the EC2 instance.
    vpcId :: Core.Maybe Types.String
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

-- | A list of IPv6 addresses for the EC2 instance.
--
-- /Note:/ Consider using 'ipv6Addresses' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
niIpv6Addresses :: Lens.Lens' NetworkInterface (Core.Maybe [Types.String])
niIpv6Addresses = Lens.field @"ipv6Addresses"
{-# DEPRECATED niIpv6Addresses "Use generic-lens or generic-optics with 'ipv6Addresses' instead." #-}

-- | The ID of the network interface.
--
-- /Note:/ Consider using 'networkInterfaceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
niNetworkInterfaceId :: Lens.Lens' NetworkInterface (Core.Maybe Types.String)
niNetworkInterfaceId = Lens.field @"networkInterfaceId"
{-# DEPRECATED niNetworkInterfaceId "Use generic-lens or generic-optics with 'networkInterfaceId' instead." #-}

-- | The private DNS name of the EC2 instance.
--
-- /Note:/ Consider using 'privateDnsName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
niPrivateDnsName :: Lens.Lens' NetworkInterface (Core.Maybe Types.String)
niPrivateDnsName = Lens.field @"privateDnsName"
{-# DEPRECATED niPrivateDnsName "Use generic-lens or generic-optics with 'privateDnsName' instead." #-}

-- | The private IP address of the EC2 instance.
--
-- /Note:/ Consider using 'privateIpAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
niPrivateIpAddress :: Lens.Lens' NetworkInterface (Core.Maybe Types.String)
niPrivateIpAddress = Lens.field @"privateIpAddress"
{-# DEPRECATED niPrivateIpAddress "Use generic-lens or generic-optics with 'privateIpAddress' instead." #-}

-- | Other private IP address information of the EC2 instance.
--
-- /Note:/ Consider using 'privateIpAddresses' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
niPrivateIpAddresses :: Lens.Lens' NetworkInterface (Core.Maybe [Types.PrivateIpAddressDetails])
niPrivateIpAddresses = Lens.field @"privateIpAddresses"
{-# DEPRECATED niPrivateIpAddresses "Use generic-lens or generic-optics with 'privateIpAddresses' instead." #-}

-- | The public DNS name of the EC2 instance.
--
-- /Note:/ Consider using 'publicDnsName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
niPublicDnsName :: Lens.Lens' NetworkInterface (Core.Maybe Types.String)
niPublicDnsName = Lens.field @"publicDnsName"
{-# DEPRECATED niPublicDnsName "Use generic-lens or generic-optics with 'publicDnsName' instead." #-}

-- | The public IP address of the EC2 instance.
--
-- /Note:/ Consider using 'publicIp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
niPublicIp :: Lens.Lens' NetworkInterface (Core.Maybe Types.String)
niPublicIp = Lens.field @"publicIp"
{-# DEPRECATED niPublicIp "Use generic-lens or generic-optics with 'publicIp' instead." #-}

-- | The security groups associated with the EC2 instance.
--
-- /Note:/ Consider using 'securityGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
niSecurityGroups :: Lens.Lens' NetworkInterface (Core.Maybe [Types.SecurityGroup])
niSecurityGroups = Lens.field @"securityGroups"
{-# DEPRECATED niSecurityGroups "Use generic-lens or generic-optics with 'securityGroups' instead." #-}

-- | The subnet ID of the EC2 instance.
--
-- /Note:/ Consider using 'subnetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
niSubnetId :: Lens.Lens' NetworkInterface (Core.Maybe Types.String)
niSubnetId = Lens.field @"subnetId"
{-# DEPRECATED niSubnetId "Use generic-lens or generic-optics with 'subnetId' instead." #-}

-- | The VPC ID of the EC2 instance.
--
-- /Note:/ Consider using 'vpcId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
niVpcId :: Lens.Lens' NetworkInterface (Core.Maybe Types.String)
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
