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
    niPrivateIPAddresses,
    niPublicDNSName,
    niSecurityGroups,
    niVpcId,
    niSubnetId,
    niNetworkInterfaceId,
    niPrivateIPAddress,
    niPublicIP,
    niPrivateDNSName,
    niIpv6Addresses,
  )
where

import Network.AWS.Inspector.Types.PrivateIP
import Network.AWS.Inspector.Types.SecurityGroup
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains information about the network interfaces interacting with an EC2 instance. This data type is used as one of the elements of the 'AssetAttributes' data type.
--
-- /See:/ 'mkNetworkInterface' smart constructor.
data NetworkInterface = NetworkInterface'
  { -- | A list of the private IP addresses associated with the network interface. Includes the privateDnsName and privateIpAddress.
    privateIPAddresses :: Lude.Maybe [PrivateIP],
    -- | The name of a public DNS associated with the network interface.
    publicDNSName :: Lude.Maybe Lude.Text,
    -- | A list of the security groups associated with the network interface. Includes the groupId and groupName.
    securityGroups :: Lude.Maybe [SecurityGroup],
    -- | The ID of a VPC associated with the network interface.
    vpcId :: Lude.Maybe Lude.Text,
    -- | The ID of a subnet associated with the network interface.
    subnetId :: Lude.Maybe Lude.Text,
    -- | The ID of the network interface.
    networkInterfaceId :: Lude.Maybe Lude.Text,
    -- | The private IP address associated with the network interface.
    privateIPAddress :: Lude.Maybe Lude.Text,
    -- | The public IP address from which the network interface is reachable.
    publicIP :: Lude.Maybe Lude.Text,
    -- | The name of a private DNS associated with the network interface.
    privateDNSName :: Lude.Maybe Lude.Text,
    -- | The IP addresses associated with the network interface.
    ipv6Addresses :: Lude.Maybe [Lude.Text]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'NetworkInterface' with the minimum fields required to make a request.
--
-- * 'privateIPAddresses' - A list of the private IP addresses associated with the network interface. Includes the privateDnsName and privateIpAddress.
-- * 'publicDNSName' - The name of a public DNS associated with the network interface.
-- * 'securityGroups' - A list of the security groups associated with the network interface. Includes the groupId and groupName.
-- * 'vpcId' - The ID of a VPC associated with the network interface.
-- * 'subnetId' - The ID of a subnet associated with the network interface.
-- * 'networkInterfaceId' - The ID of the network interface.
-- * 'privateIPAddress' - The private IP address associated with the network interface.
-- * 'publicIP' - The public IP address from which the network interface is reachable.
-- * 'privateDNSName' - The name of a private DNS associated with the network interface.
-- * 'ipv6Addresses' - The IP addresses associated with the network interface.
mkNetworkInterface ::
  NetworkInterface
mkNetworkInterface =
  NetworkInterface'
    { privateIPAddresses = Lude.Nothing,
      publicDNSName = Lude.Nothing,
      securityGroups = Lude.Nothing,
      vpcId = Lude.Nothing,
      subnetId = Lude.Nothing,
      networkInterfaceId = Lude.Nothing,
      privateIPAddress = Lude.Nothing,
      publicIP = Lude.Nothing,
      privateDNSName = Lude.Nothing,
      ipv6Addresses = Lude.Nothing
    }

-- | A list of the private IP addresses associated with the network interface. Includes the privateDnsName and privateIpAddress.
--
-- /Note:/ Consider using 'privateIPAddresses' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
niPrivateIPAddresses :: Lens.Lens' NetworkInterface (Lude.Maybe [PrivateIP])
niPrivateIPAddresses = Lens.lens (privateIPAddresses :: NetworkInterface -> Lude.Maybe [PrivateIP]) (\s a -> s {privateIPAddresses = a} :: NetworkInterface)
{-# DEPRECATED niPrivateIPAddresses "Use generic-lens or generic-optics with 'privateIPAddresses' instead." #-}

-- | The name of a public DNS associated with the network interface.
--
-- /Note:/ Consider using 'publicDNSName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
niPublicDNSName :: Lens.Lens' NetworkInterface (Lude.Maybe Lude.Text)
niPublicDNSName = Lens.lens (publicDNSName :: NetworkInterface -> Lude.Maybe Lude.Text) (\s a -> s {publicDNSName = a} :: NetworkInterface)
{-# DEPRECATED niPublicDNSName "Use generic-lens or generic-optics with 'publicDNSName' instead." #-}

-- | A list of the security groups associated with the network interface. Includes the groupId and groupName.
--
-- /Note:/ Consider using 'securityGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
niSecurityGroups :: Lens.Lens' NetworkInterface (Lude.Maybe [SecurityGroup])
niSecurityGroups = Lens.lens (securityGroups :: NetworkInterface -> Lude.Maybe [SecurityGroup]) (\s a -> s {securityGroups = a} :: NetworkInterface)
{-# DEPRECATED niSecurityGroups "Use generic-lens or generic-optics with 'securityGroups' instead." #-}

-- | The ID of a VPC associated with the network interface.
--
-- /Note:/ Consider using 'vpcId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
niVpcId :: Lens.Lens' NetworkInterface (Lude.Maybe Lude.Text)
niVpcId = Lens.lens (vpcId :: NetworkInterface -> Lude.Maybe Lude.Text) (\s a -> s {vpcId = a} :: NetworkInterface)
{-# DEPRECATED niVpcId "Use generic-lens or generic-optics with 'vpcId' instead." #-}

-- | The ID of a subnet associated with the network interface.
--
-- /Note:/ Consider using 'subnetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
niSubnetId :: Lens.Lens' NetworkInterface (Lude.Maybe Lude.Text)
niSubnetId = Lens.lens (subnetId :: NetworkInterface -> Lude.Maybe Lude.Text) (\s a -> s {subnetId = a} :: NetworkInterface)
{-# DEPRECATED niSubnetId "Use generic-lens or generic-optics with 'subnetId' instead." #-}

-- | The ID of the network interface.
--
-- /Note:/ Consider using 'networkInterfaceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
niNetworkInterfaceId :: Lens.Lens' NetworkInterface (Lude.Maybe Lude.Text)
niNetworkInterfaceId = Lens.lens (networkInterfaceId :: NetworkInterface -> Lude.Maybe Lude.Text) (\s a -> s {networkInterfaceId = a} :: NetworkInterface)
{-# DEPRECATED niNetworkInterfaceId "Use generic-lens or generic-optics with 'networkInterfaceId' instead." #-}

-- | The private IP address associated with the network interface.
--
-- /Note:/ Consider using 'privateIPAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
niPrivateIPAddress :: Lens.Lens' NetworkInterface (Lude.Maybe Lude.Text)
niPrivateIPAddress = Lens.lens (privateIPAddress :: NetworkInterface -> Lude.Maybe Lude.Text) (\s a -> s {privateIPAddress = a} :: NetworkInterface)
{-# DEPRECATED niPrivateIPAddress "Use generic-lens or generic-optics with 'privateIPAddress' instead." #-}

-- | The public IP address from which the network interface is reachable.
--
-- /Note:/ Consider using 'publicIP' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
niPublicIP :: Lens.Lens' NetworkInterface (Lude.Maybe Lude.Text)
niPublicIP = Lens.lens (publicIP :: NetworkInterface -> Lude.Maybe Lude.Text) (\s a -> s {publicIP = a} :: NetworkInterface)
{-# DEPRECATED niPublicIP "Use generic-lens or generic-optics with 'publicIP' instead." #-}

-- | The name of a private DNS associated with the network interface.
--
-- /Note:/ Consider using 'privateDNSName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
niPrivateDNSName :: Lens.Lens' NetworkInterface (Lude.Maybe Lude.Text)
niPrivateDNSName = Lens.lens (privateDNSName :: NetworkInterface -> Lude.Maybe Lude.Text) (\s a -> s {privateDNSName = a} :: NetworkInterface)
{-# DEPRECATED niPrivateDNSName "Use generic-lens or generic-optics with 'privateDNSName' instead." #-}

-- | The IP addresses associated with the network interface.
--
-- /Note:/ Consider using 'ipv6Addresses' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
niIpv6Addresses :: Lens.Lens' NetworkInterface (Lude.Maybe [Lude.Text])
niIpv6Addresses = Lens.lens (ipv6Addresses :: NetworkInterface -> Lude.Maybe [Lude.Text]) (\s a -> s {ipv6Addresses = a} :: NetworkInterface)
{-# DEPRECATED niIpv6Addresses "Use generic-lens or generic-optics with 'ipv6Addresses' instead." #-}

instance Lude.FromJSON NetworkInterface where
  parseJSON =
    Lude.withObject
      "NetworkInterface"
      ( \x ->
          NetworkInterface'
            Lude.<$> (x Lude..:? "privateIpAddresses" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "publicDnsName")
            Lude.<*> (x Lude..:? "securityGroups" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "vpcId")
            Lude.<*> (x Lude..:? "subnetId")
            Lude.<*> (x Lude..:? "networkInterfaceId")
            Lude.<*> (x Lude..:? "privateIpAddress")
            Lude.<*> (x Lude..:? "publicIp")
            Lude.<*> (x Lude..:? "privateDnsName")
            Lude.<*> (x Lude..:? "ipv6Addresses" Lude..!= Lude.mempty)
      )
