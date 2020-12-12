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
    niPrivateIPAddresses,
    niPublicDNSName,
    niSecurityGroups,
    niVPCId,
    niNetworkInterfaceId,
    niSubnetId,
    niPrivateIPAddress,
    niPublicIP,
    niPrivateDNSName,
    niIPv6Addresses,
  )
where

import Network.AWS.GuardDuty.Types.PrivateIPAddressDetails
import Network.AWS.GuardDuty.Types.SecurityGroup
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains information about the elastic network interface of the EC2 instance.
--
-- /See:/ 'mkNetworkInterface' smart constructor.
data NetworkInterface = NetworkInterface'
  { privateIPAddresses ::
      Lude.Maybe [PrivateIPAddressDetails],
    publicDNSName :: Lude.Maybe Lude.Text,
    securityGroups :: Lude.Maybe [SecurityGroup],
    vpcId :: Lude.Maybe Lude.Text,
    networkInterfaceId :: Lude.Maybe Lude.Text,
    subnetId :: Lude.Maybe Lude.Text,
    privateIPAddress :: Lude.Maybe Lude.Text,
    publicIP :: Lude.Maybe Lude.Text,
    privateDNSName :: Lude.Maybe Lude.Text,
    ipv6Addresses :: Lude.Maybe [Lude.Text]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'NetworkInterface' with the minimum fields required to make a request.
--
-- * 'ipv6Addresses' - A list of IPv6 addresses for the EC2 instance.
-- * 'networkInterfaceId' - The ID of the network interface.
-- * 'privateDNSName' - The private DNS name of the EC2 instance.
-- * 'privateIPAddress' - The private IP address of the EC2 instance.
-- * 'privateIPAddresses' - Other private IP address information of the EC2 instance.
-- * 'publicDNSName' - The public DNS name of the EC2 instance.
-- * 'publicIP' - The public IP address of the EC2 instance.
-- * 'securityGroups' - The security groups associated with the EC2 instance.
-- * 'subnetId' - The subnet ID of the EC2 instance.
-- * 'vpcId' - The VPC ID of the EC2 instance.
mkNetworkInterface ::
  NetworkInterface
mkNetworkInterface =
  NetworkInterface'
    { privateIPAddresses = Lude.Nothing,
      publicDNSName = Lude.Nothing,
      securityGroups = Lude.Nothing,
      vpcId = Lude.Nothing,
      networkInterfaceId = Lude.Nothing,
      subnetId = Lude.Nothing,
      privateIPAddress = Lude.Nothing,
      publicIP = Lude.Nothing,
      privateDNSName = Lude.Nothing,
      ipv6Addresses = Lude.Nothing
    }

-- | Other private IP address information of the EC2 instance.
--
-- /Note:/ Consider using 'privateIPAddresses' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
niPrivateIPAddresses :: Lens.Lens' NetworkInterface (Lude.Maybe [PrivateIPAddressDetails])
niPrivateIPAddresses = Lens.lens (privateIPAddresses :: NetworkInterface -> Lude.Maybe [PrivateIPAddressDetails]) (\s a -> s {privateIPAddresses = a} :: NetworkInterface)
{-# DEPRECATED niPrivateIPAddresses "Use generic-lens or generic-optics with 'privateIPAddresses' instead." #-}

-- | The public DNS name of the EC2 instance.
--
-- /Note:/ Consider using 'publicDNSName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
niPublicDNSName :: Lens.Lens' NetworkInterface (Lude.Maybe Lude.Text)
niPublicDNSName = Lens.lens (publicDNSName :: NetworkInterface -> Lude.Maybe Lude.Text) (\s a -> s {publicDNSName = a} :: NetworkInterface)
{-# DEPRECATED niPublicDNSName "Use generic-lens or generic-optics with 'publicDNSName' instead." #-}

-- | The security groups associated with the EC2 instance.
--
-- /Note:/ Consider using 'securityGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
niSecurityGroups :: Lens.Lens' NetworkInterface (Lude.Maybe [SecurityGroup])
niSecurityGroups = Lens.lens (securityGroups :: NetworkInterface -> Lude.Maybe [SecurityGroup]) (\s a -> s {securityGroups = a} :: NetworkInterface)
{-# DEPRECATED niSecurityGroups "Use generic-lens or generic-optics with 'securityGroups' instead." #-}

-- | The VPC ID of the EC2 instance.
--
-- /Note:/ Consider using 'vpcId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
niVPCId :: Lens.Lens' NetworkInterface (Lude.Maybe Lude.Text)
niVPCId = Lens.lens (vpcId :: NetworkInterface -> Lude.Maybe Lude.Text) (\s a -> s {vpcId = a} :: NetworkInterface)
{-# DEPRECATED niVPCId "Use generic-lens or generic-optics with 'vpcId' instead." #-}

-- | The ID of the network interface.
--
-- /Note:/ Consider using 'networkInterfaceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
niNetworkInterfaceId :: Lens.Lens' NetworkInterface (Lude.Maybe Lude.Text)
niNetworkInterfaceId = Lens.lens (networkInterfaceId :: NetworkInterface -> Lude.Maybe Lude.Text) (\s a -> s {networkInterfaceId = a} :: NetworkInterface)
{-# DEPRECATED niNetworkInterfaceId "Use generic-lens or generic-optics with 'networkInterfaceId' instead." #-}

-- | The subnet ID of the EC2 instance.
--
-- /Note:/ Consider using 'subnetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
niSubnetId :: Lens.Lens' NetworkInterface (Lude.Maybe Lude.Text)
niSubnetId = Lens.lens (subnetId :: NetworkInterface -> Lude.Maybe Lude.Text) (\s a -> s {subnetId = a} :: NetworkInterface)
{-# DEPRECATED niSubnetId "Use generic-lens or generic-optics with 'subnetId' instead." #-}

-- | The private IP address of the EC2 instance.
--
-- /Note:/ Consider using 'privateIPAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
niPrivateIPAddress :: Lens.Lens' NetworkInterface (Lude.Maybe Lude.Text)
niPrivateIPAddress = Lens.lens (privateIPAddress :: NetworkInterface -> Lude.Maybe Lude.Text) (\s a -> s {privateIPAddress = a} :: NetworkInterface)
{-# DEPRECATED niPrivateIPAddress "Use generic-lens or generic-optics with 'privateIPAddress' instead." #-}

-- | The public IP address of the EC2 instance.
--
-- /Note:/ Consider using 'publicIP' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
niPublicIP :: Lens.Lens' NetworkInterface (Lude.Maybe Lude.Text)
niPublicIP = Lens.lens (publicIP :: NetworkInterface -> Lude.Maybe Lude.Text) (\s a -> s {publicIP = a} :: NetworkInterface)
{-# DEPRECATED niPublicIP "Use generic-lens or generic-optics with 'publicIP' instead." #-}

-- | The private DNS name of the EC2 instance.
--
-- /Note:/ Consider using 'privateDNSName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
niPrivateDNSName :: Lens.Lens' NetworkInterface (Lude.Maybe Lude.Text)
niPrivateDNSName = Lens.lens (privateDNSName :: NetworkInterface -> Lude.Maybe Lude.Text) (\s a -> s {privateDNSName = a} :: NetworkInterface)
{-# DEPRECATED niPrivateDNSName "Use generic-lens or generic-optics with 'privateDNSName' instead." #-}

-- | A list of IPv6 addresses for the EC2 instance.
--
-- /Note:/ Consider using 'ipv6Addresses' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
niIPv6Addresses :: Lens.Lens' NetworkInterface (Lude.Maybe [Lude.Text])
niIPv6Addresses = Lens.lens (ipv6Addresses :: NetworkInterface -> Lude.Maybe [Lude.Text]) (\s a -> s {ipv6Addresses = a} :: NetworkInterface)
{-# DEPRECATED niIPv6Addresses "Use generic-lens or generic-optics with 'ipv6Addresses' instead." #-}

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
            Lude.<*> (x Lude..:? "networkInterfaceId")
            Lude.<*> (x Lude..:? "subnetId")
            Lude.<*> (x Lude..:? "privateIpAddress")
            Lude.<*> (x Lude..:? "publicIp")
            Lude.<*> (x Lude..:? "privateDnsName")
            Lude.<*> (x Lude..:? "ipv6Addresses" Lude..!= Lude.mempty)
      )
