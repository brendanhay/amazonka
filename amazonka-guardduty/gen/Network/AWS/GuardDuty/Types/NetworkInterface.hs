{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.Types.NetworkInterface
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GuardDuty.Types.NetworkInterface where

import Network.AWS.GuardDuty.Types.PrivateIpAddressDetails
import Network.AWS.GuardDuty.Types.SecurityGroup
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Contains information about the elastic network interface of the EC2
-- instance.
--
-- /See:/ 'newNetworkInterface' smart constructor.
data NetworkInterface = NetworkInterface'
  { -- | Other private IP address information of the EC2 instance.
    privateIpAddresses :: Prelude.Maybe [PrivateIpAddressDetails],
    -- | A list of IPv6 addresses for the EC2 instance.
    ipv6Addresses :: Prelude.Maybe [Prelude.Text],
    -- | The security groups associated with the EC2 instance.
    securityGroups :: Prelude.Maybe [SecurityGroup],
    -- | The public DNS name of the EC2 instance.
    publicDnsName :: Prelude.Maybe Prelude.Text,
    -- | The ID of the network interface.
    networkInterfaceId :: Prelude.Maybe Prelude.Text,
    -- | The subnet ID of the EC2 instance.
    subnetId :: Prelude.Maybe Prelude.Text,
    -- | The private DNS name of the EC2 instance.
    privateDnsName :: Prelude.Maybe Prelude.Text,
    -- | The public IP address of the EC2 instance.
    publicIp :: Prelude.Maybe Prelude.Text,
    -- | The VPC ID of the EC2 instance.
    vpcId :: Prelude.Maybe Prelude.Text,
    -- | The private IP address of the EC2 instance.
    privateIpAddress :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'NetworkInterface' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'privateIpAddresses', 'networkInterface_privateIpAddresses' - Other private IP address information of the EC2 instance.
--
-- 'ipv6Addresses', 'networkInterface_ipv6Addresses' - A list of IPv6 addresses for the EC2 instance.
--
-- 'securityGroups', 'networkInterface_securityGroups' - The security groups associated with the EC2 instance.
--
-- 'publicDnsName', 'networkInterface_publicDnsName' - The public DNS name of the EC2 instance.
--
-- 'networkInterfaceId', 'networkInterface_networkInterfaceId' - The ID of the network interface.
--
-- 'subnetId', 'networkInterface_subnetId' - The subnet ID of the EC2 instance.
--
-- 'privateDnsName', 'networkInterface_privateDnsName' - The private DNS name of the EC2 instance.
--
-- 'publicIp', 'networkInterface_publicIp' - The public IP address of the EC2 instance.
--
-- 'vpcId', 'networkInterface_vpcId' - The VPC ID of the EC2 instance.
--
-- 'privateIpAddress', 'networkInterface_privateIpAddress' - The private IP address of the EC2 instance.
newNetworkInterface ::
  NetworkInterface
newNetworkInterface =
  NetworkInterface'
    { privateIpAddresses =
        Prelude.Nothing,
      ipv6Addresses = Prelude.Nothing,
      securityGroups = Prelude.Nothing,
      publicDnsName = Prelude.Nothing,
      networkInterfaceId = Prelude.Nothing,
      subnetId = Prelude.Nothing,
      privateDnsName = Prelude.Nothing,
      publicIp = Prelude.Nothing,
      vpcId = Prelude.Nothing,
      privateIpAddress = Prelude.Nothing
    }

-- | Other private IP address information of the EC2 instance.
networkInterface_privateIpAddresses :: Lens.Lens' NetworkInterface (Prelude.Maybe [PrivateIpAddressDetails])
networkInterface_privateIpAddresses = Lens.lens (\NetworkInterface' {privateIpAddresses} -> privateIpAddresses) (\s@NetworkInterface' {} a -> s {privateIpAddresses = a} :: NetworkInterface) Prelude.. Lens.mapping Prelude._Coerce

-- | A list of IPv6 addresses for the EC2 instance.
networkInterface_ipv6Addresses :: Lens.Lens' NetworkInterface (Prelude.Maybe [Prelude.Text])
networkInterface_ipv6Addresses = Lens.lens (\NetworkInterface' {ipv6Addresses} -> ipv6Addresses) (\s@NetworkInterface' {} a -> s {ipv6Addresses = a} :: NetworkInterface) Prelude.. Lens.mapping Prelude._Coerce

-- | The security groups associated with the EC2 instance.
networkInterface_securityGroups :: Lens.Lens' NetworkInterface (Prelude.Maybe [SecurityGroup])
networkInterface_securityGroups = Lens.lens (\NetworkInterface' {securityGroups} -> securityGroups) (\s@NetworkInterface' {} a -> s {securityGroups = a} :: NetworkInterface) Prelude.. Lens.mapping Prelude._Coerce

-- | The public DNS name of the EC2 instance.
networkInterface_publicDnsName :: Lens.Lens' NetworkInterface (Prelude.Maybe Prelude.Text)
networkInterface_publicDnsName = Lens.lens (\NetworkInterface' {publicDnsName} -> publicDnsName) (\s@NetworkInterface' {} a -> s {publicDnsName = a} :: NetworkInterface)

-- | The ID of the network interface.
networkInterface_networkInterfaceId :: Lens.Lens' NetworkInterface (Prelude.Maybe Prelude.Text)
networkInterface_networkInterfaceId = Lens.lens (\NetworkInterface' {networkInterfaceId} -> networkInterfaceId) (\s@NetworkInterface' {} a -> s {networkInterfaceId = a} :: NetworkInterface)

-- | The subnet ID of the EC2 instance.
networkInterface_subnetId :: Lens.Lens' NetworkInterface (Prelude.Maybe Prelude.Text)
networkInterface_subnetId = Lens.lens (\NetworkInterface' {subnetId} -> subnetId) (\s@NetworkInterface' {} a -> s {subnetId = a} :: NetworkInterface)

-- | The private DNS name of the EC2 instance.
networkInterface_privateDnsName :: Lens.Lens' NetworkInterface (Prelude.Maybe Prelude.Text)
networkInterface_privateDnsName = Lens.lens (\NetworkInterface' {privateDnsName} -> privateDnsName) (\s@NetworkInterface' {} a -> s {privateDnsName = a} :: NetworkInterface)

-- | The public IP address of the EC2 instance.
networkInterface_publicIp :: Lens.Lens' NetworkInterface (Prelude.Maybe Prelude.Text)
networkInterface_publicIp = Lens.lens (\NetworkInterface' {publicIp} -> publicIp) (\s@NetworkInterface' {} a -> s {publicIp = a} :: NetworkInterface)

-- | The VPC ID of the EC2 instance.
networkInterface_vpcId :: Lens.Lens' NetworkInterface (Prelude.Maybe Prelude.Text)
networkInterface_vpcId = Lens.lens (\NetworkInterface' {vpcId} -> vpcId) (\s@NetworkInterface' {} a -> s {vpcId = a} :: NetworkInterface)

-- | The private IP address of the EC2 instance.
networkInterface_privateIpAddress :: Lens.Lens' NetworkInterface (Prelude.Maybe Prelude.Text)
networkInterface_privateIpAddress = Lens.lens (\NetworkInterface' {privateIpAddress} -> privateIpAddress) (\s@NetworkInterface' {} a -> s {privateIpAddress = a} :: NetworkInterface)

instance Prelude.FromJSON NetworkInterface where
  parseJSON =
    Prelude.withObject
      "NetworkInterface"
      ( \x ->
          NetworkInterface'
            Prelude.<$> ( x Prelude..:? "privateIpAddresses"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> ( x Prelude..:? "ipv6Addresses"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> ( x Prelude..:? "securityGroups"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..:? "publicDnsName")
            Prelude.<*> (x Prelude..:? "networkInterfaceId")
            Prelude.<*> (x Prelude..:? "subnetId")
            Prelude.<*> (x Prelude..:? "privateDnsName")
            Prelude.<*> (x Prelude..:? "publicIp")
            Prelude.<*> (x Prelude..:? "vpcId")
            Prelude.<*> (x Prelude..:? "privateIpAddress")
      )

instance Prelude.Hashable NetworkInterface

instance Prelude.NFData NetworkInterface
