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
-- Module      : Network.AWS.Inspector.Types.NetworkInterface
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Inspector.Types.NetworkInterface where

import Network.AWS.Inspector.Types.PrivateIp
import Network.AWS.Inspector.Types.SecurityGroup
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Contains information about the network interfaces interacting with an
-- EC2 instance. This data type is used as one of the elements of the
-- AssetAttributes data type.
--
-- /See:/ 'newNetworkInterface' smart constructor.
data NetworkInterface = NetworkInterface'
  { -- | A list of the private IP addresses associated with the network
    -- interface. Includes the privateDnsName and privateIpAddress.
    privateIpAddresses :: Prelude.Maybe [PrivateIp],
    -- | The IP addresses associated with the network interface.
    ipv6Addresses :: Prelude.Maybe [Prelude.Text],
    -- | A list of the security groups associated with the network interface.
    -- Includes the groupId and groupName.
    securityGroups :: Prelude.Maybe [SecurityGroup],
    -- | The name of a public DNS associated with the network interface.
    publicDnsName :: Prelude.Maybe Prelude.Text,
    -- | The ID of a subnet associated with the network interface.
    subnetId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the network interface.
    networkInterfaceId :: Prelude.Maybe Prelude.Text,
    -- | The name of a private DNS associated with the network interface.
    privateDnsName :: Prelude.Maybe Prelude.Text,
    -- | The ID of a VPC associated with the network interface.
    vpcId :: Prelude.Maybe Prelude.Text,
    -- | The public IP address from which the network interface is reachable.
    publicIp :: Prelude.Maybe Prelude.Text,
    -- | The private IP address associated with the network interface.
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
-- 'privateIpAddresses', 'networkInterface_privateIpAddresses' - A list of the private IP addresses associated with the network
-- interface. Includes the privateDnsName and privateIpAddress.
--
-- 'ipv6Addresses', 'networkInterface_ipv6Addresses' - The IP addresses associated with the network interface.
--
-- 'securityGroups', 'networkInterface_securityGroups' - A list of the security groups associated with the network interface.
-- Includes the groupId and groupName.
--
-- 'publicDnsName', 'networkInterface_publicDnsName' - The name of a public DNS associated with the network interface.
--
-- 'subnetId', 'networkInterface_subnetId' - The ID of a subnet associated with the network interface.
--
-- 'networkInterfaceId', 'networkInterface_networkInterfaceId' - The ID of the network interface.
--
-- 'privateDnsName', 'networkInterface_privateDnsName' - The name of a private DNS associated with the network interface.
--
-- 'vpcId', 'networkInterface_vpcId' - The ID of a VPC associated with the network interface.
--
-- 'publicIp', 'networkInterface_publicIp' - The public IP address from which the network interface is reachable.
--
-- 'privateIpAddress', 'networkInterface_privateIpAddress' - The private IP address associated with the network interface.
newNetworkInterface ::
  NetworkInterface
newNetworkInterface =
  NetworkInterface'
    { privateIpAddresses =
        Prelude.Nothing,
      ipv6Addresses = Prelude.Nothing,
      securityGroups = Prelude.Nothing,
      publicDnsName = Prelude.Nothing,
      subnetId = Prelude.Nothing,
      networkInterfaceId = Prelude.Nothing,
      privateDnsName = Prelude.Nothing,
      vpcId = Prelude.Nothing,
      publicIp = Prelude.Nothing,
      privateIpAddress = Prelude.Nothing
    }

-- | A list of the private IP addresses associated with the network
-- interface. Includes the privateDnsName and privateIpAddress.
networkInterface_privateIpAddresses :: Lens.Lens' NetworkInterface (Prelude.Maybe [PrivateIp])
networkInterface_privateIpAddresses = Lens.lens (\NetworkInterface' {privateIpAddresses} -> privateIpAddresses) (\s@NetworkInterface' {} a -> s {privateIpAddresses = a} :: NetworkInterface) Prelude.. Lens.mapping Prelude._Coerce

-- | The IP addresses associated with the network interface.
networkInterface_ipv6Addresses :: Lens.Lens' NetworkInterface (Prelude.Maybe [Prelude.Text])
networkInterface_ipv6Addresses = Lens.lens (\NetworkInterface' {ipv6Addresses} -> ipv6Addresses) (\s@NetworkInterface' {} a -> s {ipv6Addresses = a} :: NetworkInterface) Prelude.. Lens.mapping Prelude._Coerce

-- | A list of the security groups associated with the network interface.
-- Includes the groupId and groupName.
networkInterface_securityGroups :: Lens.Lens' NetworkInterface (Prelude.Maybe [SecurityGroup])
networkInterface_securityGroups = Lens.lens (\NetworkInterface' {securityGroups} -> securityGroups) (\s@NetworkInterface' {} a -> s {securityGroups = a} :: NetworkInterface) Prelude.. Lens.mapping Prelude._Coerce

-- | The name of a public DNS associated with the network interface.
networkInterface_publicDnsName :: Lens.Lens' NetworkInterface (Prelude.Maybe Prelude.Text)
networkInterface_publicDnsName = Lens.lens (\NetworkInterface' {publicDnsName} -> publicDnsName) (\s@NetworkInterface' {} a -> s {publicDnsName = a} :: NetworkInterface)

-- | The ID of a subnet associated with the network interface.
networkInterface_subnetId :: Lens.Lens' NetworkInterface (Prelude.Maybe Prelude.Text)
networkInterface_subnetId = Lens.lens (\NetworkInterface' {subnetId} -> subnetId) (\s@NetworkInterface' {} a -> s {subnetId = a} :: NetworkInterface)

-- | The ID of the network interface.
networkInterface_networkInterfaceId :: Lens.Lens' NetworkInterface (Prelude.Maybe Prelude.Text)
networkInterface_networkInterfaceId = Lens.lens (\NetworkInterface' {networkInterfaceId} -> networkInterfaceId) (\s@NetworkInterface' {} a -> s {networkInterfaceId = a} :: NetworkInterface)

-- | The name of a private DNS associated with the network interface.
networkInterface_privateDnsName :: Lens.Lens' NetworkInterface (Prelude.Maybe Prelude.Text)
networkInterface_privateDnsName = Lens.lens (\NetworkInterface' {privateDnsName} -> privateDnsName) (\s@NetworkInterface' {} a -> s {privateDnsName = a} :: NetworkInterface)

-- | The ID of a VPC associated with the network interface.
networkInterface_vpcId :: Lens.Lens' NetworkInterface (Prelude.Maybe Prelude.Text)
networkInterface_vpcId = Lens.lens (\NetworkInterface' {vpcId} -> vpcId) (\s@NetworkInterface' {} a -> s {vpcId = a} :: NetworkInterface)

-- | The public IP address from which the network interface is reachable.
networkInterface_publicIp :: Lens.Lens' NetworkInterface (Prelude.Maybe Prelude.Text)
networkInterface_publicIp = Lens.lens (\NetworkInterface' {publicIp} -> publicIp) (\s@NetworkInterface' {} a -> s {publicIp = a} :: NetworkInterface)

-- | The private IP address associated with the network interface.
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
            Prelude.<*> (x Prelude..:? "subnetId")
            Prelude.<*> (x Prelude..:? "networkInterfaceId")
            Prelude.<*> (x Prelude..:? "privateDnsName")
            Prelude.<*> (x Prelude..:? "vpcId")
            Prelude.<*> (x Prelude..:? "publicIp")
            Prelude.<*> (x Prelude..:? "privateIpAddress")
      )

instance Prelude.Hashable NetworkInterface

instance Prelude.NFData NetworkInterface
