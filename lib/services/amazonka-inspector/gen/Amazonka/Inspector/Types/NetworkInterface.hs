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
-- Module      : Amazonka.Inspector.Types.NetworkInterface
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Inspector.Types.NetworkInterface where

import qualified Amazonka.Core as Core
import Amazonka.Inspector.Types.PrivateIp
import Amazonka.Inspector.Types.SecurityGroup
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Contains information about the network interfaces interacting with an
-- EC2 instance. This data type is used as one of the elements of the
-- AssetAttributes data type.
--
-- /See:/ 'newNetworkInterface' smart constructor.
data NetworkInterface = NetworkInterface'
  { -- | A list of the private IP addresses associated with the network
    -- interface. Includes the privateDnsName and privateIpAddress.
    privateIpAddresses :: Prelude.Maybe [PrivateIp],
    -- | The name of a public DNS associated with the network interface.
    publicDnsName :: Prelude.Maybe Prelude.Text,
    -- | A list of the security groups associated with the network interface.
    -- Includes the groupId and groupName.
    securityGroups :: Prelude.Maybe [SecurityGroup],
    -- | The ID of a VPC associated with the network interface.
    vpcId :: Prelude.Maybe Prelude.Text,
    -- | The ID of a subnet associated with the network interface.
    subnetId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the network interface.
    networkInterfaceId :: Prelude.Maybe Prelude.Text,
    -- | The private IP address associated with the network interface.
    privateIpAddress :: Prelude.Maybe Prelude.Text,
    -- | The public IP address from which the network interface is reachable.
    publicIp :: Prelude.Maybe Prelude.Text,
    -- | The name of a private DNS associated with the network interface.
    privateDnsName :: Prelude.Maybe Prelude.Text,
    -- | The IP addresses associated with the network interface.
    ipv6Addresses :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'publicDnsName', 'networkInterface_publicDnsName' - The name of a public DNS associated with the network interface.
--
-- 'securityGroups', 'networkInterface_securityGroups' - A list of the security groups associated with the network interface.
-- Includes the groupId and groupName.
--
-- 'vpcId', 'networkInterface_vpcId' - The ID of a VPC associated with the network interface.
--
-- 'subnetId', 'networkInterface_subnetId' - The ID of a subnet associated with the network interface.
--
-- 'networkInterfaceId', 'networkInterface_networkInterfaceId' - The ID of the network interface.
--
-- 'privateIpAddress', 'networkInterface_privateIpAddress' - The private IP address associated with the network interface.
--
-- 'publicIp', 'networkInterface_publicIp' - The public IP address from which the network interface is reachable.
--
-- 'privateDnsName', 'networkInterface_privateDnsName' - The name of a private DNS associated with the network interface.
--
-- 'ipv6Addresses', 'networkInterface_ipv6Addresses' - The IP addresses associated with the network interface.
newNetworkInterface ::
  NetworkInterface
newNetworkInterface =
  NetworkInterface'
    { privateIpAddresses =
        Prelude.Nothing,
      publicDnsName = Prelude.Nothing,
      securityGroups = Prelude.Nothing,
      vpcId = Prelude.Nothing,
      subnetId = Prelude.Nothing,
      networkInterfaceId = Prelude.Nothing,
      privateIpAddress = Prelude.Nothing,
      publicIp = Prelude.Nothing,
      privateDnsName = Prelude.Nothing,
      ipv6Addresses = Prelude.Nothing
    }

-- | A list of the private IP addresses associated with the network
-- interface. Includes the privateDnsName and privateIpAddress.
networkInterface_privateIpAddresses :: Lens.Lens' NetworkInterface (Prelude.Maybe [PrivateIp])
networkInterface_privateIpAddresses = Lens.lens (\NetworkInterface' {privateIpAddresses} -> privateIpAddresses) (\s@NetworkInterface' {} a -> s {privateIpAddresses = a} :: NetworkInterface) Prelude.. Lens.mapping Lens.coerced

-- | The name of a public DNS associated with the network interface.
networkInterface_publicDnsName :: Lens.Lens' NetworkInterface (Prelude.Maybe Prelude.Text)
networkInterface_publicDnsName = Lens.lens (\NetworkInterface' {publicDnsName} -> publicDnsName) (\s@NetworkInterface' {} a -> s {publicDnsName = a} :: NetworkInterface)

-- | A list of the security groups associated with the network interface.
-- Includes the groupId and groupName.
networkInterface_securityGroups :: Lens.Lens' NetworkInterface (Prelude.Maybe [SecurityGroup])
networkInterface_securityGroups = Lens.lens (\NetworkInterface' {securityGroups} -> securityGroups) (\s@NetworkInterface' {} a -> s {securityGroups = a} :: NetworkInterface) Prelude.. Lens.mapping Lens.coerced

-- | The ID of a VPC associated with the network interface.
networkInterface_vpcId :: Lens.Lens' NetworkInterface (Prelude.Maybe Prelude.Text)
networkInterface_vpcId = Lens.lens (\NetworkInterface' {vpcId} -> vpcId) (\s@NetworkInterface' {} a -> s {vpcId = a} :: NetworkInterface)

-- | The ID of a subnet associated with the network interface.
networkInterface_subnetId :: Lens.Lens' NetworkInterface (Prelude.Maybe Prelude.Text)
networkInterface_subnetId = Lens.lens (\NetworkInterface' {subnetId} -> subnetId) (\s@NetworkInterface' {} a -> s {subnetId = a} :: NetworkInterface)

-- | The ID of the network interface.
networkInterface_networkInterfaceId :: Lens.Lens' NetworkInterface (Prelude.Maybe Prelude.Text)
networkInterface_networkInterfaceId = Lens.lens (\NetworkInterface' {networkInterfaceId} -> networkInterfaceId) (\s@NetworkInterface' {} a -> s {networkInterfaceId = a} :: NetworkInterface)

-- | The private IP address associated with the network interface.
networkInterface_privateIpAddress :: Lens.Lens' NetworkInterface (Prelude.Maybe Prelude.Text)
networkInterface_privateIpAddress = Lens.lens (\NetworkInterface' {privateIpAddress} -> privateIpAddress) (\s@NetworkInterface' {} a -> s {privateIpAddress = a} :: NetworkInterface)

-- | The public IP address from which the network interface is reachable.
networkInterface_publicIp :: Lens.Lens' NetworkInterface (Prelude.Maybe Prelude.Text)
networkInterface_publicIp = Lens.lens (\NetworkInterface' {publicIp} -> publicIp) (\s@NetworkInterface' {} a -> s {publicIp = a} :: NetworkInterface)

-- | The name of a private DNS associated with the network interface.
networkInterface_privateDnsName :: Lens.Lens' NetworkInterface (Prelude.Maybe Prelude.Text)
networkInterface_privateDnsName = Lens.lens (\NetworkInterface' {privateDnsName} -> privateDnsName) (\s@NetworkInterface' {} a -> s {privateDnsName = a} :: NetworkInterface)

-- | The IP addresses associated with the network interface.
networkInterface_ipv6Addresses :: Lens.Lens' NetworkInterface (Prelude.Maybe [Prelude.Text])
networkInterface_ipv6Addresses = Lens.lens (\NetworkInterface' {ipv6Addresses} -> ipv6Addresses) (\s@NetworkInterface' {} a -> s {ipv6Addresses = a} :: NetworkInterface) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON NetworkInterface where
  parseJSON =
    Core.withObject
      "NetworkInterface"
      ( \x ->
          NetworkInterface'
            Prelude.<$> ( x Core..:? "privateIpAddresses"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "publicDnsName")
            Prelude.<*> (x Core..:? "securityGroups" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "vpcId")
            Prelude.<*> (x Core..:? "subnetId")
            Prelude.<*> (x Core..:? "networkInterfaceId")
            Prelude.<*> (x Core..:? "privateIpAddress")
            Prelude.<*> (x Core..:? "publicIp")
            Prelude.<*> (x Core..:? "privateDnsName")
            Prelude.<*> (x Core..:? "ipv6Addresses" Core..!= Prelude.mempty)
      )

instance Prelude.Hashable NetworkInterface where
  hashWithSalt salt' NetworkInterface' {..} =
    salt' `Prelude.hashWithSalt` ipv6Addresses
      `Prelude.hashWithSalt` privateDnsName
      `Prelude.hashWithSalt` publicIp
      `Prelude.hashWithSalt` privateIpAddress
      `Prelude.hashWithSalt` networkInterfaceId
      `Prelude.hashWithSalt` subnetId
      `Prelude.hashWithSalt` vpcId
      `Prelude.hashWithSalt` securityGroups
      `Prelude.hashWithSalt` publicDnsName
      `Prelude.hashWithSalt` privateIpAddresses

instance Prelude.NFData NetworkInterface where
  rnf NetworkInterface' {..} =
    Prelude.rnf privateIpAddresses
      `Prelude.seq` Prelude.rnf ipv6Addresses
      `Prelude.seq` Prelude.rnf privateDnsName
      `Prelude.seq` Prelude.rnf publicIp
      `Prelude.seq` Prelude.rnf privateIpAddress
      `Prelude.seq` Prelude.rnf networkInterfaceId
      `Prelude.seq` Prelude.rnf subnetId
      `Prelude.seq` Prelude.rnf vpcId
      `Prelude.seq` Prelude.rnf securityGroups
      `Prelude.seq` Prelude.rnf publicDnsName
