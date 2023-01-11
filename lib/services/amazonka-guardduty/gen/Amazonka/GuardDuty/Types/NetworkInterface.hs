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
-- Module      : Amazonka.GuardDuty.Types.NetworkInterface
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GuardDuty.Types.NetworkInterface where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GuardDuty.Types.PrivateIpAddressDetails
import Amazonka.GuardDuty.Types.SecurityGroup
import qualified Amazonka.Prelude as Prelude

-- | Contains information about the elastic network interface of the EC2
-- instance.
--
-- /See:/ 'newNetworkInterface' smart constructor.
data NetworkInterface = NetworkInterface'
  { -- | A list of IPv6 addresses for the EC2 instance.
    ipv6Addresses :: Prelude.Maybe [Prelude.Text],
    -- | The ID of the network interface.
    networkInterfaceId :: Prelude.Maybe Prelude.Text,
    -- | The private DNS name of the EC2 instance.
    privateDnsName :: Prelude.Maybe Prelude.Text,
    -- | The private IP address of the EC2 instance.
    privateIpAddress :: Prelude.Maybe Prelude.Text,
    -- | Other private IP address information of the EC2 instance.
    privateIpAddresses :: Prelude.Maybe [PrivateIpAddressDetails],
    -- | The public DNS name of the EC2 instance.
    publicDnsName :: Prelude.Maybe Prelude.Text,
    -- | The public IP address of the EC2 instance.
    publicIp :: Prelude.Maybe Prelude.Text,
    -- | The security groups associated with the EC2 instance.
    securityGroups :: Prelude.Maybe [SecurityGroup],
    -- | The subnet ID of the EC2 instance.
    subnetId :: Prelude.Maybe Prelude.Text,
    -- | The VPC ID of the EC2 instance.
    vpcId :: Prelude.Maybe Prelude.Text
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
-- 'ipv6Addresses', 'networkInterface_ipv6Addresses' - A list of IPv6 addresses for the EC2 instance.
--
-- 'networkInterfaceId', 'networkInterface_networkInterfaceId' - The ID of the network interface.
--
-- 'privateDnsName', 'networkInterface_privateDnsName' - The private DNS name of the EC2 instance.
--
-- 'privateIpAddress', 'networkInterface_privateIpAddress' - The private IP address of the EC2 instance.
--
-- 'privateIpAddresses', 'networkInterface_privateIpAddresses' - Other private IP address information of the EC2 instance.
--
-- 'publicDnsName', 'networkInterface_publicDnsName' - The public DNS name of the EC2 instance.
--
-- 'publicIp', 'networkInterface_publicIp' - The public IP address of the EC2 instance.
--
-- 'securityGroups', 'networkInterface_securityGroups' - The security groups associated with the EC2 instance.
--
-- 'subnetId', 'networkInterface_subnetId' - The subnet ID of the EC2 instance.
--
-- 'vpcId', 'networkInterface_vpcId' - The VPC ID of the EC2 instance.
newNetworkInterface ::
  NetworkInterface
newNetworkInterface =
  NetworkInterface'
    { ipv6Addresses = Prelude.Nothing,
      networkInterfaceId = Prelude.Nothing,
      privateDnsName = Prelude.Nothing,
      privateIpAddress = Prelude.Nothing,
      privateIpAddresses = Prelude.Nothing,
      publicDnsName = Prelude.Nothing,
      publicIp = Prelude.Nothing,
      securityGroups = Prelude.Nothing,
      subnetId = Prelude.Nothing,
      vpcId = Prelude.Nothing
    }

-- | A list of IPv6 addresses for the EC2 instance.
networkInterface_ipv6Addresses :: Lens.Lens' NetworkInterface (Prelude.Maybe [Prelude.Text])
networkInterface_ipv6Addresses = Lens.lens (\NetworkInterface' {ipv6Addresses} -> ipv6Addresses) (\s@NetworkInterface' {} a -> s {ipv6Addresses = a} :: NetworkInterface) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the network interface.
networkInterface_networkInterfaceId :: Lens.Lens' NetworkInterface (Prelude.Maybe Prelude.Text)
networkInterface_networkInterfaceId = Lens.lens (\NetworkInterface' {networkInterfaceId} -> networkInterfaceId) (\s@NetworkInterface' {} a -> s {networkInterfaceId = a} :: NetworkInterface)

-- | The private DNS name of the EC2 instance.
networkInterface_privateDnsName :: Lens.Lens' NetworkInterface (Prelude.Maybe Prelude.Text)
networkInterface_privateDnsName = Lens.lens (\NetworkInterface' {privateDnsName} -> privateDnsName) (\s@NetworkInterface' {} a -> s {privateDnsName = a} :: NetworkInterface)

-- | The private IP address of the EC2 instance.
networkInterface_privateIpAddress :: Lens.Lens' NetworkInterface (Prelude.Maybe Prelude.Text)
networkInterface_privateIpAddress = Lens.lens (\NetworkInterface' {privateIpAddress} -> privateIpAddress) (\s@NetworkInterface' {} a -> s {privateIpAddress = a} :: NetworkInterface)

-- | Other private IP address information of the EC2 instance.
networkInterface_privateIpAddresses :: Lens.Lens' NetworkInterface (Prelude.Maybe [PrivateIpAddressDetails])
networkInterface_privateIpAddresses = Lens.lens (\NetworkInterface' {privateIpAddresses} -> privateIpAddresses) (\s@NetworkInterface' {} a -> s {privateIpAddresses = a} :: NetworkInterface) Prelude.. Lens.mapping Lens.coerced

-- | The public DNS name of the EC2 instance.
networkInterface_publicDnsName :: Lens.Lens' NetworkInterface (Prelude.Maybe Prelude.Text)
networkInterface_publicDnsName = Lens.lens (\NetworkInterface' {publicDnsName} -> publicDnsName) (\s@NetworkInterface' {} a -> s {publicDnsName = a} :: NetworkInterface)

-- | The public IP address of the EC2 instance.
networkInterface_publicIp :: Lens.Lens' NetworkInterface (Prelude.Maybe Prelude.Text)
networkInterface_publicIp = Lens.lens (\NetworkInterface' {publicIp} -> publicIp) (\s@NetworkInterface' {} a -> s {publicIp = a} :: NetworkInterface)

-- | The security groups associated with the EC2 instance.
networkInterface_securityGroups :: Lens.Lens' NetworkInterface (Prelude.Maybe [SecurityGroup])
networkInterface_securityGroups = Lens.lens (\NetworkInterface' {securityGroups} -> securityGroups) (\s@NetworkInterface' {} a -> s {securityGroups = a} :: NetworkInterface) Prelude.. Lens.mapping Lens.coerced

-- | The subnet ID of the EC2 instance.
networkInterface_subnetId :: Lens.Lens' NetworkInterface (Prelude.Maybe Prelude.Text)
networkInterface_subnetId = Lens.lens (\NetworkInterface' {subnetId} -> subnetId) (\s@NetworkInterface' {} a -> s {subnetId = a} :: NetworkInterface)

-- | The VPC ID of the EC2 instance.
networkInterface_vpcId :: Lens.Lens' NetworkInterface (Prelude.Maybe Prelude.Text)
networkInterface_vpcId = Lens.lens (\NetworkInterface' {vpcId} -> vpcId) (\s@NetworkInterface' {} a -> s {vpcId = a} :: NetworkInterface)

instance Data.FromJSON NetworkInterface where
  parseJSON =
    Data.withObject
      "NetworkInterface"
      ( \x ->
          NetworkInterface'
            Prelude.<$> (x Data..:? "ipv6Addresses" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "networkInterfaceId")
            Prelude.<*> (x Data..:? "privateDnsName")
            Prelude.<*> (x Data..:? "privateIpAddress")
            Prelude.<*> ( x Data..:? "privateIpAddresses"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "publicDnsName")
            Prelude.<*> (x Data..:? "publicIp")
            Prelude.<*> (x Data..:? "securityGroups" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "subnetId")
            Prelude.<*> (x Data..:? "vpcId")
      )

instance Prelude.Hashable NetworkInterface where
  hashWithSalt _salt NetworkInterface' {..} =
    _salt `Prelude.hashWithSalt` ipv6Addresses
      `Prelude.hashWithSalt` networkInterfaceId
      `Prelude.hashWithSalt` privateDnsName
      `Prelude.hashWithSalt` privateIpAddress
      `Prelude.hashWithSalt` privateIpAddresses
      `Prelude.hashWithSalt` publicDnsName
      `Prelude.hashWithSalt` publicIp
      `Prelude.hashWithSalt` securityGroups
      `Prelude.hashWithSalt` subnetId
      `Prelude.hashWithSalt` vpcId

instance Prelude.NFData NetworkInterface where
  rnf NetworkInterface' {..} =
    Prelude.rnf ipv6Addresses
      `Prelude.seq` Prelude.rnf networkInterfaceId
      `Prelude.seq` Prelude.rnf privateDnsName
      `Prelude.seq` Prelude.rnf privateIpAddress
      `Prelude.seq` Prelude.rnf privateIpAddresses
      `Prelude.seq` Prelude.rnf publicDnsName
      `Prelude.seq` Prelude.rnf publicIp
      `Prelude.seq` Prelude.rnf securityGroups
      `Prelude.seq` Prelude.rnf subnetId
      `Prelude.seq` Prelude.rnf vpcId
