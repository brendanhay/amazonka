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
-- Module      : Amazonka.EC2.Types.Address
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.Address where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.DomainType
import Amazonka.EC2.Types.Tag
import qualified Amazonka.Prelude as Prelude

-- | Describes an Elastic IP address, or a carrier IP address.
--
-- /See:/ 'newAddress' smart constructor.
data Address = Address'
  { -- | Any tags assigned to the Elastic IP address.
    tags :: Prelude.Maybe [Tag],
    -- | The ID representing the allocation of the address for use with EC2-VPC.
    allocationId :: Prelude.Maybe Prelude.Text,
    -- | The name of the unique set of Availability Zones, Local Zones, or
    -- Wavelength Zones from which Amazon Web Services advertises IP addresses.
    networkBorderGroup :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether this Elastic IP address is for use with instances in
    -- EC2-Classic (@standard@) or instances in a VPC (@vpc@).
    domain :: Prelude.Maybe DomainType,
    -- | The ID of the Amazon Web Services account that owns the network
    -- interface.
    networkInterfaceOwnerId :: Prelude.Maybe Prelude.Text,
    -- | The carrier IP address associated. This option is only available for
    -- network interfaces which reside in a subnet in a Wavelength Zone (for
    -- example an EC2 instance).
    carrierIp :: Prelude.Maybe Prelude.Text,
    -- | The ID of the customer-owned address pool.
    customerOwnedIpv4Pool :: Prelude.Maybe Prelude.Text,
    -- | The Elastic IP address.
    publicIp :: Prelude.Maybe Prelude.Text,
    -- | The ID of the instance that the address is associated with (if any).
    instanceId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the network interface.
    networkInterfaceId :: Prelude.Maybe Prelude.Text,
    -- | The private IP address associated with the Elastic IP address.
    privateIpAddress :: Prelude.Maybe Prelude.Text,
    -- | The customer-owned IP address.
    customerOwnedIp :: Prelude.Maybe Prelude.Text,
    -- | The ID representing the association of the address with an instance in a
    -- VPC.
    associationId :: Prelude.Maybe Prelude.Text,
    -- | The ID of an address pool.
    publicIpv4Pool :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Address' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'address_tags' - Any tags assigned to the Elastic IP address.
--
-- 'allocationId', 'address_allocationId' - The ID representing the allocation of the address for use with EC2-VPC.
--
-- 'networkBorderGroup', 'address_networkBorderGroup' - The name of the unique set of Availability Zones, Local Zones, or
-- Wavelength Zones from which Amazon Web Services advertises IP addresses.
--
-- 'domain', 'address_domain' - Indicates whether this Elastic IP address is for use with instances in
-- EC2-Classic (@standard@) or instances in a VPC (@vpc@).
--
-- 'networkInterfaceOwnerId', 'address_networkInterfaceOwnerId' - The ID of the Amazon Web Services account that owns the network
-- interface.
--
-- 'carrierIp', 'address_carrierIp' - The carrier IP address associated. This option is only available for
-- network interfaces which reside in a subnet in a Wavelength Zone (for
-- example an EC2 instance).
--
-- 'customerOwnedIpv4Pool', 'address_customerOwnedIpv4Pool' - The ID of the customer-owned address pool.
--
-- 'publicIp', 'address_publicIp' - The Elastic IP address.
--
-- 'instanceId', 'address_instanceId' - The ID of the instance that the address is associated with (if any).
--
-- 'networkInterfaceId', 'address_networkInterfaceId' - The ID of the network interface.
--
-- 'privateIpAddress', 'address_privateIpAddress' - The private IP address associated with the Elastic IP address.
--
-- 'customerOwnedIp', 'address_customerOwnedIp' - The customer-owned IP address.
--
-- 'associationId', 'address_associationId' - The ID representing the association of the address with an instance in a
-- VPC.
--
-- 'publicIpv4Pool', 'address_publicIpv4Pool' - The ID of an address pool.
newAddress ::
  Address
newAddress =
  Address'
    { tags = Prelude.Nothing,
      allocationId = Prelude.Nothing,
      networkBorderGroup = Prelude.Nothing,
      domain = Prelude.Nothing,
      networkInterfaceOwnerId = Prelude.Nothing,
      carrierIp = Prelude.Nothing,
      customerOwnedIpv4Pool = Prelude.Nothing,
      publicIp = Prelude.Nothing,
      instanceId = Prelude.Nothing,
      networkInterfaceId = Prelude.Nothing,
      privateIpAddress = Prelude.Nothing,
      customerOwnedIp = Prelude.Nothing,
      associationId = Prelude.Nothing,
      publicIpv4Pool = Prelude.Nothing
    }

-- | Any tags assigned to the Elastic IP address.
address_tags :: Lens.Lens' Address (Prelude.Maybe [Tag])
address_tags = Lens.lens (\Address' {tags} -> tags) (\s@Address' {} a -> s {tags = a} :: Address) Prelude.. Lens.mapping Lens.coerced

-- | The ID representing the allocation of the address for use with EC2-VPC.
address_allocationId :: Lens.Lens' Address (Prelude.Maybe Prelude.Text)
address_allocationId = Lens.lens (\Address' {allocationId} -> allocationId) (\s@Address' {} a -> s {allocationId = a} :: Address)

-- | The name of the unique set of Availability Zones, Local Zones, or
-- Wavelength Zones from which Amazon Web Services advertises IP addresses.
address_networkBorderGroup :: Lens.Lens' Address (Prelude.Maybe Prelude.Text)
address_networkBorderGroup = Lens.lens (\Address' {networkBorderGroup} -> networkBorderGroup) (\s@Address' {} a -> s {networkBorderGroup = a} :: Address)

-- | Indicates whether this Elastic IP address is for use with instances in
-- EC2-Classic (@standard@) or instances in a VPC (@vpc@).
address_domain :: Lens.Lens' Address (Prelude.Maybe DomainType)
address_domain = Lens.lens (\Address' {domain} -> domain) (\s@Address' {} a -> s {domain = a} :: Address)

-- | The ID of the Amazon Web Services account that owns the network
-- interface.
address_networkInterfaceOwnerId :: Lens.Lens' Address (Prelude.Maybe Prelude.Text)
address_networkInterfaceOwnerId = Lens.lens (\Address' {networkInterfaceOwnerId} -> networkInterfaceOwnerId) (\s@Address' {} a -> s {networkInterfaceOwnerId = a} :: Address)

-- | The carrier IP address associated. This option is only available for
-- network interfaces which reside in a subnet in a Wavelength Zone (for
-- example an EC2 instance).
address_carrierIp :: Lens.Lens' Address (Prelude.Maybe Prelude.Text)
address_carrierIp = Lens.lens (\Address' {carrierIp} -> carrierIp) (\s@Address' {} a -> s {carrierIp = a} :: Address)

-- | The ID of the customer-owned address pool.
address_customerOwnedIpv4Pool :: Lens.Lens' Address (Prelude.Maybe Prelude.Text)
address_customerOwnedIpv4Pool = Lens.lens (\Address' {customerOwnedIpv4Pool} -> customerOwnedIpv4Pool) (\s@Address' {} a -> s {customerOwnedIpv4Pool = a} :: Address)

-- | The Elastic IP address.
address_publicIp :: Lens.Lens' Address (Prelude.Maybe Prelude.Text)
address_publicIp = Lens.lens (\Address' {publicIp} -> publicIp) (\s@Address' {} a -> s {publicIp = a} :: Address)

-- | The ID of the instance that the address is associated with (if any).
address_instanceId :: Lens.Lens' Address (Prelude.Maybe Prelude.Text)
address_instanceId = Lens.lens (\Address' {instanceId} -> instanceId) (\s@Address' {} a -> s {instanceId = a} :: Address)

-- | The ID of the network interface.
address_networkInterfaceId :: Lens.Lens' Address (Prelude.Maybe Prelude.Text)
address_networkInterfaceId = Lens.lens (\Address' {networkInterfaceId} -> networkInterfaceId) (\s@Address' {} a -> s {networkInterfaceId = a} :: Address)

-- | The private IP address associated with the Elastic IP address.
address_privateIpAddress :: Lens.Lens' Address (Prelude.Maybe Prelude.Text)
address_privateIpAddress = Lens.lens (\Address' {privateIpAddress} -> privateIpAddress) (\s@Address' {} a -> s {privateIpAddress = a} :: Address)

-- | The customer-owned IP address.
address_customerOwnedIp :: Lens.Lens' Address (Prelude.Maybe Prelude.Text)
address_customerOwnedIp = Lens.lens (\Address' {customerOwnedIp} -> customerOwnedIp) (\s@Address' {} a -> s {customerOwnedIp = a} :: Address)

-- | The ID representing the association of the address with an instance in a
-- VPC.
address_associationId :: Lens.Lens' Address (Prelude.Maybe Prelude.Text)
address_associationId = Lens.lens (\Address' {associationId} -> associationId) (\s@Address' {} a -> s {associationId = a} :: Address)

-- | The ID of an address pool.
address_publicIpv4Pool :: Lens.Lens' Address (Prelude.Maybe Prelude.Text)
address_publicIpv4Pool = Lens.lens (\Address' {publicIpv4Pool} -> publicIpv4Pool) (\s@Address' {} a -> s {publicIpv4Pool = a} :: Address)

instance Data.FromXML Address where
  parseXML x =
    Address'
      Prelude.<$> ( x Data..@? "tagSet" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "item")
                  )
      Prelude.<*> (x Data..@? "allocationId")
      Prelude.<*> (x Data..@? "networkBorderGroup")
      Prelude.<*> (x Data..@? "domain")
      Prelude.<*> (x Data..@? "networkInterfaceOwnerId")
      Prelude.<*> (x Data..@? "carrierIp")
      Prelude.<*> (x Data..@? "customerOwnedIpv4Pool")
      Prelude.<*> (x Data..@? "publicIp")
      Prelude.<*> (x Data..@? "instanceId")
      Prelude.<*> (x Data..@? "networkInterfaceId")
      Prelude.<*> (x Data..@? "privateIpAddress")
      Prelude.<*> (x Data..@? "customerOwnedIp")
      Prelude.<*> (x Data..@? "associationId")
      Prelude.<*> (x Data..@? "publicIpv4Pool")

instance Prelude.Hashable Address where
  hashWithSalt _salt Address' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` allocationId
      `Prelude.hashWithSalt` networkBorderGroup
      `Prelude.hashWithSalt` domain
      `Prelude.hashWithSalt` networkInterfaceOwnerId
      `Prelude.hashWithSalt` carrierIp
      `Prelude.hashWithSalt` customerOwnedIpv4Pool
      `Prelude.hashWithSalt` publicIp
      `Prelude.hashWithSalt` instanceId
      `Prelude.hashWithSalt` networkInterfaceId
      `Prelude.hashWithSalt` privateIpAddress
      `Prelude.hashWithSalt` customerOwnedIp
      `Prelude.hashWithSalt` associationId
      `Prelude.hashWithSalt` publicIpv4Pool

instance Prelude.NFData Address where
  rnf Address' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf allocationId
      `Prelude.seq` Prelude.rnf networkBorderGroup
      `Prelude.seq` Prelude.rnf domain
      `Prelude.seq` Prelude.rnf networkInterfaceOwnerId
      `Prelude.seq` Prelude.rnf carrierIp
      `Prelude.seq` Prelude.rnf customerOwnedIpv4Pool
      `Prelude.seq` Prelude.rnf publicIp
      `Prelude.seq` Prelude.rnf instanceId
      `Prelude.seq` Prelude.rnf networkInterfaceId
      `Prelude.seq` Prelude.rnf privateIpAddress
      `Prelude.seq` Prelude.rnf customerOwnedIp
      `Prelude.seq` Prelude.rnf associationId
      `Prelude.seq` Prelude.rnf publicIpv4Pool
