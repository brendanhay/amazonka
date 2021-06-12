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
-- Module      : Network.AWS.EC2.Types.Address
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.Address where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.DomainType
import Network.AWS.EC2.Types.Tag
import qualified Network.AWS.Lens as Lens

-- | Describes an Elastic IP address, or a carrier IP address.
--
-- /See:/ 'newAddress' smart constructor.
data Address = Address'
  { -- | The ID of the customer-owned address pool.
    customerOwnedIpv4Pool :: Core.Maybe Core.Text,
    -- | The ID of the instance that the address is associated with (if any).
    instanceId :: Core.Maybe Core.Text,
    -- | Indicates whether this Elastic IP address is for use with instances in
    -- EC2-Classic (@standard@) or instances in a VPC (@vpc@).
    domain :: Core.Maybe DomainType,
    -- | The carrier IP address associated. This option is only available for
    -- network interfaces which reside in a subnet in a Wavelength Zone (for
    -- example an EC2 instance).
    carrierIp :: Core.Maybe Core.Text,
    -- | The customer-owned IP address.
    customerOwnedIp :: Core.Maybe Core.Text,
    -- | The ID of the AWS account that owns the network interface.
    networkInterfaceOwnerId :: Core.Maybe Core.Text,
    -- | The ID representing the association of the address with an instance in a
    -- VPC.
    associationId :: Core.Maybe Core.Text,
    -- | Any tags assigned to the Elastic IP address.
    tags :: Core.Maybe [Tag],
    -- | The ID of the network interface.
    networkInterfaceId :: Core.Maybe Core.Text,
    -- | The ID of an address pool.
    publicIpv4Pool :: Core.Maybe Core.Text,
    -- | The Elastic IP address.
    publicIp :: Core.Maybe Core.Text,
    -- | The ID representing the allocation of the address for use with EC2-VPC.
    allocationId :: Core.Maybe Core.Text,
    -- | The name of the unique set of Availability Zones, Local Zones, or
    -- Wavelength Zones from which AWS advertises IP addresses.
    networkBorderGroup :: Core.Maybe Core.Text,
    -- | The private IP address associated with the Elastic IP address.
    privateIpAddress :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Address' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'customerOwnedIpv4Pool', 'address_customerOwnedIpv4Pool' - The ID of the customer-owned address pool.
--
-- 'instanceId', 'address_instanceId' - The ID of the instance that the address is associated with (if any).
--
-- 'domain', 'address_domain' - Indicates whether this Elastic IP address is for use with instances in
-- EC2-Classic (@standard@) or instances in a VPC (@vpc@).
--
-- 'carrierIp', 'address_carrierIp' - The carrier IP address associated. This option is only available for
-- network interfaces which reside in a subnet in a Wavelength Zone (for
-- example an EC2 instance).
--
-- 'customerOwnedIp', 'address_customerOwnedIp' - The customer-owned IP address.
--
-- 'networkInterfaceOwnerId', 'address_networkInterfaceOwnerId' - The ID of the AWS account that owns the network interface.
--
-- 'associationId', 'address_associationId' - The ID representing the association of the address with an instance in a
-- VPC.
--
-- 'tags', 'address_tags' - Any tags assigned to the Elastic IP address.
--
-- 'networkInterfaceId', 'address_networkInterfaceId' - The ID of the network interface.
--
-- 'publicIpv4Pool', 'address_publicIpv4Pool' - The ID of an address pool.
--
-- 'publicIp', 'address_publicIp' - The Elastic IP address.
--
-- 'allocationId', 'address_allocationId' - The ID representing the allocation of the address for use with EC2-VPC.
--
-- 'networkBorderGroup', 'address_networkBorderGroup' - The name of the unique set of Availability Zones, Local Zones, or
-- Wavelength Zones from which AWS advertises IP addresses.
--
-- 'privateIpAddress', 'address_privateIpAddress' - The private IP address associated with the Elastic IP address.
newAddress ::
  Address
newAddress =
  Address'
    { customerOwnedIpv4Pool = Core.Nothing,
      instanceId = Core.Nothing,
      domain = Core.Nothing,
      carrierIp = Core.Nothing,
      customerOwnedIp = Core.Nothing,
      networkInterfaceOwnerId = Core.Nothing,
      associationId = Core.Nothing,
      tags = Core.Nothing,
      networkInterfaceId = Core.Nothing,
      publicIpv4Pool = Core.Nothing,
      publicIp = Core.Nothing,
      allocationId = Core.Nothing,
      networkBorderGroup = Core.Nothing,
      privateIpAddress = Core.Nothing
    }

-- | The ID of the customer-owned address pool.
address_customerOwnedIpv4Pool :: Lens.Lens' Address (Core.Maybe Core.Text)
address_customerOwnedIpv4Pool = Lens.lens (\Address' {customerOwnedIpv4Pool} -> customerOwnedIpv4Pool) (\s@Address' {} a -> s {customerOwnedIpv4Pool = a} :: Address)

-- | The ID of the instance that the address is associated with (if any).
address_instanceId :: Lens.Lens' Address (Core.Maybe Core.Text)
address_instanceId = Lens.lens (\Address' {instanceId} -> instanceId) (\s@Address' {} a -> s {instanceId = a} :: Address)

-- | Indicates whether this Elastic IP address is for use with instances in
-- EC2-Classic (@standard@) or instances in a VPC (@vpc@).
address_domain :: Lens.Lens' Address (Core.Maybe DomainType)
address_domain = Lens.lens (\Address' {domain} -> domain) (\s@Address' {} a -> s {domain = a} :: Address)

-- | The carrier IP address associated. This option is only available for
-- network interfaces which reside in a subnet in a Wavelength Zone (for
-- example an EC2 instance).
address_carrierIp :: Lens.Lens' Address (Core.Maybe Core.Text)
address_carrierIp = Lens.lens (\Address' {carrierIp} -> carrierIp) (\s@Address' {} a -> s {carrierIp = a} :: Address)

-- | The customer-owned IP address.
address_customerOwnedIp :: Lens.Lens' Address (Core.Maybe Core.Text)
address_customerOwnedIp = Lens.lens (\Address' {customerOwnedIp} -> customerOwnedIp) (\s@Address' {} a -> s {customerOwnedIp = a} :: Address)

-- | The ID of the AWS account that owns the network interface.
address_networkInterfaceOwnerId :: Lens.Lens' Address (Core.Maybe Core.Text)
address_networkInterfaceOwnerId = Lens.lens (\Address' {networkInterfaceOwnerId} -> networkInterfaceOwnerId) (\s@Address' {} a -> s {networkInterfaceOwnerId = a} :: Address)

-- | The ID representing the association of the address with an instance in a
-- VPC.
address_associationId :: Lens.Lens' Address (Core.Maybe Core.Text)
address_associationId = Lens.lens (\Address' {associationId} -> associationId) (\s@Address' {} a -> s {associationId = a} :: Address)

-- | Any tags assigned to the Elastic IP address.
address_tags :: Lens.Lens' Address (Core.Maybe [Tag])
address_tags = Lens.lens (\Address' {tags} -> tags) (\s@Address' {} a -> s {tags = a} :: Address) Core.. Lens.mapping Lens._Coerce

-- | The ID of the network interface.
address_networkInterfaceId :: Lens.Lens' Address (Core.Maybe Core.Text)
address_networkInterfaceId = Lens.lens (\Address' {networkInterfaceId} -> networkInterfaceId) (\s@Address' {} a -> s {networkInterfaceId = a} :: Address)

-- | The ID of an address pool.
address_publicIpv4Pool :: Lens.Lens' Address (Core.Maybe Core.Text)
address_publicIpv4Pool = Lens.lens (\Address' {publicIpv4Pool} -> publicIpv4Pool) (\s@Address' {} a -> s {publicIpv4Pool = a} :: Address)

-- | The Elastic IP address.
address_publicIp :: Lens.Lens' Address (Core.Maybe Core.Text)
address_publicIp = Lens.lens (\Address' {publicIp} -> publicIp) (\s@Address' {} a -> s {publicIp = a} :: Address)

-- | The ID representing the allocation of the address for use with EC2-VPC.
address_allocationId :: Lens.Lens' Address (Core.Maybe Core.Text)
address_allocationId = Lens.lens (\Address' {allocationId} -> allocationId) (\s@Address' {} a -> s {allocationId = a} :: Address)

-- | The name of the unique set of Availability Zones, Local Zones, or
-- Wavelength Zones from which AWS advertises IP addresses.
address_networkBorderGroup :: Lens.Lens' Address (Core.Maybe Core.Text)
address_networkBorderGroup = Lens.lens (\Address' {networkBorderGroup} -> networkBorderGroup) (\s@Address' {} a -> s {networkBorderGroup = a} :: Address)

-- | The private IP address associated with the Elastic IP address.
address_privateIpAddress :: Lens.Lens' Address (Core.Maybe Core.Text)
address_privateIpAddress = Lens.lens (\Address' {privateIpAddress} -> privateIpAddress) (\s@Address' {} a -> s {privateIpAddress = a} :: Address)

instance Core.FromXML Address where
  parseXML x =
    Address'
      Core.<$> (x Core..@? "customerOwnedIpv4Pool")
      Core.<*> (x Core..@? "instanceId")
      Core.<*> (x Core..@? "domain")
      Core.<*> (x Core..@? "carrierIp")
      Core.<*> (x Core..@? "customerOwnedIp")
      Core.<*> (x Core..@? "networkInterfaceOwnerId")
      Core.<*> (x Core..@? "associationId")
      Core.<*> ( x Core..@? "tagSet" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "item")
               )
      Core.<*> (x Core..@? "networkInterfaceId")
      Core.<*> (x Core..@? "publicIpv4Pool")
      Core.<*> (x Core..@? "publicIp")
      Core.<*> (x Core..@? "allocationId")
      Core.<*> (x Core..@? "networkBorderGroup")
      Core.<*> (x Core..@? "privateIpAddress")

instance Core.Hashable Address

instance Core.NFData Address
