{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.EC2.ModifySubnetAttribute
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies a subnet attribute. You can only modify one attribute at a
-- time.
--
-- Use this action to modify subnets on Amazon Web Services Outposts.
--
-- -   To modify a subnet on an Outpost rack, set both
--     @MapCustomerOwnedIpOnLaunch@ and @CustomerOwnedIpv4Pool@. These two
--     parameters act as a single attribute.
--
-- -   To modify a subnet on an Outpost server, set either
--     @EnableLniAtDeviceIndex@ or @DisableLniAtDeviceIndex@.
--
-- For more information about Amazon Web Services Outposts, see the
-- following:
--
-- -   <https://docs.aws.amazon.com/outposts/latest/userguide/how-servers-work.html Outpost servers>
--
-- -   <https://docs.aws.amazon.com/outposts/latest/userguide/how-racks-work.html Outpost racks>
module Amazonka.EC2.ModifySubnetAttribute
  ( -- * Creating a Request
    ModifySubnetAttribute (..),
    newModifySubnetAttribute,

    -- * Request Lenses
    modifySubnetAttribute_privateDnsHostnameTypeOnLaunch,
    modifySubnetAttribute_mapPublicIpOnLaunch,
    modifySubnetAttribute_enableLniAtDeviceIndex,
    modifySubnetAttribute_customerOwnedIpv4Pool,
    modifySubnetAttribute_mapCustomerOwnedIpOnLaunch,
    modifySubnetAttribute_disableLniAtDeviceIndex,
    modifySubnetAttribute_enableResourceNameDnsAAAARecordOnLaunch,
    modifySubnetAttribute_assignIpv6AddressOnCreation,
    modifySubnetAttribute_enableDns64,
    modifySubnetAttribute_enableResourceNameDnsARecordOnLaunch,
    modifySubnetAttribute_subnetId,

    -- * Destructuring the Response
    ModifySubnetAttributeResponse (..),
    newModifySubnetAttributeResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newModifySubnetAttribute' smart constructor.
data ModifySubnetAttribute = ModifySubnetAttribute'
  { -- | The type of hostname to assign to instances in the subnet at launch. For
    -- IPv4-only and dual-stack (IPv4 and IPv6) subnets, an instance DNS name
    -- can be based on the instance IPv4 address (ip-name) or the instance ID
    -- (resource-name). For IPv6 only subnets, an instance DNS name must be
    -- based on the instance ID (resource-name).
    privateDnsHostnameTypeOnLaunch :: Prelude.Maybe HostnameType,
    -- | Specify @true@ to indicate that network interfaces attached to instances
    -- created in the specified subnet should be assigned a public IPv4
    -- address.
    mapPublicIpOnLaunch :: Prelude.Maybe AttributeBooleanValue,
    -- | Indicates the device position for local network interfaces in this
    -- subnet. For example, @1@ indicates local network interfaces in this
    -- subnet are the secondary network interface (eth1). A local network
    -- interface cannot be the primary network interface (eth0).
    enableLniAtDeviceIndex :: Prelude.Maybe Prelude.Int,
    -- | The customer-owned IPv4 address pool associated with the subnet.
    --
    -- You must set this value when you specify @true@ for
    -- @MapCustomerOwnedIpOnLaunch@.
    customerOwnedIpv4Pool :: Prelude.Maybe Prelude.Text,
    -- | Specify @true@ to indicate that network interfaces attached to instances
    -- created in the specified subnet should be assigned a customer-owned IPv4
    -- address.
    --
    -- When this value is @true@, you must specify the customer-owned IP pool
    -- using @CustomerOwnedIpv4Pool@.
    mapCustomerOwnedIpOnLaunch :: Prelude.Maybe AttributeBooleanValue,
    -- | Specify @true@ to indicate that local network interfaces at the current
    -- position should be disabled.
    disableLniAtDeviceIndex :: Prelude.Maybe AttributeBooleanValue,
    -- | Indicates whether to respond to DNS queries for instance hostnames with
    -- DNS AAAA records.
    enableResourceNameDnsAAAARecordOnLaunch :: Prelude.Maybe AttributeBooleanValue,
    -- | Specify @true@ to indicate that network interfaces created in the
    -- specified subnet should be assigned an IPv6 address. This includes a
    -- network interface that\'s created when launching an instance into the
    -- subnet (the instance therefore receives an IPv6 address).
    --
    -- If you enable the IPv6 addressing feature for your subnet, your network
    -- interface or instance only receives an IPv6 address if it\'s created
    -- using version @2016-11-15@ or later of the Amazon EC2 API.
    assignIpv6AddressOnCreation :: Prelude.Maybe AttributeBooleanValue,
    -- | Indicates whether DNS queries made to the Amazon-provided DNS Resolver
    -- in this subnet should return synthetic IPv6 addresses for IPv4-only
    -- destinations.
    enableDns64 :: Prelude.Maybe AttributeBooleanValue,
    -- | Indicates whether to respond to DNS queries for instance hostnames with
    -- DNS A records.
    enableResourceNameDnsARecordOnLaunch :: Prelude.Maybe AttributeBooleanValue,
    -- | The ID of the subnet.
    subnetId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModifySubnetAttribute' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'privateDnsHostnameTypeOnLaunch', 'modifySubnetAttribute_privateDnsHostnameTypeOnLaunch' - The type of hostname to assign to instances in the subnet at launch. For
-- IPv4-only and dual-stack (IPv4 and IPv6) subnets, an instance DNS name
-- can be based on the instance IPv4 address (ip-name) or the instance ID
-- (resource-name). For IPv6 only subnets, an instance DNS name must be
-- based on the instance ID (resource-name).
--
-- 'mapPublicIpOnLaunch', 'modifySubnetAttribute_mapPublicIpOnLaunch' - Specify @true@ to indicate that network interfaces attached to instances
-- created in the specified subnet should be assigned a public IPv4
-- address.
--
-- 'enableLniAtDeviceIndex', 'modifySubnetAttribute_enableLniAtDeviceIndex' - Indicates the device position for local network interfaces in this
-- subnet. For example, @1@ indicates local network interfaces in this
-- subnet are the secondary network interface (eth1). A local network
-- interface cannot be the primary network interface (eth0).
--
-- 'customerOwnedIpv4Pool', 'modifySubnetAttribute_customerOwnedIpv4Pool' - The customer-owned IPv4 address pool associated with the subnet.
--
-- You must set this value when you specify @true@ for
-- @MapCustomerOwnedIpOnLaunch@.
--
-- 'mapCustomerOwnedIpOnLaunch', 'modifySubnetAttribute_mapCustomerOwnedIpOnLaunch' - Specify @true@ to indicate that network interfaces attached to instances
-- created in the specified subnet should be assigned a customer-owned IPv4
-- address.
--
-- When this value is @true@, you must specify the customer-owned IP pool
-- using @CustomerOwnedIpv4Pool@.
--
-- 'disableLniAtDeviceIndex', 'modifySubnetAttribute_disableLniAtDeviceIndex' - Specify @true@ to indicate that local network interfaces at the current
-- position should be disabled.
--
-- 'enableResourceNameDnsAAAARecordOnLaunch', 'modifySubnetAttribute_enableResourceNameDnsAAAARecordOnLaunch' - Indicates whether to respond to DNS queries for instance hostnames with
-- DNS AAAA records.
--
-- 'assignIpv6AddressOnCreation', 'modifySubnetAttribute_assignIpv6AddressOnCreation' - Specify @true@ to indicate that network interfaces created in the
-- specified subnet should be assigned an IPv6 address. This includes a
-- network interface that\'s created when launching an instance into the
-- subnet (the instance therefore receives an IPv6 address).
--
-- If you enable the IPv6 addressing feature for your subnet, your network
-- interface or instance only receives an IPv6 address if it\'s created
-- using version @2016-11-15@ or later of the Amazon EC2 API.
--
-- 'enableDns64', 'modifySubnetAttribute_enableDns64' - Indicates whether DNS queries made to the Amazon-provided DNS Resolver
-- in this subnet should return synthetic IPv6 addresses for IPv4-only
-- destinations.
--
-- 'enableResourceNameDnsARecordOnLaunch', 'modifySubnetAttribute_enableResourceNameDnsARecordOnLaunch' - Indicates whether to respond to DNS queries for instance hostnames with
-- DNS A records.
--
-- 'subnetId', 'modifySubnetAttribute_subnetId' - The ID of the subnet.
newModifySubnetAttribute ::
  -- | 'subnetId'
  Prelude.Text ->
  ModifySubnetAttribute
newModifySubnetAttribute pSubnetId_ =
  ModifySubnetAttribute'
    { privateDnsHostnameTypeOnLaunch =
        Prelude.Nothing,
      mapPublicIpOnLaunch = Prelude.Nothing,
      enableLniAtDeviceIndex = Prelude.Nothing,
      customerOwnedIpv4Pool = Prelude.Nothing,
      mapCustomerOwnedIpOnLaunch = Prelude.Nothing,
      disableLniAtDeviceIndex = Prelude.Nothing,
      enableResourceNameDnsAAAARecordOnLaunch =
        Prelude.Nothing,
      assignIpv6AddressOnCreation = Prelude.Nothing,
      enableDns64 = Prelude.Nothing,
      enableResourceNameDnsARecordOnLaunch =
        Prelude.Nothing,
      subnetId = pSubnetId_
    }

-- | The type of hostname to assign to instances in the subnet at launch. For
-- IPv4-only and dual-stack (IPv4 and IPv6) subnets, an instance DNS name
-- can be based on the instance IPv4 address (ip-name) or the instance ID
-- (resource-name). For IPv6 only subnets, an instance DNS name must be
-- based on the instance ID (resource-name).
modifySubnetAttribute_privateDnsHostnameTypeOnLaunch :: Lens.Lens' ModifySubnetAttribute (Prelude.Maybe HostnameType)
modifySubnetAttribute_privateDnsHostnameTypeOnLaunch = Lens.lens (\ModifySubnetAttribute' {privateDnsHostnameTypeOnLaunch} -> privateDnsHostnameTypeOnLaunch) (\s@ModifySubnetAttribute' {} a -> s {privateDnsHostnameTypeOnLaunch = a} :: ModifySubnetAttribute)

-- | Specify @true@ to indicate that network interfaces attached to instances
-- created in the specified subnet should be assigned a public IPv4
-- address.
modifySubnetAttribute_mapPublicIpOnLaunch :: Lens.Lens' ModifySubnetAttribute (Prelude.Maybe AttributeBooleanValue)
modifySubnetAttribute_mapPublicIpOnLaunch = Lens.lens (\ModifySubnetAttribute' {mapPublicIpOnLaunch} -> mapPublicIpOnLaunch) (\s@ModifySubnetAttribute' {} a -> s {mapPublicIpOnLaunch = a} :: ModifySubnetAttribute)

-- | Indicates the device position for local network interfaces in this
-- subnet. For example, @1@ indicates local network interfaces in this
-- subnet are the secondary network interface (eth1). A local network
-- interface cannot be the primary network interface (eth0).
modifySubnetAttribute_enableLniAtDeviceIndex :: Lens.Lens' ModifySubnetAttribute (Prelude.Maybe Prelude.Int)
modifySubnetAttribute_enableLniAtDeviceIndex = Lens.lens (\ModifySubnetAttribute' {enableLniAtDeviceIndex} -> enableLniAtDeviceIndex) (\s@ModifySubnetAttribute' {} a -> s {enableLniAtDeviceIndex = a} :: ModifySubnetAttribute)

-- | The customer-owned IPv4 address pool associated with the subnet.
--
-- You must set this value when you specify @true@ for
-- @MapCustomerOwnedIpOnLaunch@.
modifySubnetAttribute_customerOwnedIpv4Pool :: Lens.Lens' ModifySubnetAttribute (Prelude.Maybe Prelude.Text)
modifySubnetAttribute_customerOwnedIpv4Pool = Lens.lens (\ModifySubnetAttribute' {customerOwnedIpv4Pool} -> customerOwnedIpv4Pool) (\s@ModifySubnetAttribute' {} a -> s {customerOwnedIpv4Pool = a} :: ModifySubnetAttribute)

-- | Specify @true@ to indicate that network interfaces attached to instances
-- created in the specified subnet should be assigned a customer-owned IPv4
-- address.
--
-- When this value is @true@, you must specify the customer-owned IP pool
-- using @CustomerOwnedIpv4Pool@.
modifySubnetAttribute_mapCustomerOwnedIpOnLaunch :: Lens.Lens' ModifySubnetAttribute (Prelude.Maybe AttributeBooleanValue)
modifySubnetAttribute_mapCustomerOwnedIpOnLaunch = Lens.lens (\ModifySubnetAttribute' {mapCustomerOwnedIpOnLaunch} -> mapCustomerOwnedIpOnLaunch) (\s@ModifySubnetAttribute' {} a -> s {mapCustomerOwnedIpOnLaunch = a} :: ModifySubnetAttribute)

-- | Specify @true@ to indicate that local network interfaces at the current
-- position should be disabled.
modifySubnetAttribute_disableLniAtDeviceIndex :: Lens.Lens' ModifySubnetAttribute (Prelude.Maybe AttributeBooleanValue)
modifySubnetAttribute_disableLniAtDeviceIndex = Lens.lens (\ModifySubnetAttribute' {disableLniAtDeviceIndex} -> disableLniAtDeviceIndex) (\s@ModifySubnetAttribute' {} a -> s {disableLniAtDeviceIndex = a} :: ModifySubnetAttribute)

-- | Indicates whether to respond to DNS queries for instance hostnames with
-- DNS AAAA records.
modifySubnetAttribute_enableResourceNameDnsAAAARecordOnLaunch :: Lens.Lens' ModifySubnetAttribute (Prelude.Maybe AttributeBooleanValue)
modifySubnetAttribute_enableResourceNameDnsAAAARecordOnLaunch = Lens.lens (\ModifySubnetAttribute' {enableResourceNameDnsAAAARecordOnLaunch} -> enableResourceNameDnsAAAARecordOnLaunch) (\s@ModifySubnetAttribute' {} a -> s {enableResourceNameDnsAAAARecordOnLaunch = a} :: ModifySubnetAttribute)

-- | Specify @true@ to indicate that network interfaces created in the
-- specified subnet should be assigned an IPv6 address. This includes a
-- network interface that\'s created when launching an instance into the
-- subnet (the instance therefore receives an IPv6 address).
--
-- If you enable the IPv6 addressing feature for your subnet, your network
-- interface or instance only receives an IPv6 address if it\'s created
-- using version @2016-11-15@ or later of the Amazon EC2 API.
modifySubnetAttribute_assignIpv6AddressOnCreation :: Lens.Lens' ModifySubnetAttribute (Prelude.Maybe AttributeBooleanValue)
modifySubnetAttribute_assignIpv6AddressOnCreation = Lens.lens (\ModifySubnetAttribute' {assignIpv6AddressOnCreation} -> assignIpv6AddressOnCreation) (\s@ModifySubnetAttribute' {} a -> s {assignIpv6AddressOnCreation = a} :: ModifySubnetAttribute)

-- | Indicates whether DNS queries made to the Amazon-provided DNS Resolver
-- in this subnet should return synthetic IPv6 addresses for IPv4-only
-- destinations.
modifySubnetAttribute_enableDns64 :: Lens.Lens' ModifySubnetAttribute (Prelude.Maybe AttributeBooleanValue)
modifySubnetAttribute_enableDns64 = Lens.lens (\ModifySubnetAttribute' {enableDns64} -> enableDns64) (\s@ModifySubnetAttribute' {} a -> s {enableDns64 = a} :: ModifySubnetAttribute)

-- | Indicates whether to respond to DNS queries for instance hostnames with
-- DNS A records.
modifySubnetAttribute_enableResourceNameDnsARecordOnLaunch :: Lens.Lens' ModifySubnetAttribute (Prelude.Maybe AttributeBooleanValue)
modifySubnetAttribute_enableResourceNameDnsARecordOnLaunch = Lens.lens (\ModifySubnetAttribute' {enableResourceNameDnsARecordOnLaunch} -> enableResourceNameDnsARecordOnLaunch) (\s@ModifySubnetAttribute' {} a -> s {enableResourceNameDnsARecordOnLaunch = a} :: ModifySubnetAttribute)

-- | The ID of the subnet.
modifySubnetAttribute_subnetId :: Lens.Lens' ModifySubnetAttribute Prelude.Text
modifySubnetAttribute_subnetId = Lens.lens (\ModifySubnetAttribute' {subnetId} -> subnetId) (\s@ModifySubnetAttribute' {} a -> s {subnetId = a} :: ModifySubnetAttribute)

instance Core.AWSRequest ModifySubnetAttribute where
  type
    AWSResponse ModifySubnetAttribute =
      ModifySubnetAttributeResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveNull ModifySubnetAttributeResponse'

instance Prelude.Hashable ModifySubnetAttribute where
  hashWithSalt _salt ModifySubnetAttribute' {..} =
    _salt
      `Prelude.hashWithSalt` privateDnsHostnameTypeOnLaunch
      `Prelude.hashWithSalt` mapPublicIpOnLaunch
      `Prelude.hashWithSalt` enableLniAtDeviceIndex
      `Prelude.hashWithSalt` customerOwnedIpv4Pool
      `Prelude.hashWithSalt` mapCustomerOwnedIpOnLaunch
      `Prelude.hashWithSalt` disableLniAtDeviceIndex
      `Prelude.hashWithSalt` enableResourceNameDnsAAAARecordOnLaunch
      `Prelude.hashWithSalt` assignIpv6AddressOnCreation
      `Prelude.hashWithSalt` enableDns64
      `Prelude.hashWithSalt` enableResourceNameDnsARecordOnLaunch
      `Prelude.hashWithSalt` subnetId

instance Prelude.NFData ModifySubnetAttribute where
  rnf ModifySubnetAttribute' {..} =
    Prelude.rnf privateDnsHostnameTypeOnLaunch
      `Prelude.seq` Prelude.rnf mapPublicIpOnLaunch
      `Prelude.seq` Prelude.rnf enableLniAtDeviceIndex
      `Prelude.seq` Prelude.rnf customerOwnedIpv4Pool
      `Prelude.seq` Prelude.rnf mapCustomerOwnedIpOnLaunch
      `Prelude.seq` Prelude.rnf disableLniAtDeviceIndex
      `Prelude.seq` Prelude.rnf enableResourceNameDnsAAAARecordOnLaunch
      `Prelude.seq` Prelude.rnf assignIpv6AddressOnCreation
      `Prelude.seq` Prelude.rnf enableDns64
      `Prelude.seq` Prelude.rnf enableResourceNameDnsARecordOnLaunch
      `Prelude.seq` Prelude.rnf subnetId

instance Data.ToHeaders ModifySubnetAttribute where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ModifySubnetAttribute where
  toPath = Prelude.const "/"

instance Data.ToQuery ModifySubnetAttribute where
  toQuery ModifySubnetAttribute' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("ModifySubnetAttribute" :: Prelude.ByteString),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "PrivateDnsHostnameTypeOnLaunch"
          Data.=: privateDnsHostnameTypeOnLaunch,
        "MapPublicIpOnLaunch" Data.=: mapPublicIpOnLaunch,
        "EnableLniAtDeviceIndex"
          Data.=: enableLniAtDeviceIndex,
        "CustomerOwnedIpv4Pool"
          Data.=: customerOwnedIpv4Pool,
        "MapCustomerOwnedIpOnLaunch"
          Data.=: mapCustomerOwnedIpOnLaunch,
        "DisableLniAtDeviceIndex"
          Data.=: disableLniAtDeviceIndex,
        "EnableResourceNameDnsAAAARecordOnLaunch"
          Data.=: enableResourceNameDnsAAAARecordOnLaunch,
        "AssignIpv6AddressOnCreation"
          Data.=: assignIpv6AddressOnCreation,
        "EnableDns64" Data.=: enableDns64,
        "EnableResourceNameDnsARecordOnLaunch"
          Data.=: enableResourceNameDnsARecordOnLaunch,
        "SubnetId" Data.=: subnetId
      ]

-- | /See:/ 'newModifySubnetAttributeResponse' smart constructor.
data ModifySubnetAttributeResponse = ModifySubnetAttributeResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModifySubnetAttributeResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newModifySubnetAttributeResponse ::
  ModifySubnetAttributeResponse
newModifySubnetAttributeResponse =
  ModifySubnetAttributeResponse'

instance Prelude.NFData ModifySubnetAttributeResponse where
  rnf _ = ()
