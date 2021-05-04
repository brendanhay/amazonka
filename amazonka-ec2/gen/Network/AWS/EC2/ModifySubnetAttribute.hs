{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.EC2.ModifySubnetAttribute
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies a subnet attribute. You can only modify one attribute at a
-- time.
module Network.AWS.EC2.ModifySubnetAttribute
  ( -- * Creating a Request
    ModifySubnetAttribute (..),
    newModifySubnetAttribute,

    -- * Request Lenses
    modifySubnetAttribute_customerOwnedIpv4Pool,
    modifySubnetAttribute_assignIpv6AddressOnCreation,
    modifySubnetAttribute_mapPublicIpOnLaunch,
    modifySubnetAttribute_mapCustomerOwnedIpOnLaunch,
    modifySubnetAttribute_subnetId,

    -- * Destructuring the Response
    ModifySubnetAttributeResponse (..),
    newModifySubnetAttributeResponse,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newModifySubnetAttribute' smart constructor.
data ModifySubnetAttribute = ModifySubnetAttribute'
  { -- | The customer-owned IPv4 address pool associated with the subnet.
    --
    -- You must set this value when you specify @true@ for
    -- @MapCustomerOwnedIpOnLaunch@.
    customerOwnedIpv4Pool :: Prelude.Maybe Prelude.Text,
    -- | Specify @true@ to indicate that network interfaces created in the
    -- specified subnet should be assigned an IPv6 address. This includes a
    -- network interface that\'s created when launching an instance into the
    -- subnet (the instance therefore receives an IPv6 address).
    --
    -- If you enable the IPv6 addressing feature for your subnet, your network
    -- interface or instance only receives an IPv6 address if it\'s created
    -- using version @2016-11-15@ or later of the Amazon EC2 API.
    assignIpv6AddressOnCreation :: Prelude.Maybe AttributeBooleanValue,
    -- | Specify @true@ to indicate that network interfaces attached to instances
    -- created in the specified subnet should be assigned a public IPv4
    -- address.
    mapPublicIpOnLaunch :: Prelude.Maybe AttributeBooleanValue,
    -- | Specify @true@ to indicate that network interfaces attached to instances
    -- created in the specified subnet should be assigned a customer-owned IPv4
    -- address.
    --
    -- When this value is @true@, you must specify the customer-owned IP pool
    -- using @CustomerOwnedIpv4Pool@.
    mapCustomerOwnedIpOnLaunch :: Prelude.Maybe AttributeBooleanValue,
    -- | The ID of the subnet.
    subnetId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ModifySubnetAttribute' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'customerOwnedIpv4Pool', 'modifySubnetAttribute_customerOwnedIpv4Pool' - The customer-owned IPv4 address pool associated with the subnet.
--
-- You must set this value when you specify @true@ for
-- @MapCustomerOwnedIpOnLaunch@.
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
-- 'mapPublicIpOnLaunch', 'modifySubnetAttribute_mapPublicIpOnLaunch' - Specify @true@ to indicate that network interfaces attached to instances
-- created in the specified subnet should be assigned a public IPv4
-- address.
--
-- 'mapCustomerOwnedIpOnLaunch', 'modifySubnetAttribute_mapCustomerOwnedIpOnLaunch' - Specify @true@ to indicate that network interfaces attached to instances
-- created in the specified subnet should be assigned a customer-owned IPv4
-- address.
--
-- When this value is @true@, you must specify the customer-owned IP pool
-- using @CustomerOwnedIpv4Pool@.
--
-- 'subnetId', 'modifySubnetAttribute_subnetId' - The ID of the subnet.
newModifySubnetAttribute ::
  -- | 'subnetId'
  Prelude.Text ->
  ModifySubnetAttribute
newModifySubnetAttribute pSubnetId_ =
  ModifySubnetAttribute'
    { customerOwnedIpv4Pool =
        Prelude.Nothing,
      assignIpv6AddressOnCreation = Prelude.Nothing,
      mapPublicIpOnLaunch = Prelude.Nothing,
      mapCustomerOwnedIpOnLaunch = Prelude.Nothing,
      subnetId = pSubnetId_
    }

-- | The customer-owned IPv4 address pool associated with the subnet.
--
-- You must set this value when you specify @true@ for
-- @MapCustomerOwnedIpOnLaunch@.
modifySubnetAttribute_customerOwnedIpv4Pool :: Lens.Lens' ModifySubnetAttribute (Prelude.Maybe Prelude.Text)
modifySubnetAttribute_customerOwnedIpv4Pool = Lens.lens (\ModifySubnetAttribute' {customerOwnedIpv4Pool} -> customerOwnedIpv4Pool) (\s@ModifySubnetAttribute' {} a -> s {customerOwnedIpv4Pool = a} :: ModifySubnetAttribute)

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

-- | Specify @true@ to indicate that network interfaces attached to instances
-- created in the specified subnet should be assigned a public IPv4
-- address.
modifySubnetAttribute_mapPublicIpOnLaunch :: Lens.Lens' ModifySubnetAttribute (Prelude.Maybe AttributeBooleanValue)
modifySubnetAttribute_mapPublicIpOnLaunch = Lens.lens (\ModifySubnetAttribute' {mapPublicIpOnLaunch} -> mapPublicIpOnLaunch) (\s@ModifySubnetAttribute' {} a -> s {mapPublicIpOnLaunch = a} :: ModifySubnetAttribute)

-- | Specify @true@ to indicate that network interfaces attached to instances
-- created in the specified subnet should be assigned a customer-owned IPv4
-- address.
--
-- When this value is @true@, you must specify the customer-owned IP pool
-- using @CustomerOwnedIpv4Pool@.
modifySubnetAttribute_mapCustomerOwnedIpOnLaunch :: Lens.Lens' ModifySubnetAttribute (Prelude.Maybe AttributeBooleanValue)
modifySubnetAttribute_mapCustomerOwnedIpOnLaunch = Lens.lens (\ModifySubnetAttribute' {mapCustomerOwnedIpOnLaunch} -> mapCustomerOwnedIpOnLaunch) (\s@ModifySubnetAttribute' {} a -> s {mapCustomerOwnedIpOnLaunch = a} :: ModifySubnetAttribute)

-- | The ID of the subnet.
modifySubnetAttribute_subnetId :: Lens.Lens' ModifySubnetAttribute Prelude.Text
modifySubnetAttribute_subnetId = Lens.lens (\ModifySubnetAttribute' {subnetId} -> subnetId) (\s@ModifySubnetAttribute' {} a -> s {subnetId = a} :: ModifySubnetAttribute)

instance Prelude.AWSRequest ModifySubnetAttribute where
  type
    Rs ModifySubnetAttribute =
      ModifySubnetAttributeResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveNull ModifySubnetAttributeResponse'

instance Prelude.Hashable ModifySubnetAttribute

instance Prelude.NFData ModifySubnetAttribute

instance Prelude.ToHeaders ModifySubnetAttribute where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath ModifySubnetAttribute where
  toPath = Prelude.const "/"

instance Prelude.ToQuery ModifySubnetAttribute where
  toQuery ModifySubnetAttribute' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("ModifySubnetAttribute" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2016-11-15" :: Prelude.ByteString),
        "CustomerOwnedIpv4Pool"
          Prelude.=: customerOwnedIpv4Pool,
        "AssignIpv6AddressOnCreation"
          Prelude.=: assignIpv6AddressOnCreation,
        "MapPublicIpOnLaunch" Prelude.=: mapPublicIpOnLaunch,
        "MapCustomerOwnedIpOnLaunch"
          Prelude.=: mapCustomerOwnedIpOnLaunch,
        "SubnetId" Prelude.=: subnetId
      ]

-- | /See:/ 'newModifySubnetAttributeResponse' smart constructor.
data ModifySubnetAttributeResponse = ModifySubnetAttributeResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ModifySubnetAttributeResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newModifySubnetAttributeResponse ::
  ModifySubnetAttributeResponse
newModifySubnetAttributeResponse =
  ModifySubnetAttributeResponse'

instance Prelude.NFData ModifySubnetAttributeResponse
