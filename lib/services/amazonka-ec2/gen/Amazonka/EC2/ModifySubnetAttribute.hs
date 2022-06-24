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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies a subnet attribute. You can only modify one attribute at a
-- time.
module Amazonka.EC2.ModifySubnetAttribute
  ( -- * Creating a Request
    ModifySubnetAttribute (..),
    newModifySubnetAttribute,

    -- * Request Lenses
    modifySubnetAttribute_mapPublicIpOnLaunch,
    modifySubnetAttribute_customerOwnedIpv4Pool,
    modifySubnetAttribute_mapCustomerOwnedIpOnLaunch,
    modifySubnetAttribute_assignIpv6AddressOnCreation,
    modifySubnetAttribute_subnetId,

    -- * Destructuring the Response
    ModifySubnetAttributeResponse (..),
    newModifySubnetAttributeResponse,
  )
where

import qualified Amazonka.Core as Core
import Amazonka.EC2.Types
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newModifySubnetAttribute' smart constructor.
data ModifySubnetAttribute = ModifySubnetAttribute'
  { -- | Specify @true@ to indicate that network interfaces attached to instances
    -- created in the specified subnet should be assigned a public IPv4
    -- address.
    mapPublicIpOnLaunch :: Prelude.Maybe AttributeBooleanValue,
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
    -- | Specify @true@ to indicate that network interfaces created in the
    -- specified subnet should be assigned an IPv6 address. This includes a
    -- network interface that\'s created when launching an instance into the
    -- subnet (the instance therefore receives an IPv6 address).
    --
    -- If you enable the IPv6 addressing feature for your subnet, your network
    -- interface or instance only receives an IPv6 address if it\'s created
    -- using version @2016-11-15@ or later of the Amazon EC2 API.
    assignIpv6AddressOnCreation :: Prelude.Maybe AttributeBooleanValue,
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
-- 'mapPublicIpOnLaunch', 'modifySubnetAttribute_mapPublicIpOnLaunch' - Specify @true@ to indicate that network interfaces attached to instances
-- created in the specified subnet should be assigned a public IPv4
-- address.
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
-- 'assignIpv6AddressOnCreation', 'modifySubnetAttribute_assignIpv6AddressOnCreation' - Specify @true@ to indicate that network interfaces created in the
-- specified subnet should be assigned an IPv6 address. This includes a
-- network interface that\'s created when launching an instance into the
-- subnet (the instance therefore receives an IPv6 address).
--
-- If you enable the IPv6 addressing feature for your subnet, your network
-- interface or instance only receives an IPv6 address if it\'s created
-- using version @2016-11-15@ or later of the Amazon EC2 API.
--
-- 'subnetId', 'modifySubnetAttribute_subnetId' - The ID of the subnet.
newModifySubnetAttribute ::
  -- | 'subnetId'
  Prelude.Text ->
  ModifySubnetAttribute
newModifySubnetAttribute pSubnetId_ =
  ModifySubnetAttribute'
    { mapPublicIpOnLaunch =
        Prelude.Nothing,
      customerOwnedIpv4Pool = Prelude.Nothing,
      mapCustomerOwnedIpOnLaunch = Prelude.Nothing,
      assignIpv6AddressOnCreation = Prelude.Nothing,
      subnetId = pSubnetId_
    }

-- | Specify @true@ to indicate that network interfaces attached to instances
-- created in the specified subnet should be assigned a public IPv4
-- address.
modifySubnetAttribute_mapPublicIpOnLaunch :: Lens.Lens' ModifySubnetAttribute (Prelude.Maybe AttributeBooleanValue)
modifySubnetAttribute_mapPublicIpOnLaunch = Lens.lens (\ModifySubnetAttribute' {mapPublicIpOnLaunch} -> mapPublicIpOnLaunch) (\s@ModifySubnetAttribute' {} a -> s {mapPublicIpOnLaunch = a} :: ModifySubnetAttribute)

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

-- | The ID of the subnet.
modifySubnetAttribute_subnetId :: Lens.Lens' ModifySubnetAttribute Prelude.Text
modifySubnetAttribute_subnetId = Lens.lens (\ModifySubnetAttribute' {subnetId} -> subnetId) (\s@ModifySubnetAttribute' {} a -> s {subnetId = a} :: ModifySubnetAttribute)

instance Core.AWSRequest ModifySubnetAttribute where
  type
    AWSResponse ModifySubnetAttribute =
      ModifySubnetAttributeResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveNull ModifySubnetAttributeResponse'

instance Prelude.Hashable ModifySubnetAttribute where
  hashWithSalt _salt ModifySubnetAttribute' {..} =
    _salt `Prelude.hashWithSalt` mapPublicIpOnLaunch
      `Prelude.hashWithSalt` customerOwnedIpv4Pool
      `Prelude.hashWithSalt` mapCustomerOwnedIpOnLaunch
      `Prelude.hashWithSalt` assignIpv6AddressOnCreation
      `Prelude.hashWithSalt` subnetId

instance Prelude.NFData ModifySubnetAttribute where
  rnf ModifySubnetAttribute' {..} =
    Prelude.rnf mapPublicIpOnLaunch
      `Prelude.seq` Prelude.rnf customerOwnedIpv4Pool
      `Prelude.seq` Prelude.rnf mapCustomerOwnedIpOnLaunch
      `Prelude.seq` Prelude.rnf assignIpv6AddressOnCreation
      `Prelude.seq` Prelude.rnf subnetId

instance Core.ToHeaders ModifySubnetAttribute where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath ModifySubnetAttribute where
  toPath = Prelude.const "/"

instance Core.ToQuery ModifySubnetAttribute where
  toQuery ModifySubnetAttribute' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("ModifySubnetAttribute" :: Prelude.ByteString),
        "Version"
          Core.=: ("2016-11-15" :: Prelude.ByteString),
        "MapPublicIpOnLaunch" Core.=: mapPublicIpOnLaunch,
        "CustomerOwnedIpv4Pool"
          Core.=: customerOwnedIpv4Pool,
        "MapCustomerOwnedIpOnLaunch"
          Core.=: mapCustomerOwnedIpOnLaunch,
        "AssignIpv6AddressOnCreation"
          Core.=: assignIpv6AddressOnCreation,
        "SubnetId" Core.=: subnetId
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
