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
-- Module      : Amazonka.EC2.AssignIpv6Addresses
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Assigns one or more IPv6 addresses to the specified network interface.
-- You can specify one or more specific IPv6 addresses, or you can specify
-- the number of IPv6 addresses to be automatically assigned from within
-- the subnet\'s IPv6 CIDR block range. You can assign as many IPv6
-- addresses to a network interface as you can assign private IPv4
-- addresses, and the limit varies per instance type. For information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/using-eni.html#AvailableIpPerENI IP Addresses Per Network Interface Per Instance Type>
-- in the /Amazon Elastic Compute Cloud User Guide/.
--
-- You must specify either the IPv6 addresses or the IPv6 address count in
-- the request.
--
-- You can optionally use Prefix Delegation on the network interface. You
-- must specify either the IPV6 Prefix Delegation prefixes, or the IPv6
-- Prefix Delegation count. For information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-prefix-eni.html Assigning prefixes to Amazon EC2 network interfaces>
-- in the /Amazon Elastic Compute Cloud User Guide/.
module Amazonka.EC2.AssignIpv6Addresses
  ( -- * Creating a Request
    AssignIpv6Addresses (..),
    newAssignIpv6Addresses,

    -- * Request Lenses
    assignIpv6Addresses_ipv6AddressCount,
    assignIpv6Addresses_ipv6PrefixCount,
    assignIpv6Addresses_ipv6Prefixes,
    assignIpv6Addresses_ipv6Addresses,
    assignIpv6Addresses_networkInterfaceId,

    -- * Destructuring the Response
    AssignIpv6AddressesResponse (..),
    newAssignIpv6AddressesResponse,

    -- * Response Lenses
    assignIpv6AddressesResponse_assignedIpv6Prefixes,
    assignIpv6AddressesResponse_networkInterfaceId,
    assignIpv6AddressesResponse_assignedIpv6Addresses,
    assignIpv6AddressesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newAssignIpv6Addresses' smart constructor.
data AssignIpv6Addresses = AssignIpv6Addresses'
  { -- | The number of additional IPv6 addresses to assign to the network
    -- interface. The specified number of IPv6 addresses are assigned in
    -- addition to the existing IPv6 addresses that are already assigned to the
    -- network interface. Amazon EC2 automatically selects the IPv6 addresses
    -- from the subnet range. You can\'t use this option if specifying specific
    -- IPv6 addresses.
    ipv6AddressCount :: Prelude.Maybe Prelude.Int,
    -- | The number of IPv6 prefixes that Amazon Web Services automatically
    -- assigns to the network interface. You cannot use this option if you use
    -- the @Ipv6Prefixes@ option.
    ipv6PrefixCount :: Prelude.Maybe Prelude.Int,
    -- | One or more IPv6 prefixes assigned to the network interface. You cannot
    -- use this option if you use the @Ipv6PrefixCount@ option.
    ipv6Prefixes :: Prelude.Maybe [Prelude.Text],
    -- | The IPv6 addresses to be assigned to the network interface. You can\'t
    -- use this option if you\'re specifying a number of IPv6 addresses.
    ipv6Addresses :: Prelude.Maybe [Prelude.Text],
    -- | The ID of the network interface.
    networkInterfaceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssignIpv6Addresses' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ipv6AddressCount', 'assignIpv6Addresses_ipv6AddressCount' - The number of additional IPv6 addresses to assign to the network
-- interface. The specified number of IPv6 addresses are assigned in
-- addition to the existing IPv6 addresses that are already assigned to the
-- network interface. Amazon EC2 automatically selects the IPv6 addresses
-- from the subnet range. You can\'t use this option if specifying specific
-- IPv6 addresses.
--
-- 'ipv6PrefixCount', 'assignIpv6Addresses_ipv6PrefixCount' - The number of IPv6 prefixes that Amazon Web Services automatically
-- assigns to the network interface. You cannot use this option if you use
-- the @Ipv6Prefixes@ option.
--
-- 'ipv6Prefixes', 'assignIpv6Addresses_ipv6Prefixes' - One or more IPv6 prefixes assigned to the network interface. You cannot
-- use this option if you use the @Ipv6PrefixCount@ option.
--
-- 'ipv6Addresses', 'assignIpv6Addresses_ipv6Addresses' - The IPv6 addresses to be assigned to the network interface. You can\'t
-- use this option if you\'re specifying a number of IPv6 addresses.
--
-- 'networkInterfaceId', 'assignIpv6Addresses_networkInterfaceId' - The ID of the network interface.
newAssignIpv6Addresses ::
  -- | 'networkInterfaceId'
  Prelude.Text ->
  AssignIpv6Addresses
newAssignIpv6Addresses pNetworkInterfaceId_ =
  AssignIpv6Addresses'
    { ipv6AddressCount =
        Prelude.Nothing,
      ipv6PrefixCount = Prelude.Nothing,
      ipv6Prefixes = Prelude.Nothing,
      ipv6Addresses = Prelude.Nothing,
      networkInterfaceId = pNetworkInterfaceId_
    }

-- | The number of additional IPv6 addresses to assign to the network
-- interface. The specified number of IPv6 addresses are assigned in
-- addition to the existing IPv6 addresses that are already assigned to the
-- network interface. Amazon EC2 automatically selects the IPv6 addresses
-- from the subnet range. You can\'t use this option if specifying specific
-- IPv6 addresses.
assignIpv6Addresses_ipv6AddressCount :: Lens.Lens' AssignIpv6Addresses (Prelude.Maybe Prelude.Int)
assignIpv6Addresses_ipv6AddressCount = Lens.lens (\AssignIpv6Addresses' {ipv6AddressCount} -> ipv6AddressCount) (\s@AssignIpv6Addresses' {} a -> s {ipv6AddressCount = a} :: AssignIpv6Addresses)

-- | The number of IPv6 prefixes that Amazon Web Services automatically
-- assigns to the network interface. You cannot use this option if you use
-- the @Ipv6Prefixes@ option.
assignIpv6Addresses_ipv6PrefixCount :: Lens.Lens' AssignIpv6Addresses (Prelude.Maybe Prelude.Int)
assignIpv6Addresses_ipv6PrefixCount = Lens.lens (\AssignIpv6Addresses' {ipv6PrefixCount} -> ipv6PrefixCount) (\s@AssignIpv6Addresses' {} a -> s {ipv6PrefixCount = a} :: AssignIpv6Addresses)

-- | One or more IPv6 prefixes assigned to the network interface. You cannot
-- use this option if you use the @Ipv6PrefixCount@ option.
assignIpv6Addresses_ipv6Prefixes :: Lens.Lens' AssignIpv6Addresses (Prelude.Maybe [Prelude.Text])
assignIpv6Addresses_ipv6Prefixes = Lens.lens (\AssignIpv6Addresses' {ipv6Prefixes} -> ipv6Prefixes) (\s@AssignIpv6Addresses' {} a -> s {ipv6Prefixes = a} :: AssignIpv6Addresses) Prelude.. Lens.mapping Lens.coerced

-- | The IPv6 addresses to be assigned to the network interface. You can\'t
-- use this option if you\'re specifying a number of IPv6 addresses.
assignIpv6Addresses_ipv6Addresses :: Lens.Lens' AssignIpv6Addresses (Prelude.Maybe [Prelude.Text])
assignIpv6Addresses_ipv6Addresses = Lens.lens (\AssignIpv6Addresses' {ipv6Addresses} -> ipv6Addresses) (\s@AssignIpv6Addresses' {} a -> s {ipv6Addresses = a} :: AssignIpv6Addresses) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the network interface.
assignIpv6Addresses_networkInterfaceId :: Lens.Lens' AssignIpv6Addresses Prelude.Text
assignIpv6Addresses_networkInterfaceId = Lens.lens (\AssignIpv6Addresses' {networkInterfaceId} -> networkInterfaceId) (\s@AssignIpv6Addresses' {} a -> s {networkInterfaceId = a} :: AssignIpv6Addresses)

instance Core.AWSRequest AssignIpv6Addresses where
  type
    AWSResponse AssignIpv6Addresses =
      AssignIpv6AddressesResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          AssignIpv6AddressesResponse'
            Prelude.<$> ( x Data..@? "assignedIpv6PrefixSet"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "item")
                        )
            Prelude.<*> (x Data..@? "networkInterfaceId")
            Prelude.<*> ( x Data..@? "assignedIpv6Addresses"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "item")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable AssignIpv6Addresses where
  hashWithSalt _salt AssignIpv6Addresses' {..} =
    _salt `Prelude.hashWithSalt` ipv6AddressCount
      `Prelude.hashWithSalt` ipv6PrefixCount
      `Prelude.hashWithSalt` ipv6Prefixes
      `Prelude.hashWithSalt` ipv6Addresses
      `Prelude.hashWithSalt` networkInterfaceId

instance Prelude.NFData AssignIpv6Addresses where
  rnf AssignIpv6Addresses' {..} =
    Prelude.rnf ipv6AddressCount
      `Prelude.seq` Prelude.rnf ipv6PrefixCount
      `Prelude.seq` Prelude.rnf ipv6Prefixes
      `Prelude.seq` Prelude.rnf ipv6Addresses
      `Prelude.seq` Prelude.rnf networkInterfaceId

instance Data.ToHeaders AssignIpv6Addresses where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath AssignIpv6Addresses where
  toPath = Prelude.const "/"

instance Data.ToQuery AssignIpv6Addresses where
  toQuery AssignIpv6Addresses' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("AssignIpv6Addresses" :: Prelude.ByteString),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "Ipv6AddressCount" Data.=: ipv6AddressCount,
        "Ipv6PrefixCount" Data.=: ipv6PrefixCount,
        Data.toQuery
          ( Data.toQueryList "Ipv6Prefix"
              Prelude.<$> ipv6Prefixes
          ),
        Data.toQuery
          ( Data.toQueryList "Ipv6Addresses"
              Prelude.<$> ipv6Addresses
          ),
        "NetworkInterfaceId" Data.=: networkInterfaceId
      ]

-- | /See:/ 'newAssignIpv6AddressesResponse' smart constructor.
data AssignIpv6AddressesResponse = AssignIpv6AddressesResponse'
  { -- | The IPv6 prefixes that are assigned to the network interface.
    assignedIpv6Prefixes :: Prelude.Maybe [Prelude.Text],
    -- | The ID of the network interface.
    networkInterfaceId :: Prelude.Maybe Prelude.Text,
    -- | The new IPv6 addresses assigned to the network interface. Existing IPv6
    -- addresses that were assigned to the network interface before the request
    -- are not included.
    assignedIpv6Addresses :: Prelude.Maybe [Prelude.Text],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssignIpv6AddressesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'assignedIpv6Prefixes', 'assignIpv6AddressesResponse_assignedIpv6Prefixes' - The IPv6 prefixes that are assigned to the network interface.
--
-- 'networkInterfaceId', 'assignIpv6AddressesResponse_networkInterfaceId' - The ID of the network interface.
--
-- 'assignedIpv6Addresses', 'assignIpv6AddressesResponse_assignedIpv6Addresses' - The new IPv6 addresses assigned to the network interface. Existing IPv6
-- addresses that were assigned to the network interface before the request
-- are not included.
--
-- 'httpStatus', 'assignIpv6AddressesResponse_httpStatus' - The response's http status code.
newAssignIpv6AddressesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  AssignIpv6AddressesResponse
newAssignIpv6AddressesResponse pHttpStatus_ =
  AssignIpv6AddressesResponse'
    { assignedIpv6Prefixes =
        Prelude.Nothing,
      networkInterfaceId = Prelude.Nothing,
      assignedIpv6Addresses = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The IPv6 prefixes that are assigned to the network interface.
assignIpv6AddressesResponse_assignedIpv6Prefixes :: Lens.Lens' AssignIpv6AddressesResponse (Prelude.Maybe [Prelude.Text])
assignIpv6AddressesResponse_assignedIpv6Prefixes = Lens.lens (\AssignIpv6AddressesResponse' {assignedIpv6Prefixes} -> assignedIpv6Prefixes) (\s@AssignIpv6AddressesResponse' {} a -> s {assignedIpv6Prefixes = a} :: AssignIpv6AddressesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the network interface.
assignIpv6AddressesResponse_networkInterfaceId :: Lens.Lens' AssignIpv6AddressesResponse (Prelude.Maybe Prelude.Text)
assignIpv6AddressesResponse_networkInterfaceId = Lens.lens (\AssignIpv6AddressesResponse' {networkInterfaceId} -> networkInterfaceId) (\s@AssignIpv6AddressesResponse' {} a -> s {networkInterfaceId = a} :: AssignIpv6AddressesResponse)

-- | The new IPv6 addresses assigned to the network interface. Existing IPv6
-- addresses that were assigned to the network interface before the request
-- are not included.
assignIpv6AddressesResponse_assignedIpv6Addresses :: Lens.Lens' AssignIpv6AddressesResponse (Prelude.Maybe [Prelude.Text])
assignIpv6AddressesResponse_assignedIpv6Addresses = Lens.lens (\AssignIpv6AddressesResponse' {assignedIpv6Addresses} -> assignedIpv6Addresses) (\s@AssignIpv6AddressesResponse' {} a -> s {assignedIpv6Addresses = a} :: AssignIpv6AddressesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
assignIpv6AddressesResponse_httpStatus :: Lens.Lens' AssignIpv6AddressesResponse Prelude.Int
assignIpv6AddressesResponse_httpStatus = Lens.lens (\AssignIpv6AddressesResponse' {httpStatus} -> httpStatus) (\s@AssignIpv6AddressesResponse' {} a -> s {httpStatus = a} :: AssignIpv6AddressesResponse)

instance Prelude.NFData AssignIpv6AddressesResponse where
  rnf AssignIpv6AddressesResponse' {..} =
    Prelude.rnf assignedIpv6Prefixes
      `Prelude.seq` Prelude.rnf networkInterfaceId
      `Prelude.seq` Prelude.rnf assignedIpv6Addresses
      `Prelude.seq` Prelude.rnf httpStatus
