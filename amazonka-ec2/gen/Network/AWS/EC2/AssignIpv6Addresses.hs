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
-- Module      : Network.AWS.EC2.AssignIpv6Addresses
-- Copyright   : (c) 2013-2021 Brendan Hay
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
module Network.AWS.EC2.AssignIpv6Addresses
  ( -- * Creating a Request
    AssignIpv6Addresses (..),
    newAssignIpv6Addresses,

    -- * Request Lenses
    assignIpv6Addresses_ipv6Addresses,
    assignIpv6Addresses_ipv6AddressCount,
    assignIpv6Addresses_networkInterfaceId,

    -- * Destructuring the Response
    AssignIpv6AddressesResponse (..),
    newAssignIpv6AddressesResponse,

    -- * Response Lenses
    assignIpv6AddressesResponse_assignedIpv6Addresses,
    assignIpv6AddressesResponse_networkInterfaceId,
    assignIpv6AddressesResponse_httpStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newAssignIpv6Addresses' smart constructor.
data AssignIpv6Addresses = AssignIpv6Addresses'
  { -- | One or more specific IPv6 addresses to be assigned to the network
    -- interface. You can\'t use this option if you\'re specifying a number of
    -- IPv6 addresses.
    ipv6Addresses :: Prelude.Maybe [Prelude.Text],
    -- | The number of additional IPv6 addresses to assign to the network
    -- interface. The specified number of IPv6 addresses are assigned in
    -- addition to the existing IPv6 addresses that are already assigned to the
    -- network interface. Amazon EC2 automatically selects the IPv6 addresses
    -- from the subnet range. You can\'t use this option if specifying specific
    -- IPv6 addresses.
    ipv6AddressCount :: Prelude.Maybe Prelude.Int,
    -- | The ID of the network interface.
    networkInterfaceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'AssignIpv6Addresses' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ipv6Addresses', 'assignIpv6Addresses_ipv6Addresses' - One or more specific IPv6 addresses to be assigned to the network
-- interface. You can\'t use this option if you\'re specifying a number of
-- IPv6 addresses.
--
-- 'ipv6AddressCount', 'assignIpv6Addresses_ipv6AddressCount' - The number of additional IPv6 addresses to assign to the network
-- interface. The specified number of IPv6 addresses are assigned in
-- addition to the existing IPv6 addresses that are already assigned to the
-- network interface. Amazon EC2 automatically selects the IPv6 addresses
-- from the subnet range. You can\'t use this option if specifying specific
-- IPv6 addresses.
--
-- 'networkInterfaceId', 'assignIpv6Addresses_networkInterfaceId' - The ID of the network interface.
newAssignIpv6Addresses ::
  -- | 'networkInterfaceId'
  Prelude.Text ->
  AssignIpv6Addresses
newAssignIpv6Addresses pNetworkInterfaceId_ =
  AssignIpv6Addresses'
    { ipv6Addresses =
        Prelude.Nothing,
      ipv6AddressCount = Prelude.Nothing,
      networkInterfaceId = pNetworkInterfaceId_
    }

-- | One or more specific IPv6 addresses to be assigned to the network
-- interface. You can\'t use this option if you\'re specifying a number of
-- IPv6 addresses.
assignIpv6Addresses_ipv6Addresses :: Lens.Lens' AssignIpv6Addresses (Prelude.Maybe [Prelude.Text])
assignIpv6Addresses_ipv6Addresses = Lens.lens (\AssignIpv6Addresses' {ipv6Addresses} -> ipv6Addresses) (\s@AssignIpv6Addresses' {} a -> s {ipv6Addresses = a} :: AssignIpv6Addresses) Prelude.. Lens.mapping Prelude._Coerce

-- | The number of additional IPv6 addresses to assign to the network
-- interface. The specified number of IPv6 addresses are assigned in
-- addition to the existing IPv6 addresses that are already assigned to the
-- network interface. Amazon EC2 automatically selects the IPv6 addresses
-- from the subnet range. You can\'t use this option if specifying specific
-- IPv6 addresses.
assignIpv6Addresses_ipv6AddressCount :: Lens.Lens' AssignIpv6Addresses (Prelude.Maybe Prelude.Int)
assignIpv6Addresses_ipv6AddressCount = Lens.lens (\AssignIpv6Addresses' {ipv6AddressCount} -> ipv6AddressCount) (\s@AssignIpv6Addresses' {} a -> s {ipv6AddressCount = a} :: AssignIpv6Addresses)

-- | The ID of the network interface.
assignIpv6Addresses_networkInterfaceId :: Lens.Lens' AssignIpv6Addresses Prelude.Text
assignIpv6Addresses_networkInterfaceId = Lens.lens (\AssignIpv6Addresses' {networkInterfaceId} -> networkInterfaceId) (\s@AssignIpv6Addresses' {} a -> s {networkInterfaceId = a} :: AssignIpv6Addresses)

instance Prelude.AWSRequest AssignIpv6Addresses where
  type
    Rs AssignIpv6Addresses =
      AssignIpv6AddressesResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          AssignIpv6AddressesResponse'
            Prelude.<$> ( x Prelude..@? "assignedIpv6Addresses"
                            Prelude..!@ Prelude.mempty
                            Prelude.>>= Prelude.may (Prelude.parseXMLList "item")
                        )
            Prelude.<*> (x Prelude..@? "networkInterfaceId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable AssignIpv6Addresses

instance Prelude.NFData AssignIpv6Addresses

instance Prelude.ToHeaders AssignIpv6Addresses where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath AssignIpv6Addresses where
  toPath = Prelude.const "/"

instance Prelude.ToQuery AssignIpv6Addresses where
  toQuery AssignIpv6Addresses' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("AssignIpv6Addresses" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2016-11-15" :: Prelude.ByteString),
        Prelude.toQuery
          ( Prelude.toQueryList "Ipv6Addresses"
              Prelude.<$> ipv6Addresses
          ),
        "Ipv6AddressCount" Prelude.=: ipv6AddressCount,
        "NetworkInterfaceId" Prelude.=: networkInterfaceId
      ]

-- | /See:/ 'newAssignIpv6AddressesResponse' smart constructor.
data AssignIpv6AddressesResponse = AssignIpv6AddressesResponse'
  { -- | The new IPv6 addresses assigned to the network interface. Existing IPv6
    -- addresses that were assigned to the network interface before the request
    -- are not included.
    assignedIpv6Addresses :: Prelude.Maybe [Prelude.Text],
    -- | The ID of the network interface.
    networkInterfaceId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'AssignIpv6AddressesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'assignedIpv6Addresses', 'assignIpv6AddressesResponse_assignedIpv6Addresses' - The new IPv6 addresses assigned to the network interface. Existing IPv6
-- addresses that were assigned to the network interface before the request
-- are not included.
--
-- 'networkInterfaceId', 'assignIpv6AddressesResponse_networkInterfaceId' - The ID of the network interface.
--
-- 'httpStatus', 'assignIpv6AddressesResponse_httpStatus' - The response's http status code.
newAssignIpv6AddressesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  AssignIpv6AddressesResponse
newAssignIpv6AddressesResponse pHttpStatus_ =
  AssignIpv6AddressesResponse'
    { assignedIpv6Addresses =
        Prelude.Nothing,
      networkInterfaceId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The new IPv6 addresses assigned to the network interface. Existing IPv6
-- addresses that were assigned to the network interface before the request
-- are not included.
assignIpv6AddressesResponse_assignedIpv6Addresses :: Lens.Lens' AssignIpv6AddressesResponse (Prelude.Maybe [Prelude.Text])
assignIpv6AddressesResponse_assignedIpv6Addresses = Lens.lens (\AssignIpv6AddressesResponse' {assignedIpv6Addresses} -> assignedIpv6Addresses) (\s@AssignIpv6AddressesResponse' {} a -> s {assignedIpv6Addresses = a} :: AssignIpv6AddressesResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The ID of the network interface.
assignIpv6AddressesResponse_networkInterfaceId :: Lens.Lens' AssignIpv6AddressesResponse (Prelude.Maybe Prelude.Text)
assignIpv6AddressesResponse_networkInterfaceId = Lens.lens (\AssignIpv6AddressesResponse' {networkInterfaceId} -> networkInterfaceId) (\s@AssignIpv6AddressesResponse' {} a -> s {networkInterfaceId = a} :: AssignIpv6AddressesResponse)

-- | The response's http status code.
assignIpv6AddressesResponse_httpStatus :: Lens.Lens' AssignIpv6AddressesResponse Prelude.Int
assignIpv6AddressesResponse_httpStatus = Lens.lens (\AssignIpv6AddressesResponse' {httpStatus} -> httpStatus) (\s@AssignIpv6AddressesResponse' {} a -> s {httpStatus = a} :: AssignIpv6AddressesResponse)

instance Prelude.NFData AssignIpv6AddressesResponse
