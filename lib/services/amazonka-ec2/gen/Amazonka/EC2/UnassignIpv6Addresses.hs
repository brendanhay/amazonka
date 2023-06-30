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
-- Module      : Amazonka.EC2.UnassignIpv6Addresses
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Unassigns one or more IPv6 addresses IPv4 Prefix Delegation prefixes
-- from a network interface.
module Amazonka.EC2.UnassignIpv6Addresses
  ( -- * Creating a Request
    UnassignIpv6Addresses (..),
    newUnassignIpv6Addresses,

    -- * Request Lenses
    unassignIpv6Addresses_ipv6Addresses,
    unassignIpv6Addresses_ipv6Prefixes,
    unassignIpv6Addresses_networkInterfaceId,

    -- * Destructuring the Response
    UnassignIpv6AddressesResponse (..),
    newUnassignIpv6AddressesResponse,

    -- * Response Lenses
    unassignIpv6AddressesResponse_networkInterfaceId,
    unassignIpv6AddressesResponse_unassignedIpv6Addresses,
    unassignIpv6AddressesResponse_unassignedIpv6Prefixes,
    unassignIpv6AddressesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUnassignIpv6Addresses' smart constructor.
data UnassignIpv6Addresses = UnassignIpv6Addresses'
  { -- | The IPv6 addresses to unassign from the network interface.
    ipv6Addresses :: Prelude.Maybe [Prelude.Text],
    -- | The IPv6 prefixes to unassign from the network interface.
    ipv6Prefixes :: Prelude.Maybe [Prelude.Text],
    -- | The ID of the network interface.
    networkInterfaceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UnassignIpv6Addresses' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ipv6Addresses', 'unassignIpv6Addresses_ipv6Addresses' - The IPv6 addresses to unassign from the network interface.
--
-- 'ipv6Prefixes', 'unassignIpv6Addresses_ipv6Prefixes' - The IPv6 prefixes to unassign from the network interface.
--
-- 'networkInterfaceId', 'unassignIpv6Addresses_networkInterfaceId' - The ID of the network interface.
newUnassignIpv6Addresses ::
  -- | 'networkInterfaceId'
  Prelude.Text ->
  UnassignIpv6Addresses
newUnassignIpv6Addresses pNetworkInterfaceId_ =
  UnassignIpv6Addresses'
    { ipv6Addresses =
        Prelude.Nothing,
      ipv6Prefixes = Prelude.Nothing,
      networkInterfaceId = pNetworkInterfaceId_
    }

-- | The IPv6 addresses to unassign from the network interface.
unassignIpv6Addresses_ipv6Addresses :: Lens.Lens' UnassignIpv6Addresses (Prelude.Maybe [Prelude.Text])
unassignIpv6Addresses_ipv6Addresses = Lens.lens (\UnassignIpv6Addresses' {ipv6Addresses} -> ipv6Addresses) (\s@UnassignIpv6Addresses' {} a -> s {ipv6Addresses = a} :: UnassignIpv6Addresses) Prelude.. Lens.mapping Lens.coerced

-- | The IPv6 prefixes to unassign from the network interface.
unassignIpv6Addresses_ipv6Prefixes :: Lens.Lens' UnassignIpv6Addresses (Prelude.Maybe [Prelude.Text])
unassignIpv6Addresses_ipv6Prefixes = Lens.lens (\UnassignIpv6Addresses' {ipv6Prefixes} -> ipv6Prefixes) (\s@UnassignIpv6Addresses' {} a -> s {ipv6Prefixes = a} :: UnassignIpv6Addresses) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the network interface.
unassignIpv6Addresses_networkInterfaceId :: Lens.Lens' UnassignIpv6Addresses Prelude.Text
unassignIpv6Addresses_networkInterfaceId = Lens.lens (\UnassignIpv6Addresses' {networkInterfaceId} -> networkInterfaceId) (\s@UnassignIpv6Addresses' {} a -> s {networkInterfaceId = a} :: UnassignIpv6Addresses)

instance Core.AWSRequest UnassignIpv6Addresses where
  type
    AWSResponse UnassignIpv6Addresses =
      UnassignIpv6AddressesResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          UnassignIpv6AddressesResponse'
            Prelude.<$> (x Data..@? "networkInterfaceId")
            Prelude.<*> ( x
                            Data..@? "unassignedIpv6Addresses"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "item")
                        )
            Prelude.<*> ( x
                            Data..@? "unassignedIpv6PrefixSet"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "item")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UnassignIpv6Addresses where
  hashWithSalt _salt UnassignIpv6Addresses' {..} =
    _salt
      `Prelude.hashWithSalt` ipv6Addresses
      `Prelude.hashWithSalt` ipv6Prefixes
      `Prelude.hashWithSalt` networkInterfaceId

instance Prelude.NFData UnassignIpv6Addresses where
  rnf UnassignIpv6Addresses' {..} =
    Prelude.rnf ipv6Addresses
      `Prelude.seq` Prelude.rnf ipv6Prefixes
      `Prelude.seq` Prelude.rnf networkInterfaceId

instance Data.ToHeaders UnassignIpv6Addresses where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath UnassignIpv6Addresses where
  toPath = Prelude.const "/"

instance Data.ToQuery UnassignIpv6Addresses where
  toQuery UnassignIpv6Addresses' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("UnassignIpv6Addresses" :: Prelude.ByteString),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        Data.toQuery
          ( Data.toQueryList "Ipv6Addresses"
              Prelude.<$> ipv6Addresses
          ),
        Data.toQuery
          ( Data.toQueryList "Ipv6Prefix"
              Prelude.<$> ipv6Prefixes
          ),
        "NetworkInterfaceId" Data.=: networkInterfaceId
      ]

-- | /See:/ 'newUnassignIpv6AddressesResponse' smart constructor.
data UnassignIpv6AddressesResponse = UnassignIpv6AddressesResponse'
  { -- | The ID of the network interface.
    networkInterfaceId :: Prelude.Maybe Prelude.Text,
    -- | The IPv6 addresses that have been unassigned from the network interface.
    unassignedIpv6Addresses :: Prelude.Maybe [Prelude.Text],
    -- | The IPv4 prefixes that have been unassigned from the network interface.
    unassignedIpv6Prefixes :: Prelude.Maybe [Prelude.Text],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UnassignIpv6AddressesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'networkInterfaceId', 'unassignIpv6AddressesResponse_networkInterfaceId' - The ID of the network interface.
--
-- 'unassignedIpv6Addresses', 'unassignIpv6AddressesResponse_unassignedIpv6Addresses' - The IPv6 addresses that have been unassigned from the network interface.
--
-- 'unassignedIpv6Prefixes', 'unassignIpv6AddressesResponse_unassignedIpv6Prefixes' - The IPv4 prefixes that have been unassigned from the network interface.
--
-- 'httpStatus', 'unassignIpv6AddressesResponse_httpStatus' - The response's http status code.
newUnassignIpv6AddressesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UnassignIpv6AddressesResponse
newUnassignIpv6AddressesResponse pHttpStatus_ =
  UnassignIpv6AddressesResponse'
    { networkInterfaceId =
        Prelude.Nothing,
      unassignedIpv6Addresses = Prelude.Nothing,
      unassignedIpv6Prefixes = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ID of the network interface.
unassignIpv6AddressesResponse_networkInterfaceId :: Lens.Lens' UnassignIpv6AddressesResponse (Prelude.Maybe Prelude.Text)
unassignIpv6AddressesResponse_networkInterfaceId = Lens.lens (\UnassignIpv6AddressesResponse' {networkInterfaceId} -> networkInterfaceId) (\s@UnassignIpv6AddressesResponse' {} a -> s {networkInterfaceId = a} :: UnassignIpv6AddressesResponse)

-- | The IPv6 addresses that have been unassigned from the network interface.
unassignIpv6AddressesResponse_unassignedIpv6Addresses :: Lens.Lens' UnassignIpv6AddressesResponse (Prelude.Maybe [Prelude.Text])
unassignIpv6AddressesResponse_unassignedIpv6Addresses = Lens.lens (\UnassignIpv6AddressesResponse' {unassignedIpv6Addresses} -> unassignedIpv6Addresses) (\s@UnassignIpv6AddressesResponse' {} a -> s {unassignedIpv6Addresses = a} :: UnassignIpv6AddressesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The IPv4 prefixes that have been unassigned from the network interface.
unassignIpv6AddressesResponse_unassignedIpv6Prefixes :: Lens.Lens' UnassignIpv6AddressesResponse (Prelude.Maybe [Prelude.Text])
unassignIpv6AddressesResponse_unassignedIpv6Prefixes = Lens.lens (\UnassignIpv6AddressesResponse' {unassignedIpv6Prefixes} -> unassignedIpv6Prefixes) (\s@UnassignIpv6AddressesResponse' {} a -> s {unassignedIpv6Prefixes = a} :: UnassignIpv6AddressesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
unassignIpv6AddressesResponse_httpStatus :: Lens.Lens' UnassignIpv6AddressesResponse Prelude.Int
unassignIpv6AddressesResponse_httpStatus = Lens.lens (\UnassignIpv6AddressesResponse' {httpStatus} -> httpStatus) (\s@UnassignIpv6AddressesResponse' {} a -> s {httpStatus = a} :: UnassignIpv6AddressesResponse)

instance Prelude.NFData UnassignIpv6AddressesResponse where
  rnf UnassignIpv6AddressesResponse' {..} =
    Prelude.rnf networkInterfaceId
      `Prelude.seq` Prelude.rnf unassignedIpv6Addresses
      `Prelude.seq` Prelude.rnf unassignedIpv6Prefixes
      `Prelude.seq` Prelude.rnf httpStatus
