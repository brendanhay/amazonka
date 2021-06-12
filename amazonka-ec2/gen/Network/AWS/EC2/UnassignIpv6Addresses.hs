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
-- Module      : Network.AWS.EC2.UnassignIpv6Addresses
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Unassigns one or more IPv6 addresses from a network interface.
module Network.AWS.EC2.UnassignIpv6Addresses
  ( -- * Creating a Request
    UnassignIpv6Addresses (..),
    newUnassignIpv6Addresses,

    -- * Request Lenses
    unassignIpv6Addresses_ipv6Addresses,
    unassignIpv6Addresses_networkInterfaceId,

    -- * Destructuring the Response
    UnassignIpv6AddressesResponse (..),
    newUnassignIpv6AddressesResponse,

    -- * Response Lenses
    unassignIpv6AddressesResponse_unassignedIpv6Addresses,
    unassignIpv6AddressesResponse_networkInterfaceId,
    unassignIpv6AddressesResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUnassignIpv6Addresses' smart constructor.
data UnassignIpv6Addresses = UnassignIpv6Addresses'
  { -- | The IPv6 addresses to unassign from the network interface.
    ipv6Addresses :: [Core.Text],
    -- | The ID of the network interface.
    networkInterfaceId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
-- 'networkInterfaceId', 'unassignIpv6Addresses_networkInterfaceId' - The ID of the network interface.
newUnassignIpv6Addresses ::
  -- | 'networkInterfaceId'
  Core.Text ->
  UnassignIpv6Addresses
newUnassignIpv6Addresses pNetworkInterfaceId_ =
  UnassignIpv6Addresses'
    { ipv6Addresses = Core.mempty,
      networkInterfaceId = pNetworkInterfaceId_
    }

-- | The IPv6 addresses to unassign from the network interface.
unassignIpv6Addresses_ipv6Addresses :: Lens.Lens' UnassignIpv6Addresses [Core.Text]
unassignIpv6Addresses_ipv6Addresses = Lens.lens (\UnassignIpv6Addresses' {ipv6Addresses} -> ipv6Addresses) (\s@UnassignIpv6Addresses' {} a -> s {ipv6Addresses = a} :: UnassignIpv6Addresses) Core.. Lens._Coerce

-- | The ID of the network interface.
unassignIpv6Addresses_networkInterfaceId :: Lens.Lens' UnassignIpv6Addresses Core.Text
unassignIpv6Addresses_networkInterfaceId = Lens.lens (\UnassignIpv6Addresses' {networkInterfaceId} -> networkInterfaceId) (\s@UnassignIpv6Addresses' {} a -> s {networkInterfaceId = a} :: UnassignIpv6Addresses)

instance Core.AWSRequest UnassignIpv6Addresses where
  type
    AWSResponse UnassignIpv6Addresses =
      UnassignIpv6AddressesResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          UnassignIpv6AddressesResponse'
            Core.<$> ( x Core..@? "unassignedIpv6Addresses"
                         Core..!@ Core.mempty
                         Core.>>= Core.may (Core.parseXMLList "item")
                     )
            Core.<*> (x Core..@? "networkInterfaceId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable UnassignIpv6Addresses

instance Core.NFData UnassignIpv6Addresses

instance Core.ToHeaders UnassignIpv6Addresses where
  toHeaders = Core.const Core.mempty

instance Core.ToPath UnassignIpv6Addresses where
  toPath = Core.const "/"

instance Core.ToQuery UnassignIpv6Addresses where
  toQuery UnassignIpv6Addresses' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("UnassignIpv6Addresses" :: Core.ByteString),
        "Version" Core.=: ("2016-11-15" :: Core.ByteString),
        Core.toQueryList "Ipv6Addresses" ipv6Addresses,
        "NetworkInterfaceId" Core.=: networkInterfaceId
      ]

-- | /See:/ 'newUnassignIpv6AddressesResponse' smart constructor.
data UnassignIpv6AddressesResponse = UnassignIpv6AddressesResponse'
  { -- | The IPv6 addresses that have been unassigned from the network interface.
    unassignedIpv6Addresses :: Core.Maybe [Core.Text],
    -- | The ID of the network interface.
    networkInterfaceId :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UnassignIpv6AddressesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'unassignedIpv6Addresses', 'unassignIpv6AddressesResponse_unassignedIpv6Addresses' - The IPv6 addresses that have been unassigned from the network interface.
--
-- 'networkInterfaceId', 'unassignIpv6AddressesResponse_networkInterfaceId' - The ID of the network interface.
--
-- 'httpStatus', 'unassignIpv6AddressesResponse_httpStatus' - The response's http status code.
newUnassignIpv6AddressesResponse ::
  -- | 'httpStatus'
  Core.Int ->
  UnassignIpv6AddressesResponse
newUnassignIpv6AddressesResponse pHttpStatus_ =
  UnassignIpv6AddressesResponse'
    { unassignedIpv6Addresses =
        Core.Nothing,
      networkInterfaceId = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The IPv6 addresses that have been unassigned from the network interface.
unassignIpv6AddressesResponse_unassignedIpv6Addresses :: Lens.Lens' UnassignIpv6AddressesResponse (Core.Maybe [Core.Text])
unassignIpv6AddressesResponse_unassignedIpv6Addresses = Lens.lens (\UnassignIpv6AddressesResponse' {unassignedIpv6Addresses} -> unassignedIpv6Addresses) (\s@UnassignIpv6AddressesResponse' {} a -> s {unassignedIpv6Addresses = a} :: UnassignIpv6AddressesResponse) Core.. Lens.mapping Lens._Coerce

-- | The ID of the network interface.
unassignIpv6AddressesResponse_networkInterfaceId :: Lens.Lens' UnassignIpv6AddressesResponse (Core.Maybe Core.Text)
unassignIpv6AddressesResponse_networkInterfaceId = Lens.lens (\UnassignIpv6AddressesResponse' {networkInterfaceId} -> networkInterfaceId) (\s@UnassignIpv6AddressesResponse' {} a -> s {networkInterfaceId = a} :: UnassignIpv6AddressesResponse)

-- | The response's http status code.
unassignIpv6AddressesResponse_httpStatus :: Lens.Lens' UnassignIpv6AddressesResponse Core.Int
unassignIpv6AddressesResponse_httpStatus = Lens.lens (\UnassignIpv6AddressesResponse' {httpStatus} -> httpStatus) (\s@UnassignIpv6AddressesResponse' {} a -> s {httpStatus = a} :: UnassignIpv6AddressesResponse)

instance Core.NFData UnassignIpv6AddressesResponse
