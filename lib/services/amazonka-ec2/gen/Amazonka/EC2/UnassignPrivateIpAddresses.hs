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
-- Module      : Amazonka.EC2.UnassignPrivateIpAddresses
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Unassigns one or more secondary private IP addresses, or IPv4 Prefix
-- Delegation prefixes from a network interface.
module Amazonka.EC2.UnassignPrivateIpAddresses
  ( -- * Creating a Request
    UnassignPrivateIpAddresses (..),
    newUnassignPrivateIpAddresses,

    -- * Request Lenses
    unassignPrivateIpAddresses_ipv4Prefixes,
    unassignPrivateIpAddresses_privateIpAddresses,
    unassignPrivateIpAddresses_networkInterfaceId,

    -- * Destructuring the Response
    UnassignPrivateIpAddressesResponse (..),
    newUnassignPrivateIpAddressesResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Contains the parameters for UnassignPrivateIpAddresses.
--
-- /See:/ 'newUnassignPrivateIpAddresses' smart constructor.
data UnassignPrivateIpAddresses = UnassignPrivateIpAddresses'
  { -- | The IPv4 prefixes to unassign from the network interface.
    ipv4Prefixes :: Prelude.Maybe [Prelude.Text],
    -- | The secondary private IP addresses to unassign from the network
    -- interface. You can specify this option multiple times to unassign more
    -- than one IP address.
    privateIpAddresses :: Prelude.Maybe [Prelude.Text],
    -- | The ID of the network interface.
    networkInterfaceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UnassignPrivateIpAddresses' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ipv4Prefixes', 'unassignPrivateIpAddresses_ipv4Prefixes' - The IPv4 prefixes to unassign from the network interface.
--
-- 'privateIpAddresses', 'unassignPrivateIpAddresses_privateIpAddresses' - The secondary private IP addresses to unassign from the network
-- interface. You can specify this option multiple times to unassign more
-- than one IP address.
--
-- 'networkInterfaceId', 'unassignPrivateIpAddresses_networkInterfaceId' - The ID of the network interface.
newUnassignPrivateIpAddresses ::
  -- | 'networkInterfaceId'
  Prelude.Text ->
  UnassignPrivateIpAddresses
newUnassignPrivateIpAddresses pNetworkInterfaceId_ =
  UnassignPrivateIpAddresses'
    { ipv4Prefixes =
        Prelude.Nothing,
      privateIpAddresses = Prelude.Nothing,
      networkInterfaceId = pNetworkInterfaceId_
    }

-- | The IPv4 prefixes to unassign from the network interface.
unassignPrivateIpAddresses_ipv4Prefixes :: Lens.Lens' UnassignPrivateIpAddresses (Prelude.Maybe [Prelude.Text])
unassignPrivateIpAddresses_ipv4Prefixes = Lens.lens (\UnassignPrivateIpAddresses' {ipv4Prefixes} -> ipv4Prefixes) (\s@UnassignPrivateIpAddresses' {} a -> s {ipv4Prefixes = a} :: UnassignPrivateIpAddresses) Prelude.. Lens.mapping Lens.coerced

-- | The secondary private IP addresses to unassign from the network
-- interface. You can specify this option multiple times to unassign more
-- than one IP address.
unassignPrivateIpAddresses_privateIpAddresses :: Lens.Lens' UnassignPrivateIpAddresses (Prelude.Maybe [Prelude.Text])
unassignPrivateIpAddresses_privateIpAddresses = Lens.lens (\UnassignPrivateIpAddresses' {privateIpAddresses} -> privateIpAddresses) (\s@UnassignPrivateIpAddresses' {} a -> s {privateIpAddresses = a} :: UnassignPrivateIpAddresses) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the network interface.
unassignPrivateIpAddresses_networkInterfaceId :: Lens.Lens' UnassignPrivateIpAddresses Prelude.Text
unassignPrivateIpAddresses_networkInterfaceId = Lens.lens (\UnassignPrivateIpAddresses' {networkInterfaceId} -> networkInterfaceId) (\s@UnassignPrivateIpAddresses' {} a -> s {networkInterfaceId = a} :: UnassignPrivateIpAddresses)

instance Core.AWSRequest UnassignPrivateIpAddresses where
  type
    AWSResponse UnassignPrivateIpAddresses =
      UnassignPrivateIpAddressesResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveNull
      UnassignPrivateIpAddressesResponse'

instance Prelude.Hashable UnassignPrivateIpAddresses where
  hashWithSalt _salt UnassignPrivateIpAddresses' {..} =
    _salt
      `Prelude.hashWithSalt` ipv4Prefixes
      `Prelude.hashWithSalt` privateIpAddresses
      `Prelude.hashWithSalt` networkInterfaceId

instance Prelude.NFData UnassignPrivateIpAddresses where
  rnf UnassignPrivateIpAddresses' {..} =
    Prelude.rnf ipv4Prefixes
      `Prelude.seq` Prelude.rnf privateIpAddresses
      `Prelude.seq` Prelude.rnf networkInterfaceId

instance Data.ToHeaders UnassignPrivateIpAddresses where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath UnassignPrivateIpAddresses where
  toPath = Prelude.const "/"

instance Data.ToQuery UnassignPrivateIpAddresses where
  toQuery UnassignPrivateIpAddresses' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("UnassignPrivateIpAddresses" :: Prelude.ByteString),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        Data.toQuery
          ( Data.toQueryList "Ipv4Prefix"
              Prelude.<$> ipv4Prefixes
          ),
        Data.toQuery
          ( Data.toQueryList "PrivateIpAddress"
              Prelude.<$> privateIpAddresses
          ),
        "NetworkInterfaceId" Data.=: networkInterfaceId
      ]

-- | /See:/ 'newUnassignPrivateIpAddressesResponse' smart constructor.
data UnassignPrivateIpAddressesResponse = UnassignPrivateIpAddressesResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UnassignPrivateIpAddressesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newUnassignPrivateIpAddressesResponse ::
  UnassignPrivateIpAddressesResponse
newUnassignPrivateIpAddressesResponse =
  UnassignPrivateIpAddressesResponse'

instance
  Prelude.NFData
    UnassignPrivateIpAddressesResponse
  where
  rnf _ = ()
