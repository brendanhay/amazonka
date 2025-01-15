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
-- Module      : Amazonka.StorageGateway.Types.NetworkInterface
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.StorageGateway.Types.NetworkInterface where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes a gateway\'s network interface.
--
-- /See:/ 'newNetworkInterface' smart constructor.
data NetworkInterface = NetworkInterface'
  { -- | The Internet Protocol version 4 (IPv4) address of the interface.
    ipv4Address :: Prelude.Maybe Prelude.Text,
    -- | The Internet Protocol version 6 (IPv6) address of the interface.
    -- /Currently not supported/.
    ipv6Address :: Prelude.Maybe Prelude.Text,
    -- | The Media Access Control (MAC) address of the interface.
    --
    -- This is currently unsupported and will not be returned in output.
    macAddress :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'NetworkInterface' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ipv4Address', 'networkInterface_ipv4Address' - The Internet Protocol version 4 (IPv4) address of the interface.
--
-- 'ipv6Address', 'networkInterface_ipv6Address' - The Internet Protocol version 6 (IPv6) address of the interface.
-- /Currently not supported/.
--
-- 'macAddress', 'networkInterface_macAddress' - The Media Access Control (MAC) address of the interface.
--
-- This is currently unsupported and will not be returned in output.
newNetworkInterface ::
  NetworkInterface
newNetworkInterface =
  NetworkInterface'
    { ipv4Address = Prelude.Nothing,
      ipv6Address = Prelude.Nothing,
      macAddress = Prelude.Nothing
    }

-- | The Internet Protocol version 4 (IPv4) address of the interface.
networkInterface_ipv4Address :: Lens.Lens' NetworkInterface (Prelude.Maybe Prelude.Text)
networkInterface_ipv4Address = Lens.lens (\NetworkInterface' {ipv4Address} -> ipv4Address) (\s@NetworkInterface' {} a -> s {ipv4Address = a} :: NetworkInterface)

-- | The Internet Protocol version 6 (IPv6) address of the interface.
-- /Currently not supported/.
networkInterface_ipv6Address :: Lens.Lens' NetworkInterface (Prelude.Maybe Prelude.Text)
networkInterface_ipv6Address = Lens.lens (\NetworkInterface' {ipv6Address} -> ipv6Address) (\s@NetworkInterface' {} a -> s {ipv6Address = a} :: NetworkInterface)

-- | The Media Access Control (MAC) address of the interface.
--
-- This is currently unsupported and will not be returned in output.
networkInterface_macAddress :: Lens.Lens' NetworkInterface (Prelude.Maybe Prelude.Text)
networkInterface_macAddress = Lens.lens (\NetworkInterface' {macAddress} -> macAddress) (\s@NetworkInterface' {} a -> s {macAddress = a} :: NetworkInterface)

instance Data.FromJSON NetworkInterface where
  parseJSON =
    Data.withObject
      "NetworkInterface"
      ( \x ->
          NetworkInterface'
            Prelude.<$> (x Data..:? "Ipv4Address")
            Prelude.<*> (x Data..:? "Ipv6Address")
            Prelude.<*> (x Data..:? "MacAddress")
      )

instance Prelude.Hashable NetworkInterface where
  hashWithSalt _salt NetworkInterface' {..} =
    _salt
      `Prelude.hashWithSalt` ipv4Address
      `Prelude.hashWithSalt` ipv6Address
      `Prelude.hashWithSalt` macAddress

instance Prelude.NFData NetworkInterface where
  rnf NetworkInterface' {..} =
    Prelude.rnf ipv4Address `Prelude.seq`
      Prelude.rnf ipv6Address `Prelude.seq`
        Prelude.rnf macAddress
