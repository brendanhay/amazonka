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
-- Module      : Network.AWS.StorageGateway.Types.NetworkInterface
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.StorageGateway.Types.NetworkInterface where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Describes a gateway\'s network interface.
--
-- /See:/ 'newNetworkInterface' smart constructor.
data NetworkInterface = NetworkInterface'
  { -- | The Media Access Control (MAC) address of the interface.
    --
    -- This is currently unsupported and will not be returned in output.
    macAddress :: Core.Maybe Core.Text,
    -- | The Internet Protocol version 6 (IPv6) address of the interface.
    -- /Currently not supported/.
    ipv6Address :: Core.Maybe Core.Text,
    -- | The Internet Protocol version 4 (IPv4) address of the interface.
    ipv4Address :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'NetworkInterface' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'macAddress', 'networkInterface_macAddress' - The Media Access Control (MAC) address of the interface.
--
-- This is currently unsupported and will not be returned in output.
--
-- 'ipv6Address', 'networkInterface_ipv6Address' - The Internet Protocol version 6 (IPv6) address of the interface.
-- /Currently not supported/.
--
-- 'ipv4Address', 'networkInterface_ipv4Address' - The Internet Protocol version 4 (IPv4) address of the interface.
newNetworkInterface ::
  NetworkInterface
newNetworkInterface =
  NetworkInterface'
    { macAddress = Core.Nothing,
      ipv6Address = Core.Nothing,
      ipv4Address = Core.Nothing
    }

-- | The Media Access Control (MAC) address of the interface.
--
-- This is currently unsupported and will not be returned in output.
networkInterface_macAddress :: Lens.Lens' NetworkInterface (Core.Maybe Core.Text)
networkInterface_macAddress = Lens.lens (\NetworkInterface' {macAddress} -> macAddress) (\s@NetworkInterface' {} a -> s {macAddress = a} :: NetworkInterface)

-- | The Internet Protocol version 6 (IPv6) address of the interface.
-- /Currently not supported/.
networkInterface_ipv6Address :: Lens.Lens' NetworkInterface (Core.Maybe Core.Text)
networkInterface_ipv6Address = Lens.lens (\NetworkInterface' {ipv6Address} -> ipv6Address) (\s@NetworkInterface' {} a -> s {ipv6Address = a} :: NetworkInterface)

-- | The Internet Protocol version 4 (IPv4) address of the interface.
networkInterface_ipv4Address :: Lens.Lens' NetworkInterface (Core.Maybe Core.Text)
networkInterface_ipv4Address = Lens.lens (\NetworkInterface' {ipv4Address} -> ipv4Address) (\s@NetworkInterface' {} a -> s {ipv4Address = a} :: NetworkInterface)

instance Core.FromJSON NetworkInterface where
  parseJSON =
    Core.withObject
      "NetworkInterface"
      ( \x ->
          NetworkInterface'
            Core.<$> (x Core..:? "MacAddress")
            Core.<*> (x Core..:? "Ipv6Address")
            Core.<*> (x Core..:? "Ipv4Address")
      )

instance Core.Hashable NetworkInterface

instance Core.NFData NetworkInterface
