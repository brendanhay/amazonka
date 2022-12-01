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
-- Module      : Amazonka.DrS.Types.NetworkInterface
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DrS.Types.NetworkInterface where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Network interface.
--
-- /See:/ 'newNetworkInterface' smart constructor.
data NetworkInterface = NetworkInterface'
  { -- | Network interface IPs.
    ips :: Prelude.Maybe [Prelude.Text],
    -- | The MAC address of the network interface.
    macAddress :: Prelude.Maybe Prelude.Text,
    -- | Whether this is the primary network interface.
    isPrimary :: Prelude.Maybe Prelude.Bool
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
-- 'ips', 'networkInterface_ips' - Network interface IPs.
--
-- 'macAddress', 'networkInterface_macAddress' - The MAC address of the network interface.
--
-- 'isPrimary', 'networkInterface_isPrimary' - Whether this is the primary network interface.
newNetworkInterface ::
  NetworkInterface
newNetworkInterface =
  NetworkInterface'
    { ips = Prelude.Nothing,
      macAddress = Prelude.Nothing,
      isPrimary = Prelude.Nothing
    }

-- | Network interface IPs.
networkInterface_ips :: Lens.Lens' NetworkInterface (Prelude.Maybe [Prelude.Text])
networkInterface_ips = Lens.lens (\NetworkInterface' {ips} -> ips) (\s@NetworkInterface' {} a -> s {ips = a} :: NetworkInterface) Prelude.. Lens.mapping Lens.coerced

-- | The MAC address of the network interface.
networkInterface_macAddress :: Lens.Lens' NetworkInterface (Prelude.Maybe Prelude.Text)
networkInterface_macAddress = Lens.lens (\NetworkInterface' {macAddress} -> macAddress) (\s@NetworkInterface' {} a -> s {macAddress = a} :: NetworkInterface)

-- | Whether this is the primary network interface.
networkInterface_isPrimary :: Lens.Lens' NetworkInterface (Prelude.Maybe Prelude.Bool)
networkInterface_isPrimary = Lens.lens (\NetworkInterface' {isPrimary} -> isPrimary) (\s@NetworkInterface' {} a -> s {isPrimary = a} :: NetworkInterface)

instance Core.FromJSON NetworkInterface where
  parseJSON =
    Core.withObject
      "NetworkInterface"
      ( \x ->
          NetworkInterface'
            Prelude.<$> (x Core..:? "ips" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "macAddress")
            Prelude.<*> (x Core..:? "isPrimary")
      )

instance Prelude.Hashable NetworkInterface where
  hashWithSalt _salt NetworkInterface' {..} =
    _salt `Prelude.hashWithSalt` ips
      `Prelude.hashWithSalt` macAddress
      `Prelude.hashWithSalt` isPrimary

instance Prelude.NFData NetworkInterface where
  rnf NetworkInterface' {..} =
    Prelude.rnf ips
      `Prelude.seq` Prelude.rnf macAddress
      `Prelude.seq` Prelude.rnf isPrimary
