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
-- Module      : Amazonka.MGN.Types.NetworkInterface
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MGN.Types.NetworkInterface where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Network interface.
--
-- /See:/ 'newNetworkInterface' smart constructor.
data NetworkInterface = NetworkInterface'
  { -- | Network interface IPs.
    ips :: Prelude.Maybe [Prelude.Text],
    -- | Network interface Mac address.
    macAddress :: Prelude.Maybe Prelude.Text,
    -- | Network interface primary IP.
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
-- 'macAddress', 'networkInterface_macAddress' - Network interface Mac address.
--
-- 'isPrimary', 'networkInterface_isPrimary' - Network interface primary IP.
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

-- | Network interface Mac address.
networkInterface_macAddress :: Lens.Lens' NetworkInterface (Prelude.Maybe Prelude.Text)
networkInterface_macAddress = Lens.lens (\NetworkInterface' {macAddress} -> macAddress) (\s@NetworkInterface' {} a -> s {macAddress = a} :: NetworkInterface)

-- | Network interface primary IP.
networkInterface_isPrimary :: Lens.Lens' NetworkInterface (Prelude.Maybe Prelude.Bool)
networkInterface_isPrimary = Lens.lens (\NetworkInterface' {isPrimary} -> isPrimary) (\s@NetworkInterface' {} a -> s {isPrimary = a} :: NetworkInterface)

instance Data.FromJSON NetworkInterface where
  parseJSON =
    Data.withObject
      "NetworkInterface"
      ( \x ->
          NetworkInterface'
            Prelude.<$> (x Data..:? "ips" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "macAddress")
            Prelude.<*> (x Data..:? "isPrimary")
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
