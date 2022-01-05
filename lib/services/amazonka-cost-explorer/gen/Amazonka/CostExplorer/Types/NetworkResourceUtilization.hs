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
-- Module      : Amazonka.CostExplorer.Types.NetworkResourceUtilization
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CostExplorer.Types.NetworkResourceUtilization where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | The network field that contains a list of network metrics that are
-- associated with the current instance.
--
-- /See:/ 'newNetworkResourceUtilization' smart constructor.
data NetworkResourceUtilization = NetworkResourceUtilization'
  { -- | The network outgress packets that are measured in packets per second.
    networkPacketsOutPerSecond :: Prelude.Maybe Prelude.Text,
    -- | The network inbound throughput utilization measured in Bytes per second.
    networkInBytesPerSecond :: Prelude.Maybe Prelude.Text,
    -- | The network ingress packets that are measured in packets per second.
    networkPacketsInPerSecond :: Prelude.Maybe Prelude.Text,
    -- | The network outbound throughput utilization measured in Bytes per
    -- second.
    networkOutBytesPerSecond :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'NetworkResourceUtilization' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'networkPacketsOutPerSecond', 'networkResourceUtilization_networkPacketsOutPerSecond' - The network outgress packets that are measured in packets per second.
--
-- 'networkInBytesPerSecond', 'networkResourceUtilization_networkInBytesPerSecond' - The network inbound throughput utilization measured in Bytes per second.
--
-- 'networkPacketsInPerSecond', 'networkResourceUtilization_networkPacketsInPerSecond' - The network ingress packets that are measured in packets per second.
--
-- 'networkOutBytesPerSecond', 'networkResourceUtilization_networkOutBytesPerSecond' - The network outbound throughput utilization measured in Bytes per
-- second.
newNetworkResourceUtilization ::
  NetworkResourceUtilization
newNetworkResourceUtilization =
  NetworkResourceUtilization'
    { networkPacketsOutPerSecond =
        Prelude.Nothing,
      networkInBytesPerSecond = Prelude.Nothing,
      networkPacketsInPerSecond = Prelude.Nothing,
      networkOutBytesPerSecond = Prelude.Nothing
    }

-- | The network outgress packets that are measured in packets per second.
networkResourceUtilization_networkPacketsOutPerSecond :: Lens.Lens' NetworkResourceUtilization (Prelude.Maybe Prelude.Text)
networkResourceUtilization_networkPacketsOutPerSecond = Lens.lens (\NetworkResourceUtilization' {networkPacketsOutPerSecond} -> networkPacketsOutPerSecond) (\s@NetworkResourceUtilization' {} a -> s {networkPacketsOutPerSecond = a} :: NetworkResourceUtilization)

-- | The network inbound throughput utilization measured in Bytes per second.
networkResourceUtilization_networkInBytesPerSecond :: Lens.Lens' NetworkResourceUtilization (Prelude.Maybe Prelude.Text)
networkResourceUtilization_networkInBytesPerSecond = Lens.lens (\NetworkResourceUtilization' {networkInBytesPerSecond} -> networkInBytesPerSecond) (\s@NetworkResourceUtilization' {} a -> s {networkInBytesPerSecond = a} :: NetworkResourceUtilization)

-- | The network ingress packets that are measured in packets per second.
networkResourceUtilization_networkPacketsInPerSecond :: Lens.Lens' NetworkResourceUtilization (Prelude.Maybe Prelude.Text)
networkResourceUtilization_networkPacketsInPerSecond = Lens.lens (\NetworkResourceUtilization' {networkPacketsInPerSecond} -> networkPacketsInPerSecond) (\s@NetworkResourceUtilization' {} a -> s {networkPacketsInPerSecond = a} :: NetworkResourceUtilization)

-- | The network outbound throughput utilization measured in Bytes per
-- second.
networkResourceUtilization_networkOutBytesPerSecond :: Lens.Lens' NetworkResourceUtilization (Prelude.Maybe Prelude.Text)
networkResourceUtilization_networkOutBytesPerSecond = Lens.lens (\NetworkResourceUtilization' {networkOutBytesPerSecond} -> networkOutBytesPerSecond) (\s@NetworkResourceUtilization' {} a -> s {networkOutBytesPerSecond = a} :: NetworkResourceUtilization)

instance Core.FromJSON NetworkResourceUtilization where
  parseJSON =
    Core.withObject
      "NetworkResourceUtilization"
      ( \x ->
          NetworkResourceUtilization'
            Prelude.<$> (x Core..:? "NetworkPacketsOutPerSecond")
            Prelude.<*> (x Core..:? "NetworkInBytesPerSecond")
            Prelude.<*> (x Core..:? "NetworkPacketsInPerSecond")
            Prelude.<*> (x Core..:? "NetworkOutBytesPerSecond")
      )

instance Prelude.Hashable NetworkResourceUtilization where
  hashWithSalt _salt NetworkResourceUtilization' {..} =
    _salt
      `Prelude.hashWithSalt` networkPacketsOutPerSecond
      `Prelude.hashWithSalt` networkInBytesPerSecond
      `Prelude.hashWithSalt` networkPacketsInPerSecond
      `Prelude.hashWithSalt` networkOutBytesPerSecond

instance Prelude.NFData NetworkResourceUtilization where
  rnf NetworkResourceUtilization' {..} =
    Prelude.rnf networkPacketsOutPerSecond
      `Prelude.seq` Prelude.rnf networkInBytesPerSecond
      `Prelude.seq` Prelude.rnf networkPacketsInPerSecond
      `Prelude.seq` Prelude.rnf networkOutBytesPerSecond
