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
-- Module      : Amazonka.MediaConnect.Types.FailoverConfig
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConnect.Types.FailoverConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaConnect.Types.FailoverMode
import Amazonka.MediaConnect.Types.SourcePriority
import Amazonka.MediaConnect.Types.State
import qualified Amazonka.Prelude as Prelude

-- | The settings for source failover.
--
-- /See:/ 'newFailoverConfig' smart constructor.
data FailoverConfig = FailoverConfig'
  { -- | Search window time to look for dash-7 packets
    recoveryWindow :: Prelude.Maybe Prelude.Int,
    state :: Prelude.Maybe State,
    -- | The priority you want to assign to a source. You can have a primary
    -- stream and a backup stream or two equally prioritized streams.
    sourcePriority :: Prelude.Maybe SourcePriority,
    -- | The type of failover you choose for this flow. MERGE combines the source
    -- streams into a single stream, allowing graceful recovery from any
    -- single-source loss. FAILOVER allows switching between different streams.
    failoverMode :: Prelude.Maybe FailoverMode
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FailoverConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'recoveryWindow', 'failoverConfig_recoveryWindow' - Search window time to look for dash-7 packets
--
-- 'state', 'failoverConfig_state' - Undocumented member.
--
-- 'sourcePriority', 'failoverConfig_sourcePriority' - The priority you want to assign to a source. You can have a primary
-- stream and a backup stream or two equally prioritized streams.
--
-- 'failoverMode', 'failoverConfig_failoverMode' - The type of failover you choose for this flow. MERGE combines the source
-- streams into a single stream, allowing graceful recovery from any
-- single-source loss. FAILOVER allows switching between different streams.
newFailoverConfig ::
  FailoverConfig
newFailoverConfig =
  FailoverConfig'
    { recoveryWindow = Prelude.Nothing,
      state = Prelude.Nothing,
      sourcePriority = Prelude.Nothing,
      failoverMode = Prelude.Nothing
    }

-- | Search window time to look for dash-7 packets
failoverConfig_recoveryWindow :: Lens.Lens' FailoverConfig (Prelude.Maybe Prelude.Int)
failoverConfig_recoveryWindow = Lens.lens (\FailoverConfig' {recoveryWindow} -> recoveryWindow) (\s@FailoverConfig' {} a -> s {recoveryWindow = a} :: FailoverConfig)

-- | Undocumented member.
failoverConfig_state :: Lens.Lens' FailoverConfig (Prelude.Maybe State)
failoverConfig_state = Lens.lens (\FailoverConfig' {state} -> state) (\s@FailoverConfig' {} a -> s {state = a} :: FailoverConfig)

-- | The priority you want to assign to a source. You can have a primary
-- stream and a backup stream or two equally prioritized streams.
failoverConfig_sourcePriority :: Lens.Lens' FailoverConfig (Prelude.Maybe SourcePriority)
failoverConfig_sourcePriority = Lens.lens (\FailoverConfig' {sourcePriority} -> sourcePriority) (\s@FailoverConfig' {} a -> s {sourcePriority = a} :: FailoverConfig)

-- | The type of failover you choose for this flow. MERGE combines the source
-- streams into a single stream, allowing graceful recovery from any
-- single-source loss. FAILOVER allows switching between different streams.
failoverConfig_failoverMode :: Lens.Lens' FailoverConfig (Prelude.Maybe FailoverMode)
failoverConfig_failoverMode = Lens.lens (\FailoverConfig' {failoverMode} -> failoverMode) (\s@FailoverConfig' {} a -> s {failoverMode = a} :: FailoverConfig)

instance Data.FromJSON FailoverConfig where
  parseJSON =
    Data.withObject
      "FailoverConfig"
      ( \x ->
          FailoverConfig'
            Prelude.<$> (x Data..:? "recoveryWindow")
            Prelude.<*> (x Data..:? "state")
            Prelude.<*> (x Data..:? "sourcePriority")
            Prelude.<*> (x Data..:? "failoverMode")
      )

instance Prelude.Hashable FailoverConfig where
  hashWithSalt _salt FailoverConfig' {..} =
    _salt `Prelude.hashWithSalt` recoveryWindow
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` sourcePriority
      `Prelude.hashWithSalt` failoverMode

instance Prelude.NFData FailoverConfig where
  rnf FailoverConfig' {..} =
    Prelude.rnf recoveryWindow
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf sourcePriority
      `Prelude.seq` Prelude.rnf failoverMode

instance Data.ToJSON FailoverConfig where
  toJSON FailoverConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("recoveryWindow" Data..=)
              Prelude.<$> recoveryWindow,
            ("state" Data..=) Prelude.<$> state,
            ("sourcePriority" Data..=)
              Prelude.<$> sourcePriority,
            ("failoverMode" Data..=) Prelude.<$> failoverMode
          ]
      )
