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
-- Module      : Amazonka.MediaConnect.Types.UpdateFailoverConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConnect.Types.UpdateFailoverConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaConnect.Types.FailoverMode
import Amazonka.MediaConnect.Types.SourcePriority
import Amazonka.MediaConnect.Types.State
import qualified Amazonka.Prelude as Prelude

-- | The settings for source failover.
--
-- /See:/ 'newUpdateFailoverConfig' smart constructor.
data UpdateFailoverConfig = UpdateFailoverConfig'
  { -- | The type of failover you choose for this flow. MERGE combines the source
    -- streams into a single stream, allowing graceful recovery from any
    -- single-source loss. FAILOVER allows switching between different streams.
    failoverMode :: Prelude.Maybe FailoverMode,
    -- | Recovery window time to look for dash-7 packets
    recoveryWindow :: Prelude.Maybe Prelude.Int,
    -- | The priority you want to assign to a source. You can have a primary
    -- stream and a backup stream or two equally prioritized streams.
    sourcePriority :: Prelude.Maybe SourcePriority,
    state :: Prelude.Maybe State
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateFailoverConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'failoverMode', 'updateFailoverConfig_failoverMode' - The type of failover you choose for this flow. MERGE combines the source
-- streams into a single stream, allowing graceful recovery from any
-- single-source loss. FAILOVER allows switching between different streams.
--
-- 'recoveryWindow', 'updateFailoverConfig_recoveryWindow' - Recovery window time to look for dash-7 packets
--
-- 'sourcePriority', 'updateFailoverConfig_sourcePriority' - The priority you want to assign to a source. You can have a primary
-- stream and a backup stream or two equally prioritized streams.
--
-- 'state', 'updateFailoverConfig_state' - Undocumented member.
newUpdateFailoverConfig ::
  UpdateFailoverConfig
newUpdateFailoverConfig =
  UpdateFailoverConfig'
    { failoverMode =
        Prelude.Nothing,
      recoveryWindow = Prelude.Nothing,
      sourcePriority = Prelude.Nothing,
      state = Prelude.Nothing
    }

-- | The type of failover you choose for this flow. MERGE combines the source
-- streams into a single stream, allowing graceful recovery from any
-- single-source loss. FAILOVER allows switching between different streams.
updateFailoverConfig_failoverMode :: Lens.Lens' UpdateFailoverConfig (Prelude.Maybe FailoverMode)
updateFailoverConfig_failoverMode = Lens.lens (\UpdateFailoverConfig' {failoverMode} -> failoverMode) (\s@UpdateFailoverConfig' {} a -> s {failoverMode = a} :: UpdateFailoverConfig)

-- | Recovery window time to look for dash-7 packets
updateFailoverConfig_recoveryWindow :: Lens.Lens' UpdateFailoverConfig (Prelude.Maybe Prelude.Int)
updateFailoverConfig_recoveryWindow = Lens.lens (\UpdateFailoverConfig' {recoveryWindow} -> recoveryWindow) (\s@UpdateFailoverConfig' {} a -> s {recoveryWindow = a} :: UpdateFailoverConfig)

-- | The priority you want to assign to a source. You can have a primary
-- stream and a backup stream or two equally prioritized streams.
updateFailoverConfig_sourcePriority :: Lens.Lens' UpdateFailoverConfig (Prelude.Maybe SourcePriority)
updateFailoverConfig_sourcePriority = Lens.lens (\UpdateFailoverConfig' {sourcePriority} -> sourcePriority) (\s@UpdateFailoverConfig' {} a -> s {sourcePriority = a} :: UpdateFailoverConfig)

-- | Undocumented member.
updateFailoverConfig_state :: Lens.Lens' UpdateFailoverConfig (Prelude.Maybe State)
updateFailoverConfig_state = Lens.lens (\UpdateFailoverConfig' {state} -> state) (\s@UpdateFailoverConfig' {} a -> s {state = a} :: UpdateFailoverConfig)

instance Prelude.Hashable UpdateFailoverConfig where
  hashWithSalt _salt UpdateFailoverConfig' {..} =
    _salt `Prelude.hashWithSalt` failoverMode
      `Prelude.hashWithSalt` recoveryWindow
      `Prelude.hashWithSalt` sourcePriority
      `Prelude.hashWithSalt` state

instance Prelude.NFData UpdateFailoverConfig where
  rnf UpdateFailoverConfig' {..} =
    Prelude.rnf failoverMode
      `Prelude.seq` Prelude.rnf recoveryWindow
      `Prelude.seq` Prelude.rnf sourcePriority
      `Prelude.seq` Prelude.rnf state

instance Data.ToJSON UpdateFailoverConfig where
  toJSON UpdateFailoverConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("failoverMode" Data..=) Prelude.<$> failoverMode,
            ("recoveryWindow" Data..=)
              Prelude.<$> recoveryWindow,
            ("sourcePriority" Data..=)
              Prelude.<$> sourcePriority,
            ("state" Data..=) Prelude.<$> state
          ]
      )
