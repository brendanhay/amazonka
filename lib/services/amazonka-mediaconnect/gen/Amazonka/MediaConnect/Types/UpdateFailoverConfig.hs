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
-- Copyright   : (c) 2013-2022 Brendan Hay
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
  { -- | Recovery window time to look for dash-7 packets
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
-- Create a value of 'UpdateFailoverConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'recoveryWindow', 'updateFailoverConfig_recoveryWindow' - Recovery window time to look for dash-7 packets
--
-- 'state', 'updateFailoverConfig_state' - Undocumented member.
--
-- 'sourcePriority', 'updateFailoverConfig_sourcePriority' - The priority you want to assign to a source. You can have a primary
-- stream and a backup stream or two equally prioritized streams.
--
-- 'failoverMode', 'updateFailoverConfig_failoverMode' - The type of failover you choose for this flow. MERGE combines the source
-- streams into a single stream, allowing graceful recovery from any
-- single-source loss. FAILOVER allows switching between different streams.
newUpdateFailoverConfig ::
  UpdateFailoverConfig
newUpdateFailoverConfig =
  UpdateFailoverConfig'
    { recoveryWindow =
        Prelude.Nothing,
      state = Prelude.Nothing,
      sourcePriority = Prelude.Nothing,
      failoverMode = Prelude.Nothing
    }

-- | Recovery window time to look for dash-7 packets
updateFailoverConfig_recoveryWindow :: Lens.Lens' UpdateFailoverConfig (Prelude.Maybe Prelude.Int)
updateFailoverConfig_recoveryWindow = Lens.lens (\UpdateFailoverConfig' {recoveryWindow} -> recoveryWindow) (\s@UpdateFailoverConfig' {} a -> s {recoveryWindow = a} :: UpdateFailoverConfig)

-- | Undocumented member.
updateFailoverConfig_state :: Lens.Lens' UpdateFailoverConfig (Prelude.Maybe State)
updateFailoverConfig_state = Lens.lens (\UpdateFailoverConfig' {state} -> state) (\s@UpdateFailoverConfig' {} a -> s {state = a} :: UpdateFailoverConfig)

-- | The priority you want to assign to a source. You can have a primary
-- stream and a backup stream or two equally prioritized streams.
updateFailoverConfig_sourcePriority :: Lens.Lens' UpdateFailoverConfig (Prelude.Maybe SourcePriority)
updateFailoverConfig_sourcePriority = Lens.lens (\UpdateFailoverConfig' {sourcePriority} -> sourcePriority) (\s@UpdateFailoverConfig' {} a -> s {sourcePriority = a} :: UpdateFailoverConfig)

-- | The type of failover you choose for this flow. MERGE combines the source
-- streams into a single stream, allowing graceful recovery from any
-- single-source loss. FAILOVER allows switching between different streams.
updateFailoverConfig_failoverMode :: Lens.Lens' UpdateFailoverConfig (Prelude.Maybe FailoverMode)
updateFailoverConfig_failoverMode = Lens.lens (\UpdateFailoverConfig' {failoverMode} -> failoverMode) (\s@UpdateFailoverConfig' {} a -> s {failoverMode = a} :: UpdateFailoverConfig)

instance Prelude.Hashable UpdateFailoverConfig where
  hashWithSalt _salt UpdateFailoverConfig' {..} =
    _salt `Prelude.hashWithSalt` recoveryWindow
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` sourcePriority
      `Prelude.hashWithSalt` failoverMode

instance Prelude.NFData UpdateFailoverConfig where
  rnf UpdateFailoverConfig' {..} =
    Prelude.rnf recoveryWindow
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf sourcePriority
      `Prelude.seq` Prelude.rnf failoverMode

instance Data.ToJSON UpdateFailoverConfig where
  toJSON UpdateFailoverConfig' {..} =
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
