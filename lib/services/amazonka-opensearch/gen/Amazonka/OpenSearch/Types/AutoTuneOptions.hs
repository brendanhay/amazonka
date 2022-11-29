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
-- Module      : Amazonka.OpenSearch.Types.AutoTuneOptions
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.OpenSearch.Types.AutoTuneOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.OpenSearch.Types.AutoTuneDesiredState
import Amazonka.OpenSearch.Types.AutoTuneMaintenanceSchedule
import Amazonka.OpenSearch.Types.RollbackOnDisable
import qualified Amazonka.Prelude as Prelude

-- | Auto-Tune settings when updating a domain. For more information, see
-- <https://docs.aws.amazon.com/opensearch-service/latest/developerguide/auto-tune.html Auto-Tune for Amazon OpenSearch Service>.
--
-- /See:/ 'newAutoTuneOptions' smart constructor.
data AutoTuneOptions = AutoTuneOptions'
  { -- | A list of maintenance schedules during which Auto-Tune can deploy
    -- changes.
    maintenanceSchedules :: Prelude.Maybe [AutoTuneMaintenanceSchedule],
    -- | Whether Auto-Tune is enabled or disabled.
    desiredState :: Prelude.Maybe AutoTuneDesiredState,
    -- | When disabling Auto-Tune, specify @NO_ROLLBACK@ to retain all prior
    -- Auto-Tune settings or @DEFAULT_ROLLBACK@ to revert to the OpenSearch
    -- Service defaults. If you specify @DEFAULT_ROLLBACK@, you must include a
    -- @MaintenanceSchedule@ in the request. Otherwise, OpenSearch Service is
    -- unable to perform the rollback.
    rollbackOnDisable :: Prelude.Maybe RollbackOnDisable
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AutoTuneOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maintenanceSchedules', 'autoTuneOptions_maintenanceSchedules' - A list of maintenance schedules during which Auto-Tune can deploy
-- changes.
--
-- 'desiredState', 'autoTuneOptions_desiredState' - Whether Auto-Tune is enabled or disabled.
--
-- 'rollbackOnDisable', 'autoTuneOptions_rollbackOnDisable' - When disabling Auto-Tune, specify @NO_ROLLBACK@ to retain all prior
-- Auto-Tune settings or @DEFAULT_ROLLBACK@ to revert to the OpenSearch
-- Service defaults. If you specify @DEFAULT_ROLLBACK@, you must include a
-- @MaintenanceSchedule@ in the request. Otherwise, OpenSearch Service is
-- unable to perform the rollback.
newAutoTuneOptions ::
  AutoTuneOptions
newAutoTuneOptions =
  AutoTuneOptions'
    { maintenanceSchedules =
        Prelude.Nothing,
      desiredState = Prelude.Nothing,
      rollbackOnDisable = Prelude.Nothing
    }

-- | A list of maintenance schedules during which Auto-Tune can deploy
-- changes.
autoTuneOptions_maintenanceSchedules :: Lens.Lens' AutoTuneOptions (Prelude.Maybe [AutoTuneMaintenanceSchedule])
autoTuneOptions_maintenanceSchedules = Lens.lens (\AutoTuneOptions' {maintenanceSchedules} -> maintenanceSchedules) (\s@AutoTuneOptions' {} a -> s {maintenanceSchedules = a} :: AutoTuneOptions) Prelude.. Lens.mapping Lens.coerced

-- | Whether Auto-Tune is enabled or disabled.
autoTuneOptions_desiredState :: Lens.Lens' AutoTuneOptions (Prelude.Maybe AutoTuneDesiredState)
autoTuneOptions_desiredState = Lens.lens (\AutoTuneOptions' {desiredState} -> desiredState) (\s@AutoTuneOptions' {} a -> s {desiredState = a} :: AutoTuneOptions)

-- | When disabling Auto-Tune, specify @NO_ROLLBACK@ to retain all prior
-- Auto-Tune settings or @DEFAULT_ROLLBACK@ to revert to the OpenSearch
-- Service defaults. If you specify @DEFAULT_ROLLBACK@, you must include a
-- @MaintenanceSchedule@ in the request. Otherwise, OpenSearch Service is
-- unable to perform the rollback.
autoTuneOptions_rollbackOnDisable :: Lens.Lens' AutoTuneOptions (Prelude.Maybe RollbackOnDisable)
autoTuneOptions_rollbackOnDisable = Lens.lens (\AutoTuneOptions' {rollbackOnDisable} -> rollbackOnDisable) (\s@AutoTuneOptions' {} a -> s {rollbackOnDisable = a} :: AutoTuneOptions)

instance Core.FromJSON AutoTuneOptions where
  parseJSON =
    Core.withObject
      "AutoTuneOptions"
      ( \x ->
          AutoTuneOptions'
            Prelude.<$> ( x Core..:? "MaintenanceSchedules"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "DesiredState")
            Prelude.<*> (x Core..:? "RollbackOnDisable")
      )

instance Prelude.Hashable AutoTuneOptions where
  hashWithSalt _salt AutoTuneOptions' {..} =
    _salt `Prelude.hashWithSalt` maintenanceSchedules
      `Prelude.hashWithSalt` desiredState
      `Prelude.hashWithSalt` rollbackOnDisable

instance Prelude.NFData AutoTuneOptions where
  rnf AutoTuneOptions' {..} =
    Prelude.rnf maintenanceSchedules
      `Prelude.seq` Prelude.rnf desiredState
      `Prelude.seq` Prelude.rnf rollbackOnDisable

instance Core.ToJSON AutoTuneOptions where
  toJSON AutoTuneOptions' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("MaintenanceSchedules" Core..=)
              Prelude.<$> maintenanceSchedules,
            ("DesiredState" Core..=) Prelude.<$> desiredState,
            ("RollbackOnDisable" Core..=)
              Prelude.<$> rollbackOnDisable
          ]
      )
