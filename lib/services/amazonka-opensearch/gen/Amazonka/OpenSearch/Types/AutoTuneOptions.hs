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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.OpenSearch.Types.AutoTuneOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.OpenSearch.Types.AutoTuneDesiredState
import Amazonka.OpenSearch.Types.AutoTuneMaintenanceSchedule
import Amazonka.OpenSearch.Types.RollbackOnDisable
import qualified Amazonka.Prelude as Prelude

-- | Auto-Tune settings when updating a domain. For more information, see
-- <https://docs.aws.amazon.com/opensearch-service/latest/developerguide/auto-tune.html Auto-Tune for Amazon OpenSearch Service>.
--
-- /See:/ 'newAutoTuneOptions' smart constructor.
data AutoTuneOptions = AutoTuneOptions'
  { -- | Whether Auto-Tune is enabled or disabled.
    desiredState :: Prelude.Maybe AutoTuneDesiredState,
    -- | A list of maintenance schedules during which Auto-Tune can deploy
    -- changes.
    maintenanceSchedules :: Prelude.Maybe [AutoTuneMaintenanceSchedule],
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
-- 'desiredState', 'autoTuneOptions_desiredState' - Whether Auto-Tune is enabled or disabled.
--
-- 'maintenanceSchedules', 'autoTuneOptions_maintenanceSchedules' - A list of maintenance schedules during which Auto-Tune can deploy
-- changes.
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
    { desiredState = Prelude.Nothing,
      maintenanceSchedules = Prelude.Nothing,
      rollbackOnDisable = Prelude.Nothing
    }

-- | Whether Auto-Tune is enabled or disabled.
autoTuneOptions_desiredState :: Lens.Lens' AutoTuneOptions (Prelude.Maybe AutoTuneDesiredState)
autoTuneOptions_desiredState = Lens.lens (\AutoTuneOptions' {desiredState} -> desiredState) (\s@AutoTuneOptions' {} a -> s {desiredState = a} :: AutoTuneOptions)

-- | A list of maintenance schedules during which Auto-Tune can deploy
-- changes.
autoTuneOptions_maintenanceSchedules :: Lens.Lens' AutoTuneOptions (Prelude.Maybe [AutoTuneMaintenanceSchedule])
autoTuneOptions_maintenanceSchedules = Lens.lens (\AutoTuneOptions' {maintenanceSchedules} -> maintenanceSchedules) (\s@AutoTuneOptions' {} a -> s {maintenanceSchedules = a} :: AutoTuneOptions) Prelude.. Lens.mapping Lens.coerced

-- | When disabling Auto-Tune, specify @NO_ROLLBACK@ to retain all prior
-- Auto-Tune settings or @DEFAULT_ROLLBACK@ to revert to the OpenSearch
-- Service defaults. If you specify @DEFAULT_ROLLBACK@, you must include a
-- @MaintenanceSchedule@ in the request. Otherwise, OpenSearch Service is
-- unable to perform the rollback.
autoTuneOptions_rollbackOnDisable :: Lens.Lens' AutoTuneOptions (Prelude.Maybe RollbackOnDisable)
autoTuneOptions_rollbackOnDisable = Lens.lens (\AutoTuneOptions' {rollbackOnDisable} -> rollbackOnDisable) (\s@AutoTuneOptions' {} a -> s {rollbackOnDisable = a} :: AutoTuneOptions)

instance Data.FromJSON AutoTuneOptions where
  parseJSON =
    Data.withObject
      "AutoTuneOptions"
      ( \x ->
          AutoTuneOptions'
            Prelude.<$> (x Data..:? "DesiredState")
            Prelude.<*> ( x
                            Data..:? "MaintenanceSchedules"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "RollbackOnDisable")
      )

instance Prelude.Hashable AutoTuneOptions where
  hashWithSalt _salt AutoTuneOptions' {..} =
    _salt
      `Prelude.hashWithSalt` desiredState
      `Prelude.hashWithSalt` maintenanceSchedules
      `Prelude.hashWithSalt` rollbackOnDisable

instance Prelude.NFData AutoTuneOptions where
  rnf AutoTuneOptions' {..} =
    Prelude.rnf desiredState
      `Prelude.seq` Prelude.rnf maintenanceSchedules
      `Prelude.seq` Prelude.rnf rollbackOnDisable

instance Data.ToJSON AutoTuneOptions where
  toJSON AutoTuneOptions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DesiredState" Data..=) Prelude.<$> desiredState,
            ("MaintenanceSchedules" Data..=)
              Prelude.<$> maintenanceSchedules,
            ("RollbackOnDisable" Data..=)
              Prelude.<$> rollbackOnDisable
          ]
      )
