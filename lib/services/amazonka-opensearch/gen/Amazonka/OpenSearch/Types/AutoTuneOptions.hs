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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.OpenSearch.Types.AutoTuneOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.OpenSearch.Types.AutoTuneDesiredState
import Amazonka.OpenSearch.Types.AutoTuneMaintenanceSchedule
import Amazonka.OpenSearch.Types.RollbackOnDisable
import qualified Amazonka.Prelude as Prelude

-- | The Auto-Tune options: the Auto-Tune desired state for the domain,
-- rollback state when disabling Auto-Tune options and list of maintenance
-- schedules.
--
-- /See:/ 'newAutoTuneOptions' smart constructor.
data AutoTuneOptions = AutoTuneOptions'
  { -- | The Auto-Tune desired state. Valid values are ENABLED and DISABLED.
    desiredState :: Prelude.Maybe AutoTuneDesiredState,
    -- | The rollback state while disabling Auto-Tune for the domain. Valid
    -- values are NO_ROLLBACK and DEFAULT_ROLLBACK.
    rollbackOnDisable :: Prelude.Maybe RollbackOnDisable,
    -- | A list of maintenance schedules. See
    -- <https://docs.aws.amazon.com/opensearch-service/latest/developerguide/auto-tune.html Auto-Tune for Amazon OpenSearch Service>
    -- for more information.
    maintenanceSchedules :: Prelude.Maybe [AutoTuneMaintenanceSchedule]
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
-- 'desiredState', 'autoTuneOptions_desiredState' - The Auto-Tune desired state. Valid values are ENABLED and DISABLED.
--
-- 'rollbackOnDisable', 'autoTuneOptions_rollbackOnDisable' - The rollback state while disabling Auto-Tune for the domain. Valid
-- values are NO_ROLLBACK and DEFAULT_ROLLBACK.
--
-- 'maintenanceSchedules', 'autoTuneOptions_maintenanceSchedules' - A list of maintenance schedules. See
-- <https://docs.aws.amazon.com/opensearch-service/latest/developerguide/auto-tune.html Auto-Tune for Amazon OpenSearch Service>
-- for more information.
newAutoTuneOptions ::
  AutoTuneOptions
newAutoTuneOptions =
  AutoTuneOptions'
    { desiredState = Prelude.Nothing,
      rollbackOnDisable = Prelude.Nothing,
      maintenanceSchedules = Prelude.Nothing
    }

-- | The Auto-Tune desired state. Valid values are ENABLED and DISABLED.
autoTuneOptions_desiredState :: Lens.Lens' AutoTuneOptions (Prelude.Maybe AutoTuneDesiredState)
autoTuneOptions_desiredState = Lens.lens (\AutoTuneOptions' {desiredState} -> desiredState) (\s@AutoTuneOptions' {} a -> s {desiredState = a} :: AutoTuneOptions)

-- | The rollback state while disabling Auto-Tune for the domain. Valid
-- values are NO_ROLLBACK and DEFAULT_ROLLBACK.
autoTuneOptions_rollbackOnDisable :: Lens.Lens' AutoTuneOptions (Prelude.Maybe RollbackOnDisable)
autoTuneOptions_rollbackOnDisable = Lens.lens (\AutoTuneOptions' {rollbackOnDisable} -> rollbackOnDisable) (\s@AutoTuneOptions' {} a -> s {rollbackOnDisable = a} :: AutoTuneOptions)

-- | A list of maintenance schedules. See
-- <https://docs.aws.amazon.com/opensearch-service/latest/developerguide/auto-tune.html Auto-Tune for Amazon OpenSearch Service>
-- for more information.
autoTuneOptions_maintenanceSchedules :: Lens.Lens' AutoTuneOptions (Prelude.Maybe [AutoTuneMaintenanceSchedule])
autoTuneOptions_maintenanceSchedules = Lens.lens (\AutoTuneOptions' {maintenanceSchedules} -> maintenanceSchedules) (\s@AutoTuneOptions' {} a -> s {maintenanceSchedules = a} :: AutoTuneOptions) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON AutoTuneOptions where
  parseJSON =
    Core.withObject
      "AutoTuneOptions"
      ( \x ->
          AutoTuneOptions'
            Prelude.<$> (x Core..:? "DesiredState")
            Prelude.<*> (x Core..:? "RollbackOnDisable")
            Prelude.<*> ( x Core..:? "MaintenanceSchedules"
                            Core..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable AutoTuneOptions where
  hashWithSalt salt' AutoTuneOptions' {..} =
    salt' `Prelude.hashWithSalt` maintenanceSchedules
      `Prelude.hashWithSalt` rollbackOnDisable
      `Prelude.hashWithSalt` desiredState

instance Prelude.NFData AutoTuneOptions where
  rnf AutoTuneOptions' {..} =
    Prelude.rnf desiredState
      `Prelude.seq` Prelude.rnf maintenanceSchedules
      `Prelude.seq` Prelude.rnf rollbackOnDisable

instance Core.ToJSON AutoTuneOptions where
  toJSON AutoTuneOptions' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("DesiredState" Core..=) Prelude.<$> desiredState,
            ("RollbackOnDisable" Core..=)
              Prelude.<$> rollbackOnDisable,
            ("MaintenanceSchedules" Core..=)
              Prelude.<$> maintenanceSchedules
          ]
      )
