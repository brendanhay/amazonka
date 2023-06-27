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
-- Module      : Amazonka.ElasticSearch.Types.AutoTuneOptions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ElasticSearch.Types.AutoTuneOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ElasticSearch.Types.AutoTuneDesiredState
import Amazonka.ElasticSearch.Types.AutoTuneMaintenanceSchedule
import Amazonka.ElasticSearch.Types.RollbackOnDisable
import qualified Amazonka.Prelude as Prelude

-- | Specifies the Auto-Tune options: the Auto-Tune desired state for the
-- domain, rollback state when disabling Auto-Tune options and list of
-- maintenance schedules.
--
-- /See:/ 'newAutoTuneOptions' smart constructor.
data AutoTuneOptions = AutoTuneOptions'
  { -- | Specifies the Auto-Tune desired state. Valid values are ENABLED,
    -- DISABLED.
    desiredState :: Prelude.Maybe AutoTuneDesiredState,
    -- | Specifies list of maitenance schedules. See the
    -- <https://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/auto-tune.html Developer Guide>
    -- for more information.
    maintenanceSchedules :: Prelude.Maybe [AutoTuneMaintenanceSchedule],
    -- | Specifies the rollback state while disabling Auto-Tune for the domain.
    -- Valid values are NO_ROLLBACK, DEFAULT_ROLLBACK.
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
-- 'desiredState', 'autoTuneOptions_desiredState' - Specifies the Auto-Tune desired state. Valid values are ENABLED,
-- DISABLED.
--
-- 'maintenanceSchedules', 'autoTuneOptions_maintenanceSchedules' - Specifies list of maitenance schedules. See the
-- <https://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/auto-tune.html Developer Guide>
-- for more information.
--
-- 'rollbackOnDisable', 'autoTuneOptions_rollbackOnDisable' - Specifies the rollback state while disabling Auto-Tune for the domain.
-- Valid values are NO_ROLLBACK, DEFAULT_ROLLBACK.
newAutoTuneOptions ::
  AutoTuneOptions
newAutoTuneOptions =
  AutoTuneOptions'
    { desiredState = Prelude.Nothing,
      maintenanceSchedules = Prelude.Nothing,
      rollbackOnDisable = Prelude.Nothing
    }

-- | Specifies the Auto-Tune desired state. Valid values are ENABLED,
-- DISABLED.
autoTuneOptions_desiredState :: Lens.Lens' AutoTuneOptions (Prelude.Maybe AutoTuneDesiredState)
autoTuneOptions_desiredState = Lens.lens (\AutoTuneOptions' {desiredState} -> desiredState) (\s@AutoTuneOptions' {} a -> s {desiredState = a} :: AutoTuneOptions)

-- | Specifies list of maitenance schedules. See the
-- <https://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/auto-tune.html Developer Guide>
-- for more information.
autoTuneOptions_maintenanceSchedules :: Lens.Lens' AutoTuneOptions (Prelude.Maybe [AutoTuneMaintenanceSchedule])
autoTuneOptions_maintenanceSchedules = Lens.lens (\AutoTuneOptions' {maintenanceSchedules} -> maintenanceSchedules) (\s@AutoTuneOptions' {} a -> s {maintenanceSchedules = a} :: AutoTuneOptions) Prelude.. Lens.mapping Lens.coerced

-- | Specifies the rollback state while disabling Auto-Tune for the domain.
-- Valid values are NO_ROLLBACK, DEFAULT_ROLLBACK.
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
