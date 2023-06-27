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
-- Module      : Amazonka.OpenSearch.Types.AutoTuneOptionsInput
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.OpenSearch.Types.AutoTuneOptionsInput where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.OpenSearch.Types.AutoTuneDesiredState
import Amazonka.OpenSearch.Types.AutoTuneMaintenanceSchedule
import qualified Amazonka.Prelude as Prelude

-- | Options for configuring Auto-Tune. For more information, see
-- <https://docs.aws.amazon.com/opensearch-service/latest/developerguide/auto-tune.html Auto-Tune for Amazon OpenSearch Service>
--
-- /See:/ 'newAutoTuneOptionsInput' smart constructor.
data AutoTuneOptionsInput = AutoTuneOptionsInput'
  { -- | Whether Auto-Tune is enabled or disabled.
    desiredState :: Prelude.Maybe AutoTuneDesiredState,
    -- | A list of maintenance schedules during which Auto-Tune can deploy
    -- changes. Maintenance windows are deprecated and have been replaced with
    -- <https://docs.aws.amazon.com/opensearch-service/latest/developerguide/off-peak.html off-peak windows>.
    maintenanceSchedules :: Prelude.Maybe [AutoTuneMaintenanceSchedule],
    -- | Whether to schedule Auto-Tune optimizations that require blue\/green
    -- deployments during the domain\'s configured daily off-peak window.
    useOffPeakWindow :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AutoTuneOptionsInput' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'desiredState', 'autoTuneOptionsInput_desiredState' - Whether Auto-Tune is enabled or disabled.
--
-- 'maintenanceSchedules', 'autoTuneOptionsInput_maintenanceSchedules' - A list of maintenance schedules during which Auto-Tune can deploy
-- changes. Maintenance windows are deprecated and have been replaced with
-- <https://docs.aws.amazon.com/opensearch-service/latest/developerguide/off-peak.html off-peak windows>.
--
-- 'useOffPeakWindow', 'autoTuneOptionsInput_useOffPeakWindow' - Whether to schedule Auto-Tune optimizations that require blue\/green
-- deployments during the domain\'s configured daily off-peak window.
newAutoTuneOptionsInput ::
  AutoTuneOptionsInput
newAutoTuneOptionsInput =
  AutoTuneOptionsInput'
    { desiredState =
        Prelude.Nothing,
      maintenanceSchedules = Prelude.Nothing,
      useOffPeakWindow = Prelude.Nothing
    }

-- | Whether Auto-Tune is enabled or disabled.
autoTuneOptionsInput_desiredState :: Lens.Lens' AutoTuneOptionsInput (Prelude.Maybe AutoTuneDesiredState)
autoTuneOptionsInput_desiredState = Lens.lens (\AutoTuneOptionsInput' {desiredState} -> desiredState) (\s@AutoTuneOptionsInput' {} a -> s {desiredState = a} :: AutoTuneOptionsInput)

-- | A list of maintenance schedules during which Auto-Tune can deploy
-- changes. Maintenance windows are deprecated and have been replaced with
-- <https://docs.aws.amazon.com/opensearch-service/latest/developerguide/off-peak.html off-peak windows>.
autoTuneOptionsInput_maintenanceSchedules :: Lens.Lens' AutoTuneOptionsInput (Prelude.Maybe [AutoTuneMaintenanceSchedule])
autoTuneOptionsInput_maintenanceSchedules = Lens.lens (\AutoTuneOptionsInput' {maintenanceSchedules} -> maintenanceSchedules) (\s@AutoTuneOptionsInput' {} a -> s {maintenanceSchedules = a} :: AutoTuneOptionsInput) Prelude.. Lens.mapping Lens.coerced

-- | Whether to schedule Auto-Tune optimizations that require blue\/green
-- deployments during the domain\'s configured daily off-peak window.
autoTuneOptionsInput_useOffPeakWindow :: Lens.Lens' AutoTuneOptionsInput (Prelude.Maybe Prelude.Bool)
autoTuneOptionsInput_useOffPeakWindow = Lens.lens (\AutoTuneOptionsInput' {useOffPeakWindow} -> useOffPeakWindow) (\s@AutoTuneOptionsInput' {} a -> s {useOffPeakWindow = a} :: AutoTuneOptionsInput)

instance Prelude.Hashable AutoTuneOptionsInput where
  hashWithSalt _salt AutoTuneOptionsInput' {..} =
    _salt
      `Prelude.hashWithSalt` desiredState
      `Prelude.hashWithSalt` maintenanceSchedules
      `Prelude.hashWithSalt` useOffPeakWindow

instance Prelude.NFData AutoTuneOptionsInput where
  rnf AutoTuneOptionsInput' {..} =
    Prelude.rnf desiredState
      `Prelude.seq` Prelude.rnf maintenanceSchedules
      `Prelude.seq` Prelude.rnf useOffPeakWindow

instance Data.ToJSON AutoTuneOptionsInput where
  toJSON AutoTuneOptionsInput' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DesiredState" Data..=) Prelude.<$> desiredState,
            ("MaintenanceSchedules" Data..=)
              Prelude.<$> maintenanceSchedules,
            ("UseOffPeakWindow" Data..=)
              Prelude.<$> useOffPeakWindow
          ]
      )
