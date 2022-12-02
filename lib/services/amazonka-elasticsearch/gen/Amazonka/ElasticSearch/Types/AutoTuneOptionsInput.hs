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
-- Module      : Amazonka.ElasticSearch.Types.AutoTuneOptionsInput
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ElasticSearch.Types.AutoTuneOptionsInput where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ElasticSearch.Types.AutoTuneDesiredState
import Amazonka.ElasticSearch.Types.AutoTuneMaintenanceSchedule
import qualified Amazonka.Prelude as Prelude

-- | Specifies the Auto-Tune options: the Auto-Tune desired state for the
-- domain and list of maintenance schedules.
--
-- /See:/ 'newAutoTuneOptionsInput' smart constructor.
data AutoTuneOptionsInput = AutoTuneOptionsInput'
  { -- | Specifies list of maitenance schedules. See the
    -- <https://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/auto-tune.html Developer Guide>
    -- for more information.
    maintenanceSchedules :: Prelude.Maybe [AutoTuneMaintenanceSchedule],
    -- | Specifies the Auto-Tune desired state. Valid values are ENABLED,
    -- DISABLED.
    desiredState :: Prelude.Maybe AutoTuneDesiredState
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
-- 'maintenanceSchedules', 'autoTuneOptionsInput_maintenanceSchedules' - Specifies list of maitenance schedules. See the
-- <https://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/auto-tune.html Developer Guide>
-- for more information.
--
-- 'desiredState', 'autoTuneOptionsInput_desiredState' - Specifies the Auto-Tune desired state. Valid values are ENABLED,
-- DISABLED.
newAutoTuneOptionsInput ::
  AutoTuneOptionsInput
newAutoTuneOptionsInput =
  AutoTuneOptionsInput'
    { maintenanceSchedules =
        Prelude.Nothing,
      desiredState = Prelude.Nothing
    }

-- | Specifies list of maitenance schedules. See the
-- <https://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/auto-tune.html Developer Guide>
-- for more information.
autoTuneOptionsInput_maintenanceSchedules :: Lens.Lens' AutoTuneOptionsInput (Prelude.Maybe [AutoTuneMaintenanceSchedule])
autoTuneOptionsInput_maintenanceSchedules = Lens.lens (\AutoTuneOptionsInput' {maintenanceSchedules} -> maintenanceSchedules) (\s@AutoTuneOptionsInput' {} a -> s {maintenanceSchedules = a} :: AutoTuneOptionsInput) Prelude.. Lens.mapping Lens.coerced

-- | Specifies the Auto-Tune desired state. Valid values are ENABLED,
-- DISABLED.
autoTuneOptionsInput_desiredState :: Lens.Lens' AutoTuneOptionsInput (Prelude.Maybe AutoTuneDesiredState)
autoTuneOptionsInput_desiredState = Lens.lens (\AutoTuneOptionsInput' {desiredState} -> desiredState) (\s@AutoTuneOptionsInput' {} a -> s {desiredState = a} :: AutoTuneOptionsInput)

instance Prelude.Hashable AutoTuneOptionsInput where
  hashWithSalt _salt AutoTuneOptionsInput' {..} =
    _salt `Prelude.hashWithSalt` maintenanceSchedules
      `Prelude.hashWithSalt` desiredState

instance Prelude.NFData AutoTuneOptionsInput where
  rnf AutoTuneOptionsInput' {..} =
    Prelude.rnf maintenanceSchedules
      `Prelude.seq` Prelude.rnf desiredState

instance Data.ToJSON AutoTuneOptionsInput where
  toJSON AutoTuneOptionsInput' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MaintenanceSchedules" Data..=)
              Prelude.<$> maintenanceSchedules,
            ("DesiredState" Data..=) Prelude.<$> desiredState
          ]
      )
