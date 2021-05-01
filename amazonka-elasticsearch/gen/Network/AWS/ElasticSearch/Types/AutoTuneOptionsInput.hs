{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.ElasticSearch.Types.AutoTuneOptionsInput
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticSearch.Types.AutoTuneOptionsInput where

import Network.AWS.ElasticSearch.Types.AutoTuneDesiredState
import Network.AWS.ElasticSearch.Types.AutoTuneMaintenanceSchedule
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Specifies the Auto-Tune options: the Auto-Tune desired state for the
-- domain and list of maintenance schedules.
--
-- /See:/ 'newAutoTuneOptionsInput' smart constructor.
data AutoTuneOptionsInput = AutoTuneOptionsInput'
  { -- | Specifies the Auto-Tune desired state. Valid values are ENABLED,
    -- DISABLED.
    desiredState :: Prelude.Maybe AutoTuneDesiredState,
    -- | Specifies list of maitenance schedules. See the
    -- <https://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/auto-tune.html Developer Guide>
    -- for more information.
    maintenanceSchedules :: Prelude.Maybe [AutoTuneMaintenanceSchedule]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'AutoTuneOptionsInput' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'desiredState', 'autoTuneOptionsInput_desiredState' - Specifies the Auto-Tune desired state. Valid values are ENABLED,
-- DISABLED.
--
-- 'maintenanceSchedules', 'autoTuneOptionsInput_maintenanceSchedules' - Specifies list of maitenance schedules. See the
-- <https://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/auto-tune.html Developer Guide>
-- for more information.
newAutoTuneOptionsInput ::
  AutoTuneOptionsInput
newAutoTuneOptionsInput =
  AutoTuneOptionsInput'
    { desiredState =
        Prelude.Nothing,
      maintenanceSchedules = Prelude.Nothing
    }

-- | Specifies the Auto-Tune desired state. Valid values are ENABLED,
-- DISABLED.
autoTuneOptionsInput_desiredState :: Lens.Lens' AutoTuneOptionsInput (Prelude.Maybe AutoTuneDesiredState)
autoTuneOptionsInput_desiredState = Lens.lens (\AutoTuneOptionsInput' {desiredState} -> desiredState) (\s@AutoTuneOptionsInput' {} a -> s {desiredState = a} :: AutoTuneOptionsInput)

-- | Specifies list of maitenance schedules. See the
-- <https://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/auto-tune.html Developer Guide>
-- for more information.
autoTuneOptionsInput_maintenanceSchedules :: Lens.Lens' AutoTuneOptionsInput (Prelude.Maybe [AutoTuneMaintenanceSchedule])
autoTuneOptionsInput_maintenanceSchedules = Lens.lens (\AutoTuneOptionsInput' {maintenanceSchedules} -> maintenanceSchedules) (\s@AutoTuneOptionsInput' {} a -> s {maintenanceSchedules = a} :: AutoTuneOptionsInput) Prelude.. Lens.mapping Prelude._Coerce

instance Prelude.Hashable AutoTuneOptionsInput

instance Prelude.NFData AutoTuneOptionsInput

instance Prelude.ToJSON AutoTuneOptionsInput where
  toJSON AutoTuneOptionsInput' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("DesiredState" Prelude..=)
              Prelude.<$> desiredState,
            ("MaintenanceSchedules" Prelude..=)
              Prelude.<$> maintenanceSchedules
          ]
      )
