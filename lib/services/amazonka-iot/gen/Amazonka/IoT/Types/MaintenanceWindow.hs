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
-- Module      : Amazonka.IoT.Types.MaintenanceWindow
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT.Types.MaintenanceWindow where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An optional configuration within the @SchedulingConfig@ to setup a
-- recurring maintenance window with a predetermined start time and
-- duration for the rollout of a job document to all devices in a target
-- group for a job.
--
-- /See:/ 'newMaintenanceWindow' smart constructor.
data MaintenanceWindow = MaintenanceWindow'
  { -- | Displays the start time of the next maintenance window.
    startTime :: Prelude.Text,
    -- | Displays the duration of the next maintenance window.
    durationInMinutes :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MaintenanceWindow' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'startTime', 'maintenanceWindow_startTime' - Displays the start time of the next maintenance window.
--
-- 'durationInMinutes', 'maintenanceWindow_durationInMinutes' - Displays the duration of the next maintenance window.
newMaintenanceWindow ::
  -- | 'startTime'
  Prelude.Text ->
  -- | 'durationInMinutes'
  Prelude.Natural ->
  MaintenanceWindow
newMaintenanceWindow pStartTime_ pDurationInMinutes_ =
  MaintenanceWindow'
    { startTime = pStartTime_,
      durationInMinutes = pDurationInMinutes_
    }

-- | Displays the start time of the next maintenance window.
maintenanceWindow_startTime :: Lens.Lens' MaintenanceWindow Prelude.Text
maintenanceWindow_startTime = Lens.lens (\MaintenanceWindow' {startTime} -> startTime) (\s@MaintenanceWindow' {} a -> s {startTime = a} :: MaintenanceWindow)

-- | Displays the duration of the next maintenance window.
maintenanceWindow_durationInMinutes :: Lens.Lens' MaintenanceWindow Prelude.Natural
maintenanceWindow_durationInMinutes = Lens.lens (\MaintenanceWindow' {durationInMinutes} -> durationInMinutes) (\s@MaintenanceWindow' {} a -> s {durationInMinutes = a} :: MaintenanceWindow)

instance Data.FromJSON MaintenanceWindow where
  parseJSON =
    Data.withObject
      "MaintenanceWindow"
      ( \x ->
          MaintenanceWindow'
            Prelude.<$> (x Data..: "startTime")
            Prelude.<*> (x Data..: "durationInMinutes")
      )

instance Prelude.Hashable MaintenanceWindow where
  hashWithSalt _salt MaintenanceWindow' {..} =
    _salt
      `Prelude.hashWithSalt` startTime
      `Prelude.hashWithSalt` durationInMinutes

instance Prelude.NFData MaintenanceWindow where
  rnf MaintenanceWindow' {..} =
    Prelude.rnf startTime
      `Prelude.seq` Prelude.rnf durationInMinutes

instance Data.ToJSON MaintenanceWindow where
  toJSON MaintenanceWindow' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("startTime" Data..= startTime),
            Prelude.Just
              ("durationInMinutes" Data..= durationInMinutes)
          ]
      )
