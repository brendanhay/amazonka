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
-- Module      : Amazonka.MediaConnect.Types.UpdateMaintenance
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConnect.Types.UpdateMaintenance where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaConnect.Types.MaintenanceDay
import qualified Amazonka.Prelude as Prelude

-- | Update maintenance setting for a flow
--
-- /See:/ 'newUpdateMaintenance' smart constructor.
data UpdateMaintenance = UpdateMaintenance'
  { -- | A day of a week when the maintenance will happen. use
    -- Monday\/Tuesday\/Wednesday\/Thursday\/Friday\/Saturday\/Sunday.
    maintenanceDay :: Prelude.Maybe MaintenanceDay,
    -- | A scheduled date in ISO UTC format when the maintenance will happen. Use
    -- YYYY-MM-DD format. Example: 2021-01-30.
    maintenanceScheduledDate :: Prelude.Maybe Prelude.Text,
    -- | UTC time when the maintenance will happen. Use 24-hour HH:MM format.
    -- Minutes must be 00. Example: 13:00. The default value is 02:00.
    maintenanceStartHour :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateMaintenance' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maintenanceDay', 'updateMaintenance_maintenanceDay' - A day of a week when the maintenance will happen. use
-- Monday\/Tuesday\/Wednesday\/Thursday\/Friday\/Saturday\/Sunday.
--
-- 'maintenanceScheduledDate', 'updateMaintenance_maintenanceScheduledDate' - A scheduled date in ISO UTC format when the maintenance will happen. Use
-- YYYY-MM-DD format. Example: 2021-01-30.
--
-- 'maintenanceStartHour', 'updateMaintenance_maintenanceStartHour' - UTC time when the maintenance will happen. Use 24-hour HH:MM format.
-- Minutes must be 00. Example: 13:00. The default value is 02:00.
newUpdateMaintenance ::
  UpdateMaintenance
newUpdateMaintenance =
  UpdateMaintenance'
    { maintenanceDay =
        Prelude.Nothing,
      maintenanceScheduledDate = Prelude.Nothing,
      maintenanceStartHour = Prelude.Nothing
    }

-- | A day of a week when the maintenance will happen. use
-- Monday\/Tuesday\/Wednesday\/Thursday\/Friday\/Saturday\/Sunday.
updateMaintenance_maintenanceDay :: Lens.Lens' UpdateMaintenance (Prelude.Maybe MaintenanceDay)
updateMaintenance_maintenanceDay = Lens.lens (\UpdateMaintenance' {maintenanceDay} -> maintenanceDay) (\s@UpdateMaintenance' {} a -> s {maintenanceDay = a} :: UpdateMaintenance)

-- | A scheduled date in ISO UTC format when the maintenance will happen. Use
-- YYYY-MM-DD format. Example: 2021-01-30.
updateMaintenance_maintenanceScheduledDate :: Lens.Lens' UpdateMaintenance (Prelude.Maybe Prelude.Text)
updateMaintenance_maintenanceScheduledDate = Lens.lens (\UpdateMaintenance' {maintenanceScheduledDate} -> maintenanceScheduledDate) (\s@UpdateMaintenance' {} a -> s {maintenanceScheduledDate = a} :: UpdateMaintenance)

-- | UTC time when the maintenance will happen. Use 24-hour HH:MM format.
-- Minutes must be 00. Example: 13:00. The default value is 02:00.
updateMaintenance_maintenanceStartHour :: Lens.Lens' UpdateMaintenance (Prelude.Maybe Prelude.Text)
updateMaintenance_maintenanceStartHour = Lens.lens (\UpdateMaintenance' {maintenanceStartHour} -> maintenanceStartHour) (\s@UpdateMaintenance' {} a -> s {maintenanceStartHour = a} :: UpdateMaintenance)

instance Prelude.Hashable UpdateMaintenance where
  hashWithSalt _salt UpdateMaintenance' {..} =
    _salt `Prelude.hashWithSalt` maintenanceDay
      `Prelude.hashWithSalt` maintenanceScheduledDate
      `Prelude.hashWithSalt` maintenanceStartHour

instance Prelude.NFData UpdateMaintenance where
  rnf UpdateMaintenance' {..} =
    Prelude.rnf maintenanceDay
      `Prelude.seq` Prelude.rnf maintenanceScheduledDate
      `Prelude.seq` Prelude.rnf maintenanceStartHour

instance Data.ToJSON UpdateMaintenance where
  toJSON UpdateMaintenance' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("maintenanceDay" Data..=)
              Prelude.<$> maintenanceDay,
            ("maintenanceScheduledDate" Data..=)
              Prelude.<$> maintenanceScheduledDate,
            ("maintenanceStartHour" Data..=)
              Prelude.<$> maintenanceStartHour
          ]
      )
