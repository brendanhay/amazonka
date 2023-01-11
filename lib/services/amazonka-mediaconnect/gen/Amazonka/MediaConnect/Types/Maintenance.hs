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
-- Module      : Amazonka.MediaConnect.Types.Maintenance
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConnect.Types.Maintenance where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaConnect.Types.MaintenanceDay
import qualified Amazonka.Prelude as Prelude

-- | The maintenance setting of a flow
--
-- /See:/ 'newMaintenance' smart constructor.
data Maintenance = Maintenance'
  { -- | A day of a week when the maintenance will happen. Use
    -- Monday\/Tuesday\/Wednesday\/Thursday\/Friday\/Saturday\/Sunday.
    maintenanceDay :: Prelude.Maybe MaintenanceDay,
    -- | The Maintenance has to be performed before this deadline in ISO UTC
    -- format. Example: 2021-01-30T08:30:00Z.
    maintenanceDeadline :: Prelude.Maybe Prelude.Text,
    -- | A scheduled date in ISO UTC format when the maintenance will happen. Use
    -- YYYY-MM-DD format. Example: 2021-01-30.
    maintenanceScheduledDate :: Prelude.Maybe Prelude.Text,
    -- | UTC time when the maintenance will happen. Use 24-hour HH:MM format.
    -- Minutes must be 00. Example: 13:00. The default value is 02:00.
    maintenanceStartHour :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Maintenance' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maintenanceDay', 'maintenance_maintenanceDay' - A day of a week when the maintenance will happen. Use
-- Monday\/Tuesday\/Wednesday\/Thursday\/Friday\/Saturday\/Sunday.
--
-- 'maintenanceDeadline', 'maintenance_maintenanceDeadline' - The Maintenance has to be performed before this deadline in ISO UTC
-- format. Example: 2021-01-30T08:30:00Z.
--
-- 'maintenanceScheduledDate', 'maintenance_maintenanceScheduledDate' - A scheduled date in ISO UTC format when the maintenance will happen. Use
-- YYYY-MM-DD format. Example: 2021-01-30.
--
-- 'maintenanceStartHour', 'maintenance_maintenanceStartHour' - UTC time when the maintenance will happen. Use 24-hour HH:MM format.
-- Minutes must be 00. Example: 13:00. The default value is 02:00.
newMaintenance ::
  Maintenance
newMaintenance =
  Maintenance'
    { maintenanceDay = Prelude.Nothing,
      maintenanceDeadline = Prelude.Nothing,
      maintenanceScheduledDate = Prelude.Nothing,
      maintenanceStartHour = Prelude.Nothing
    }

-- | A day of a week when the maintenance will happen. Use
-- Monday\/Tuesday\/Wednesday\/Thursday\/Friday\/Saturday\/Sunday.
maintenance_maintenanceDay :: Lens.Lens' Maintenance (Prelude.Maybe MaintenanceDay)
maintenance_maintenanceDay = Lens.lens (\Maintenance' {maintenanceDay} -> maintenanceDay) (\s@Maintenance' {} a -> s {maintenanceDay = a} :: Maintenance)

-- | The Maintenance has to be performed before this deadline in ISO UTC
-- format. Example: 2021-01-30T08:30:00Z.
maintenance_maintenanceDeadline :: Lens.Lens' Maintenance (Prelude.Maybe Prelude.Text)
maintenance_maintenanceDeadline = Lens.lens (\Maintenance' {maintenanceDeadline} -> maintenanceDeadline) (\s@Maintenance' {} a -> s {maintenanceDeadline = a} :: Maintenance)

-- | A scheduled date in ISO UTC format when the maintenance will happen. Use
-- YYYY-MM-DD format. Example: 2021-01-30.
maintenance_maintenanceScheduledDate :: Lens.Lens' Maintenance (Prelude.Maybe Prelude.Text)
maintenance_maintenanceScheduledDate = Lens.lens (\Maintenance' {maintenanceScheduledDate} -> maintenanceScheduledDate) (\s@Maintenance' {} a -> s {maintenanceScheduledDate = a} :: Maintenance)

-- | UTC time when the maintenance will happen. Use 24-hour HH:MM format.
-- Minutes must be 00. Example: 13:00. The default value is 02:00.
maintenance_maintenanceStartHour :: Lens.Lens' Maintenance (Prelude.Maybe Prelude.Text)
maintenance_maintenanceStartHour = Lens.lens (\Maintenance' {maintenanceStartHour} -> maintenanceStartHour) (\s@Maintenance' {} a -> s {maintenanceStartHour = a} :: Maintenance)

instance Data.FromJSON Maintenance where
  parseJSON =
    Data.withObject
      "Maintenance"
      ( \x ->
          Maintenance'
            Prelude.<$> (x Data..:? "maintenanceDay")
            Prelude.<*> (x Data..:? "maintenanceDeadline")
            Prelude.<*> (x Data..:? "maintenanceScheduledDate")
            Prelude.<*> (x Data..:? "maintenanceStartHour")
      )

instance Prelude.Hashable Maintenance where
  hashWithSalt _salt Maintenance' {..} =
    _salt `Prelude.hashWithSalt` maintenanceDay
      `Prelude.hashWithSalt` maintenanceDeadline
      `Prelude.hashWithSalt` maintenanceScheduledDate
      `Prelude.hashWithSalt` maintenanceStartHour

instance Prelude.NFData Maintenance where
  rnf Maintenance' {..} =
    Prelude.rnf maintenanceDay
      `Prelude.seq` Prelude.rnf maintenanceDeadline
      `Prelude.seq` Prelude.rnf maintenanceScheduledDate
      `Prelude.seq` Prelude.rnf maintenanceStartHour
