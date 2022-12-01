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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConnect.Types.Maintenance where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.MediaConnect.Types.MaintenanceDay
import qualified Amazonka.Prelude as Prelude

-- | The maintenance setting of a flow
--
-- /See:/ 'newMaintenance' smart constructor.
data Maintenance = Maintenance'
  { -- | The Maintenance has to be performed before this deadline in ISO UTC
    -- format. Example: 2021-01-30T08:30:00Z.
    maintenanceDeadline :: Prelude.Maybe Prelude.Text,
    -- | A scheduled date in ISO UTC format when the maintenance will happen. Use
    -- YYYY-MM-DD format. Example: 2021-01-30.
    maintenanceScheduledDate :: Prelude.Maybe Prelude.Text,
    -- | UTC time when the maintenance will happen. Use 24-hour HH:MM format.
    -- Minutes must be 00. Example: 13:00. The default value is 02:00.
    maintenanceStartHour :: Prelude.Maybe Prelude.Text,
    -- | A day of a week when the maintenance will happen. Use
    -- Monday\/Tuesday\/Wednesday\/Thursday\/Friday\/Saturday\/Sunday.
    maintenanceDay :: Prelude.Maybe MaintenanceDay
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
-- 'maintenanceDeadline', 'maintenance_maintenanceDeadline' - The Maintenance has to be performed before this deadline in ISO UTC
-- format. Example: 2021-01-30T08:30:00Z.
--
-- 'maintenanceScheduledDate', 'maintenance_maintenanceScheduledDate' - A scheduled date in ISO UTC format when the maintenance will happen. Use
-- YYYY-MM-DD format. Example: 2021-01-30.
--
-- 'maintenanceStartHour', 'maintenance_maintenanceStartHour' - UTC time when the maintenance will happen. Use 24-hour HH:MM format.
-- Minutes must be 00. Example: 13:00. The default value is 02:00.
--
-- 'maintenanceDay', 'maintenance_maintenanceDay' - A day of a week when the maintenance will happen. Use
-- Monday\/Tuesday\/Wednesday\/Thursday\/Friday\/Saturday\/Sunday.
newMaintenance ::
  Maintenance
newMaintenance =
  Maintenance'
    { maintenanceDeadline = Prelude.Nothing,
      maintenanceScheduledDate = Prelude.Nothing,
      maintenanceStartHour = Prelude.Nothing,
      maintenanceDay = Prelude.Nothing
    }

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

-- | A day of a week when the maintenance will happen. Use
-- Monday\/Tuesday\/Wednesday\/Thursday\/Friday\/Saturday\/Sunday.
maintenance_maintenanceDay :: Lens.Lens' Maintenance (Prelude.Maybe MaintenanceDay)
maintenance_maintenanceDay = Lens.lens (\Maintenance' {maintenanceDay} -> maintenanceDay) (\s@Maintenance' {} a -> s {maintenanceDay = a} :: Maintenance)

instance Core.FromJSON Maintenance where
  parseJSON =
    Core.withObject
      "Maintenance"
      ( \x ->
          Maintenance'
            Prelude.<$> (x Core..:? "maintenanceDeadline")
            Prelude.<*> (x Core..:? "maintenanceScheduledDate")
            Prelude.<*> (x Core..:? "maintenanceStartHour")
            Prelude.<*> (x Core..:? "maintenanceDay")
      )

instance Prelude.Hashable Maintenance where
  hashWithSalt _salt Maintenance' {..} =
    _salt `Prelude.hashWithSalt` maintenanceDeadline
      `Prelude.hashWithSalt` maintenanceScheduledDate
      `Prelude.hashWithSalt` maintenanceStartHour
      `Prelude.hashWithSalt` maintenanceDay

instance Prelude.NFData Maintenance where
  rnf Maintenance' {..} =
    Prelude.rnf maintenanceDeadline
      `Prelude.seq` Prelude.rnf maintenanceScheduledDate
      `Prelude.seq` Prelude.rnf maintenanceStartHour
      `Prelude.seq` Prelude.rnf maintenanceDay
