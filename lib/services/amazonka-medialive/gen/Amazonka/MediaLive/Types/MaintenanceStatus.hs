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
-- Module      : Amazonka.MediaLive.Types.MaintenanceStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.MaintenanceStatus where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.MediaLive.Types.MaintenanceDay
import qualified Amazonka.Prelude as Prelude

-- | Placeholder documentation for MaintenanceStatus
--
-- /See:/ 'newMaintenanceStatus' smart constructor.
data MaintenanceStatus = MaintenanceStatus'
  { -- | Maintenance is required by the displayed date and time. Date and time is
    -- in ISO.
    maintenanceDeadline :: Prelude.Maybe Prelude.Text,
    -- | The currently scheduled maintenance date and time. Date and time is in
    -- ISO.
    maintenanceScheduledDate :: Prelude.Maybe Prelude.Text,
    -- | The currently selected maintenance day.
    maintenanceDay :: Prelude.Maybe MaintenanceDay,
    -- | The currently selected maintenance start time. Time is in UTC.
    maintenanceStartTime :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MaintenanceStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maintenanceDeadline', 'maintenanceStatus_maintenanceDeadline' - Maintenance is required by the displayed date and time. Date and time is
-- in ISO.
--
-- 'maintenanceScheduledDate', 'maintenanceStatus_maintenanceScheduledDate' - The currently scheduled maintenance date and time. Date and time is in
-- ISO.
--
-- 'maintenanceDay', 'maintenanceStatus_maintenanceDay' - The currently selected maintenance day.
--
-- 'maintenanceStartTime', 'maintenanceStatus_maintenanceStartTime' - The currently selected maintenance start time. Time is in UTC.
newMaintenanceStatus ::
  MaintenanceStatus
newMaintenanceStatus =
  MaintenanceStatus'
    { maintenanceDeadline =
        Prelude.Nothing,
      maintenanceScheduledDate = Prelude.Nothing,
      maintenanceDay = Prelude.Nothing,
      maintenanceStartTime = Prelude.Nothing
    }

-- | Maintenance is required by the displayed date and time. Date and time is
-- in ISO.
maintenanceStatus_maintenanceDeadline :: Lens.Lens' MaintenanceStatus (Prelude.Maybe Prelude.Text)
maintenanceStatus_maintenanceDeadline = Lens.lens (\MaintenanceStatus' {maintenanceDeadline} -> maintenanceDeadline) (\s@MaintenanceStatus' {} a -> s {maintenanceDeadline = a} :: MaintenanceStatus)

-- | The currently scheduled maintenance date and time. Date and time is in
-- ISO.
maintenanceStatus_maintenanceScheduledDate :: Lens.Lens' MaintenanceStatus (Prelude.Maybe Prelude.Text)
maintenanceStatus_maintenanceScheduledDate = Lens.lens (\MaintenanceStatus' {maintenanceScheduledDate} -> maintenanceScheduledDate) (\s@MaintenanceStatus' {} a -> s {maintenanceScheduledDate = a} :: MaintenanceStatus)

-- | The currently selected maintenance day.
maintenanceStatus_maintenanceDay :: Lens.Lens' MaintenanceStatus (Prelude.Maybe MaintenanceDay)
maintenanceStatus_maintenanceDay = Lens.lens (\MaintenanceStatus' {maintenanceDay} -> maintenanceDay) (\s@MaintenanceStatus' {} a -> s {maintenanceDay = a} :: MaintenanceStatus)

-- | The currently selected maintenance start time. Time is in UTC.
maintenanceStatus_maintenanceStartTime :: Lens.Lens' MaintenanceStatus (Prelude.Maybe Prelude.Text)
maintenanceStatus_maintenanceStartTime = Lens.lens (\MaintenanceStatus' {maintenanceStartTime} -> maintenanceStartTime) (\s@MaintenanceStatus' {} a -> s {maintenanceStartTime = a} :: MaintenanceStatus)

instance Core.FromJSON MaintenanceStatus where
  parseJSON =
    Core.withObject
      "MaintenanceStatus"
      ( \x ->
          MaintenanceStatus'
            Prelude.<$> (x Core..:? "maintenanceDeadline")
            Prelude.<*> (x Core..:? "maintenanceScheduledDate")
            Prelude.<*> (x Core..:? "maintenanceDay")
            Prelude.<*> (x Core..:? "maintenanceStartTime")
      )

instance Prelude.Hashable MaintenanceStatus where
  hashWithSalt _salt MaintenanceStatus' {..} =
    _salt `Prelude.hashWithSalt` maintenanceDeadline
      `Prelude.hashWithSalt` maintenanceScheduledDate
      `Prelude.hashWithSalt` maintenanceDay
      `Prelude.hashWithSalt` maintenanceStartTime

instance Prelude.NFData MaintenanceStatus where
  rnf MaintenanceStatus' {..} =
    Prelude.rnf maintenanceDeadline
      `Prelude.seq` Prelude.rnf maintenanceScheduledDate
      `Prelude.seq` Prelude.rnf maintenanceDay
      `Prelude.seq` Prelude.rnf maintenanceStartTime
