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
-- Module      : Amazonka.MediaLive.Types.MaintenanceUpdateSettings
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.MaintenanceUpdateSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaLive.Types.MaintenanceDay
import qualified Amazonka.Prelude as Prelude

-- | Placeholder documentation for MaintenanceUpdateSettings
--
-- /See:/ 'newMaintenanceUpdateSettings' smart constructor.
data MaintenanceUpdateSettings = MaintenanceUpdateSettings'
  { -- | Choose one day of the week for maintenance. The chosen day is used for
    -- all future maintenance windows.
    maintenanceDay :: Prelude.Maybe MaintenanceDay,
    -- | Choose a specific date for maintenance to occur. The chosen date is used
    -- for the next maintenance window only.
    maintenanceScheduledDate :: Prelude.Maybe Prelude.Text,
    -- | Choose the hour that maintenance will start. The chosen time is used for
    -- all future maintenance windows.
    maintenanceStartTime :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MaintenanceUpdateSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maintenanceDay', 'maintenanceUpdateSettings_maintenanceDay' - Choose one day of the week for maintenance. The chosen day is used for
-- all future maintenance windows.
--
-- 'maintenanceScheduledDate', 'maintenanceUpdateSettings_maintenanceScheduledDate' - Choose a specific date for maintenance to occur. The chosen date is used
-- for the next maintenance window only.
--
-- 'maintenanceStartTime', 'maintenanceUpdateSettings_maintenanceStartTime' - Choose the hour that maintenance will start. The chosen time is used for
-- all future maintenance windows.
newMaintenanceUpdateSettings ::
  MaintenanceUpdateSettings
newMaintenanceUpdateSettings =
  MaintenanceUpdateSettings'
    { maintenanceDay =
        Prelude.Nothing,
      maintenanceScheduledDate = Prelude.Nothing,
      maintenanceStartTime = Prelude.Nothing
    }

-- | Choose one day of the week for maintenance. The chosen day is used for
-- all future maintenance windows.
maintenanceUpdateSettings_maintenanceDay :: Lens.Lens' MaintenanceUpdateSettings (Prelude.Maybe MaintenanceDay)
maintenanceUpdateSettings_maintenanceDay = Lens.lens (\MaintenanceUpdateSettings' {maintenanceDay} -> maintenanceDay) (\s@MaintenanceUpdateSettings' {} a -> s {maintenanceDay = a} :: MaintenanceUpdateSettings)

-- | Choose a specific date for maintenance to occur. The chosen date is used
-- for the next maintenance window only.
maintenanceUpdateSettings_maintenanceScheduledDate :: Lens.Lens' MaintenanceUpdateSettings (Prelude.Maybe Prelude.Text)
maintenanceUpdateSettings_maintenanceScheduledDate = Lens.lens (\MaintenanceUpdateSettings' {maintenanceScheduledDate} -> maintenanceScheduledDate) (\s@MaintenanceUpdateSettings' {} a -> s {maintenanceScheduledDate = a} :: MaintenanceUpdateSettings)

-- | Choose the hour that maintenance will start. The chosen time is used for
-- all future maintenance windows.
maintenanceUpdateSettings_maintenanceStartTime :: Lens.Lens' MaintenanceUpdateSettings (Prelude.Maybe Prelude.Text)
maintenanceUpdateSettings_maintenanceStartTime = Lens.lens (\MaintenanceUpdateSettings' {maintenanceStartTime} -> maintenanceStartTime) (\s@MaintenanceUpdateSettings' {} a -> s {maintenanceStartTime = a} :: MaintenanceUpdateSettings)

instance Prelude.Hashable MaintenanceUpdateSettings where
  hashWithSalt _salt MaintenanceUpdateSettings' {..} =
    _salt `Prelude.hashWithSalt` maintenanceDay
      `Prelude.hashWithSalt` maintenanceScheduledDate
      `Prelude.hashWithSalt` maintenanceStartTime

instance Prelude.NFData MaintenanceUpdateSettings where
  rnf MaintenanceUpdateSettings' {..} =
    Prelude.rnf maintenanceDay
      `Prelude.seq` Prelude.rnf maintenanceScheduledDate
      `Prelude.seq` Prelude.rnf maintenanceStartTime

instance Data.ToJSON MaintenanceUpdateSettings where
  toJSON MaintenanceUpdateSettings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("maintenanceDay" Data..=)
              Prelude.<$> maintenanceDay,
            ("maintenanceScheduledDate" Data..=)
              Prelude.<$> maintenanceScheduledDate,
            ("maintenanceStartTime" Data..=)
              Prelude.<$> maintenanceStartTime
          ]
      )
