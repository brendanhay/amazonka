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
-- Module      : Amazonka.MediaLive.Types.MaintenanceCreateSettings
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.MaintenanceCreateSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.MediaLive.Types.MaintenanceDay
import qualified Amazonka.Prelude as Prelude

-- | Placeholder documentation for MaintenanceCreateSettings
--
-- /See:/ 'newMaintenanceCreateSettings' smart constructor.
data MaintenanceCreateSettings = MaintenanceCreateSettings'
  { -- | Choose one day of the week for maintenance. The chosen day is used for
    -- all future maintenance windows.
    maintenanceDay :: Prelude.Maybe MaintenanceDay,
    -- | Choose the hour that maintenance will start. The chosen time is used for
    -- all future maintenance windows.
    maintenanceStartTime :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MaintenanceCreateSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maintenanceDay', 'maintenanceCreateSettings_maintenanceDay' - Choose one day of the week for maintenance. The chosen day is used for
-- all future maintenance windows.
--
-- 'maintenanceStartTime', 'maintenanceCreateSettings_maintenanceStartTime' - Choose the hour that maintenance will start. The chosen time is used for
-- all future maintenance windows.
newMaintenanceCreateSettings ::
  MaintenanceCreateSettings
newMaintenanceCreateSettings =
  MaintenanceCreateSettings'
    { maintenanceDay =
        Prelude.Nothing,
      maintenanceStartTime = Prelude.Nothing
    }

-- | Choose one day of the week for maintenance. The chosen day is used for
-- all future maintenance windows.
maintenanceCreateSettings_maintenanceDay :: Lens.Lens' MaintenanceCreateSettings (Prelude.Maybe MaintenanceDay)
maintenanceCreateSettings_maintenanceDay = Lens.lens (\MaintenanceCreateSettings' {maintenanceDay} -> maintenanceDay) (\s@MaintenanceCreateSettings' {} a -> s {maintenanceDay = a} :: MaintenanceCreateSettings)

-- | Choose the hour that maintenance will start. The chosen time is used for
-- all future maintenance windows.
maintenanceCreateSettings_maintenanceStartTime :: Lens.Lens' MaintenanceCreateSettings (Prelude.Maybe Prelude.Text)
maintenanceCreateSettings_maintenanceStartTime = Lens.lens (\MaintenanceCreateSettings' {maintenanceStartTime} -> maintenanceStartTime) (\s@MaintenanceCreateSettings' {} a -> s {maintenanceStartTime = a} :: MaintenanceCreateSettings)

instance Prelude.Hashable MaintenanceCreateSettings where
  hashWithSalt _salt MaintenanceCreateSettings' {..} =
    _salt `Prelude.hashWithSalt` maintenanceDay
      `Prelude.hashWithSalt` maintenanceStartTime

instance Prelude.NFData MaintenanceCreateSettings where
  rnf MaintenanceCreateSettings' {..} =
    Prelude.rnf maintenanceDay
      `Prelude.seq` Prelude.rnf maintenanceStartTime

instance Core.ToJSON MaintenanceCreateSettings where
  toJSON MaintenanceCreateSettings' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("maintenanceDay" Core..=)
              Prelude.<$> maintenanceDay,
            ("maintenanceStartTime" Core..=)
              Prelude.<$> maintenanceStartTime
          ]
      )
