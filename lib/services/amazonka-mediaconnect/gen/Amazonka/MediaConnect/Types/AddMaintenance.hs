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
-- Module      : Amazonka.MediaConnect.Types.AddMaintenance
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConnect.Types.AddMaintenance where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaConnect.Types.MaintenanceDay
import qualified Amazonka.Prelude as Prelude

-- | Create maintenance setting for a flow
--
-- /See:/ 'newAddMaintenance' smart constructor.
data AddMaintenance = AddMaintenance'
  { -- | A day of a week when the maintenance will happen. Use
    -- Monday\/Tuesday\/Wednesday\/Thursday\/Friday\/Saturday\/Sunday.
    maintenanceDay :: MaintenanceDay,
    -- | UTC time when the maintenance will happen. Use 24-hour HH:MM format.
    -- Minutes must be 00. Example: 13:00. The default value is 02:00.
    maintenanceStartHour :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AddMaintenance' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maintenanceDay', 'addMaintenance_maintenanceDay' - A day of a week when the maintenance will happen. Use
-- Monday\/Tuesday\/Wednesday\/Thursday\/Friday\/Saturday\/Sunday.
--
-- 'maintenanceStartHour', 'addMaintenance_maintenanceStartHour' - UTC time when the maintenance will happen. Use 24-hour HH:MM format.
-- Minutes must be 00. Example: 13:00. The default value is 02:00.
newAddMaintenance ::
  -- | 'maintenanceDay'
  MaintenanceDay ->
  -- | 'maintenanceStartHour'
  Prelude.Text ->
  AddMaintenance
newAddMaintenance
  pMaintenanceDay_
  pMaintenanceStartHour_ =
    AddMaintenance'
      { maintenanceDay = pMaintenanceDay_,
        maintenanceStartHour = pMaintenanceStartHour_
      }

-- | A day of a week when the maintenance will happen. Use
-- Monday\/Tuesday\/Wednesday\/Thursday\/Friday\/Saturday\/Sunday.
addMaintenance_maintenanceDay :: Lens.Lens' AddMaintenance MaintenanceDay
addMaintenance_maintenanceDay = Lens.lens (\AddMaintenance' {maintenanceDay} -> maintenanceDay) (\s@AddMaintenance' {} a -> s {maintenanceDay = a} :: AddMaintenance)

-- | UTC time when the maintenance will happen. Use 24-hour HH:MM format.
-- Minutes must be 00. Example: 13:00. The default value is 02:00.
addMaintenance_maintenanceStartHour :: Lens.Lens' AddMaintenance Prelude.Text
addMaintenance_maintenanceStartHour = Lens.lens (\AddMaintenance' {maintenanceStartHour} -> maintenanceStartHour) (\s@AddMaintenance' {} a -> s {maintenanceStartHour = a} :: AddMaintenance)

instance Prelude.Hashable AddMaintenance where
  hashWithSalt _salt AddMaintenance' {..} =
    _salt
      `Prelude.hashWithSalt` maintenanceDay
      `Prelude.hashWithSalt` maintenanceStartHour

instance Prelude.NFData AddMaintenance where
  rnf AddMaintenance' {..} =
    Prelude.rnf maintenanceDay
      `Prelude.seq` Prelude.rnf maintenanceStartHour

instance Data.ToJSON AddMaintenance where
  toJSON AddMaintenance' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("maintenanceDay" Data..= maintenanceDay),
            Prelude.Just
              ( "maintenanceStartHour"
                  Data..= maintenanceStartHour
              )
          ]
      )
