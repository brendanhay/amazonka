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
-- Module      : Amazonka.FSx.Types.UpdateFileCacheLustreConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FSx.Types.UpdateFileCacheLustreConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The configuration update for an Amazon File Cache resource.
--
-- /See:/ 'newUpdateFileCacheLustreConfiguration' smart constructor.
data UpdateFileCacheLustreConfiguration = UpdateFileCacheLustreConfiguration'
  { weeklyMaintenanceStartTime :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateFileCacheLustreConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'weeklyMaintenanceStartTime', 'updateFileCacheLustreConfiguration_weeklyMaintenanceStartTime' - Undocumented member.
newUpdateFileCacheLustreConfiguration ::
  UpdateFileCacheLustreConfiguration
newUpdateFileCacheLustreConfiguration =
  UpdateFileCacheLustreConfiguration'
    { weeklyMaintenanceStartTime =
        Prelude.Nothing
    }

-- | Undocumented member.
updateFileCacheLustreConfiguration_weeklyMaintenanceStartTime :: Lens.Lens' UpdateFileCacheLustreConfiguration (Prelude.Maybe Prelude.Text)
updateFileCacheLustreConfiguration_weeklyMaintenanceStartTime = Lens.lens (\UpdateFileCacheLustreConfiguration' {weeklyMaintenanceStartTime} -> weeklyMaintenanceStartTime) (\s@UpdateFileCacheLustreConfiguration' {} a -> s {weeklyMaintenanceStartTime = a} :: UpdateFileCacheLustreConfiguration)

instance
  Prelude.Hashable
    UpdateFileCacheLustreConfiguration
  where
  hashWithSalt
    _salt
    UpdateFileCacheLustreConfiguration' {..} =
      _salt
        `Prelude.hashWithSalt` weeklyMaintenanceStartTime

instance
  Prelude.NFData
    UpdateFileCacheLustreConfiguration
  where
  rnf UpdateFileCacheLustreConfiguration' {..} =
    Prelude.rnf weeklyMaintenanceStartTime

instance
  Data.ToJSON
    UpdateFileCacheLustreConfiguration
  where
  toJSON UpdateFileCacheLustreConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("WeeklyMaintenanceStartTime" Data..=)
              Prelude.<$> weeklyMaintenanceStartTime
          ]
      )
