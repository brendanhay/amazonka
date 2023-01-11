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
-- Module      : Amazonka.SSM.Types.MaintenanceWindowIdentityForTarget
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSM.Types.MaintenanceWindowIdentityForTarget where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The maintenance window to which the specified target belongs.
--
-- /See:/ 'newMaintenanceWindowIdentityForTarget' smart constructor.
data MaintenanceWindowIdentityForTarget = MaintenanceWindowIdentityForTarget'
  { -- | The name of the maintenance window.
    name :: Prelude.Maybe Prelude.Text,
    -- | The ID of the maintenance window.
    windowId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MaintenanceWindowIdentityForTarget' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'maintenanceWindowIdentityForTarget_name' - The name of the maintenance window.
--
-- 'windowId', 'maintenanceWindowIdentityForTarget_windowId' - The ID of the maintenance window.
newMaintenanceWindowIdentityForTarget ::
  MaintenanceWindowIdentityForTarget
newMaintenanceWindowIdentityForTarget =
  MaintenanceWindowIdentityForTarget'
    { name =
        Prelude.Nothing,
      windowId = Prelude.Nothing
    }

-- | The name of the maintenance window.
maintenanceWindowIdentityForTarget_name :: Lens.Lens' MaintenanceWindowIdentityForTarget (Prelude.Maybe Prelude.Text)
maintenanceWindowIdentityForTarget_name = Lens.lens (\MaintenanceWindowIdentityForTarget' {name} -> name) (\s@MaintenanceWindowIdentityForTarget' {} a -> s {name = a} :: MaintenanceWindowIdentityForTarget)

-- | The ID of the maintenance window.
maintenanceWindowIdentityForTarget_windowId :: Lens.Lens' MaintenanceWindowIdentityForTarget (Prelude.Maybe Prelude.Text)
maintenanceWindowIdentityForTarget_windowId = Lens.lens (\MaintenanceWindowIdentityForTarget' {windowId} -> windowId) (\s@MaintenanceWindowIdentityForTarget' {} a -> s {windowId = a} :: MaintenanceWindowIdentityForTarget)

instance
  Data.FromJSON
    MaintenanceWindowIdentityForTarget
  where
  parseJSON =
    Data.withObject
      "MaintenanceWindowIdentityForTarget"
      ( \x ->
          MaintenanceWindowIdentityForTarget'
            Prelude.<$> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "WindowId")
      )

instance
  Prelude.Hashable
    MaintenanceWindowIdentityForTarget
  where
  hashWithSalt
    _salt
    MaintenanceWindowIdentityForTarget' {..} =
      _salt `Prelude.hashWithSalt` name
        `Prelude.hashWithSalt` windowId

instance
  Prelude.NFData
    MaintenanceWindowIdentityForTarget
  where
  rnf MaintenanceWindowIdentityForTarget' {..} =
    Prelude.rnf name `Prelude.seq` Prelude.rnf windowId
