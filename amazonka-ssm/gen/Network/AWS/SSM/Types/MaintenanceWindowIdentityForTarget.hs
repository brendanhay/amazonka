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
-- Module      : Network.AWS.SSM.Types.MaintenanceWindowIdentityForTarget
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.MaintenanceWindowIdentityForTarget where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | The maintenance window to which the specified target belongs.
--
-- /See:/ 'newMaintenanceWindowIdentityForTarget' smart constructor.
data MaintenanceWindowIdentityForTarget = MaintenanceWindowIdentityForTarget'
  { -- | The name of the maintenance window.
    name :: Core.Maybe Core.Text,
    -- | The ID of the maintenance window.
    windowId :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
        Core.Nothing,
      windowId = Core.Nothing
    }

-- | The name of the maintenance window.
maintenanceWindowIdentityForTarget_name :: Lens.Lens' MaintenanceWindowIdentityForTarget (Core.Maybe Core.Text)
maintenanceWindowIdentityForTarget_name = Lens.lens (\MaintenanceWindowIdentityForTarget' {name} -> name) (\s@MaintenanceWindowIdentityForTarget' {} a -> s {name = a} :: MaintenanceWindowIdentityForTarget)

-- | The ID of the maintenance window.
maintenanceWindowIdentityForTarget_windowId :: Lens.Lens' MaintenanceWindowIdentityForTarget (Core.Maybe Core.Text)
maintenanceWindowIdentityForTarget_windowId = Lens.lens (\MaintenanceWindowIdentityForTarget' {windowId} -> windowId) (\s@MaintenanceWindowIdentityForTarget' {} a -> s {windowId = a} :: MaintenanceWindowIdentityForTarget)

instance
  Core.FromJSON
    MaintenanceWindowIdentityForTarget
  where
  parseJSON =
    Core.withObject
      "MaintenanceWindowIdentityForTarget"
      ( \x ->
          MaintenanceWindowIdentityForTarget'
            Core.<$> (x Core..:? "Name") Core.<*> (x Core..:? "WindowId")
      )

instance
  Core.Hashable
    MaintenanceWindowIdentityForTarget

instance
  Core.NFData
    MaintenanceWindowIdentityForTarget
