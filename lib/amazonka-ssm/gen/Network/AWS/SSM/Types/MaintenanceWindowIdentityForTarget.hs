{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.MaintenanceWindowIdentityForTarget
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.MaintenanceWindowIdentityForTarget
  ( MaintenanceWindowIdentityForTarget (..),

    -- * Smart constructor
    mkMaintenanceWindowIdentityForTarget,

    -- * Lenses
    mwiftName,
    mwiftWindowId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SSM.Types.MaintenanceWindowId as Types
import qualified Network.AWS.SSM.Types.MaintenanceWindowName as Types

-- | The maintenance window to which the specified target belongs.
--
-- /See:/ 'mkMaintenanceWindowIdentityForTarget' smart constructor.
data MaintenanceWindowIdentityForTarget = MaintenanceWindowIdentityForTarget'
  { -- | The name of the maintenance window.
    name :: Core.Maybe Types.MaintenanceWindowName,
    -- | The ID of the maintenance window.
    windowId :: Core.Maybe Types.MaintenanceWindowId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'MaintenanceWindowIdentityForTarget' value with any optional fields omitted.
mkMaintenanceWindowIdentityForTarget ::
  MaintenanceWindowIdentityForTarget
mkMaintenanceWindowIdentityForTarget =
  MaintenanceWindowIdentityForTarget'
    { name = Core.Nothing,
      windowId = Core.Nothing
    }

-- | The name of the maintenance window.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mwiftName :: Lens.Lens' MaintenanceWindowIdentityForTarget (Core.Maybe Types.MaintenanceWindowName)
mwiftName = Lens.field @"name"
{-# DEPRECATED mwiftName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The ID of the maintenance window.
--
-- /Note:/ Consider using 'windowId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mwiftWindowId :: Lens.Lens' MaintenanceWindowIdentityForTarget (Core.Maybe Types.MaintenanceWindowId)
mwiftWindowId = Lens.field @"windowId"
{-# DEPRECATED mwiftWindowId "Use generic-lens or generic-optics with 'windowId' instead." #-}

instance Core.FromJSON MaintenanceWindowIdentityForTarget where
  parseJSON =
    Core.withObject "MaintenanceWindowIdentityForTarget" Core.$
      \x ->
        MaintenanceWindowIdentityForTarget'
          Core.<$> (x Core..:? "Name") Core.<*> (x Core..:? "WindowId")
