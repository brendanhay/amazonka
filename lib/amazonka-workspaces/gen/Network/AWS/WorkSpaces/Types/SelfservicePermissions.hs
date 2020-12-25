{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.Types.SelfservicePermissions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkSpaces.Types.SelfservicePermissions
  ( SelfservicePermissions (..),

    -- * Smart constructor
    mkSelfservicePermissions,

    -- * Lenses
    spChangeComputeType,
    spIncreaseVolumeSize,
    spRebuildWorkspace,
    spRestartWorkspace,
    spSwitchRunningMode,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.WorkSpaces.Types.ReconnectEnum as Types

-- | Describes the self-service permissions for a directory. For more information, see <https://docs.aws.amazon.com/workspaces/latest/adminguide/enable-user-self-service-workspace-management.html Enable Self-Service WorkSpace Management Capabilities for Your Users> .
--
-- /See:/ 'mkSelfservicePermissions' smart constructor.
data SelfservicePermissions = SelfservicePermissions'
  { -- | Specifies whether users can change the compute type (bundle) for their WorkSpace.
    changeComputeType :: Core.Maybe Types.ReconnectEnum,
    -- | Specifies whether users can increase the volume size of the drives on their WorkSpace.
    increaseVolumeSize :: Core.Maybe Types.ReconnectEnum,
    -- | Specifies whether users can rebuild the operating system of a WorkSpace to its original state.
    rebuildWorkspace :: Core.Maybe Types.ReconnectEnum,
    -- | Specifies whether users can restart their WorkSpace.
    restartWorkspace :: Core.Maybe Types.ReconnectEnum,
    -- | Specifies whether users can switch the running mode of their WorkSpace.
    switchRunningMode :: Core.Maybe Types.ReconnectEnum
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SelfservicePermissions' value with any optional fields omitted.
mkSelfservicePermissions ::
  SelfservicePermissions
mkSelfservicePermissions =
  SelfservicePermissions'
    { changeComputeType = Core.Nothing,
      increaseVolumeSize = Core.Nothing,
      rebuildWorkspace = Core.Nothing,
      restartWorkspace = Core.Nothing,
      switchRunningMode = Core.Nothing
    }

-- | Specifies whether users can change the compute type (bundle) for their WorkSpace.
--
-- /Note:/ Consider using 'changeComputeType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spChangeComputeType :: Lens.Lens' SelfservicePermissions (Core.Maybe Types.ReconnectEnum)
spChangeComputeType = Lens.field @"changeComputeType"
{-# DEPRECATED spChangeComputeType "Use generic-lens or generic-optics with 'changeComputeType' instead." #-}

-- | Specifies whether users can increase the volume size of the drives on their WorkSpace.
--
-- /Note:/ Consider using 'increaseVolumeSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spIncreaseVolumeSize :: Lens.Lens' SelfservicePermissions (Core.Maybe Types.ReconnectEnum)
spIncreaseVolumeSize = Lens.field @"increaseVolumeSize"
{-# DEPRECATED spIncreaseVolumeSize "Use generic-lens or generic-optics with 'increaseVolumeSize' instead." #-}

-- | Specifies whether users can rebuild the operating system of a WorkSpace to its original state.
--
-- /Note:/ Consider using 'rebuildWorkspace' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spRebuildWorkspace :: Lens.Lens' SelfservicePermissions (Core.Maybe Types.ReconnectEnum)
spRebuildWorkspace = Lens.field @"rebuildWorkspace"
{-# DEPRECATED spRebuildWorkspace "Use generic-lens or generic-optics with 'rebuildWorkspace' instead." #-}

-- | Specifies whether users can restart their WorkSpace.
--
-- /Note:/ Consider using 'restartWorkspace' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spRestartWorkspace :: Lens.Lens' SelfservicePermissions (Core.Maybe Types.ReconnectEnum)
spRestartWorkspace = Lens.field @"restartWorkspace"
{-# DEPRECATED spRestartWorkspace "Use generic-lens or generic-optics with 'restartWorkspace' instead." #-}

-- | Specifies whether users can switch the running mode of their WorkSpace.
--
-- /Note:/ Consider using 'switchRunningMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spSwitchRunningMode :: Lens.Lens' SelfservicePermissions (Core.Maybe Types.ReconnectEnum)
spSwitchRunningMode = Lens.field @"switchRunningMode"
{-# DEPRECATED spSwitchRunningMode "Use generic-lens or generic-optics with 'switchRunningMode' instead." #-}

instance Core.FromJSON SelfservicePermissions where
  toJSON SelfservicePermissions {..} =
    Core.object
      ( Core.catMaybes
          [ ("ChangeComputeType" Core..=) Core.<$> changeComputeType,
            ("IncreaseVolumeSize" Core..=) Core.<$> increaseVolumeSize,
            ("RebuildWorkspace" Core..=) Core.<$> rebuildWorkspace,
            ("RestartWorkspace" Core..=) Core.<$> restartWorkspace,
            ("SwitchRunningMode" Core..=) Core.<$> switchRunningMode
          ]
      )

instance Core.FromJSON SelfservicePermissions where
  parseJSON =
    Core.withObject "SelfservicePermissions" Core.$
      \x ->
        SelfservicePermissions'
          Core.<$> (x Core..:? "ChangeComputeType")
          Core.<*> (x Core..:? "IncreaseVolumeSize")
          Core.<*> (x Core..:? "RebuildWorkspace")
          Core.<*> (x Core..:? "RestartWorkspace")
          Core.<*> (x Core..:? "SwitchRunningMode")
