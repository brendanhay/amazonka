{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.Types.WorkspaceProperties
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkSpaces.Types.WorkspaceProperties
  ( WorkspaceProperties (..),

    -- * Smart constructor
    mkWorkspaceProperties,

    -- * Lenses
    wpComputeTypeName,
    wpRootVolumeSizeGib,
    wpRunningMode,
    wpRunningModeAutoStopTimeoutInMinutes,
    wpUserVolumeSizeGib,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.WorkSpaces.Types.Compute as Types
import qualified Network.AWS.WorkSpaces.Types.RunningMode as Types

-- | Describes a WorkSpace.
--
-- /See:/ 'mkWorkspaceProperties' smart constructor.
data WorkspaceProperties = WorkspaceProperties'
  { -- | The compute type. For more information, see <http://aws.amazon.com/workspaces/details/#Amazon_WorkSpaces_Bundles Amazon WorkSpaces Bundles> .
    computeTypeName :: Core.Maybe Types.Compute,
    -- | The size of the root volume. For important information about how to modify the size of the root and user volumes, see <https://docs.aws.amazon.com/workspaces/latest/adminguide/modify-workspaces.html Modify a WorkSpace> .
    rootVolumeSizeGib :: Core.Maybe Core.Int,
    -- | The running mode. For more information, see <https://docs.aws.amazon.com/workspaces/latest/adminguide/running-mode.html Manage the WorkSpace Running Mode> .
    runningMode :: Core.Maybe Types.RunningMode,
    -- | The time after a user logs off when WorkSpaces are automatically stopped. Configured in 60-minute intervals.
    runningModeAutoStopTimeoutInMinutes :: Core.Maybe Core.Int,
    -- | The size of the user storage. For important information about how to modify the size of the root and user volumes, see <https://docs.aws.amazon.com/workspaces/latest/adminguide/modify-workspaces.html Modify a WorkSpace> .
    userVolumeSizeGib :: Core.Maybe Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'WorkspaceProperties' value with any optional fields omitted.
mkWorkspaceProperties ::
  WorkspaceProperties
mkWorkspaceProperties =
  WorkspaceProperties'
    { computeTypeName = Core.Nothing,
      rootVolumeSizeGib = Core.Nothing,
      runningMode = Core.Nothing,
      runningModeAutoStopTimeoutInMinutes = Core.Nothing,
      userVolumeSizeGib = Core.Nothing
    }

-- | The compute type. For more information, see <http://aws.amazon.com/workspaces/details/#Amazon_WorkSpaces_Bundles Amazon WorkSpaces Bundles> .
--
-- /Note:/ Consider using 'computeTypeName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wpComputeTypeName :: Lens.Lens' WorkspaceProperties (Core.Maybe Types.Compute)
wpComputeTypeName = Lens.field @"computeTypeName"
{-# DEPRECATED wpComputeTypeName "Use generic-lens or generic-optics with 'computeTypeName' instead." #-}

-- | The size of the root volume. For important information about how to modify the size of the root and user volumes, see <https://docs.aws.amazon.com/workspaces/latest/adminguide/modify-workspaces.html Modify a WorkSpace> .
--
-- /Note:/ Consider using 'rootVolumeSizeGib' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wpRootVolumeSizeGib :: Lens.Lens' WorkspaceProperties (Core.Maybe Core.Int)
wpRootVolumeSizeGib = Lens.field @"rootVolumeSizeGib"
{-# DEPRECATED wpRootVolumeSizeGib "Use generic-lens or generic-optics with 'rootVolumeSizeGib' instead." #-}

-- | The running mode. For more information, see <https://docs.aws.amazon.com/workspaces/latest/adminguide/running-mode.html Manage the WorkSpace Running Mode> .
--
-- /Note:/ Consider using 'runningMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wpRunningMode :: Lens.Lens' WorkspaceProperties (Core.Maybe Types.RunningMode)
wpRunningMode = Lens.field @"runningMode"
{-# DEPRECATED wpRunningMode "Use generic-lens or generic-optics with 'runningMode' instead." #-}

-- | The time after a user logs off when WorkSpaces are automatically stopped. Configured in 60-minute intervals.
--
-- /Note:/ Consider using 'runningModeAutoStopTimeoutInMinutes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wpRunningModeAutoStopTimeoutInMinutes :: Lens.Lens' WorkspaceProperties (Core.Maybe Core.Int)
wpRunningModeAutoStopTimeoutInMinutes = Lens.field @"runningModeAutoStopTimeoutInMinutes"
{-# DEPRECATED wpRunningModeAutoStopTimeoutInMinutes "Use generic-lens or generic-optics with 'runningModeAutoStopTimeoutInMinutes' instead." #-}

-- | The size of the user storage. For important information about how to modify the size of the root and user volumes, see <https://docs.aws.amazon.com/workspaces/latest/adminguide/modify-workspaces.html Modify a WorkSpace> .
--
-- /Note:/ Consider using 'userVolumeSizeGib' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wpUserVolumeSizeGib :: Lens.Lens' WorkspaceProperties (Core.Maybe Core.Int)
wpUserVolumeSizeGib = Lens.field @"userVolumeSizeGib"
{-# DEPRECATED wpUserVolumeSizeGib "Use generic-lens or generic-optics with 'userVolumeSizeGib' instead." #-}

instance Core.FromJSON WorkspaceProperties where
  toJSON WorkspaceProperties {..} =
    Core.object
      ( Core.catMaybes
          [ ("ComputeTypeName" Core..=) Core.<$> computeTypeName,
            ("RootVolumeSizeGib" Core..=) Core.<$> rootVolumeSizeGib,
            ("RunningMode" Core..=) Core.<$> runningMode,
            ("RunningModeAutoStopTimeoutInMinutes" Core..=)
              Core.<$> runningModeAutoStopTimeoutInMinutes,
            ("UserVolumeSizeGib" Core..=) Core.<$> userVolumeSizeGib
          ]
      )

instance Core.FromJSON WorkspaceProperties where
  parseJSON =
    Core.withObject "WorkspaceProperties" Core.$
      \x ->
        WorkspaceProperties'
          Core.<$> (x Core..:? "ComputeTypeName")
          Core.<*> (x Core..:? "RootVolumeSizeGib")
          Core.<*> (x Core..:? "RunningMode")
          Core.<*> (x Core..:? "RunningModeAutoStopTimeoutInMinutes")
          Core.<*> (x Core..:? "UserVolumeSizeGib")
