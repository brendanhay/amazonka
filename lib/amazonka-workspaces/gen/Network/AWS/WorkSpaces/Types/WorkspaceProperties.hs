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
    wpRunningMode,
    wpRootVolumeSizeGib,
    wpRunningModeAutoStopTimeoutInMinutes,
    wpUserVolumeSizeGib,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.WorkSpaces.Types.Compute
import Network.AWS.WorkSpaces.Types.RunningMode

-- | Describes a WorkSpace.
--
-- /See:/ 'mkWorkspaceProperties' smart constructor.
data WorkspaceProperties = WorkspaceProperties'
  { computeTypeName ::
      Lude.Maybe Compute,
    runningMode :: Lude.Maybe RunningMode,
    rootVolumeSizeGib :: Lude.Maybe Lude.Int,
    runningModeAutoStopTimeoutInMinutes ::
      Lude.Maybe Lude.Int,
    userVolumeSizeGib :: Lude.Maybe Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'WorkspaceProperties' with the minimum fields required to make a request.
--
-- * 'computeTypeName' - The compute type. For more information, see <http://aws.amazon.com/workspaces/details/#Amazon_WorkSpaces_Bundles Amazon WorkSpaces Bundles> .
-- * 'rootVolumeSizeGib' - The size of the root volume. For important information about how to modify the size of the root and user volumes, see <https://docs.aws.amazon.com/workspaces/latest/adminguide/modify-workspaces.html Modify a WorkSpace> .
-- * 'runningMode' - The running mode. For more information, see <https://docs.aws.amazon.com/workspaces/latest/adminguide/running-mode.html Manage the WorkSpace Running Mode> .
-- * 'runningModeAutoStopTimeoutInMinutes' - The time after a user logs off when WorkSpaces are automatically stopped. Configured in 60-minute intervals.
-- * 'userVolumeSizeGib' - The size of the user storage. For important information about how to modify the size of the root and user volumes, see <https://docs.aws.amazon.com/workspaces/latest/adminguide/modify-workspaces.html Modify a WorkSpace> .
mkWorkspaceProperties ::
  WorkspaceProperties
mkWorkspaceProperties =
  WorkspaceProperties'
    { computeTypeName = Lude.Nothing,
      runningMode = Lude.Nothing,
      rootVolumeSizeGib = Lude.Nothing,
      runningModeAutoStopTimeoutInMinutes = Lude.Nothing,
      userVolumeSizeGib = Lude.Nothing
    }

-- | The compute type. For more information, see <http://aws.amazon.com/workspaces/details/#Amazon_WorkSpaces_Bundles Amazon WorkSpaces Bundles> .
--
-- /Note:/ Consider using 'computeTypeName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wpComputeTypeName :: Lens.Lens' WorkspaceProperties (Lude.Maybe Compute)
wpComputeTypeName = Lens.lens (computeTypeName :: WorkspaceProperties -> Lude.Maybe Compute) (\s a -> s {computeTypeName = a} :: WorkspaceProperties)
{-# DEPRECATED wpComputeTypeName "Use generic-lens or generic-optics with 'computeTypeName' instead." #-}

-- | The running mode. For more information, see <https://docs.aws.amazon.com/workspaces/latest/adminguide/running-mode.html Manage the WorkSpace Running Mode> .
--
-- /Note:/ Consider using 'runningMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wpRunningMode :: Lens.Lens' WorkspaceProperties (Lude.Maybe RunningMode)
wpRunningMode = Lens.lens (runningMode :: WorkspaceProperties -> Lude.Maybe RunningMode) (\s a -> s {runningMode = a} :: WorkspaceProperties)
{-# DEPRECATED wpRunningMode "Use generic-lens or generic-optics with 'runningMode' instead." #-}

-- | The size of the root volume. For important information about how to modify the size of the root and user volumes, see <https://docs.aws.amazon.com/workspaces/latest/adminguide/modify-workspaces.html Modify a WorkSpace> .
--
-- /Note:/ Consider using 'rootVolumeSizeGib' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wpRootVolumeSizeGib :: Lens.Lens' WorkspaceProperties (Lude.Maybe Lude.Int)
wpRootVolumeSizeGib = Lens.lens (rootVolumeSizeGib :: WorkspaceProperties -> Lude.Maybe Lude.Int) (\s a -> s {rootVolumeSizeGib = a} :: WorkspaceProperties)
{-# DEPRECATED wpRootVolumeSizeGib "Use generic-lens or generic-optics with 'rootVolumeSizeGib' instead." #-}

-- | The time after a user logs off when WorkSpaces are automatically stopped. Configured in 60-minute intervals.
--
-- /Note:/ Consider using 'runningModeAutoStopTimeoutInMinutes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wpRunningModeAutoStopTimeoutInMinutes :: Lens.Lens' WorkspaceProperties (Lude.Maybe Lude.Int)
wpRunningModeAutoStopTimeoutInMinutes = Lens.lens (runningModeAutoStopTimeoutInMinutes :: WorkspaceProperties -> Lude.Maybe Lude.Int) (\s a -> s {runningModeAutoStopTimeoutInMinutes = a} :: WorkspaceProperties)
{-# DEPRECATED wpRunningModeAutoStopTimeoutInMinutes "Use generic-lens or generic-optics with 'runningModeAutoStopTimeoutInMinutes' instead." #-}

-- | The size of the user storage. For important information about how to modify the size of the root and user volumes, see <https://docs.aws.amazon.com/workspaces/latest/adminguide/modify-workspaces.html Modify a WorkSpace> .
--
-- /Note:/ Consider using 'userVolumeSizeGib' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wpUserVolumeSizeGib :: Lens.Lens' WorkspaceProperties (Lude.Maybe Lude.Int)
wpUserVolumeSizeGib = Lens.lens (userVolumeSizeGib :: WorkspaceProperties -> Lude.Maybe Lude.Int) (\s a -> s {userVolumeSizeGib = a} :: WorkspaceProperties)
{-# DEPRECATED wpUserVolumeSizeGib "Use generic-lens or generic-optics with 'userVolumeSizeGib' instead." #-}

instance Lude.FromJSON WorkspaceProperties where
  parseJSON =
    Lude.withObject
      "WorkspaceProperties"
      ( \x ->
          WorkspaceProperties'
            Lude.<$> (x Lude..:? "ComputeTypeName")
            Lude.<*> (x Lude..:? "RunningMode")
            Lude.<*> (x Lude..:? "RootVolumeSizeGib")
            Lude.<*> (x Lude..:? "RunningModeAutoStopTimeoutInMinutes")
            Lude.<*> (x Lude..:? "UserVolumeSizeGib")
      )

instance Lude.ToJSON WorkspaceProperties where
  toJSON WorkspaceProperties' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("ComputeTypeName" Lude..=) Lude.<$> computeTypeName,
            ("RunningMode" Lude..=) Lude.<$> runningMode,
            ("RootVolumeSizeGib" Lude..=) Lude.<$> rootVolumeSizeGib,
            ("RunningModeAutoStopTimeoutInMinutes" Lude..=)
              Lude.<$> runningModeAutoStopTimeoutInMinutes,
            ("UserVolumeSizeGib" Lude..=) Lude.<$> userVolumeSizeGib
          ]
      )
