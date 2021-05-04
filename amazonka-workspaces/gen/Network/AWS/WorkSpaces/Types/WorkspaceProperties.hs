{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.WorkSpaces.Types.WorkspaceProperties
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkSpaces.Types.WorkspaceProperties where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.WorkSpaces.Types.Compute
import Network.AWS.WorkSpaces.Types.RunningMode

-- | Describes a WorkSpace.
--
-- /See:/ 'newWorkspaceProperties' smart constructor.
data WorkspaceProperties = WorkspaceProperties'
  { -- | The size of the root volume. For important information about how to
    -- modify the size of the root and user volumes, see
    -- <https://docs.aws.amazon.com/workspaces/latest/adminguide/modify-workspaces.html Modify a WorkSpace>.
    rootVolumeSizeGib :: Prelude.Maybe Prelude.Int,
    -- | The running mode. For more information, see
    -- <https://docs.aws.amazon.com/workspaces/latest/adminguide/running-mode.html Manage the WorkSpace Running Mode>.
    runningMode :: Prelude.Maybe RunningMode,
    -- | The size of the user storage. For important information about how to
    -- modify the size of the root and user volumes, see
    -- <https://docs.aws.amazon.com/workspaces/latest/adminguide/modify-workspaces.html Modify a WorkSpace>.
    userVolumeSizeGib :: Prelude.Maybe Prelude.Int,
    -- | The time after a user logs off when WorkSpaces are automatically
    -- stopped. Configured in 60-minute intervals.
    runningModeAutoStopTimeoutInMinutes :: Prelude.Maybe Prelude.Int,
    -- | The compute type. For more information, see
    -- <http://aws.amazon.com/workspaces/details/#Amazon_WorkSpaces_Bundles Amazon WorkSpaces Bundles>.
    computeTypeName :: Prelude.Maybe Compute
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'WorkspaceProperties' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'rootVolumeSizeGib', 'workspaceProperties_rootVolumeSizeGib' - The size of the root volume. For important information about how to
-- modify the size of the root and user volumes, see
-- <https://docs.aws.amazon.com/workspaces/latest/adminguide/modify-workspaces.html Modify a WorkSpace>.
--
-- 'runningMode', 'workspaceProperties_runningMode' - The running mode. For more information, see
-- <https://docs.aws.amazon.com/workspaces/latest/adminguide/running-mode.html Manage the WorkSpace Running Mode>.
--
-- 'userVolumeSizeGib', 'workspaceProperties_userVolumeSizeGib' - The size of the user storage. For important information about how to
-- modify the size of the root and user volumes, see
-- <https://docs.aws.amazon.com/workspaces/latest/adminguide/modify-workspaces.html Modify a WorkSpace>.
--
-- 'runningModeAutoStopTimeoutInMinutes', 'workspaceProperties_runningModeAutoStopTimeoutInMinutes' - The time after a user logs off when WorkSpaces are automatically
-- stopped. Configured in 60-minute intervals.
--
-- 'computeTypeName', 'workspaceProperties_computeTypeName' - The compute type. For more information, see
-- <http://aws.amazon.com/workspaces/details/#Amazon_WorkSpaces_Bundles Amazon WorkSpaces Bundles>.
newWorkspaceProperties ::
  WorkspaceProperties
newWorkspaceProperties =
  WorkspaceProperties'
    { rootVolumeSizeGib =
        Prelude.Nothing,
      runningMode = Prelude.Nothing,
      userVolumeSizeGib = Prelude.Nothing,
      runningModeAutoStopTimeoutInMinutes =
        Prelude.Nothing,
      computeTypeName = Prelude.Nothing
    }

-- | The size of the root volume. For important information about how to
-- modify the size of the root and user volumes, see
-- <https://docs.aws.amazon.com/workspaces/latest/adminguide/modify-workspaces.html Modify a WorkSpace>.
workspaceProperties_rootVolumeSizeGib :: Lens.Lens' WorkspaceProperties (Prelude.Maybe Prelude.Int)
workspaceProperties_rootVolumeSizeGib = Lens.lens (\WorkspaceProperties' {rootVolumeSizeGib} -> rootVolumeSizeGib) (\s@WorkspaceProperties' {} a -> s {rootVolumeSizeGib = a} :: WorkspaceProperties)

-- | The running mode. For more information, see
-- <https://docs.aws.amazon.com/workspaces/latest/adminguide/running-mode.html Manage the WorkSpace Running Mode>.
workspaceProperties_runningMode :: Lens.Lens' WorkspaceProperties (Prelude.Maybe RunningMode)
workspaceProperties_runningMode = Lens.lens (\WorkspaceProperties' {runningMode} -> runningMode) (\s@WorkspaceProperties' {} a -> s {runningMode = a} :: WorkspaceProperties)

-- | The size of the user storage. For important information about how to
-- modify the size of the root and user volumes, see
-- <https://docs.aws.amazon.com/workspaces/latest/adminguide/modify-workspaces.html Modify a WorkSpace>.
workspaceProperties_userVolumeSizeGib :: Lens.Lens' WorkspaceProperties (Prelude.Maybe Prelude.Int)
workspaceProperties_userVolumeSizeGib = Lens.lens (\WorkspaceProperties' {userVolumeSizeGib} -> userVolumeSizeGib) (\s@WorkspaceProperties' {} a -> s {userVolumeSizeGib = a} :: WorkspaceProperties)

-- | The time after a user logs off when WorkSpaces are automatically
-- stopped. Configured in 60-minute intervals.
workspaceProperties_runningModeAutoStopTimeoutInMinutes :: Lens.Lens' WorkspaceProperties (Prelude.Maybe Prelude.Int)
workspaceProperties_runningModeAutoStopTimeoutInMinutes = Lens.lens (\WorkspaceProperties' {runningModeAutoStopTimeoutInMinutes} -> runningModeAutoStopTimeoutInMinutes) (\s@WorkspaceProperties' {} a -> s {runningModeAutoStopTimeoutInMinutes = a} :: WorkspaceProperties)

-- | The compute type. For more information, see
-- <http://aws.amazon.com/workspaces/details/#Amazon_WorkSpaces_Bundles Amazon WorkSpaces Bundles>.
workspaceProperties_computeTypeName :: Lens.Lens' WorkspaceProperties (Prelude.Maybe Compute)
workspaceProperties_computeTypeName = Lens.lens (\WorkspaceProperties' {computeTypeName} -> computeTypeName) (\s@WorkspaceProperties' {} a -> s {computeTypeName = a} :: WorkspaceProperties)

instance Prelude.FromJSON WorkspaceProperties where
  parseJSON =
    Prelude.withObject
      "WorkspaceProperties"
      ( \x ->
          WorkspaceProperties'
            Prelude.<$> (x Prelude..:? "RootVolumeSizeGib")
            Prelude.<*> (x Prelude..:? "RunningMode")
            Prelude.<*> (x Prelude..:? "UserVolumeSizeGib")
            Prelude.<*> (x Prelude..:? "RunningModeAutoStopTimeoutInMinutes")
            Prelude.<*> (x Prelude..:? "ComputeTypeName")
      )

instance Prelude.Hashable WorkspaceProperties

instance Prelude.NFData WorkspaceProperties

instance Prelude.ToJSON WorkspaceProperties where
  toJSON WorkspaceProperties' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("RootVolumeSizeGib" Prelude..=)
              Prelude.<$> rootVolumeSizeGib,
            ("RunningMode" Prelude..=) Prelude.<$> runningMode,
            ("UserVolumeSizeGib" Prelude..=)
              Prelude.<$> userVolumeSizeGib,
            ("RunningModeAutoStopTimeoutInMinutes" Prelude..=)
              Prelude.<$> runningModeAutoStopTimeoutInMinutes,
            ("ComputeTypeName" Prelude..=)
              Prelude.<$> computeTypeName
          ]
      )
