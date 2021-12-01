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
-- Module      : Amazonka.WorkSpaces.Types.WorkspaceProperties
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WorkSpaces.Types.WorkspaceProperties where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.WorkSpaces.Types.Compute
import Amazonka.WorkSpaces.Types.RunningMode

-- | Describes a WorkSpace.
--
-- /See:/ 'newWorkspaceProperties' smart constructor.
data WorkspaceProperties = WorkspaceProperties'
  { -- | The compute type. For more information, see
    -- <http://aws.amazon.com/workspaces/details/#Amazon_WorkSpaces_Bundles Amazon WorkSpaces Bundles>.
    computeTypeName :: Prelude.Maybe Compute,
    -- | The running mode. For more information, see
    -- <https://docs.aws.amazon.com/workspaces/latest/adminguide/running-mode.html Manage the WorkSpace Running Mode>.
    runningMode :: Prelude.Maybe RunningMode,
    -- | The size of the root volume. For important information about how to
    -- modify the size of the root and user volumes, see
    -- <https://docs.aws.amazon.com/workspaces/latest/adminguide/modify-workspaces.html Modify a WorkSpace>.
    rootVolumeSizeGib :: Prelude.Maybe Prelude.Int,
    -- | The time after a user logs off when WorkSpaces are automatically
    -- stopped. Configured in 60-minute intervals.
    runningModeAutoStopTimeoutInMinutes :: Prelude.Maybe Prelude.Int,
    -- | The size of the user storage. For important information about how to
    -- modify the size of the root and user volumes, see
    -- <https://docs.aws.amazon.com/workspaces/latest/adminguide/modify-workspaces.html Modify a WorkSpace>.
    userVolumeSizeGib :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'WorkspaceProperties' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'computeTypeName', 'workspaceProperties_computeTypeName' - The compute type. For more information, see
-- <http://aws.amazon.com/workspaces/details/#Amazon_WorkSpaces_Bundles Amazon WorkSpaces Bundles>.
--
-- 'runningMode', 'workspaceProperties_runningMode' - The running mode. For more information, see
-- <https://docs.aws.amazon.com/workspaces/latest/adminguide/running-mode.html Manage the WorkSpace Running Mode>.
--
-- 'rootVolumeSizeGib', 'workspaceProperties_rootVolumeSizeGib' - The size of the root volume. For important information about how to
-- modify the size of the root and user volumes, see
-- <https://docs.aws.amazon.com/workspaces/latest/adminguide/modify-workspaces.html Modify a WorkSpace>.
--
-- 'runningModeAutoStopTimeoutInMinutes', 'workspaceProperties_runningModeAutoStopTimeoutInMinutes' - The time after a user logs off when WorkSpaces are automatically
-- stopped. Configured in 60-minute intervals.
--
-- 'userVolumeSizeGib', 'workspaceProperties_userVolumeSizeGib' - The size of the user storage. For important information about how to
-- modify the size of the root and user volumes, see
-- <https://docs.aws.amazon.com/workspaces/latest/adminguide/modify-workspaces.html Modify a WorkSpace>.
newWorkspaceProperties ::
  WorkspaceProperties
newWorkspaceProperties =
  WorkspaceProperties'
    { computeTypeName =
        Prelude.Nothing,
      runningMode = Prelude.Nothing,
      rootVolumeSizeGib = Prelude.Nothing,
      runningModeAutoStopTimeoutInMinutes =
        Prelude.Nothing,
      userVolumeSizeGib = Prelude.Nothing
    }

-- | The compute type. For more information, see
-- <http://aws.amazon.com/workspaces/details/#Amazon_WorkSpaces_Bundles Amazon WorkSpaces Bundles>.
workspaceProperties_computeTypeName :: Lens.Lens' WorkspaceProperties (Prelude.Maybe Compute)
workspaceProperties_computeTypeName = Lens.lens (\WorkspaceProperties' {computeTypeName} -> computeTypeName) (\s@WorkspaceProperties' {} a -> s {computeTypeName = a} :: WorkspaceProperties)

-- | The running mode. For more information, see
-- <https://docs.aws.amazon.com/workspaces/latest/adminguide/running-mode.html Manage the WorkSpace Running Mode>.
workspaceProperties_runningMode :: Lens.Lens' WorkspaceProperties (Prelude.Maybe RunningMode)
workspaceProperties_runningMode = Lens.lens (\WorkspaceProperties' {runningMode} -> runningMode) (\s@WorkspaceProperties' {} a -> s {runningMode = a} :: WorkspaceProperties)

-- | The size of the root volume. For important information about how to
-- modify the size of the root and user volumes, see
-- <https://docs.aws.amazon.com/workspaces/latest/adminguide/modify-workspaces.html Modify a WorkSpace>.
workspaceProperties_rootVolumeSizeGib :: Lens.Lens' WorkspaceProperties (Prelude.Maybe Prelude.Int)
workspaceProperties_rootVolumeSizeGib = Lens.lens (\WorkspaceProperties' {rootVolumeSizeGib} -> rootVolumeSizeGib) (\s@WorkspaceProperties' {} a -> s {rootVolumeSizeGib = a} :: WorkspaceProperties)

-- | The time after a user logs off when WorkSpaces are automatically
-- stopped. Configured in 60-minute intervals.
workspaceProperties_runningModeAutoStopTimeoutInMinutes :: Lens.Lens' WorkspaceProperties (Prelude.Maybe Prelude.Int)
workspaceProperties_runningModeAutoStopTimeoutInMinutes = Lens.lens (\WorkspaceProperties' {runningModeAutoStopTimeoutInMinutes} -> runningModeAutoStopTimeoutInMinutes) (\s@WorkspaceProperties' {} a -> s {runningModeAutoStopTimeoutInMinutes = a} :: WorkspaceProperties)

-- | The size of the user storage. For important information about how to
-- modify the size of the root and user volumes, see
-- <https://docs.aws.amazon.com/workspaces/latest/adminguide/modify-workspaces.html Modify a WorkSpace>.
workspaceProperties_userVolumeSizeGib :: Lens.Lens' WorkspaceProperties (Prelude.Maybe Prelude.Int)
workspaceProperties_userVolumeSizeGib = Lens.lens (\WorkspaceProperties' {userVolumeSizeGib} -> userVolumeSizeGib) (\s@WorkspaceProperties' {} a -> s {userVolumeSizeGib = a} :: WorkspaceProperties)

instance Core.FromJSON WorkspaceProperties where
  parseJSON =
    Core.withObject
      "WorkspaceProperties"
      ( \x ->
          WorkspaceProperties'
            Prelude.<$> (x Core..:? "ComputeTypeName")
            Prelude.<*> (x Core..:? "RunningMode")
            Prelude.<*> (x Core..:? "RootVolumeSizeGib")
            Prelude.<*> (x Core..:? "RunningModeAutoStopTimeoutInMinutes")
            Prelude.<*> (x Core..:? "UserVolumeSizeGib")
      )

instance Prelude.Hashable WorkspaceProperties where
  hashWithSalt salt' WorkspaceProperties' {..} =
    salt' `Prelude.hashWithSalt` userVolumeSizeGib
      `Prelude.hashWithSalt` runningModeAutoStopTimeoutInMinutes
      `Prelude.hashWithSalt` rootVolumeSizeGib
      `Prelude.hashWithSalt` runningMode
      `Prelude.hashWithSalt` computeTypeName

instance Prelude.NFData WorkspaceProperties where
  rnf WorkspaceProperties' {..} =
    Prelude.rnf computeTypeName
      `Prelude.seq` Prelude.rnf userVolumeSizeGib
      `Prelude.seq` Prelude.rnf runningModeAutoStopTimeoutInMinutes
      `Prelude.seq` Prelude.rnf rootVolumeSizeGib
      `Prelude.seq` Prelude.rnf runningMode

instance Core.ToJSON WorkspaceProperties where
  toJSON WorkspaceProperties' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("ComputeTypeName" Core..=)
              Prelude.<$> computeTypeName,
            ("RunningMode" Core..=) Prelude.<$> runningMode,
            ("RootVolumeSizeGib" Core..=)
              Prelude.<$> rootVolumeSizeGib,
            ("RunningModeAutoStopTimeoutInMinutes" Core..=)
              Prelude.<$> runningModeAutoStopTimeoutInMinutes,
            ("UserVolumeSizeGib" Core..=)
              Prelude.<$> userVolumeSizeGib
          ]
      )
