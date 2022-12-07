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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WorkSpaces.Types.WorkspaceProperties where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.WorkSpaces.Types.Compute
import Amazonka.WorkSpaces.Types.Protocol
import Amazonka.WorkSpaces.Types.RunningMode

-- | Describes a WorkSpace.
--
-- /See:/ 'newWorkspaceProperties' smart constructor.
data WorkspaceProperties = WorkspaceProperties'
  { -- | The size of the user storage. For important information about how to
    -- modify the size of the root and user volumes, see
    -- <https://docs.aws.amazon.com/workspaces/latest/adminguide/modify-workspaces.html Modify a WorkSpace>.
    userVolumeSizeGib :: Prelude.Maybe Prelude.Int,
    -- | The running mode. For more information, see
    -- <https://docs.aws.amazon.com/workspaces/latest/adminguide/running-mode.html Manage the WorkSpace Running Mode>.
    --
    -- The @MANUAL@ value is only supported by Amazon WorkSpaces Core. Contact
    -- your account team to be allow-listed to use this value. For more
    -- information, see
    -- <http://aws.amazon.com/workspaces/core/ Amazon WorkSpaces Core>.
    runningMode :: Prelude.Maybe RunningMode,
    -- | The protocol. For more information, see
    -- <https://docs.aws.amazon.com/workspaces/latest/adminguide/amazon-workspaces-protocols.html Protocols for Amazon WorkSpaces>.
    --
    -- -   Only available for WorkSpaces created with PCoIP bundles.
    --
    -- -   The @Protocols@ property is case sensitive. Ensure you use @PCOIP@
    --     or @WSP@.
    --
    -- -   Unavailable for Windows 7 WorkSpaces and WorkSpaces using GPU-based
    --     bundles (Graphics, GraphicsPro, Graphics.g4dn, and
    --     GraphicsPro.g4dn).
    protocols :: Prelude.Maybe [Protocol],
    -- | The compute type. For more information, see
    -- <http://aws.amazon.com/workspaces/details/#Amazon_WorkSpaces_Bundles Amazon WorkSpaces Bundles>.
    computeTypeName :: Prelude.Maybe Compute,
    -- | The size of the root volume. For important information about how to
    -- modify the size of the root and user volumes, see
    -- <https://docs.aws.amazon.com/workspaces/latest/adminguide/modify-workspaces.html Modify a WorkSpace>.
    rootVolumeSizeGib :: Prelude.Maybe Prelude.Int,
    -- | The time after a user logs off when WorkSpaces are automatically
    -- stopped. Configured in 60-minute intervals.
    runningModeAutoStopTimeoutInMinutes :: Prelude.Maybe Prelude.Int
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
-- 'userVolumeSizeGib', 'workspaceProperties_userVolumeSizeGib' - The size of the user storage. For important information about how to
-- modify the size of the root and user volumes, see
-- <https://docs.aws.amazon.com/workspaces/latest/adminguide/modify-workspaces.html Modify a WorkSpace>.
--
-- 'runningMode', 'workspaceProperties_runningMode' - The running mode. For more information, see
-- <https://docs.aws.amazon.com/workspaces/latest/adminguide/running-mode.html Manage the WorkSpace Running Mode>.
--
-- The @MANUAL@ value is only supported by Amazon WorkSpaces Core. Contact
-- your account team to be allow-listed to use this value. For more
-- information, see
-- <http://aws.amazon.com/workspaces/core/ Amazon WorkSpaces Core>.
--
-- 'protocols', 'workspaceProperties_protocols' - The protocol. For more information, see
-- <https://docs.aws.amazon.com/workspaces/latest/adminguide/amazon-workspaces-protocols.html Protocols for Amazon WorkSpaces>.
--
-- -   Only available for WorkSpaces created with PCoIP bundles.
--
-- -   The @Protocols@ property is case sensitive. Ensure you use @PCOIP@
--     or @WSP@.
--
-- -   Unavailable for Windows 7 WorkSpaces and WorkSpaces using GPU-based
--     bundles (Graphics, GraphicsPro, Graphics.g4dn, and
--     GraphicsPro.g4dn).
--
-- 'computeTypeName', 'workspaceProperties_computeTypeName' - The compute type. For more information, see
-- <http://aws.amazon.com/workspaces/details/#Amazon_WorkSpaces_Bundles Amazon WorkSpaces Bundles>.
--
-- 'rootVolumeSizeGib', 'workspaceProperties_rootVolumeSizeGib' - The size of the root volume. For important information about how to
-- modify the size of the root and user volumes, see
-- <https://docs.aws.amazon.com/workspaces/latest/adminguide/modify-workspaces.html Modify a WorkSpace>.
--
-- 'runningModeAutoStopTimeoutInMinutes', 'workspaceProperties_runningModeAutoStopTimeoutInMinutes' - The time after a user logs off when WorkSpaces are automatically
-- stopped. Configured in 60-minute intervals.
newWorkspaceProperties ::
  WorkspaceProperties
newWorkspaceProperties =
  WorkspaceProperties'
    { userVolumeSizeGib =
        Prelude.Nothing,
      runningMode = Prelude.Nothing,
      protocols = Prelude.Nothing,
      computeTypeName = Prelude.Nothing,
      rootVolumeSizeGib = Prelude.Nothing,
      runningModeAutoStopTimeoutInMinutes =
        Prelude.Nothing
    }

-- | The size of the user storage. For important information about how to
-- modify the size of the root and user volumes, see
-- <https://docs.aws.amazon.com/workspaces/latest/adminguide/modify-workspaces.html Modify a WorkSpace>.
workspaceProperties_userVolumeSizeGib :: Lens.Lens' WorkspaceProperties (Prelude.Maybe Prelude.Int)
workspaceProperties_userVolumeSizeGib = Lens.lens (\WorkspaceProperties' {userVolumeSizeGib} -> userVolumeSizeGib) (\s@WorkspaceProperties' {} a -> s {userVolumeSizeGib = a} :: WorkspaceProperties)

-- | The running mode. For more information, see
-- <https://docs.aws.amazon.com/workspaces/latest/adminguide/running-mode.html Manage the WorkSpace Running Mode>.
--
-- The @MANUAL@ value is only supported by Amazon WorkSpaces Core. Contact
-- your account team to be allow-listed to use this value. For more
-- information, see
-- <http://aws.amazon.com/workspaces/core/ Amazon WorkSpaces Core>.
workspaceProperties_runningMode :: Lens.Lens' WorkspaceProperties (Prelude.Maybe RunningMode)
workspaceProperties_runningMode = Lens.lens (\WorkspaceProperties' {runningMode} -> runningMode) (\s@WorkspaceProperties' {} a -> s {runningMode = a} :: WorkspaceProperties)

-- | The protocol. For more information, see
-- <https://docs.aws.amazon.com/workspaces/latest/adminguide/amazon-workspaces-protocols.html Protocols for Amazon WorkSpaces>.
--
-- -   Only available for WorkSpaces created with PCoIP bundles.
--
-- -   The @Protocols@ property is case sensitive. Ensure you use @PCOIP@
--     or @WSP@.
--
-- -   Unavailable for Windows 7 WorkSpaces and WorkSpaces using GPU-based
--     bundles (Graphics, GraphicsPro, Graphics.g4dn, and
--     GraphicsPro.g4dn).
workspaceProperties_protocols :: Lens.Lens' WorkspaceProperties (Prelude.Maybe [Protocol])
workspaceProperties_protocols = Lens.lens (\WorkspaceProperties' {protocols} -> protocols) (\s@WorkspaceProperties' {} a -> s {protocols = a} :: WorkspaceProperties) Prelude.. Lens.mapping Lens.coerced

-- | The compute type. For more information, see
-- <http://aws.amazon.com/workspaces/details/#Amazon_WorkSpaces_Bundles Amazon WorkSpaces Bundles>.
workspaceProperties_computeTypeName :: Lens.Lens' WorkspaceProperties (Prelude.Maybe Compute)
workspaceProperties_computeTypeName = Lens.lens (\WorkspaceProperties' {computeTypeName} -> computeTypeName) (\s@WorkspaceProperties' {} a -> s {computeTypeName = a} :: WorkspaceProperties)

-- | The size of the root volume. For important information about how to
-- modify the size of the root and user volumes, see
-- <https://docs.aws.amazon.com/workspaces/latest/adminguide/modify-workspaces.html Modify a WorkSpace>.
workspaceProperties_rootVolumeSizeGib :: Lens.Lens' WorkspaceProperties (Prelude.Maybe Prelude.Int)
workspaceProperties_rootVolumeSizeGib = Lens.lens (\WorkspaceProperties' {rootVolumeSizeGib} -> rootVolumeSizeGib) (\s@WorkspaceProperties' {} a -> s {rootVolumeSizeGib = a} :: WorkspaceProperties)

-- | The time after a user logs off when WorkSpaces are automatically
-- stopped. Configured in 60-minute intervals.
workspaceProperties_runningModeAutoStopTimeoutInMinutes :: Lens.Lens' WorkspaceProperties (Prelude.Maybe Prelude.Int)
workspaceProperties_runningModeAutoStopTimeoutInMinutes = Lens.lens (\WorkspaceProperties' {runningModeAutoStopTimeoutInMinutes} -> runningModeAutoStopTimeoutInMinutes) (\s@WorkspaceProperties' {} a -> s {runningModeAutoStopTimeoutInMinutes = a} :: WorkspaceProperties)

instance Data.FromJSON WorkspaceProperties where
  parseJSON =
    Data.withObject
      "WorkspaceProperties"
      ( \x ->
          WorkspaceProperties'
            Prelude.<$> (x Data..:? "UserVolumeSizeGib")
            Prelude.<*> (x Data..:? "RunningMode")
            Prelude.<*> (x Data..:? "Protocols" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "ComputeTypeName")
            Prelude.<*> (x Data..:? "RootVolumeSizeGib")
            Prelude.<*> (x Data..:? "RunningModeAutoStopTimeoutInMinutes")
      )

instance Prelude.Hashable WorkspaceProperties where
  hashWithSalt _salt WorkspaceProperties' {..} =
    _salt `Prelude.hashWithSalt` userVolumeSizeGib
      `Prelude.hashWithSalt` runningMode
      `Prelude.hashWithSalt` protocols
      `Prelude.hashWithSalt` computeTypeName
      `Prelude.hashWithSalt` rootVolumeSizeGib
      `Prelude.hashWithSalt` runningModeAutoStopTimeoutInMinutes

instance Prelude.NFData WorkspaceProperties where
  rnf WorkspaceProperties' {..} =
    Prelude.rnf userVolumeSizeGib
      `Prelude.seq` Prelude.rnf runningMode
      `Prelude.seq` Prelude.rnf protocols
      `Prelude.seq` Prelude.rnf computeTypeName
      `Prelude.seq` Prelude.rnf rootVolumeSizeGib
      `Prelude.seq` Prelude.rnf runningModeAutoStopTimeoutInMinutes

instance Data.ToJSON WorkspaceProperties where
  toJSON WorkspaceProperties' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("UserVolumeSizeGib" Data..=)
              Prelude.<$> userVolumeSizeGib,
            ("RunningMode" Data..=) Prelude.<$> runningMode,
            ("Protocols" Data..=) Prelude.<$> protocols,
            ("ComputeTypeName" Data..=)
              Prelude.<$> computeTypeName,
            ("RootVolumeSizeGib" Data..=)
              Prelude.<$> rootVolumeSizeGib,
            ("RunningModeAutoStopTimeoutInMinutes" Data..=)
              Prelude.<$> runningModeAutoStopTimeoutInMinutes
          ]
      )
