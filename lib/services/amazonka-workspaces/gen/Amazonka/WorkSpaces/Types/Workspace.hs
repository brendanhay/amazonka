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
-- Module      : Amazonka.WorkSpaces.Types.Workspace
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WorkSpaces.Types.Workspace where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.WorkSpaces.Types.ModificationState
import Amazonka.WorkSpaces.Types.RelatedWorkspaceProperties
import Amazonka.WorkSpaces.Types.WorkspaceProperties
import Amazonka.WorkSpaces.Types.WorkspaceState

-- | Describes a WorkSpace.
--
-- /See:/ 'newWorkspace' smart constructor.
data Workspace = Workspace'
  { -- | The properties of the WorkSpace.
    workspaceProperties :: Prelude.Maybe WorkspaceProperties,
    -- | The identifier of the Directory Service directory for the WorkSpace.
    directoryId :: Prelude.Maybe Prelude.Text,
    -- | The Standby WorkSpace or Primary WorkSpace related to the specified
    -- WorkSpace.
    relatedWorkspaces :: Prelude.Maybe [RelatedWorkspaceProperties],
    -- | The symmetric KMS key used to encrypt data stored on your WorkSpace.
    -- Amazon WorkSpaces does not support asymmetric KMS keys.
    volumeEncryptionKey :: Prelude.Maybe Prelude.Text,
    -- | The text of the error message that is returned if the WorkSpace cannot
    -- be created.
    errorMessage :: Prelude.Maybe Prelude.Text,
    -- | The user for the WorkSpace.
    userName :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the subnet for the WorkSpace.
    subnetId :: Prelude.Maybe Prelude.Text,
    -- | The name of the WorkSpace, as seen by the operating system. The format
    -- of this name varies. For more information, see
    -- <https://docs.aws.amazon.com/workspaces/latest/adminguide/launch-workspaces-tutorials.html Launch a WorkSpace>.
    computerName :: Prelude.Maybe Prelude.Text,
    -- | The operational state of the WorkSpace.
    --
    -- After a WorkSpace is terminated, the @TERMINATED@ state is returned only
    -- briefly before the WorkSpace directory metadata is cleaned up, so this
    -- state is rarely returned. To confirm that a WorkSpace is terminated,
    -- check for the WorkSpace ID by using
    -- <https://docs.aws.amazon.com/workspaces/latest/api/API_DescribeWorkspaces.html DescribeWorkSpaces>.
    -- If the WorkSpace ID isn\'t returned, then the WorkSpace has been
    -- successfully terminated.
    state :: Prelude.Maybe WorkspaceState,
    -- | The identifier of the WorkSpace.
    workspaceId :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether the data stored on the user volume is encrypted.
    userVolumeEncryptionEnabled :: Prelude.Maybe Prelude.Bool,
    -- | The identifier of the bundle used to create the WorkSpace.
    bundleId :: Prelude.Maybe Prelude.Text,
    -- | The error code that is returned if the WorkSpace cannot be created.
    errorCode :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether the data stored on the root volume is encrypted.
    rootVolumeEncryptionEnabled :: Prelude.Maybe Prelude.Bool,
    -- | The modification states of the WorkSpace.
    modificationStates :: Prelude.Maybe [ModificationState],
    -- | The IP address of the WorkSpace.
    ipAddress :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Workspace' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'workspaceProperties', 'workspace_workspaceProperties' - The properties of the WorkSpace.
--
-- 'directoryId', 'workspace_directoryId' - The identifier of the Directory Service directory for the WorkSpace.
--
-- 'relatedWorkspaces', 'workspace_relatedWorkspaces' - The Standby WorkSpace or Primary WorkSpace related to the specified
-- WorkSpace.
--
-- 'volumeEncryptionKey', 'workspace_volumeEncryptionKey' - The symmetric KMS key used to encrypt data stored on your WorkSpace.
-- Amazon WorkSpaces does not support asymmetric KMS keys.
--
-- 'errorMessage', 'workspace_errorMessage' - The text of the error message that is returned if the WorkSpace cannot
-- be created.
--
-- 'userName', 'workspace_userName' - The user for the WorkSpace.
--
-- 'subnetId', 'workspace_subnetId' - The identifier of the subnet for the WorkSpace.
--
-- 'computerName', 'workspace_computerName' - The name of the WorkSpace, as seen by the operating system. The format
-- of this name varies. For more information, see
-- <https://docs.aws.amazon.com/workspaces/latest/adminguide/launch-workspaces-tutorials.html Launch a WorkSpace>.
--
-- 'state', 'workspace_state' - The operational state of the WorkSpace.
--
-- After a WorkSpace is terminated, the @TERMINATED@ state is returned only
-- briefly before the WorkSpace directory metadata is cleaned up, so this
-- state is rarely returned. To confirm that a WorkSpace is terminated,
-- check for the WorkSpace ID by using
-- <https://docs.aws.amazon.com/workspaces/latest/api/API_DescribeWorkspaces.html DescribeWorkSpaces>.
-- If the WorkSpace ID isn\'t returned, then the WorkSpace has been
-- successfully terminated.
--
-- 'workspaceId', 'workspace_workspaceId' - The identifier of the WorkSpace.
--
-- 'userVolumeEncryptionEnabled', 'workspace_userVolumeEncryptionEnabled' - Indicates whether the data stored on the user volume is encrypted.
--
-- 'bundleId', 'workspace_bundleId' - The identifier of the bundle used to create the WorkSpace.
--
-- 'errorCode', 'workspace_errorCode' - The error code that is returned if the WorkSpace cannot be created.
--
-- 'rootVolumeEncryptionEnabled', 'workspace_rootVolumeEncryptionEnabled' - Indicates whether the data stored on the root volume is encrypted.
--
-- 'modificationStates', 'workspace_modificationStates' - The modification states of the WorkSpace.
--
-- 'ipAddress', 'workspace_ipAddress' - The IP address of the WorkSpace.
newWorkspace ::
  Workspace
newWorkspace =
  Workspace'
    { workspaceProperties = Prelude.Nothing,
      directoryId = Prelude.Nothing,
      relatedWorkspaces = Prelude.Nothing,
      volumeEncryptionKey = Prelude.Nothing,
      errorMessage = Prelude.Nothing,
      userName = Prelude.Nothing,
      subnetId = Prelude.Nothing,
      computerName = Prelude.Nothing,
      state = Prelude.Nothing,
      workspaceId = Prelude.Nothing,
      userVolumeEncryptionEnabled = Prelude.Nothing,
      bundleId = Prelude.Nothing,
      errorCode = Prelude.Nothing,
      rootVolumeEncryptionEnabled = Prelude.Nothing,
      modificationStates = Prelude.Nothing,
      ipAddress = Prelude.Nothing
    }

-- | The properties of the WorkSpace.
workspace_workspaceProperties :: Lens.Lens' Workspace (Prelude.Maybe WorkspaceProperties)
workspace_workspaceProperties = Lens.lens (\Workspace' {workspaceProperties} -> workspaceProperties) (\s@Workspace' {} a -> s {workspaceProperties = a} :: Workspace)

-- | The identifier of the Directory Service directory for the WorkSpace.
workspace_directoryId :: Lens.Lens' Workspace (Prelude.Maybe Prelude.Text)
workspace_directoryId = Lens.lens (\Workspace' {directoryId} -> directoryId) (\s@Workspace' {} a -> s {directoryId = a} :: Workspace)

-- | The Standby WorkSpace or Primary WorkSpace related to the specified
-- WorkSpace.
workspace_relatedWorkspaces :: Lens.Lens' Workspace (Prelude.Maybe [RelatedWorkspaceProperties])
workspace_relatedWorkspaces = Lens.lens (\Workspace' {relatedWorkspaces} -> relatedWorkspaces) (\s@Workspace' {} a -> s {relatedWorkspaces = a} :: Workspace) Prelude.. Lens.mapping Lens.coerced

-- | The symmetric KMS key used to encrypt data stored on your WorkSpace.
-- Amazon WorkSpaces does not support asymmetric KMS keys.
workspace_volumeEncryptionKey :: Lens.Lens' Workspace (Prelude.Maybe Prelude.Text)
workspace_volumeEncryptionKey = Lens.lens (\Workspace' {volumeEncryptionKey} -> volumeEncryptionKey) (\s@Workspace' {} a -> s {volumeEncryptionKey = a} :: Workspace)

-- | The text of the error message that is returned if the WorkSpace cannot
-- be created.
workspace_errorMessage :: Lens.Lens' Workspace (Prelude.Maybe Prelude.Text)
workspace_errorMessage = Lens.lens (\Workspace' {errorMessage} -> errorMessage) (\s@Workspace' {} a -> s {errorMessage = a} :: Workspace)

-- | The user for the WorkSpace.
workspace_userName :: Lens.Lens' Workspace (Prelude.Maybe Prelude.Text)
workspace_userName = Lens.lens (\Workspace' {userName} -> userName) (\s@Workspace' {} a -> s {userName = a} :: Workspace)

-- | The identifier of the subnet for the WorkSpace.
workspace_subnetId :: Lens.Lens' Workspace (Prelude.Maybe Prelude.Text)
workspace_subnetId = Lens.lens (\Workspace' {subnetId} -> subnetId) (\s@Workspace' {} a -> s {subnetId = a} :: Workspace)

-- | The name of the WorkSpace, as seen by the operating system. The format
-- of this name varies. For more information, see
-- <https://docs.aws.amazon.com/workspaces/latest/adminguide/launch-workspaces-tutorials.html Launch a WorkSpace>.
workspace_computerName :: Lens.Lens' Workspace (Prelude.Maybe Prelude.Text)
workspace_computerName = Lens.lens (\Workspace' {computerName} -> computerName) (\s@Workspace' {} a -> s {computerName = a} :: Workspace)

-- | The operational state of the WorkSpace.
--
-- After a WorkSpace is terminated, the @TERMINATED@ state is returned only
-- briefly before the WorkSpace directory metadata is cleaned up, so this
-- state is rarely returned. To confirm that a WorkSpace is terminated,
-- check for the WorkSpace ID by using
-- <https://docs.aws.amazon.com/workspaces/latest/api/API_DescribeWorkspaces.html DescribeWorkSpaces>.
-- If the WorkSpace ID isn\'t returned, then the WorkSpace has been
-- successfully terminated.
workspace_state :: Lens.Lens' Workspace (Prelude.Maybe WorkspaceState)
workspace_state = Lens.lens (\Workspace' {state} -> state) (\s@Workspace' {} a -> s {state = a} :: Workspace)

-- | The identifier of the WorkSpace.
workspace_workspaceId :: Lens.Lens' Workspace (Prelude.Maybe Prelude.Text)
workspace_workspaceId = Lens.lens (\Workspace' {workspaceId} -> workspaceId) (\s@Workspace' {} a -> s {workspaceId = a} :: Workspace)

-- | Indicates whether the data stored on the user volume is encrypted.
workspace_userVolumeEncryptionEnabled :: Lens.Lens' Workspace (Prelude.Maybe Prelude.Bool)
workspace_userVolumeEncryptionEnabled = Lens.lens (\Workspace' {userVolumeEncryptionEnabled} -> userVolumeEncryptionEnabled) (\s@Workspace' {} a -> s {userVolumeEncryptionEnabled = a} :: Workspace)

-- | The identifier of the bundle used to create the WorkSpace.
workspace_bundleId :: Lens.Lens' Workspace (Prelude.Maybe Prelude.Text)
workspace_bundleId = Lens.lens (\Workspace' {bundleId} -> bundleId) (\s@Workspace' {} a -> s {bundleId = a} :: Workspace)

-- | The error code that is returned if the WorkSpace cannot be created.
workspace_errorCode :: Lens.Lens' Workspace (Prelude.Maybe Prelude.Text)
workspace_errorCode = Lens.lens (\Workspace' {errorCode} -> errorCode) (\s@Workspace' {} a -> s {errorCode = a} :: Workspace)

-- | Indicates whether the data stored on the root volume is encrypted.
workspace_rootVolumeEncryptionEnabled :: Lens.Lens' Workspace (Prelude.Maybe Prelude.Bool)
workspace_rootVolumeEncryptionEnabled = Lens.lens (\Workspace' {rootVolumeEncryptionEnabled} -> rootVolumeEncryptionEnabled) (\s@Workspace' {} a -> s {rootVolumeEncryptionEnabled = a} :: Workspace)

-- | The modification states of the WorkSpace.
workspace_modificationStates :: Lens.Lens' Workspace (Prelude.Maybe [ModificationState])
workspace_modificationStates = Lens.lens (\Workspace' {modificationStates} -> modificationStates) (\s@Workspace' {} a -> s {modificationStates = a} :: Workspace) Prelude.. Lens.mapping Lens.coerced

-- | The IP address of the WorkSpace.
workspace_ipAddress :: Lens.Lens' Workspace (Prelude.Maybe Prelude.Text)
workspace_ipAddress = Lens.lens (\Workspace' {ipAddress} -> ipAddress) (\s@Workspace' {} a -> s {ipAddress = a} :: Workspace)

instance Data.FromJSON Workspace where
  parseJSON =
    Data.withObject
      "Workspace"
      ( \x ->
          Workspace'
            Prelude.<$> (x Data..:? "WorkspaceProperties")
            Prelude.<*> (x Data..:? "DirectoryId")
            Prelude.<*> ( x Data..:? "RelatedWorkspaces"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "VolumeEncryptionKey")
            Prelude.<*> (x Data..:? "ErrorMessage")
            Prelude.<*> (x Data..:? "UserName")
            Prelude.<*> (x Data..:? "SubnetId")
            Prelude.<*> (x Data..:? "ComputerName")
            Prelude.<*> (x Data..:? "State")
            Prelude.<*> (x Data..:? "WorkspaceId")
            Prelude.<*> (x Data..:? "UserVolumeEncryptionEnabled")
            Prelude.<*> (x Data..:? "BundleId")
            Prelude.<*> (x Data..:? "ErrorCode")
            Prelude.<*> (x Data..:? "RootVolumeEncryptionEnabled")
            Prelude.<*> ( x Data..:? "ModificationStates"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "IpAddress")
      )

instance Prelude.Hashable Workspace where
  hashWithSalt _salt Workspace' {..} =
    _salt `Prelude.hashWithSalt` workspaceProperties
      `Prelude.hashWithSalt` directoryId
      `Prelude.hashWithSalt` relatedWorkspaces
      `Prelude.hashWithSalt` volumeEncryptionKey
      `Prelude.hashWithSalt` errorMessage
      `Prelude.hashWithSalt` userName
      `Prelude.hashWithSalt` subnetId
      `Prelude.hashWithSalt` computerName
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` workspaceId
      `Prelude.hashWithSalt` userVolumeEncryptionEnabled
      `Prelude.hashWithSalt` bundleId
      `Prelude.hashWithSalt` errorCode
      `Prelude.hashWithSalt` rootVolumeEncryptionEnabled
      `Prelude.hashWithSalt` modificationStates
      `Prelude.hashWithSalt` ipAddress

instance Prelude.NFData Workspace where
  rnf Workspace' {..} =
    Prelude.rnf workspaceProperties
      `Prelude.seq` Prelude.rnf directoryId
      `Prelude.seq` Prelude.rnf relatedWorkspaces
      `Prelude.seq` Prelude.rnf volumeEncryptionKey
      `Prelude.seq` Prelude.rnf errorMessage
      `Prelude.seq` Prelude.rnf userName
      `Prelude.seq` Prelude.rnf subnetId
      `Prelude.seq` Prelude.rnf computerName
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf workspaceId
      `Prelude.seq` Prelude.rnf userVolumeEncryptionEnabled
      `Prelude.seq` Prelude.rnf bundleId
      `Prelude.seq` Prelude.rnf errorCode
      `Prelude.seq` Prelude.rnf rootVolumeEncryptionEnabled
      `Prelude.seq` Prelude.rnf modificationStates
      `Prelude.seq` Prelude.rnf ipAddress
