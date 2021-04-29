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
-- Module      : Network.AWS.WorkSpaces.Types.Workspace
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkSpaces.Types.Workspace where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.WorkSpaces.Types.ModificationState
import Network.AWS.WorkSpaces.Types.WorkspaceProperties
import Network.AWS.WorkSpaces.Types.WorkspaceState

-- | Describes a WorkSpace.
--
-- /See:/ 'newWorkspace' smart constructor.
data Workspace = Workspace'
  { -- | The properties of the WorkSpace.
    workspaceProperties :: Prelude.Maybe WorkspaceProperties,
    -- | Indicates whether the data stored on the root volume is encrypted.
    rootVolumeEncryptionEnabled :: Prelude.Maybe Prelude.Bool,
    -- | The identifier of the bundle used to create the WorkSpace.
    bundleId :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether the data stored on the user volume is encrypted.
    userVolumeEncryptionEnabled :: Prelude.Maybe Prelude.Bool,
    -- | The symmetric AWS KMS customer master key (CMK) used to encrypt data
    -- stored on your WorkSpace. Amazon WorkSpaces does not support asymmetric
    -- CMKs.
    volumeEncryptionKey :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the WorkSpace.
    workspaceId :: Prelude.Maybe Prelude.Text,
    -- | The modification states of the WorkSpace.
    modificationStates :: Prelude.Maybe [ModificationState],
    -- | The IP address of the WorkSpace.
    ipAddress :: Prelude.Maybe Prelude.Text,
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
    -- | The identifier of the AWS Directory Service directory for the WorkSpace.
    directoryId :: Prelude.Maybe Prelude.Text,
    -- | The user for the WorkSpace.
    userName :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the subnet for the WorkSpace.
    subnetId :: Prelude.Maybe Prelude.Text,
    -- | The text of the error message that is returned if the WorkSpace cannot
    -- be created.
    errorMessage :: Prelude.Maybe Prelude.Text,
    -- | The name of the WorkSpace, as seen by the operating system. The format
    -- of this name varies. For more information, see
    -- <https://docs.aws.amazon.com/workspaces/latest/adminguide/launch-workspaces-tutorials.html Launch a WorkSpace>.
    computerName :: Prelude.Maybe Prelude.Text,
    -- | The error code that is returned if the WorkSpace cannot be created.
    errorCode :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
-- 'rootVolumeEncryptionEnabled', 'workspace_rootVolumeEncryptionEnabled' - Indicates whether the data stored on the root volume is encrypted.
--
-- 'bundleId', 'workspace_bundleId' - The identifier of the bundle used to create the WorkSpace.
--
-- 'userVolumeEncryptionEnabled', 'workspace_userVolumeEncryptionEnabled' - Indicates whether the data stored on the user volume is encrypted.
--
-- 'volumeEncryptionKey', 'workspace_volumeEncryptionKey' - The symmetric AWS KMS customer master key (CMK) used to encrypt data
-- stored on your WorkSpace. Amazon WorkSpaces does not support asymmetric
-- CMKs.
--
-- 'workspaceId', 'workspace_workspaceId' - The identifier of the WorkSpace.
--
-- 'modificationStates', 'workspace_modificationStates' - The modification states of the WorkSpace.
--
-- 'ipAddress', 'workspace_ipAddress' - The IP address of the WorkSpace.
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
-- 'directoryId', 'workspace_directoryId' - The identifier of the AWS Directory Service directory for the WorkSpace.
--
-- 'userName', 'workspace_userName' - The user for the WorkSpace.
--
-- 'subnetId', 'workspace_subnetId' - The identifier of the subnet for the WorkSpace.
--
-- 'errorMessage', 'workspace_errorMessage' - The text of the error message that is returned if the WorkSpace cannot
-- be created.
--
-- 'computerName', 'workspace_computerName' - The name of the WorkSpace, as seen by the operating system. The format
-- of this name varies. For more information, see
-- <https://docs.aws.amazon.com/workspaces/latest/adminguide/launch-workspaces-tutorials.html Launch a WorkSpace>.
--
-- 'errorCode', 'workspace_errorCode' - The error code that is returned if the WorkSpace cannot be created.
newWorkspace ::
  Workspace
newWorkspace =
  Workspace'
    { workspaceProperties = Prelude.Nothing,
      rootVolumeEncryptionEnabled = Prelude.Nothing,
      bundleId = Prelude.Nothing,
      userVolumeEncryptionEnabled = Prelude.Nothing,
      volumeEncryptionKey = Prelude.Nothing,
      workspaceId = Prelude.Nothing,
      modificationStates = Prelude.Nothing,
      ipAddress = Prelude.Nothing,
      state = Prelude.Nothing,
      directoryId = Prelude.Nothing,
      userName = Prelude.Nothing,
      subnetId = Prelude.Nothing,
      errorMessage = Prelude.Nothing,
      computerName = Prelude.Nothing,
      errorCode = Prelude.Nothing
    }

-- | The properties of the WorkSpace.
workspace_workspaceProperties :: Lens.Lens' Workspace (Prelude.Maybe WorkspaceProperties)
workspace_workspaceProperties = Lens.lens (\Workspace' {workspaceProperties} -> workspaceProperties) (\s@Workspace' {} a -> s {workspaceProperties = a} :: Workspace)

-- | Indicates whether the data stored on the root volume is encrypted.
workspace_rootVolumeEncryptionEnabled :: Lens.Lens' Workspace (Prelude.Maybe Prelude.Bool)
workspace_rootVolumeEncryptionEnabled = Lens.lens (\Workspace' {rootVolumeEncryptionEnabled} -> rootVolumeEncryptionEnabled) (\s@Workspace' {} a -> s {rootVolumeEncryptionEnabled = a} :: Workspace)

-- | The identifier of the bundle used to create the WorkSpace.
workspace_bundleId :: Lens.Lens' Workspace (Prelude.Maybe Prelude.Text)
workspace_bundleId = Lens.lens (\Workspace' {bundleId} -> bundleId) (\s@Workspace' {} a -> s {bundleId = a} :: Workspace)

-- | Indicates whether the data stored on the user volume is encrypted.
workspace_userVolumeEncryptionEnabled :: Lens.Lens' Workspace (Prelude.Maybe Prelude.Bool)
workspace_userVolumeEncryptionEnabled = Lens.lens (\Workspace' {userVolumeEncryptionEnabled} -> userVolumeEncryptionEnabled) (\s@Workspace' {} a -> s {userVolumeEncryptionEnabled = a} :: Workspace)

-- | The symmetric AWS KMS customer master key (CMK) used to encrypt data
-- stored on your WorkSpace. Amazon WorkSpaces does not support asymmetric
-- CMKs.
workspace_volumeEncryptionKey :: Lens.Lens' Workspace (Prelude.Maybe Prelude.Text)
workspace_volumeEncryptionKey = Lens.lens (\Workspace' {volumeEncryptionKey} -> volumeEncryptionKey) (\s@Workspace' {} a -> s {volumeEncryptionKey = a} :: Workspace)

-- | The identifier of the WorkSpace.
workspace_workspaceId :: Lens.Lens' Workspace (Prelude.Maybe Prelude.Text)
workspace_workspaceId = Lens.lens (\Workspace' {workspaceId} -> workspaceId) (\s@Workspace' {} a -> s {workspaceId = a} :: Workspace)

-- | The modification states of the WorkSpace.
workspace_modificationStates :: Lens.Lens' Workspace (Prelude.Maybe [ModificationState])
workspace_modificationStates = Lens.lens (\Workspace' {modificationStates} -> modificationStates) (\s@Workspace' {} a -> s {modificationStates = a} :: Workspace) Prelude.. Lens.mapping Prelude._Coerce

-- | The IP address of the WorkSpace.
workspace_ipAddress :: Lens.Lens' Workspace (Prelude.Maybe Prelude.Text)
workspace_ipAddress = Lens.lens (\Workspace' {ipAddress} -> ipAddress) (\s@Workspace' {} a -> s {ipAddress = a} :: Workspace)

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

-- | The identifier of the AWS Directory Service directory for the WorkSpace.
workspace_directoryId :: Lens.Lens' Workspace (Prelude.Maybe Prelude.Text)
workspace_directoryId = Lens.lens (\Workspace' {directoryId} -> directoryId) (\s@Workspace' {} a -> s {directoryId = a} :: Workspace)

-- | The user for the WorkSpace.
workspace_userName :: Lens.Lens' Workspace (Prelude.Maybe Prelude.Text)
workspace_userName = Lens.lens (\Workspace' {userName} -> userName) (\s@Workspace' {} a -> s {userName = a} :: Workspace)

-- | The identifier of the subnet for the WorkSpace.
workspace_subnetId :: Lens.Lens' Workspace (Prelude.Maybe Prelude.Text)
workspace_subnetId = Lens.lens (\Workspace' {subnetId} -> subnetId) (\s@Workspace' {} a -> s {subnetId = a} :: Workspace)

-- | The text of the error message that is returned if the WorkSpace cannot
-- be created.
workspace_errorMessage :: Lens.Lens' Workspace (Prelude.Maybe Prelude.Text)
workspace_errorMessage = Lens.lens (\Workspace' {errorMessage} -> errorMessage) (\s@Workspace' {} a -> s {errorMessage = a} :: Workspace)

-- | The name of the WorkSpace, as seen by the operating system. The format
-- of this name varies. For more information, see
-- <https://docs.aws.amazon.com/workspaces/latest/adminguide/launch-workspaces-tutorials.html Launch a WorkSpace>.
workspace_computerName :: Lens.Lens' Workspace (Prelude.Maybe Prelude.Text)
workspace_computerName = Lens.lens (\Workspace' {computerName} -> computerName) (\s@Workspace' {} a -> s {computerName = a} :: Workspace)

-- | The error code that is returned if the WorkSpace cannot be created.
workspace_errorCode :: Lens.Lens' Workspace (Prelude.Maybe Prelude.Text)
workspace_errorCode = Lens.lens (\Workspace' {errorCode} -> errorCode) (\s@Workspace' {} a -> s {errorCode = a} :: Workspace)

instance Prelude.FromJSON Workspace where
  parseJSON =
    Prelude.withObject
      "Workspace"
      ( \x ->
          Workspace'
            Prelude.<$> (x Prelude..:? "WorkspaceProperties")
            Prelude.<*> (x Prelude..:? "RootVolumeEncryptionEnabled")
            Prelude.<*> (x Prelude..:? "BundleId")
            Prelude.<*> (x Prelude..:? "UserVolumeEncryptionEnabled")
            Prelude.<*> (x Prelude..:? "VolumeEncryptionKey")
            Prelude.<*> (x Prelude..:? "WorkspaceId")
            Prelude.<*> ( x Prelude..:? "ModificationStates"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..:? "IpAddress")
            Prelude.<*> (x Prelude..:? "State")
            Prelude.<*> (x Prelude..:? "DirectoryId")
            Prelude.<*> (x Prelude..:? "UserName")
            Prelude.<*> (x Prelude..:? "SubnetId")
            Prelude.<*> (x Prelude..:? "ErrorMessage")
            Prelude.<*> (x Prelude..:? "ComputerName")
            Prelude.<*> (x Prelude..:? "ErrorCode")
      )

instance Prelude.Hashable Workspace

instance Prelude.NFData Workspace
