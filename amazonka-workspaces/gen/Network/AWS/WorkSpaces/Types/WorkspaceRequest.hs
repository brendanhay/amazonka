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
-- Module      : Network.AWS.WorkSpaces.Types.WorkspaceRequest
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkSpaces.Types.WorkspaceRequest where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.WorkSpaces.Types.Tag
import Network.AWS.WorkSpaces.Types.WorkspaceProperties

-- | Describes the information used to create a WorkSpace.
--
-- /See:/ 'newWorkspaceRequest' smart constructor.
data WorkspaceRequest = WorkspaceRequest'
  { -- | The WorkSpace properties.
    workspaceProperties :: Prelude.Maybe WorkspaceProperties,
    -- | Indicates whether the data stored on the root volume is encrypted.
    rootVolumeEncryptionEnabled :: Prelude.Maybe Prelude.Bool,
    -- | Indicates whether the data stored on the user volume is encrypted.
    userVolumeEncryptionEnabled :: Prelude.Maybe Prelude.Bool,
    -- | The symmetric AWS KMS customer master key (CMK) used to encrypt data
    -- stored on your WorkSpace. Amazon WorkSpaces does not support asymmetric
    -- CMKs.
    volumeEncryptionKey :: Prelude.Maybe Prelude.Text,
    -- | The tags for the WorkSpace.
    tags :: Prelude.Maybe [Tag],
    -- | The identifier of the AWS Directory Service directory for the WorkSpace.
    -- You can use DescribeWorkspaceDirectories to list the available
    -- directories.
    directoryId :: Prelude.Text,
    -- | The user name of the user for the WorkSpace. This user name must exist
    -- in the AWS Directory Service directory for the WorkSpace.
    userName :: Prelude.Text,
    -- | The identifier of the bundle for the WorkSpace. You can use
    -- DescribeWorkspaceBundles to list the available bundles.
    bundleId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'WorkspaceRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'workspaceProperties', 'workspaceRequest_workspaceProperties' - The WorkSpace properties.
--
-- 'rootVolumeEncryptionEnabled', 'workspaceRequest_rootVolumeEncryptionEnabled' - Indicates whether the data stored on the root volume is encrypted.
--
-- 'userVolumeEncryptionEnabled', 'workspaceRequest_userVolumeEncryptionEnabled' - Indicates whether the data stored on the user volume is encrypted.
--
-- 'volumeEncryptionKey', 'workspaceRequest_volumeEncryptionKey' - The symmetric AWS KMS customer master key (CMK) used to encrypt data
-- stored on your WorkSpace. Amazon WorkSpaces does not support asymmetric
-- CMKs.
--
-- 'tags', 'workspaceRequest_tags' - The tags for the WorkSpace.
--
-- 'directoryId', 'workspaceRequest_directoryId' - The identifier of the AWS Directory Service directory for the WorkSpace.
-- You can use DescribeWorkspaceDirectories to list the available
-- directories.
--
-- 'userName', 'workspaceRequest_userName' - The user name of the user for the WorkSpace. This user name must exist
-- in the AWS Directory Service directory for the WorkSpace.
--
-- 'bundleId', 'workspaceRequest_bundleId' - The identifier of the bundle for the WorkSpace. You can use
-- DescribeWorkspaceBundles to list the available bundles.
newWorkspaceRequest ::
  -- | 'directoryId'
  Prelude.Text ->
  -- | 'userName'
  Prelude.Text ->
  -- | 'bundleId'
  Prelude.Text ->
  WorkspaceRequest
newWorkspaceRequest
  pDirectoryId_
  pUserName_
  pBundleId_ =
    WorkspaceRequest'
      { workspaceProperties =
          Prelude.Nothing,
        rootVolumeEncryptionEnabled = Prelude.Nothing,
        userVolumeEncryptionEnabled = Prelude.Nothing,
        volumeEncryptionKey = Prelude.Nothing,
        tags = Prelude.Nothing,
        directoryId = pDirectoryId_,
        userName = pUserName_,
        bundleId = pBundleId_
      }

-- | The WorkSpace properties.
workspaceRequest_workspaceProperties :: Lens.Lens' WorkspaceRequest (Prelude.Maybe WorkspaceProperties)
workspaceRequest_workspaceProperties = Lens.lens (\WorkspaceRequest' {workspaceProperties} -> workspaceProperties) (\s@WorkspaceRequest' {} a -> s {workspaceProperties = a} :: WorkspaceRequest)

-- | Indicates whether the data stored on the root volume is encrypted.
workspaceRequest_rootVolumeEncryptionEnabled :: Lens.Lens' WorkspaceRequest (Prelude.Maybe Prelude.Bool)
workspaceRequest_rootVolumeEncryptionEnabled = Lens.lens (\WorkspaceRequest' {rootVolumeEncryptionEnabled} -> rootVolumeEncryptionEnabled) (\s@WorkspaceRequest' {} a -> s {rootVolumeEncryptionEnabled = a} :: WorkspaceRequest)

-- | Indicates whether the data stored on the user volume is encrypted.
workspaceRequest_userVolumeEncryptionEnabled :: Lens.Lens' WorkspaceRequest (Prelude.Maybe Prelude.Bool)
workspaceRequest_userVolumeEncryptionEnabled = Lens.lens (\WorkspaceRequest' {userVolumeEncryptionEnabled} -> userVolumeEncryptionEnabled) (\s@WorkspaceRequest' {} a -> s {userVolumeEncryptionEnabled = a} :: WorkspaceRequest)

-- | The symmetric AWS KMS customer master key (CMK) used to encrypt data
-- stored on your WorkSpace. Amazon WorkSpaces does not support asymmetric
-- CMKs.
workspaceRequest_volumeEncryptionKey :: Lens.Lens' WorkspaceRequest (Prelude.Maybe Prelude.Text)
workspaceRequest_volumeEncryptionKey = Lens.lens (\WorkspaceRequest' {volumeEncryptionKey} -> volumeEncryptionKey) (\s@WorkspaceRequest' {} a -> s {volumeEncryptionKey = a} :: WorkspaceRequest)

-- | The tags for the WorkSpace.
workspaceRequest_tags :: Lens.Lens' WorkspaceRequest (Prelude.Maybe [Tag])
workspaceRequest_tags = Lens.lens (\WorkspaceRequest' {tags} -> tags) (\s@WorkspaceRequest' {} a -> s {tags = a} :: WorkspaceRequest) Prelude.. Lens.mapping Prelude._Coerce

-- | The identifier of the AWS Directory Service directory for the WorkSpace.
-- You can use DescribeWorkspaceDirectories to list the available
-- directories.
workspaceRequest_directoryId :: Lens.Lens' WorkspaceRequest Prelude.Text
workspaceRequest_directoryId = Lens.lens (\WorkspaceRequest' {directoryId} -> directoryId) (\s@WorkspaceRequest' {} a -> s {directoryId = a} :: WorkspaceRequest)

-- | The user name of the user for the WorkSpace. This user name must exist
-- in the AWS Directory Service directory for the WorkSpace.
workspaceRequest_userName :: Lens.Lens' WorkspaceRequest Prelude.Text
workspaceRequest_userName = Lens.lens (\WorkspaceRequest' {userName} -> userName) (\s@WorkspaceRequest' {} a -> s {userName = a} :: WorkspaceRequest)

-- | The identifier of the bundle for the WorkSpace. You can use
-- DescribeWorkspaceBundles to list the available bundles.
workspaceRequest_bundleId :: Lens.Lens' WorkspaceRequest Prelude.Text
workspaceRequest_bundleId = Lens.lens (\WorkspaceRequest' {bundleId} -> bundleId) (\s@WorkspaceRequest' {} a -> s {bundleId = a} :: WorkspaceRequest)

instance Prelude.FromJSON WorkspaceRequest where
  parseJSON =
    Prelude.withObject
      "WorkspaceRequest"
      ( \x ->
          WorkspaceRequest'
            Prelude.<$> (x Prelude..:? "WorkspaceProperties")
            Prelude.<*> (x Prelude..:? "RootVolumeEncryptionEnabled")
            Prelude.<*> (x Prelude..:? "UserVolumeEncryptionEnabled")
            Prelude.<*> (x Prelude..:? "VolumeEncryptionKey")
            Prelude.<*> (x Prelude..:? "Tags" Prelude..!= Prelude.mempty)
            Prelude.<*> (x Prelude..: "DirectoryId")
            Prelude.<*> (x Prelude..: "UserName")
            Prelude.<*> (x Prelude..: "BundleId")
      )

instance Prelude.Hashable WorkspaceRequest

instance Prelude.NFData WorkspaceRequest

instance Prelude.ToJSON WorkspaceRequest where
  toJSON WorkspaceRequest' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("WorkspaceProperties" Prelude..=)
              Prelude.<$> workspaceProperties,
            ("RootVolumeEncryptionEnabled" Prelude..=)
              Prelude.<$> rootVolumeEncryptionEnabled,
            ("UserVolumeEncryptionEnabled" Prelude..=)
              Prelude.<$> userVolumeEncryptionEnabled,
            ("VolumeEncryptionKey" Prelude..=)
              Prelude.<$> volumeEncryptionKey,
            ("Tags" Prelude..=) Prelude.<$> tags,
            Prelude.Just ("DirectoryId" Prelude..= directoryId),
            Prelude.Just ("UserName" Prelude..= userName),
            Prelude.Just ("BundleId" Prelude..= bundleId)
          ]
      )
