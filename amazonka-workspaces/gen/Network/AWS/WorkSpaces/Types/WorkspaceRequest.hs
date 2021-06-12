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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.WorkSpaces.Types.Tag
import Network.AWS.WorkSpaces.Types.WorkspaceProperties

-- | Describes the information used to create a WorkSpace.
--
-- /See:/ 'newWorkspaceRequest' smart constructor.
data WorkspaceRequest = WorkspaceRequest'
  { -- | The WorkSpace properties.
    workspaceProperties :: Core.Maybe WorkspaceProperties,
    -- | Indicates whether the data stored on the root volume is encrypted.
    rootVolumeEncryptionEnabled :: Core.Maybe Core.Bool,
    -- | Indicates whether the data stored on the user volume is encrypted.
    userVolumeEncryptionEnabled :: Core.Maybe Core.Bool,
    -- | The symmetric AWS KMS customer master key (CMK) used to encrypt data
    -- stored on your WorkSpace. Amazon WorkSpaces does not support asymmetric
    -- CMKs.
    volumeEncryptionKey :: Core.Maybe Core.Text,
    -- | The tags for the WorkSpace.
    tags :: Core.Maybe [Tag],
    -- | The identifier of the AWS Directory Service directory for the WorkSpace.
    -- You can use DescribeWorkspaceDirectories to list the available
    -- directories.
    directoryId :: Core.Text,
    -- | The user name of the user for the WorkSpace. This user name must exist
    -- in the AWS Directory Service directory for the WorkSpace.
    userName :: Core.Text,
    -- | The identifier of the bundle for the WorkSpace. You can use
    -- DescribeWorkspaceBundles to list the available bundles.
    bundleId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  -- | 'userName'
  Core.Text ->
  -- | 'bundleId'
  Core.Text ->
  WorkspaceRequest
newWorkspaceRequest
  pDirectoryId_
  pUserName_
  pBundleId_ =
    WorkspaceRequest'
      { workspaceProperties =
          Core.Nothing,
        rootVolumeEncryptionEnabled = Core.Nothing,
        userVolumeEncryptionEnabled = Core.Nothing,
        volumeEncryptionKey = Core.Nothing,
        tags = Core.Nothing,
        directoryId = pDirectoryId_,
        userName = pUserName_,
        bundleId = pBundleId_
      }

-- | The WorkSpace properties.
workspaceRequest_workspaceProperties :: Lens.Lens' WorkspaceRequest (Core.Maybe WorkspaceProperties)
workspaceRequest_workspaceProperties = Lens.lens (\WorkspaceRequest' {workspaceProperties} -> workspaceProperties) (\s@WorkspaceRequest' {} a -> s {workspaceProperties = a} :: WorkspaceRequest)

-- | Indicates whether the data stored on the root volume is encrypted.
workspaceRequest_rootVolumeEncryptionEnabled :: Lens.Lens' WorkspaceRequest (Core.Maybe Core.Bool)
workspaceRequest_rootVolumeEncryptionEnabled = Lens.lens (\WorkspaceRequest' {rootVolumeEncryptionEnabled} -> rootVolumeEncryptionEnabled) (\s@WorkspaceRequest' {} a -> s {rootVolumeEncryptionEnabled = a} :: WorkspaceRequest)

-- | Indicates whether the data stored on the user volume is encrypted.
workspaceRequest_userVolumeEncryptionEnabled :: Lens.Lens' WorkspaceRequest (Core.Maybe Core.Bool)
workspaceRequest_userVolumeEncryptionEnabled = Lens.lens (\WorkspaceRequest' {userVolumeEncryptionEnabled} -> userVolumeEncryptionEnabled) (\s@WorkspaceRequest' {} a -> s {userVolumeEncryptionEnabled = a} :: WorkspaceRequest)

-- | The symmetric AWS KMS customer master key (CMK) used to encrypt data
-- stored on your WorkSpace. Amazon WorkSpaces does not support asymmetric
-- CMKs.
workspaceRequest_volumeEncryptionKey :: Lens.Lens' WorkspaceRequest (Core.Maybe Core.Text)
workspaceRequest_volumeEncryptionKey = Lens.lens (\WorkspaceRequest' {volumeEncryptionKey} -> volumeEncryptionKey) (\s@WorkspaceRequest' {} a -> s {volumeEncryptionKey = a} :: WorkspaceRequest)

-- | The tags for the WorkSpace.
workspaceRequest_tags :: Lens.Lens' WorkspaceRequest (Core.Maybe [Tag])
workspaceRequest_tags = Lens.lens (\WorkspaceRequest' {tags} -> tags) (\s@WorkspaceRequest' {} a -> s {tags = a} :: WorkspaceRequest) Core.. Lens.mapping Lens._Coerce

-- | The identifier of the AWS Directory Service directory for the WorkSpace.
-- You can use DescribeWorkspaceDirectories to list the available
-- directories.
workspaceRequest_directoryId :: Lens.Lens' WorkspaceRequest Core.Text
workspaceRequest_directoryId = Lens.lens (\WorkspaceRequest' {directoryId} -> directoryId) (\s@WorkspaceRequest' {} a -> s {directoryId = a} :: WorkspaceRequest)

-- | The user name of the user for the WorkSpace. This user name must exist
-- in the AWS Directory Service directory for the WorkSpace.
workspaceRequest_userName :: Lens.Lens' WorkspaceRequest Core.Text
workspaceRequest_userName = Lens.lens (\WorkspaceRequest' {userName} -> userName) (\s@WorkspaceRequest' {} a -> s {userName = a} :: WorkspaceRequest)

-- | The identifier of the bundle for the WorkSpace. You can use
-- DescribeWorkspaceBundles to list the available bundles.
workspaceRequest_bundleId :: Lens.Lens' WorkspaceRequest Core.Text
workspaceRequest_bundleId = Lens.lens (\WorkspaceRequest' {bundleId} -> bundleId) (\s@WorkspaceRequest' {} a -> s {bundleId = a} :: WorkspaceRequest)

instance Core.FromJSON WorkspaceRequest where
  parseJSON =
    Core.withObject
      "WorkspaceRequest"
      ( \x ->
          WorkspaceRequest'
            Core.<$> (x Core..:? "WorkspaceProperties")
            Core.<*> (x Core..:? "RootVolumeEncryptionEnabled")
            Core.<*> (x Core..:? "UserVolumeEncryptionEnabled")
            Core.<*> (x Core..:? "VolumeEncryptionKey")
            Core.<*> (x Core..:? "Tags" Core..!= Core.mempty)
            Core.<*> (x Core..: "DirectoryId")
            Core.<*> (x Core..: "UserName")
            Core.<*> (x Core..: "BundleId")
      )

instance Core.Hashable WorkspaceRequest

instance Core.NFData WorkspaceRequest

instance Core.ToJSON WorkspaceRequest where
  toJSON WorkspaceRequest' {..} =
    Core.object
      ( Core.catMaybes
          [ ("WorkspaceProperties" Core..=)
              Core.<$> workspaceProperties,
            ("RootVolumeEncryptionEnabled" Core..=)
              Core.<$> rootVolumeEncryptionEnabled,
            ("UserVolumeEncryptionEnabled" Core..=)
              Core.<$> userVolumeEncryptionEnabled,
            ("VolumeEncryptionKey" Core..=)
              Core.<$> volumeEncryptionKey,
            ("Tags" Core..=) Core.<$> tags,
            Core.Just ("DirectoryId" Core..= directoryId),
            Core.Just ("UserName" Core..= userName),
            Core.Just ("BundleId" Core..= bundleId)
          ]
      )
