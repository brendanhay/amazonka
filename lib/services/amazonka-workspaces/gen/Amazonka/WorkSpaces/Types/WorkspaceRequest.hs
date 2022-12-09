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
-- Module      : Amazonka.WorkSpaces.Types.WorkspaceRequest
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WorkSpaces.Types.WorkspaceRequest where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.WorkSpaces.Types.Tag
import Amazonka.WorkSpaces.Types.WorkspaceProperties

-- | Describes the information used to create a WorkSpace.
--
-- /See:/ 'newWorkspaceRequest' smart constructor.
data WorkspaceRequest = WorkspaceRequest'
  { -- | Indicates whether the data stored on the root volume is encrypted.
    rootVolumeEncryptionEnabled :: Prelude.Maybe Prelude.Bool,
    -- | The tags for the WorkSpace.
    tags :: Prelude.Maybe [Tag],
    -- | Indicates whether the data stored on the user volume is encrypted.
    userVolumeEncryptionEnabled :: Prelude.Maybe Prelude.Bool,
    -- | The symmetric KMS key used to encrypt data stored on your WorkSpace.
    -- Amazon WorkSpaces does not support asymmetric KMS keys.
    volumeEncryptionKey :: Prelude.Maybe Prelude.Text,
    -- | The WorkSpace properties.
    workspaceProperties :: Prelude.Maybe WorkspaceProperties,
    -- | The identifier of the Directory Service directory for the WorkSpace. You
    -- can use DescribeWorkspaceDirectories to list the available directories.
    directoryId :: Prelude.Text,
    -- | The user name of the user for the WorkSpace. This user name must exist
    -- in the Directory Service directory for the WorkSpace.
    userName :: Prelude.Text,
    -- | The identifier of the bundle for the WorkSpace. You can use
    -- DescribeWorkspaceBundles to list the available bundles.
    bundleId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'WorkspaceRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'rootVolumeEncryptionEnabled', 'workspaceRequest_rootVolumeEncryptionEnabled' - Indicates whether the data stored on the root volume is encrypted.
--
-- 'tags', 'workspaceRequest_tags' - The tags for the WorkSpace.
--
-- 'userVolumeEncryptionEnabled', 'workspaceRequest_userVolumeEncryptionEnabled' - Indicates whether the data stored on the user volume is encrypted.
--
-- 'volumeEncryptionKey', 'workspaceRequest_volumeEncryptionKey' - The symmetric KMS key used to encrypt data stored on your WorkSpace.
-- Amazon WorkSpaces does not support asymmetric KMS keys.
--
-- 'workspaceProperties', 'workspaceRequest_workspaceProperties' - The WorkSpace properties.
--
-- 'directoryId', 'workspaceRequest_directoryId' - The identifier of the Directory Service directory for the WorkSpace. You
-- can use DescribeWorkspaceDirectories to list the available directories.
--
-- 'userName', 'workspaceRequest_userName' - The user name of the user for the WorkSpace. This user name must exist
-- in the Directory Service directory for the WorkSpace.
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
      { rootVolumeEncryptionEnabled =
          Prelude.Nothing,
        tags = Prelude.Nothing,
        userVolumeEncryptionEnabled = Prelude.Nothing,
        volumeEncryptionKey = Prelude.Nothing,
        workspaceProperties = Prelude.Nothing,
        directoryId = pDirectoryId_,
        userName = pUserName_,
        bundleId = pBundleId_
      }

-- | Indicates whether the data stored on the root volume is encrypted.
workspaceRequest_rootVolumeEncryptionEnabled :: Lens.Lens' WorkspaceRequest (Prelude.Maybe Prelude.Bool)
workspaceRequest_rootVolumeEncryptionEnabled = Lens.lens (\WorkspaceRequest' {rootVolumeEncryptionEnabled} -> rootVolumeEncryptionEnabled) (\s@WorkspaceRequest' {} a -> s {rootVolumeEncryptionEnabled = a} :: WorkspaceRequest)

-- | The tags for the WorkSpace.
workspaceRequest_tags :: Lens.Lens' WorkspaceRequest (Prelude.Maybe [Tag])
workspaceRequest_tags = Lens.lens (\WorkspaceRequest' {tags} -> tags) (\s@WorkspaceRequest' {} a -> s {tags = a} :: WorkspaceRequest) Prelude.. Lens.mapping Lens.coerced

-- | Indicates whether the data stored on the user volume is encrypted.
workspaceRequest_userVolumeEncryptionEnabled :: Lens.Lens' WorkspaceRequest (Prelude.Maybe Prelude.Bool)
workspaceRequest_userVolumeEncryptionEnabled = Lens.lens (\WorkspaceRequest' {userVolumeEncryptionEnabled} -> userVolumeEncryptionEnabled) (\s@WorkspaceRequest' {} a -> s {userVolumeEncryptionEnabled = a} :: WorkspaceRequest)

-- | The symmetric KMS key used to encrypt data stored on your WorkSpace.
-- Amazon WorkSpaces does not support asymmetric KMS keys.
workspaceRequest_volumeEncryptionKey :: Lens.Lens' WorkspaceRequest (Prelude.Maybe Prelude.Text)
workspaceRequest_volumeEncryptionKey = Lens.lens (\WorkspaceRequest' {volumeEncryptionKey} -> volumeEncryptionKey) (\s@WorkspaceRequest' {} a -> s {volumeEncryptionKey = a} :: WorkspaceRequest)

-- | The WorkSpace properties.
workspaceRequest_workspaceProperties :: Lens.Lens' WorkspaceRequest (Prelude.Maybe WorkspaceProperties)
workspaceRequest_workspaceProperties = Lens.lens (\WorkspaceRequest' {workspaceProperties} -> workspaceProperties) (\s@WorkspaceRequest' {} a -> s {workspaceProperties = a} :: WorkspaceRequest)

-- | The identifier of the Directory Service directory for the WorkSpace. You
-- can use DescribeWorkspaceDirectories to list the available directories.
workspaceRequest_directoryId :: Lens.Lens' WorkspaceRequest Prelude.Text
workspaceRequest_directoryId = Lens.lens (\WorkspaceRequest' {directoryId} -> directoryId) (\s@WorkspaceRequest' {} a -> s {directoryId = a} :: WorkspaceRequest)

-- | The user name of the user for the WorkSpace. This user name must exist
-- in the Directory Service directory for the WorkSpace.
workspaceRequest_userName :: Lens.Lens' WorkspaceRequest Prelude.Text
workspaceRequest_userName = Lens.lens (\WorkspaceRequest' {userName} -> userName) (\s@WorkspaceRequest' {} a -> s {userName = a} :: WorkspaceRequest)

-- | The identifier of the bundle for the WorkSpace. You can use
-- DescribeWorkspaceBundles to list the available bundles.
workspaceRequest_bundleId :: Lens.Lens' WorkspaceRequest Prelude.Text
workspaceRequest_bundleId = Lens.lens (\WorkspaceRequest' {bundleId} -> bundleId) (\s@WorkspaceRequest' {} a -> s {bundleId = a} :: WorkspaceRequest)

instance Data.FromJSON WorkspaceRequest where
  parseJSON =
    Data.withObject
      "WorkspaceRequest"
      ( \x ->
          WorkspaceRequest'
            Prelude.<$> (x Data..:? "RootVolumeEncryptionEnabled")
            Prelude.<*> (x Data..:? "Tags" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "UserVolumeEncryptionEnabled")
            Prelude.<*> (x Data..:? "VolumeEncryptionKey")
            Prelude.<*> (x Data..:? "WorkspaceProperties")
            Prelude.<*> (x Data..: "DirectoryId")
            Prelude.<*> (x Data..: "UserName")
            Prelude.<*> (x Data..: "BundleId")
      )

instance Prelude.Hashable WorkspaceRequest where
  hashWithSalt _salt WorkspaceRequest' {..} =
    _salt
      `Prelude.hashWithSalt` rootVolumeEncryptionEnabled
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` userVolumeEncryptionEnabled
      `Prelude.hashWithSalt` volumeEncryptionKey
      `Prelude.hashWithSalt` workspaceProperties
      `Prelude.hashWithSalt` directoryId
      `Prelude.hashWithSalt` userName
      `Prelude.hashWithSalt` bundleId

instance Prelude.NFData WorkspaceRequest where
  rnf WorkspaceRequest' {..} =
    Prelude.rnf rootVolumeEncryptionEnabled
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf userVolumeEncryptionEnabled
      `Prelude.seq` Prelude.rnf volumeEncryptionKey
      `Prelude.seq` Prelude.rnf workspaceProperties
      `Prelude.seq` Prelude.rnf directoryId
      `Prelude.seq` Prelude.rnf userName
      `Prelude.seq` Prelude.rnf bundleId

instance Data.ToJSON WorkspaceRequest where
  toJSON WorkspaceRequest' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("RootVolumeEncryptionEnabled" Data..=)
              Prelude.<$> rootVolumeEncryptionEnabled,
            ("Tags" Data..=) Prelude.<$> tags,
            ("UserVolumeEncryptionEnabled" Data..=)
              Prelude.<$> userVolumeEncryptionEnabled,
            ("VolumeEncryptionKey" Data..=)
              Prelude.<$> volumeEncryptionKey,
            ("WorkspaceProperties" Data..=)
              Prelude.<$> workspaceProperties,
            Prelude.Just ("DirectoryId" Data..= directoryId),
            Prelude.Just ("UserName" Data..= userName),
            Prelude.Just ("BundleId" Data..= bundleId)
          ]
      )
