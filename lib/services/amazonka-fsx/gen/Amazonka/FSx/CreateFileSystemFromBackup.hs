{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.FSx.CreateFileSystemFromBackup
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new Amazon FSx for Lustre, Amazon FSx for Windows File Server,
-- or Amazon FSx for OpenZFS file system from an existing Amazon FSx
-- backup.
--
-- If a file system with the specified client request token exists and the
-- parameters match, this operation returns the description of the file
-- system. If a file system with the specified client request token exists
-- but the parameters don\'t match, this call returns
-- @IncompatibleParameterError@. If a file system with the specified client
-- request token doesn\'t exist, this operation does the following:
--
-- -   Creates a new Amazon FSx file system from backup with an assigned
--     ID, and an initial lifecycle state of @CREATING@.
--
-- -   Returns the description of the file system.
--
-- Parameters like the Active Directory, default share name, automatic
-- backup, and backup settings default to the parameters of the file system
-- that was backed up, unless overridden. You can explicitly supply other
-- settings.
--
-- By using the idempotent operation, you can retry a
-- @CreateFileSystemFromBackup@ call without the risk of creating an extra
-- file system. This approach can be useful when an initial call fails in a
-- way that makes it unclear whether a file system was created. Examples
-- are if a transport level timeout occurred, or your connection was reset.
-- If you use the same client request token and the initial call created a
-- file system, the client receives a success message as long as the
-- parameters are the same.
--
-- The @CreateFileSystemFromBackup@ call returns while the file system\'s
-- lifecycle state is still @CREATING@. You can check the file-system
-- creation status by calling the
-- <https://docs.aws.amazon.com/fsx/latest/APIReference/API_DescribeFileSystems.html DescribeFileSystems>
-- operation, which returns the file system state along with other
-- information.
module Amazonka.FSx.CreateFileSystemFromBackup
  ( -- * Creating a Request
    CreateFileSystemFromBackup (..),
    newCreateFileSystemFromBackup,

    -- * Request Lenses
    createFileSystemFromBackup_tags,
    createFileSystemFromBackup_clientRequestToken,
    createFileSystemFromBackup_fileSystemTypeVersion,
    createFileSystemFromBackup_securityGroupIds,
    createFileSystemFromBackup_openZFSConfiguration,
    createFileSystemFromBackup_storageCapacity,
    createFileSystemFromBackup_storageType,
    createFileSystemFromBackup_windowsConfiguration,
    createFileSystemFromBackup_kmsKeyId,
    createFileSystemFromBackup_lustreConfiguration,
    createFileSystemFromBackup_backupId,
    createFileSystemFromBackup_subnetIds,

    -- * Destructuring the Response
    CreateFileSystemFromBackupResponse (..),
    newCreateFileSystemFromBackupResponse,

    -- * Response Lenses
    createFileSystemFromBackupResponse_fileSystem,
    createFileSystemFromBackupResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FSx.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | The request object for the @CreateFileSystemFromBackup@ operation.
--
-- /See:/ 'newCreateFileSystemFromBackup' smart constructor.
data CreateFileSystemFromBackup = CreateFileSystemFromBackup'
  { -- | The tags to be applied to the file system at file system creation. The
    -- key value of the @Name@ tag appears in the console as the file system
    -- name.
    tags :: Prelude.Maybe (Prelude.NonEmpty Tag),
    -- | A string of up to 64 ASCII characters that Amazon FSx uses to ensure
    -- idempotent creation. This string is automatically filled on your behalf
    -- when you use the Command Line Interface (CLI) or an Amazon Web Services
    -- SDK.
    clientRequestToken :: Prelude.Maybe Prelude.Text,
    -- | Sets the version for the Amazon FSx for Lustre file system that you\'re
    -- creating from a backup. Valid values are @2.10@ and @2.12@.
    --
    -- You don\'t need to specify @FileSystemTypeVersion@ because it will be
    -- applied using the backup\'s @FileSystemTypeVersion@ setting. If you
    -- choose to specify @FileSystemTypeVersion@ when creating from backup, the
    -- value must match the backup\'s @FileSystemTypeVersion@ setting.
    fileSystemTypeVersion :: Prelude.Maybe Prelude.Text,
    -- | A list of IDs for the security groups that apply to the specified
    -- network interfaces created for file system access. These security groups
    -- apply to all network interfaces. This value isn\'t returned in later
    -- @DescribeFileSystem@ requests.
    securityGroupIds :: Prelude.Maybe [Prelude.Text],
    -- | The OpenZFS configuration for the file system that\'s being created.
    openZFSConfiguration :: Prelude.Maybe CreateFileSystemOpenZFSConfiguration,
    -- | Sets the storage capacity of the OpenZFS file system that you\'re
    -- creating from a backup, in gibibytes (GiB). Valid values are from 64 GiB
    -- up to 524,288 GiB (512 TiB). However, the value that you specify must be
    -- equal to or greater than the backup\'s storage capacity value. If you
    -- don\'t use the @StorageCapacity@ parameter, the default is the backup\'s
    -- @StorageCapacity@ value.
    --
    -- If used to create a file system other than OpenZFS, you must provide a
    -- value that matches the backup\'s @StorageCapacity@ value. If you provide
    -- any other value, Amazon FSx responds with a 400 Bad Request.
    storageCapacity :: Prelude.Maybe Prelude.Natural,
    -- | Sets the storage type for the Windows or OpenZFS file system that
    -- you\'re creating from a backup. Valid values are @SSD@ and @HDD@.
    --
    -- -   Set to @SSD@ to use solid state drive storage. SSD is supported on
    --     all Windows and OpenZFS deployment types.
    --
    -- -   Set to @HDD@ to use hard disk drive storage. HDD is supported on
    --     @SINGLE_AZ_2@ and @MULTI_AZ_1@ FSx for Windows File Server file
    --     system deployment types.
    --
    -- The default value is @SSD@.
    --
    -- HDD and SSD storage types have different minimum storage capacity
    -- requirements. A restored file system\'s storage capacity is tied to the
    -- file system that was backed up. You can create a file system that uses
    -- HDD storage from a backup of a file system that used SSD storage if the
    -- original SSD file system had a storage capacity of at least 2000 GiB.
    storageType :: Prelude.Maybe StorageType,
    -- | The configuration for this Microsoft Windows file system.
    windowsConfiguration :: Prelude.Maybe CreateFileSystemWindowsConfiguration,
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    lustreConfiguration :: Prelude.Maybe CreateFileSystemLustreConfiguration,
    backupId :: Prelude.Text,
    -- | Specifies the IDs of the subnets that the file system will be accessible
    -- from. For Windows @MULTI_AZ_1@ file system deployment types, provide
    -- exactly two subnet IDs, one for the preferred file server and one for
    -- the standby file server. You specify one of these subnets as the
    -- preferred subnet using the @WindowsConfiguration > PreferredSubnetID@
    -- property.
    --
    -- Windows @SINGLE_AZ_1@ and @SINGLE_AZ_2@ file system deployment types,
    -- Lustre file systems, and OpenZFS file systems provide exactly one subnet
    -- ID. The file server is launched in that subnet\'s Availability Zone.
    subnetIds :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateFileSystemFromBackup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createFileSystemFromBackup_tags' - The tags to be applied to the file system at file system creation. The
-- key value of the @Name@ tag appears in the console as the file system
-- name.
--
-- 'clientRequestToken', 'createFileSystemFromBackup_clientRequestToken' - A string of up to 64 ASCII characters that Amazon FSx uses to ensure
-- idempotent creation. This string is automatically filled on your behalf
-- when you use the Command Line Interface (CLI) or an Amazon Web Services
-- SDK.
--
-- 'fileSystemTypeVersion', 'createFileSystemFromBackup_fileSystemTypeVersion' - Sets the version for the Amazon FSx for Lustre file system that you\'re
-- creating from a backup. Valid values are @2.10@ and @2.12@.
--
-- You don\'t need to specify @FileSystemTypeVersion@ because it will be
-- applied using the backup\'s @FileSystemTypeVersion@ setting. If you
-- choose to specify @FileSystemTypeVersion@ when creating from backup, the
-- value must match the backup\'s @FileSystemTypeVersion@ setting.
--
-- 'securityGroupIds', 'createFileSystemFromBackup_securityGroupIds' - A list of IDs for the security groups that apply to the specified
-- network interfaces created for file system access. These security groups
-- apply to all network interfaces. This value isn\'t returned in later
-- @DescribeFileSystem@ requests.
--
-- 'openZFSConfiguration', 'createFileSystemFromBackup_openZFSConfiguration' - The OpenZFS configuration for the file system that\'s being created.
--
-- 'storageCapacity', 'createFileSystemFromBackup_storageCapacity' - Sets the storage capacity of the OpenZFS file system that you\'re
-- creating from a backup, in gibibytes (GiB). Valid values are from 64 GiB
-- up to 524,288 GiB (512 TiB). However, the value that you specify must be
-- equal to or greater than the backup\'s storage capacity value. If you
-- don\'t use the @StorageCapacity@ parameter, the default is the backup\'s
-- @StorageCapacity@ value.
--
-- If used to create a file system other than OpenZFS, you must provide a
-- value that matches the backup\'s @StorageCapacity@ value. If you provide
-- any other value, Amazon FSx responds with a 400 Bad Request.
--
-- 'storageType', 'createFileSystemFromBackup_storageType' - Sets the storage type for the Windows or OpenZFS file system that
-- you\'re creating from a backup. Valid values are @SSD@ and @HDD@.
--
-- -   Set to @SSD@ to use solid state drive storage. SSD is supported on
--     all Windows and OpenZFS deployment types.
--
-- -   Set to @HDD@ to use hard disk drive storage. HDD is supported on
--     @SINGLE_AZ_2@ and @MULTI_AZ_1@ FSx for Windows File Server file
--     system deployment types.
--
-- The default value is @SSD@.
--
-- HDD and SSD storage types have different minimum storage capacity
-- requirements. A restored file system\'s storage capacity is tied to the
-- file system that was backed up. You can create a file system that uses
-- HDD storage from a backup of a file system that used SSD storage if the
-- original SSD file system had a storage capacity of at least 2000 GiB.
--
-- 'windowsConfiguration', 'createFileSystemFromBackup_windowsConfiguration' - The configuration for this Microsoft Windows file system.
--
-- 'kmsKeyId', 'createFileSystemFromBackup_kmsKeyId' - Undocumented member.
--
-- 'lustreConfiguration', 'createFileSystemFromBackup_lustreConfiguration' - Undocumented member.
--
-- 'backupId', 'createFileSystemFromBackup_backupId' - Undocumented member.
--
-- 'subnetIds', 'createFileSystemFromBackup_subnetIds' - Specifies the IDs of the subnets that the file system will be accessible
-- from. For Windows @MULTI_AZ_1@ file system deployment types, provide
-- exactly two subnet IDs, one for the preferred file server and one for
-- the standby file server. You specify one of these subnets as the
-- preferred subnet using the @WindowsConfiguration > PreferredSubnetID@
-- property.
--
-- Windows @SINGLE_AZ_1@ and @SINGLE_AZ_2@ file system deployment types,
-- Lustre file systems, and OpenZFS file systems provide exactly one subnet
-- ID. The file server is launched in that subnet\'s Availability Zone.
newCreateFileSystemFromBackup ::
  -- | 'backupId'
  Prelude.Text ->
  CreateFileSystemFromBackup
newCreateFileSystemFromBackup pBackupId_ =
  CreateFileSystemFromBackup'
    { tags = Prelude.Nothing,
      clientRequestToken = Prelude.Nothing,
      fileSystemTypeVersion = Prelude.Nothing,
      securityGroupIds = Prelude.Nothing,
      openZFSConfiguration = Prelude.Nothing,
      storageCapacity = Prelude.Nothing,
      storageType = Prelude.Nothing,
      windowsConfiguration = Prelude.Nothing,
      kmsKeyId = Prelude.Nothing,
      lustreConfiguration = Prelude.Nothing,
      backupId = pBackupId_,
      subnetIds = Prelude.mempty
    }

-- | The tags to be applied to the file system at file system creation. The
-- key value of the @Name@ tag appears in the console as the file system
-- name.
createFileSystemFromBackup_tags :: Lens.Lens' CreateFileSystemFromBackup (Prelude.Maybe (Prelude.NonEmpty Tag))
createFileSystemFromBackup_tags = Lens.lens (\CreateFileSystemFromBackup' {tags} -> tags) (\s@CreateFileSystemFromBackup' {} a -> s {tags = a} :: CreateFileSystemFromBackup) Prelude.. Lens.mapping Lens.coerced

-- | A string of up to 64 ASCII characters that Amazon FSx uses to ensure
-- idempotent creation. This string is automatically filled on your behalf
-- when you use the Command Line Interface (CLI) or an Amazon Web Services
-- SDK.
createFileSystemFromBackup_clientRequestToken :: Lens.Lens' CreateFileSystemFromBackup (Prelude.Maybe Prelude.Text)
createFileSystemFromBackup_clientRequestToken = Lens.lens (\CreateFileSystemFromBackup' {clientRequestToken} -> clientRequestToken) (\s@CreateFileSystemFromBackup' {} a -> s {clientRequestToken = a} :: CreateFileSystemFromBackup)

-- | Sets the version for the Amazon FSx for Lustre file system that you\'re
-- creating from a backup. Valid values are @2.10@ and @2.12@.
--
-- You don\'t need to specify @FileSystemTypeVersion@ because it will be
-- applied using the backup\'s @FileSystemTypeVersion@ setting. If you
-- choose to specify @FileSystemTypeVersion@ when creating from backup, the
-- value must match the backup\'s @FileSystemTypeVersion@ setting.
createFileSystemFromBackup_fileSystemTypeVersion :: Lens.Lens' CreateFileSystemFromBackup (Prelude.Maybe Prelude.Text)
createFileSystemFromBackup_fileSystemTypeVersion = Lens.lens (\CreateFileSystemFromBackup' {fileSystemTypeVersion} -> fileSystemTypeVersion) (\s@CreateFileSystemFromBackup' {} a -> s {fileSystemTypeVersion = a} :: CreateFileSystemFromBackup)

-- | A list of IDs for the security groups that apply to the specified
-- network interfaces created for file system access. These security groups
-- apply to all network interfaces. This value isn\'t returned in later
-- @DescribeFileSystem@ requests.
createFileSystemFromBackup_securityGroupIds :: Lens.Lens' CreateFileSystemFromBackup (Prelude.Maybe [Prelude.Text])
createFileSystemFromBackup_securityGroupIds = Lens.lens (\CreateFileSystemFromBackup' {securityGroupIds} -> securityGroupIds) (\s@CreateFileSystemFromBackup' {} a -> s {securityGroupIds = a} :: CreateFileSystemFromBackup) Prelude.. Lens.mapping Lens.coerced

-- | The OpenZFS configuration for the file system that\'s being created.
createFileSystemFromBackup_openZFSConfiguration :: Lens.Lens' CreateFileSystemFromBackup (Prelude.Maybe CreateFileSystemOpenZFSConfiguration)
createFileSystemFromBackup_openZFSConfiguration = Lens.lens (\CreateFileSystemFromBackup' {openZFSConfiguration} -> openZFSConfiguration) (\s@CreateFileSystemFromBackup' {} a -> s {openZFSConfiguration = a} :: CreateFileSystemFromBackup)

-- | Sets the storage capacity of the OpenZFS file system that you\'re
-- creating from a backup, in gibibytes (GiB). Valid values are from 64 GiB
-- up to 524,288 GiB (512 TiB). However, the value that you specify must be
-- equal to or greater than the backup\'s storage capacity value. If you
-- don\'t use the @StorageCapacity@ parameter, the default is the backup\'s
-- @StorageCapacity@ value.
--
-- If used to create a file system other than OpenZFS, you must provide a
-- value that matches the backup\'s @StorageCapacity@ value. If you provide
-- any other value, Amazon FSx responds with a 400 Bad Request.
createFileSystemFromBackup_storageCapacity :: Lens.Lens' CreateFileSystemFromBackup (Prelude.Maybe Prelude.Natural)
createFileSystemFromBackup_storageCapacity = Lens.lens (\CreateFileSystemFromBackup' {storageCapacity} -> storageCapacity) (\s@CreateFileSystemFromBackup' {} a -> s {storageCapacity = a} :: CreateFileSystemFromBackup)

-- | Sets the storage type for the Windows or OpenZFS file system that
-- you\'re creating from a backup. Valid values are @SSD@ and @HDD@.
--
-- -   Set to @SSD@ to use solid state drive storage. SSD is supported on
--     all Windows and OpenZFS deployment types.
--
-- -   Set to @HDD@ to use hard disk drive storage. HDD is supported on
--     @SINGLE_AZ_2@ and @MULTI_AZ_1@ FSx for Windows File Server file
--     system deployment types.
--
-- The default value is @SSD@.
--
-- HDD and SSD storage types have different minimum storage capacity
-- requirements. A restored file system\'s storage capacity is tied to the
-- file system that was backed up. You can create a file system that uses
-- HDD storage from a backup of a file system that used SSD storage if the
-- original SSD file system had a storage capacity of at least 2000 GiB.
createFileSystemFromBackup_storageType :: Lens.Lens' CreateFileSystemFromBackup (Prelude.Maybe StorageType)
createFileSystemFromBackup_storageType = Lens.lens (\CreateFileSystemFromBackup' {storageType} -> storageType) (\s@CreateFileSystemFromBackup' {} a -> s {storageType = a} :: CreateFileSystemFromBackup)

-- | The configuration for this Microsoft Windows file system.
createFileSystemFromBackup_windowsConfiguration :: Lens.Lens' CreateFileSystemFromBackup (Prelude.Maybe CreateFileSystemWindowsConfiguration)
createFileSystemFromBackup_windowsConfiguration = Lens.lens (\CreateFileSystemFromBackup' {windowsConfiguration} -> windowsConfiguration) (\s@CreateFileSystemFromBackup' {} a -> s {windowsConfiguration = a} :: CreateFileSystemFromBackup)

-- | Undocumented member.
createFileSystemFromBackup_kmsKeyId :: Lens.Lens' CreateFileSystemFromBackup (Prelude.Maybe Prelude.Text)
createFileSystemFromBackup_kmsKeyId = Lens.lens (\CreateFileSystemFromBackup' {kmsKeyId} -> kmsKeyId) (\s@CreateFileSystemFromBackup' {} a -> s {kmsKeyId = a} :: CreateFileSystemFromBackup)

-- | Undocumented member.
createFileSystemFromBackup_lustreConfiguration :: Lens.Lens' CreateFileSystemFromBackup (Prelude.Maybe CreateFileSystemLustreConfiguration)
createFileSystemFromBackup_lustreConfiguration = Lens.lens (\CreateFileSystemFromBackup' {lustreConfiguration} -> lustreConfiguration) (\s@CreateFileSystemFromBackup' {} a -> s {lustreConfiguration = a} :: CreateFileSystemFromBackup)

-- | Undocumented member.
createFileSystemFromBackup_backupId :: Lens.Lens' CreateFileSystemFromBackup Prelude.Text
createFileSystemFromBackup_backupId = Lens.lens (\CreateFileSystemFromBackup' {backupId} -> backupId) (\s@CreateFileSystemFromBackup' {} a -> s {backupId = a} :: CreateFileSystemFromBackup)

-- | Specifies the IDs of the subnets that the file system will be accessible
-- from. For Windows @MULTI_AZ_1@ file system deployment types, provide
-- exactly two subnet IDs, one for the preferred file server and one for
-- the standby file server. You specify one of these subnets as the
-- preferred subnet using the @WindowsConfiguration > PreferredSubnetID@
-- property.
--
-- Windows @SINGLE_AZ_1@ and @SINGLE_AZ_2@ file system deployment types,
-- Lustre file systems, and OpenZFS file systems provide exactly one subnet
-- ID. The file server is launched in that subnet\'s Availability Zone.
createFileSystemFromBackup_subnetIds :: Lens.Lens' CreateFileSystemFromBackup [Prelude.Text]
createFileSystemFromBackup_subnetIds = Lens.lens (\CreateFileSystemFromBackup' {subnetIds} -> subnetIds) (\s@CreateFileSystemFromBackup' {} a -> s {subnetIds = a} :: CreateFileSystemFromBackup) Prelude.. Lens.coerced

instance Core.AWSRequest CreateFileSystemFromBackup where
  type
    AWSResponse CreateFileSystemFromBackup =
      CreateFileSystemFromBackupResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateFileSystemFromBackupResponse'
            Prelude.<$> (x Data..?> "FileSystem")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateFileSystemFromBackup where
  hashWithSalt _salt CreateFileSystemFromBackup' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` clientRequestToken
      `Prelude.hashWithSalt` fileSystemTypeVersion
      `Prelude.hashWithSalt` securityGroupIds
      `Prelude.hashWithSalt` openZFSConfiguration
      `Prelude.hashWithSalt` storageCapacity
      `Prelude.hashWithSalt` storageType
      `Prelude.hashWithSalt` windowsConfiguration
      `Prelude.hashWithSalt` kmsKeyId
      `Prelude.hashWithSalt` lustreConfiguration
      `Prelude.hashWithSalt` backupId
      `Prelude.hashWithSalt` subnetIds

instance Prelude.NFData CreateFileSystemFromBackup where
  rnf CreateFileSystemFromBackup' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf clientRequestToken
      `Prelude.seq` Prelude.rnf fileSystemTypeVersion
      `Prelude.seq` Prelude.rnf securityGroupIds
      `Prelude.seq` Prelude.rnf openZFSConfiguration
      `Prelude.seq` Prelude.rnf storageCapacity
      `Prelude.seq` Prelude.rnf storageType
      `Prelude.seq` Prelude.rnf windowsConfiguration
      `Prelude.seq` Prelude.rnf kmsKeyId
      `Prelude.seq` Prelude.rnf lustreConfiguration
      `Prelude.seq` Prelude.rnf backupId
      `Prelude.seq` Prelude.rnf subnetIds

instance Data.ToHeaders CreateFileSystemFromBackup where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSSimbaAPIService_v20180301.CreateFileSystemFromBackup" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateFileSystemFromBackup where
  toJSON CreateFileSystemFromBackup' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Tags" Data..=) Prelude.<$> tags,
            ("ClientRequestToken" Data..=)
              Prelude.<$> clientRequestToken,
            ("FileSystemTypeVersion" Data..=)
              Prelude.<$> fileSystemTypeVersion,
            ("SecurityGroupIds" Data..=)
              Prelude.<$> securityGroupIds,
            ("OpenZFSConfiguration" Data..=)
              Prelude.<$> openZFSConfiguration,
            ("StorageCapacity" Data..=)
              Prelude.<$> storageCapacity,
            ("StorageType" Data..=) Prelude.<$> storageType,
            ("WindowsConfiguration" Data..=)
              Prelude.<$> windowsConfiguration,
            ("KmsKeyId" Data..=) Prelude.<$> kmsKeyId,
            ("LustreConfiguration" Data..=)
              Prelude.<$> lustreConfiguration,
            Prelude.Just ("BackupId" Data..= backupId),
            Prelude.Just ("SubnetIds" Data..= subnetIds)
          ]
      )

instance Data.ToPath CreateFileSystemFromBackup where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateFileSystemFromBackup where
  toQuery = Prelude.const Prelude.mempty

-- | The response object for the @CreateFileSystemFromBackup@ operation.
--
-- /See:/ 'newCreateFileSystemFromBackupResponse' smart constructor.
data CreateFileSystemFromBackupResponse = CreateFileSystemFromBackupResponse'
  { -- | A description of the file system.
    fileSystem :: Prelude.Maybe FileSystem,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateFileSystemFromBackupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fileSystem', 'createFileSystemFromBackupResponse_fileSystem' - A description of the file system.
--
-- 'httpStatus', 'createFileSystemFromBackupResponse_httpStatus' - The response's http status code.
newCreateFileSystemFromBackupResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateFileSystemFromBackupResponse
newCreateFileSystemFromBackupResponse pHttpStatus_ =
  CreateFileSystemFromBackupResponse'
    { fileSystem =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A description of the file system.
createFileSystemFromBackupResponse_fileSystem :: Lens.Lens' CreateFileSystemFromBackupResponse (Prelude.Maybe FileSystem)
createFileSystemFromBackupResponse_fileSystem = Lens.lens (\CreateFileSystemFromBackupResponse' {fileSystem} -> fileSystem) (\s@CreateFileSystemFromBackupResponse' {} a -> s {fileSystem = a} :: CreateFileSystemFromBackupResponse)

-- | The response's http status code.
createFileSystemFromBackupResponse_httpStatus :: Lens.Lens' CreateFileSystemFromBackupResponse Prelude.Int
createFileSystemFromBackupResponse_httpStatus = Lens.lens (\CreateFileSystemFromBackupResponse' {httpStatus} -> httpStatus) (\s@CreateFileSystemFromBackupResponse' {} a -> s {httpStatus = a} :: CreateFileSystemFromBackupResponse)

instance
  Prelude.NFData
    CreateFileSystemFromBackupResponse
  where
  rnf CreateFileSystemFromBackupResponse' {..} =
    Prelude.rnf fileSystem
      `Prelude.seq` Prelude.rnf httpStatus
