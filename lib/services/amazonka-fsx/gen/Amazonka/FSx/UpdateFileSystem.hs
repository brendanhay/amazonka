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
-- Module      : Amazonka.FSx.UpdateFileSystem
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Use this operation to update the configuration of an existing Amazon FSx
-- file system. You can update multiple properties in a single request.
--
-- For Amazon FSx for Windows File Server file systems, you can update the
-- following properties:
--
-- -   @AuditLogConfiguration@
--
-- -   @AutomaticBackupRetentionDays@
--
-- -   @DailyAutomaticBackupStartTime@
--
-- -   @SelfManagedActiveDirectoryConfiguration@
--
-- -   @StorageCapacity@
--
-- -   @ThroughputCapacity@
--
-- -   @WeeklyMaintenanceStartTime@
--
-- For Amazon FSx for Lustre file systems, you can update the following
-- properties:
--
-- -   @AutoImportPolicy@
--
-- -   @AutomaticBackupRetentionDays@
--
-- -   @DailyAutomaticBackupStartTime@
--
-- -   @DataCompressionType@
--
-- -   @LustreRootSquashConfiguration@
--
-- -   @StorageCapacity@
--
-- -   @WeeklyMaintenanceStartTime@
--
-- For Amazon FSx for NetApp ONTAP file systems, you can update the
-- following properties:
--
-- -   @AutomaticBackupRetentionDays@
--
-- -   @DailyAutomaticBackupStartTime@
--
-- -   @DiskIopsConfiguration@
--
-- -   @FsxAdminPassword@
--
-- -   @StorageCapacity@
--
-- -   @ThroughputCapacity@
--
-- -   @WeeklyMaintenanceStartTime@
--
-- For the Amazon FSx for OpenZFS file systems, you can update the
-- following properties:
--
-- -   @AutomaticBackupRetentionDays@
--
-- -   @CopyTagsToBackups@
--
-- -   @CopyTagsToVolumes@
--
-- -   @DailyAutomaticBackupStartTime@
--
-- -   @ThroughputCapacity@
--
-- -   @WeeklyMaintenanceStartTime@
module Amazonka.FSx.UpdateFileSystem
  ( -- * Creating a Request
    UpdateFileSystem (..),
    newUpdateFileSystem,

    -- * Request Lenses
    updateFileSystem_clientRequestToken,
    updateFileSystem_lustreConfiguration,
    updateFileSystem_ontapConfiguration,
    updateFileSystem_openZFSConfiguration,
    updateFileSystem_storageCapacity,
    updateFileSystem_windowsConfiguration,
    updateFileSystem_fileSystemId,

    -- * Destructuring the Response
    UpdateFileSystemResponse (..),
    newUpdateFileSystemResponse,

    -- * Response Lenses
    updateFileSystemResponse_fileSystem,
    updateFileSystemResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FSx.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | The request object for the @UpdateFileSystem@ operation.
--
-- /See:/ 'newUpdateFileSystem' smart constructor.
data UpdateFileSystem = UpdateFileSystem'
  { -- | A string of up to 64 ASCII characters that Amazon FSx uses to ensure
    -- idempotent updates. This string is automatically filled on your behalf
    -- when you use the Command Line Interface (CLI) or an Amazon Web Services
    -- SDK.
    clientRequestToken :: Prelude.Maybe Prelude.Text,
    lustreConfiguration :: Prelude.Maybe UpdateFileSystemLustreConfiguration,
    ontapConfiguration :: Prelude.Maybe UpdateFileSystemOntapConfiguration,
    -- | The configuration updates for an Amazon FSx for OpenZFS file system.
    openZFSConfiguration :: Prelude.Maybe UpdateFileSystemOpenZFSConfiguration,
    -- | Use this parameter to increase the storage capacity of an Amazon FSx for
    -- Windows File Server, Amazon FSx for Lustre, or Amazon FSx for NetApp
    -- ONTAP file system. Specifies the storage capacity target value, in GiB,
    -- to increase the storage capacity for the file system that you\'re
    -- updating.
    --
    -- You can\'t make a storage capacity increase request if there is an
    -- existing storage capacity increase request in progress.
    --
    -- For Windows file systems, the storage capacity target value must be at
    -- least 10 percent greater than the current storage capacity value. To
    -- increase storage capacity, the file system must have at least 16 MBps of
    -- throughput capacity. For more information, see
    -- <https://docs.aws.amazon.com/fsx/latest/WindowsGuide/managing-storage-capacity.html Managing storage capacity>
    -- in the /Amazon FSx for Windows File Server User Guide/.
    --
    -- For Lustre file systems, the storage capacity target value can be the
    -- following:
    --
    -- -   For @SCRATCH_2@, @PERSISTENT_1@, and @PERSISTENT_2 SSD@ deployment
    --     types, valid values are in multiples of 2400 GiB. The value must be
    --     greater than the current storage capacity.
    --
    -- -   For @PERSISTENT HDD@ file systems, valid values are multiples of
    --     6000 GiB for 12-MBps throughput per TiB file systems and multiples
    --     of 1800 GiB for 40-MBps throughput per TiB file systems. The values
    --     must be greater than the current storage capacity.
    --
    -- -   For @SCRATCH_1@ file systems, you can\'t increase the storage
    --     capacity.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/fsx/latest/LustreGuide/managing-storage-capacity.html Managing storage and throughput capacity>
    -- in the /Amazon FSx for Lustre User Guide/.
    --
    -- For ONTAP file systems, the storage capacity target value must be at
    -- least 10 percent greater than the current storage capacity value. For
    -- more information, see
    -- <https://docs.aws.amazon.com/fsx/latest/ONTAPGuide/managing-storage-capacity.html Managing storage capacity and provisioned IOPS>
    -- in the /Amazon FSx for NetApp ONTAP User Guide/.
    storageCapacity :: Prelude.Maybe Prelude.Natural,
    -- | The configuration updates for an Amazon FSx for Windows File Server file
    -- system.
    windowsConfiguration :: Prelude.Maybe UpdateFileSystemWindowsConfiguration,
    -- | The ID of the file system that you are updating.
    fileSystemId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateFileSystem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientRequestToken', 'updateFileSystem_clientRequestToken' - A string of up to 64 ASCII characters that Amazon FSx uses to ensure
-- idempotent updates. This string is automatically filled on your behalf
-- when you use the Command Line Interface (CLI) or an Amazon Web Services
-- SDK.
--
-- 'lustreConfiguration', 'updateFileSystem_lustreConfiguration' - Undocumented member.
--
-- 'ontapConfiguration', 'updateFileSystem_ontapConfiguration' - Undocumented member.
--
-- 'openZFSConfiguration', 'updateFileSystem_openZFSConfiguration' - The configuration updates for an Amazon FSx for OpenZFS file system.
--
-- 'storageCapacity', 'updateFileSystem_storageCapacity' - Use this parameter to increase the storage capacity of an Amazon FSx for
-- Windows File Server, Amazon FSx for Lustre, or Amazon FSx for NetApp
-- ONTAP file system. Specifies the storage capacity target value, in GiB,
-- to increase the storage capacity for the file system that you\'re
-- updating.
--
-- You can\'t make a storage capacity increase request if there is an
-- existing storage capacity increase request in progress.
--
-- For Windows file systems, the storage capacity target value must be at
-- least 10 percent greater than the current storage capacity value. To
-- increase storage capacity, the file system must have at least 16 MBps of
-- throughput capacity. For more information, see
-- <https://docs.aws.amazon.com/fsx/latest/WindowsGuide/managing-storage-capacity.html Managing storage capacity>
-- in the /Amazon FSx for Windows File Server User Guide/.
--
-- For Lustre file systems, the storage capacity target value can be the
-- following:
--
-- -   For @SCRATCH_2@, @PERSISTENT_1@, and @PERSISTENT_2 SSD@ deployment
--     types, valid values are in multiples of 2400 GiB. The value must be
--     greater than the current storage capacity.
--
-- -   For @PERSISTENT HDD@ file systems, valid values are multiples of
--     6000 GiB for 12-MBps throughput per TiB file systems and multiples
--     of 1800 GiB for 40-MBps throughput per TiB file systems. The values
--     must be greater than the current storage capacity.
--
-- -   For @SCRATCH_1@ file systems, you can\'t increase the storage
--     capacity.
--
-- For more information, see
-- <https://docs.aws.amazon.com/fsx/latest/LustreGuide/managing-storage-capacity.html Managing storage and throughput capacity>
-- in the /Amazon FSx for Lustre User Guide/.
--
-- For ONTAP file systems, the storage capacity target value must be at
-- least 10 percent greater than the current storage capacity value. For
-- more information, see
-- <https://docs.aws.amazon.com/fsx/latest/ONTAPGuide/managing-storage-capacity.html Managing storage capacity and provisioned IOPS>
-- in the /Amazon FSx for NetApp ONTAP User Guide/.
--
-- 'windowsConfiguration', 'updateFileSystem_windowsConfiguration' - The configuration updates for an Amazon FSx for Windows File Server file
-- system.
--
-- 'fileSystemId', 'updateFileSystem_fileSystemId' - The ID of the file system that you are updating.
newUpdateFileSystem ::
  -- | 'fileSystemId'
  Prelude.Text ->
  UpdateFileSystem
newUpdateFileSystem pFileSystemId_ =
  UpdateFileSystem'
    { clientRequestToken =
        Prelude.Nothing,
      lustreConfiguration = Prelude.Nothing,
      ontapConfiguration = Prelude.Nothing,
      openZFSConfiguration = Prelude.Nothing,
      storageCapacity = Prelude.Nothing,
      windowsConfiguration = Prelude.Nothing,
      fileSystemId = pFileSystemId_
    }

-- | A string of up to 64 ASCII characters that Amazon FSx uses to ensure
-- idempotent updates. This string is automatically filled on your behalf
-- when you use the Command Line Interface (CLI) or an Amazon Web Services
-- SDK.
updateFileSystem_clientRequestToken :: Lens.Lens' UpdateFileSystem (Prelude.Maybe Prelude.Text)
updateFileSystem_clientRequestToken = Lens.lens (\UpdateFileSystem' {clientRequestToken} -> clientRequestToken) (\s@UpdateFileSystem' {} a -> s {clientRequestToken = a} :: UpdateFileSystem)

-- | Undocumented member.
updateFileSystem_lustreConfiguration :: Lens.Lens' UpdateFileSystem (Prelude.Maybe UpdateFileSystemLustreConfiguration)
updateFileSystem_lustreConfiguration = Lens.lens (\UpdateFileSystem' {lustreConfiguration} -> lustreConfiguration) (\s@UpdateFileSystem' {} a -> s {lustreConfiguration = a} :: UpdateFileSystem)

-- | Undocumented member.
updateFileSystem_ontapConfiguration :: Lens.Lens' UpdateFileSystem (Prelude.Maybe UpdateFileSystemOntapConfiguration)
updateFileSystem_ontapConfiguration = Lens.lens (\UpdateFileSystem' {ontapConfiguration} -> ontapConfiguration) (\s@UpdateFileSystem' {} a -> s {ontapConfiguration = a} :: UpdateFileSystem)

-- | The configuration updates for an Amazon FSx for OpenZFS file system.
updateFileSystem_openZFSConfiguration :: Lens.Lens' UpdateFileSystem (Prelude.Maybe UpdateFileSystemOpenZFSConfiguration)
updateFileSystem_openZFSConfiguration = Lens.lens (\UpdateFileSystem' {openZFSConfiguration} -> openZFSConfiguration) (\s@UpdateFileSystem' {} a -> s {openZFSConfiguration = a} :: UpdateFileSystem)

-- | Use this parameter to increase the storage capacity of an Amazon FSx for
-- Windows File Server, Amazon FSx for Lustre, or Amazon FSx for NetApp
-- ONTAP file system. Specifies the storage capacity target value, in GiB,
-- to increase the storage capacity for the file system that you\'re
-- updating.
--
-- You can\'t make a storage capacity increase request if there is an
-- existing storage capacity increase request in progress.
--
-- For Windows file systems, the storage capacity target value must be at
-- least 10 percent greater than the current storage capacity value. To
-- increase storage capacity, the file system must have at least 16 MBps of
-- throughput capacity. For more information, see
-- <https://docs.aws.amazon.com/fsx/latest/WindowsGuide/managing-storage-capacity.html Managing storage capacity>
-- in the /Amazon FSx for Windows File Server User Guide/.
--
-- For Lustre file systems, the storage capacity target value can be the
-- following:
--
-- -   For @SCRATCH_2@, @PERSISTENT_1@, and @PERSISTENT_2 SSD@ deployment
--     types, valid values are in multiples of 2400 GiB. The value must be
--     greater than the current storage capacity.
--
-- -   For @PERSISTENT HDD@ file systems, valid values are multiples of
--     6000 GiB for 12-MBps throughput per TiB file systems and multiples
--     of 1800 GiB for 40-MBps throughput per TiB file systems. The values
--     must be greater than the current storage capacity.
--
-- -   For @SCRATCH_1@ file systems, you can\'t increase the storage
--     capacity.
--
-- For more information, see
-- <https://docs.aws.amazon.com/fsx/latest/LustreGuide/managing-storage-capacity.html Managing storage and throughput capacity>
-- in the /Amazon FSx for Lustre User Guide/.
--
-- For ONTAP file systems, the storage capacity target value must be at
-- least 10 percent greater than the current storage capacity value. For
-- more information, see
-- <https://docs.aws.amazon.com/fsx/latest/ONTAPGuide/managing-storage-capacity.html Managing storage capacity and provisioned IOPS>
-- in the /Amazon FSx for NetApp ONTAP User Guide/.
updateFileSystem_storageCapacity :: Lens.Lens' UpdateFileSystem (Prelude.Maybe Prelude.Natural)
updateFileSystem_storageCapacity = Lens.lens (\UpdateFileSystem' {storageCapacity} -> storageCapacity) (\s@UpdateFileSystem' {} a -> s {storageCapacity = a} :: UpdateFileSystem)

-- | The configuration updates for an Amazon FSx for Windows File Server file
-- system.
updateFileSystem_windowsConfiguration :: Lens.Lens' UpdateFileSystem (Prelude.Maybe UpdateFileSystemWindowsConfiguration)
updateFileSystem_windowsConfiguration = Lens.lens (\UpdateFileSystem' {windowsConfiguration} -> windowsConfiguration) (\s@UpdateFileSystem' {} a -> s {windowsConfiguration = a} :: UpdateFileSystem)

-- | The ID of the file system that you are updating.
updateFileSystem_fileSystemId :: Lens.Lens' UpdateFileSystem Prelude.Text
updateFileSystem_fileSystemId = Lens.lens (\UpdateFileSystem' {fileSystemId} -> fileSystemId) (\s@UpdateFileSystem' {} a -> s {fileSystemId = a} :: UpdateFileSystem)

instance Core.AWSRequest UpdateFileSystem where
  type
    AWSResponse UpdateFileSystem =
      UpdateFileSystemResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateFileSystemResponse'
            Prelude.<$> (x Data..?> "FileSystem")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateFileSystem where
  hashWithSalt _salt UpdateFileSystem' {..} =
    _salt
      `Prelude.hashWithSalt` clientRequestToken
      `Prelude.hashWithSalt` lustreConfiguration
      `Prelude.hashWithSalt` ontapConfiguration
      `Prelude.hashWithSalt` openZFSConfiguration
      `Prelude.hashWithSalt` storageCapacity
      `Prelude.hashWithSalt` windowsConfiguration
      `Prelude.hashWithSalt` fileSystemId

instance Prelude.NFData UpdateFileSystem where
  rnf UpdateFileSystem' {..} =
    Prelude.rnf clientRequestToken `Prelude.seq`
      Prelude.rnf lustreConfiguration `Prelude.seq`
        Prelude.rnf ontapConfiguration `Prelude.seq`
          Prelude.rnf openZFSConfiguration `Prelude.seq`
            Prelude.rnf storageCapacity `Prelude.seq`
              Prelude.rnf windowsConfiguration `Prelude.seq`
                Prelude.rnf fileSystemId

instance Data.ToHeaders UpdateFileSystem where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSSimbaAPIService_v20180301.UpdateFileSystem" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateFileSystem where
  toJSON UpdateFileSystem' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ClientRequestToken" Data..=)
              Prelude.<$> clientRequestToken,
            ("LustreConfiguration" Data..=)
              Prelude.<$> lustreConfiguration,
            ("OntapConfiguration" Data..=)
              Prelude.<$> ontapConfiguration,
            ("OpenZFSConfiguration" Data..=)
              Prelude.<$> openZFSConfiguration,
            ("StorageCapacity" Data..=)
              Prelude.<$> storageCapacity,
            ("WindowsConfiguration" Data..=)
              Prelude.<$> windowsConfiguration,
            Prelude.Just ("FileSystemId" Data..= fileSystemId)
          ]
      )

instance Data.ToPath UpdateFileSystem where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateFileSystem where
  toQuery = Prelude.const Prelude.mempty

-- | The response object for the @UpdateFileSystem@ operation.
--
-- /See:/ 'newUpdateFileSystemResponse' smart constructor.
data UpdateFileSystemResponse = UpdateFileSystemResponse'
  { -- | A description of the file system that was updated.
    fileSystem :: Prelude.Maybe FileSystem,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateFileSystemResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fileSystem', 'updateFileSystemResponse_fileSystem' - A description of the file system that was updated.
--
-- 'httpStatus', 'updateFileSystemResponse_httpStatus' - The response's http status code.
newUpdateFileSystemResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateFileSystemResponse
newUpdateFileSystemResponse pHttpStatus_ =
  UpdateFileSystemResponse'
    { fileSystem =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A description of the file system that was updated.
updateFileSystemResponse_fileSystem :: Lens.Lens' UpdateFileSystemResponse (Prelude.Maybe FileSystem)
updateFileSystemResponse_fileSystem = Lens.lens (\UpdateFileSystemResponse' {fileSystem} -> fileSystem) (\s@UpdateFileSystemResponse' {} a -> s {fileSystem = a} :: UpdateFileSystemResponse)

-- | The response's http status code.
updateFileSystemResponse_httpStatus :: Lens.Lens' UpdateFileSystemResponse Prelude.Int
updateFileSystemResponse_httpStatus = Lens.lens (\UpdateFileSystemResponse' {httpStatus} -> httpStatus) (\s@UpdateFileSystemResponse' {} a -> s {httpStatus = a} :: UpdateFileSystemResponse)

instance Prelude.NFData UpdateFileSystemResponse where
  rnf UpdateFileSystemResponse' {..} =
    Prelude.rnf fileSystem `Prelude.seq`
      Prelude.rnf httpStatus
