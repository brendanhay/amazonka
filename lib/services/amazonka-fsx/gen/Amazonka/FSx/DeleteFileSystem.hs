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
-- Module      : Amazonka.FSx.DeleteFileSystem
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a file system. After deletion, the file system no longer exists,
-- and its data is gone. Any existing automatic backups and snapshots are
-- also deleted.
--
-- To delete an Amazon FSx for NetApp ONTAP file system, first delete all
-- the volumes and storage virtual machines (SVMs) on the file system. Then
-- provide a @FileSystemId@ value to the @DeleFileSystem@ operation.
--
-- By default, when you delete an Amazon FSx for Windows File Server file
-- system, a final backup is created upon deletion. This final backup
-- isn\'t subject to the file system\'s retention policy, and must be
-- manually deleted.
--
-- The @DeleteFileSystem@ operation returns while the file system has the
-- @DELETING@ status. You can check the file system deletion status by
-- calling the
-- <https://docs.aws.amazon.com/fsx/latest/APIReference/API_DescribeFileSystems.html DescribeFileSystems>
-- operation, which returns a list of file systems in your account. If you
-- pass the file system ID for a deleted file system, the
-- @DescribeFileSystems@ operation returns a @FileSystemNotFound@ error.
--
-- If a data repository task is in a @PENDING@ or @EXECUTING@ state,
-- deleting an Amazon FSx for Lustre file system will fail with an HTTP
-- status code 400 (Bad Request).
--
-- The data in a deleted file system is also deleted and can\'t be
-- recovered by any means.
module Amazonka.FSx.DeleteFileSystem
  ( -- * Creating a Request
    DeleteFileSystem (..),
    newDeleteFileSystem,

    -- * Request Lenses
    deleteFileSystem_clientRequestToken,
    deleteFileSystem_lustreConfiguration,
    deleteFileSystem_openZFSConfiguration,
    deleteFileSystem_windowsConfiguration,
    deleteFileSystem_fileSystemId,

    -- * Destructuring the Response
    DeleteFileSystemResponse (..),
    newDeleteFileSystemResponse,

    -- * Response Lenses
    deleteFileSystemResponse_fileSystemId,
    deleteFileSystemResponse_lifecycle,
    deleteFileSystemResponse_lustreResponse,
    deleteFileSystemResponse_openZFSResponse,
    deleteFileSystemResponse_windowsResponse,
    deleteFileSystemResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FSx.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | The request object for @DeleteFileSystem@ operation.
--
-- /See:/ 'newDeleteFileSystem' smart constructor.
data DeleteFileSystem = DeleteFileSystem'
  { -- | A string of up to 63 ASCII characters that Amazon FSx uses to ensure
    -- idempotent deletion. This token is automatically filled on your behalf
    -- when using the Command Line Interface (CLI) or an Amazon Web Services
    -- SDK.
    clientRequestToken :: Prelude.Maybe Prelude.Text,
    lustreConfiguration :: Prelude.Maybe DeleteFileSystemLustreConfiguration,
    -- | The configuration object for the OpenZFS file system used in the
    -- @DeleteFileSystem@ operation.
    openZFSConfiguration :: Prelude.Maybe DeleteFileSystemOpenZFSConfiguration,
    windowsConfiguration :: Prelude.Maybe DeleteFileSystemWindowsConfiguration,
    -- | The ID of the file system that you want to delete.
    fileSystemId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteFileSystem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientRequestToken', 'deleteFileSystem_clientRequestToken' - A string of up to 63 ASCII characters that Amazon FSx uses to ensure
-- idempotent deletion. This token is automatically filled on your behalf
-- when using the Command Line Interface (CLI) or an Amazon Web Services
-- SDK.
--
-- 'lustreConfiguration', 'deleteFileSystem_lustreConfiguration' - Undocumented member.
--
-- 'openZFSConfiguration', 'deleteFileSystem_openZFSConfiguration' - The configuration object for the OpenZFS file system used in the
-- @DeleteFileSystem@ operation.
--
-- 'windowsConfiguration', 'deleteFileSystem_windowsConfiguration' - Undocumented member.
--
-- 'fileSystemId', 'deleteFileSystem_fileSystemId' - The ID of the file system that you want to delete.
newDeleteFileSystem ::
  -- | 'fileSystemId'
  Prelude.Text ->
  DeleteFileSystem
newDeleteFileSystem pFileSystemId_ =
  DeleteFileSystem'
    { clientRequestToken =
        Prelude.Nothing,
      lustreConfiguration = Prelude.Nothing,
      openZFSConfiguration = Prelude.Nothing,
      windowsConfiguration = Prelude.Nothing,
      fileSystemId = pFileSystemId_
    }

-- | A string of up to 63 ASCII characters that Amazon FSx uses to ensure
-- idempotent deletion. This token is automatically filled on your behalf
-- when using the Command Line Interface (CLI) or an Amazon Web Services
-- SDK.
deleteFileSystem_clientRequestToken :: Lens.Lens' DeleteFileSystem (Prelude.Maybe Prelude.Text)
deleteFileSystem_clientRequestToken = Lens.lens (\DeleteFileSystem' {clientRequestToken} -> clientRequestToken) (\s@DeleteFileSystem' {} a -> s {clientRequestToken = a} :: DeleteFileSystem)

-- | Undocumented member.
deleteFileSystem_lustreConfiguration :: Lens.Lens' DeleteFileSystem (Prelude.Maybe DeleteFileSystemLustreConfiguration)
deleteFileSystem_lustreConfiguration = Lens.lens (\DeleteFileSystem' {lustreConfiguration} -> lustreConfiguration) (\s@DeleteFileSystem' {} a -> s {lustreConfiguration = a} :: DeleteFileSystem)

-- | The configuration object for the OpenZFS file system used in the
-- @DeleteFileSystem@ operation.
deleteFileSystem_openZFSConfiguration :: Lens.Lens' DeleteFileSystem (Prelude.Maybe DeleteFileSystemOpenZFSConfiguration)
deleteFileSystem_openZFSConfiguration = Lens.lens (\DeleteFileSystem' {openZFSConfiguration} -> openZFSConfiguration) (\s@DeleteFileSystem' {} a -> s {openZFSConfiguration = a} :: DeleteFileSystem)

-- | Undocumented member.
deleteFileSystem_windowsConfiguration :: Lens.Lens' DeleteFileSystem (Prelude.Maybe DeleteFileSystemWindowsConfiguration)
deleteFileSystem_windowsConfiguration = Lens.lens (\DeleteFileSystem' {windowsConfiguration} -> windowsConfiguration) (\s@DeleteFileSystem' {} a -> s {windowsConfiguration = a} :: DeleteFileSystem)

-- | The ID of the file system that you want to delete.
deleteFileSystem_fileSystemId :: Lens.Lens' DeleteFileSystem Prelude.Text
deleteFileSystem_fileSystemId = Lens.lens (\DeleteFileSystem' {fileSystemId} -> fileSystemId) (\s@DeleteFileSystem' {} a -> s {fileSystemId = a} :: DeleteFileSystem)

instance Core.AWSRequest DeleteFileSystem where
  type
    AWSResponse DeleteFileSystem =
      DeleteFileSystemResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteFileSystemResponse'
            Prelude.<$> (x Data..?> "FileSystemId")
            Prelude.<*> (x Data..?> "Lifecycle")
            Prelude.<*> (x Data..?> "LustreResponse")
            Prelude.<*> (x Data..?> "OpenZFSResponse")
            Prelude.<*> (x Data..?> "WindowsResponse")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteFileSystem where
  hashWithSalt _salt DeleteFileSystem' {..} =
    _salt
      `Prelude.hashWithSalt` clientRequestToken
      `Prelude.hashWithSalt` lustreConfiguration
      `Prelude.hashWithSalt` openZFSConfiguration
      `Prelude.hashWithSalt` windowsConfiguration
      `Prelude.hashWithSalt` fileSystemId

instance Prelude.NFData DeleteFileSystem where
  rnf DeleteFileSystem' {..} =
    Prelude.rnf clientRequestToken
      `Prelude.seq` Prelude.rnf lustreConfiguration
      `Prelude.seq` Prelude.rnf openZFSConfiguration
      `Prelude.seq` Prelude.rnf windowsConfiguration
      `Prelude.seq` Prelude.rnf fileSystemId

instance Data.ToHeaders DeleteFileSystem where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSSimbaAPIService_v20180301.DeleteFileSystem" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteFileSystem where
  toJSON DeleteFileSystem' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ClientRequestToken" Data..=)
              Prelude.<$> clientRequestToken,
            ("LustreConfiguration" Data..=)
              Prelude.<$> lustreConfiguration,
            ("OpenZFSConfiguration" Data..=)
              Prelude.<$> openZFSConfiguration,
            ("WindowsConfiguration" Data..=)
              Prelude.<$> windowsConfiguration,
            Prelude.Just ("FileSystemId" Data..= fileSystemId)
          ]
      )

instance Data.ToPath DeleteFileSystem where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteFileSystem where
  toQuery = Prelude.const Prelude.mempty

-- | The response object for the @DeleteFileSystem@ operation.
--
-- /See:/ 'newDeleteFileSystemResponse' smart constructor.
data DeleteFileSystemResponse = DeleteFileSystemResponse'
  { -- | The ID of the file system that\'s being deleted.
    fileSystemId :: Prelude.Maybe Prelude.Text,
    -- | The file system lifecycle for the deletion request. If the
    -- @DeleteFileSystem@ operation is successful, this status is @DELETING@.
    lifecycle :: Prelude.Maybe FileSystemLifecycle,
    lustreResponse :: Prelude.Maybe DeleteFileSystemLustreResponse,
    -- | The response object for the OpenZFS file system that\'s being deleted in
    -- the @DeleteFileSystem@ operation.
    openZFSResponse :: Prelude.Maybe DeleteFileSystemOpenZFSResponse,
    windowsResponse :: Prelude.Maybe DeleteFileSystemWindowsResponse,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteFileSystemResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fileSystemId', 'deleteFileSystemResponse_fileSystemId' - The ID of the file system that\'s being deleted.
--
-- 'lifecycle', 'deleteFileSystemResponse_lifecycle' - The file system lifecycle for the deletion request. If the
-- @DeleteFileSystem@ operation is successful, this status is @DELETING@.
--
-- 'lustreResponse', 'deleteFileSystemResponse_lustreResponse' - Undocumented member.
--
-- 'openZFSResponse', 'deleteFileSystemResponse_openZFSResponse' - The response object for the OpenZFS file system that\'s being deleted in
-- the @DeleteFileSystem@ operation.
--
-- 'windowsResponse', 'deleteFileSystemResponse_windowsResponse' - Undocumented member.
--
-- 'httpStatus', 'deleteFileSystemResponse_httpStatus' - The response's http status code.
newDeleteFileSystemResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteFileSystemResponse
newDeleteFileSystemResponse pHttpStatus_ =
  DeleteFileSystemResponse'
    { fileSystemId =
        Prelude.Nothing,
      lifecycle = Prelude.Nothing,
      lustreResponse = Prelude.Nothing,
      openZFSResponse = Prelude.Nothing,
      windowsResponse = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ID of the file system that\'s being deleted.
deleteFileSystemResponse_fileSystemId :: Lens.Lens' DeleteFileSystemResponse (Prelude.Maybe Prelude.Text)
deleteFileSystemResponse_fileSystemId = Lens.lens (\DeleteFileSystemResponse' {fileSystemId} -> fileSystemId) (\s@DeleteFileSystemResponse' {} a -> s {fileSystemId = a} :: DeleteFileSystemResponse)

-- | The file system lifecycle for the deletion request. If the
-- @DeleteFileSystem@ operation is successful, this status is @DELETING@.
deleteFileSystemResponse_lifecycle :: Lens.Lens' DeleteFileSystemResponse (Prelude.Maybe FileSystemLifecycle)
deleteFileSystemResponse_lifecycle = Lens.lens (\DeleteFileSystemResponse' {lifecycle} -> lifecycle) (\s@DeleteFileSystemResponse' {} a -> s {lifecycle = a} :: DeleteFileSystemResponse)

-- | Undocumented member.
deleteFileSystemResponse_lustreResponse :: Lens.Lens' DeleteFileSystemResponse (Prelude.Maybe DeleteFileSystemLustreResponse)
deleteFileSystemResponse_lustreResponse = Lens.lens (\DeleteFileSystemResponse' {lustreResponse} -> lustreResponse) (\s@DeleteFileSystemResponse' {} a -> s {lustreResponse = a} :: DeleteFileSystemResponse)

-- | The response object for the OpenZFS file system that\'s being deleted in
-- the @DeleteFileSystem@ operation.
deleteFileSystemResponse_openZFSResponse :: Lens.Lens' DeleteFileSystemResponse (Prelude.Maybe DeleteFileSystemOpenZFSResponse)
deleteFileSystemResponse_openZFSResponse = Lens.lens (\DeleteFileSystemResponse' {openZFSResponse} -> openZFSResponse) (\s@DeleteFileSystemResponse' {} a -> s {openZFSResponse = a} :: DeleteFileSystemResponse)

-- | Undocumented member.
deleteFileSystemResponse_windowsResponse :: Lens.Lens' DeleteFileSystemResponse (Prelude.Maybe DeleteFileSystemWindowsResponse)
deleteFileSystemResponse_windowsResponse = Lens.lens (\DeleteFileSystemResponse' {windowsResponse} -> windowsResponse) (\s@DeleteFileSystemResponse' {} a -> s {windowsResponse = a} :: DeleteFileSystemResponse)

-- | The response's http status code.
deleteFileSystemResponse_httpStatus :: Lens.Lens' DeleteFileSystemResponse Prelude.Int
deleteFileSystemResponse_httpStatus = Lens.lens (\DeleteFileSystemResponse' {httpStatus} -> httpStatus) (\s@DeleteFileSystemResponse' {} a -> s {httpStatus = a} :: DeleteFileSystemResponse)

instance Prelude.NFData DeleteFileSystemResponse where
  rnf DeleteFileSystemResponse' {..} =
    Prelude.rnf fileSystemId
      `Prelude.seq` Prelude.rnf lifecycle
      `Prelude.seq` Prelude.rnf lustreResponse
      `Prelude.seq` Prelude.rnf openZFSResponse
      `Prelude.seq` Prelude.rnf windowsResponse
      `Prelude.seq` Prelude.rnf httpStatus
