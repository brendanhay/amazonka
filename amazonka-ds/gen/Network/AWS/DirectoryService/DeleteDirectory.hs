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
-- Module      : Network.AWS.DirectoryService.DeleteDirectory
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an AWS Directory Service directory.
--
-- Before you call @DeleteDirectory@, ensure that all of the required
-- permissions have been explicitly granted through a policy. For details
-- about what permissions are required to run the @DeleteDirectory@
-- operation, see
-- <http://docs.aws.amazon.com/directoryservice/latest/admin-guide/UsingWithDS_IAM_ResourcePermissions.html AWS Directory Service API Permissions: Actions, Resources, and Conditions Reference>.
module Network.AWS.DirectoryService.DeleteDirectory
  ( -- * Creating a Request
    DeleteDirectory (..),
    newDeleteDirectory,

    -- * Request Lenses
    deleteDirectory_directoryId,

    -- * Destructuring the Response
    DeleteDirectoryResponse (..),
    newDeleteDirectoryResponse,

    -- * Response Lenses
    deleteDirectoryResponse_directoryId,
    deleteDirectoryResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.DirectoryService.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the inputs for the DeleteDirectory operation.
--
-- /See:/ 'newDeleteDirectory' smart constructor.
data DeleteDirectory = DeleteDirectory'
  { -- | The identifier of the directory to delete.
    directoryId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteDirectory' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'directoryId', 'deleteDirectory_directoryId' - The identifier of the directory to delete.
newDeleteDirectory ::
  -- | 'directoryId'
  Core.Text ->
  DeleteDirectory
newDeleteDirectory pDirectoryId_ =
  DeleteDirectory' {directoryId = pDirectoryId_}

-- | The identifier of the directory to delete.
deleteDirectory_directoryId :: Lens.Lens' DeleteDirectory Core.Text
deleteDirectory_directoryId = Lens.lens (\DeleteDirectory' {directoryId} -> directoryId) (\s@DeleteDirectory' {} a -> s {directoryId = a} :: DeleteDirectory)

instance Core.AWSRequest DeleteDirectory where
  type
    AWSResponse DeleteDirectory =
      DeleteDirectoryResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteDirectoryResponse'
            Core.<$> (x Core..?> "DirectoryId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DeleteDirectory

instance Core.NFData DeleteDirectory

instance Core.ToHeaders DeleteDirectory where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "DirectoryService_20150416.DeleteDirectory" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DeleteDirectory where
  toJSON DeleteDirectory' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("DirectoryId" Core..= directoryId)]
      )

instance Core.ToPath DeleteDirectory where
  toPath = Core.const "/"

instance Core.ToQuery DeleteDirectory where
  toQuery = Core.const Core.mempty

-- | Contains the results of the DeleteDirectory operation.
--
-- /See:/ 'newDeleteDirectoryResponse' smart constructor.
data DeleteDirectoryResponse = DeleteDirectoryResponse'
  { -- | The directory identifier.
    directoryId :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteDirectoryResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'directoryId', 'deleteDirectoryResponse_directoryId' - The directory identifier.
--
-- 'httpStatus', 'deleteDirectoryResponse_httpStatus' - The response's http status code.
newDeleteDirectoryResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DeleteDirectoryResponse
newDeleteDirectoryResponse pHttpStatus_ =
  DeleteDirectoryResponse'
    { directoryId =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The directory identifier.
deleteDirectoryResponse_directoryId :: Lens.Lens' DeleteDirectoryResponse (Core.Maybe Core.Text)
deleteDirectoryResponse_directoryId = Lens.lens (\DeleteDirectoryResponse' {directoryId} -> directoryId) (\s@DeleteDirectoryResponse' {} a -> s {directoryId = a} :: DeleteDirectoryResponse)

-- | The response's http status code.
deleteDirectoryResponse_httpStatus :: Lens.Lens' DeleteDirectoryResponse Core.Int
deleteDirectoryResponse_httpStatus = Lens.lens (\DeleteDirectoryResponse' {httpStatus} -> httpStatus) (\s@DeleteDirectoryResponse' {} a -> s {httpStatus = a} :: DeleteDirectoryResponse)

instance Core.NFData DeleteDirectoryResponse
