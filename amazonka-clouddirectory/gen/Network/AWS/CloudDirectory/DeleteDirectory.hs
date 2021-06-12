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
-- Module      : Network.AWS.CloudDirectory.DeleteDirectory
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a directory. Only disabled directories can be deleted. A deleted
-- directory cannot be undone. Exercise extreme caution when deleting
-- directories.
module Network.AWS.CloudDirectory.DeleteDirectory
  ( -- * Creating a Request
    DeleteDirectory (..),
    newDeleteDirectory,

    -- * Request Lenses
    deleteDirectory_directoryArn,

    -- * Destructuring the Response
    DeleteDirectoryResponse (..),
    newDeleteDirectoryResponse,

    -- * Response Lenses
    deleteDirectoryResponse_httpStatus,
    deleteDirectoryResponse_directoryArn,
  )
where

import Network.AWS.CloudDirectory.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteDirectory' smart constructor.
data DeleteDirectory = DeleteDirectory'
  { -- | The ARN of the directory to delete.
    directoryArn :: Core.Text
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
-- 'directoryArn', 'deleteDirectory_directoryArn' - The ARN of the directory to delete.
newDeleteDirectory ::
  -- | 'directoryArn'
  Core.Text ->
  DeleteDirectory
newDeleteDirectory pDirectoryArn_ =
  DeleteDirectory' {directoryArn = pDirectoryArn_}

-- | The ARN of the directory to delete.
deleteDirectory_directoryArn :: Lens.Lens' DeleteDirectory Core.Text
deleteDirectory_directoryArn = Lens.lens (\DeleteDirectory' {directoryArn} -> directoryArn) (\s@DeleteDirectory' {} a -> s {directoryArn = a} :: DeleteDirectory)

instance Core.AWSRequest DeleteDirectory where
  type
    AWSResponse DeleteDirectory =
      DeleteDirectoryResponse
  request = Request.putJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteDirectoryResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
            Core.<*> (x Core..:> "DirectoryArn")
      )

instance Core.Hashable DeleteDirectory

instance Core.NFData DeleteDirectory

instance Core.ToHeaders DeleteDirectory where
  toHeaders DeleteDirectory' {..} =
    Core.mconcat
      ["x-amz-data-partition" Core.=# directoryArn]

instance Core.ToJSON DeleteDirectory where
  toJSON = Core.const (Core.Object Core.mempty)

instance Core.ToPath DeleteDirectory where
  toPath =
    Core.const
      "/amazonclouddirectory/2017-01-11/directory"

instance Core.ToQuery DeleteDirectory where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeleteDirectoryResponse' smart constructor.
data DeleteDirectoryResponse = DeleteDirectoryResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int,
    -- | The ARN of the deleted directory.
    directoryArn :: Core.Text
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
-- 'httpStatus', 'deleteDirectoryResponse_httpStatus' - The response's http status code.
--
-- 'directoryArn', 'deleteDirectoryResponse_directoryArn' - The ARN of the deleted directory.
newDeleteDirectoryResponse ::
  -- | 'httpStatus'
  Core.Int ->
  -- | 'directoryArn'
  Core.Text ->
  DeleteDirectoryResponse
newDeleteDirectoryResponse
  pHttpStatus_
  pDirectoryArn_ =
    DeleteDirectoryResponse'
      { httpStatus = pHttpStatus_,
        directoryArn = pDirectoryArn_
      }

-- | The response's http status code.
deleteDirectoryResponse_httpStatus :: Lens.Lens' DeleteDirectoryResponse Core.Int
deleteDirectoryResponse_httpStatus = Lens.lens (\DeleteDirectoryResponse' {httpStatus} -> httpStatus) (\s@DeleteDirectoryResponse' {} a -> s {httpStatus = a} :: DeleteDirectoryResponse)

-- | The ARN of the deleted directory.
deleteDirectoryResponse_directoryArn :: Lens.Lens' DeleteDirectoryResponse Core.Text
deleteDirectoryResponse_directoryArn = Lens.lens (\DeleteDirectoryResponse' {directoryArn} -> directoryArn) (\s@DeleteDirectoryResponse' {} a -> s {directoryArn = a} :: DeleteDirectoryResponse)

instance Core.NFData DeleteDirectoryResponse
