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
-- Module      : Amazonka.EFS.DeleteFileSystem
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a file system, permanently severing access to its contents. Upon
-- return, the file system no longer exists and you can\'t access any
-- contents of the deleted file system.
--
-- You need to manually delete mount targets attached to a file system
-- before you can delete an EFS file system. This step is performed for you
-- when you use the Amazon Web Services console to delete a file system.
--
-- You cannot delete a file system that is part of an EFS Replication
-- configuration. You need to delete the replication configuration first.
--
-- You can\'t delete a file system that is in use. That is, if the file
-- system has any mount targets, you must first delete them. For more
-- information, see DescribeMountTargets and DeleteMountTarget.
--
-- The @DeleteFileSystem@ call returns while the file system state is still
-- @deleting@. You can check the file system deletion status by calling the
-- DescribeFileSystems operation, which returns a list of file systems in
-- your account. If you pass file system ID or creation token for the
-- deleted file system, the DescribeFileSystems returns a
-- @404 FileSystemNotFound@ error.
--
-- This operation requires permissions for the
-- @elasticfilesystem:DeleteFileSystem@ action.
module Amazonka.EFS.DeleteFileSystem
  ( -- * Creating a Request
    DeleteFileSystem (..),
    newDeleteFileSystem,

    -- * Request Lenses
    deleteFileSystem_fileSystemId,

    -- * Destructuring the Response
    DeleteFileSystemResponse (..),
    newDeleteFileSystemResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EFS.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- |
--
-- /See:/ 'newDeleteFileSystem' smart constructor.
data DeleteFileSystem = DeleteFileSystem'
  { -- | The ID of the file system you want to delete.
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
-- 'fileSystemId', 'deleteFileSystem_fileSystemId' - The ID of the file system you want to delete.
newDeleteFileSystem ::
  -- | 'fileSystemId'
  Prelude.Text ->
  DeleteFileSystem
newDeleteFileSystem pFileSystemId_ =
  DeleteFileSystem' {fileSystemId = pFileSystemId_}

-- | The ID of the file system you want to delete.
deleteFileSystem_fileSystemId :: Lens.Lens' DeleteFileSystem Prelude.Text
deleteFileSystem_fileSystemId = Lens.lens (\DeleteFileSystem' {fileSystemId} -> fileSystemId) (\s@DeleteFileSystem' {} a -> s {fileSystemId = a} :: DeleteFileSystem)

instance Core.AWSRequest DeleteFileSystem where
  type
    AWSResponse DeleteFileSystem =
      DeleteFileSystemResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveNull DeleteFileSystemResponse'

instance Prelude.Hashable DeleteFileSystem where
  hashWithSalt _salt DeleteFileSystem' {..} =
    _salt `Prelude.hashWithSalt` fileSystemId

instance Prelude.NFData DeleteFileSystem where
  rnf DeleteFileSystem' {..} = Prelude.rnf fileSystemId

instance Data.ToHeaders DeleteFileSystem where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DeleteFileSystem where
  toPath DeleteFileSystem' {..} =
    Prelude.mconcat
      ["/2015-02-01/file-systems/", Data.toBS fileSystemId]

instance Data.ToQuery DeleteFileSystem where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteFileSystemResponse' smart constructor.
data DeleteFileSystemResponse = DeleteFileSystemResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteFileSystemResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteFileSystemResponse ::
  DeleteFileSystemResponse
newDeleteFileSystemResponse =
  DeleteFileSystemResponse'

instance Prelude.NFData DeleteFileSystemResponse where
  rnf _ = ()
