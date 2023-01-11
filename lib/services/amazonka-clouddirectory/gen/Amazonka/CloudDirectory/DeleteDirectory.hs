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
-- Module      : Amazonka.CloudDirectory.DeleteDirectory
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a directory. Only disabled directories can be deleted. A deleted
-- directory cannot be undone. Exercise extreme caution when deleting
-- directories.
module Amazonka.CloudDirectory.DeleteDirectory
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

import Amazonka.CloudDirectory.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteDirectory' smart constructor.
data DeleteDirectory = DeleteDirectory'
  { -- | The ARN of the directory to delete.
    directoryArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  DeleteDirectory
newDeleteDirectory pDirectoryArn_ =
  DeleteDirectory' {directoryArn = pDirectoryArn_}

-- | The ARN of the directory to delete.
deleteDirectory_directoryArn :: Lens.Lens' DeleteDirectory Prelude.Text
deleteDirectory_directoryArn = Lens.lens (\DeleteDirectory' {directoryArn} -> directoryArn) (\s@DeleteDirectory' {} a -> s {directoryArn = a} :: DeleteDirectory)

instance Core.AWSRequest DeleteDirectory where
  type
    AWSResponse DeleteDirectory =
      DeleteDirectoryResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteDirectoryResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "DirectoryArn")
      )

instance Prelude.Hashable DeleteDirectory where
  hashWithSalt _salt DeleteDirectory' {..} =
    _salt `Prelude.hashWithSalt` directoryArn

instance Prelude.NFData DeleteDirectory where
  rnf DeleteDirectory' {..} = Prelude.rnf directoryArn

instance Data.ToHeaders DeleteDirectory where
  toHeaders DeleteDirectory' {..} =
    Prelude.mconcat
      ["x-amz-data-partition" Data.=# directoryArn]

instance Data.ToJSON DeleteDirectory where
  toJSON = Prelude.const (Data.Object Prelude.mempty)

instance Data.ToPath DeleteDirectory where
  toPath =
    Prelude.const
      "/amazonclouddirectory/2017-01-11/directory"

instance Data.ToQuery DeleteDirectory where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteDirectoryResponse' smart constructor.
data DeleteDirectoryResponse = DeleteDirectoryResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The ARN of the deleted directory.
    directoryArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  -- | 'directoryArn'
  Prelude.Text ->
  DeleteDirectoryResponse
newDeleteDirectoryResponse
  pHttpStatus_
  pDirectoryArn_ =
    DeleteDirectoryResponse'
      { httpStatus = pHttpStatus_,
        directoryArn = pDirectoryArn_
      }

-- | The response's http status code.
deleteDirectoryResponse_httpStatus :: Lens.Lens' DeleteDirectoryResponse Prelude.Int
deleteDirectoryResponse_httpStatus = Lens.lens (\DeleteDirectoryResponse' {httpStatus} -> httpStatus) (\s@DeleteDirectoryResponse' {} a -> s {httpStatus = a} :: DeleteDirectoryResponse)

-- | The ARN of the deleted directory.
deleteDirectoryResponse_directoryArn :: Lens.Lens' DeleteDirectoryResponse Prelude.Text
deleteDirectoryResponse_directoryArn = Lens.lens (\DeleteDirectoryResponse' {directoryArn} -> directoryArn) (\s@DeleteDirectoryResponse' {} a -> s {directoryArn = a} :: DeleteDirectoryResponse)

instance Prelude.NFData DeleteDirectoryResponse where
  rnf DeleteDirectoryResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf directoryArn
