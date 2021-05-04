{-# LANGUAGE DeriveDataTypeable #-}
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

import Network.AWS.DirectoryService.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the inputs for the DeleteDirectory operation.
--
-- /See:/ 'newDeleteDirectory' smart constructor.
data DeleteDirectory = DeleteDirectory'
  { -- | The identifier of the directory to delete.
    directoryId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  DeleteDirectory
newDeleteDirectory pDirectoryId_ =
  DeleteDirectory' {directoryId = pDirectoryId_}

-- | The identifier of the directory to delete.
deleteDirectory_directoryId :: Lens.Lens' DeleteDirectory Prelude.Text
deleteDirectory_directoryId = Lens.lens (\DeleteDirectory' {directoryId} -> directoryId) (\s@DeleteDirectory' {} a -> s {directoryId = a} :: DeleteDirectory)

instance Prelude.AWSRequest DeleteDirectory where
  type Rs DeleteDirectory = DeleteDirectoryResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteDirectoryResponse'
            Prelude.<$> (x Prelude..?> "DirectoryId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteDirectory

instance Prelude.NFData DeleteDirectory

instance Prelude.ToHeaders DeleteDirectory where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "DirectoryService_20150416.DeleteDirectory" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DeleteDirectory where
  toJSON DeleteDirectory' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("DirectoryId" Prelude..= directoryId)
          ]
      )

instance Prelude.ToPath DeleteDirectory where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeleteDirectory where
  toQuery = Prelude.const Prelude.mempty

-- | Contains the results of the DeleteDirectory operation.
--
-- /See:/ 'newDeleteDirectoryResponse' smart constructor.
data DeleteDirectoryResponse = DeleteDirectoryResponse'
  { -- | The directory identifier.
    directoryId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  DeleteDirectoryResponse
newDeleteDirectoryResponse pHttpStatus_ =
  DeleteDirectoryResponse'
    { directoryId =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The directory identifier.
deleteDirectoryResponse_directoryId :: Lens.Lens' DeleteDirectoryResponse (Prelude.Maybe Prelude.Text)
deleteDirectoryResponse_directoryId = Lens.lens (\DeleteDirectoryResponse' {directoryId} -> directoryId) (\s@DeleteDirectoryResponse' {} a -> s {directoryId = a} :: DeleteDirectoryResponse)

-- | The response's http status code.
deleteDirectoryResponse_httpStatus :: Lens.Lens' DeleteDirectoryResponse Prelude.Int
deleteDirectoryResponse_httpStatus = Lens.lens (\DeleteDirectoryResponse' {httpStatus} -> httpStatus) (\s@DeleteDirectoryResponse' {} a -> s {httpStatus = a} :: DeleteDirectoryResponse)

instance Prelude.NFData DeleteDirectoryResponse
