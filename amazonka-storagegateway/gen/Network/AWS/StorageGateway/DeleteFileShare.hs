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
-- Module      : Network.AWS.StorageGateway.DeleteFileShare
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a file share from a file gateway. This operation is only
-- supported for file gateways.
module Network.AWS.StorageGateway.DeleteFileShare
  ( -- * Creating a Request
    DeleteFileShare (..),
    newDeleteFileShare,

    -- * Request Lenses
    deleteFileShare_forceDelete,
    deleteFileShare_fileShareARN,

    -- * Destructuring the Response
    DeleteFileShareResponse (..),
    newDeleteFileShareResponse,

    -- * Response Lenses
    deleteFileShareResponse_fileShareARN,
    deleteFileShareResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.StorageGateway.Types

-- | DeleteFileShareInput
--
-- /See:/ 'newDeleteFileShare' smart constructor.
data DeleteFileShare = DeleteFileShare'
  { -- | If this value is set to @true@, the operation deletes a file share
    -- immediately and aborts all data uploads to AWS. Otherwise, the file
    -- share is not deleted until all data is uploaded to AWS. This process
    -- aborts the data upload process, and the file share enters the
    -- @FORCE_DELETING@ status.
    --
    -- Valid Values: @true@ | @false@
    forceDelete :: Core.Maybe Core.Bool,
    -- | The Amazon Resource Name (ARN) of the file share to be deleted.
    fileShareARN :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteFileShare' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'forceDelete', 'deleteFileShare_forceDelete' - If this value is set to @true@, the operation deletes a file share
-- immediately and aborts all data uploads to AWS. Otherwise, the file
-- share is not deleted until all data is uploaded to AWS. This process
-- aborts the data upload process, and the file share enters the
-- @FORCE_DELETING@ status.
--
-- Valid Values: @true@ | @false@
--
-- 'fileShareARN', 'deleteFileShare_fileShareARN' - The Amazon Resource Name (ARN) of the file share to be deleted.
newDeleteFileShare ::
  -- | 'fileShareARN'
  Core.Text ->
  DeleteFileShare
newDeleteFileShare pFileShareARN_ =
  DeleteFileShare'
    { forceDelete = Core.Nothing,
      fileShareARN = pFileShareARN_
    }

-- | If this value is set to @true@, the operation deletes a file share
-- immediately and aborts all data uploads to AWS. Otherwise, the file
-- share is not deleted until all data is uploaded to AWS. This process
-- aborts the data upload process, and the file share enters the
-- @FORCE_DELETING@ status.
--
-- Valid Values: @true@ | @false@
deleteFileShare_forceDelete :: Lens.Lens' DeleteFileShare (Core.Maybe Core.Bool)
deleteFileShare_forceDelete = Lens.lens (\DeleteFileShare' {forceDelete} -> forceDelete) (\s@DeleteFileShare' {} a -> s {forceDelete = a} :: DeleteFileShare)

-- | The Amazon Resource Name (ARN) of the file share to be deleted.
deleteFileShare_fileShareARN :: Lens.Lens' DeleteFileShare Core.Text
deleteFileShare_fileShareARN = Lens.lens (\DeleteFileShare' {fileShareARN} -> fileShareARN) (\s@DeleteFileShare' {} a -> s {fileShareARN = a} :: DeleteFileShare)

instance Core.AWSRequest DeleteFileShare where
  type
    AWSResponse DeleteFileShare =
      DeleteFileShareResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteFileShareResponse'
            Core.<$> (x Core..?> "FileShareARN")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DeleteFileShare

instance Core.NFData DeleteFileShare

instance Core.ToHeaders DeleteFileShare where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "StorageGateway_20130630.DeleteFileShare" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DeleteFileShare where
  toJSON DeleteFileShare' {..} =
    Core.object
      ( Core.catMaybes
          [ ("ForceDelete" Core..=) Core.<$> forceDelete,
            Core.Just ("FileShareARN" Core..= fileShareARN)
          ]
      )

instance Core.ToPath DeleteFileShare where
  toPath = Core.const "/"

instance Core.ToQuery DeleteFileShare where
  toQuery = Core.const Core.mempty

-- | DeleteFileShareOutput
--
-- /See:/ 'newDeleteFileShareResponse' smart constructor.
data DeleteFileShareResponse = DeleteFileShareResponse'
  { -- | The Amazon Resource Name (ARN) of the deleted file share.
    fileShareARN :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteFileShareResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fileShareARN', 'deleteFileShareResponse_fileShareARN' - The Amazon Resource Name (ARN) of the deleted file share.
--
-- 'httpStatus', 'deleteFileShareResponse_httpStatus' - The response's http status code.
newDeleteFileShareResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DeleteFileShareResponse
newDeleteFileShareResponse pHttpStatus_ =
  DeleteFileShareResponse'
    { fileShareARN =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the deleted file share.
deleteFileShareResponse_fileShareARN :: Lens.Lens' DeleteFileShareResponse (Core.Maybe Core.Text)
deleteFileShareResponse_fileShareARN = Lens.lens (\DeleteFileShareResponse' {fileShareARN} -> fileShareARN) (\s@DeleteFileShareResponse' {} a -> s {fileShareARN = a} :: DeleteFileShareResponse)

-- | The response's http status code.
deleteFileShareResponse_httpStatus :: Lens.Lens' DeleteFileShareResponse Core.Int
deleteFileShareResponse_httpStatus = Lens.lens (\DeleteFileShareResponse' {httpStatus} -> httpStatus) (\s@DeleteFileShareResponse' {} a -> s {httpStatus = a} :: DeleteFileShareResponse)

instance Core.NFData DeleteFileShareResponse
