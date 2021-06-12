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
-- Module      : Network.AWS.CloudWatchEvents.DeleteArchive
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified archive.
module Network.AWS.CloudWatchEvents.DeleteArchive
  ( -- * Creating a Request
    DeleteArchive (..),
    newDeleteArchive,

    -- * Request Lenses
    deleteArchive_archiveName,

    -- * Destructuring the Response
    DeleteArchiveResponse (..),
    newDeleteArchiveResponse,

    -- * Response Lenses
    deleteArchiveResponse_httpStatus,
  )
where

import Network.AWS.CloudWatchEvents.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteArchive' smart constructor.
data DeleteArchive = DeleteArchive'
  { -- | The name of the archive to delete.
    archiveName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteArchive' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'archiveName', 'deleteArchive_archiveName' - The name of the archive to delete.
newDeleteArchive ::
  -- | 'archiveName'
  Core.Text ->
  DeleteArchive
newDeleteArchive pArchiveName_ =
  DeleteArchive' {archiveName = pArchiveName_}

-- | The name of the archive to delete.
deleteArchive_archiveName :: Lens.Lens' DeleteArchive Core.Text
deleteArchive_archiveName = Lens.lens (\DeleteArchive' {archiveName} -> archiveName) (\s@DeleteArchive' {} a -> s {archiveName = a} :: DeleteArchive)

instance Core.AWSRequest DeleteArchive where
  type
    AWSResponse DeleteArchive =
      DeleteArchiveResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteArchiveResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DeleteArchive

instance Core.NFData DeleteArchive

instance Core.ToHeaders DeleteArchive where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("AWSEvents.DeleteArchive" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DeleteArchive where
  toJSON DeleteArchive' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("ArchiveName" Core..= archiveName)]
      )

instance Core.ToPath DeleteArchive where
  toPath = Core.const "/"

instance Core.ToQuery DeleteArchive where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeleteArchiveResponse' smart constructor.
data DeleteArchiveResponse = DeleteArchiveResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteArchiveResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteArchiveResponse_httpStatus' - The response's http status code.
newDeleteArchiveResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DeleteArchiveResponse
newDeleteArchiveResponse pHttpStatus_ =
  DeleteArchiveResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
deleteArchiveResponse_httpStatus :: Lens.Lens' DeleteArchiveResponse Core.Int
deleteArchiveResponse_httpStatus = Lens.lens (\DeleteArchiveResponse' {httpStatus} -> httpStatus) (\s@DeleteArchiveResponse' {} a -> s {httpStatus = a} :: DeleteArchiveResponse)

instance Core.NFData DeleteArchiveResponse
