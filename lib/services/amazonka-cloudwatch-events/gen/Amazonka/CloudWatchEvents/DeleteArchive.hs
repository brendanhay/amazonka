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
-- Module      : Amazonka.CloudWatchEvents.DeleteArchive
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified archive.
module Amazonka.CloudWatchEvents.DeleteArchive
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

import Amazonka.CloudWatchEvents.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteArchive' smart constructor.
data DeleteArchive = DeleteArchive'
  { -- | The name of the archive to delete.
    archiveName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  DeleteArchive
newDeleteArchive pArchiveName_ =
  DeleteArchive' {archiveName = pArchiveName_}

-- | The name of the archive to delete.
deleteArchive_archiveName :: Lens.Lens' DeleteArchive Prelude.Text
deleteArchive_archiveName = Lens.lens (\DeleteArchive' {archiveName} -> archiveName) (\s@DeleteArchive' {} a -> s {archiveName = a} :: DeleteArchive)

instance Core.AWSRequest DeleteArchive where
  type
    AWSResponse DeleteArchive =
      DeleteArchiveResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteArchiveResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteArchive where
  hashWithSalt _salt DeleteArchive' {..} =
    _salt `Prelude.hashWithSalt` archiveName

instance Prelude.NFData DeleteArchive where
  rnf DeleteArchive' {..} = Prelude.rnf archiveName

instance Data.ToHeaders DeleteArchive where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("AWSEvents.DeleteArchive" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteArchive where
  toJSON DeleteArchive' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("ArchiveName" Data..= archiveName)]
      )

instance Data.ToPath DeleteArchive where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteArchive where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteArchiveResponse' smart constructor.
data DeleteArchiveResponse = DeleteArchiveResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  DeleteArchiveResponse
newDeleteArchiveResponse pHttpStatus_ =
  DeleteArchiveResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
deleteArchiveResponse_httpStatus :: Lens.Lens' DeleteArchiveResponse Prelude.Int
deleteArchiveResponse_httpStatus = Lens.lens (\DeleteArchiveResponse' {httpStatus} -> httpStatus) (\s@DeleteArchiveResponse' {} a -> s {httpStatus = a} :: DeleteArchiveResponse)

instance Prelude.NFData DeleteArchiveResponse where
  rnf DeleteArchiveResponse' {..} =
    Prelude.rnf httpStatus
