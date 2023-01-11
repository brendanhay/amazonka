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
-- Module      : Amazonka.LexV2Models.DeleteExport
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes a previous export and the associated files stored in an S3
-- bucket.
module Amazonka.LexV2Models.DeleteExport
  ( -- * Creating a Request
    DeleteExport (..),
    newDeleteExport,

    -- * Request Lenses
    deleteExport_exportId,

    -- * Destructuring the Response
    DeleteExportResponse (..),
    newDeleteExportResponse,

    -- * Response Lenses
    deleteExportResponse_exportId,
    deleteExportResponse_exportStatus,
    deleteExportResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LexV2Models.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteExport' smart constructor.
data DeleteExport = DeleteExport'
  { -- | The unique identifier of the export to delete.
    exportId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteExport' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'exportId', 'deleteExport_exportId' - The unique identifier of the export to delete.
newDeleteExport ::
  -- | 'exportId'
  Prelude.Text ->
  DeleteExport
newDeleteExport pExportId_ =
  DeleteExport' {exportId = pExportId_}

-- | The unique identifier of the export to delete.
deleteExport_exportId :: Lens.Lens' DeleteExport Prelude.Text
deleteExport_exportId = Lens.lens (\DeleteExport' {exportId} -> exportId) (\s@DeleteExport' {} a -> s {exportId = a} :: DeleteExport)

instance Core.AWSRequest DeleteExport where
  type AWSResponse DeleteExport = DeleteExportResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteExportResponse'
            Prelude.<$> (x Data..?> "exportId")
            Prelude.<*> (x Data..?> "exportStatus")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteExport where
  hashWithSalt _salt DeleteExport' {..} =
    _salt `Prelude.hashWithSalt` exportId

instance Prelude.NFData DeleteExport where
  rnf DeleteExport' {..} = Prelude.rnf exportId

instance Data.ToHeaders DeleteExport where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteExport where
  toPath DeleteExport' {..} =
    Prelude.mconcat
      ["/exports/", Data.toBS exportId, "/"]

instance Data.ToQuery DeleteExport where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteExportResponse' smart constructor.
data DeleteExportResponse = DeleteExportResponse'
  { -- | The unique identifier of the deleted export.
    exportId :: Prelude.Maybe Prelude.Text,
    -- | The current status of the deletion. When the deletion is complete, the
    -- export will no longer be returned by the
    -- <https://docs.aws.amazon.com/lexv2/latest/dg/API_ListExports.html ListExports>
    -- operation and calls to the
    -- <https://docs.aws.amazon.com/lexv2/latest/dg/API_DescribeExport.html DescribeExport>
    -- operation with the export identifier will fail.
    exportStatus :: Prelude.Maybe ExportStatus,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteExportResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'exportId', 'deleteExportResponse_exportId' - The unique identifier of the deleted export.
--
-- 'exportStatus', 'deleteExportResponse_exportStatus' - The current status of the deletion. When the deletion is complete, the
-- export will no longer be returned by the
-- <https://docs.aws.amazon.com/lexv2/latest/dg/API_ListExports.html ListExports>
-- operation and calls to the
-- <https://docs.aws.amazon.com/lexv2/latest/dg/API_DescribeExport.html DescribeExport>
-- operation with the export identifier will fail.
--
-- 'httpStatus', 'deleteExportResponse_httpStatus' - The response's http status code.
newDeleteExportResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteExportResponse
newDeleteExportResponse pHttpStatus_ =
  DeleteExportResponse'
    { exportId = Prelude.Nothing,
      exportStatus = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The unique identifier of the deleted export.
deleteExportResponse_exportId :: Lens.Lens' DeleteExportResponse (Prelude.Maybe Prelude.Text)
deleteExportResponse_exportId = Lens.lens (\DeleteExportResponse' {exportId} -> exportId) (\s@DeleteExportResponse' {} a -> s {exportId = a} :: DeleteExportResponse)

-- | The current status of the deletion. When the deletion is complete, the
-- export will no longer be returned by the
-- <https://docs.aws.amazon.com/lexv2/latest/dg/API_ListExports.html ListExports>
-- operation and calls to the
-- <https://docs.aws.amazon.com/lexv2/latest/dg/API_DescribeExport.html DescribeExport>
-- operation with the export identifier will fail.
deleteExportResponse_exportStatus :: Lens.Lens' DeleteExportResponse (Prelude.Maybe ExportStatus)
deleteExportResponse_exportStatus = Lens.lens (\DeleteExportResponse' {exportStatus} -> exportStatus) (\s@DeleteExportResponse' {} a -> s {exportStatus = a} :: DeleteExportResponse)

-- | The response's http status code.
deleteExportResponse_httpStatus :: Lens.Lens' DeleteExportResponse Prelude.Int
deleteExportResponse_httpStatus = Lens.lens (\DeleteExportResponse' {httpStatus} -> httpStatus) (\s@DeleteExportResponse' {} a -> s {httpStatus = a} :: DeleteExportResponse)

instance Prelude.NFData DeleteExportResponse where
  rnf DeleteExportResponse' {..} =
    Prelude.rnf exportId
      `Prelude.seq` Prelude.rnf exportStatus
      `Prelude.seq` Prelude.rnf httpStatus
