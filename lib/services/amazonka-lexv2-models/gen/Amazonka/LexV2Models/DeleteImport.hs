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
-- Module      : Amazonka.LexV2Models.DeleteImport
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes a previous import and the associated file stored in an S3
-- bucket.
module Amazonka.LexV2Models.DeleteImport
  ( -- * Creating a Request
    DeleteImport (..),
    newDeleteImport,

    -- * Request Lenses
    deleteImport_importId,

    -- * Destructuring the Response
    DeleteImportResponse (..),
    newDeleteImportResponse,

    -- * Response Lenses
    deleteImportResponse_importId,
    deleteImportResponse_importStatus,
    deleteImportResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LexV2Models.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteImport' smart constructor.
data DeleteImport = DeleteImport'
  { -- | The unique identifier of the import to delete.
    importId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteImport' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'importId', 'deleteImport_importId' - The unique identifier of the import to delete.
newDeleteImport ::
  -- | 'importId'
  Prelude.Text ->
  DeleteImport
newDeleteImport pImportId_ =
  DeleteImport' {importId = pImportId_}

-- | The unique identifier of the import to delete.
deleteImport_importId :: Lens.Lens' DeleteImport Prelude.Text
deleteImport_importId = Lens.lens (\DeleteImport' {importId} -> importId) (\s@DeleteImport' {} a -> s {importId = a} :: DeleteImport)

instance Core.AWSRequest DeleteImport where
  type AWSResponse DeleteImport = DeleteImportResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteImportResponse'
            Prelude.<$> (x Data..?> "importId")
            Prelude.<*> (x Data..?> "importStatus")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteImport where
  hashWithSalt _salt DeleteImport' {..} =
    _salt `Prelude.hashWithSalt` importId

instance Prelude.NFData DeleteImport where
  rnf DeleteImport' {..} = Prelude.rnf importId

instance Data.ToHeaders DeleteImport where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteImport where
  toPath DeleteImport' {..} =
    Prelude.mconcat
      ["/imports/", Data.toBS importId, "/"]

instance Data.ToQuery DeleteImport where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteImportResponse' smart constructor.
data DeleteImportResponse = DeleteImportResponse'
  { -- | The unique identifier of the deleted import.
    importId :: Prelude.Maybe Prelude.Text,
    -- | The current status of the deletion. When the deletion is complete, the
    -- import will no longer be returned by the
    -- <https://docs.aws.amazon.com/lexv2/latest/dg/API_ListImports.html ListImports>
    -- operation and calls to the
    -- <https://docs.aws.amazon.com/lexv2/latest/dg/API_DescribeImport.html DescribeImport>
    -- operation with the import identifier will fail.
    importStatus :: Prelude.Maybe ImportStatus,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteImportResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'importId', 'deleteImportResponse_importId' - The unique identifier of the deleted import.
--
-- 'importStatus', 'deleteImportResponse_importStatus' - The current status of the deletion. When the deletion is complete, the
-- import will no longer be returned by the
-- <https://docs.aws.amazon.com/lexv2/latest/dg/API_ListImports.html ListImports>
-- operation and calls to the
-- <https://docs.aws.amazon.com/lexv2/latest/dg/API_DescribeImport.html DescribeImport>
-- operation with the import identifier will fail.
--
-- 'httpStatus', 'deleteImportResponse_httpStatus' - The response's http status code.
newDeleteImportResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteImportResponse
newDeleteImportResponse pHttpStatus_ =
  DeleteImportResponse'
    { importId = Prelude.Nothing,
      importStatus = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The unique identifier of the deleted import.
deleteImportResponse_importId :: Lens.Lens' DeleteImportResponse (Prelude.Maybe Prelude.Text)
deleteImportResponse_importId = Lens.lens (\DeleteImportResponse' {importId} -> importId) (\s@DeleteImportResponse' {} a -> s {importId = a} :: DeleteImportResponse)

-- | The current status of the deletion. When the deletion is complete, the
-- import will no longer be returned by the
-- <https://docs.aws.amazon.com/lexv2/latest/dg/API_ListImports.html ListImports>
-- operation and calls to the
-- <https://docs.aws.amazon.com/lexv2/latest/dg/API_DescribeImport.html DescribeImport>
-- operation with the import identifier will fail.
deleteImportResponse_importStatus :: Lens.Lens' DeleteImportResponse (Prelude.Maybe ImportStatus)
deleteImportResponse_importStatus = Lens.lens (\DeleteImportResponse' {importStatus} -> importStatus) (\s@DeleteImportResponse' {} a -> s {importStatus = a} :: DeleteImportResponse)

-- | The response's http status code.
deleteImportResponse_httpStatus :: Lens.Lens' DeleteImportResponse Prelude.Int
deleteImportResponse_httpStatus = Lens.lens (\DeleteImportResponse' {httpStatus} -> httpStatus) (\s@DeleteImportResponse' {} a -> s {httpStatus = a} :: DeleteImportResponse)

instance Prelude.NFData DeleteImportResponse where
  rnf DeleteImportResponse' {..} =
    Prelude.rnf importId
      `Prelude.seq` Prelude.rnf importStatus
      `Prelude.seq` Prelude.rnf httpStatus
