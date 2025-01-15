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
-- Module      : Amazonka.Discovery.BatchDeleteImportData
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes one or more import tasks, each identified by their import ID.
-- Each import task has a number of records that can identify servers or
-- applications.
--
-- Amazon Web Services Application Discovery Service has built-in matching
-- logic that will identify when discovered servers match existing entries
-- that you\'ve previously discovered, the information for the
-- already-existing discovered server is updated. When you delete an import
-- task that contains records that were used to match, the information in
-- those matched records that comes from the deleted records will also be
-- deleted.
module Amazonka.Discovery.BatchDeleteImportData
  ( -- * Creating a Request
    BatchDeleteImportData (..),
    newBatchDeleteImportData,

    -- * Request Lenses
    batchDeleteImportData_importTaskIds,

    -- * Destructuring the Response
    BatchDeleteImportDataResponse (..),
    newBatchDeleteImportDataResponse,

    -- * Response Lenses
    batchDeleteImportDataResponse_errors,
    batchDeleteImportDataResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Discovery.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newBatchDeleteImportData' smart constructor.
data BatchDeleteImportData = BatchDeleteImportData'
  { -- | The IDs for the import tasks that you want to delete.
    importTaskIds :: Prelude.NonEmpty Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchDeleteImportData' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'importTaskIds', 'batchDeleteImportData_importTaskIds' - The IDs for the import tasks that you want to delete.
newBatchDeleteImportData ::
  -- | 'importTaskIds'
  Prelude.NonEmpty Prelude.Text ->
  BatchDeleteImportData
newBatchDeleteImportData pImportTaskIds_ =
  BatchDeleteImportData'
    { importTaskIds =
        Lens.coerced Lens.# pImportTaskIds_
    }

-- | The IDs for the import tasks that you want to delete.
batchDeleteImportData_importTaskIds :: Lens.Lens' BatchDeleteImportData (Prelude.NonEmpty Prelude.Text)
batchDeleteImportData_importTaskIds = Lens.lens (\BatchDeleteImportData' {importTaskIds} -> importTaskIds) (\s@BatchDeleteImportData' {} a -> s {importTaskIds = a} :: BatchDeleteImportData) Prelude.. Lens.coerced

instance Core.AWSRequest BatchDeleteImportData where
  type
    AWSResponse BatchDeleteImportData =
      BatchDeleteImportDataResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          BatchDeleteImportDataResponse'
            Prelude.<$> (x Data..?> "errors" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable BatchDeleteImportData where
  hashWithSalt _salt BatchDeleteImportData' {..} =
    _salt `Prelude.hashWithSalt` importTaskIds

instance Prelude.NFData BatchDeleteImportData where
  rnf BatchDeleteImportData' {..} =
    Prelude.rnf importTaskIds

instance Data.ToHeaders BatchDeleteImportData where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSPoseidonService_V2015_11_01.BatchDeleteImportData" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON BatchDeleteImportData where
  toJSON BatchDeleteImportData' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("importTaskIds" Data..= importTaskIds)
          ]
      )

instance Data.ToPath BatchDeleteImportData where
  toPath = Prelude.const "/"

instance Data.ToQuery BatchDeleteImportData where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newBatchDeleteImportDataResponse' smart constructor.
data BatchDeleteImportDataResponse = BatchDeleteImportDataResponse'
  { -- | Error messages returned for each import task that you deleted as a
    -- response for this command.
    errors :: Prelude.Maybe [BatchDeleteImportDataError],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchDeleteImportDataResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'errors', 'batchDeleteImportDataResponse_errors' - Error messages returned for each import task that you deleted as a
-- response for this command.
--
-- 'httpStatus', 'batchDeleteImportDataResponse_httpStatus' - The response's http status code.
newBatchDeleteImportDataResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  BatchDeleteImportDataResponse
newBatchDeleteImportDataResponse pHttpStatus_ =
  BatchDeleteImportDataResponse'
    { errors =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Error messages returned for each import task that you deleted as a
-- response for this command.
batchDeleteImportDataResponse_errors :: Lens.Lens' BatchDeleteImportDataResponse (Prelude.Maybe [BatchDeleteImportDataError])
batchDeleteImportDataResponse_errors = Lens.lens (\BatchDeleteImportDataResponse' {errors} -> errors) (\s@BatchDeleteImportDataResponse' {} a -> s {errors = a} :: BatchDeleteImportDataResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
batchDeleteImportDataResponse_httpStatus :: Lens.Lens' BatchDeleteImportDataResponse Prelude.Int
batchDeleteImportDataResponse_httpStatus = Lens.lens (\BatchDeleteImportDataResponse' {httpStatus} -> httpStatus) (\s@BatchDeleteImportDataResponse' {} a -> s {httpStatus = a} :: BatchDeleteImportDataResponse)

instance Prelude.NFData BatchDeleteImportDataResponse where
  rnf BatchDeleteImportDataResponse' {..} =
    Prelude.rnf errors `Prelude.seq`
      Prelude.rnf httpStatus
