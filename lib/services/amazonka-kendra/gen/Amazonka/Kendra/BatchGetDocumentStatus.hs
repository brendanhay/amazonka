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
-- Module      : Amazonka.Kendra.BatchGetDocumentStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the indexing status for one or more documents submitted with the
-- <https://docs.aws.amazon.com/kendra/latest/dg/API_BatchPutDocument.html BatchPutDocument>
-- API.
--
-- When you use the @BatchPutDocument@ API, documents are indexed
-- asynchronously. You can use the @BatchGetDocumentStatus@ API to get the
-- current status of a list of documents so that you can determine if they
-- have been successfully indexed.
--
-- You can also use the @BatchGetDocumentStatus@ API to check the status of
-- the
-- <https://docs.aws.amazon.com/kendra/latest/dg/API_BatchDeleteDocument.html BatchDeleteDocument>
-- API. When a document is deleted from the index, Amazon Kendra returns
-- @NOT_FOUND@ as the status.
module Amazonka.Kendra.BatchGetDocumentStatus
  ( -- * Creating a Request
    BatchGetDocumentStatus (..),
    newBatchGetDocumentStatus,

    -- * Request Lenses
    batchGetDocumentStatus_indexId,
    batchGetDocumentStatus_documentInfoList,

    -- * Destructuring the Response
    BatchGetDocumentStatusResponse (..),
    newBatchGetDocumentStatusResponse,

    -- * Response Lenses
    batchGetDocumentStatusResponse_documentStatusList,
    batchGetDocumentStatusResponse_errors,
    batchGetDocumentStatusResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Kendra.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newBatchGetDocumentStatus' smart constructor.
data BatchGetDocumentStatus = BatchGetDocumentStatus'
  { -- | The identifier of the index to add documents to. The index ID is
    -- returned by the
    -- <https://docs.aws.amazon.com/kendra/latest/dg/API_CreateIndex.html CreateIndex>
    -- API.
    indexId :: Prelude.Text,
    -- | A list of @DocumentInfo@ objects that identify the documents for which
    -- to get the status. You identify the documents by their document ID and
    -- optional attributes.
    documentInfoList :: Prelude.NonEmpty DocumentInfo
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchGetDocumentStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'indexId', 'batchGetDocumentStatus_indexId' - The identifier of the index to add documents to. The index ID is
-- returned by the
-- <https://docs.aws.amazon.com/kendra/latest/dg/API_CreateIndex.html CreateIndex>
-- API.
--
-- 'documentInfoList', 'batchGetDocumentStatus_documentInfoList' - A list of @DocumentInfo@ objects that identify the documents for which
-- to get the status. You identify the documents by their document ID and
-- optional attributes.
newBatchGetDocumentStatus ::
  -- | 'indexId'
  Prelude.Text ->
  -- | 'documentInfoList'
  Prelude.NonEmpty DocumentInfo ->
  BatchGetDocumentStatus
newBatchGetDocumentStatus
  pIndexId_
  pDocumentInfoList_ =
    BatchGetDocumentStatus'
      { indexId = pIndexId_,
        documentInfoList =
          Lens.coerced Lens.# pDocumentInfoList_
      }

-- | The identifier of the index to add documents to. The index ID is
-- returned by the
-- <https://docs.aws.amazon.com/kendra/latest/dg/API_CreateIndex.html CreateIndex>
-- API.
batchGetDocumentStatus_indexId :: Lens.Lens' BatchGetDocumentStatus Prelude.Text
batchGetDocumentStatus_indexId = Lens.lens (\BatchGetDocumentStatus' {indexId} -> indexId) (\s@BatchGetDocumentStatus' {} a -> s {indexId = a} :: BatchGetDocumentStatus)

-- | A list of @DocumentInfo@ objects that identify the documents for which
-- to get the status. You identify the documents by their document ID and
-- optional attributes.
batchGetDocumentStatus_documentInfoList :: Lens.Lens' BatchGetDocumentStatus (Prelude.NonEmpty DocumentInfo)
batchGetDocumentStatus_documentInfoList = Lens.lens (\BatchGetDocumentStatus' {documentInfoList} -> documentInfoList) (\s@BatchGetDocumentStatus' {} a -> s {documentInfoList = a} :: BatchGetDocumentStatus) Prelude.. Lens.coerced

instance Core.AWSRequest BatchGetDocumentStatus where
  type
    AWSResponse BatchGetDocumentStatus =
      BatchGetDocumentStatusResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          BatchGetDocumentStatusResponse'
            Prelude.<$> ( x Data..?> "DocumentStatusList"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "Errors" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable BatchGetDocumentStatus where
  hashWithSalt _salt BatchGetDocumentStatus' {..} =
    _salt `Prelude.hashWithSalt` indexId
      `Prelude.hashWithSalt` documentInfoList

instance Prelude.NFData BatchGetDocumentStatus where
  rnf BatchGetDocumentStatus' {..} =
    Prelude.rnf indexId
      `Prelude.seq` Prelude.rnf documentInfoList

instance Data.ToHeaders BatchGetDocumentStatus where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSKendraFrontendService.BatchGetDocumentStatus" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON BatchGetDocumentStatus where
  toJSON BatchGetDocumentStatus' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("IndexId" Data..= indexId),
            Prelude.Just
              ("DocumentInfoList" Data..= documentInfoList)
          ]
      )

instance Data.ToPath BatchGetDocumentStatus where
  toPath = Prelude.const "/"

instance Data.ToQuery BatchGetDocumentStatus where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newBatchGetDocumentStatusResponse' smart constructor.
data BatchGetDocumentStatusResponse = BatchGetDocumentStatusResponse'
  { -- | The status of documents. The status indicates if the document is waiting
    -- to be indexed, is in the process of indexing, has completed indexing, or
    -- failed indexing. If a document failed indexing, the status provides the
    -- reason why.
    documentStatusList :: Prelude.Maybe [Status],
    -- | A list of documents that Amazon Kendra couldn\'t get the status for. The
    -- list includes the ID of the document and the reason that the status
    -- couldn\'t be found.
    errors :: Prelude.Maybe [BatchGetDocumentStatusResponseError],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchGetDocumentStatusResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'documentStatusList', 'batchGetDocumentStatusResponse_documentStatusList' - The status of documents. The status indicates if the document is waiting
-- to be indexed, is in the process of indexing, has completed indexing, or
-- failed indexing. If a document failed indexing, the status provides the
-- reason why.
--
-- 'errors', 'batchGetDocumentStatusResponse_errors' - A list of documents that Amazon Kendra couldn\'t get the status for. The
-- list includes the ID of the document and the reason that the status
-- couldn\'t be found.
--
-- 'httpStatus', 'batchGetDocumentStatusResponse_httpStatus' - The response's http status code.
newBatchGetDocumentStatusResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  BatchGetDocumentStatusResponse
newBatchGetDocumentStatusResponse pHttpStatus_ =
  BatchGetDocumentStatusResponse'
    { documentStatusList =
        Prelude.Nothing,
      errors = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The status of documents. The status indicates if the document is waiting
-- to be indexed, is in the process of indexing, has completed indexing, or
-- failed indexing. If a document failed indexing, the status provides the
-- reason why.
batchGetDocumentStatusResponse_documentStatusList :: Lens.Lens' BatchGetDocumentStatusResponse (Prelude.Maybe [Status])
batchGetDocumentStatusResponse_documentStatusList = Lens.lens (\BatchGetDocumentStatusResponse' {documentStatusList} -> documentStatusList) (\s@BatchGetDocumentStatusResponse' {} a -> s {documentStatusList = a} :: BatchGetDocumentStatusResponse) Prelude.. Lens.mapping Lens.coerced

-- | A list of documents that Amazon Kendra couldn\'t get the status for. The
-- list includes the ID of the document and the reason that the status
-- couldn\'t be found.
batchGetDocumentStatusResponse_errors :: Lens.Lens' BatchGetDocumentStatusResponse (Prelude.Maybe [BatchGetDocumentStatusResponseError])
batchGetDocumentStatusResponse_errors = Lens.lens (\BatchGetDocumentStatusResponse' {errors} -> errors) (\s@BatchGetDocumentStatusResponse' {} a -> s {errors = a} :: BatchGetDocumentStatusResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
batchGetDocumentStatusResponse_httpStatus :: Lens.Lens' BatchGetDocumentStatusResponse Prelude.Int
batchGetDocumentStatusResponse_httpStatus = Lens.lens (\BatchGetDocumentStatusResponse' {httpStatus} -> httpStatus) (\s@BatchGetDocumentStatusResponse' {} a -> s {httpStatus = a} :: BatchGetDocumentStatusResponse)

instance
  Prelude.NFData
    BatchGetDocumentStatusResponse
  where
  rnf BatchGetDocumentStatusResponse' {..} =
    Prelude.rnf documentStatusList
      `Prelude.seq` Prelude.rnf errors
      `Prelude.seq` Prelude.rnf httpStatus
