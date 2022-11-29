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
-- Module      : Amazonka.Kendra.BatchDeleteDocument
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes one or more documents from an index. The documents must have
-- been added with the @BatchPutDocument@ API.
--
-- The documents are deleted asynchronously. You can see the progress of
-- the deletion by using Amazon Web Services CloudWatch. Any error messages
-- related to the processing of the batch are sent to you CloudWatch log.
module Amazonka.Kendra.BatchDeleteDocument
  ( -- * Creating a Request
    BatchDeleteDocument (..),
    newBatchDeleteDocument,

    -- * Request Lenses
    batchDeleteDocument_dataSourceSyncJobMetricTarget,
    batchDeleteDocument_indexId,
    batchDeleteDocument_documentIdList,

    -- * Destructuring the Response
    BatchDeleteDocumentResponse (..),
    newBatchDeleteDocumentResponse,

    -- * Response Lenses
    batchDeleteDocumentResponse_failedDocuments,
    batchDeleteDocumentResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Kendra.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newBatchDeleteDocument' smart constructor.
data BatchDeleteDocument = BatchDeleteDocument'
  { dataSourceSyncJobMetricTarget :: Prelude.Maybe DataSourceSyncJobMetricTarget,
    -- | The identifier of the index that contains the documents to delete.
    indexId :: Prelude.Text,
    -- | One or more identifiers for documents to delete from the index.
    documentIdList :: Prelude.NonEmpty Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchDeleteDocument' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dataSourceSyncJobMetricTarget', 'batchDeleteDocument_dataSourceSyncJobMetricTarget' - Undocumented member.
--
-- 'indexId', 'batchDeleteDocument_indexId' - The identifier of the index that contains the documents to delete.
--
-- 'documentIdList', 'batchDeleteDocument_documentIdList' - One or more identifiers for documents to delete from the index.
newBatchDeleteDocument ::
  -- | 'indexId'
  Prelude.Text ->
  -- | 'documentIdList'
  Prelude.NonEmpty Prelude.Text ->
  BatchDeleteDocument
newBatchDeleteDocument pIndexId_ pDocumentIdList_ =
  BatchDeleteDocument'
    { dataSourceSyncJobMetricTarget =
        Prelude.Nothing,
      indexId = pIndexId_,
      documentIdList =
        Lens.coerced Lens.# pDocumentIdList_
    }

-- | Undocumented member.
batchDeleteDocument_dataSourceSyncJobMetricTarget :: Lens.Lens' BatchDeleteDocument (Prelude.Maybe DataSourceSyncJobMetricTarget)
batchDeleteDocument_dataSourceSyncJobMetricTarget = Lens.lens (\BatchDeleteDocument' {dataSourceSyncJobMetricTarget} -> dataSourceSyncJobMetricTarget) (\s@BatchDeleteDocument' {} a -> s {dataSourceSyncJobMetricTarget = a} :: BatchDeleteDocument)

-- | The identifier of the index that contains the documents to delete.
batchDeleteDocument_indexId :: Lens.Lens' BatchDeleteDocument Prelude.Text
batchDeleteDocument_indexId = Lens.lens (\BatchDeleteDocument' {indexId} -> indexId) (\s@BatchDeleteDocument' {} a -> s {indexId = a} :: BatchDeleteDocument)

-- | One or more identifiers for documents to delete from the index.
batchDeleteDocument_documentIdList :: Lens.Lens' BatchDeleteDocument (Prelude.NonEmpty Prelude.Text)
batchDeleteDocument_documentIdList = Lens.lens (\BatchDeleteDocument' {documentIdList} -> documentIdList) (\s@BatchDeleteDocument' {} a -> s {documentIdList = a} :: BatchDeleteDocument) Prelude.. Lens.coerced

instance Core.AWSRequest BatchDeleteDocument where
  type
    AWSResponse BatchDeleteDocument =
      BatchDeleteDocumentResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          BatchDeleteDocumentResponse'
            Prelude.<$> ( x Core..?> "FailedDocuments"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable BatchDeleteDocument where
  hashWithSalt _salt BatchDeleteDocument' {..} =
    _salt
      `Prelude.hashWithSalt` dataSourceSyncJobMetricTarget
      `Prelude.hashWithSalt` indexId
      `Prelude.hashWithSalt` documentIdList

instance Prelude.NFData BatchDeleteDocument where
  rnf BatchDeleteDocument' {..} =
    Prelude.rnf dataSourceSyncJobMetricTarget
      `Prelude.seq` Prelude.rnf indexId
      `Prelude.seq` Prelude.rnf documentIdList

instance Core.ToHeaders BatchDeleteDocument where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSKendraFrontendService.BatchDeleteDocument" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON BatchDeleteDocument where
  toJSON BatchDeleteDocument' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("DataSourceSyncJobMetricTarget" Core..=)
              Prelude.<$> dataSourceSyncJobMetricTarget,
            Prelude.Just ("IndexId" Core..= indexId),
            Prelude.Just
              ("DocumentIdList" Core..= documentIdList)
          ]
      )

instance Core.ToPath BatchDeleteDocument where
  toPath = Prelude.const "/"

instance Core.ToQuery BatchDeleteDocument where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newBatchDeleteDocumentResponse' smart constructor.
data BatchDeleteDocumentResponse = BatchDeleteDocumentResponse'
  { -- | A list of documents that could not be removed from the index. Each entry
    -- contains an error message that indicates why the document couldn\'t be
    -- removed from the index.
    failedDocuments :: Prelude.Maybe [BatchDeleteDocumentResponseFailedDocument],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchDeleteDocumentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'failedDocuments', 'batchDeleteDocumentResponse_failedDocuments' - A list of documents that could not be removed from the index. Each entry
-- contains an error message that indicates why the document couldn\'t be
-- removed from the index.
--
-- 'httpStatus', 'batchDeleteDocumentResponse_httpStatus' - The response's http status code.
newBatchDeleteDocumentResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  BatchDeleteDocumentResponse
newBatchDeleteDocumentResponse pHttpStatus_ =
  BatchDeleteDocumentResponse'
    { failedDocuments =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of documents that could not be removed from the index. Each entry
-- contains an error message that indicates why the document couldn\'t be
-- removed from the index.
batchDeleteDocumentResponse_failedDocuments :: Lens.Lens' BatchDeleteDocumentResponse (Prelude.Maybe [BatchDeleteDocumentResponseFailedDocument])
batchDeleteDocumentResponse_failedDocuments = Lens.lens (\BatchDeleteDocumentResponse' {failedDocuments} -> failedDocuments) (\s@BatchDeleteDocumentResponse' {} a -> s {failedDocuments = a} :: BatchDeleteDocumentResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
batchDeleteDocumentResponse_httpStatus :: Lens.Lens' BatchDeleteDocumentResponse Prelude.Int
batchDeleteDocumentResponse_httpStatus = Lens.lens (\BatchDeleteDocumentResponse' {httpStatus} -> httpStatus) (\s@BatchDeleteDocumentResponse' {} a -> s {httpStatus = a} :: BatchDeleteDocumentResponse)

instance Prelude.NFData BatchDeleteDocumentResponse where
  rnf BatchDeleteDocumentResponse' {..} =
    Prelude.rnf failedDocuments
      `Prelude.seq` Prelude.rnf httpStatus
