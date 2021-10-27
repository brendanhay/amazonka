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
-- Module      : Network.AWS.Kendra.BatchPutDocument
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds one or more documents to an index.
--
-- The @BatchPutDocument@ operation enables you to ingest inline documents
-- or a set of documents stored in an Amazon S3 bucket. Use this operation
-- to ingest your text and unstructured text into an index, add custom
-- attributes to the documents, and to attach an access control list to the
-- documents added to the index.
--
-- The documents are indexed asynchronously. You can see the progress of
-- the batch using Amazon Web Services CloudWatch. Any error messages
-- related to processing the batch are sent to your Amazon Web Services
-- CloudWatch log.
module Network.AWS.Kendra.BatchPutDocument
  ( -- * Creating a Request
    BatchPutDocument (..),
    newBatchPutDocument,

    -- * Request Lenses
    batchPutDocument_roleArn,
    batchPutDocument_indexId,
    batchPutDocument_documents,

    -- * Destructuring the Response
    BatchPutDocumentResponse (..),
    newBatchPutDocumentResponse,

    -- * Response Lenses
    batchPutDocumentResponse_failedDocuments,
    batchPutDocumentResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Kendra.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newBatchPutDocument' smart constructor.
data BatchPutDocument = BatchPutDocument'
  { -- | The Amazon Resource Name (ARN) of a role that is allowed to run the
    -- @BatchPutDocument@ operation. For more information, see
    -- <https://docs.aws.amazon.com/kendra/latest/dg/iam-roles.html IAM Roles for Amazon Kendra>.
    roleArn :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the index to add the documents to. You need to create
    -- the index first using the @CreateIndex@ operation.
    indexId :: Prelude.Text,
    -- | One or more documents to add to the index.
    --
    -- Documents can include custom attributes. For example, \'DataSourceId\'
    -- and \'DataSourceSyncJobId\' are custom attributes that provide
    -- information on the synchronization of documents running on a data
    -- source. Note, \'DataSourceSyncJobId\' could be an optional custom
    -- attribute as Amazon Kendra will use the ID of a running sync job.
    --
    -- Documents have the following file size limits.
    --
    -- -   5 MB total size for inline documents
    --
    -- -   50 MB total size for files from an S3 bucket
    --
    -- -   5 MB extracted text for any file
    --
    -- For more information about file size and transaction per second quotas,
    -- see <https://docs.aws.amazon.com/kendra/latest/dg/quotas.html Quotas>.
    documents :: Prelude.NonEmpty Document
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchPutDocument' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'roleArn', 'batchPutDocument_roleArn' - The Amazon Resource Name (ARN) of a role that is allowed to run the
-- @BatchPutDocument@ operation. For more information, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/iam-roles.html IAM Roles for Amazon Kendra>.
--
-- 'indexId', 'batchPutDocument_indexId' - The identifier of the index to add the documents to. You need to create
-- the index first using the @CreateIndex@ operation.
--
-- 'documents', 'batchPutDocument_documents' - One or more documents to add to the index.
--
-- Documents can include custom attributes. For example, \'DataSourceId\'
-- and \'DataSourceSyncJobId\' are custom attributes that provide
-- information on the synchronization of documents running on a data
-- source. Note, \'DataSourceSyncJobId\' could be an optional custom
-- attribute as Amazon Kendra will use the ID of a running sync job.
--
-- Documents have the following file size limits.
--
-- -   5 MB total size for inline documents
--
-- -   50 MB total size for files from an S3 bucket
--
-- -   5 MB extracted text for any file
--
-- For more information about file size and transaction per second quotas,
-- see <https://docs.aws.amazon.com/kendra/latest/dg/quotas.html Quotas>.
newBatchPutDocument ::
  -- | 'indexId'
  Prelude.Text ->
  -- | 'documents'
  Prelude.NonEmpty Document ->
  BatchPutDocument
newBatchPutDocument pIndexId_ pDocuments_ =
  BatchPutDocument'
    { roleArn = Prelude.Nothing,
      indexId = pIndexId_,
      documents = Lens.coerced Lens.# pDocuments_
    }

-- | The Amazon Resource Name (ARN) of a role that is allowed to run the
-- @BatchPutDocument@ operation. For more information, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/iam-roles.html IAM Roles for Amazon Kendra>.
batchPutDocument_roleArn :: Lens.Lens' BatchPutDocument (Prelude.Maybe Prelude.Text)
batchPutDocument_roleArn = Lens.lens (\BatchPutDocument' {roleArn} -> roleArn) (\s@BatchPutDocument' {} a -> s {roleArn = a} :: BatchPutDocument)

-- | The identifier of the index to add the documents to. You need to create
-- the index first using the @CreateIndex@ operation.
batchPutDocument_indexId :: Lens.Lens' BatchPutDocument Prelude.Text
batchPutDocument_indexId = Lens.lens (\BatchPutDocument' {indexId} -> indexId) (\s@BatchPutDocument' {} a -> s {indexId = a} :: BatchPutDocument)

-- | One or more documents to add to the index.
--
-- Documents can include custom attributes. For example, \'DataSourceId\'
-- and \'DataSourceSyncJobId\' are custom attributes that provide
-- information on the synchronization of documents running on a data
-- source. Note, \'DataSourceSyncJobId\' could be an optional custom
-- attribute as Amazon Kendra will use the ID of a running sync job.
--
-- Documents have the following file size limits.
--
-- -   5 MB total size for inline documents
--
-- -   50 MB total size for files from an S3 bucket
--
-- -   5 MB extracted text for any file
--
-- For more information about file size and transaction per second quotas,
-- see <https://docs.aws.amazon.com/kendra/latest/dg/quotas.html Quotas>.
batchPutDocument_documents :: Lens.Lens' BatchPutDocument (Prelude.NonEmpty Document)
batchPutDocument_documents = Lens.lens (\BatchPutDocument' {documents} -> documents) (\s@BatchPutDocument' {} a -> s {documents = a} :: BatchPutDocument) Prelude.. Lens.coerced

instance Core.AWSRequest BatchPutDocument where
  type
    AWSResponse BatchPutDocument =
      BatchPutDocumentResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          BatchPutDocumentResponse'
            Prelude.<$> ( x Core..?> "FailedDocuments"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable BatchPutDocument

instance Prelude.NFData BatchPutDocument

instance Core.ToHeaders BatchPutDocument where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSKendraFrontendService.BatchPutDocument" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON BatchPutDocument where
  toJSON BatchPutDocument' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("RoleArn" Core..=) Prelude.<$> roleArn,
            Prelude.Just ("IndexId" Core..= indexId),
            Prelude.Just ("Documents" Core..= documents)
          ]
      )

instance Core.ToPath BatchPutDocument where
  toPath = Prelude.const "/"

instance Core.ToQuery BatchPutDocument where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newBatchPutDocumentResponse' smart constructor.
data BatchPutDocumentResponse = BatchPutDocumentResponse'
  { -- | A list of documents that were not added to the index because the
    -- document failed a validation check. Each document contains an error
    -- message that indicates why the document couldn\'t be added to the index.
    --
    -- If there was an error adding a document to an index the error is
    -- reported in your Amazon Web Services CloudWatch log. For more
    -- information, see
    -- <https://docs.aws.amazon.com/kendra/latest/dg/cloudwatch-logs.html Monitoring Amazon Kendra with Amazon CloudWatch Logs>
    failedDocuments :: Prelude.Maybe [BatchPutDocumentResponseFailedDocument],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchPutDocumentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'failedDocuments', 'batchPutDocumentResponse_failedDocuments' - A list of documents that were not added to the index because the
-- document failed a validation check. Each document contains an error
-- message that indicates why the document couldn\'t be added to the index.
--
-- If there was an error adding a document to an index the error is
-- reported in your Amazon Web Services CloudWatch log. For more
-- information, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/cloudwatch-logs.html Monitoring Amazon Kendra with Amazon CloudWatch Logs>
--
-- 'httpStatus', 'batchPutDocumentResponse_httpStatus' - The response's http status code.
newBatchPutDocumentResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  BatchPutDocumentResponse
newBatchPutDocumentResponse pHttpStatus_ =
  BatchPutDocumentResponse'
    { failedDocuments =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of documents that were not added to the index because the
-- document failed a validation check. Each document contains an error
-- message that indicates why the document couldn\'t be added to the index.
--
-- If there was an error adding a document to an index the error is
-- reported in your Amazon Web Services CloudWatch log. For more
-- information, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/cloudwatch-logs.html Monitoring Amazon Kendra with Amazon CloudWatch Logs>
batchPutDocumentResponse_failedDocuments :: Lens.Lens' BatchPutDocumentResponse (Prelude.Maybe [BatchPutDocumentResponseFailedDocument])
batchPutDocumentResponse_failedDocuments = Lens.lens (\BatchPutDocumentResponse' {failedDocuments} -> failedDocuments) (\s@BatchPutDocumentResponse' {} a -> s {failedDocuments = a} :: BatchPutDocumentResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
batchPutDocumentResponse_httpStatus :: Lens.Lens' BatchPutDocumentResponse Prelude.Int
batchPutDocumentResponse_httpStatus = Lens.lens (\BatchPutDocumentResponse' {httpStatus} -> httpStatus) (\s@BatchPutDocumentResponse' {} a -> s {httpStatus = a} :: BatchPutDocumentResponse)

instance Prelude.NFData BatchPutDocumentResponse
