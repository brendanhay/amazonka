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
-- Module      : Amazonka.Kendra.BatchPutDocument
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds one or more documents to an index.
--
-- The @BatchPutDocument@ API enables you to ingest inline documents or a
-- set of documents stored in an Amazon S3 bucket. Use this API to ingest
-- your text and unstructured text into an index, add custom attributes to
-- the documents, and to attach an access control list to the documents
-- added to the index.
--
-- The documents are indexed asynchronously. You can see the progress of
-- the batch using Amazon Web Services CloudWatch. Any error messages
-- related to processing the batch are sent to your Amazon Web Services
-- CloudWatch log.
--
-- For an example of ingesting inline documents using Python and Java SDKs,
-- see
-- <https://docs.aws.amazon.com/kendra/latest/dg/in-adding-binary-doc.html Adding files directly to an index>.
module Amazonka.Kendra.BatchPutDocument
  ( -- * Creating a Request
    BatchPutDocument (..),
    newBatchPutDocument,

    -- * Request Lenses
    batchPutDocument_customDocumentEnrichmentConfiguration,
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Kendra.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newBatchPutDocument' smart constructor.
data BatchPutDocument = BatchPutDocument'
  { -- | Configuration information for altering your document metadata and
    -- content during the document ingestion process when you use the
    -- @BatchPutDocument@ API.
    --
    -- For more information on how to create, modify and delete document
    -- metadata, or make other content alterations when you ingest documents
    -- into Amazon Kendra, see
    -- <https://docs.aws.amazon.com/kendra/latest/dg/custom-document-enrichment.html Customizing document metadata during the ingestion process>.
    customDocumentEnrichmentConfiguration :: Prelude.Maybe CustomDocumentEnrichmentConfiguration,
    -- | The Amazon Resource Name (ARN) of a role that is allowed to run the
    -- @BatchPutDocument@ API. For more information, see
    -- <https://docs.aws.amazon.com/kendra/latest/dg/iam-roles.html IAM Roles for Amazon Kendra>.
    roleArn :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the index to add the documents to. You need to create
    -- the index first using the @CreateIndex@ API.
    indexId :: Prelude.Text,
    -- | One or more documents to add to the index.
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
-- 'customDocumentEnrichmentConfiguration', 'batchPutDocument_customDocumentEnrichmentConfiguration' - Configuration information for altering your document metadata and
-- content during the document ingestion process when you use the
-- @BatchPutDocument@ API.
--
-- For more information on how to create, modify and delete document
-- metadata, or make other content alterations when you ingest documents
-- into Amazon Kendra, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/custom-document-enrichment.html Customizing document metadata during the ingestion process>.
--
-- 'roleArn', 'batchPutDocument_roleArn' - The Amazon Resource Name (ARN) of a role that is allowed to run the
-- @BatchPutDocument@ API. For more information, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/iam-roles.html IAM Roles for Amazon Kendra>.
--
-- 'indexId', 'batchPutDocument_indexId' - The identifier of the index to add the documents to. You need to create
-- the index first using the @CreateIndex@ API.
--
-- 'documents', 'batchPutDocument_documents' - One or more documents to add to the index.
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
    { customDocumentEnrichmentConfiguration =
        Prelude.Nothing,
      roleArn = Prelude.Nothing,
      indexId = pIndexId_,
      documents = Lens.coerced Lens.# pDocuments_
    }

-- | Configuration information for altering your document metadata and
-- content during the document ingestion process when you use the
-- @BatchPutDocument@ API.
--
-- For more information on how to create, modify and delete document
-- metadata, or make other content alterations when you ingest documents
-- into Amazon Kendra, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/custom-document-enrichment.html Customizing document metadata during the ingestion process>.
batchPutDocument_customDocumentEnrichmentConfiguration :: Lens.Lens' BatchPutDocument (Prelude.Maybe CustomDocumentEnrichmentConfiguration)
batchPutDocument_customDocumentEnrichmentConfiguration = Lens.lens (\BatchPutDocument' {customDocumentEnrichmentConfiguration} -> customDocumentEnrichmentConfiguration) (\s@BatchPutDocument' {} a -> s {customDocumentEnrichmentConfiguration = a} :: BatchPutDocument)

-- | The Amazon Resource Name (ARN) of a role that is allowed to run the
-- @BatchPutDocument@ API. For more information, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/iam-roles.html IAM Roles for Amazon Kendra>.
batchPutDocument_roleArn :: Lens.Lens' BatchPutDocument (Prelude.Maybe Prelude.Text)
batchPutDocument_roleArn = Lens.lens (\BatchPutDocument' {roleArn} -> roleArn) (\s@BatchPutDocument' {} a -> s {roleArn = a} :: BatchPutDocument)

-- | The identifier of the index to add the documents to. You need to create
-- the index first using the @CreateIndex@ API.
batchPutDocument_indexId :: Lens.Lens' BatchPutDocument Prelude.Text
batchPutDocument_indexId = Lens.lens (\BatchPutDocument' {indexId} -> indexId) (\s@BatchPutDocument' {} a -> s {indexId = a} :: BatchPutDocument)

-- | One or more documents to add to the index.
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
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          BatchPutDocumentResponse'
            Prelude.<$> ( x
                            Data..?> "FailedDocuments"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable BatchPutDocument where
  hashWithSalt _salt BatchPutDocument' {..} =
    _salt
      `Prelude.hashWithSalt` customDocumentEnrichmentConfiguration
      `Prelude.hashWithSalt` roleArn
      `Prelude.hashWithSalt` indexId
      `Prelude.hashWithSalt` documents

instance Prelude.NFData BatchPutDocument where
  rnf BatchPutDocument' {..} =
    Prelude.rnf customDocumentEnrichmentConfiguration
      `Prelude.seq` Prelude.rnf roleArn
      `Prelude.seq` Prelude.rnf indexId
      `Prelude.seq` Prelude.rnf documents

instance Data.ToHeaders BatchPutDocument where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSKendraFrontendService.BatchPutDocument" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON BatchPutDocument where
  toJSON BatchPutDocument' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CustomDocumentEnrichmentConfiguration" Data..=)
              Prelude.<$> customDocumentEnrichmentConfiguration,
            ("RoleArn" Data..=) Prelude.<$> roleArn,
            Prelude.Just ("IndexId" Data..= indexId),
            Prelude.Just ("Documents" Data..= documents)
          ]
      )

instance Data.ToPath BatchPutDocument where
  toPath = Prelude.const "/"

instance Data.ToQuery BatchPutDocument where
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

instance Prelude.NFData BatchPutDocumentResponse where
  rnf BatchPutDocumentResponse' {..} =
    Prelude.rnf failedDocuments
      `Prelude.seq` Prelude.rnf httpStatus
