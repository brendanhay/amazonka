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
-- Module      : Amazonka.CloudSearchDomains.UploadDocuments
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Posts a batch of documents to a search domain for indexing. A document
-- batch is a collection of add and delete operations that represent the
-- documents you want to add, update, or delete from your domain. Batches
-- can be described in either JSON or XML. Each item that you want Amazon
-- CloudSearch to return as a search result (such as a product) is
-- represented as a document. Every document has a unique ID and one or
-- more fields that contain the data that you want to search and return in
-- results. Individual documents cannot contain more than 1 MB of data. The
-- entire batch cannot exceed 5 MB. To get the best possible upload
-- performance, group add and delete operations in batches that are close
-- the 5 MB limit. Submitting a large volume of single-document batches can
-- overload a domain\'s document service.
--
-- The endpoint for submitting @UploadDocuments@ requests is
-- domain-specific. To get the document endpoint for your domain, use the
-- Amazon CloudSearch configuration service @DescribeDomains@ action. A
-- domain\'s endpoints are also displayed on the domain dashboard in the
-- Amazon CloudSearch console.
--
-- For more information about formatting your data for Amazon CloudSearch,
-- see
-- <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/preparing-data.html Preparing Your Data>
-- in the /Amazon CloudSearch Developer Guide/. For more information about
-- uploading data for indexing, see
-- <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/uploading-data.html Uploading Data>
-- in the /Amazon CloudSearch Developer Guide/.
module Amazonka.CloudSearchDomains.UploadDocuments
  ( -- * Creating a Request
    UploadDocuments (..),
    newUploadDocuments,

    -- * Request Lenses
    uploadDocuments_contentType,
    uploadDocuments_documents,

    -- * Destructuring the Response
    UploadDocumentsResponse (..),
    newUploadDocumentsResponse,

    -- * Response Lenses
    uploadDocumentsResponse_adds,
    uploadDocumentsResponse_deletes,
    uploadDocumentsResponse_status,
    uploadDocumentsResponse_warnings,
    uploadDocumentsResponse_httpStatus,
  )
where

import Amazonka.CloudSearchDomains.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Container for the parameters to the @UploadDocuments@ request.
--
-- /See:/ 'newUploadDocuments' smart constructor.
data UploadDocuments = UploadDocuments'
  { -- | The format of the batch you are uploading. Amazon CloudSearch supports
    -- two document batch formats:
    --
    -- -   application\/json
    -- -   application\/xml
    contentType :: ContentType,
    -- | A batch of documents formatted in JSON or HTML.
    documents :: Data.HashedBody
  }
  deriving (Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UploadDocuments' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'contentType', 'uploadDocuments_contentType' - The format of the batch you are uploading. Amazon CloudSearch supports
-- two document batch formats:
--
-- -   application\/json
-- -   application\/xml
--
-- 'documents', 'uploadDocuments_documents' - A batch of documents formatted in JSON or HTML.
newUploadDocuments ::
  -- | 'contentType'
  ContentType ->
  -- | 'documents'
  Data.HashedBody ->
  UploadDocuments
newUploadDocuments pContentType_ pDocuments_ =
  UploadDocuments'
    { contentType = pContentType_,
      documents = pDocuments_
    }

-- | The format of the batch you are uploading. Amazon CloudSearch supports
-- two document batch formats:
--
-- -   application\/json
-- -   application\/xml
uploadDocuments_contentType :: Lens.Lens' UploadDocuments ContentType
uploadDocuments_contentType = Lens.lens (\UploadDocuments' {contentType} -> contentType) (\s@UploadDocuments' {} a -> s {contentType = a} :: UploadDocuments)

-- | A batch of documents formatted in JSON or HTML.
uploadDocuments_documents :: Lens.Lens' UploadDocuments Data.HashedBody
uploadDocuments_documents = Lens.lens (\UploadDocuments' {documents} -> documents) (\s@UploadDocuments' {} a -> s {documents = a} :: UploadDocuments)

instance Core.AWSRequest UploadDocuments where
  type
    AWSResponse UploadDocuments =
      UploadDocumentsResponse
  request overrides =
    Request.postBody (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UploadDocumentsResponse'
            Prelude.<$> (x Data..?> "adds")
            Prelude.<*> (x Data..?> "deletes")
            Prelude.<*> (x Data..?> "status")
            Prelude.<*> (x Data..?> "warnings" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Data.ToBody UploadDocuments where
  toBody UploadDocuments' {..} = Data.toBody documents

instance Data.ToHeaders UploadDocuments where
  toHeaders UploadDocuments' {..} =
    Prelude.mconcat
      ["Content-Type" Data.=# contentType]

instance Data.ToPath UploadDocuments where
  toPath = Prelude.const "/2013-01-01/documents/batch"

instance Data.ToQuery UploadDocuments where
  toQuery =
    Prelude.const (Prelude.mconcat ["format=sdk"])

-- | Contains the response to an @UploadDocuments@ request.
--
-- /See:/ 'newUploadDocumentsResponse' smart constructor.
data UploadDocumentsResponse = UploadDocumentsResponse'
  { -- | The number of documents that were added to the search domain.
    adds :: Prelude.Maybe Prelude.Integer,
    -- | The number of documents that were deleted from the search domain.
    deletes :: Prelude.Maybe Prelude.Integer,
    -- | The status of an @UploadDocumentsRequest@.
    status :: Prelude.Maybe Prelude.Text,
    -- | Any warnings returned by the document service about the documents being
    -- uploaded.
    warnings :: Prelude.Maybe [DocumentServiceWarning],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UploadDocumentsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'adds', 'uploadDocumentsResponse_adds' - The number of documents that were added to the search domain.
--
-- 'deletes', 'uploadDocumentsResponse_deletes' - The number of documents that were deleted from the search domain.
--
-- 'status', 'uploadDocumentsResponse_status' - The status of an @UploadDocumentsRequest@.
--
-- 'warnings', 'uploadDocumentsResponse_warnings' - Any warnings returned by the document service about the documents being
-- uploaded.
--
-- 'httpStatus', 'uploadDocumentsResponse_httpStatus' - The response's http status code.
newUploadDocumentsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UploadDocumentsResponse
newUploadDocumentsResponse pHttpStatus_ =
  UploadDocumentsResponse'
    { adds = Prelude.Nothing,
      deletes = Prelude.Nothing,
      status = Prelude.Nothing,
      warnings = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The number of documents that were added to the search domain.
uploadDocumentsResponse_adds :: Lens.Lens' UploadDocumentsResponse (Prelude.Maybe Prelude.Integer)
uploadDocumentsResponse_adds = Lens.lens (\UploadDocumentsResponse' {adds} -> adds) (\s@UploadDocumentsResponse' {} a -> s {adds = a} :: UploadDocumentsResponse)

-- | The number of documents that were deleted from the search domain.
uploadDocumentsResponse_deletes :: Lens.Lens' UploadDocumentsResponse (Prelude.Maybe Prelude.Integer)
uploadDocumentsResponse_deletes = Lens.lens (\UploadDocumentsResponse' {deletes} -> deletes) (\s@UploadDocumentsResponse' {} a -> s {deletes = a} :: UploadDocumentsResponse)

-- | The status of an @UploadDocumentsRequest@.
uploadDocumentsResponse_status :: Lens.Lens' UploadDocumentsResponse (Prelude.Maybe Prelude.Text)
uploadDocumentsResponse_status = Lens.lens (\UploadDocumentsResponse' {status} -> status) (\s@UploadDocumentsResponse' {} a -> s {status = a} :: UploadDocumentsResponse)

-- | Any warnings returned by the document service about the documents being
-- uploaded.
uploadDocumentsResponse_warnings :: Lens.Lens' UploadDocumentsResponse (Prelude.Maybe [DocumentServiceWarning])
uploadDocumentsResponse_warnings = Lens.lens (\UploadDocumentsResponse' {warnings} -> warnings) (\s@UploadDocumentsResponse' {} a -> s {warnings = a} :: UploadDocumentsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
uploadDocumentsResponse_httpStatus :: Lens.Lens' UploadDocumentsResponse Prelude.Int
uploadDocumentsResponse_httpStatus = Lens.lens (\UploadDocumentsResponse' {httpStatus} -> httpStatus) (\s@UploadDocumentsResponse' {} a -> s {httpStatus = a} :: UploadDocumentsResponse)

instance Prelude.NFData UploadDocumentsResponse where
  rnf UploadDocumentsResponse' {..} =
    Prelude.rnf adds `Prelude.seq`
      Prelude.rnf deletes `Prelude.seq`
        Prelude.rnf status `Prelude.seq`
          Prelude.rnf warnings `Prelude.seq`
            Prelude.rnf httpStatus
