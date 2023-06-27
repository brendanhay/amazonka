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
-- Module      : Amazonka.Omics.ListReadSetUploadParts
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation will list all parts in a requested multipart upload for a
-- sequence store.
--
-- This operation returns paginated results.
module Amazonka.Omics.ListReadSetUploadParts
  ( -- * Creating a Request
    ListReadSetUploadParts (..),
    newListReadSetUploadParts,

    -- * Request Lenses
    listReadSetUploadParts_filter,
    listReadSetUploadParts_maxResults,
    listReadSetUploadParts_nextToken,
    listReadSetUploadParts_sequenceStoreId,
    listReadSetUploadParts_uploadId,
    listReadSetUploadParts_partSource,

    -- * Destructuring the Response
    ListReadSetUploadPartsResponse (..),
    newListReadSetUploadPartsResponse,

    -- * Response Lenses
    listReadSetUploadPartsResponse_nextToken,
    listReadSetUploadPartsResponse_parts,
    listReadSetUploadPartsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Omics.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListReadSetUploadParts' smart constructor.
data ListReadSetUploadParts = ListReadSetUploadParts'
  { -- | Attributes used to filter for a specific subset of read set part
    -- uploads.
    filter' :: Prelude.Maybe ReadSetUploadPartListFilter,
    -- | The maximum number of read set upload parts returned in a page.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | Next token returned in the response of a previous
    -- ListReadSetUploadPartsRequest call. Used to get the next page of
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The Sequence Store ID used for the multipart uploads.
    sequenceStoreId :: Prelude.Text,
    -- | The ID for the initiated multipart upload.
    uploadId :: Prelude.Text,
    -- | The source file for the upload part.
    partSource :: ReadSetPartSource
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListReadSetUploadParts' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filter'', 'listReadSetUploadParts_filter' - Attributes used to filter for a specific subset of read set part
-- uploads.
--
-- 'maxResults', 'listReadSetUploadParts_maxResults' - The maximum number of read set upload parts returned in a page.
--
-- 'nextToken', 'listReadSetUploadParts_nextToken' - Next token returned in the response of a previous
-- ListReadSetUploadPartsRequest call. Used to get the next page of
-- results.
--
-- 'sequenceStoreId', 'listReadSetUploadParts_sequenceStoreId' - The Sequence Store ID used for the multipart uploads.
--
-- 'uploadId', 'listReadSetUploadParts_uploadId' - The ID for the initiated multipart upload.
--
-- 'partSource', 'listReadSetUploadParts_partSource' - The source file for the upload part.
newListReadSetUploadParts ::
  -- | 'sequenceStoreId'
  Prelude.Text ->
  -- | 'uploadId'
  Prelude.Text ->
  -- | 'partSource'
  ReadSetPartSource ->
  ListReadSetUploadParts
newListReadSetUploadParts
  pSequenceStoreId_
  pUploadId_
  pPartSource_ =
    ListReadSetUploadParts'
      { filter' = Prelude.Nothing,
        maxResults = Prelude.Nothing,
        nextToken = Prelude.Nothing,
        sequenceStoreId = pSequenceStoreId_,
        uploadId = pUploadId_,
        partSource = pPartSource_
      }

-- | Attributes used to filter for a specific subset of read set part
-- uploads.
listReadSetUploadParts_filter :: Lens.Lens' ListReadSetUploadParts (Prelude.Maybe ReadSetUploadPartListFilter)
listReadSetUploadParts_filter = Lens.lens (\ListReadSetUploadParts' {filter'} -> filter') (\s@ListReadSetUploadParts' {} a -> s {filter' = a} :: ListReadSetUploadParts)

-- | The maximum number of read set upload parts returned in a page.
listReadSetUploadParts_maxResults :: Lens.Lens' ListReadSetUploadParts (Prelude.Maybe Prelude.Natural)
listReadSetUploadParts_maxResults = Lens.lens (\ListReadSetUploadParts' {maxResults} -> maxResults) (\s@ListReadSetUploadParts' {} a -> s {maxResults = a} :: ListReadSetUploadParts)

-- | Next token returned in the response of a previous
-- ListReadSetUploadPartsRequest call. Used to get the next page of
-- results.
listReadSetUploadParts_nextToken :: Lens.Lens' ListReadSetUploadParts (Prelude.Maybe Prelude.Text)
listReadSetUploadParts_nextToken = Lens.lens (\ListReadSetUploadParts' {nextToken} -> nextToken) (\s@ListReadSetUploadParts' {} a -> s {nextToken = a} :: ListReadSetUploadParts)

-- | The Sequence Store ID used for the multipart uploads.
listReadSetUploadParts_sequenceStoreId :: Lens.Lens' ListReadSetUploadParts Prelude.Text
listReadSetUploadParts_sequenceStoreId = Lens.lens (\ListReadSetUploadParts' {sequenceStoreId} -> sequenceStoreId) (\s@ListReadSetUploadParts' {} a -> s {sequenceStoreId = a} :: ListReadSetUploadParts)

-- | The ID for the initiated multipart upload.
listReadSetUploadParts_uploadId :: Lens.Lens' ListReadSetUploadParts Prelude.Text
listReadSetUploadParts_uploadId = Lens.lens (\ListReadSetUploadParts' {uploadId} -> uploadId) (\s@ListReadSetUploadParts' {} a -> s {uploadId = a} :: ListReadSetUploadParts)

-- | The source file for the upload part.
listReadSetUploadParts_partSource :: Lens.Lens' ListReadSetUploadParts ReadSetPartSource
listReadSetUploadParts_partSource = Lens.lens (\ListReadSetUploadParts' {partSource} -> partSource) (\s@ListReadSetUploadParts' {} a -> s {partSource = a} :: ListReadSetUploadParts)

instance Core.AWSPager ListReadSetUploadParts where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listReadSetUploadPartsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listReadSetUploadPartsResponse_parts
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listReadSetUploadParts_nextToken
          Lens..~ rs
          Lens.^? listReadSetUploadPartsResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListReadSetUploadParts where
  type
    AWSResponse ListReadSetUploadParts =
      ListReadSetUploadPartsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListReadSetUploadPartsResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (x Data..?> "parts" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListReadSetUploadParts where
  hashWithSalt _salt ListReadSetUploadParts' {..} =
    _salt
      `Prelude.hashWithSalt` filter'
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` sequenceStoreId
      `Prelude.hashWithSalt` uploadId
      `Prelude.hashWithSalt` partSource

instance Prelude.NFData ListReadSetUploadParts where
  rnf ListReadSetUploadParts' {..} =
    Prelude.rnf filter'
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf sequenceStoreId
      `Prelude.seq` Prelude.rnf uploadId
      `Prelude.seq` Prelude.rnf partSource

instance Data.ToHeaders ListReadSetUploadParts where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListReadSetUploadParts where
  toJSON ListReadSetUploadParts' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("filter" Data..=) Prelude.<$> filter',
            Prelude.Just ("partSource" Data..= partSource)
          ]
      )

instance Data.ToPath ListReadSetUploadParts where
  toPath ListReadSetUploadParts' {..} =
    Prelude.mconcat
      [ "/sequencestore/",
        Data.toBS sequenceStoreId,
        "/upload/",
        Data.toBS uploadId,
        "/parts"
      ]

instance Data.ToQuery ListReadSetUploadParts where
  toQuery ListReadSetUploadParts' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken
      ]

-- | /See:/ 'newListReadSetUploadPartsResponse' smart constructor.
data ListReadSetUploadPartsResponse = ListReadSetUploadPartsResponse'
  { -- | Next token returned in the response of a previous ListReadSetUploadParts
    -- call. Used to get the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | An array of upload parts.
    parts :: Prelude.Maybe [ReadSetUploadPartListItem],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListReadSetUploadPartsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listReadSetUploadPartsResponse_nextToken' - Next token returned in the response of a previous ListReadSetUploadParts
-- call. Used to get the next page of results.
--
-- 'parts', 'listReadSetUploadPartsResponse_parts' - An array of upload parts.
--
-- 'httpStatus', 'listReadSetUploadPartsResponse_httpStatus' - The response's http status code.
newListReadSetUploadPartsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListReadSetUploadPartsResponse
newListReadSetUploadPartsResponse pHttpStatus_ =
  ListReadSetUploadPartsResponse'
    { nextToken =
        Prelude.Nothing,
      parts = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Next token returned in the response of a previous ListReadSetUploadParts
-- call. Used to get the next page of results.
listReadSetUploadPartsResponse_nextToken :: Lens.Lens' ListReadSetUploadPartsResponse (Prelude.Maybe Prelude.Text)
listReadSetUploadPartsResponse_nextToken = Lens.lens (\ListReadSetUploadPartsResponse' {nextToken} -> nextToken) (\s@ListReadSetUploadPartsResponse' {} a -> s {nextToken = a} :: ListReadSetUploadPartsResponse)

-- | An array of upload parts.
listReadSetUploadPartsResponse_parts :: Lens.Lens' ListReadSetUploadPartsResponse (Prelude.Maybe [ReadSetUploadPartListItem])
listReadSetUploadPartsResponse_parts = Lens.lens (\ListReadSetUploadPartsResponse' {parts} -> parts) (\s@ListReadSetUploadPartsResponse' {} a -> s {parts = a} :: ListReadSetUploadPartsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listReadSetUploadPartsResponse_httpStatus :: Lens.Lens' ListReadSetUploadPartsResponse Prelude.Int
listReadSetUploadPartsResponse_httpStatus = Lens.lens (\ListReadSetUploadPartsResponse' {httpStatus} -> httpStatus) (\s@ListReadSetUploadPartsResponse' {} a -> s {httpStatus = a} :: ListReadSetUploadPartsResponse)

instance
  Prelude.NFData
    ListReadSetUploadPartsResponse
  where
  rnf ListReadSetUploadPartsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf parts
      `Prelude.seq` Prelude.rnf httpStatus
