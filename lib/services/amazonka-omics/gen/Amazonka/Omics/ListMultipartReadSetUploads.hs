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
-- Module      : Amazonka.Omics.ListMultipartReadSetUploads
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all multipart read set uploads and their statuses.
--
-- This operation returns paginated results.
module Amazonka.Omics.ListMultipartReadSetUploads
  ( -- * Creating a Request
    ListMultipartReadSetUploads (..),
    newListMultipartReadSetUploads,

    -- * Request Lenses
    listMultipartReadSetUploads_maxResults,
    listMultipartReadSetUploads_nextToken,
    listMultipartReadSetUploads_sequenceStoreId,

    -- * Destructuring the Response
    ListMultipartReadSetUploadsResponse (..),
    newListMultipartReadSetUploadsResponse,

    -- * Response Lenses
    listMultipartReadSetUploadsResponse_nextToken,
    listMultipartReadSetUploadsResponse_uploads,
    listMultipartReadSetUploadsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Omics.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListMultipartReadSetUploads' smart constructor.
data ListMultipartReadSetUploads = ListMultipartReadSetUploads'
  { -- | The maximum number of multipart uploads returned in a page.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | Next token returned in the response of a previous
    -- ListMultipartReadSetUploads call. Used to get the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The Sequence Store ID used for the multipart uploads.
    sequenceStoreId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListMultipartReadSetUploads' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listMultipartReadSetUploads_maxResults' - The maximum number of multipart uploads returned in a page.
--
-- 'nextToken', 'listMultipartReadSetUploads_nextToken' - Next token returned in the response of a previous
-- ListMultipartReadSetUploads call. Used to get the next page of results.
--
-- 'sequenceStoreId', 'listMultipartReadSetUploads_sequenceStoreId' - The Sequence Store ID used for the multipart uploads.
newListMultipartReadSetUploads ::
  -- | 'sequenceStoreId'
  Prelude.Text ->
  ListMultipartReadSetUploads
newListMultipartReadSetUploads pSequenceStoreId_ =
  ListMultipartReadSetUploads'
    { maxResults =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      sequenceStoreId = pSequenceStoreId_
    }

-- | The maximum number of multipart uploads returned in a page.
listMultipartReadSetUploads_maxResults :: Lens.Lens' ListMultipartReadSetUploads (Prelude.Maybe Prelude.Natural)
listMultipartReadSetUploads_maxResults = Lens.lens (\ListMultipartReadSetUploads' {maxResults} -> maxResults) (\s@ListMultipartReadSetUploads' {} a -> s {maxResults = a} :: ListMultipartReadSetUploads)

-- | Next token returned in the response of a previous
-- ListMultipartReadSetUploads call. Used to get the next page of results.
listMultipartReadSetUploads_nextToken :: Lens.Lens' ListMultipartReadSetUploads (Prelude.Maybe Prelude.Text)
listMultipartReadSetUploads_nextToken = Lens.lens (\ListMultipartReadSetUploads' {nextToken} -> nextToken) (\s@ListMultipartReadSetUploads' {} a -> s {nextToken = a} :: ListMultipartReadSetUploads)

-- | The Sequence Store ID used for the multipart uploads.
listMultipartReadSetUploads_sequenceStoreId :: Lens.Lens' ListMultipartReadSetUploads Prelude.Text
listMultipartReadSetUploads_sequenceStoreId = Lens.lens (\ListMultipartReadSetUploads' {sequenceStoreId} -> sequenceStoreId) (\s@ListMultipartReadSetUploads' {} a -> s {sequenceStoreId = a} :: ListMultipartReadSetUploads)

instance Core.AWSPager ListMultipartReadSetUploads where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listMultipartReadSetUploadsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listMultipartReadSetUploadsResponse_uploads
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listMultipartReadSetUploads_nextToken
          Lens..~ rs
          Lens.^? listMultipartReadSetUploadsResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListMultipartReadSetUploads where
  type
    AWSResponse ListMultipartReadSetUploads =
      ListMultipartReadSetUploadsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListMultipartReadSetUploadsResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (x Data..?> "uploads" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListMultipartReadSetUploads where
  hashWithSalt _salt ListMultipartReadSetUploads' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` sequenceStoreId

instance Prelude.NFData ListMultipartReadSetUploads where
  rnf ListMultipartReadSetUploads' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf sequenceStoreId

instance Data.ToHeaders ListMultipartReadSetUploads where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListMultipartReadSetUploads where
  toJSON = Prelude.const (Data.Object Prelude.mempty)

instance Data.ToPath ListMultipartReadSetUploads where
  toPath ListMultipartReadSetUploads' {..} =
    Prelude.mconcat
      [ "/sequencestore/",
        Data.toBS sequenceStoreId,
        "/uploads"
      ]

instance Data.ToQuery ListMultipartReadSetUploads where
  toQuery ListMultipartReadSetUploads' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken
      ]

-- | /See:/ 'newListMultipartReadSetUploadsResponse' smart constructor.
data ListMultipartReadSetUploadsResponse = ListMultipartReadSetUploadsResponse'
  { -- | Next token returned in the response of a previous
    -- ListMultipartReadSetUploads call. Used to get the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | An array of multipart uploads.
    uploads :: Prelude.Maybe [MultipartReadSetUploadListItem],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListMultipartReadSetUploadsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listMultipartReadSetUploadsResponse_nextToken' - Next token returned in the response of a previous
-- ListMultipartReadSetUploads call. Used to get the next page of results.
--
-- 'uploads', 'listMultipartReadSetUploadsResponse_uploads' - An array of multipart uploads.
--
-- 'httpStatus', 'listMultipartReadSetUploadsResponse_httpStatus' - The response's http status code.
newListMultipartReadSetUploadsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListMultipartReadSetUploadsResponse
newListMultipartReadSetUploadsResponse pHttpStatus_ =
  ListMultipartReadSetUploadsResponse'
    { nextToken =
        Prelude.Nothing,
      uploads = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Next token returned in the response of a previous
-- ListMultipartReadSetUploads call. Used to get the next page of results.
listMultipartReadSetUploadsResponse_nextToken :: Lens.Lens' ListMultipartReadSetUploadsResponse (Prelude.Maybe Prelude.Text)
listMultipartReadSetUploadsResponse_nextToken = Lens.lens (\ListMultipartReadSetUploadsResponse' {nextToken} -> nextToken) (\s@ListMultipartReadSetUploadsResponse' {} a -> s {nextToken = a} :: ListMultipartReadSetUploadsResponse)

-- | An array of multipart uploads.
listMultipartReadSetUploadsResponse_uploads :: Lens.Lens' ListMultipartReadSetUploadsResponse (Prelude.Maybe [MultipartReadSetUploadListItem])
listMultipartReadSetUploadsResponse_uploads = Lens.lens (\ListMultipartReadSetUploadsResponse' {uploads} -> uploads) (\s@ListMultipartReadSetUploadsResponse' {} a -> s {uploads = a} :: ListMultipartReadSetUploadsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listMultipartReadSetUploadsResponse_httpStatus :: Lens.Lens' ListMultipartReadSetUploadsResponse Prelude.Int
listMultipartReadSetUploadsResponse_httpStatus = Lens.lens (\ListMultipartReadSetUploadsResponse' {httpStatus} -> httpStatus) (\s@ListMultipartReadSetUploadsResponse' {} a -> s {httpStatus = a} :: ListMultipartReadSetUploadsResponse)

instance
  Prelude.NFData
    ListMultipartReadSetUploadsResponse
  where
  rnf ListMultipartReadSetUploadsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf uploads
      `Prelude.seq` Prelude.rnf httpStatus
