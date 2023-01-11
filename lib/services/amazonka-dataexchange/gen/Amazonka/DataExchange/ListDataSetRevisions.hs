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
-- Module      : Amazonka.DataExchange.ListDataSetRevisions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation lists a data set\'s revisions sorted by CreatedAt in
-- descending order.
--
-- This operation returns paginated results.
module Amazonka.DataExchange.ListDataSetRevisions
  ( -- * Creating a Request
    ListDataSetRevisions (..),
    newListDataSetRevisions,

    -- * Request Lenses
    listDataSetRevisions_maxResults,
    listDataSetRevisions_nextToken,
    listDataSetRevisions_dataSetId,

    -- * Destructuring the Response
    ListDataSetRevisionsResponse (..),
    newListDataSetRevisionsResponse,

    -- * Response Lenses
    listDataSetRevisionsResponse_nextToken,
    listDataSetRevisionsResponse_revisions,
    listDataSetRevisionsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DataExchange.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListDataSetRevisions' smart constructor.
data ListDataSetRevisions = ListDataSetRevisions'
  { -- | The maximum number of results returned by a single call.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token value retrieved from a previous call to access the next page
    -- of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier for a data set.
    dataSetId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListDataSetRevisions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listDataSetRevisions_maxResults' - The maximum number of results returned by a single call.
--
-- 'nextToken', 'listDataSetRevisions_nextToken' - The token value retrieved from a previous call to access the next page
-- of results.
--
-- 'dataSetId', 'listDataSetRevisions_dataSetId' - The unique identifier for a data set.
newListDataSetRevisions ::
  -- | 'dataSetId'
  Prelude.Text ->
  ListDataSetRevisions
newListDataSetRevisions pDataSetId_ =
  ListDataSetRevisions'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      dataSetId = pDataSetId_
    }

-- | The maximum number of results returned by a single call.
listDataSetRevisions_maxResults :: Lens.Lens' ListDataSetRevisions (Prelude.Maybe Prelude.Natural)
listDataSetRevisions_maxResults = Lens.lens (\ListDataSetRevisions' {maxResults} -> maxResults) (\s@ListDataSetRevisions' {} a -> s {maxResults = a} :: ListDataSetRevisions)

-- | The token value retrieved from a previous call to access the next page
-- of results.
listDataSetRevisions_nextToken :: Lens.Lens' ListDataSetRevisions (Prelude.Maybe Prelude.Text)
listDataSetRevisions_nextToken = Lens.lens (\ListDataSetRevisions' {nextToken} -> nextToken) (\s@ListDataSetRevisions' {} a -> s {nextToken = a} :: ListDataSetRevisions)

-- | The unique identifier for a data set.
listDataSetRevisions_dataSetId :: Lens.Lens' ListDataSetRevisions Prelude.Text
listDataSetRevisions_dataSetId = Lens.lens (\ListDataSetRevisions' {dataSetId} -> dataSetId) (\s@ListDataSetRevisions' {} a -> s {dataSetId = a} :: ListDataSetRevisions)

instance Core.AWSPager ListDataSetRevisions where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listDataSetRevisionsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listDataSetRevisionsResponse_revisions
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listDataSetRevisions_nextToken
          Lens..~ rs
          Lens.^? listDataSetRevisionsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListDataSetRevisions where
  type
    AWSResponse ListDataSetRevisions =
      ListDataSetRevisionsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListDataSetRevisionsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "Revisions" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListDataSetRevisions where
  hashWithSalt _salt ListDataSetRevisions' {..} =
    _salt `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` dataSetId

instance Prelude.NFData ListDataSetRevisions where
  rnf ListDataSetRevisions' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf dataSetId

instance Data.ToHeaders ListDataSetRevisions where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListDataSetRevisions where
  toPath ListDataSetRevisions' {..} =
    Prelude.mconcat
      ["/v1/data-sets/", Data.toBS dataSetId, "/revisions"]

instance Data.ToQuery ListDataSetRevisions where
  toQuery ListDataSetRevisions' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken
      ]

-- | /See:/ 'newListDataSetRevisionsResponse' smart constructor.
data ListDataSetRevisionsResponse = ListDataSetRevisionsResponse'
  { -- | The token value retrieved from a previous call to access the next page
    -- of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The asset objects listed by the request.
    revisions :: Prelude.Maybe [RevisionEntry],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListDataSetRevisionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listDataSetRevisionsResponse_nextToken' - The token value retrieved from a previous call to access the next page
-- of results.
--
-- 'revisions', 'listDataSetRevisionsResponse_revisions' - The asset objects listed by the request.
--
-- 'httpStatus', 'listDataSetRevisionsResponse_httpStatus' - The response's http status code.
newListDataSetRevisionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListDataSetRevisionsResponse
newListDataSetRevisionsResponse pHttpStatus_ =
  ListDataSetRevisionsResponse'
    { nextToken =
        Prelude.Nothing,
      revisions = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token value retrieved from a previous call to access the next page
-- of results.
listDataSetRevisionsResponse_nextToken :: Lens.Lens' ListDataSetRevisionsResponse (Prelude.Maybe Prelude.Text)
listDataSetRevisionsResponse_nextToken = Lens.lens (\ListDataSetRevisionsResponse' {nextToken} -> nextToken) (\s@ListDataSetRevisionsResponse' {} a -> s {nextToken = a} :: ListDataSetRevisionsResponse)

-- | The asset objects listed by the request.
listDataSetRevisionsResponse_revisions :: Lens.Lens' ListDataSetRevisionsResponse (Prelude.Maybe [RevisionEntry])
listDataSetRevisionsResponse_revisions = Lens.lens (\ListDataSetRevisionsResponse' {revisions} -> revisions) (\s@ListDataSetRevisionsResponse' {} a -> s {revisions = a} :: ListDataSetRevisionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listDataSetRevisionsResponse_httpStatus :: Lens.Lens' ListDataSetRevisionsResponse Prelude.Int
listDataSetRevisionsResponse_httpStatus = Lens.lens (\ListDataSetRevisionsResponse' {httpStatus} -> httpStatus) (\s@ListDataSetRevisionsResponse' {} a -> s {httpStatus = a} :: ListDataSetRevisionsResponse)

instance Prelude.NFData ListDataSetRevisionsResponse where
  rnf ListDataSetRevisionsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf revisions
      `Prelude.seq` Prelude.rnf httpStatus
