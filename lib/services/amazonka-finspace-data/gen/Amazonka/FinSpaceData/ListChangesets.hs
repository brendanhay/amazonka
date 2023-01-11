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
-- Module      : Amazonka.FinSpaceData.ListChangesets
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the FinSpace Changesets for a Dataset.
--
-- This operation returns paginated results.
module Amazonka.FinSpaceData.ListChangesets
  ( -- * Creating a Request
    ListChangesets (..),
    newListChangesets,

    -- * Request Lenses
    listChangesets_maxResults,
    listChangesets_nextToken,
    listChangesets_datasetId,

    -- * Destructuring the Response
    ListChangesetsResponse (..),
    newListChangesetsResponse,

    -- * Response Lenses
    listChangesetsResponse_changesets,
    listChangesetsResponse_nextToken,
    listChangesetsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FinSpaceData.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Request to ListChangesetsRequest. It exposes minimal query filters.
--
-- /See:/ 'newListChangesets' smart constructor.
data ListChangesets = ListChangesets'
  { -- | The maximum number of results per page.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | A token that indicates where a results page should begin.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier for the FinSpace Dataset to which the Changeset
    -- belongs.
    datasetId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListChangesets' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listChangesets_maxResults' - The maximum number of results per page.
--
-- 'nextToken', 'listChangesets_nextToken' - A token that indicates where a results page should begin.
--
-- 'datasetId', 'listChangesets_datasetId' - The unique identifier for the FinSpace Dataset to which the Changeset
-- belongs.
newListChangesets ::
  -- | 'datasetId'
  Prelude.Text ->
  ListChangesets
newListChangesets pDatasetId_ =
  ListChangesets'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      datasetId = pDatasetId_
    }

-- | The maximum number of results per page.
listChangesets_maxResults :: Lens.Lens' ListChangesets (Prelude.Maybe Prelude.Natural)
listChangesets_maxResults = Lens.lens (\ListChangesets' {maxResults} -> maxResults) (\s@ListChangesets' {} a -> s {maxResults = a} :: ListChangesets)

-- | A token that indicates where a results page should begin.
listChangesets_nextToken :: Lens.Lens' ListChangesets (Prelude.Maybe Prelude.Text)
listChangesets_nextToken = Lens.lens (\ListChangesets' {nextToken} -> nextToken) (\s@ListChangesets' {} a -> s {nextToken = a} :: ListChangesets)

-- | The unique identifier for the FinSpace Dataset to which the Changeset
-- belongs.
listChangesets_datasetId :: Lens.Lens' ListChangesets Prelude.Text
listChangesets_datasetId = Lens.lens (\ListChangesets' {datasetId} -> datasetId) (\s@ListChangesets' {} a -> s {datasetId = a} :: ListChangesets)

instance Core.AWSPager ListChangesets where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listChangesetsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listChangesetsResponse_changesets
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listChangesets_nextToken
          Lens..~ rs
          Lens.^? listChangesetsResponse_nextToken Prelude.. Lens._Just

instance Core.AWSRequest ListChangesets where
  type
    AWSResponse ListChangesets =
      ListChangesetsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListChangesetsResponse'
            Prelude.<$> (x Data..?> "changesets" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListChangesets where
  hashWithSalt _salt ListChangesets' {..} =
    _salt `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` datasetId

instance Prelude.NFData ListChangesets where
  rnf ListChangesets' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf datasetId

instance Data.ToHeaders ListChangesets where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListChangesets where
  toPath ListChangesets' {..} =
    Prelude.mconcat
      ["/datasets/", Data.toBS datasetId, "/changesetsv2"]

instance Data.ToQuery ListChangesets where
  toQuery ListChangesets' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken
      ]

-- | Response to ListChangesetsResponse. This returns a list of dataset
-- changesets that match the query criteria.
--
-- /See:/ 'newListChangesetsResponse' smart constructor.
data ListChangesetsResponse = ListChangesetsResponse'
  { -- | List of Changesets found.
    changesets :: Prelude.Maybe [ChangesetSummary],
    -- | A token that indicates where a results page should begin.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListChangesetsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'changesets', 'listChangesetsResponse_changesets' - List of Changesets found.
--
-- 'nextToken', 'listChangesetsResponse_nextToken' - A token that indicates where a results page should begin.
--
-- 'httpStatus', 'listChangesetsResponse_httpStatus' - The response's http status code.
newListChangesetsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListChangesetsResponse
newListChangesetsResponse pHttpStatus_ =
  ListChangesetsResponse'
    { changesets =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | List of Changesets found.
listChangesetsResponse_changesets :: Lens.Lens' ListChangesetsResponse (Prelude.Maybe [ChangesetSummary])
listChangesetsResponse_changesets = Lens.lens (\ListChangesetsResponse' {changesets} -> changesets) (\s@ListChangesetsResponse' {} a -> s {changesets = a} :: ListChangesetsResponse) Prelude.. Lens.mapping Lens.coerced

-- | A token that indicates where a results page should begin.
listChangesetsResponse_nextToken :: Lens.Lens' ListChangesetsResponse (Prelude.Maybe Prelude.Text)
listChangesetsResponse_nextToken = Lens.lens (\ListChangesetsResponse' {nextToken} -> nextToken) (\s@ListChangesetsResponse' {} a -> s {nextToken = a} :: ListChangesetsResponse)

-- | The response's http status code.
listChangesetsResponse_httpStatus :: Lens.Lens' ListChangesetsResponse Prelude.Int
listChangesetsResponse_httpStatus = Lens.lens (\ListChangesetsResponse' {httpStatus} -> httpStatus) (\s@ListChangesetsResponse' {} a -> s {httpStatus = a} :: ListChangesetsResponse)

instance Prelude.NFData ListChangesetsResponse where
  rnf ListChangesetsResponse' {..} =
    Prelude.rnf changesets
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
