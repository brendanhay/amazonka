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
-- Module      : Amazonka.Omics.ListRuns
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list of runs.
--
-- This operation returns paginated results.
module Amazonka.Omics.ListRuns
  ( -- * Creating a Request
    ListRuns (..),
    newListRuns,

    -- * Request Lenses
    listRuns_maxResults,
    listRuns_name,
    listRuns_runGroupId,
    listRuns_startingToken,

    -- * Destructuring the Response
    ListRunsResponse (..),
    newListRunsResponse,

    -- * Response Lenses
    listRunsResponse_items,
    listRunsResponse_nextToken,
    listRunsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Omics.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListRuns' smart constructor.
data ListRuns = ListRuns'
  { -- | The maximum number of runs to return in one page of results.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | Filter the list by run name.
    name :: Prelude.Maybe Prelude.Text,
    -- | Filter the list by run group ID.
    runGroupId :: Prelude.Maybe Prelude.Text,
    -- | Specify the pagination token from a previous request to retrieve the
    -- next page of results.
    startingToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListRuns' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listRuns_maxResults' - The maximum number of runs to return in one page of results.
--
-- 'name', 'listRuns_name' - Filter the list by run name.
--
-- 'runGroupId', 'listRuns_runGroupId' - Filter the list by run group ID.
--
-- 'startingToken', 'listRuns_startingToken' - Specify the pagination token from a previous request to retrieve the
-- next page of results.
newListRuns ::
  ListRuns
newListRuns =
  ListRuns'
    { maxResults = Prelude.Nothing,
      name = Prelude.Nothing,
      runGroupId = Prelude.Nothing,
      startingToken = Prelude.Nothing
    }

-- | The maximum number of runs to return in one page of results.
listRuns_maxResults :: Lens.Lens' ListRuns (Prelude.Maybe Prelude.Natural)
listRuns_maxResults = Lens.lens (\ListRuns' {maxResults} -> maxResults) (\s@ListRuns' {} a -> s {maxResults = a} :: ListRuns)

-- | Filter the list by run name.
listRuns_name :: Lens.Lens' ListRuns (Prelude.Maybe Prelude.Text)
listRuns_name = Lens.lens (\ListRuns' {name} -> name) (\s@ListRuns' {} a -> s {name = a} :: ListRuns)

-- | Filter the list by run group ID.
listRuns_runGroupId :: Lens.Lens' ListRuns (Prelude.Maybe Prelude.Text)
listRuns_runGroupId = Lens.lens (\ListRuns' {runGroupId} -> runGroupId) (\s@ListRuns' {} a -> s {runGroupId = a} :: ListRuns)

-- | Specify the pagination token from a previous request to retrieve the
-- next page of results.
listRuns_startingToken :: Lens.Lens' ListRuns (Prelude.Maybe Prelude.Text)
listRuns_startingToken = Lens.lens (\ListRuns' {startingToken} -> startingToken) (\s@ListRuns' {} a -> s {startingToken = a} :: ListRuns)

instance Core.AWSPager ListRuns where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listRunsResponse_nextToken Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listRunsResponse_items Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listRuns_startingToken
          Lens..~ rs
          Lens.^? listRunsResponse_nextToken Prelude.. Lens._Just

instance Core.AWSRequest ListRuns where
  type AWSResponse ListRuns = ListRunsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListRunsResponse'
            Prelude.<$> (x Data..?> "items" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListRuns where
  hashWithSalt _salt ListRuns' {..} =
    _salt `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` runGroupId
      `Prelude.hashWithSalt` startingToken

instance Prelude.NFData ListRuns where
  rnf ListRuns' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf runGroupId
      `Prelude.seq` Prelude.rnf startingToken

instance Data.ToHeaders ListRuns where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListRuns where
  toPath = Prelude.const "/run"

instance Data.ToQuery ListRuns where
  toQuery ListRuns' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "name" Data.=: name,
        "runGroupId" Data.=: runGroupId,
        "startingToken" Data.=: startingToken
      ]

-- | /See:/ 'newListRunsResponse' smart constructor.
data ListRunsResponse = ListRunsResponse'
  { -- | A list of runs.
    items :: Prelude.Maybe [RunListItem],
    -- | A pagination token that\'s included if more results are available.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListRunsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'items', 'listRunsResponse_items' - A list of runs.
--
-- 'nextToken', 'listRunsResponse_nextToken' - A pagination token that\'s included if more results are available.
--
-- 'httpStatus', 'listRunsResponse_httpStatus' - The response's http status code.
newListRunsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListRunsResponse
newListRunsResponse pHttpStatus_ =
  ListRunsResponse'
    { items = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of runs.
listRunsResponse_items :: Lens.Lens' ListRunsResponse (Prelude.Maybe [RunListItem])
listRunsResponse_items = Lens.lens (\ListRunsResponse' {items} -> items) (\s@ListRunsResponse' {} a -> s {items = a} :: ListRunsResponse) Prelude.. Lens.mapping Lens.coerced

-- | A pagination token that\'s included if more results are available.
listRunsResponse_nextToken :: Lens.Lens' ListRunsResponse (Prelude.Maybe Prelude.Text)
listRunsResponse_nextToken = Lens.lens (\ListRunsResponse' {nextToken} -> nextToken) (\s@ListRunsResponse' {} a -> s {nextToken = a} :: ListRunsResponse)

-- | The response's http status code.
listRunsResponse_httpStatus :: Lens.Lens' ListRunsResponse Prelude.Int
listRunsResponse_httpStatus = Lens.lens (\ListRunsResponse' {httpStatus} -> httpStatus) (\s@ListRunsResponse' {} a -> s {httpStatus = a} :: ListRunsResponse)

instance Prelude.NFData ListRunsResponse where
  rnf ListRunsResponse' {..} =
    Prelude.rnf items
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
