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
-- Module      : Amazonka.Omics.ListRunGroups
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list of run groups.
--
-- This operation returns paginated results.
module Amazonka.Omics.ListRunGroups
  ( -- * Creating a Request
    ListRunGroups (..),
    newListRunGroups,

    -- * Request Lenses
    listRunGroups_maxResults,
    listRunGroups_name,
    listRunGroups_startingToken,

    -- * Destructuring the Response
    ListRunGroupsResponse (..),
    newListRunGroupsResponse,

    -- * Response Lenses
    listRunGroupsResponse_items,
    listRunGroupsResponse_nextToken,
    listRunGroupsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Omics.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListRunGroups' smart constructor.
data ListRunGroups = ListRunGroups'
  { -- | The maximum number of run groups to return in one page of results.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The run groups\' name.
    name :: Prelude.Maybe Prelude.Text,
    -- | Specify the pagination token from a previous request to retrieve the
    -- next page of results.
    startingToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListRunGroups' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listRunGroups_maxResults' - The maximum number of run groups to return in one page of results.
--
-- 'name', 'listRunGroups_name' - The run groups\' name.
--
-- 'startingToken', 'listRunGroups_startingToken' - Specify the pagination token from a previous request to retrieve the
-- next page of results.
newListRunGroups ::
  ListRunGroups
newListRunGroups =
  ListRunGroups'
    { maxResults = Prelude.Nothing,
      name = Prelude.Nothing,
      startingToken = Prelude.Nothing
    }

-- | The maximum number of run groups to return in one page of results.
listRunGroups_maxResults :: Lens.Lens' ListRunGroups (Prelude.Maybe Prelude.Natural)
listRunGroups_maxResults = Lens.lens (\ListRunGroups' {maxResults} -> maxResults) (\s@ListRunGroups' {} a -> s {maxResults = a} :: ListRunGroups)

-- | The run groups\' name.
listRunGroups_name :: Lens.Lens' ListRunGroups (Prelude.Maybe Prelude.Text)
listRunGroups_name = Lens.lens (\ListRunGroups' {name} -> name) (\s@ListRunGroups' {} a -> s {name = a} :: ListRunGroups)

-- | Specify the pagination token from a previous request to retrieve the
-- next page of results.
listRunGroups_startingToken :: Lens.Lens' ListRunGroups (Prelude.Maybe Prelude.Text)
listRunGroups_startingToken = Lens.lens (\ListRunGroups' {startingToken} -> startingToken) (\s@ListRunGroups' {} a -> s {startingToken = a} :: ListRunGroups)

instance Core.AWSPager ListRunGroups where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listRunGroupsResponse_nextToken Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listRunGroupsResponse_items Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listRunGroups_startingToken
          Lens..~ rs
          Lens.^? listRunGroupsResponse_nextToken Prelude.. Lens._Just

instance Core.AWSRequest ListRunGroups where
  type
    AWSResponse ListRunGroups =
      ListRunGroupsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListRunGroupsResponse'
            Prelude.<$> (x Data..?> "items" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListRunGroups where
  hashWithSalt _salt ListRunGroups' {..} =
    _salt `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` startingToken

instance Prelude.NFData ListRunGroups where
  rnf ListRunGroups' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf startingToken

instance Data.ToHeaders ListRunGroups where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListRunGroups where
  toPath = Prelude.const "/runGroup"

instance Data.ToQuery ListRunGroups where
  toQuery ListRunGroups' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "name" Data.=: name,
        "startingToken" Data.=: startingToken
      ]

-- | /See:/ 'newListRunGroupsResponse' smart constructor.
data ListRunGroupsResponse = ListRunGroupsResponse'
  { -- | A list of groups.
    items :: Prelude.Maybe [RunGroupListItem],
    -- | A pagination token that\'s included if more results are available.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListRunGroupsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'items', 'listRunGroupsResponse_items' - A list of groups.
--
-- 'nextToken', 'listRunGroupsResponse_nextToken' - A pagination token that\'s included if more results are available.
--
-- 'httpStatus', 'listRunGroupsResponse_httpStatus' - The response's http status code.
newListRunGroupsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListRunGroupsResponse
newListRunGroupsResponse pHttpStatus_ =
  ListRunGroupsResponse'
    { items = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of groups.
listRunGroupsResponse_items :: Lens.Lens' ListRunGroupsResponse (Prelude.Maybe [RunGroupListItem])
listRunGroupsResponse_items = Lens.lens (\ListRunGroupsResponse' {items} -> items) (\s@ListRunGroupsResponse' {} a -> s {items = a} :: ListRunGroupsResponse) Prelude.. Lens.mapping Lens.coerced

-- | A pagination token that\'s included if more results are available.
listRunGroupsResponse_nextToken :: Lens.Lens' ListRunGroupsResponse (Prelude.Maybe Prelude.Text)
listRunGroupsResponse_nextToken = Lens.lens (\ListRunGroupsResponse' {nextToken} -> nextToken) (\s@ListRunGroupsResponse' {} a -> s {nextToken = a} :: ListRunGroupsResponse)

-- | The response's http status code.
listRunGroupsResponse_httpStatus :: Lens.Lens' ListRunGroupsResponse Prelude.Int
listRunGroupsResponse_httpStatus = Lens.lens (\ListRunGroupsResponse' {httpStatus} -> httpStatus) (\s@ListRunGroupsResponse' {} a -> s {httpStatus = a} :: ListRunGroupsResponse)

instance Prelude.NFData ListRunGroupsResponse where
  rnf ListRunGroupsResponse' {..} =
    Prelude.rnf items
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
