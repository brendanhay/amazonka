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
-- Module      : Amazonka.MGN.ListSourceServerActions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List source server post migration custom actions.
--
-- This operation returns paginated results.
module Amazonka.MGN.ListSourceServerActions
  ( -- * Creating a Request
    ListSourceServerActions (..),
    newListSourceServerActions,

    -- * Request Lenses
    listSourceServerActions_filters,
    listSourceServerActions_maxResults,
    listSourceServerActions_nextToken,
    listSourceServerActions_sourceServerID,

    -- * Destructuring the Response
    ListSourceServerActionsResponse (..),
    newListSourceServerActionsResponse,

    -- * Response Lenses
    listSourceServerActionsResponse_items,
    listSourceServerActionsResponse_nextToken,
    listSourceServerActionsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MGN.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListSourceServerActions' smart constructor.
data ListSourceServerActions = ListSourceServerActions'
  { -- | Filters to apply when listing source server post migration custom
    -- actions.
    filters :: Prelude.Maybe SourceServerActionsRequestFilters,
    -- | Maximum amount of items to return when listing source server post
    -- migration custom actions.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | Next token to use when listing source server post migration custom
    -- actions.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Source server ID.
    sourceServerID :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListSourceServerActions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filters', 'listSourceServerActions_filters' - Filters to apply when listing source server post migration custom
-- actions.
--
-- 'maxResults', 'listSourceServerActions_maxResults' - Maximum amount of items to return when listing source server post
-- migration custom actions.
--
-- 'nextToken', 'listSourceServerActions_nextToken' - Next token to use when listing source server post migration custom
-- actions.
--
-- 'sourceServerID', 'listSourceServerActions_sourceServerID' - Source server ID.
newListSourceServerActions ::
  -- | 'sourceServerID'
  Prelude.Text ->
  ListSourceServerActions
newListSourceServerActions pSourceServerID_ =
  ListSourceServerActions'
    { filters = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      sourceServerID = pSourceServerID_
    }

-- | Filters to apply when listing source server post migration custom
-- actions.
listSourceServerActions_filters :: Lens.Lens' ListSourceServerActions (Prelude.Maybe SourceServerActionsRequestFilters)
listSourceServerActions_filters = Lens.lens (\ListSourceServerActions' {filters} -> filters) (\s@ListSourceServerActions' {} a -> s {filters = a} :: ListSourceServerActions)

-- | Maximum amount of items to return when listing source server post
-- migration custom actions.
listSourceServerActions_maxResults :: Lens.Lens' ListSourceServerActions (Prelude.Maybe Prelude.Natural)
listSourceServerActions_maxResults = Lens.lens (\ListSourceServerActions' {maxResults} -> maxResults) (\s@ListSourceServerActions' {} a -> s {maxResults = a} :: ListSourceServerActions)

-- | Next token to use when listing source server post migration custom
-- actions.
listSourceServerActions_nextToken :: Lens.Lens' ListSourceServerActions (Prelude.Maybe Prelude.Text)
listSourceServerActions_nextToken = Lens.lens (\ListSourceServerActions' {nextToken} -> nextToken) (\s@ListSourceServerActions' {} a -> s {nextToken = a} :: ListSourceServerActions)

-- | Source server ID.
listSourceServerActions_sourceServerID :: Lens.Lens' ListSourceServerActions Prelude.Text
listSourceServerActions_sourceServerID = Lens.lens (\ListSourceServerActions' {sourceServerID} -> sourceServerID) (\s@ListSourceServerActions' {} a -> s {sourceServerID = a} :: ListSourceServerActions)

instance Core.AWSPager ListSourceServerActions where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listSourceServerActionsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listSourceServerActionsResponse_items
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listSourceServerActions_nextToken
          Lens..~ rs
          Lens.^? listSourceServerActionsResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListSourceServerActions where
  type
    AWSResponse ListSourceServerActions =
      ListSourceServerActionsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListSourceServerActionsResponse'
            Prelude.<$> (x Data..?> "items" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListSourceServerActions where
  hashWithSalt _salt ListSourceServerActions' {..} =
    _salt
      `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` sourceServerID

instance Prelude.NFData ListSourceServerActions where
  rnf ListSourceServerActions' {..} =
    Prelude.rnf filters
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf sourceServerID

instance Data.ToHeaders ListSourceServerActions where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListSourceServerActions where
  toJSON ListSourceServerActions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("filters" Data..=) Prelude.<$> filters,
            ("maxResults" Data..=) Prelude.<$> maxResults,
            ("nextToken" Data..=) Prelude.<$> nextToken,
            Prelude.Just
              ("sourceServerID" Data..= sourceServerID)
          ]
      )

instance Data.ToPath ListSourceServerActions where
  toPath = Prelude.const "/ListSourceServerActions"

instance Data.ToQuery ListSourceServerActions where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListSourceServerActionsResponse' smart constructor.
data ListSourceServerActionsResponse = ListSourceServerActionsResponse'
  { -- | List of source server post migration custom actions.
    items :: Prelude.Maybe [SourceServerActionDocument],
    -- | Next token returned when listing source server post migration custom
    -- actions.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListSourceServerActionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'items', 'listSourceServerActionsResponse_items' - List of source server post migration custom actions.
--
-- 'nextToken', 'listSourceServerActionsResponse_nextToken' - Next token returned when listing source server post migration custom
-- actions.
--
-- 'httpStatus', 'listSourceServerActionsResponse_httpStatus' - The response's http status code.
newListSourceServerActionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListSourceServerActionsResponse
newListSourceServerActionsResponse pHttpStatus_ =
  ListSourceServerActionsResponse'
    { items =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | List of source server post migration custom actions.
listSourceServerActionsResponse_items :: Lens.Lens' ListSourceServerActionsResponse (Prelude.Maybe [SourceServerActionDocument])
listSourceServerActionsResponse_items = Lens.lens (\ListSourceServerActionsResponse' {items} -> items) (\s@ListSourceServerActionsResponse' {} a -> s {items = a} :: ListSourceServerActionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | Next token returned when listing source server post migration custom
-- actions.
listSourceServerActionsResponse_nextToken :: Lens.Lens' ListSourceServerActionsResponse (Prelude.Maybe Prelude.Text)
listSourceServerActionsResponse_nextToken = Lens.lens (\ListSourceServerActionsResponse' {nextToken} -> nextToken) (\s@ListSourceServerActionsResponse' {} a -> s {nextToken = a} :: ListSourceServerActionsResponse)

-- | The response's http status code.
listSourceServerActionsResponse_httpStatus :: Lens.Lens' ListSourceServerActionsResponse Prelude.Int
listSourceServerActionsResponse_httpStatus = Lens.lens (\ListSourceServerActionsResponse' {httpStatus} -> httpStatus) (\s@ListSourceServerActionsResponse' {} a -> s {httpStatus = a} :: ListSourceServerActionsResponse)

instance
  Prelude.NFData
    ListSourceServerActionsResponse
  where
  rnf ListSourceServerActionsResponse' {..} =
    Prelude.rnf items
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
