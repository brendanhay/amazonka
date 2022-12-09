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
-- Module      : Amazonka.SageMaker.ListSpaces
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists spaces.
--
-- This operation returns paginated results.
module Amazonka.SageMaker.ListSpaces
  ( -- * Creating a Request
    ListSpaces (..),
    newListSpaces,

    -- * Request Lenses
    listSpaces_domainIdEquals,
    listSpaces_maxResults,
    listSpaces_nextToken,
    listSpaces_sortBy,
    listSpaces_sortOrder,
    listSpaces_spaceNameContains,

    -- * Destructuring the Response
    ListSpacesResponse (..),
    newListSpacesResponse,

    -- * Response Lenses
    listSpacesResponse_nextToken,
    listSpacesResponse_spaces,
    listSpacesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newListSpaces' smart constructor.
data ListSpaces = ListSpaces'
  { -- | A parameter to search for the Domain ID.
    domainIdEquals :: Prelude.Maybe Prelude.Text,
    -- | Returns a list up to a specified limit.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | If the previous response was truncated, you will receive this token. Use
    -- it in your next request to receive the next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The parameter by which to sort the results. The default is
    -- @CreationTime@.
    sortBy :: Prelude.Maybe SpaceSortKey,
    -- | The sort order for the results. The default is @Ascending@.
    sortOrder :: Prelude.Maybe SortOrder,
    -- | A parameter by which to filter the results.
    spaceNameContains :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListSpaces' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domainIdEquals', 'listSpaces_domainIdEquals' - A parameter to search for the Domain ID.
--
-- 'maxResults', 'listSpaces_maxResults' - Returns a list up to a specified limit.
--
-- 'nextToken', 'listSpaces_nextToken' - If the previous response was truncated, you will receive this token. Use
-- it in your next request to receive the next set of results.
--
-- 'sortBy', 'listSpaces_sortBy' - The parameter by which to sort the results. The default is
-- @CreationTime@.
--
-- 'sortOrder', 'listSpaces_sortOrder' - The sort order for the results. The default is @Ascending@.
--
-- 'spaceNameContains', 'listSpaces_spaceNameContains' - A parameter by which to filter the results.
newListSpaces ::
  ListSpaces
newListSpaces =
  ListSpaces'
    { domainIdEquals = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      sortBy = Prelude.Nothing,
      sortOrder = Prelude.Nothing,
      spaceNameContains = Prelude.Nothing
    }

-- | A parameter to search for the Domain ID.
listSpaces_domainIdEquals :: Lens.Lens' ListSpaces (Prelude.Maybe Prelude.Text)
listSpaces_domainIdEquals = Lens.lens (\ListSpaces' {domainIdEquals} -> domainIdEquals) (\s@ListSpaces' {} a -> s {domainIdEquals = a} :: ListSpaces)

-- | Returns a list up to a specified limit.
listSpaces_maxResults :: Lens.Lens' ListSpaces (Prelude.Maybe Prelude.Natural)
listSpaces_maxResults = Lens.lens (\ListSpaces' {maxResults} -> maxResults) (\s@ListSpaces' {} a -> s {maxResults = a} :: ListSpaces)

-- | If the previous response was truncated, you will receive this token. Use
-- it in your next request to receive the next set of results.
listSpaces_nextToken :: Lens.Lens' ListSpaces (Prelude.Maybe Prelude.Text)
listSpaces_nextToken = Lens.lens (\ListSpaces' {nextToken} -> nextToken) (\s@ListSpaces' {} a -> s {nextToken = a} :: ListSpaces)

-- | The parameter by which to sort the results. The default is
-- @CreationTime@.
listSpaces_sortBy :: Lens.Lens' ListSpaces (Prelude.Maybe SpaceSortKey)
listSpaces_sortBy = Lens.lens (\ListSpaces' {sortBy} -> sortBy) (\s@ListSpaces' {} a -> s {sortBy = a} :: ListSpaces)

-- | The sort order for the results. The default is @Ascending@.
listSpaces_sortOrder :: Lens.Lens' ListSpaces (Prelude.Maybe SortOrder)
listSpaces_sortOrder = Lens.lens (\ListSpaces' {sortOrder} -> sortOrder) (\s@ListSpaces' {} a -> s {sortOrder = a} :: ListSpaces)

-- | A parameter by which to filter the results.
listSpaces_spaceNameContains :: Lens.Lens' ListSpaces (Prelude.Maybe Prelude.Text)
listSpaces_spaceNameContains = Lens.lens (\ListSpaces' {spaceNameContains} -> spaceNameContains) (\s@ListSpaces' {} a -> s {spaceNameContains = a} :: ListSpaces)

instance Core.AWSPager ListSpaces where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listSpacesResponse_nextToken Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listSpacesResponse_spaces Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listSpaces_nextToken
          Lens..~ rs
          Lens.^? listSpacesResponse_nextToken Prelude.. Lens._Just

instance Core.AWSRequest ListSpaces where
  type AWSResponse ListSpaces = ListSpacesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListSpacesResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "Spaces" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListSpaces where
  hashWithSalt _salt ListSpaces' {..} =
    _salt `Prelude.hashWithSalt` domainIdEquals
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` sortBy
      `Prelude.hashWithSalt` sortOrder
      `Prelude.hashWithSalt` spaceNameContains

instance Prelude.NFData ListSpaces where
  rnf ListSpaces' {..} =
    Prelude.rnf domainIdEquals
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf sortBy
      `Prelude.seq` Prelude.rnf sortOrder
      `Prelude.seq` Prelude.rnf spaceNameContains

instance Data.ToHeaders ListSpaces where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("SageMaker.ListSpaces" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListSpaces where
  toJSON ListSpaces' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DomainIdEquals" Data..=)
              Prelude.<$> domainIdEquals,
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            ("SortBy" Data..=) Prelude.<$> sortBy,
            ("SortOrder" Data..=) Prelude.<$> sortOrder,
            ("SpaceNameContains" Data..=)
              Prelude.<$> spaceNameContains
          ]
      )

instance Data.ToPath ListSpaces where
  toPath = Prelude.const "/"

instance Data.ToQuery ListSpaces where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListSpacesResponse' smart constructor.
data ListSpacesResponse = ListSpacesResponse'
  { -- | If the previous response was truncated, you will receive this token. Use
    -- it in your next request to receive the next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The list of spaces.
    spaces :: Prelude.Maybe [SpaceDetails],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListSpacesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listSpacesResponse_nextToken' - If the previous response was truncated, you will receive this token. Use
-- it in your next request to receive the next set of results.
--
-- 'spaces', 'listSpacesResponse_spaces' - The list of spaces.
--
-- 'httpStatus', 'listSpacesResponse_httpStatus' - The response's http status code.
newListSpacesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListSpacesResponse
newListSpacesResponse pHttpStatus_ =
  ListSpacesResponse'
    { nextToken = Prelude.Nothing,
      spaces = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If the previous response was truncated, you will receive this token. Use
-- it in your next request to receive the next set of results.
listSpacesResponse_nextToken :: Lens.Lens' ListSpacesResponse (Prelude.Maybe Prelude.Text)
listSpacesResponse_nextToken = Lens.lens (\ListSpacesResponse' {nextToken} -> nextToken) (\s@ListSpacesResponse' {} a -> s {nextToken = a} :: ListSpacesResponse)

-- | The list of spaces.
listSpacesResponse_spaces :: Lens.Lens' ListSpacesResponse (Prelude.Maybe [SpaceDetails])
listSpacesResponse_spaces = Lens.lens (\ListSpacesResponse' {spaces} -> spaces) (\s@ListSpacesResponse' {} a -> s {spaces = a} :: ListSpacesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listSpacesResponse_httpStatus :: Lens.Lens' ListSpacesResponse Prelude.Int
listSpacesResponse_httpStatus = Lens.lens (\ListSpacesResponse' {httpStatus} -> httpStatus) (\s@ListSpacesResponse' {} a -> s {httpStatus = a} :: ListSpacesResponse)

instance Prelude.NFData ListSpacesResponse where
  rnf ListSpacesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf spaces
      `Prelude.seq` Prelude.rnf httpStatus
