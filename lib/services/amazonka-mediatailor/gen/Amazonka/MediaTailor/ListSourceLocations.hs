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
-- Module      : Amazonka.MediaTailor.ListSourceLocations
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the source locations for a channel. A source location defines the
-- host server URL, and contains a list of sources.
--
-- This operation returns paginated results.
module Amazonka.MediaTailor.ListSourceLocations
  ( -- * Creating a Request
    ListSourceLocations (..),
    newListSourceLocations,

    -- * Request Lenses
    listSourceLocations_maxResults,
    listSourceLocations_nextToken,

    -- * Destructuring the Response
    ListSourceLocationsResponse (..),
    newListSourceLocationsResponse,

    -- * Response Lenses
    listSourceLocationsResponse_items,
    listSourceLocationsResponse_nextToken,
    listSourceLocationsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaTailor.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListSourceLocations' smart constructor.
data ListSourceLocations = ListSourceLocations'
  { -- | The maximum number of source locations that you want MediaTailor to
    -- return in response to the current request. If there are more than
    -- @MaxResults@ source locations, use the value of @NextToken@ in the
    -- response to get the next page of results.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | Pagination token returned by the list request when results exceed the
    -- maximum allowed. Use the token to fetch the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListSourceLocations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listSourceLocations_maxResults' - The maximum number of source locations that you want MediaTailor to
-- return in response to the current request. If there are more than
-- @MaxResults@ source locations, use the value of @NextToken@ in the
-- response to get the next page of results.
--
-- 'nextToken', 'listSourceLocations_nextToken' - Pagination token returned by the list request when results exceed the
-- maximum allowed. Use the token to fetch the next page of results.
newListSourceLocations ::
  ListSourceLocations
newListSourceLocations =
  ListSourceLocations'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | The maximum number of source locations that you want MediaTailor to
-- return in response to the current request. If there are more than
-- @MaxResults@ source locations, use the value of @NextToken@ in the
-- response to get the next page of results.
listSourceLocations_maxResults :: Lens.Lens' ListSourceLocations (Prelude.Maybe Prelude.Natural)
listSourceLocations_maxResults = Lens.lens (\ListSourceLocations' {maxResults} -> maxResults) (\s@ListSourceLocations' {} a -> s {maxResults = a} :: ListSourceLocations)

-- | Pagination token returned by the list request when results exceed the
-- maximum allowed. Use the token to fetch the next page of results.
listSourceLocations_nextToken :: Lens.Lens' ListSourceLocations (Prelude.Maybe Prelude.Text)
listSourceLocations_nextToken = Lens.lens (\ListSourceLocations' {nextToken} -> nextToken) (\s@ListSourceLocations' {} a -> s {nextToken = a} :: ListSourceLocations)

instance Core.AWSPager ListSourceLocations where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listSourceLocationsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listSourceLocationsResponse_items
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listSourceLocations_nextToken
          Lens..~ rs
          Lens.^? listSourceLocationsResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListSourceLocations where
  type
    AWSResponse ListSourceLocations =
      ListSourceLocationsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListSourceLocationsResponse'
            Prelude.<$> (x Data..?> "Items" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListSourceLocations where
  hashWithSalt _salt ListSourceLocations' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListSourceLocations where
  rnf ListSourceLocations' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders ListSourceLocations where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListSourceLocations where
  toPath = Prelude.const "/sourceLocations"

instance Data.ToQuery ListSourceLocations where
  toQuery ListSourceLocations' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken
      ]

-- | /See:/ 'newListSourceLocationsResponse' smart constructor.
data ListSourceLocationsResponse = ListSourceLocationsResponse'
  { -- | A list of source locations.
    items :: Prelude.Maybe [SourceLocation],
    -- | Pagination token returned by the list request when results exceed the
    -- maximum allowed. Use the token to fetch the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListSourceLocationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'items', 'listSourceLocationsResponse_items' - A list of source locations.
--
-- 'nextToken', 'listSourceLocationsResponse_nextToken' - Pagination token returned by the list request when results exceed the
-- maximum allowed. Use the token to fetch the next page of results.
--
-- 'httpStatus', 'listSourceLocationsResponse_httpStatus' - The response's http status code.
newListSourceLocationsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListSourceLocationsResponse
newListSourceLocationsResponse pHttpStatus_ =
  ListSourceLocationsResponse'
    { items =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of source locations.
listSourceLocationsResponse_items :: Lens.Lens' ListSourceLocationsResponse (Prelude.Maybe [SourceLocation])
listSourceLocationsResponse_items = Lens.lens (\ListSourceLocationsResponse' {items} -> items) (\s@ListSourceLocationsResponse' {} a -> s {items = a} :: ListSourceLocationsResponse) Prelude.. Lens.mapping Lens.coerced

-- | Pagination token returned by the list request when results exceed the
-- maximum allowed. Use the token to fetch the next page of results.
listSourceLocationsResponse_nextToken :: Lens.Lens' ListSourceLocationsResponse (Prelude.Maybe Prelude.Text)
listSourceLocationsResponse_nextToken = Lens.lens (\ListSourceLocationsResponse' {nextToken} -> nextToken) (\s@ListSourceLocationsResponse' {} a -> s {nextToken = a} :: ListSourceLocationsResponse)

-- | The response's http status code.
listSourceLocationsResponse_httpStatus :: Lens.Lens' ListSourceLocationsResponse Prelude.Int
listSourceLocationsResponse_httpStatus = Lens.lens (\ListSourceLocationsResponse' {httpStatus} -> httpStatus) (\s@ListSourceLocationsResponse' {} a -> s {httpStatus = a} :: ListSourceLocationsResponse)

instance Prelude.NFData ListSourceLocationsResponse where
  rnf ListSourceLocationsResponse' {..} =
    Prelude.rnf items
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
