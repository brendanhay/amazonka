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
-- Module      : Amazonka.Location.ListPlaceIndexes
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists place index resources in your AWS account.
--
-- This operation returns paginated results.
module Amazonka.Location.ListPlaceIndexes
  ( -- * Creating a Request
    ListPlaceIndexes (..),
    newListPlaceIndexes,

    -- * Request Lenses
    listPlaceIndexes_maxResults,
    listPlaceIndexes_nextToken,

    -- * Destructuring the Response
    ListPlaceIndexesResponse (..),
    newListPlaceIndexesResponse,

    -- * Response Lenses
    listPlaceIndexesResponse_nextToken,
    listPlaceIndexesResponse_httpStatus,
    listPlaceIndexesResponse_entries,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Location.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListPlaceIndexes' smart constructor.
data ListPlaceIndexes = ListPlaceIndexes'
  { -- | An optional limit for the maximum number of results returned in a single
    -- call.
    --
    -- Default value: @100@
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The pagination token specifying which page of results to return in the
    -- response. If no token is provided, the default page is the first page.
    --
    -- Default value: @null@
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListPlaceIndexes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listPlaceIndexes_maxResults' - An optional limit for the maximum number of results returned in a single
-- call.
--
-- Default value: @100@
--
-- 'nextToken', 'listPlaceIndexes_nextToken' - The pagination token specifying which page of results to return in the
-- response. If no token is provided, the default page is the first page.
--
-- Default value: @null@
newListPlaceIndexes ::
  ListPlaceIndexes
newListPlaceIndexes =
  ListPlaceIndexes'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | An optional limit for the maximum number of results returned in a single
-- call.
--
-- Default value: @100@
listPlaceIndexes_maxResults :: Lens.Lens' ListPlaceIndexes (Prelude.Maybe Prelude.Natural)
listPlaceIndexes_maxResults = Lens.lens (\ListPlaceIndexes' {maxResults} -> maxResults) (\s@ListPlaceIndexes' {} a -> s {maxResults = a} :: ListPlaceIndexes)

-- | The pagination token specifying which page of results to return in the
-- response. If no token is provided, the default page is the first page.
--
-- Default value: @null@
listPlaceIndexes_nextToken :: Lens.Lens' ListPlaceIndexes (Prelude.Maybe Prelude.Text)
listPlaceIndexes_nextToken = Lens.lens (\ListPlaceIndexes' {nextToken} -> nextToken) (\s@ListPlaceIndexes' {} a -> s {nextToken = a} :: ListPlaceIndexes)

instance Core.AWSPager ListPlaceIndexes where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listPlaceIndexesResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        (rs Lens.^. listPlaceIndexesResponse_entries) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listPlaceIndexes_nextToken
          Lens..~ rs
          Lens.^? listPlaceIndexesResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListPlaceIndexes where
  type
    AWSResponse ListPlaceIndexes =
      ListPlaceIndexesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListPlaceIndexesResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..?> "Entries" Core..!@ Prelude.mempty)
      )

instance Prelude.Hashable ListPlaceIndexes where
  hashWithSalt _salt ListPlaceIndexes' {..} =
    _salt `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListPlaceIndexes where
  rnf ListPlaceIndexes' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders ListPlaceIndexes where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListPlaceIndexes where
  toJSON ListPlaceIndexes' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken
          ]
      )

instance Data.ToPath ListPlaceIndexes where
  toPath = Prelude.const "/places/v0/list-indexes"

instance Data.ToQuery ListPlaceIndexes where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListPlaceIndexesResponse' smart constructor.
data ListPlaceIndexesResponse = ListPlaceIndexesResponse'
  { -- | A pagination token indicating that there are additional pages available.
    -- You can use the token in a new request to fetch the next page of
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Lists the place index resources that exist in your AWS account
    entries :: [ListPlaceIndexesResponseEntry]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListPlaceIndexesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listPlaceIndexesResponse_nextToken' - A pagination token indicating that there are additional pages available.
-- You can use the token in a new request to fetch the next page of
-- results.
--
-- 'httpStatus', 'listPlaceIndexesResponse_httpStatus' - The response's http status code.
--
-- 'entries', 'listPlaceIndexesResponse_entries' - Lists the place index resources that exist in your AWS account
newListPlaceIndexesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListPlaceIndexesResponse
newListPlaceIndexesResponse pHttpStatus_ =
  ListPlaceIndexesResponse'
    { nextToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      entries = Prelude.mempty
    }

-- | A pagination token indicating that there are additional pages available.
-- You can use the token in a new request to fetch the next page of
-- results.
listPlaceIndexesResponse_nextToken :: Lens.Lens' ListPlaceIndexesResponse (Prelude.Maybe Prelude.Text)
listPlaceIndexesResponse_nextToken = Lens.lens (\ListPlaceIndexesResponse' {nextToken} -> nextToken) (\s@ListPlaceIndexesResponse' {} a -> s {nextToken = a} :: ListPlaceIndexesResponse)

-- | The response's http status code.
listPlaceIndexesResponse_httpStatus :: Lens.Lens' ListPlaceIndexesResponse Prelude.Int
listPlaceIndexesResponse_httpStatus = Lens.lens (\ListPlaceIndexesResponse' {httpStatus} -> httpStatus) (\s@ListPlaceIndexesResponse' {} a -> s {httpStatus = a} :: ListPlaceIndexesResponse)

-- | Lists the place index resources that exist in your AWS account
listPlaceIndexesResponse_entries :: Lens.Lens' ListPlaceIndexesResponse [ListPlaceIndexesResponseEntry]
listPlaceIndexesResponse_entries = Lens.lens (\ListPlaceIndexesResponse' {entries} -> entries) (\s@ListPlaceIndexesResponse' {} a -> s {entries = a} :: ListPlaceIndexesResponse) Prelude.. Lens.coerced

instance Prelude.NFData ListPlaceIndexesResponse where
  rnf ListPlaceIndexesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf entries
