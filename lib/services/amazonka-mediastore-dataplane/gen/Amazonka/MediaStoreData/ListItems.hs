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
-- Module      : Amazonka.MediaStoreData.ListItems
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides a list of metadata entries about folders and objects in the
-- specified folder.
--
-- This operation returns paginated results.
module Amazonka.MediaStoreData.ListItems
  ( -- * Creating a Request
    ListItems (..),
    newListItems,

    -- * Request Lenses
    listItems_nextToken,
    listItems_path,
    listItems_maxResults,

    -- * Destructuring the Response
    ListItemsResponse (..),
    newListItemsResponse,

    -- * Response Lenses
    listItemsResponse_items,
    listItemsResponse_nextToken,
    listItemsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaStoreData.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListItems' smart constructor.
data ListItems = ListItems'
  { -- | The token that identifies which batch of results that you want to see.
    -- For example, you submit a @ListItems@ request with @MaxResults@ set at
    -- 500. The service returns the first batch of results (up to 500) and a
    -- @NextToken@ value. To see the next batch of results, you can submit the
    -- @ListItems@ request a second time and specify the @NextToken@ value.
    --
    -- Tokens expire after 15 minutes.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The path in the container from which to retrieve items. Format: \<folder
    -- name>\/\<folder name>\/\<file name>
    path :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to return per API request. For example,
    -- you submit a @ListItems@ request with @MaxResults@ set at 500. Although
    -- 2,000 items match your request, the service returns no more than the
    -- first 500 items. (The service also returns a @NextToken@ value that you
    -- can use to fetch the next batch of results.) The service might return
    -- fewer results than the @MaxResults@ value.
    --
    -- If @MaxResults@ is not included in the request, the service defaults to
    -- pagination with a maximum of 1,000 results per page.
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListItems' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listItems_nextToken' - The token that identifies which batch of results that you want to see.
-- For example, you submit a @ListItems@ request with @MaxResults@ set at
-- 500. The service returns the first batch of results (up to 500) and a
-- @NextToken@ value. To see the next batch of results, you can submit the
-- @ListItems@ request a second time and specify the @NextToken@ value.
--
-- Tokens expire after 15 minutes.
--
-- 'path', 'listItems_path' - The path in the container from which to retrieve items. Format: \<folder
-- name>\/\<folder name>\/\<file name>
--
-- 'maxResults', 'listItems_maxResults' - The maximum number of results to return per API request. For example,
-- you submit a @ListItems@ request with @MaxResults@ set at 500. Although
-- 2,000 items match your request, the service returns no more than the
-- first 500 items. (The service also returns a @NextToken@ value that you
-- can use to fetch the next batch of results.) The service might return
-- fewer results than the @MaxResults@ value.
--
-- If @MaxResults@ is not included in the request, the service defaults to
-- pagination with a maximum of 1,000 results per page.
newListItems ::
  ListItems
newListItems =
  ListItems'
    { nextToken = Prelude.Nothing,
      path = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | The token that identifies which batch of results that you want to see.
-- For example, you submit a @ListItems@ request with @MaxResults@ set at
-- 500. The service returns the first batch of results (up to 500) and a
-- @NextToken@ value. To see the next batch of results, you can submit the
-- @ListItems@ request a second time and specify the @NextToken@ value.
--
-- Tokens expire after 15 minutes.
listItems_nextToken :: Lens.Lens' ListItems (Prelude.Maybe Prelude.Text)
listItems_nextToken = Lens.lens (\ListItems' {nextToken} -> nextToken) (\s@ListItems' {} a -> s {nextToken = a} :: ListItems)

-- | The path in the container from which to retrieve items. Format: \<folder
-- name>\/\<folder name>\/\<file name>
listItems_path :: Lens.Lens' ListItems (Prelude.Maybe Prelude.Text)
listItems_path = Lens.lens (\ListItems' {path} -> path) (\s@ListItems' {} a -> s {path = a} :: ListItems)

-- | The maximum number of results to return per API request. For example,
-- you submit a @ListItems@ request with @MaxResults@ set at 500. Although
-- 2,000 items match your request, the service returns no more than the
-- first 500 items. (The service also returns a @NextToken@ value that you
-- can use to fetch the next batch of results.) The service might return
-- fewer results than the @MaxResults@ value.
--
-- If @MaxResults@ is not included in the request, the service defaults to
-- pagination with a maximum of 1,000 results per page.
listItems_maxResults :: Lens.Lens' ListItems (Prelude.Maybe Prelude.Natural)
listItems_maxResults = Lens.lens (\ListItems' {maxResults} -> maxResults) (\s@ListItems' {} a -> s {maxResults = a} :: ListItems)

instance Core.AWSPager ListItems where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listItemsResponse_nextToken Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listItemsResponse_items Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listItems_nextToken
          Lens..~ rs
          Lens.^? listItemsResponse_nextToken Prelude.. Lens._Just

instance Core.AWSRequest ListItems where
  type AWSResponse ListItems = ListItemsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListItemsResponse'
            Prelude.<$> (x Data..?> "Items" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListItems where
  hashWithSalt _salt ListItems' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` path
      `Prelude.hashWithSalt` maxResults

instance Prelude.NFData ListItems where
  rnf ListItems' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf path
      `Prelude.seq` Prelude.rnf maxResults

instance Data.ToHeaders ListItems where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ListItems where
  toPath = Prelude.const "/"

instance Data.ToQuery ListItems where
  toQuery ListItems' {..} =
    Prelude.mconcat
      [ "NextToken" Data.=: nextToken,
        "Path" Data.=: path,
        "MaxResults" Data.=: maxResults
      ]

-- | /See:/ 'newListItemsResponse' smart constructor.
data ListItemsResponse = ListItemsResponse'
  { -- | The metadata entries for the folders and objects at the requested path.
    items :: Prelude.Maybe [Item],
    -- | The token that can be used in a request to view the next set of results.
    -- For example, you submit a @ListItems@ request that matches 2,000 items
    -- with @MaxResults@ set at 500. The service returns the first batch of
    -- results (up to 500) and a @NextToken@ value that can be used to fetch
    -- the next batch of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListItemsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'items', 'listItemsResponse_items' - The metadata entries for the folders and objects at the requested path.
--
-- 'nextToken', 'listItemsResponse_nextToken' - The token that can be used in a request to view the next set of results.
-- For example, you submit a @ListItems@ request that matches 2,000 items
-- with @MaxResults@ set at 500. The service returns the first batch of
-- results (up to 500) and a @NextToken@ value that can be used to fetch
-- the next batch of results.
--
-- 'httpStatus', 'listItemsResponse_httpStatus' - The response's http status code.
newListItemsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListItemsResponse
newListItemsResponse pHttpStatus_ =
  ListItemsResponse'
    { items = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The metadata entries for the folders and objects at the requested path.
listItemsResponse_items :: Lens.Lens' ListItemsResponse (Prelude.Maybe [Item])
listItemsResponse_items = Lens.lens (\ListItemsResponse' {items} -> items) (\s@ListItemsResponse' {} a -> s {items = a} :: ListItemsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token that can be used in a request to view the next set of results.
-- For example, you submit a @ListItems@ request that matches 2,000 items
-- with @MaxResults@ set at 500. The service returns the first batch of
-- results (up to 500) and a @NextToken@ value that can be used to fetch
-- the next batch of results.
listItemsResponse_nextToken :: Lens.Lens' ListItemsResponse (Prelude.Maybe Prelude.Text)
listItemsResponse_nextToken = Lens.lens (\ListItemsResponse' {nextToken} -> nextToken) (\s@ListItemsResponse' {} a -> s {nextToken = a} :: ListItemsResponse)

-- | The response's http status code.
listItemsResponse_httpStatus :: Lens.Lens' ListItemsResponse Prelude.Int
listItemsResponse_httpStatus = Lens.lens (\ListItemsResponse' {httpStatus} -> httpStatus) (\s@ListItemsResponse' {} a -> s {httpStatus = a} :: ListItemsResponse)

instance Prelude.NFData ListItemsResponse where
  rnf ListItemsResponse' {..} =
    Prelude.rnf items
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
