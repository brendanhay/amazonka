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
-- Module      : Amazonka.MediaTailor.ListLiveSources
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the live sources contained in a source location. A source
-- represents a piece of content.
--
-- This operation returns paginated results.
module Amazonka.MediaTailor.ListLiveSources
  ( -- * Creating a Request
    ListLiveSources (..),
    newListLiveSources,

    -- * Request Lenses
    listLiveSources_maxResults,
    listLiveSources_nextToken,
    listLiveSources_sourceLocationName,

    -- * Destructuring the Response
    ListLiveSourcesResponse (..),
    newListLiveSourcesResponse,

    -- * Response Lenses
    listLiveSourcesResponse_items,
    listLiveSourcesResponse_nextToken,
    listLiveSourcesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaTailor.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListLiveSources' smart constructor.
data ListLiveSources = ListLiveSources'
  { -- | The maximum number of live sources that you want MediaTailor to return
    -- in response to the current request. If there are more than @MaxResults@
    -- live sources, use the value of @NextToken@ in the response to get the
    -- next page of results.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | Pagination token returned by the list request when results exceed the
    -- maximum allowed. Use the token to fetch the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The name of the source location associated with this Live Sources list.
    sourceLocationName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListLiveSources' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listLiveSources_maxResults' - The maximum number of live sources that you want MediaTailor to return
-- in response to the current request. If there are more than @MaxResults@
-- live sources, use the value of @NextToken@ in the response to get the
-- next page of results.
--
-- 'nextToken', 'listLiveSources_nextToken' - Pagination token returned by the list request when results exceed the
-- maximum allowed. Use the token to fetch the next page of results.
--
-- 'sourceLocationName', 'listLiveSources_sourceLocationName' - The name of the source location associated with this Live Sources list.
newListLiveSources ::
  -- | 'sourceLocationName'
  Prelude.Text ->
  ListLiveSources
newListLiveSources pSourceLocationName_ =
  ListLiveSources'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      sourceLocationName = pSourceLocationName_
    }

-- | The maximum number of live sources that you want MediaTailor to return
-- in response to the current request. If there are more than @MaxResults@
-- live sources, use the value of @NextToken@ in the response to get the
-- next page of results.
listLiveSources_maxResults :: Lens.Lens' ListLiveSources (Prelude.Maybe Prelude.Natural)
listLiveSources_maxResults = Lens.lens (\ListLiveSources' {maxResults} -> maxResults) (\s@ListLiveSources' {} a -> s {maxResults = a} :: ListLiveSources)

-- | Pagination token returned by the list request when results exceed the
-- maximum allowed. Use the token to fetch the next page of results.
listLiveSources_nextToken :: Lens.Lens' ListLiveSources (Prelude.Maybe Prelude.Text)
listLiveSources_nextToken = Lens.lens (\ListLiveSources' {nextToken} -> nextToken) (\s@ListLiveSources' {} a -> s {nextToken = a} :: ListLiveSources)

-- | The name of the source location associated with this Live Sources list.
listLiveSources_sourceLocationName :: Lens.Lens' ListLiveSources Prelude.Text
listLiveSources_sourceLocationName = Lens.lens (\ListLiveSources' {sourceLocationName} -> sourceLocationName) (\s@ListLiveSources' {} a -> s {sourceLocationName = a} :: ListLiveSources)

instance Core.AWSPager ListLiveSources where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listLiveSourcesResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listLiveSourcesResponse_items
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listLiveSources_nextToken
          Lens..~ rs
          Lens.^? listLiveSourcesResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListLiveSources where
  type
    AWSResponse ListLiveSources =
      ListLiveSourcesResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListLiveSourcesResponse'
            Prelude.<$> (x Data..?> "Items" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListLiveSources where
  hashWithSalt _salt ListLiveSources' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` sourceLocationName

instance Prelude.NFData ListLiveSources where
  rnf ListLiveSources' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf sourceLocationName

instance Data.ToHeaders ListLiveSources where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListLiveSources where
  toPath ListLiveSources' {..} =
    Prelude.mconcat
      [ "/sourceLocation/",
        Data.toBS sourceLocationName,
        "/liveSources"
      ]

instance Data.ToQuery ListLiveSources where
  toQuery ListLiveSources' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken
      ]

-- | /See:/ 'newListLiveSourcesResponse' smart constructor.
data ListLiveSourcesResponse = ListLiveSourcesResponse'
  { -- | Lists the live sources.
    items :: Prelude.Maybe [LiveSource],
    -- | Pagination token returned by the list request when results exceed the
    -- maximum allowed. Use the token to fetch the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListLiveSourcesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'items', 'listLiveSourcesResponse_items' - Lists the live sources.
--
-- 'nextToken', 'listLiveSourcesResponse_nextToken' - Pagination token returned by the list request when results exceed the
-- maximum allowed. Use the token to fetch the next page of results.
--
-- 'httpStatus', 'listLiveSourcesResponse_httpStatus' - The response's http status code.
newListLiveSourcesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListLiveSourcesResponse
newListLiveSourcesResponse pHttpStatus_ =
  ListLiveSourcesResponse'
    { items = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Lists the live sources.
listLiveSourcesResponse_items :: Lens.Lens' ListLiveSourcesResponse (Prelude.Maybe [LiveSource])
listLiveSourcesResponse_items = Lens.lens (\ListLiveSourcesResponse' {items} -> items) (\s@ListLiveSourcesResponse' {} a -> s {items = a} :: ListLiveSourcesResponse) Prelude.. Lens.mapping Lens.coerced

-- | Pagination token returned by the list request when results exceed the
-- maximum allowed. Use the token to fetch the next page of results.
listLiveSourcesResponse_nextToken :: Lens.Lens' ListLiveSourcesResponse (Prelude.Maybe Prelude.Text)
listLiveSourcesResponse_nextToken = Lens.lens (\ListLiveSourcesResponse' {nextToken} -> nextToken) (\s@ListLiveSourcesResponse' {} a -> s {nextToken = a} :: ListLiveSourcesResponse)

-- | The response's http status code.
listLiveSourcesResponse_httpStatus :: Lens.Lens' ListLiveSourcesResponse Prelude.Int
listLiveSourcesResponse_httpStatus = Lens.lens (\ListLiveSourcesResponse' {httpStatus} -> httpStatus) (\s@ListLiveSourcesResponse' {} a -> s {httpStatus = a} :: ListLiveSourcesResponse)

instance Prelude.NFData ListLiveSourcesResponse where
  rnf ListLiveSourcesResponse' {..} =
    Prelude.rnf items
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
