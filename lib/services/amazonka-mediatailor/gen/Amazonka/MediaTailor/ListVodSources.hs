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
-- Module      : Amazonka.MediaTailor.ListVodSources
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the VOD sources contained in a source location. A source
-- represents a piece of content.
--
-- This operation returns paginated results.
module Amazonka.MediaTailor.ListVodSources
  ( -- * Creating a Request
    ListVodSources (..),
    newListVodSources,

    -- * Request Lenses
    listVodSources_maxResults,
    listVodSources_nextToken,
    listVodSources_sourceLocationName,

    -- * Destructuring the Response
    ListVodSourcesResponse (..),
    newListVodSourcesResponse,

    -- * Response Lenses
    listVodSourcesResponse_items,
    listVodSourcesResponse_nextToken,
    listVodSourcesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaTailor.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListVodSources' smart constructor.
data ListVodSources = ListVodSources'
  { -- | The maximum number of VOD sources that you want MediaTailor to return in
    -- response to the current request. If there are more than @MaxResults@ VOD
    -- sources, use the value of @NextToken@ in the response to get the next
    -- page of results.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | Pagination token returned by the list request when results exceed the
    -- maximum allowed. Use the token to fetch the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The name of the source location associated with this VOD Source list.
    sourceLocationName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListVodSources' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listVodSources_maxResults' - The maximum number of VOD sources that you want MediaTailor to return in
-- response to the current request. If there are more than @MaxResults@ VOD
-- sources, use the value of @NextToken@ in the response to get the next
-- page of results.
--
-- 'nextToken', 'listVodSources_nextToken' - Pagination token returned by the list request when results exceed the
-- maximum allowed. Use the token to fetch the next page of results.
--
-- 'sourceLocationName', 'listVodSources_sourceLocationName' - The name of the source location associated with this VOD Source list.
newListVodSources ::
  -- | 'sourceLocationName'
  Prelude.Text ->
  ListVodSources
newListVodSources pSourceLocationName_ =
  ListVodSources'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      sourceLocationName = pSourceLocationName_
    }

-- | The maximum number of VOD sources that you want MediaTailor to return in
-- response to the current request. If there are more than @MaxResults@ VOD
-- sources, use the value of @NextToken@ in the response to get the next
-- page of results.
listVodSources_maxResults :: Lens.Lens' ListVodSources (Prelude.Maybe Prelude.Natural)
listVodSources_maxResults = Lens.lens (\ListVodSources' {maxResults} -> maxResults) (\s@ListVodSources' {} a -> s {maxResults = a} :: ListVodSources)

-- | Pagination token returned by the list request when results exceed the
-- maximum allowed. Use the token to fetch the next page of results.
listVodSources_nextToken :: Lens.Lens' ListVodSources (Prelude.Maybe Prelude.Text)
listVodSources_nextToken = Lens.lens (\ListVodSources' {nextToken} -> nextToken) (\s@ListVodSources' {} a -> s {nextToken = a} :: ListVodSources)

-- | The name of the source location associated with this VOD Source list.
listVodSources_sourceLocationName :: Lens.Lens' ListVodSources Prelude.Text
listVodSources_sourceLocationName = Lens.lens (\ListVodSources' {sourceLocationName} -> sourceLocationName) (\s@ListVodSources' {} a -> s {sourceLocationName = a} :: ListVodSources)

instance Core.AWSPager ListVodSources where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listVodSourcesResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listVodSourcesResponse_items
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listVodSources_nextToken
          Lens..~ rs
          Lens.^? listVodSourcesResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListVodSources where
  type
    AWSResponse ListVodSources =
      ListVodSourcesResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListVodSourcesResponse'
            Prelude.<$> (x Data..?> "Items" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListVodSources where
  hashWithSalt _salt ListVodSources' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` sourceLocationName

instance Prelude.NFData ListVodSources where
  rnf ListVodSources' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf sourceLocationName

instance Data.ToHeaders ListVodSources where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListVodSources where
  toPath ListVodSources' {..} =
    Prelude.mconcat
      [ "/sourceLocation/",
        Data.toBS sourceLocationName,
        "/vodSources"
      ]

instance Data.ToQuery ListVodSources where
  toQuery ListVodSources' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken
      ]

-- | /See:/ 'newListVodSourcesResponse' smart constructor.
data ListVodSourcesResponse = ListVodSourcesResponse'
  { -- | Lists the VOD sources.
    items :: Prelude.Maybe [VodSource],
    -- | Pagination token returned by the list request when results exceed the
    -- maximum allowed. Use the token to fetch the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListVodSourcesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'items', 'listVodSourcesResponse_items' - Lists the VOD sources.
--
-- 'nextToken', 'listVodSourcesResponse_nextToken' - Pagination token returned by the list request when results exceed the
-- maximum allowed. Use the token to fetch the next page of results.
--
-- 'httpStatus', 'listVodSourcesResponse_httpStatus' - The response's http status code.
newListVodSourcesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListVodSourcesResponse
newListVodSourcesResponse pHttpStatus_ =
  ListVodSourcesResponse'
    { items = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Lists the VOD sources.
listVodSourcesResponse_items :: Lens.Lens' ListVodSourcesResponse (Prelude.Maybe [VodSource])
listVodSourcesResponse_items = Lens.lens (\ListVodSourcesResponse' {items} -> items) (\s@ListVodSourcesResponse' {} a -> s {items = a} :: ListVodSourcesResponse) Prelude.. Lens.mapping Lens.coerced

-- | Pagination token returned by the list request when results exceed the
-- maximum allowed. Use the token to fetch the next page of results.
listVodSourcesResponse_nextToken :: Lens.Lens' ListVodSourcesResponse (Prelude.Maybe Prelude.Text)
listVodSourcesResponse_nextToken = Lens.lens (\ListVodSourcesResponse' {nextToken} -> nextToken) (\s@ListVodSourcesResponse' {} a -> s {nextToken = a} :: ListVodSourcesResponse)

-- | The response's http status code.
listVodSourcesResponse_httpStatus :: Lens.Lens' ListVodSourcesResponse Prelude.Int
listVodSourcesResponse_httpStatus = Lens.lens (\ListVodSourcesResponse' {httpStatus} -> httpStatus) (\s@ListVodSourcesResponse' {} a -> s {httpStatus = a} :: ListVodSourcesResponse)

instance Prelude.NFData ListVodSourcesResponse where
  rnf ListVodSourcesResponse' {..} =
    Prelude.rnf items
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
