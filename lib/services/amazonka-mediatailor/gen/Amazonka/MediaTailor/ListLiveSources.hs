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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- lists all the live sources in a source location.
--
-- This operation returns paginated results.
module Amazonka.MediaTailor.ListLiveSources
  ( -- * Creating a Request
    ListLiveSources (..),
    newListLiveSources,

    -- * Request Lenses
    listLiveSources_nextToken,
    listLiveSources_maxResults,
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
import qualified Amazonka.Lens as Lens
import Amazonka.MediaTailor.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListLiveSources' smart constructor.
data ListLiveSources = ListLiveSources'
  { -- | Pagination token from the GET list request. Use the token to fetch the
    -- next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Upper bound on number of records to return. The maximum number of
    -- results is 100.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The identifier for the source location you are working on.
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
-- 'nextToken', 'listLiveSources_nextToken' - Pagination token from the GET list request. Use the token to fetch the
-- next page of results.
--
-- 'maxResults', 'listLiveSources_maxResults' - Upper bound on number of records to return. The maximum number of
-- results is 100.
--
-- 'sourceLocationName', 'listLiveSources_sourceLocationName' - The identifier for the source location you are working on.
newListLiveSources ::
  -- | 'sourceLocationName'
  Prelude.Text ->
  ListLiveSources
newListLiveSources pSourceLocationName_ =
  ListLiveSources'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      sourceLocationName = pSourceLocationName_
    }

-- | Pagination token from the GET list request. Use the token to fetch the
-- next page of results.
listLiveSources_nextToken :: Lens.Lens' ListLiveSources (Prelude.Maybe Prelude.Text)
listLiveSources_nextToken = Lens.lens (\ListLiveSources' {nextToken} -> nextToken) (\s@ListLiveSources' {} a -> s {nextToken = a} :: ListLiveSources)

-- | Upper bound on number of records to return. The maximum number of
-- results is 100.
listLiveSources_maxResults :: Lens.Lens' ListLiveSources (Prelude.Maybe Prelude.Natural)
listLiveSources_maxResults = Lens.lens (\ListLiveSources' {maxResults} -> maxResults) (\s@ListLiveSources' {} a -> s {maxResults = a} :: ListLiveSources)

-- | The identifier for the source location you are working on.
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
            Lens.^? listLiveSourcesResponse_items Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listLiveSources_nextToken
          Lens..~ rs
          Lens.^? listLiveSourcesResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListLiveSources where
  type
    AWSResponse ListLiveSources =
      ListLiveSourcesResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListLiveSourcesResponse'
            Prelude.<$> (x Core..?> "Items" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListLiveSources where
  hashWithSalt _salt ListLiveSources' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` sourceLocationName

instance Prelude.NFData ListLiveSources where
  rnf ListLiveSources' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf sourceLocationName

instance Core.ToHeaders ListLiveSources where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath ListLiveSources where
  toPath ListLiveSources' {..} =
    Prelude.mconcat
      [ "/sourceLocation/",
        Core.toBS sourceLocationName,
        "/liveSources"
      ]

instance Core.ToQuery ListLiveSources where
  toQuery ListLiveSources' {..} =
    Prelude.mconcat
      [ "nextToken" Core.=: nextToken,
        "maxResults" Core.=: maxResults
      ]

-- | /See:/ 'newListLiveSourcesResponse' smart constructor.
data ListLiveSourcesResponse = ListLiveSourcesResponse'
  { -- | Lists the live sources.
    items :: Prelude.Maybe [LiveSource],
    -- | Pagination token from the list request. Use the token to fetch the next
    -- page of results.
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
-- 'nextToken', 'listLiveSourcesResponse_nextToken' - Pagination token from the list request. Use the token to fetch the next
-- page of results.
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

-- | Pagination token from the list request. Use the token to fetch the next
-- page of results.
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
