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
-- Module      : Amazonka.OpenSearch.ListVersions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all versions of OpenSearch and Elasticsearch that Amazon
-- OpenSearch Service supports.
module Amazonka.OpenSearch.ListVersions
  ( -- * Creating a Request
    ListVersions (..),
    newListVersions,

    -- * Request Lenses
    listVersions_maxResults,
    listVersions_nextToken,

    -- * Destructuring the Response
    ListVersionsResponse (..),
    newListVersionsResponse,

    -- * Response Lenses
    listVersionsResponse_nextToken,
    listVersionsResponse_versions,
    listVersionsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.OpenSearch.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Container for the request parameters to the @ListVersions@ operation.
--
-- /See:/ 'newListVersions' smart constructor.
data ListVersions = ListVersions'
  { -- | An optional parameter that specifies the maximum number of results to
    -- return. You can use @nextToken@ to get the next page of results.
    maxResults :: Prelude.Maybe Prelude.Int,
    -- | If your initial @ListVersions@ operation returns a @nextToken@, you can
    -- include the returned @nextToken@ in subsequent @ListVersions@
    -- operations, which returns results in the next page.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListVersions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listVersions_maxResults' - An optional parameter that specifies the maximum number of results to
-- return. You can use @nextToken@ to get the next page of results.
--
-- 'nextToken', 'listVersions_nextToken' - If your initial @ListVersions@ operation returns a @nextToken@, you can
-- include the returned @nextToken@ in subsequent @ListVersions@
-- operations, which returns results in the next page.
newListVersions ::
  ListVersions
newListVersions =
  ListVersions'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | An optional parameter that specifies the maximum number of results to
-- return. You can use @nextToken@ to get the next page of results.
listVersions_maxResults :: Lens.Lens' ListVersions (Prelude.Maybe Prelude.Int)
listVersions_maxResults = Lens.lens (\ListVersions' {maxResults} -> maxResults) (\s@ListVersions' {} a -> s {maxResults = a} :: ListVersions)

-- | If your initial @ListVersions@ operation returns a @nextToken@, you can
-- include the returned @nextToken@ in subsequent @ListVersions@
-- operations, which returns results in the next page.
listVersions_nextToken :: Lens.Lens' ListVersions (Prelude.Maybe Prelude.Text)
listVersions_nextToken = Lens.lens (\ListVersions' {nextToken} -> nextToken) (\s@ListVersions' {} a -> s {nextToken = a} :: ListVersions)

instance Core.AWSRequest ListVersions where
  type AWSResponse ListVersions = ListVersionsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListVersionsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "Versions" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListVersions where
  hashWithSalt _salt ListVersions' {..} =
    _salt `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListVersions where
  rnf ListVersions' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders ListVersions where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ListVersions where
  toPath =
    Prelude.const "/2021-01-01/opensearch/versions"

instance Data.ToQuery ListVersions where
  toQuery ListVersions' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken
      ]

-- | Container for the parameters for response received from the
-- @ListVersions@ operation.
--
-- /See:/ 'newListVersionsResponse' smart constructor.
data ListVersionsResponse = ListVersionsResponse'
  { -- | When @nextToken@ is returned, there are more results available. The
    -- value of @nextToken@ is a unique pagination token for each page. Make
    -- the call again using the returned token to retrieve the next page.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of all versions of OpenSearch and Elasticsearch that Amazon
    -- OpenSearch Service supports.
    versions :: Prelude.Maybe [Prelude.Text],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListVersionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listVersionsResponse_nextToken' - When @nextToken@ is returned, there are more results available. The
-- value of @nextToken@ is a unique pagination token for each page. Make
-- the call again using the returned token to retrieve the next page.
--
-- 'versions', 'listVersionsResponse_versions' - A list of all versions of OpenSearch and Elasticsearch that Amazon
-- OpenSearch Service supports.
--
-- 'httpStatus', 'listVersionsResponse_httpStatus' - The response's http status code.
newListVersionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListVersionsResponse
newListVersionsResponse pHttpStatus_ =
  ListVersionsResponse'
    { nextToken = Prelude.Nothing,
      versions = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | When @nextToken@ is returned, there are more results available. The
-- value of @nextToken@ is a unique pagination token for each page. Make
-- the call again using the returned token to retrieve the next page.
listVersionsResponse_nextToken :: Lens.Lens' ListVersionsResponse (Prelude.Maybe Prelude.Text)
listVersionsResponse_nextToken = Lens.lens (\ListVersionsResponse' {nextToken} -> nextToken) (\s@ListVersionsResponse' {} a -> s {nextToken = a} :: ListVersionsResponse)

-- | A list of all versions of OpenSearch and Elasticsearch that Amazon
-- OpenSearch Service supports.
listVersionsResponse_versions :: Lens.Lens' ListVersionsResponse (Prelude.Maybe [Prelude.Text])
listVersionsResponse_versions = Lens.lens (\ListVersionsResponse' {versions} -> versions) (\s@ListVersionsResponse' {} a -> s {versions = a} :: ListVersionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listVersionsResponse_httpStatus :: Lens.Lens' ListVersionsResponse Prelude.Int
listVersionsResponse_httpStatus = Lens.lens (\ListVersionsResponse' {httpStatus} -> httpStatus) (\s@ListVersionsResponse' {} a -> s {httpStatus = a} :: ListVersionsResponse)

instance Prelude.NFData ListVersionsResponse where
  rnf ListVersionsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf versions
      `Prelude.seq` Prelude.rnf httpStatus
