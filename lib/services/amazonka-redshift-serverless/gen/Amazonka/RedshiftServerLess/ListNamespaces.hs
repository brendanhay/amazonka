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
-- Module      : Amazonka.RedshiftServerLess.ListNamespaces
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a list of specified namespaces.
--
-- This operation returns paginated results.
module Amazonka.RedshiftServerLess.ListNamespaces
  ( -- * Creating a Request
    ListNamespaces (..),
    newListNamespaces,

    -- * Request Lenses
    listNamespaces_nextToken,
    listNamespaces_maxResults,

    -- * Destructuring the Response
    ListNamespacesResponse (..),
    newListNamespacesResponse,

    -- * Response Lenses
    listNamespacesResponse_nextToken,
    listNamespacesResponse_httpStatus,
    listNamespacesResponse_namespaces,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.RedshiftServerLess.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListNamespaces' smart constructor.
data ListNamespaces = ListNamespaces'
  { -- | If your initial @ListNamespaces@ operation returns a @nextToken@, you
    -- can include the returned @nextToken@ in subsequent @ListNamespaces@
    -- operations, which returns results in the next page.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | An optional parameter that specifies the maximum number of results to
    -- return. You can use @nextToken@ to get the next page of results.
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListNamespaces' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listNamespaces_nextToken' - If your initial @ListNamespaces@ operation returns a @nextToken@, you
-- can include the returned @nextToken@ in subsequent @ListNamespaces@
-- operations, which returns results in the next page.
--
-- 'maxResults', 'listNamespaces_maxResults' - An optional parameter that specifies the maximum number of results to
-- return. You can use @nextToken@ to get the next page of results.
newListNamespaces ::
  ListNamespaces
newListNamespaces =
  ListNamespaces'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | If your initial @ListNamespaces@ operation returns a @nextToken@, you
-- can include the returned @nextToken@ in subsequent @ListNamespaces@
-- operations, which returns results in the next page.
listNamespaces_nextToken :: Lens.Lens' ListNamespaces (Prelude.Maybe Prelude.Text)
listNamespaces_nextToken = Lens.lens (\ListNamespaces' {nextToken} -> nextToken) (\s@ListNamespaces' {} a -> s {nextToken = a} :: ListNamespaces)

-- | An optional parameter that specifies the maximum number of results to
-- return. You can use @nextToken@ to get the next page of results.
listNamespaces_maxResults :: Lens.Lens' ListNamespaces (Prelude.Maybe Prelude.Natural)
listNamespaces_maxResults = Lens.lens (\ListNamespaces' {maxResults} -> maxResults) (\s@ListNamespaces' {} a -> s {maxResults = a} :: ListNamespaces)

instance Core.AWSPager ListNamespaces where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listNamespacesResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        (rs Lens.^. listNamespacesResponse_namespaces) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listNamespaces_nextToken
          Lens..~ rs
          Lens.^? listNamespacesResponse_nextToken Prelude.. Lens._Just

instance Core.AWSRequest ListNamespaces where
  type
    AWSResponse ListNamespaces =
      ListNamespacesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListNamespacesResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..?> "namespaces" Core..!@ Prelude.mempty)
      )

instance Prelude.Hashable ListNamespaces where
  hashWithSalt _salt ListNamespaces' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults

instance Prelude.NFData ListNamespaces where
  rnf ListNamespaces' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults

instance Data.ToHeaders ListNamespaces where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "RedshiftServerless.ListNamespaces" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListNamespaces where
  toJSON ListNamespaces' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("nextToken" Data..=) Prelude.<$> nextToken,
            ("maxResults" Data..=) Prelude.<$> maxResults
          ]
      )

instance Data.ToPath ListNamespaces where
  toPath = Prelude.const "/"

instance Data.ToQuery ListNamespaces where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListNamespacesResponse' smart constructor.
data ListNamespacesResponse = ListNamespacesResponse'
  { -- | When @nextToken@ is returned, there are more results available. The
    -- value of @nextToken@ is a unique pagination token for each page. Make
    -- the call again using the returned token to retrieve the next page.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The list of returned namespaces.
    namespaces :: [Namespace]
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListNamespacesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listNamespacesResponse_nextToken' - When @nextToken@ is returned, there are more results available. The
-- value of @nextToken@ is a unique pagination token for each page. Make
-- the call again using the returned token to retrieve the next page.
--
-- 'httpStatus', 'listNamespacesResponse_httpStatus' - The response's http status code.
--
-- 'namespaces', 'listNamespacesResponse_namespaces' - The list of returned namespaces.
newListNamespacesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListNamespacesResponse
newListNamespacesResponse pHttpStatus_ =
  ListNamespacesResponse'
    { nextToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      namespaces = Prelude.mempty
    }

-- | When @nextToken@ is returned, there are more results available. The
-- value of @nextToken@ is a unique pagination token for each page. Make
-- the call again using the returned token to retrieve the next page.
listNamespacesResponse_nextToken :: Lens.Lens' ListNamespacesResponse (Prelude.Maybe Prelude.Text)
listNamespacesResponse_nextToken = Lens.lens (\ListNamespacesResponse' {nextToken} -> nextToken) (\s@ListNamespacesResponse' {} a -> s {nextToken = a} :: ListNamespacesResponse)

-- | The response's http status code.
listNamespacesResponse_httpStatus :: Lens.Lens' ListNamespacesResponse Prelude.Int
listNamespacesResponse_httpStatus = Lens.lens (\ListNamespacesResponse' {httpStatus} -> httpStatus) (\s@ListNamespacesResponse' {} a -> s {httpStatus = a} :: ListNamespacesResponse)

-- | The list of returned namespaces.
listNamespacesResponse_namespaces :: Lens.Lens' ListNamespacesResponse [Namespace]
listNamespacesResponse_namespaces = Lens.lens (\ListNamespacesResponse' {namespaces} -> namespaces) (\s@ListNamespacesResponse' {} a -> s {namespaces = a} :: ListNamespacesResponse) Prelude.. Lens.coerced

instance Prelude.NFData ListNamespacesResponse where
  rnf ListNamespacesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf namespaces
