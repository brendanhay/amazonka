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
-- Module      : Amazonka.AppMesh.ListMeshes
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of existing service meshes.
--
-- This operation returns paginated results.
module Amazonka.AppMesh.ListMeshes
  ( -- * Creating a Request
    ListMeshes (..),
    newListMeshes,

    -- * Request Lenses
    listMeshes_nextToken,
    listMeshes_limit,

    -- * Destructuring the Response
    ListMeshesResponse (..),
    newListMeshesResponse,

    -- * Response Lenses
    listMeshesResponse_nextToken,
    listMeshesResponse_httpStatus,
    listMeshesResponse_meshes,
  )
where

import Amazonka.AppMesh.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- |
--
-- /See:/ 'newListMeshes' smart constructor.
data ListMeshes = ListMeshes'
  { -- | The @nextToken@ value returned from a previous paginated @ListMeshes@
    -- request where @limit@ was used and the results exceeded the value of
    -- that parameter. Pagination continues from the end of the previous
    -- results that returned the @nextToken@ value.
    --
    -- This token should be treated as an opaque identifier that is used only
    -- to retrieve the next items in a list and not for other programmatic
    -- purposes.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results returned by @ListMeshes@ in paginated
    -- output. When you use this parameter, @ListMeshes@ returns only @limit@
    -- results in a single page along with a @nextToken@ response element. You
    -- can see the remaining results of the initial request by sending another
    -- @ListMeshes@ request with the returned @nextToken@ value. This value can
    -- be between 1 and 100. If you don\'t use this parameter, @ListMeshes@
    -- returns up to 100 results and a @nextToken@ value if applicable.
    limit :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListMeshes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listMeshes_nextToken' - The @nextToken@ value returned from a previous paginated @ListMeshes@
-- request where @limit@ was used and the results exceeded the value of
-- that parameter. Pagination continues from the end of the previous
-- results that returned the @nextToken@ value.
--
-- This token should be treated as an opaque identifier that is used only
-- to retrieve the next items in a list and not for other programmatic
-- purposes.
--
-- 'limit', 'listMeshes_limit' - The maximum number of results returned by @ListMeshes@ in paginated
-- output. When you use this parameter, @ListMeshes@ returns only @limit@
-- results in a single page along with a @nextToken@ response element. You
-- can see the remaining results of the initial request by sending another
-- @ListMeshes@ request with the returned @nextToken@ value. This value can
-- be between 1 and 100. If you don\'t use this parameter, @ListMeshes@
-- returns up to 100 results and a @nextToken@ value if applicable.
newListMeshes ::
  ListMeshes
newListMeshes =
  ListMeshes'
    { nextToken = Prelude.Nothing,
      limit = Prelude.Nothing
    }

-- | The @nextToken@ value returned from a previous paginated @ListMeshes@
-- request where @limit@ was used and the results exceeded the value of
-- that parameter. Pagination continues from the end of the previous
-- results that returned the @nextToken@ value.
--
-- This token should be treated as an opaque identifier that is used only
-- to retrieve the next items in a list and not for other programmatic
-- purposes.
listMeshes_nextToken :: Lens.Lens' ListMeshes (Prelude.Maybe Prelude.Text)
listMeshes_nextToken = Lens.lens (\ListMeshes' {nextToken} -> nextToken) (\s@ListMeshes' {} a -> s {nextToken = a} :: ListMeshes)

-- | The maximum number of results returned by @ListMeshes@ in paginated
-- output. When you use this parameter, @ListMeshes@ returns only @limit@
-- results in a single page along with a @nextToken@ response element. You
-- can see the remaining results of the initial request by sending another
-- @ListMeshes@ request with the returned @nextToken@ value. This value can
-- be between 1 and 100. If you don\'t use this parameter, @ListMeshes@
-- returns up to 100 results and a @nextToken@ value if applicable.
listMeshes_limit :: Lens.Lens' ListMeshes (Prelude.Maybe Prelude.Natural)
listMeshes_limit = Lens.lens (\ListMeshes' {limit} -> limit) (\s@ListMeshes' {} a -> s {limit = a} :: ListMeshes)

instance Core.AWSPager ListMeshes where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listMeshesResponse_nextToken Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop (rs Lens.^. listMeshesResponse_meshes) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listMeshes_nextToken
          Lens..~ rs
          Lens.^? listMeshesResponse_nextToken Prelude.. Lens._Just

instance Core.AWSRequest ListMeshes where
  type AWSResponse ListMeshes = ListMeshesResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListMeshesResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..?> "meshes" Core..!@ Prelude.mempty)
      )

instance Prelude.Hashable ListMeshes where
  hashWithSalt _salt ListMeshes' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` limit

instance Prelude.NFData ListMeshes where
  rnf ListMeshes' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf limit

instance Data.ToHeaders ListMeshes where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListMeshes where
  toPath = Prelude.const "/v20190125/meshes"

instance Data.ToQuery ListMeshes where
  toQuery ListMeshes' {..} =
    Prelude.mconcat
      [ "nextToken" Data.=: nextToken,
        "limit" Data.=: limit
      ]

-- |
--
-- /See:/ 'newListMeshesResponse' smart constructor.
data ListMeshesResponse = ListMeshesResponse'
  { -- | The @nextToken@ value to include in a future @ListMeshes@ request. When
    -- the results of a @ListMeshes@ request exceed @limit@, you can use this
    -- value to retrieve the next page of results. This value is @null@ when
    -- there are no more results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The list of existing service meshes.
    meshes :: [MeshRef]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListMeshesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listMeshesResponse_nextToken' - The @nextToken@ value to include in a future @ListMeshes@ request. When
-- the results of a @ListMeshes@ request exceed @limit@, you can use this
-- value to retrieve the next page of results. This value is @null@ when
-- there are no more results to return.
--
-- 'httpStatus', 'listMeshesResponse_httpStatus' - The response's http status code.
--
-- 'meshes', 'listMeshesResponse_meshes' - The list of existing service meshes.
newListMeshesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListMeshesResponse
newListMeshesResponse pHttpStatus_ =
  ListMeshesResponse'
    { nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_,
      meshes = Prelude.mempty
    }

-- | The @nextToken@ value to include in a future @ListMeshes@ request. When
-- the results of a @ListMeshes@ request exceed @limit@, you can use this
-- value to retrieve the next page of results. This value is @null@ when
-- there are no more results to return.
listMeshesResponse_nextToken :: Lens.Lens' ListMeshesResponse (Prelude.Maybe Prelude.Text)
listMeshesResponse_nextToken = Lens.lens (\ListMeshesResponse' {nextToken} -> nextToken) (\s@ListMeshesResponse' {} a -> s {nextToken = a} :: ListMeshesResponse)

-- | The response's http status code.
listMeshesResponse_httpStatus :: Lens.Lens' ListMeshesResponse Prelude.Int
listMeshesResponse_httpStatus = Lens.lens (\ListMeshesResponse' {httpStatus} -> httpStatus) (\s@ListMeshesResponse' {} a -> s {httpStatus = a} :: ListMeshesResponse)

-- | The list of existing service meshes.
listMeshesResponse_meshes :: Lens.Lens' ListMeshesResponse [MeshRef]
listMeshesResponse_meshes = Lens.lens (\ListMeshesResponse' {meshes} -> meshes) (\s@ListMeshesResponse' {} a -> s {meshes = a} :: ListMeshesResponse) Prelude.. Lens.coerced

instance Prelude.NFData ListMeshesResponse where
  rnf ListMeshesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf meshes
