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
-- Module      : Amazonka.AppMesh.ListRoutes
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of existing routes in a service mesh.
--
-- This operation returns paginated results.
module Amazonka.AppMesh.ListRoutes
  ( -- * Creating a Request
    ListRoutes (..),
    newListRoutes,

    -- * Request Lenses
    listRoutes_nextToken,
    listRoutes_meshOwner,
    listRoutes_limit,
    listRoutes_meshName,
    listRoutes_virtualRouterName,

    -- * Destructuring the Response
    ListRoutesResponse (..),
    newListRoutesResponse,

    -- * Response Lenses
    listRoutesResponse_nextToken,
    listRoutesResponse_httpStatus,
    listRoutesResponse_routes,
  )
where

import Amazonka.AppMesh.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- |
--
-- /See:/ 'newListRoutes' smart constructor.
data ListRoutes = ListRoutes'
  { -- | The @nextToken@ value returned from a previous paginated @ListRoutes@
    -- request where @limit@ was used and the results exceeded the value of
    -- that parameter. Pagination continues from the end of the previous
    -- results that returned the @nextToken@ value.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services IAM account ID of the service mesh owner. If the
    -- account ID is not your own, then it\'s the ID of the account that shared
    -- the mesh with your account. For more information about mesh sharing, see
    -- <https://docs.aws.amazon.com/app-mesh/latest/userguide/sharing.html Working with shared meshes>.
    meshOwner :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results returned by @ListRoutes@ in paginated
    -- output. When you use this parameter, @ListRoutes@ returns only @limit@
    -- results in a single page along with a @nextToken@ response element. You
    -- can see the remaining results of the initial request by sending another
    -- @ListRoutes@ request with the returned @nextToken@ value. This value can
    -- be between 1 and 100. If you don\'t use this parameter, @ListRoutes@
    -- returns up to 100 results and a @nextToken@ value if applicable.
    limit :: Prelude.Maybe Prelude.Natural,
    -- | The name of the service mesh to list routes in.
    meshName :: Prelude.Text,
    -- | The name of the virtual router to list routes in.
    virtualRouterName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListRoutes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listRoutes_nextToken' - The @nextToken@ value returned from a previous paginated @ListRoutes@
-- request where @limit@ was used and the results exceeded the value of
-- that parameter. Pagination continues from the end of the previous
-- results that returned the @nextToken@ value.
--
-- 'meshOwner', 'listRoutes_meshOwner' - The Amazon Web Services IAM account ID of the service mesh owner. If the
-- account ID is not your own, then it\'s the ID of the account that shared
-- the mesh with your account. For more information about mesh sharing, see
-- <https://docs.aws.amazon.com/app-mesh/latest/userguide/sharing.html Working with shared meshes>.
--
-- 'limit', 'listRoutes_limit' - The maximum number of results returned by @ListRoutes@ in paginated
-- output. When you use this parameter, @ListRoutes@ returns only @limit@
-- results in a single page along with a @nextToken@ response element. You
-- can see the remaining results of the initial request by sending another
-- @ListRoutes@ request with the returned @nextToken@ value. This value can
-- be between 1 and 100. If you don\'t use this parameter, @ListRoutes@
-- returns up to 100 results and a @nextToken@ value if applicable.
--
-- 'meshName', 'listRoutes_meshName' - The name of the service mesh to list routes in.
--
-- 'virtualRouterName', 'listRoutes_virtualRouterName' - The name of the virtual router to list routes in.
newListRoutes ::
  -- | 'meshName'
  Prelude.Text ->
  -- | 'virtualRouterName'
  Prelude.Text ->
  ListRoutes
newListRoutes pMeshName_ pVirtualRouterName_ =
  ListRoutes'
    { nextToken = Prelude.Nothing,
      meshOwner = Prelude.Nothing,
      limit = Prelude.Nothing,
      meshName = pMeshName_,
      virtualRouterName = pVirtualRouterName_
    }

-- | The @nextToken@ value returned from a previous paginated @ListRoutes@
-- request where @limit@ was used and the results exceeded the value of
-- that parameter. Pagination continues from the end of the previous
-- results that returned the @nextToken@ value.
listRoutes_nextToken :: Lens.Lens' ListRoutes (Prelude.Maybe Prelude.Text)
listRoutes_nextToken = Lens.lens (\ListRoutes' {nextToken} -> nextToken) (\s@ListRoutes' {} a -> s {nextToken = a} :: ListRoutes)

-- | The Amazon Web Services IAM account ID of the service mesh owner. If the
-- account ID is not your own, then it\'s the ID of the account that shared
-- the mesh with your account. For more information about mesh sharing, see
-- <https://docs.aws.amazon.com/app-mesh/latest/userguide/sharing.html Working with shared meshes>.
listRoutes_meshOwner :: Lens.Lens' ListRoutes (Prelude.Maybe Prelude.Text)
listRoutes_meshOwner = Lens.lens (\ListRoutes' {meshOwner} -> meshOwner) (\s@ListRoutes' {} a -> s {meshOwner = a} :: ListRoutes)

-- | The maximum number of results returned by @ListRoutes@ in paginated
-- output. When you use this parameter, @ListRoutes@ returns only @limit@
-- results in a single page along with a @nextToken@ response element. You
-- can see the remaining results of the initial request by sending another
-- @ListRoutes@ request with the returned @nextToken@ value. This value can
-- be between 1 and 100. If you don\'t use this parameter, @ListRoutes@
-- returns up to 100 results and a @nextToken@ value if applicable.
listRoutes_limit :: Lens.Lens' ListRoutes (Prelude.Maybe Prelude.Natural)
listRoutes_limit = Lens.lens (\ListRoutes' {limit} -> limit) (\s@ListRoutes' {} a -> s {limit = a} :: ListRoutes)

-- | The name of the service mesh to list routes in.
listRoutes_meshName :: Lens.Lens' ListRoutes Prelude.Text
listRoutes_meshName = Lens.lens (\ListRoutes' {meshName} -> meshName) (\s@ListRoutes' {} a -> s {meshName = a} :: ListRoutes)

-- | The name of the virtual router to list routes in.
listRoutes_virtualRouterName :: Lens.Lens' ListRoutes Prelude.Text
listRoutes_virtualRouterName = Lens.lens (\ListRoutes' {virtualRouterName} -> virtualRouterName) (\s@ListRoutes' {} a -> s {virtualRouterName = a} :: ListRoutes)

instance Core.AWSPager ListRoutes where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listRoutesResponse_nextToken Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop (rs Lens.^. listRoutesResponse_routes) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listRoutes_nextToken
          Lens..~ rs
          Lens.^? listRoutesResponse_nextToken Prelude.. Lens._Just

instance Core.AWSRequest ListRoutes where
  type AWSResponse ListRoutes = ListRoutesResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListRoutesResponse'
            Prelude.<$> (x Core..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..?> "routes" Core..!@ Prelude.mempty)
      )

instance Prelude.Hashable ListRoutes where
  hashWithSalt _salt ListRoutes' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` meshOwner
      `Prelude.hashWithSalt` limit
      `Prelude.hashWithSalt` meshName
      `Prelude.hashWithSalt` virtualRouterName

instance Prelude.NFData ListRoutes where
  rnf ListRoutes' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf meshOwner
      `Prelude.seq` Prelude.rnf limit
      `Prelude.seq` Prelude.rnf meshName
      `Prelude.seq` Prelude.rnf virtualRouterName

instance Core.ToHeaders ListRoutes where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath ListRoutes where
  toPath ListRoutes' {..} =
    Prelude.mconcat
      [ "/v20190125/meshes/",
        Core.toBS meshName,
        "/virtualRouter/",
        Core.toBS virtualRouterName,
        "/routes"
      ]

instance Core.ToQuery ListRoutes where
  toQuery ListRoutes' {..} =
    Prelude.mconcat
      [ "nextToken" Core.=: nextToken,
        "meshOwner" Core.=: meshOwner,
        "limit" Core.=: limit
      ]

-- |
--
-- /See:/ 'newListRoutesResponse' smart constructor.
data ListRoutesResponse = ListRoutesResponse'
  { -- | The @nextToken@ value to include in a future @ListRoutes@ request. When
    -- the results of a @ListRoutes@ request exceed @limit@, you can use this
    -- value to retrieve the next page of results. This value is @null@ when
    -- there are no more results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The list of existing routes for the specified service mesh and virtual
    -- router.
    routes :: [RouteRef]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListRoutesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listRoutesResponse_nextToken' - The @nextToken@ value to include in a future @ListRoutes@ request. When
-- the results of a @ListRoutes@ request exceed @limit@, you can use this
-- value to retrieve the next page of results. This value is @null@ when
-- there are no more results to return.
--
-- 'httpStatus', 'listRoutesResponse_httpStatus' - The response's http status code.
--
-- 'routes', 'listRoutesResponse_routes' - The list of existing routes for the specified service mesh and virtual
-- router.
newListRoutesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListRoutesResponse
newListRoutesResponse pHttpStatus_ =
  ListRoutesResponse'
    { nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_,
      routes = Prelude.mempty
    }

-- | The @nextToken@ value to include in a future @ListRoutes@ request. When
-- the results of a @ListRoutes@ request exceed @limit@, you can use this
-- value to retrieve the next page of results. This value is @null@ when
-- there are no more results to return.
listRoutesResponse_nextToken :: Lens.Lens' ListRoutesResponse (Prelude.Maybe Prelude.Text)
listRoutesResponse_nextToken = Lens.lens (\ListRoutesResponse' {nextToken} -> nextToken) (\s@ListRoutesResponse' {} a -> s {nextToken = a} :: ListRoutesResponse)

-- | The response's http status code.
listRoutesResponse_httpStatus :: Lens.Lens' ListRoutesResponse Prelude.Int
listRoutesResponse_httpStatus = Lens.lens (\ListRoutesResponse' {httpStatus} -> httpStatus) (\s@ListRoutesResponse' {} a -> s {httpStatus = a} :: ListRoutesResponse)

-- | The list of existing routes for the specified service mesh and virtual
-- router.
listRoutesResponse_routes :: Lens.Lens' ListRoutesResponse [RouteRef]
listRoutesResponse_routes = Lens.lens (\ListRoutesResponse' {routes} -> routes) (\s@ListRoutesResponse' {} a -> s {routes = a} :: ListRoutesResponse) Prelude.. Lens.coerced

instance Prelude.NFData ListRoutesResponse where
  rnf ListRoutesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf routes
