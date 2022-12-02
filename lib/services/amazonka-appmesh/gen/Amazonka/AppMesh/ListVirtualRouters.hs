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
-- Module      : Amazonka.AppMesh.ListVirtualRouters
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of existing virtual routers in a service mesh.
--
-- This operation returns paginated results.
module Amazonka.AppMesh.ListVirtualRouters
  ( -- * Creating a Request
    ListVirtualRouters (..),
    newListVirtualRouters,

    -- * Request Lenses
    listVirtualRouters_nextToken,
    listVirtualRouters_meshOwner,
    listVirtualRouters_limit,
    listVirtualRouters_meshName,

    -- * Destructuring the Response
    ListVirtualRoutersResponse (..),
    newListVirtualRoutersResponse,

    -- * Response Lenses
    listVirtualRoutersResponse_nextToken,
    listVirtualRoutersResponse_httpStatus,
    listVirtualRoutersResponse_virtualRouters,
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
-- /See:/ 'newListVirtualRouters' smart constructor.
data ListVirtualRouters = ListVirtualRouters'
  { -- | The @nextToken@ value returned from a previous paginated
    -- @ListVirtualRouters@ request where @limit@ was used and the results
    -- exceeded the value of that parameter. Pagination continues from the end
    -- of the previous results that returned the @nextToken@ value.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services IAM account ID of the service mesh owner. If the
    -- account ID is not your own, then it\'s the ID of the account that shared
    -- the mesh with your account. For more information about mesh sharing, see
    -- <https://docs.aws.amazon.com/app-mesh/latest/userguide/sharing.html Working with shared meshes>.
    meshOwner :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results returned by @ListVirtualRouters@ in
    -- paginated output. When you use this parameter, @ListVirtualRouters@
    -- returns only @limit@ results in a single page along with a @nextToken@
    -- response element. You can see the remaining results of the initial
    -- request by sending another @ListVirtualRouters@ request with the
    -- returned @nextToken@ value. This value can be between 1 and 100. If you
    -- don\'t use this parameter, @ListVirtualRouters@ returns up to 100
    -- results and a @nextToken@ value if applicable.
    limit :: Prelude.Maybe Prelude.Natural,
    -- | The name of the service mesh to list virtual routers in.
    meshName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListVirtualRouters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listVirtualRouters_nextToken' - The @nextToken@ value returned from a previous paginated
-- @ListVirtualRouters@ request where @limit@ was used and the results
-- exceeded the value of that parameter. Pagination continues from the end
-- of the previous results that returned the @nextToken@ value.
--
-- 'meshOwner', 'listVirtualRouters_meshOwner' - The Amazon Web Services IAM account ID of the service mesh owner. If the
-- account ID is not your own, then it\'s the ID of the account that shared
-- the mesh with your account. For more information about mesh sharing, see
-- <https://docs.aws.amazon.com/app-mesh/latest/userguide/sharing.html Working with shared meshes>.
--
-- 'limit', 'listVirtualRouters_limit' - The maximum number of results returned by @ListVirtualRouters@ in
-- paginated output. When you use this parameter, @ListVirtualRouters@
-- returns only @limit@ results in a single page along with a @nextToken@
-- response element. You can see the remaining results of the initial
-- request by sending another @ListVirtualRouters@ request with the
-- returned @nextToken@ value. This value can be between 1 and 100. If you
-- don\'t use this parameter, @ListVirtualRouters@ returns up to 100
-- results and a @nextToken@ value if applicable.
--
-- 'meshName', 'listVirtualRouters_meshName' - The name of the service mesh to list virtual routers in.
newListVirtualRouters ::
  -- | 'meshName'
  Prelude.Text ->
  ListVirtualRouters
newListVirtualRouters pMeshName_ =
  ListVirtualRouters'
    { nextToken = Prelude.Nothing,
      meshOwner = Prelude.Nothing,
      limit = Prelude.Nothing,
      meshName = pMeshName_
    }

-- | The @nextToken@ value returned from a previous paginated
-- @ListVirtualRouters@ request where @limit@ was used and the results
-- exceeded the value of that parameter. Pagination continues from the end
-- of the previous results that returned the @nextToken@ value.
listVirtualRouters_nextToken :: Lens.Lens' ListVirtualRouters (Prelude.Maybe Prelude.Text)
listVirtualRouters_nextToken = Lens.lens (\ListVirtualRouters' {nextToken} -> nextToken) (\s@ListVirtualRouters' {} a -> s {nextToken = a} :: ListVirtualRouters)

-- | The Amazon Web Services IAM account ID of the service mesh owner. If the
-- account ID is not your own, then it\'s the ID of the account that shared
-- the mesh with your account. For more information about mesh sharing, see
-- <https://docs.aws.amazon.com/app-mesh/latest/userguide/sharing.html Working with shared meshes>.
listVirtualRouters_meshOwner :: Lens.Lens' ListVirtualRouters (Prelude.Maybe Prelude.Text)
listVirtualRouters_meshOwner = Lens.lens (\ListVirtualRouters' {meshOwner} -> meshOwner) (\s@ListVirtualRouters' {} a -> s {meshOwner = a} :: ListVirtualRouters)

-- | The maximum number of results returned by @ListVirtualRouters@ in
-- paginated output. When you use this parameter, @ListVirtualRouters@
-- returns only @limit@ results in a single page along with a @nextToken@
-- response element. You can see the remaining results of the initial
-- request by sending another @ListVirtualRouters@ request with the
-- returned @nextToken@ value. This value can be between 1 and 100. If you
-- don\'t use this parameter, @ListVirtualRouters@ returns up to 100
-- results and a @nextToken@ value if applicable.
listVirtualRouters_limit :: Lens.Lens' ListVirtualRouters (Prelude.Maybe Prelude.Natural)
listVirtualRouters_limit = Lens.lens (\ListVirtualRouters' {limit} -> limit) (\s@ListVirtualRouters' {} a -> s {limit = a} :: ListVirtualRouters)

-- | The name of the service mesh to list virtual routers in.
listVirtualRouters_meshName :: Lens.Lens' ListVirtualRouters Prelude.Text
listVirtualRouters_meshName = Lens.lens (\ListVirtualRouters' {meshName} -> meshName) (\s@ListVirtualRouters' {} a -> s {meshName = a} :: ListVirtualRouters)

instance Core.AWSPager ListVirtualRouters where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listVirtualRoutersResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^. listVirtualRoutersResponse_virtualRouters
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listVirtualRouters_nextToken
          Lens..~ rs
          Lens.^? listVirtualRoutersResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListVirtualRouters where
  type
    AWSResponse ListVirtualRouters =
      ListVirtualRoutersResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListVirtualRoutersResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x Data..?> "virtualRouters"
                            Core..!@ Prelude.mempty
                        )
      )

instance Prelude.Hashable ListVirtualRouters where
  hashWithSalt _salt ListVirtualRouters' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` meshOwner
      `Prelude.hashWithSalt` limit
      `Prelude.hashWithSalt` meshName

instance Prelude.NFData ListVirtualRouters where
  rnf ListVirtualRouters' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf meshOwner
      `Prelude.seq` Prelude.rnf limit
      `Prelude.seq` Prelude.rnf meshName

instance Data.ToHeaders ListVirtualRouters where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListVirtualRouters where
  toPath ListVirtualRouters' {..} =
    Prelude.mconcat
      [ "/v20190125/meshes/",
        Data.toBS meshName,
        "/virtualRouters"
      ]

instance Data.ToQuery ListVirtualRouters where
  toQuery ListVirtualRouters' {..} =
    Prelude.mconcat
      [ "nextToken" Data.=: nextToken,
        "meshOwner" Data.=: meshOwner,
        "limit" Data.=: limit
      ]

-- |
--
-- /See:/ 'newListVirtualRoutersResponse' smart constructor.
data ListVirtualRoutersResponse = ListVirtualRoutersResponse'
  { -- | The @nextToken@ value to include in a future @ListVirtualRouters@
    -- request. When the results of a @ListVirtualRouters@ request exceed
    -- @limit@, you can use this value to retrieve the next page of results.
    -- This value is @null@ when there are no more results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The list of existing virtual routers for the specified service mesh.
    virtualRouters :: [VirtualRouterRef]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListVirtualRoutersResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listVirtualRoutersResponse_nextToken' - The @nextToken@ value to include in a future @ListVirtualRouters@
-- request. When the results of a @ListVirtualRouters@ request exceed
-- @limit@, you can use this value to retrieve the next page of results.
-- This value is @null@ when there are no more results to return.
--
-- 'httpStatus', 'listVirtualRoutersResponse_httpStatus' - The response's http status code.
--
-- 'virtualRouters', 'listVirtualRoutersResponse_virtualRouters' - The list of existing virtual routers for the specified service mesh.
newListVirtualRoutersResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListVirtualRoutersResponse
newListVirtualRoutersResponse pHttpStatus_ =
  ListVirtualRoutersResponse'
    { nextToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      virtualRouters = Prelude.mempty
    }

-- | The @nextToken@ value to include in a future @ListVirtualRouters@
-- request. When the results of a @ListVirtualRouters@ request exceed
-- @limit@, you can use this value to retrieve the next page of results.
-- This value is @null@ when there are no more results to return.
listVirtualRoutersResponse_nextToken :: Lens.Lens' ListVirtualRoutersResponse (Prelude.Maybe Prelude.Text)
listVirtualRoutersResponse_nextToken = Lens.lens (\ListVirtualRoutersResponse' {nextToken} -> nextToken) (\s@ListVirtualRoutersResponse' {} a -> s {nextToken = a} :: ListVirtualRoutersResponse)

-- | The response's http status code.
listVirtualRoutersResponse_httpStatus :: Lens.Lens' ListVirtualRoutersResponse Prelude.Int
listVirtualRoutersResponse_httpStatus = Lens.lens (\ListVirtualRoutersResponse' {httpStatus} -> httpStatus) (\s@ListVirtualRoutersResponse' {} a -> s {httpStatus = a} :: ListVirtualRoutersResponse)

-- | The list of existing virtual routers for the specified service mesh.
listVirtualRoutersResponse_virtualRouters :: Lens.Lens' ListVirtualRoutersResponse [VirtualRouterRef]
listVirtualRoutersResponse_virtualRouters = Lens.lens (\ListVirtualRoutersResponse' {virtualRouters} -> virtualRouters) (\s@ListVirtualRoutersResponse' {} a -> s {virtualRouters = a} :: ListVirtualRoutersResponse) Prelude.. Lens.coerced

instance Prelude.NFData ListVirtualRoutersResponse where
  rnf ListVirtualRoutersResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf virtualRouters
