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
-- Module      : Amazonka.AppMesh.ListVirtualGateways
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of existing virtual gateways in a service mesh.
--
-- This operation returns paginated results.
module Amazonka.AppMesh.ListVirtualGateways
  ( -- * Creating a Request
    ListVirtualGateways (..),
    newListVirtualGateways,

    -- * Request Lenses
    listVirtualGateways_limit,
    listVirtualGateways_meshOwner,
    listVirtualGateways_nextToken,
    listVirtualGateways_meshName,

    -- * Destructuring the Response
    ListVirtualGatewaysResponse (..),
    newListVirtualGatewaysResponse,

    -- * Response Lenses
    listVirtualGatewaysResponse_nextToken,
    listVirtualGatewaysResponse_httpStatus,
    listVirtualGatewaysResponse_virtualGateways,
  )
where

import Amazonka.AppMesh.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListVirtualGateways' smart constructor.
data ListVirtualGateways = ListVirtualGateways'
  { -- | The maximum number of results returned by @ListVirtualGateways@ in
    -- paginated output. When you use this parameter, @ListVirtualGateways@
    -- returns only @limit@ results in a single page along with a @nextToken@
    -- response element. You can see the remaining results of the initial
    -- request by sending another @ListVirtualGateways@ request with the
    -- returned @nextToken@ value. This value can be between 1 and 100. If you
    -- don\'t use this parameter, @ListVirtualGateways@ returns up to 100
    -- results and a @nextToken@ value if applicable.
    limit :: Prelude.Maybe Prelude.Natural,
    -- | The Amazon Web Services IAM account ID of the service mesh owner. If the
    -- account ID is not your own, then it\'s the ID of the account that shared
    -- the mesh with your account. For more information about mesh sharing, see
    -- <https://docs.aws.amazon.com/app-mesh/latest/userguide/sharing.html Working with shared meshes>.
    meshOwner :: Prelude.Maybe Prelude.Text,
    -- | The @nextToken@ value returned from a previous paginated
    -- @ListVirtualGateways@ request where @limit@ was used and the results
    -- exceeded the value of that parameter. Pagination continues from the end
    -- of the previous results that returned the @nextToken@ value.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The name of the service mesh to list virtual gateways in.
    meshName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListVirtualGateways' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'limit', 'listVirtualGateways_limit' - The maximum number of results returned by @ListVirtualGateways@ in
-- paginated output. When you use this parameter, @ListVirtualGateways@
-- returns only @limit@ results in a single page along with a @nextToken@
-- response element. You can see the remaining results of the initial
-- request by sending another @ListVirtualGateways@ request with the
-- returned @nextToken@ value. This value can be between 1 and 100. If you
-- don\'t use this parameter, @ListVirtualGateways@ returns up to 100
-- results and a @nextToken@ value if applicable.
--
-- 'meshOwner', 'listVirtualGateways_meshOwner' - The Amazon Web Services IAM account ID of the service mesh owner. If the
-- account ID is not your own, then it\'s the ID of the account that shared
-- the mesh with your account. For more information about mesh sharing, see
-- <https://docs.aws.amazon.com/app-mesh/latest/userguide/sharing.html Working with shared meshes>.
--
-- 'nextToken', 'listVirtualGateways_nextToken' - The @nextToken@ value returned from a previous paginated
-- @ListVirtualGateways@ request where @limit@ was used and the results
-- exceeded the value of that parameter. Pagination continues from the end
-- of the previous results that returned the @nextToken@ value.
--
-- 'meshName', 'listVirtualGateways_meshName' - The name of the service mesh to list virtual gateways in.
newListVirtualGateways ::
  -- | 'meshName'
  Prelude.Text ->
  ListVirtualGateways
newListVirtualGateways pMeshName_ =
  ListVirtualGateways'
    { limit = Prelude.Nothing,
      meshOwner = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      meshName = pMeshName_
    }

-- | The maximum number of results returned by @ListVirtualGateways@ in
-- paginated output. When you use this parameter, @ListVirtualGateways@
-- returns only @limit@ results in a single page along with a @nextToken@
-- response element. You can see the remaining results of the initial
-- request by sending another @ListVirtualGateways@ request with the
-- returned @nextToken@ value. This value can be between 1 and 100. If you
-- don\'t use this parameter, @ListVirtualGateways@ returns up to 100
-- results and a @nextToken@ value if applicable.
listVirtualGateways_limit :: Lens.Lens' ListVirtualGateways (Prelude.Maybe Prelude.Natural)
listVirtualGateways_limit = Lens.lens (\ListVirtualGateways' {limit} -> limit) (\s@ListVirtualGateways' {} a -> s {limit = a} :: ListVirtualGateways)

-- | The Amazon Web Services IAM account ID of the service mesh owner. If the
-- account ID is not your own, then it\'s the ID of the account that shared
-- the mesh with your account. For more information about mesh sharing, see
-- <https://docs.aws.amazon.com/app-mesh/latest/userguide/sharing.html Working with shared meshes>.
listVirtualGateways_meshOwner :: Lens.Lens' ListVirtualGateways (Prelude.Maybe Prelude.Text)
listVirtualGateways_meshOwner = Lens.lens (\ListVirtualGateways' {meshOwner} -> meshOwner) (\s@ListVirtualGateways' {} a -> s {meshOwner = a} :: ListVirtualGateways)

-- | The @nextToken@ value returned from a previous paginated
-- @ListVirtualGateways@ request where @limit@ was used and the results
-- exceeded the value of that parameter. Pagination continues from the end
-- of the previous results that returned the @nextToken@ value.
listVirtualGateways_nextToken :: Lens.Lens' ListVirtualGateways (Prelude.Maybe Prelude.Text)
listVirtualGateways_nextToken = Lens.lens (\ListVirtualGateways' {nextToken} -> nextToken) (\s@ListVirtualGateways' {} a -> s {nextToken = a} :: ListVirtualGateways)

-- | The name of the service mesh to list virtual gateways in.
listVirtualGateways_meshName :: Lens.Lens' ListVirtualGateways Prelude.Text
listVirtualGateways_meshName = Lens.lens (\ListVirtualGateways' {meshName} -> meshName) (\s@ListVirtualGateways' {} a -> s {meshName = a} :: ListVirtualGateways)

instance Core.AWSPager ListVirtualGateways where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listVirtualGatewaysResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^. listVirtualGatewaysResponse_virtualGateways
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listVirtualGateways_nextToken
          Lens..~ rs
          Lens.^? listVirtualGatewaysResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListVirtualGateways where
  type
    AWSResponse ListVirtualGateways =
      ListVirtualGatewaysResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListVirtualGatewaysResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x
                            Data..?> "virtualGateways"
                            Core..!@ Prelude.mempty
                        )
      )

instance Prelude.Hashable ListVirtualGateways where
  hashWithSalt _salt ListVirtualGateways' {..} =
    _salt
      `Prelude.hashWithSalt` limit
      `Prelude.hashWithSalt` meshOwner
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` meshName

instance Prelude.NFData ListVirtualGateways where
  rnf ListVirtualGateways' {..} =
    Prelude.rnf limit
      `Prelude.seq` Prelude.rnf meshOwner
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf meshName

instance Data.ToHeaders ListVirtualGateways where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListVirtualGateways where
  toPath ListVirtualGateways' {..} =
    Prelude.mconcat
      [ "/v20190125/meshes/",
        Data.toBS meshName,
        "/virtualGateways"
      ]

instance Data.ToQuery ListVirtualGateways where
  toQuery ListVirtualGateways' {..} =
    Prelude.mconcat
      [ "limit" Data.=: limit,
        "meshOwner" Data.=: meshOwner,
        "nextToken" Data.=: nextToken
      ]

-- | /See:/ 'newListVirtualGatewaysResponse' smart constructor.
data ListVirtualGatewaysResponse = ListVirtualGatewaysResponse'
  { -- | The @nextToken@ value to include in a future @ListVirtualGateways@
    -- request. When the results of a @ListVirtualGateways@ request exceed
    -- @limit@, you can use this value to retrieve the next page of results.
    -- This value is @null@ when there are no more results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The list of existing virtual gateways for the specified service mesh.
    virtualGateways :: [VirtualGatewayRef]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListVirtualGatewaysResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listVirtualGatewaysResponse_nextToken' - The @nextToken@ value to include in a future @ListVirtualGateways@
-- request. When the results of a @ListVirtualGateways@ request exceed
-- @limit@, you can use this value to retrieve the next page of results.
-- This value is @null@ when there are no more results to return.
--
-- 'httpStatus', 'listVirtualGatewaysResponse_httpStatus' - The response's http status code.
--
-- 'virtualGateways', 'listVirtualGatewaysResponse_virtualGateways' - The list of existing virtual gateways for the specified service mesh.
newListVirtualGatewaysResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListVirtualGatewaysResponse
newListVirtualGatewaysResponse pHttpStatus_ =
  ListVirtualGatewaysResponse'
    { nextToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      virtualGateways = Prelude.mempty
    }

-- | The @nextToken@ value to include in a future @ListVirtualGateways@
-- request. When the results of a @ListVirtualGateways@ request exceed
-- @limit@, you can use this value to retrieve the next page of results.
-- This value is @null@ when there are no more results to return.
listVirtualGatewaysResponse_nextToken :: Lens.Lens' ListVirtualGatewaysResponse (Prelude.Maybe Prelude.Text)
listVirtualGatewaysResponse_nextToken = Lens.lens (\ListVirtualGatewaysResponse' {nextToken} -> nextToken) (\s@ListVirtualGatewaysResponse' {} a -> s {nextToken = a} :: ListVirtualGatewaysResponse)

-- | The response's http status code.
listVirtualGatewaysResponse_httpStatus :: Lens.Lens' ListVirtualGatewaysResponse Prelude.Int
listVirtualGatewaysResponse_httpStatus = Lens.lens (\ListVirtualGatewaysResponse' {httpStatus} -> httpStatus) (\s@ListVirtualGatewaysResponse' {} a -> s {httpStatus = a} :: ListVirtualGatewaysResponse)

-- | The list of existing virtual gateways for the specified service mesh.
listVirtualGatewaysResponse_virtualGateways :: Lens.Lens' ListVirtualGatewaysResponse [VirtualGatewayRef]
listVirtualGatewaysResponse_virtualGateways = Lens.lens (\ListVirtualGatewaysResponse' {virtualGateways} -> virtualGateways) (\s@ListVirtualGatewaysResponse' {} a -> s {virtualGateways = a} :: ListVirtualGatewaysResponse) Prelude.. Lens.coerced

instance Prelude.NFData ListVirtualGatewaysResponse where
  rnf ListVirtualGatewaysResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf virtualGateways
