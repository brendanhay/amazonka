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
-- Module      : Amazonka.AppMesh.ListVirtualServices
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of existing virtual services in a service mesh.
--
-- This operation returns paginated results.
module Amazonka.AppMesh.ListVirtualServices
  ( -- * Creating a Request
    ListVirtualServices (..),
    newListVirtualServices,

    -- * Request Lenses
    listVirtualServices_limit,
    listVirtualServices_meshOwner,
    listVirtualServices_nextToken,
    listVirtualServices_meshName,

    -- * Destructuring the Response
    ListVirtualServicesResponse (..),
    newListVirtualServicesResponse,

    -- * Response Lenses
    listVirtualServicesResponse_nextToken,
    listVirtualServicesResponse_httpStatus,
    listVirtualServicesResponse_virtualServices,
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
-- /See:/ 'newListVirtualServices' smart constructor.
data ListVirtualServices = ListVirtualServices'
  { -- | The maximum number of results returned by @ListVirtualServices@ in
    -- paginated output. When you use this parameter, @ListVirtualServices@
    -- returns only @limit@ results in a single page along with a @nextToken@
    -- response element. You can see the remaining results of the initial
    -- request by sending another @ListVirtualServices@ request with the
    -- returned @nextToken@ value. This value can be between 1 and 100. If you
    -- don\'t use this parameter, @ListVirtualServices@ returns up to 100
    -- results and a @nextToken@ value if applicable.
    limit :: Prelude.Maybe Prelude.Natural,
    -- | The Amazon Web Services IAM account ID of the service mesh owner. If the
    -- account ID is not your own, then it\'s the ID of the account that shared
    -- the mesh with your account. For more information about mesh sharing, see
    -- <https://docs.aws.amazon.com/app-mesh/latest/userguide/sharing.html Working with shared meshes>.
    meshOwner :: Prelude.Maybe Prelude.Text,
    -- | The @nextToken@ value returned from a previous paginated
    -- @ListVirtualServices@ request where @limit@ was used and the results
    -- exceeded the value of that parameter. Pagination continues from the end
    -- of the previous results that returned the @nextToken@ value.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The name of the service mesh to list virtual services in.
    meshName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListVirtualServices' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'limit', 'listVirtualServices_limit' - The maximum number of results returned by @ListVirtualServices@ in
-- paginated output. When you use this parameter, @ListVirtualServices@
-- returns only @limit@ results in a single page along with a @nextToken@
-- response element. You can see the remaining results of the initial
-- request by sending another @ListVirtualServices@ request with the
-- returned @nextToken@ value. This value can be between 1 and 100. If you
-- don\'t use this parameter, @ListVirtualServices@ returns up to 100
-- results and a @nextToken@ value if applicable.
--
-- 'meshOwner', 'listVirtualServices_meshOwner' - The Amazon Web Services IAM account ID of the service mesh owner. If the
-- account ID is not your own, then it\'s the ID of the account that shared
-- the mesh with your account. For more information about mesh sharing, see
-- <https://docs.aws.amazon.com/app-mesh/latest/userguide/sharing.html Working with shared meshes>.
--
-- 'nextToken', 'listVirtualServices_nextToken' - The @nextToken@ value returned from a previous paginated
-- @ListVirtualServices@ request where @limit@ was used and the results
-- exceeded the value of that parameter. Pagination continues from the end
-- of the previous results that returned the @nextToken@ value.
--
-- 'meshName', 'listVirtualServices_meshName' - The name of the service mesh to list virtual services in.
newListVirtualServices ::
  -- | 'meshName'
  Prelude.Text ->
  ListVirtualServices
newListVirtualServices pMeshName_ =
  ListVirtualServices'
    { limit = Prelude.Nothing,
      meshOwner = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      meshName = pMeshName_
    }

-- | The maximum number of results returned by @ListVirtualServices@ in
-- paginated output. When you use this parameter, @ListVirtualServices@
-- returns only @limit@ results in a single page along with a @nextToken@
-- response element. You can see the remaining results of the initial
-- request by sending another @ListVirtualServices@ request with the
-- returned @nextToken@ value. This value can be between 1 and 100. If you
-- don\'t use this parameter, @ListVirtualServices@ returns up to 100
-- results and a @nextToken@ value if applicable.
listVirtualServices_limit :: Lens.Lens' ListVirtualServices (Prelude.Maybe Prelude.Natural)
listVirtualServices_limit = Lens.lens (\ListVirtualServices' {limit} -> limit) (\s@ListVirtualServices' {} a -> s {limit = a} :: ListVirtualServices)

-- | The Amazon Web Services IAM account ID of the service mesh owner. If the
-- account ID is not your own, then it\'s the ID of the account that shared
-- the mesh with your account. For more information about mesh sharing, see
-- <https://docs.aws.amazon.com/app-mesh/latest/userguide/sharing.html Working with shared meshes>.
listVirtualServices_meshOwner :: Lens.Lens' ListVirtualServices (Prelude.Maybe Prelude.Text)
listVirtualServices_meshOwner = Lens.lens (\ListVirtualServices' {meshOwner} -> meshOwner) (\s@ListVirtualServices' {} a -> s {meshOwner = a} :: ListVirtualServices)

-- | The @nextToken@ value returned from a previous paginated
-- @ListVirtualServices@ request where @limit@ was used and the results
-- exceeded the value of that parameter. Pagination continues from the end
-- of the previous results that returned the @nextToken@ value.
listVirtualServices_nextToken :: Lens.Lens' ListVirtualServices (Prelude.Maybe Prelude.Text)
listVirtualServices_nextToken = Lens.lens (\ListVirtualServices' {nextToken} -> nextToken) (\s@ListVirtualServices' {} a -> s {nextToken = a} :: ListVirtualServices)

-- | The name of the service mesh to list virtual services in.
listVirtualServices_meshName :: Lens.Lens' ListVirtualServices Prelude.Text
listVirtualServices_meshName = Lens.lens (\ListVirtualServices' {meshName} -> meshName) (\s@ListVirtualServices' {} a -> s {meshName = a} :: ListVirtualServices)

instance Core.AWSPager ListVirtualServices where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listVirtualServicesResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^. listVirtualServicesResponse_virtualServices
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listVirtualServices_nextToken
          Lens..~ rs
          Lens.^? listVirtualServicesResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListVirtualServices where
  type
    AWSResponse ListVirtualServices =
      ListVirtualServicesResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListVirtualServicesResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x
                            Data..?> "virtualServices"
                            Core..!@ Prelude.mempty
                        )
      )

instance Prelude.Hashable ListVirtualServices where
  hashWithSalt _salt ListVirtualServices' {..} =
    _salt
      `Prelude.hashWithSalt` limit
      `Prelude.hashWithSalt` meshOwner
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` meshName

instance Prelude.NFData ListVirtualServices where
  rnf ListVirtualServices' {..} =
    Prelude.rnf limit
      `Prelude.seq` Prelude.rnf meshOwner
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf meshName

instance Data.ToHeaders ListVirtualServices where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListVirtualServices where
  toPath ListVirtualServices' {..} =
    Prelude.mconcat
      [ "/v20190125/meshes/",
        Data.toBS meshName,
        "/virtualServices"
      ]

instance Data.ToQuery ListVirtualServices where
  toQuery ListVirtualServices' {..} =
    Prelude.mconcat
      [ "limit" Data.=: limit,
        "meshOwner" Data.=: meshOwner,
        "nextToken" Data.=: nextToken
      ]

-- |
--
-- /See:/ 'newListVirtualServicesResponse' smart constructor.
data ListVirtualServicesResponse = ListVirtualServicesResponse'
  { -- | The @nextToken@ value to include in a future @ListVirtualServices@
    -- request. When the results of a @ListVirtualServices@ request exceed
    -- @limit@, you can use this value to retrieve the next page of results.
    -- This value is @null@ when there are no more results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The list of existing virtual services for the specified service mesh.
    virtualServices :: [VirtualServiceRef]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListVirtualServicesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listVirtualServicesResponse_nextToken' - The @nextToken@ value to include in a future @ListVirtualServices@
-- request. When the results of a @ListVirtualServices@ request exceed
-- @limit@, you can use this value to retrieve the next page of results.
-- This value is @null@ when there are no more results to return.
--
-- 'httpStatus', 'listVirtualServicesResponse_httpStatus' - The response's http status code.
--
-- 'virtualServices', 'listVirtualServicesResponse_virtualServices' - The list of existing virtual services for the specified service mesh.
newListVirtualServicesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListVirtualServicesResponse
newListVirtualServicesResponse pHttpStatus_ =
  ListVirtualServicesResponse'
    { nextToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      virtualServices = Prelude.mempty
    }

-- | The @nextToken@ value to include in a future @ListVirtualServices@
-- request. When the results of a @ListVirtualServices@ request exceed
-- @limit@, you can use this value to retrieve the next page of results.
-- This value is @null@ when there are no more results to return.
listVirtualServicesResponse_nextToken :: Lens.Lens' ListVirtualServicesResponse (Prelude.Maybe Prelude.Text)
listVirtualServicesResponse_nextToken = Lens.lens (\ListVirtualServicesResponse' {nextToken} -> nextToken) (\s@ListVirtualServicesResponse' {} a -> s {nextToken = a} :: ListVirtualServicesResponse)

-- | The response's http status code.
listVirtualServicesResponse_httpStatus :: Lens.Lens' ListVirtualServicesResponse Prelude.Int
listVirtualServicesResponse_httpStatus = Lens.lens (\ListVirtualServicesResponse' {httpStatus} -> httpStatus) (\s@ListVirtualServicesResponse' {} a -> s {httpStatus = a} :: ListVirtualServicesResponse)

-- | The list of existing virtual services for the specified service mesh.
listVirtualServicesResponse_virtualServices :: Lens.Lens' ListVirtualServicesResponse [VirtualServiceRef]
listVirtualServicesResponse_virtualServices = Lens.lens (\ListVirtualServicesResponse' {virtualServices} -> virtualServices) (\s@ListVirtualServicesResponse' {} a -> s {virtualServices = a} :: ListVirtualServicesResponse) Prelude.. Lens.coerced

instance Prelude.NFData ListVirtualServicesResponse where
  rnf ListVirtualServicesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf virtualServices
