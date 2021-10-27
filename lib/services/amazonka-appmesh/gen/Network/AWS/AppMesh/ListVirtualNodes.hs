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
-- Module      : Network.AWS.AppMesh.ListVirtualNodes
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of existing virtual nodes.
--
-- This operation returns paginated results.
module Network.AWS.AppMesh.ListVirtualNodes
  ( -- * Creating a Request
    ListVirtualNodes (..),
    newListVirtualNodes,

    -- * Request Lenses
    listVirtualNodes_meshOwner,
    listVirtualNodes_nextToken,
    listVirtualNodes_limit,
    listVirtualNodes_meshName,

    -- * Destructuring the Response
    ListVirtualNodesResponse (..),
    newListVirtualNodesResponse,

    -- * Response Lenses
    listVirtualNodesResponse_nextToken,
    listVirtualNodesResponse_httpStatus,
    listVirtualNodesResponse_virtualNodes,
  )
where

import Network.AWS.AppMesh.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'newListVirtualNodes' smart constructor.
data ListVirtualNodes = ListVirtualNodes'
  { -- | The AWS IAM account ID of the service mesh owner. If the account ID is
    -- not your own, then it\'s the ID of the account that shared the mesh with
    -- your account. For more information about mesh sharing, see
    -- <https://docs.aws.amazon.com/app-mesh/latest/userguide/sharing.html Working with shared meshes>.
    meshOwner :: Prelude.Maybe Prelude.Text,
    -- | The @nextToken@ value returned from a previous paginated
    -- @ListVirtualNodes@ request where @limit@ was used and the results
    -- exceeded the value of that parameter. Pagination continues from the end
    -- of the previous results that returned the @nextToken@ value.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results returned by @ListVirtualNodes@ in
    -- paginated output. When you use this parameter, @ListVirtualNodes@
    -- returns only @limit@ results in a single page along with a @nextToken@
    -- response element. You can see the remaining results of the initial
    -- request by sending another @ListVirtualNodes@ request with the returned
    -- @nextToken@ value. This value can be between 1 and 100. If you don\'t
    -- use this parameter, @ListVirtualNodes@ returns up to 100 results and a
    -- @nextToken@ value if applicable.
    limit :: Prelude.Maybe Prelude.Natural,
    -- | The name of the service mesh to list virtual nodes in.
    meshName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListVirtualNodes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'meshOwner', 'listVirtualNodes_meshOwner' - The AWS IAM account ID of the service mesh owner. If the account ID is
-- not your own, then it\'s the ID of the account that shared the mesh with
-- your account. For more information about mesh sharing, see
-- <https://docs.aws.amazon.com/app-mesh/latest/userguide/sharing.html Working with shared meshes>.
--
-- 'nextToken', 'listVirtualNodes_nextToken' - The @nextToken@ value returned from a previous paginated
-- @ListVirtualNodes@ request where @limit@ was used and the results
-- exceeded the value of that parameter. Pagination continues from the end
-- of the previous results that returned the @nextToken@ value.
--
-- 'limit', 'listVirtualNodes_limit' - The maximum number of results returned by @ListVirtualNodes@ in
-- paginated output. When you use this parameter, @ListVirtualNodes@
-- returns only @limit@ results in a single page along with a @nextToken@
-- response element. You can see the remaining results of the initial
-- request by sending another @ListVirtualNodes@ request with the returned
-- @nextToken@ value. This value can be between 1 and 100. If you don\'t
-- use this parameter, @ListVirtualNodes@ returns up to 100 results and a
-- @nextToken@ value if applicable.
--
-- 'meshName', 'listVirtualNodes_meshName' - The name of the service mesh to list virtual nodes in.
newListVirtualNodes ::
  -- | 'meshName'
  Prelude.Text ->
  ListVirtualNodes
newListVirtualNodes pMeshName_ =
  ListVirtualNodes'
    { meshOwner = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      limit = Prelude.Nothing,
      meshName = pMeshName_
    }

-- | The AWS IAM account ID of the service mesh owner. If the account ID is
-- not your own, then it\'s the ID of the account that shared the mesh with
-- your account. For more information about mesh sharing, see
-- <https://docs.aws.amazon.com/app-mesh/latest/userguide/sharing.html Working with shared meshes>.
listVirtualNodes_meshOwner :: Lens.Lens' ListVirtualNodes (Prelude.Maybe Prelude.Text)
listVirtualNodes_meshOwner = Lens.lens (\ListVirtualNodes' {meshOwner} -> meshOwner) (\s@ListVirtualNodes' {} a -> s {meshOwner = a} :: ListVirtualNodes)

-- | The @nextToken@ value returned from a previous paginated
-- @ListVirtualNodes@ request where @limit@ was used and the results
-- exceeded the value of that parameter. Pagination continues from the end
-- of the previous results that returned the @nextToken@ value.
listVirtualNodes_nextToken :: Lens.Lens' ListVirtualNodes (Prelude.Maybe Prelude.Text)
listVirtualNodes_nextToken = Lens.lens (\ListVirtualNodes' {nextToken} -> nextToken) (\s@ListVirtualNodes' {} a -> s {nextToken = a} :: ListVirtualNodes)

-- | The maximum number of results returned by @ListVirtualNodes@ in
-- paginated output. When you use this parameter, @ListVirtualNodes@
-- returns only @limit@ results in a single page along with a @nextToken@
-- response element. You can see the remaining results of the initial
-- request by sending another @ListVirtualNodes@ request with the returned
-- @nextToken@ value. This value can be between 1 and 100. If you don\'t
-- use this parameter, @ListVirtualNodes@ returns up to 100 results and a
-- @nextToken@ value if applicable.
listVirtualNodes_limit :: Lens.Lens' ListVirtualNodes (Prelude.Maybe Prelude.Natural)
listVirtualNodes_limit = Lens.lens (\ListVirtualNodes' {limit} -> limit) (\s@ListVirtualNodes' {} a -> s {limit = a} :: ListVirtualNodes)

-- | The name of the service mesh to list virtual nodes in.
listVirtualNodes_meshName :: Lens.Lens' ListVirtualNodes Prelude.Text
listVirtualNodes_meshName = Lens.lens (\ListVirtualNodes' {meshName} -> meshName) (\s@ListVirtualNodes' {} a -> s {meshName = a} :: ListVirtualNodes)

instance Core.AWSPager ListVirtualNodes where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listVirtualNodesResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        (rs Lens.^. listVirtualNodesResponse_virtualNodes) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listVirtualNodes_nextToken
          Lens..~ rs
          Lens.^? listVirtualNodesResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListVirtualNodes where
  type
    AWSResponse ListVirtualNodes =
      ListVirtualNodesResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListVirtualNodesResponse'
            Prelude.<$> (x Core..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..?> "virtualNodes" Core..!@ Prelude.mempty)
      )

instance Prelude.Hashable ListVirtualNodes

instance Prelude.NFData ListVirtualNodes

instance Core.ToHeaders ListVirtualNodes where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath ListVirtualNodes where
  toPath ListVirtualNodes' {..} =
    Prelude.mconcat
      [ "/v20190125/meshes/",
        Core.toBS meshName,
        "/virtualNodes"
      ]

instance Core.ToQuery ListVirtualNodes where
  toQuery ListVirtualNodes' {..} =
    Prelude.mconcat
      [ "meshOwner" Core.=: meshOwner,
        "nextToken" Core.=: nextToken,
        "limit" Core.=: limit
      ]

-- |
--
-- /See:/ 'newListVirtualNodesResponse' smart constructor.
data ListVirtualNodesResponse = ListVirtualNodesResponse'
  { -- | The @nextToken@ value to include in a future @ListVirtualNodes@ request.
    -- When the results of a @ListVirtualNodes@ request exceed @limit@, you can
    -- use this value to retrieve the next page of results. This value is
    -- @null@ when there are no more results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The list of existing virtual nodes for the specified service mesh.
    virtualNodes :: [VirtualNodeRef]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListVirtualNodesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listVirtualNodesResponse_nextToken' - The @nextToken@ value to include in a future @ListVirtualNodes@ request.
-- When the results of a @ListVirtualNodes@ request exceed @limit@, you can
-- use this value to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
--
-- 'httpStatus', 'listVirtualNodesResponse_httpStatus' - The response's http status code.
--
-- 'virtualNodes', 'listVirtualNodesResponse_virtualNodes' - The list of existing virtual nodes for the specified service mesh.
newListVirtualNodesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListVirtualNodesResponse
newListVirtualNodesResponse pHttpStatus_ =
  ListVirtualNodesResponse'
    { nextToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      virtualNodes = Prelude.mempty
    }

-- | The @nextToken@ value to include in a future @ListVirtualNodes@ request.
-- When the results of a @ListVirtualNodes@ request exceed @limit@, you can
-- use this value to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
listVirtualNodesResponse_nextToken :: Lens.Lens' ListVirtualNodesResponse (Prelude.Maybe Prelude.Text)
listVirtualNodesResponse_nextToken = Lens.lens (\ListVirtualNodesResponse' {nextToken} -> nextToken) (\s@ListVirtualNodesResponse' {} a -> s {nextToken = a} :: ListVirtualNodesResponse)

-- | The response's http status code.
listVirtualNodesResponse_httpStatus :: Lens.Lens' ListVirtualNodesResponse Prelude.Int
listVirtualNodesResponse_httpStatus = Lens.lens (\ListVirtualNodesResponse' {httpStatus} -> httpStatus) (\s@ListVirtualNodesResponse' {} a -> s {httpStatus = a} :: ListVirtualNodesResponse)

-- | The list of existing virtual nodes for the specified service mesh.
listVirtualNodesResponse_virtualNodes :: Lens.Lens' ListVirtualNodesResponse [VirtualNodeRef]
listVirtualNodesResponse_virtualNodes = Lens.lens (\ListVirtualNodesResponse' {virtualNodes} -> virtualNodes) (\s@ListVirtualNodesResponse' {} a -> s {virtualNodes = a} :: ListVirtualNodesResponse) Prelude.. Lens.coerced

instance Prelude.NFData ListVirtualNodesResponse
