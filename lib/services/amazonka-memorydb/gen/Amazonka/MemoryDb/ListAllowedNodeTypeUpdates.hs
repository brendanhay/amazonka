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
-- Module      : Amazonka.MemoryDb.ListAllowedNodeTypeUpdates
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all available node types that you can scale to from your
-- cluster\'s current node type. When you use the UpdateCluster operation
-- to scale your cluster, the value of the NodeType parameter must be one
-- of the node types returned by this operation.
module Amazonka.MemoryDb.ListAllowedNodeTypeUpdates
  ( -- * Creating a Request
    ListAllowedNodeTypeUpdates (..),
    newListAllowedNodeTypeUpdates,

    -- * Request Lenses
    listAllowedNodeTypeUpdates_clusterName,

    -- * Destructuring the Response
    ListAllowedNodeTypeUpdatesResponse (..),
    newListAllowedNodeTypeUpdatesResponse,

    -- * Response Lenses
    listAllowedNodeTypeUpdatesResponse_scaleUpNodeTypes,
    listAllowedNodeTypeUpdatesResponse_scaleDownNodeTypes,
    listAllowedNodeTypeUpdatesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MemoryDb.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListAllowedNodeTypeUpdates' smart constructor.
data ListAllowedNodeTypeUpdates = ListAllowedNodeTypeUpdates'
  { -- | The name of the cluster you want to scale. MemoryDB uses the cluster
    -- name to identify the current node type being used by this cluster, and
    -- from that to create a list of node types you can scale up to.
    clusterName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListAllowedNodeTypeUpdates' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clusterName', 'listAllowedNodeTypeUpdates_clusterName' - The name of the cluster you want to scale. MemoryDB uses the cluster
-- name to identify the current node type being used by this cluster, and
-- from that to create a list of node types you can scale up to.
newListAllowedNodeTypeUpdates ::
  -- | 'clusterName'
  Prelude.Text ->
  ListAllowedNodeTypeUpdates
newListAllowedNodeTypeUpdates pClusterName_ =
  ListAllowedNodeTypeUpdates'
    { clusterName =
        pClusterName_
    }

-- | The name of the cluster you want to scale. MemoryDB uses the cluster
-- name to identify the current node type being used by this cluster, and
-- from that to create a list of node types you can scale up to.
listAllowedNodeTypeUpdates_clusterName :: Lens.Lens' ListAllowedNodeTypeUpdates Prelude.Text
listAllowedNodeTypeUpdates_clusterName = Lens.lens (\ListAllowedNodeTypeUpdates' {clusterName} -> clusterName) (\s@ListAllowedNodeTypeUpdates' {} a -> s {clusterName = a} :: ListAllowedNodeTypeUpdates)

instance Core.AWSRequest ListAllowedNodeTypeUpdates where
  type
    AWSResponse ListAllowedNodeTypeUpdates =
      ListAllowedNodeTypeUpdatesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListAllowedNodeTypeUpdatesResponse'
            Prelude.<$> ( x Data..?> "ScaleUpNodeTypes"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> ( x Data..?> "ScaleDownNodeTypes"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListAllowedNodeTypeUpdates where
  hashWithSalt _salt ListAllowedNodeTypeUpdates' {..} =
    _salt `Prelude.hashWithSalt` clusterName

instance Prelude.NFData ListAllowedNodeTypeUpdates where
  rnf ListAllowedNodeTypeUpdates' {..} =
    Prelude.rnf clusterName

instance Data.ToHeaders ListAllowedNodeTypeUpdates where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonMemoryDB.ListAllowedNodeTypeUpdates" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListAllowedNodeTypeUpdates where
  toJSON ListAllowedNodeTypeUpdates' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("ClusterName" Data..= clusterName)]
      )

instance Data.ToPath ListAllowedNodeTypeUpdates where
  toPath = Prelude.const "/"

instance Data.ToQuery ListAllowedNodeTypeUpdates where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListAllowedNodeTypeUpdatesResponse' smart constructor.
data ListAllowedNodeTypeUpdatesResponse = ListAllowedNodeTypeUpdatesResponse'
  { -- | A list node types which you can use to scale up your cluster.
    scaleUpNodeTypes :: Prelude.Maybe [Prelude.Text],
    -- | A list node types which you can use to scale down your cluster.
    scaleDownNodeTypes :: Prelude.Maybe [Prelude.Text],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListAllowedNodeTypeUpdatesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'scaleUpNodeTypes', 'listAllowedNodeTypeUpdatesResponse_scaleUpNodeTypes' - A list node types which you can use to scale up your cluster.
--
-- 'scaleDownNodeTypes', 'listAllowedNodeTypeUpdatesResponse_scaleDownNodeTypes' - A list node types which you can use to scale down your cluster.
--
-- 'httpStatus', 'listAllowedNodeTypeUpdatesResponse_httpStatus' - The response's http status code.
newListAllowedNodeTypeUpdatesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListAllowedNodeTypeUpdatesResponse
newListAllowedNodeTypeUpdatesResponse pHttpStatus_ =
  ListAllowedNodeTypeUpdatesResponse'
    { scaleUpNodeTypes =
        Prelude.Nothing,
      scaleDownNodeTypes = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list node types which you can use to scale up your cluster.
listAllowedNodeTypeUpdatesResponse_scaleUpNodeTypes :: Lens.Lens' ListAllowedNodeTypeUpdatesResponse (Prelude.Maybe [Prelude.Text])
listAllowedNodeTypeUpdatesResponse_scaleUpNodeTypes = Lens.lens (\ListAllowedNodeTypeUpdatesResponse' {scaleUpNodeTypes} -> scaleUpNodeTypes) (\s@ListAllowedNodeTypeUpdatesResponse' {} a -> s {scaleUpNodeTypes = a} :: ListAllowedNodeTypeUpdatesResponse) Prelude.. Lens.mapping Lens.coerced

-- | A list node types which you can use to scale down your cluster.
listAllowedNodeTypeUpdatesResponse_scaleDownNodeTypes :: Lens.Lens' ListAllowedNodeTypeUpdatesResponse (Prelude.Maybe [Prelude.Text])
listAllowedNodeTypeUpdatesResponse_scaleDownNodeTypes = Lens.lens (\ListAllowedNodeTypeUpdatesResponse' {scaleDownNodeTypes} -> scaleDownNodeTypes) (\s@ListAllowedNodeTypeUpdatesResponse' {} a -> s {scaleDownNodeTypes = a} :: ListAllowedNodeTypeUpdatesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listAllowedNodeTypeUpdatesResponse_httpStatus :: Lens.Lens' ListAllowedNodeTypeUpdatesResponse Prelude.Int
listAllowedNodeTypeUpdatesResponse_httpStatus = Lens.lens (\ListAllowedNodeTypeUpdatesResponse' {httpStatus} -> httpStatus) (\s@ListAllowedNodeTypeUpdatesResponse' {} a -> s {httpStatus = a} :: ListAllowedNodeTypeUpdatesResponse)

instance
  Prelude.NFData
    ListAllowedNodeTypeUpdatesResponse
  where
  rnf ListAllowedNodeTypeUpdatesResponse' {..} =
    Prelude.rnf scaleUpNodeTypes
      `Prelude.seq` Prelude.rnf scaleDownNodeTypes
      `Prelude.seq` Prelude.rnf httpStatus
