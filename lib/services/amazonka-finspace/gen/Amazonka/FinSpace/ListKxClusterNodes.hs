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
-- Module      : Amazonka.FinSpace.ListKxClusterNodes
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all the nodes in a kdb cluster.
module Amazonka.FinSpace.ListKxClusterNodes
  ( -- * Creating a Request
    ListKxClusterNodes (..),
    newListKxClusterNodes,

    -- * Request Lenses
    listKxClusterNodes_maxResults,
    listKxClusterNodes_nextToken,
    listKxClusterNodes_clusterName,
    listKxClusterNodes_environmentId,

    -- * Destructuring the Response
    ListKxClusterNodesResponse (..),
    newListKxClusterNodesResponse,

    -- * Response Lenses
    listKxClusterNodesResponse_nextToken,
    listKxClusterNodesResponse_nodes,
    listKxClusterNodesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FinSpace.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListKxClusterNodes' smart constructor.
data ListKxClusterNodes = ListKxClusterNodes'
  { -- | The maximum number of results to return in this request.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | A token that indicates where a results page should begin.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A unique name for the cluster.
    clusterName :: Prelude.Text,
    -- | A unique identifier for the kdb environment.
    environmentId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListKxClusterNodes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listKxClusterNodes_maxResults' - The maximum number of results to return in this request.
--
-- 'nextToken', 'listKxClusterNodes_nextToken' - A token that indicates where a results page should begin.
--
-- 'clusterName', 'listKxClusterNodes_clusterName' - A unique name for the cluster.
--
-- 'environmentId', 'listKxClusterNodes_environmentId' - A unique identifier for the kdb environment.
newListKxClusterNodes ::
  -- | 'clusterName'
  Prelude.Text ->
  -- | 'environmentId'
  Prelude.Text ->
  ListKxClusterNodes
newListKxClusterNodes pClusterName_ pEnvironmentId_ =
  ListKxClusterNodes'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      clusterName = pClusterName_,
      environmentId = pEnvironmentId_
    }

-- | The maximum number of results to return in this request.
listKxClusterNodes_maxResults :: Lens.Lens' ListKxClusterNodes (Prelude.Maybe Prelude.Natural)
listKxClusterNodes_maxResults = Lens.lens (\ListKxClusterNodes' {maxResults} -> maxResults) (\s@ListKxClusterNodes' {} a -> s {maxResults = a} :: ListKxClusterNodes)

-- | A token that indicates where a results page should begin.
listKxClusterNodes_nextToken :: Lens.Lens' ListKxClusterNodes (Prelude.Maybe Prelude.Text)
listKxClusterNodes_nextToken = Lens.lens (\ListKxClusterNodes' {nextToken} -> nextToken) (\s@ListKxClusterNodes' {} a -> s {nextToken = a} :: ListKxClusterNodes)

-- | A unique name for the cluster.
listKxClusterNodes_clusterName :: Lens.Lens' ListKxClusterNodes Prelude.Text
listKxClusterNodes_clusterName = Lens.lens (\ListKxClusterNodes' {clusterName} -> clusterName) (\s@ListKxClusterNodes' {} a -> s {clusterName = a} :: ListKxClusterNodes)

-- | A unique identifier for the kdb environment.
listKxClusterNodes_environmentId :: Lens.Lens' ListKxClusterNodes Prelude.Text
listKxClusterNodes_environmentId = Lens.lens (\ListKxClusterNodes' {environmentId} -> environmentId) (\s@ListKxClusterNodes' {} a -> s {environmentId = a} :: ListKxClusterNodes)

instance Core.AWSRequest ListKxClusterNodes where
  type
    AWSResponse ListKxClusterNodes =
      ListKxClusterNodesResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListKxClusterNodesResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (x Data..?> "nodes" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListKxClusterNodes where
  hashWithSalt _salt ListKxClusterNodes' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` clusterName
      `Prelude.hashWithSalt` environmentId

instance Prelude.NFData ListKxClusterNodes where
  rnf ListKxClusterNodes' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf clusterName
      `Prelude.seq` Prelude.rnf environmentId

instance Data.ToHeaders ListKxClusterNodes where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListKxClusterNodes where
  toPath ListKxClusterNodes' {..} =
    Prelude.mconcat
      [ "/kx/environments/",
        Data.toBS environmentId,
        "/clusters/",
        Data.toBS clusterName,
        "/nodes"
      ]

instance Data.ToQuery ListKxClusterNodes where
  toQuery ListKxClusterNodes' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken
      ]

-- | /See:/ 'newListKxClusterNodesResponse' smart constructor.
data ListKxClusterNodesResponse = ListKxClusterNodesResponse'
  { -- | A token that indicates where a results page should begin.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of nodes associated with the cluster.
    nodes :: Prelude.Maybe [KxNode],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListKxClusterNodesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listKxClusterNodesResponse_nextToken' - A token that indicates where a results page should begin.
--
-- 'nodes', 'listKxClusterNodesResponse_nodes' - A list of nodes associated with the cluster.
--
-- 'httpStatus', 'listKxClusterNodesResponse_httpStatus' - The response's http status code.
newListKxClusterNodesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListKxClusterNodesResponse
newListKxClusterNodesResponse pHttpStatus_ =
  ListKxClusterNodesResponse'
    { nextToken =
        Prelude.Nothing,
      nodes = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A token that indicates where a results page should begin.
listKxClusterNodesResponse_nextToken :: Lens.Lens' ListKxClusterNodesResponse (Prelude.Maybe Prelude.Text)
listKxClusterNodesResponse_nextToken = Lens.lens (\ListKxClusterNodesResponse' {nextToken} -> nextToken) (\s@ListKxClusterNodesResponse' {} a -> s {nextToken = a} :: ListKxClusterNodesResponse)

-- | A list of nodes associated with the cluster.
listKxClusterNodesResponse_nodes :: Lens.Lens' ListKxClusterNodesResponse (Prelude.Maybe [KxNode])
listKxClusterNodesResponse_nodes = Lens.lens (\ListKxClusterNodesResponse' {nodes} -> nodes) (\s@ListKxClusterNodesResponse' {} a -> s {nodes = a} :: ListKxClusterNodesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listKxClusterNodesResponse_httpStatus :: Lens.Lens' ListKxClusterNodesResponse Prelude.Int
listKxClusterNodesResponse_httpStatus = Lens.lens (\ListKxClusterNodesResponse' {httpStatus} -> httpStatus) (\s@ListKxClusterNodesResponse' {} a -> s {httpStatus = a} :: ListKxClusterNodesResponse)

instance Prelude.NFData ListKxClusterNodesResponse where
  rnf ListKxClusterNodesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf nodes
      `Prelude.seq` Prelude.rnf httpStatus
