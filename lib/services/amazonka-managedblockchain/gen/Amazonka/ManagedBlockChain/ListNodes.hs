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
-- Module      : Amazonka.ManagedBlockChain.ListNodes
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the nodes within a network.
--
-- Applies to Hyperledger Fabric and Ethereum.
module Amazonka.ManagedBlockChain.ListNodes
  ( -- * Creating a Request
    ListNodes (..),
    newListNodes,

    -- * Request Lenses
    listNodes_maxResults,
    listNodes_memberId,
    listNodes_nextToken,
    listNodes_status,
    listNodes_networkId,

    -- * Destructuring the Response
    ListNodesResponse (..),
    newListNodesResponse,

    -- * Response Lenses
    listNodesResponse_nextToken,
    listNodesResponse_nodes,
    listNodesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ManagedBlockChain.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListNodes' smart constructor.
data ListNodes = ListNodes'
  { -- | The maximum number of nodes to list.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The unique identifier of the member who owns the nodes to list.
    --
    -- Applies only to Hyperledger Fabric and is required for Hyperledger
    -- Fabric.
    memberId :: Prelude.Maybe Prelude.Text,
    -- | The pagination token that indicates the next set of results to retrieve.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | An optional status specifier. If provided, only nodes currently in this
    -- status are listed.
    status :: Prelude.Maybe NodeStatus,
    -- | The unique identifier of the network for which to list nodes.
    networkId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListNodes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listNodes_maxResults' - The maximum number of nodes to list.
--
-- 'memberId', 'listNodes_memberId' - The unique identifier of the member who owns the nodes to list.
--
-- Applies only to Hyperledger Fabric and is required for Hyperledger
-- Fabric.
--
-- 'nextToken', 'listNodes_nextToken' - The pagination token that indicates the next set of results to retrieve.
--
-- 'status', 'listNodes_status' - An optional status specifier. If provided, only nodes currently in this
-- status are listed.
--
-- 'networkId', 'listNodes_networkId' - The unique identifier of the network for which to list nodes.
newListNodes ::
  -- | 'networkId'
  Prelude.Text ->
  ListNodes
newListNodes pNetworkId_ =
  ListNodes'
    { maxResults = Prelude.Nothing,
      memberId = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      status = Prelude.Nothing,
      networkId = pNetworkId_
    }

-- | The maximum number of nodes to list.
listNodes_maxResults :: Lens.Lens' ListNodes (Prelude.Maybe Prelude.Natural)
listNodes_maxResults = Lens.lens (\ListNodes' {maxResults} -> maxResults) (\s@ListNodes' {} a -> s {maxResults = a} :: ListNodes)

-- | The unique identifier of the member who owns the nodes to list.
--
-- Applies only to Hyperledger Fabric and is required for Hyperledger
-- Fabric.
listNodes_memberId :: Lens.Lens' ListNodes (Prelude.Maybe Prelude.Text)
listNodes_memberId = Lens.lens (\ListNodes' {memberId} -> memberId) (\s@ListNodes' {} a -> s {memberId = a} :: ListNodes)

-- | The pagination token that indicates the next set of results to retrieve.
listNodes_nextToken :: Lens.Lens' ListNodes (Prelude.Maybe Prelude.Text)
listNodes_nextToken = Lens.lens (\ListNodes' {nextToken} -> nextToken) (\s@ListNodes' {} a -> s {nextToken = a} :: ListNodes)

-- | An optional status specifier. If provided, only nodes currently in this
-- status are listed.
listNodes_status :: Lens.Lens' ListNodes (Prelude.Maybe NodeStatus)
listNodes_status = Lens.lens (\ListNodes' {status} -> status) (\s@ListNodes' {} a -> s {status = a} :: ListNodes)

-- | The unique identifier of the network for which to list nodes.
listNodes_networkId :: Lens.Lens' ListNodes Prelude.Text
listNodes_networkId = Lens.lens (\ListNodes' {networkId} -> networkId) (\s@ListNodes' {} a -> s {networkId = a} :: ListNodes)

instance Core.AWSRequest ListNodes where
  type AWSResponse ListNodes = ListNodesResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListNodesResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "Nodes" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListNodes where
  hashWithSalt _salt ListNodes' {..} =
    _salt `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` memberId
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` networkId

instance Prelude.NFData ListNodes where
  rnf ListNodes' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf memberId
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf networkId

instance Data.ToHeaders ListNodes where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListNodes where
  toPath ListNodes' {..} =
    Prelude.mconcat
      ["/networks/", Data.toBS networkId, "/nodes"]

instance Data.ToQuery ListNodes where
  toQuery ListNodes' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "memberId" Data.=: memberId,
        "nextToken" Data.=: nextToken,
        "status" Data.=: status
      ]

-- | /See:/ 'newListNodesResponse' smart constructor.
data ListNodesResponse = ListNodesResponse'
  { -- | The pagination token that indicates the next set of results to retrieve.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | An array of @NodeSummary@ objects that contain configuration properties
    -- for each node.
    nodes :: Prelude.Maybe [NodeSummary],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListNodesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listNodesResponse_nextToken' - The pagination token that indicates the next set of results to retrieve.
--
-- 'nodes', 'listNodesResponse_nodes' - An array of @NodeSummary@ objects that contain configuration properties
-- for each node.
--
-- 'httpStatus', 'listNodesResponse_httpStatus' - The response's http status code.
newListNodesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListNodesResponse
newListNodesResponse pHttpStatus_ =
  ListNodesResponse'
    { nextToken = Prelude.Nothing,
      nodes = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The pagination token that indicates the next set of results to retrieve.
listNodesResponse_nextToken :: Lens.Lens' ListNodesResponse (Prelude.Maybe Prelude.Text)
listNodesResponse_nextToken = Lens.lens (\ListNodesResponse' {nextToken} -> nextToken) (\s@ListNodesResponse' {} a -> s {nextToken = a} :: ListNodesResponse)

-- | An array of @NodeSummary@ objects that contain configuration properties
-- for each node.
listNodesResponse_nodes :: Lens.Lens' ListNodesResponse (Prelude.Maybe [NodeSummary])
listNodesResponse_nodes = Lens.lens (\ListNodesResponse' {nodes} -> nodes) (\s@ListNodesResponse' {} a -> s {nodes = a} :: ListNodesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listNodesResponse_httpStatus :: Lens.Lens' ListNodesResponse Prelude.Int
listNodesResponse_httpStatus = Lens.lens (\ListNodesResponse' {httpStatus} -> httpStatus) (\s@ListNodesResponse' {} a -> s {httpStatus = a} :: ListNodesResponse)

instance Prelude.NFData ListNodesResponse where
  rnf ListNodesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf nodes
      `Prelude.seq` Prelude.rnf httpStatus
