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
-- Module      : Amazonka.ManagedBlockChain.GetNode
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns detailed information about a node.
--
-- Applies to Hyperledger Fabric and Ethereum.
module Amazonka.ManagedBlockChain.GetNode
  ( -- * Creating a Request
    GetNode (..),
    newGetNode,

    -- * Request Lenses
    getNode_memberId,
    getNode_networkId,
    getNode_nodeId,

    -- * Destructuring the Response
    GetNodeResponse (..),
    newGetNodeResponse,

    -- * Response Lenses
    getNodeResponse_node,
    getNodeResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ManagedBlockChain.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetNode' smart constructor.
data GetNode = GetNode'
  { -- | The unique identifier of the member that owns the node.
    --
    -- Applies only to Hyperledger Fabric and is required for Hyperledger
    -- Fabric.
    memberId :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier of the network that the node is on.
    networkId :: Prelude.Text,
    -- | The unique identifier of the node.
    nodeId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetNode' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'memberId', 'getNode_memberId' - The unique identifier of the member that owns the node.
--
-- Applies only to Hyperledger Fabric and is required for Hyperledger
-- Fabric.
--
-- 'networkId', 'getNode_networkId' - The unique identifier of the network that the node is on.
--
-- 'nodeId', 'getNode_nodeId' - The unique identifier of the node.
newGetNode ::
  -- | 'networkId'
  Prelude.Text ->
  -- | 'nodeId'
  Prelude.Text ->
  GetNode
newGetNode pNetworkId_ pNodeId_ =
  GetNode'
    { memberId = Prelude.Nothing,
      networkId = pNetworkId_,
      nodeId = pNodeId_
    }

-- | The unique identifier of the member that owns the node.
--
-- Applies only to Hyperledger Fabric and is required for Hyperledger
-- Fabric.
getNode_memberId :: Lens.Lens' GetNode (Prelude.Maybe Prelude.Text)
getNode_memberId = Lens.lens (\GetNode' {memberId} -> memberId) (\s@GetNode' {} a -> s {memberId = a} :: GetNode)

-- | The unique identifier of the network that the node is on.
getNode_networkId :: Lens.Lens' GetNode Prelude.Text
getNode_networkId = Lens.lens (\GetNode' {networkId} -> networkId) (\s@GetNode' {} a -> s {networkId = a} :: GetNode)

-- | The unique identifier of the node.
getNode_nodeId :: Lens.Lens' GetNode Prelude.Text
getNode_nodeId = Lens.lens (\GetNode' {nodeId} -> nodeId) (\s@GetNode' {} a -> s {nodeId = a} :: GetNode)

instance Core.AWSRequest GetNode where
  type AWSResponse GetNode = GetNodeResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetNodeResponse'
            Prelude.<$> (x Data..?> "Node")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetNode where
  hashWithSalt _salt GetNode' {..} =
    _salt
      `Prelude.hashWithSalt` memberId
      `Prelude.hashWithSalt` networkId
      `Prelude.hashWithSalt` nodeId

instance Prelude.NFData GetNode where
  rnf GetNode' {..} =
    Prelude.rnf memberId
      `Prelude.seq` Prelude.rnf networkId
      `Prelude.seq` Prelude.rnf nodeId

instance Data.ToHeaders GetNode where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetNode where
  toPath GetNode' {..} =
    Prelude.mconcat
      [ "/networks/",
        Data.toBS networkId,
        "/nodes/",
        Data.toBS nodeId
      ]

instance Data.ToQuery GetNode where
  toQuery GetNode' {..} =
    Prelude.mconcat ["memberId" Data.=: memberId]

-- | /See:/ 'newGetNodeResponse' smart constructor.
data GetNodeResponse = GetNodeResponse'
  { -- | Properties of the node configuration.
    node :: Prelude.Maybe Node,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetNodeResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'node', 'getNodeResponse_node' - Properties of the node configuration.
--
-- 'httpStatus', 'getNodeResponse_httpStatus' - The response's http status code.
newGetNodeResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetNodeResponse
newGetNodeResponse pHttpStatus_ =
  GetNodeResponse'
    { node = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Properties of the node configuration.
getNodeResponse_node :: Lens.Lens' GetNodeResponse (Prelude.Maybe Node)
getNodeResponse_node = Lens.lens (\GetNodeResponse' {node} -> node) (\s@GetNodeResponse' {} a -> s {node = a} :: GetNodeResponse)

-- | The response's http status code.
getNodeResponse_httpStatus :: Lens.Lens' GetNodeResponse Prelude.Int
getNodeResponse_httpStatus = Lens.lens (\GetNodeResponse' {httpStatus} -> httpStatus) (\s@GetNodeResponse' {} a -> s {httpStatus = a} :: GetNodeResponse)

instance Prelude.NFData GetNodeResponse where
  rnf GetNodeResponse' {..} =
    Prelude.rnf node
      `Prelude.seq` Prelude.rnf httpStatus
