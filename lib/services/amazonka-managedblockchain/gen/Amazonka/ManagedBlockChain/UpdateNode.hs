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
-- Module      : Amazonka.ManagedBlockChain.UpdateNode
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a node configuration with new parameters.
--
-- Applies only to Hyperledger Fabric.
module Amazonka.ManagedBlockChain.UpdateNode
  ( -- * Creating a Request
    UpdateNode (..),
    newUpdateNode,

    -- * Request Lenses
    updateNode_memberId,
    updateNode_logPublishingConfiguration,
    updateNode_networkId,
    updateNode_nodeId,

    -- * Destructuring the Response
    UpdateNodeResponse (..),
    newUpdateNodeResponse,

    -- * Response Lenses
    updateNodeResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ManagedBlockChain.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateNode' smart constructor.
data UpdateNode = UpdateNode'
  { -- | The unique identifier of the member that owns the node.
    --
    -- Applies only to Hyperledger Fabric.
    memberId :: Prelude.Maybe Prelude.Text,
    -- | Configuration properties for publishing to Amazon CloudWatch Logs.
    logPublishingConfiguration :: Prelude.Maybe NodeLogPublishingConfiguration,
    -- | The unique identifier of the network that the node is on.
    networkId :: Prelude.Text,
    -- | The unique identifier of the node.
    nodeId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateNode' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'memberId', 'updateNode_memberId' - The unique identifier of the member that owns the node.
--
-- Applies only to Hyperledger Fabric.
--
-- 'logPublishingConfiguration', 'updateNode_logPublishingConfiguration' - Configuration properties for publishing to Amazon CloudWatch Logs.
--
-- 'networkId', 'updateNode_networkId' - The unique identifier of the network that the node is on.
--
-- 'nodeId', 'updateNode_nodeId' - The unique identifier of the node.
newUpdateNode ::
  -- | 'networkId'
  Prelude.Text ->
  -- | 'nodeId'
  Prelude.Text ->
  UpdateNode
newUpdateNode pNetworkId_ pNodeId_ =
  UpdateNode'
    { memberId = Prelude.Nothing,
      logPublishingConfiguration = Prelude.Nothing,
      networkId = pNetworkId_,
      nodeId = pNodeId_
    }

-- | The unique identifier of the member that owns the node.
--
-- Applies only to Hyperledger Fabric.
updateNode_memberId :: Lens.Lens' UpdateNode (Prelude.Maybe Prelude.Text)
updateNode_memberId = Lens.lens (\UpdateNode' {memberId} -> memberId) (\s@UpdateNode' {} a -> s {memberId = a} :: UpdateNode)

-- | Configuration properties for publishing to Amazon CloudWatch Logs.
updateNode_logPublishingConfiguration :: Lens.Lens' UpdateNode (Prelude.Maybe NodeLogPublishingConfiguration)
updateNode_logPublishingConfiguration = Lens.lens (\UpdateNode' {logPublishingConfiguration} -> logPublishingConfiguration) (\s@UpdateNode' {} a -> s {logPublishingConfiguration = a} :: UpdateNode)

-- | The unique identifier of the network that the node is on.
updateNode_networkId :: Lens.Lens' UpdateNode Prelude.Text
updateNode_networkId = Lens.lens (\UpdateNode' {networkId} -> networkId) (\s@UpdateNode' {} a -> s {networkId = a} :: UpdateNode)

-- | The unique identifier of the node.
updateNode_nodeId :: Lens.Lens' UpdateNode Prelude.Text
updateNode_nodeId = Lens.lens (\UpdateNode' {nodeId} -> nodeId) (\s@UpdateNode' {} a -> s {nodeId = a} :: UpdateNode)

instance Core.AWSRequest UpdateNode where
  type AWSResponse UpdateNode = UpdateNodeResponse
  request overrides =
    Request.patchJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateNodeResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateNode where
  hashWithSalt _salt UpdateNode' {..} =
    _salt `Prelude.hashWithSalt` memberId
      `Prelude.hashWithSalt` logPublishingConfiguration
      `Prelude.hashWithSalt` networkId
      `Prelude.hashWithSalt` nodeId

instance Prelude.NFData UpdateNode where
  rnf UpdateNode' {..} =
    Prelude.rnf memberId
      `Prelude.seq` Prelude.rnf logPublishingConfiguration
      `Prelude.seq` Prelude.rnf networkId
      `Prelude.seq` Prelude.rnf nodeId

instance Data.ToHeaders UpdateNode where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateNode where
  toJSON UpdateNode' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MemberId" Data..=) Prelude.<$> memberId,
            ("LogPublishingConfiguration" Data..=)
              Prelude.<$> logPublishingConfiguration
          ]
      )

instance Data.ToPath UpdateNode where
  toPath UpdateNode' {..} =
    Prelude.mconcat
      [ "/networks/",
        Data.toBS networkId,
        "/nodes/",
        Data.toBS nodeId
      ]

instance Data.ToQuery UpdateNode where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateNodeResponse' smart constructor.
data UpdateNodeResponse = UpdateNodeResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateNodeResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateNodeResponse_httpStatus' - The response's http status code.
newUpdateNodeResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateNodeResponse
newUpdateNodeResponse pHttpStatus_ =
  UpdateNodeResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
updateNodeResponse_httpStatus :: Lens.Lens' UpdateNodeResponse Prelude.Int
updateNodeResponse_httpStatus = Lens.lens (\UpdateNodeResponse' {httpStatus} -> httpStatus) (\s@UpdateNodeResponse' {} a -> s {httpStatus = a} :: UpdateNodeResponse)

instance Prelude.NFData UpdateNodeResponse where
  rnf UpdateNodeResponse' {..} = Prelude.rnf httpStatus
