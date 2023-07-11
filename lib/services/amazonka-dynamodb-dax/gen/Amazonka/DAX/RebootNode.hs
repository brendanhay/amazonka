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
-- Module      : Amazonka.DAX.RebootNode
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Reboots a single node of a DAX cluster. The reboot action takes place as
-- soon as possible. During the reboot, the node status is set to
-- REBOOTING.
--
-- @RebootNode@ restarts the DAX engine process and does not remove the
-- contents of the cache.
module Amazonka.DAX.RebootNode
  ( -- * Creating a Request
    RebootNode (..),
    newRebootNode,

    -- * Request Lenses
    rebootNode_clusterName,
    rebootNode_nodeId,

    -- * Destructuring the Response
    RebootNodeResponse (..),
    newRebootNodeResponse,

    -- * Response Lenses
    rebootNodeResponse_cluster,
    rebootNodeResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.DAX.Types
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newRebootNode' smart constructor.
data RebootNode = RebootNode'
  { -- | The name of the DAX cluster containing the node to be rebooted.
    clusterName :: Prelude.Text,
    -- | The system-assigned ID of the node to be rebooted.
    nodeId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RebootNode' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clusterName', 'rebootNode_clusterName' - The name of the DAX cluster containing the node to be rebooted.
--
-- 'nodeId', 'rebootNode_nodeId' - The system-assigned ID of the node to be rebooted.
newRebootNode ::
  -- | 'clusterName'
  Prelude.Text ->
  -- | 'nodeId'
  Prelude.Text ->
  RebootNode
newRebootNode pClusterName_ pNodeId_ =
  RebootNode'
    { clusterName = pClusterName_,
      nodeId = pNodeId_
    }

-- | The name of the DAX cluster containing the node to be rebooted.
rebootNode_clusterName :: Lens.Lens' RebootNode Prelude.Text
rebootNode_clusterName = Lens.lens (\RebootNode' {clusterName} -> clusterName) (\s@RebootNode' {} a -> s {clusterName = a} :: RebootNode)

-- | The system-assigned ID of the node to be rebooted.
rebootNode_nodeId :: Lens.Lens' RebootNode Prelude.Text
rebootNode_nodeId = Lens.lens (\RebootNode' {nodeId} -> nodeId) (\s@RebootNode' {} a -> s {nodeId = a} :: RebootNode)

instance Core.AWSRequest RebootNode where
  type AWSResponse RebootNode = RebootNodeResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          RebootNodeResponse'
            Prelude.<$> (x Data..?> "Cluster")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable RebootNode where
  hashWithSalt _salt RebootNode' {..} =
    _salt
      `Prelude.hashWithSalt` clusterName
      `Prelude.hashWithSalt` nodeId

instance Prelude.NFData RebootNode where
  rnf RebootNode' {..} =
    Prelude.rnf clusterName
      `Prelude.seq` Prelude.rnf nodeId

instance Data.ToHeaders RebootNode where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("AmazonDAXV3.RebootNode" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON RebootNode where
  toJSON RebootNode' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("ClusterName" Data..= clusterName),
            Prelude.Just ("NodeId" Data..= nodeId)
          ]
      )

instance Data.ToPath RebootNode where
  toPath = Prelude.const "/"

instance Data.ToQuery RebootNode where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newRebootNodeResponse' smart constructor.
data RebootNodeResponse = RebootNodeResponse'
  { -- | A description of the DAX cluster after a node has been rebooted.
    cluster :: Prelude.Maybe Cluster,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RebootNodeResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cluster', 'rebootNodeResponse_cluster' - A description of the DAX cluster after a node has been rebooted.
--
-- 'httpStatus', 'rebootNodeResponse_httpStatus' - The response's http status code.
newRebootNodeResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  RebootNodeResponse
newRebootNodeResponse pHttpStatus_ =
  RebootNodeResponse'
    { cluster = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A description of the DAX cluster after a node has been rebooted.
rebootNodeResponse_cluster :: Lens.Lens' RebootNodeResponse (Prelude.Maybe Cluster)
rebootNodeResponse_cluster = Lens.lens (\RebootNodeResponse' {cluster} -> cluster) (\s@RebootNodeResponse' {} a -> s {cluster = a} :: RebootNodeResponse)

-- | The response's http status code.
rebootNodeResponse_httpStatus :: Lens.Lens' RebootNodeResponse Prelude.Int
rebootNodeResponse_httpStatus = Lens.lens (\RebootNodeResponse' {httpStatus} -> httpStatus) (\s@RebootNodeResponse' {} a -> s {httpStatus = a} :: RebootNodeResponse)

instance Prelude.NFData RebootNodeResponse where
  rnf RebootNodeResponse' {..} =
    Prelude.rnf cluster
      `Prelude.seq` Prelude.rnf httpStatus
