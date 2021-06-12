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
-- Module      : Network.AWS.DAX.RebootNode
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Reboots a single node of a DAX cluster. The reboot action takes place as
-- soon as possible. During the reboot, the node status is set to
-- REBOOTING.
--
-- @RebootNode@ restarts the DAX engine process and does not remove the
-- contents of the cache.
module Network.AWS.DAX.RebootNode
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

import qualified Network.AWS.Core as Core
import Network.AWS.DAX.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newRebootNode' smart constructor.
data RebootNode = RebootNode'
  { -- | The name of the DAX cluster containing the node to be rebooted.
    clusterName :: Core.Text,
    -- | The system-assigned ID of the node to be rebooted.
    nodeId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  -- | 'nodeId'
  Core.Text ->
  RebootNode
newRebootNode pClusterName_ pNodeId_ =
  RebootNode'
    { clusterName = pClusterName_,
      nodeId = pNodeId_
    }

-- | The name of the DAX cluster containing the node to be rebooted.
rebootNode_clusterName :: Lens.Lens' RebootNode Core.Text
rebootNode_clusterName = Lens.lens (\RebootNode' {clusterName} -> clusterName) (\s@RebootNode' {} a -> s {clusterName = a} :: RebootNode)

-- | The system-assigned ID of the node to be rebooted.
rebootNode_nodeId :: Lens.Lens' RebootNode Core.Text
rebootNode_nodeId = Lens.lens (\RebootNode' {nodeId} -> nodeId) (\s@RebootNode' {} a -> s {nodeId = a} :: RebootNode)

instance Core.AWSRequest RebootNode where
  type AWSResponse RebootNode = RebootNodeResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          RebootNodeResponse'
            Core.<$> (x Core..?> "Cluster")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable RebootNode

instance Core.NFData RebootNode

instance Core.ToHeaders RebootNode where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("AmazonDAXV3.RebootNode" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON RebootNode where
  toJSON RebootNode' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("ClusterName" Core..= clusterName),
            Core.Just ("NodeId" Core..= nodeId)
          ]
      )

instance Core.ToPath RebootNode where
  toPath = Core.const "/"

instance Core.ToQuery RebootNode where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newRebootNodeResponse' smart constructor.
data RebootNodeResponse = RebootNodeResponse'
  { -- | A description of the DAX cluster after a node has been rebooted.
    cluster :: Core.Maybe Cluster,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  RebootNodeResponse
newRebootNodeResponse pHttpStatus_ =
  RebootNodeResponse'
    { cluster = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A description of the DAX cluster after a node has been rebooted.
rebootNodeResponse_cluster :: Lens.Lens' RebootNodeResponse (Core.Maybe Cluster)
rebootNodeResponse_cluster = Lens.lens (\RebootNodeResponse' {cluster} -> cluster) (\s@RebootNodeResponse' {} a -> s {cluster = a} :: RebootNodeResponse)

-- | The response's http status code.
rebootNodeResponse_httpStatus :: Lens.Lens' RebootNodeResponse Core.Int
rebootNodeResponse_httpStatus = Lens.lens (\RebootNodeResponse' {httpStatus} -> httpStatus) (\s@RebootNodeResponse' {} a -> s {httpStatus = a} :: RebootNodeResponse)

instance Core.NFData RebootNodeResponse
