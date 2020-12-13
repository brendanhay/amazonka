{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DAX.RebootNode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Reboots a single node of a DAX cluster. The reboot action takes place as soon as possible. During the reboot, the node status is set to REBOOTING.
module Network.AWS.DAX.RebootNode
  ( -- * Creating a request
    RebootNode (..),
    mkRebootNode,

    -- ** Request lenses
    rnNodeId,
    rnClusterName,

    -- * Destructuring the response
    RebootNodeResponse (..),
    mkRebootNodeResponse,

    -- ** Response lenses
    rnrsCluster,
    rnrsResponseStatus,
  )
where

import Network.AWS.DAX.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkRebootNode' smart constructor.
data RebootNode = RebootNode'
  { -- | The system-assigned ID of the node to be rebooted.
    nodeId :: Lude.Text,
    -- | The name of the DAX cluster containing the node to be rebooted.
    clusterName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RebootNode' with the minimum fields required to make a request.
--
-- * 'nodeId' - The system-assigned ID of the node to be rebooted.
-- * 'clusterName' - The name of the DAX cluster containing the node to be rebooted.
mkRebootNode ::
  -- | 'nodeId'
  Lude.Text ->
  -- | 'clusterName'
  Lude.Text ->
  RebootNode
mkRebootNode pNodeId_ pClusterName_ =
  RebootNode' {nodeId = pNodeId_, clusterName = pClusterName_}

-- | The system-assigned ID of the node to be rebooted.
--
-- /Note:/ Consider using 'nodeId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rnNodeId :: Lens.Lens' RebootNode Lude.Text
rnNodeId = Lens.lens (nodeId :: RebootNode -> Lude.Text) (\s a -> s {nodeId = a} :: RebootNode)
{-# DEPRECATED rnNodeId "Use generic-lens or generic-optics with 'nodeId' instead." #-}

-- | The name of the DAX cluster containing the node to be rebooted.
--
-- /Note:/ Consider using 'clusterName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rnClusterName :: Lens.Lens' RebootNode Lude.Text
rnClusterName = Lens.lens (clusterName :: RebootNode -> Lude.Text) (\s a -> s {clusterName = a} :: RebootNode)
{-# DEPRECATED rnClusterName "Use generic-lens or generic-optics with 'clusterName' instead." #-}

instance Lude.AWSRequest RebootNode where
  type Rs RebootNode = RebootNodeResponse
  request = Req.postJSON daxService
  response =
    Res.receiveJSON
      ( \s h x ->
          RebootNodeResponse'
            Lude.<$> (x Lude..?> "Cluster") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders RebootNode where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AmazonDAXV3.RebootNode" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON RebootNode where
  toJSON RebootNode' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("NodeId" Lude..= nodeId),
            Lude.Just ("ClusterName" Lude..= clusterName)
          ]
      )

instance Lude.ToPath RebootNode where
  toPath = Lude.const "/"

instance Lude.ToQuery RebootNode where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkRebootNodeResponse' smart constructor.
data RebootNodeResponse = RebootNodeResponse'
  { -- | A description of the DAX cluster after a node has been rebooted.
    cluster :: Lude.Maybe Cluster,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RebootNodeResponse' with the minimum fields required to make a request.
--
-- * 'cluster' - A description of the DAX cluster after a node has been rebooted.
-- * 'responseStatus' - The response status code.
mkRebootNodeResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  RebootNodeResponse
mkRebootNodeResponse pResponseStatus_ =
  RebootNodeResponse'
    { cluster = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A description of the DAX cluster after a node has been rebooted.
--
-- /Note:/ Consider using 'cluster' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rnrsCluster :: Lens.Lens' RebootNodeResponse (Lude.Maybe Cluster)
rnrsCluster = Lens.lens (cluster :: RebootNodeResponse -> Lude.Maybe Cluster) (\s a -> s {cluster = a} :: RebootNodeResponse)
{-# DEPRECATED rnrsCluster "Use generic-lens or generic-optics with 'cluster' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rnrsResponseStatus :: Lens.Lens' RebootNodeResponse Lude.Int
rnrsResponseStatus = Lens.lens (responseStatus :: RebootNodeResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: RebootNodeResponse)
{-# DEPRECATED rnrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
