{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DAX.DecreaseReplicationFactor
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes one or more nodes from a DAX cluster.
module Network.AWS.DAX.DecreaseReplicationFactor
  ( -- * Creating a request
    DecreaseReplicationFactor (..),
    mkDecreaseReplicationFactor,

    -- ** Request lenses
    drfNodeIdsToRemove,
    drfAvailabilityZones,
    drfClusterName,
    drfNewReplicationFactor,

    -- * Destructuring the response
    DecreaseReplicationFactorResponse (..),
    mkDecreaseReplicationFactorResponse,

    -- ** Response lenses
    drfrsCluster,
    drfrsResponseStatus,
  )
where

import Network.AWS.DAX.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDecreaseReplicationFactor' smart constructor.
data DecreaseReplicationFactor = DecreaseReplicationFactor'
  { nodeIdsToRemove ::
      Lude.Maybe [Lude.Text],
    availabilityZones ::
      Lude.Maybe [Lude.Text],
    clusterName :: Lude.Text,
    newReplicationFactor :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DecreaseReplicationFactor' with the minimum fields required to make a request.
--
-- * 'availabilityZones' - The Availability Zone(s) from which to remove nodes.
-- * 'clusterName' - The name of the DAX cluster from which you want to remove nodes.
-- * 'newReplicationFactor' - The new number of nodes for the DAX cluster.
-- * 'nodeIdsToRemove' - The unique identifiers of the nodes to be removed from the cluster.
mkDecreaseReplicationFactor ::
  -- | 'clusterName'
  Lude.Text ->
  -- | 'newReplicationFactor'
  Lude.Int ->
  DecreaseReplicationFactor
mkDecreaseReplicationFactor pClusterName_ pNewReplicationFactor_ =
  DecreaseReplicationFactor'
    { nodeIdsToRemove = Lude.Nothing,
      availabilityZones = Lude.Nothing,
      clusterName = pClusterName_,
      newReplicationFactor = pNewReplicationFactor_
    }

-- | The unique identifiers of the nodes to be removed from the cluster.
--
-- /Note:/ Consider using 'nodeIdsToRemove' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drfNodeIdsToRemove :: Lens.Lens' DecreaseReplicationFactor (Lude.Maybe [Lude.Text])
drfNodeIdsToRemove = Lens.lens (nodeIdsToRemove :: DecreaseReplicationFactor -> Lude.Maybe [Lude.Text]) (\s a -> s {nodeIdsToRemove = a} :: DecreaseReplicationFactor)
{-# DEPRECATED drfNodeIdsToRemove "Use generic-lens or generic-optics with 'nodeIdsToRemove' instead." #-}

-- | The Availability Zone(s) from which to remove nodes.
--
-- /Note:/ Consider using 'availabilityZones' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drfAvailabilityZones :: Lens.Lens' DecreaseReplicationFactor (Lude.Maybe [Lude.Text])
drfAvailabilityZones = Lens.lens (availabilityZones :: DecreaseReplicationFactor -> Lude.Maybe [Lude.Text]) (\s a -> s {availabilityZones = a} :: DecreaseReplicationFactor)
{-# DEPRECATED drfAvailabilityZones "Use generic-lens or generic-optics with 'availabilityZones' instead." #-}

-- | The name of the DAX cluster from which you want to remove nodes.
--
-- /Note:/ Consider using 'clusterName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drfClusterName :: Lens.Lens' DecreaseReplicationFactor Lude.Text
drfClusterName = Lens.lens (clusterName :: DecreaseReplicationFactor -> Lude.Text) (\s a -> s {clusterName = a} :: DecreaseReplicationFactor)
{-# DEPRECATED drfClusterName "Use generic-lens or generic-optics with 'clusterName' instead." #-}

-- | The new number of nodes for the DAX cluster.
--
-- /Note:/ Consider using 'newReplicationFactor' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drfNewReplicationFactor :: Lens.Lens' DecreaseReplicationFactor Lude.Int
drfNewReplicationFactor = Lens.lens (newReplicationFactor :: DecreaseReplicationFactor -> Lude.Int) (\s a -> s {newReplicationFactor = a} :: DecreaseReplicationFactor)
{-# DEPRECATED drfNewReplicationFactor "Use generic-lens or generic-optics with 'newReplicationFactor' instead." #-}

instance Lude.AWSRequest DecreaseReplicationFactor where
  type
    Rs DecreaseReplicationFactor =
      DecreaseReplicationFactorResponse
  request = Req.postJSON daxService
  response =
    Res.receiveJSON
      ( \s h x ->
          DecreaseReplicationFactorResponse'
            Lude.<$> (x Lude..?> "Cluster") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DecreaseReplicationFactor where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AmazonDAXV3.DecreaseReplicationFactor" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DecreaseReplicationFactor where
  toJSON DecreaseReplicationFactor' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NodeIdsToRemove" Lude..=) Lude.<$> nodeIdsToRemove,
            ("AvailabilityZones" Lude..=) Lude.<$> availabilityZones,
            Lude.Just ("ClusterName" Lude..= clusterName),
            Lude.Just ("NewReplicationFactor" Lude..= newReplicationFactor)
          ]
      )

instance Lude.ToPath DecreaseReplicationFactor where
  toPath = Lude.const "/"

instance Lude.ToQuery DecreaseReplicationFactor where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDecreaseReplicationFactorResponse' smart constructor.
data DecreaseReplicationFactorResponse = DecreaseReplicationFactorResponse'
  { cluster ::
      Lude.Maybe Cluster,
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DecreaseReplicationFactorResponse' with the minimum fields required to make a request.
--
-- * 'cluster' - A description of the DAX cluster, after you have decreased its replication factor.
-- * 'responseStatus' - The response status code.
mkDecreaseReplicationFactorResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DecreaseReplicationFactorResponse
mkDecreaseReplicationFactorResponse pResponseStatus_ =
  DecreaseReplicationFactorResponse'
    { cluster = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A description of the DAX cluster, after you have decreased its replication factor.
--
-- /Note:/ Consider using 'cluster' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drfrsCluster :: Lens.Lens' DecreaseReplicationFactorResponse (Lude.Maybe Cluster)
drfrsCluster = Lens.lens (cluster :: DecreaseReplicationFactorResponse -> Lude.Maybe Cluster) (\s a -> s {cluster = a} :: DecreaseReplicationFactorResponse)
{-# DEPRECATED drfrsCluster "Use generic-lens or generic-optics with 'cluster' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drfrsResponseStatus :: Lens.Lens' DecreaseReplicationFactorResponse Lude.Int
drfrsResponseStatus = Lens.lens (responseStatus :: DecreaseReplicationFactorResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DecreaseReplicationFactorResponse)
{-# DEPRECATED drfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
