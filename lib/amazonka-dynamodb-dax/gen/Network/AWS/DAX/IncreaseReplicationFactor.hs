{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DAX.IncreaseReplicationFactor
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds one or more nodes to a DAX cluster.
module Network.AWS.DAX.IncreaseReplicationFactor
  ( -- * Creating a request
    IncreaseReplicationFactor (..),
    mkIncreaseReplicationFactor,

    -- ** Request lenses
    irfAvailabilityZones,
    irfClusterName,
    irfNewReplicationFactor,

    -- * Destructuring the response
    IncreaseReplicationFactorResponse (..),
    mkIncreaseReplicationFactorResponse,

    -- ** Response lenses
    irfrsCluster,
    irfrsResponseStatus,
  )
where

import Network.AWS.DAX.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkIncreaseReplicationFactor' smart constructor.
data IncreaseReplicationFactor = IncreaseReplicationFactor'
  { availabilityZones ::
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

-- | Creates a value of 'IncreaseReplicationFactor' with the minimum fields required to make a request.
--
-- * 'availabilityZones' - The Availability Zones (AZs) in which the cluster nodes will be created. All nodes belonging to the cluster are placed in these Availability Zones. Use this parameter if you want to distribute the nodes across multiple AZs.
-- * 'clusterName' - The name of the DAX cluster that will receive additional nodes.
-- * 'newReplicationFactor' - The new number of nodes for the DAX cluster.
mkIncreaseReplicationFactor ::
  -- | 'clusterName'
  Lude.Text ->
  -- | 'newReplicationFactor'
  Lude.Int ->
  IncreaseReplicationFactor
mkIncreaseReplicationFactor pClusterName_ pNewReplicationFactor_ =
  IncreaseReplicationFactor'
    { availabilityZones = Lude.Nothing,
      clusterName = pClusterName_,
      newReplicationFactor = pNewReplicationFactor_
    }

-- | The Availability Zones (AZs) in which the cluster nodes will be created. All nodes belonging to the cluster are placed in these Availability Zones. Use this parameter if you want to distribute the nodes across multiple AZs.
--
-- /Note:/ Consider using 'availabilityZones' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
irfAvailabilityZones :: Lens.Lens' IncreaseReplicationFactor (Lude.Maybe [Lude.Text])
irfAvailabilityZones = Lens.lens (availabilityZones :: IncreaseReplicationFactor -> Lude.Maybe [Lude.Text]) (\s a -> s {availabilityZones = a} :: IncreaseReplicationFactor)
{-# DEPRECATED irfAvailabilityZones "Use generic-lens or generic-optics with 'availabilityZones' instead." #-}

-- | The name of the DAX cluster that will receive additional nodes.
--
-- /Note:/ Consider using 'clusterName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
irfClusterName :: Lens.Lens' IncreaseReplicationFactor Lude.Text
irfClusterName = Lens.lens (clusterName :: IncreaseReplicationFactor -> Lude.Text) (\s a -> s {clusterName = a} :: IncreaseReplicationFactor)
{-# DEPRECATED irfClusterName "Use generic-lens or generic-optics with 'clusterName' instead." #-}

-- | The new number of nodes for the DAX cluster.
--
-- /Note:/ Consider using 'newReplicationFactor' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
irfNewReplicationFactor :: Lens.Lens' IncreaseReplicationFactor Lude.Int
irfNewReplicationFactor = Lens.lens (newReplicationFactor :: IncreaseReplicationFactor -> Lude.Int) (\s a -> s {newReplicationFactor = a} :: IncreaseReplicationFactor)
{-# DEPRECATED irfNewReplicationFactor "Use generic-lens or generic-optics with 'newReplicationFactor' instead." #-}

instance Lude.AWSRequest IncreaseReplicationFactor where
  type
    Rs IncreaseReplicationFactor =
      IncreaseReplicationFactorResponse
  request = Req.postJSON daxService
  response =
    Res.receiveJSON
      ( \s h x ->
          IncreaseReplicationFactorResponse'
            Lude.<$> (x Lude..?> "Cluster") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders IncreaseReplicationFactor where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AmazonDAXV3.IncreaseReplicationFactor" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON IncreaseReplicationFactor where
  toJSON IncreaseReplicationFactor' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("AvailabilityZones" Lude..=) Lude.<$> availabilityZones,
            Lude.Just ("ClusterName" Lude..= clusterName),
            Lude.Just ("NewReplicationFactor" Lude..= newReplicationFactor)
          ]
      )

instance Lude.ToPath IncreaseReplicationFactor where
  toPath = Lude.const "/"

instance Lude.ToQuery IncreaseReplicationFactor where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkIncreaseReplicationFactorResponse' smart constructor.
data IncreaseReplicationFactorResponse = IncreaseReplicationFactorResponse'
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

-- | Creates a value of 'IncreaseReplicationFactorResponse' with the minimum fields required to make a request.
--
-- * 'cluster' - A description of the DAX cluster. with its new replication factor.
-- * 'responseStatus' - The response status code.
mkIncreaseReplicationFactorResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  IncreaseReplicationFactorResponse
mkIncreaseReplicationFactorResponse pResponseStatus_ =
  IncreaseReplicationFactorResponse'
    { cluster = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A description of the DAX cluster. with its new replication factor.
--
-- /Note:/ Consider using 'cluster' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
irfrsCluster :: Lens.Lens' IncreaseReplicationFactorResponse (Lude.Maybe Cluster)
irfrsCluster = Lens.lens (cluster :: IncreaseReplicationFactorResponse -> Lude.Maybe Cluster) (\s a -> s {cluster = a} :: IncreaseReplicationFactorResponse)
{-# DEPRECATED irfrsCluster "Use generic-lens or generic-optics with 'cluster' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
irfrsResponseStatus :: Lens.Lens' IncreaseReplicationFactorResponse Lude.Int
irfrsResponseStatus = Lens.lens (responseStatus :: IncreaseReplicationFactorResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: IncreaseReplicationFactorResponse)
{-# DEPRECATED irfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
