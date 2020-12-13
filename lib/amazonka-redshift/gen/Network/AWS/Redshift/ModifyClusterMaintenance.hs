{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.ModifyClusterMaintenance
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the maintenance settings of a cluster.
module Network.AWS.Redshift.ModifyClusterMaintenance
  ( -- * Creating a request
    ModifyClusterMaintenance (..),
    mkModifyClusterMaintenance,

    -- ** Request lenses
    mcmDeferMaintenanceEndTime,
    mcmDeferMaintenance,
    mcmDeferMaintenanceDuration,
    mcmClusterIdentifier,
    mcmDeferMaintenanceStartTime,
    mcmDeferMaintenanceIdentifier,

    -- * Destructuring the response
    ModifyClusterMaintenanceResponse (..),
    mkModifyClusterMaintenanceResponse,

    -- ** Response lenses
    mcmrsCluster,
    mcmrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Redshift.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkModifyClusterMaintenance' smart constructor.
data ModifyClusterMaintenance = ModifyClusterMaintenance'
  { -- | A timestamp indicating end time for the deferred maintenance window. If you specify an end time, you can't specify a duration.
    deferMaintenanceEndTime :: Lude.Maybe Lude.DateTime,
    -- | A boolean indicating whether to enable the deferred maintenance window.
    deferMaintenance :: Lude.Maybe Lude.Bool,
    -- | An integer indicating the duration of the maintenance window in days. If you specify a duration, you can't specify an end time. The duration must be 45 days or less.
    deferMaintenanceDuration :: Lude.Maybe Lude.Int,
    -- | A unique identifier for the cluster.
    clusterIdentifier :: Lude.Text,
    -- | A timestamp indicating the start time for the deferred maintenance window.
    deferMaintenanceStartTime :: Lude.Maybe Lude.DateTime,
    -- | A unique identifier for the deferred maintenance window.
    deferMaintenanceIdentifier :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ModifyClusterMaintenance' with the minimum fields required to make a request.
--
-- * 'deferMaintenanceEndTime' - A timestamp indicating end time for the deferred maintenance window. If you specify an end time, you can't specify a duration.
-- * 'deferMaintenance' - A boolean indicating whether to enable the deferred maintenance window.
-- * 'deferMaintenanceDuration' - An integer indicating the duration of the maintenance window in days. If you specify a duration, you can't specify an end time. The duration must be 45 days or less.
-- * 'clusterIdentifier' - A unique identifier for the cluster.
-- * 'deferMaintenanceStartTime' - A timestamp indicating the start time for the deferred maintenance window.
-- * 'deferMaintenanceIdentifier' - A unique identifier for the deferred maintenance window.
mkModifyClusterMaintenance ::
  -- | 'clusterIdentifier'
  Lude.Text ->
  ModifyClusterMaintenance
mkModifyClusterMaintenance pClusterIdentifier_ =
  ModifyClusterMaintenance'
    { deferMaintenanceEndTime = Lude.Nothing,
      deferMaintenance = Lude.Nothing,
      deferMaintenanceDuration = Lude.Nothing,
      clusterIdentifier = pClusterIdentifier_,
      deferMaintenanceStartTime = Lude.Nothing,
      deferMaintenanceIdentifier = Lude.Nothing
    }

-- | A timestamp indicating end time for the deferred maintenance window. If you specify an end time, you can't specify a duration.
--
-- /Note:/ Consider using 'deferMaintenanceEndTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcmDeferMaintenanceEndTime :: Lens.Lens' ModifyClusterMaintenance (Lude.Maybe Lude.DateTime)
mcmDeferMaintenanceEndTime = Lens.lens (deferMaintenanceEndTime :: ModifyClusterMaintenance -> Lude.Maybe Lude.DateTime) (\s a -> s {deferMaintenanceEndTime = a} :: ModifyClusterMaintenance)
{-# DEPRECATED mcmDeferMaintenanceEndTime "Use generic-lens or generic-optics with 'deferMaintenanceEndTime' instead." #-}

-- | A boolean indicating whether to enable the deferred maintenance window.
--
-- /Note:/ Consider using 'deferMaintenance' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcmDeferMaintenance :: Lens.Lens' ModifyClusterMaintenance (Lude.Maybe Lude.Bool)
mcmDeferMaintenance = Lens.lens (deferMaintenance :: ModifyClusterMaintenance -> Lude.Maybe Lude.Bool) (\s a -> s {deferMaintenance = a} :: ModifyClusterMaintenance)
{-# DEPRECATED mcmDeferMaintenance "Use generic-lens or generic-optics with 'deferMaintenance' instead." #-}

-- | An integer indicating the duration of the maintenance window in days. If you specify a duration, you can't specify an end time. The duration must be 45 days or less.
--
-- /Note:/ Consider using 'deferMaintenanceDuration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcmDeferMaintenanceDuration :: Lens.Lens' ModifyClusterMaintenance (Lude.Maybe Lude.Int)
mcmDeferMaintenanceDuration = Lens.lens (deferMaintenanceDuration :: ModifyClusterMaintenance -> Lude.Maybe Lude.Int) (\s a -> s {deferMaintenanceDuration = a} :: ModifyClusterMaintenance)
{-# DEPRECATED mcmDeferMaintenanceDuration "Use generic-lens or generic-optics with 'deferMaintenanceDuration' instead." #-}

-- | A unique identifier for the cluster.
--
-- /Note:/ Consider using 'clusterIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcmClusterIdentifier :: Lens.Lens' ModifyClusterMaintenance Lude.Text
mcmClusterIdentifier = Lens.lens (clusterIdentifier :: ModifyClusterMaintenance -> Lude.Text) (\s a -> s {clusterIdentifier = a} :: ModifyClusterMaintenance)
{-# DEPRECATED mcmClusterIdentifier "Use generic-lens or generic-optics with 'clusterIdentifier' instead." #-}

-- | A timestamp indicating the start time for the deferred maintenance window.
--
-- /Note:/ Consider using 'deferMaintenanceStartTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcmDeferMaintenanceStartTime :: Lens.Lens' ModifyClusterMaintenance (Lude.Maybe Lude.DateTime)
mcmDeferMaintenanceStartTime = Lens.lens (deferMaintenanceStartTime :: ModifyClusterMaintenance -> Lude.Maybe Lude.DateTime) (\s a -> s {deferMaintenanceStartTime = a} :: ModifyClusterMaintenance)
{-# DEPRECATED mcmDeferMaintenanceStartTime "Use generic-lens or generic-optics with 'deferMaintenanceStartTime' instead." #-}

-- | A unique identifier for the deferred maintenance window.
--
-- /Note:/ Consider using 'deferMaintenanceIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcmDeferMaintenanceIdentifier :: Lens.Lens' ModifyClusterMaintenance (Lude.Maybe Lude.Text)
mcmDeferMaintenanceIdentifier = Lens.lens (deferMaintenanceIdentifier :: ModifyClusterMaintenance -> Lude.Maybe Lude.Text) (\s a -> s {deferMaintenanceIdentifier = a} :: ModifyClusterMaintenance)
{-# DEPRECATED mcmDeferMaintenanceIdentifier "Use generic-lens or generic-optics with 'deferMaintenanceIdentifier' instead." #-}

instance Lude.AWSRequest ModifyClusterMaintenance where
  type Rs ModifyClusterMaintenance = ModifyClusterMaintenanceResponse
  request = Req.postQuery redshiftService
  response =
    Res.receiveXMLWrapper
      "ModifyClusterMaintenanceResult"
      ( \s h x ->
          ModifyClusterMaintenanceResponse'
            Lude.<$> (x Lude..@? "Cluster") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ModifyClusterMaintenance where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ModifyClusterMaintenance where
  toPath = Lude.const "/"

instance Lude.ToQuery ModifyClusterMaintenance where
  toQuery ModifyClusterMaintenance' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("ModifyClusterMaintenance" :: Lude.ByteString),
        "Version" Lude.=: ("2012-12-01" :: Lude.ByteString),
        "DeferMaintenanceEndTime" Lude.=: deferMaintenanceEndTime,
        "DeferMaintenance" Lude.=: deferMaintenance,
        "DeferMaintenanceDuration" Lude.=: deferMaintenanceDuration,
        "ClusterIdentifier" Lude.=: clusterIdentifier,
        "DeferMaintenanceStartTime" Lude.=: deferMaintenanceStartTime,
        "DeferMaintenanceIdentifier" Lude.=: deferMaintenanceIdentifier
      ]

-- | /See:/ 'mkModifyClusterMaintenanceResponse' smart constructor.
data ModifyClusterMaintenanceResponse = ModifyClusterMaintenanceResponse'
  { cluster :: Lude.Maybe Cluster,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ModifyClusterMaintenanceResponse' with the minimum fields required to make a request.
--
-- * 'cluster' -
-- * 'responseStatus' - The response status code.
mkModifyClusterMaintenanceResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ModifyClusterMaintenanceResponse
mkModifyClusterMaintenanceResponse pResponseStatus_ =
  ModifyClusterMaintenanceResponse'
    { cluster = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'cluster' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcmrsCluster :: Lens.Lens' ModifyClusterMaintenanceResponse (Lude.Maybe Cluster)
mcmrsCluster = Lens.lens (cluster :: ModifyClusterMaintenanceResponse -> Lude.Maybe Cluster) (\s a -> s {cluster = a} :: ModifyClusterMaintenanceResponse)
{-# DEPRECATED mcmrsCluster "Use generic-lens or generic-optics with 'cluster' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcmrsResponseStatus :: Lens.Lens' ModifyClusterMaintenanceResponse Lude.Int
mcmrsResponseStatus = Lens.lens (responseStatus :: ModifyClusterMaintenanceResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ModifyClusterMaintenanceResponse)
{-# DEPRECATED mcmrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
