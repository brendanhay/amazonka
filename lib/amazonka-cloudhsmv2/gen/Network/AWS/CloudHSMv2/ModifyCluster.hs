{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudHSMv2.ModifyCluster
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies AWS CloudHSM cluster.
module Network.AWS.CloudHSMv2.ModifyCluster
  ( -- * Creating a request
    ModifyCluster (..),
    mkModifyCluster,

    -- ** Request lenses
    mcBackupRetentionPolicy,
    mcClusterId,

    -- * Destructuring the response
    ModifyClusterResponse (..),
    mkModifyClusterResponse,

    -- ** Response lenses
    mcrsCluster,
    mcrsResponseStatus,
  )
where

import Network.AWS.CloudHSMv2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkModifyCluster' smart constructor.
data ModifyCluster = ModifyCluster'
  { backupRetentionPolicy ::
      BackupRetentionPolicy,
    clusterId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ModifyCluster' with the minimum fields required to make a request.
--
-- * 'backupRetentionPolicy' - A policy that defines how the service retains backups.
-- * 'clusterId' - The identifier (ID) of the cluster that you want to modify. To find the cluster ID, use 'DescribeClusters' .
mkModifyCluster ::
  -- | 'backupRetentionPolicy'
  BackupRetentionPolicy ->
  -- | 'clusterId'
  Lude.Text ->
  ModifyCluster
mkModifyCluster pBackupRetentionPolicy_ pClusterId_ =
  ModifyCluster'
    { backupRetentionPolicy = pBackupRetentionPolicy_,
      clusterId = pClusterId_
    }

-- | A policy that defines how the service retains backups.
--
-- /Note:/ Consider using 'backupRetentionPolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcBackupRetentionPolicy :: Lens.Lens' ModifyCluster BackupRetentionPolicy
mcBackupRetentionPolicy = Lens.lens (backupRetentionPolicy :: ModifyCluster -> BackupRetentionPolicy) (\s a -> s {backupRetentionPolicy = a} :: ModifyCluster)
{-# DEPRECATED mcBackupRetentionPolicy "Use generic-lens or generic-optics with 'backupRetentionPolicy' instead." #-}

-- | The identifier (ID) of the cluster that you want to modify. To find the cluster ID, use 'DescribeClusters' .
--
-- /Note:/ Consider using 'clusterId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcClusterId :: Lens.Lens' ModifyCluster Lude.Text
mcClusterId = Lens.lens (clusterId :: ModifyCluster -> Lude.Text) (\s a -> s {clusterId = a} :: ModifyCluster)
{-# DEPRECATED mcClusterId "Use generic-lens or generic-optics with 'clusterId' instead." #-}

instance Lude.AWSRequest ModifyCluster where
  type Rs ModifyCluster = ModifyClusterResponse
  request = Req.postJSON cloudHSMv2Service
  response =
    Res.receiveJSON
      ( \s h x ->
          ModifyClusterResponse'
            Lude.<$> (x Lude..?> "Cluster") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ModifyCluster where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("BaldrApiService.ModifyCluster" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ModifyCluster where
  toJSON ModifyCluster' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("BackupRetentionPolicy" Lude..= backupRetentionPolicy),
            Lude.Just ("ClusterId" Lude..= clusterId)
          ]
      )

instance Lude.ToPath ModifyCluster where
  toPath = Lude.const "/"

instance Lude.ToQuery ModifyCluster where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkModifyClusterResponse' smart constructor.
data ModifyClusterResponse = ModifyClusterResponse'
  { cluster ::
      Lude.Maybe Cluster,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ModifyClusterResponse' with the minimum fields required to make a request.
--
-- * 'cluster' - Undocumented field.
-- * 'responseStatus' - The response status code.
mkModifyClusterResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ModifyClusterResponse
mkModifyClusterResponse pResponseStatus_ =
  ModifyClusterResponse'
    { cluster = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'cluster' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcrsCluster :: Lens.Lens' ModifyClusterResponse (Lude.Maybe Cluster)
mcrsCluster = Lens.lens (cluster :: ModifyClusterResponse -> Lude.Maybe Cluster) (\s a -> s {cluster = a} :: ModifyClusterResponse)
{-# DEPRECATED mcrsCluster "Use generic-lens or generic-optics with 'cluster' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcrsResponseStatus :: Lens.Lens' ModifyClusterResponse Lude.Int
mcrsResponseStatus = Lens.lens (responseStatus :: ModifyClusterResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ModifyClusterResponse)
{-# DEPRECATED mcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
