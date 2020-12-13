{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.Types.ProcessedUpdateAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElastiCache.Types.ProcessedUpdateAction
  ( ProcessedUpdateAction (..),

    -- * Smart constructor
    mkProcessedUpdateAction,

    -- * Lenses
    puaCacheClusterId,
    puaServiceUpdateName,
    puaUpdateActionStatus,
    puaReplicationGroupId,
  )
where

import Network.AWS.ElastiCache.Types.UpdateActionStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Update action that has been processed for the corresponding apply/stop request
--
-- /See:/ 'mkProcessedUpdateAction' smart constructor.
data ProcessedUpdateAction = ProcessedUpdateAction'
  { -- | The ID of the cache cluster
    cacheClusterId :: Lude.Maybe Lude.Text,
    -- | The unique ID of the service update
    serviceUpdateName :: Lude.Maybe Lude.Text,
    -- | The status of the update action on the Redis cluster
    updateActionStatus :: Lude.Maybe UpdateActionStatus,
    -- | The ID of the replication group
    replicationGroupId :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ProcessedUpdateAction' with the minimum fields required to make a request.
--
-- * 'cacheClusterId' - The ID of the cache cluster
-- * 'serviceUpdateName' - The unique ID of the service update
-- * 'updateActionStatus' - The status of the update action on the Redis cluster
-- * 'replicationGroupId' - The ID of the replication group
mkProcessedUpdateAction ::
  ProcessedUpdateAction
mkProcessedUpdateAction =
  ProcessedUpdateAction'
    { cacheClusterId = Lude.Nothing,
      serviceUpdateName = Lude.Nothing,
      updateActionStatus = Lude.Nothing,
      replicationGroupId = Lude.Nothing
    }

-- | The ID of the cache cluster
--
-- /Note:/ Consider using 'cacheClusterId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
puaCacheClusterId :: Lens.Lens' ProcessedUpdateAction (Lude.Maybe Lude.Text)
puaCacheClusterId = Lens.lens (cacheClusterId :: ProcessedUpdateAction -> Lude.Maybe Lude.Text) (\s a -> s {cacheClusterId = a} :: ProcessedUpdateAction)
{-# DEPRECATED puaCacheClusterId "Use generic-lens or generic-optics with 'cacheClusterId' instead." #-}

-- | The unique ID of the service update
--
-- /Note:/ Consider using 'serviceUpdateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
puaServiceUpdateName :: Lens.Lens' ProcessedUpdateAction (Lude.Maybe Lude.Text)
puaServiceUpdateName = Lens.lens (serviceUpdateName :: ProcessedUpdateAction -> Lude.Maybe Lude.Text) (\s a -> s {serviceUpdateName = a} :: ProcessedUpdateAction)
{-# DEPRECATED puaServiceUpdateName "Use generic-lens or generic-optics with 'serviceUpdateName' instead." #-}

-- | The status of the update action on the Redis cluster
--
-- /Note:/ Consider using 'updateActionStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
puaUpdateActionStatus :: Lens.Lens' ProcessedUpdateAction (Lude.Maybe UpdateActionStatus)
puaUpdateActionStatus = Lens.lens (updateActionStatus :: ProcessedUpdateAction -> Lude.Maybe UpdateActionStatus) (\s a -> s {updateActionStatus = a} :: ProcessedUpdateAction)
{-# DEPRECATED puaUpdateActionStatus "Use generic-lens or generic-optics with 'updateActionStatus' instead." #-}

-- | The ID of the replication group
--
-- /Note:/ Consider using 'replicationGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
puaReplicationGroupId :: Lens.Lens' ProcessedUpdateAction (Lude.Maybe Lude.Text)
puaReplicationGroupId = Lens.lens (replicationGroupId :: ProcessedUpdateAction -> Lude.Maybe Lude.Text) (\s a -> s {replicationGroupId = a} :: ProcessedUpdateAction)
{-# DEPRECATED puaReplicationGroupId "Use generic-lens or generic-optics with 'replicationGroupId' instead." #-}

instance Lude.FromXML ProcessedUpdateAction where
  parseXML x =
    ProcessedUpdateAction'
      Lude.<$> (x Lude..@? "CacheClusterId")
      Lude.<*> (x Lude..@? "ServiceUpdateName")
      Lude.<*> (x Lude..@? "UpdateActionStatus")
      Lude.<*> (x Lude..@? "ReplicationGroupId")
