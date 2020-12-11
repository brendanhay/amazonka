{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.IncreaseReplicaCount
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Dynamically increases the number of replics in a Redis (cluster mode disabled) replication group or the number of replica nodes in one or more node groups (shards) of a Redis (cluster mode enabled) replication group. This operation is performed with no cluster down time.
module Network.AWS.ElastiCache.IncreaseReplicaCount
  ( -- * Creating a request
    IncreaseReplicaCount (..),
    mkIncreaseReplicaCount,

    -- ** Request lenses
    ircNewReplicaCount,
    ircReplicaConfiguration,
    ircReplicationGroupId,
    ircApplyImmediately,

    -- * Destructuring the response
    IncreaseReplicaCountResponse (..),
    mkIncreaseReplicaCountResponse,

    -- ** Response lenses
    ircrsReplicationGroup,
    ircrsResponseStatus,
  )
where

import Network.AWS.ElastiCache.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkIncreaseReplicaCount' smart constructor.
data IncreaseReplicaCount = IncreaseReplicaCount'
  { newReplicaCount ::
      Lude.Maybe Lude.Int,
    replicaConfiguration ::
      Lude.Maybe [ConfigureShard],
    replicationGroupId :: Lude.Text,
    applyImmediately :: Lude.Bool
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'IncreaseReplicaCount' with the minimum fields required to make a request.
--
-- * 'applyImmediately' - If @True@ , the number of replica nodes is increased immediately. @ApplyImmediately=False@ is not currently supported.
-- * 'newReplicaCount' - The number of read replica nodes you want at the completion of this operation. For Redis (cluster mode disabled) replication groups, this is the number of replica nodes in the replication group. For Redis (cluster mode enabled) replication groups, this is the number of replica nodes in each of the replication group's node groups.
-- * 'replicaConfiguration' - A list of @ConfigureShard@ objects that can be used to configure each shard in a Redis (cluster mode enabled) replication group. The @ConfigureShard@ has three members: @NewReplicaCount@ , @NodeGroupId@ , and @PreferredAvailabilityZones@ .
-- * 'replicationGroupId' - The id of the replication group to which you want to add replica nodes.
mkIncreaseReplicaCount ::
  -- | 'replicationGroupId'
  Lude.Text ->
  -- | 'applyImmediately'
  Lude.Bool ->
  IncreaseReplicaCount
mkIncreaseReplicaCount pReplicationGroupId_ pApplyImmediately_ =
  IncreaseReplicaCount'
    { newReplicaCount = Lude.Nothing,
      replicaConfiguration = Lude.Nothing,
      replicationGroupId = pReplicationGroupId_,
      applyImmediately = pApplyImmediately_
    }

-- | The number of read replica nodes you want at the completion of this operation. For Redis (cluster mode disabled) replication groups, this is the number of replica nodes in the replication group. For Redis (cluster mode enabled) replication groups, this is the number of replica nodes in each of the replication group's node groups.
--
-- /Note:/ Consider using 'newReplicaCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ircNewReplicaCount :: Lens.Lens' IncreaseReplicaCount (Lude.Maybe Lude.Int)
ircNewReplicaCount = Lens.lens (newReplicaCount :: IncreaseReplicaCount -> Lude.Maybe Lude.Int) (\s a -> s {newReplicaCount = a} :: IncreaseReplicaCount)
{-# DEPRECATED ircNewReplicaCount "Use generic-lens or generic-optics with 'newReplicaCount' instead." #-}

-- | A list of @ConfigureShard@ objects that can be used to configure each shard in a Redis (cluster mode enabled) replication group. The @ConfigureShard@ has three members: @NewReplicaCount@ , @NodeGroupId@ , and @PreferredAvailabilityZones@ .
--
-- /Note:/ Consider using 'replicaConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ircReplicaConfiguration :: Lens.Lens' IncreaseReplicaCount (Lude.Maybe [ConfigureShard])
ircReplicaConfiguration = Lens.lens (replicaConfiguration :: IncreaseReplicaCount -> Lude.Maybe [ConfigureShard]) (\s a -> s {replicaConfiguration = a} :: IncreaseReplicaCount)
{-# DEPRECATED ircReplicaConfiguration "Use generic-lens or generic-optics with 'replicaConfiguration' instead." #-}

-- | The id of the replication group to which you want to add replica nodes.
--
-- /Note:/ Consider using 'replicationGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ircReplicationGroupId :: Lens.Lens' IncreaseReplicaCount Lude.Text
ircReplicationGroupId = Lens.lens (replicationGroupId :: IncreaseReplicaCount -> Lude.Text) (\s a -> s {replicationGroupId = a} :: IncreaseReplicaCount)
{-# DEPRECATED ircReplicationGroupId "Use generic-lens or generic-optics with 'replicationGroupId' instead." #-}

-- | If @True@ , the number of replica nodes is increased immediately. @ApplyImmediately=False@ is not currently supported.
--
-- /Note:/ Consider using 'applyImmediately' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ircApplyImmediately :: Lens.Lens' IncreaseReplicaCount Lude.Bool
ircApplyImmediately = Lens.lens (applyImmediately :: IncreaseReplicaCount -> Lude.Bool) (\s a -> s {applyImmediately = a} :: IncreaseReplicaCount)
{-# DEPRECATED ircApplyImmediately "Use generic-lens or generic-optics with 'applyImmediately' instead." #-}

instance Lude.AWSRequest IncreaseReplicaCount where
  type Rs IncreaseReplicaCount = IncreaseReplicaCountResponse
  request = Req.postQuery elastiCacheService
  response =
    Res.receiveXMLWrapper
      "IncreaseReplicaCountResult"
      ( \s h x ->
          IncreaseReplicaCountResponse'
            Lude.<$> (x Lude..@? "ReplicationGroup")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders IncreaseReplicaCount where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath IncreaseReplicaCount where
  toPath = Lude.const "/"

instance Lude.ToQuery IncreaseReplicaCount where
  toQuery IncreaseReplicaCount' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("IncreaseReplicaCount" :: Lude.ByteString),
        "Version" Lude.=: ("2015-02-02" :: Lude.ByteString),
        "NewReplicaCount" Lude.=: newReplicaCount,
        "ReplicaConfiguration"
          Lude.=: Lude.toQuery
            (Lude.toQueryList "ConfigureShard" Lude.<$> replicaConfiguration),
        "ReplicationGroupId" Lude.=: replicationGroupId,
        "ApplyImmediately" Lude.=: applyImmediately
      ]

-- | /See:/ 'mkIncreaseReplicaCountResponse' smart constructor.
data IncreaseReplicaCountResponse = IncreaseReplicaCountResponse'
  { replicationGroup ::
      Lude.Maybe ReplicationGroup,
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

-- | Creates a value of 'IncreaseReplicaCountResponse' with the minimum fields required to make a request.
--
-- * 'replicationGroup' - Undocumented field.
-- * 'responseStatus' - The response status code.
mkIncreaseReplicaCountResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  IncreaseReplicaCountResponse
mkIncreaseReplicaCountResponse pResponseStatus_ =
  IncreaseReplicaCountResponse'
    { replicationGroup = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'replicationGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ircrsReplicationGroup :: Lens.Lens' IncreaseReplicaCountResponse (Lude.Maybe ReplicationGroup)
ircrsReplicationGroup = Lens.lens (replicationGroup :: IncreaseReplicaCountResponse -> Lude.Maybe ReplicationGroup) (\s a -> s {replicationGroup = a} :: IncreaseReplicaCountResponse)
{-# DEPRECATED ircrsReplicationGroup "Use generic-lens or generic-optics with 'replicationGroup' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ircrsResponseStatus :: Lens.Lens' IncreaseReplicaCountResponse Lude.Int
ircrsResponseStatus = Lens.lens (responseStatus :: IncreaseReplicaCountResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: IncreaseReplicaCountResponse)
{-# DEPRECATED ircrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
