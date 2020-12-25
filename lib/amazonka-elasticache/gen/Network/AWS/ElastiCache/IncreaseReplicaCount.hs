{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
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
    ircReplicationGroupId,
    ircApplyImmediately,
    ircNewReplicaCount,
    ircReplicaConfiguration,

    -- * Destructuring the response
    IncreaseReplicaCountResponse (..),
    mkIncreaseReplicaCountResponse,

    -- ** Response lenses
    ircrrsReplicationGroup,
    ircrrsResponseStatus,
  )
where

import qualified Network.AWS.ElastiCache.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkIncreaseReplicaCount' smart constructor.
data IncreaseReplicaCount = IncreaseReplicaCount'
  { -- | The id of the replication group to which you want to add replica nodes.
    replicationGroupId :: Types.String,
    -- | If @True@ , the number of replica nodes is increased immediately. @ApplyImmediately=False@ is not currently supported.
    applyImmediately :: Core.Bool,
    -- | The number of read replica nodes you want at the completion of this operation. For Redis (cluster mode disabled) replication groups, this is the number of replica nodes in the replication group. For Redis (cluster mode enabled) replication groups, this is the number of replica nodes in each of the replication group's node groups.
    newReplicaCount :: Core.Maybe Core.Int,
    -- | A list of @ConfigureShard@ objects that can be used to configure each shard in a Redis (cluster mode enabled) replication group. The @ConfigureShard@ has three members: @NewReplicaCount@ , @NodeGroupId@ , and @PreferredAvailabilityZones@ .
    replicaConfiguration :: Core.Maybe [Types.ConfigureShard]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'IncreaseReplicaCount' value with any optional fields omitted.
mkIncreaseReplicaCount ::
  -- | 'replicationGroupId'
  Types.String ->
  -- | 'applyImmediately'
  Core.Bool ->
  IncreaseReplicaCount
mkIncreaseReplicaCount replicationGroupId applyImmediately =
  IncreaseReplicaCount'
    { replicationGroupId,
      applyImmediately,
      newReplicaCount = Core.Nothing,
      replicaConfiguration = Core.Nothing
    }

-- | The id of the replication group to which you want to add replica nodes.
--
-- /Note:/ Consider using 'replicationGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ircReplicationGroupId :: Lens.Lens' IncreaseReplicaCount Types.String
ircReplicationGroupId = Lens.field @"replicationGroupId"
{-# DEPRECATED ircReplicationGroupId "Use generic-lens or generic-optics with 'replicationGroupId' instead." #-}

-- | If @True@ , the number of replica nodes is increased immediately. @ApplyImmediately=False@ is not currently supported.
--
-- /Note:/ Consider using 'applyImmediately' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ircApplyImmediately :: Lens.Lens' IncreaseReplicaCount Core.Bool
ircApplyImmediately = Lens.field @"applyImmediately"
{-# DEPRECATED ircApplyImmediately "Use generic-lens or generic-optics with 'applyImmediately' instead." #-}

-- | The number of read replica nodes you want at the completion of this operation. For Redis (cluster mode disabled) replication groups, this is the number of replica nodes in the replication group. For Redis (cluster mode enabled) replication groups, this is the number of replica nodes in each of the replication group's node groups.
--
-- /Note:/ Consider using 'newReplicaCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ircNewReplicaCount :: Lens.Lens' IncreaseReplicaCount (Core.Maybe Core.Int)
ircNewReplicaCount = Lens.field @"newReplicaCount"
{-# DEPRECATED ircNewReplicaCount "Use generic-lens or generic-optics with 'newReplicaCount' instead." #-}

-- | A list of @ConfigureShard@ objects that can be used to configure each shard in a Redis (cluster mode enabled) replication group. The @ConfigureShard@ has three members: @NewReplicaCount@ , @NodeGroupId@ , and @PreferredAvailabilityZones@ .
--
-- /Note:/ Consider using 'replicaConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ircReplicaConfiguration :: Lens.Lens' IncreaseReplicaCount (Core.Maybe [Types.ConfigureShard])
ircReplicaConfiguration = Lens.field @"replicaConfiguration"
{-# DEPRECATED ircReplicaConfiguration "Use generic-lens or generic-optics with 'replicaConfiguration' instead." #-}

instance Core.AWSRequest IncreaseReplicaCount where
  type Rs IncreaseReplicaCount = IncreaseReplicaCountResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Action", "IncreaseReplicaCount")
                Core.<> (Core.pure ("Version", "2015-02-02"))
                Core.<> (Core.toQueryValue "ReplicationGroupId" replicationGroupId)
                Core.<> (Core.toQueryValue "ApplyImmediately" applyImmediately)
                Core.<> (Core.toQueryValue "NewReplicaCount" Core.<$> newReplicaCount)
                Core.<> ( Core.toQueryValue
                            "ReplicaConfiguration"
                            ( Core.toQueryList "ConfigureShard"
                                Core.<$> replicaConfiguration
                            )
                        )
            )
      }
  response =
    Response.receiveXMLWrapper
      "IncreaseReplicaCountResult"
      ( \s h x ->
          IncreaseReplicaCountResponse'
            Core.<$> (x Core..@? "ReplicationGroup")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkIncreaseReplicaCountResponse' smart constructor.
data IncreaseReplicaCountResponse = IncreaseReplicaCountResponse'
  { replicationGroup :: Core.Maybe Types.ReplicationGroup,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'IncreaseReplicaCountResponse' value with any optional fields omitted.
mkIncreaseReplicaCountResponse ::
  -- | 'responseStatus'
  Core.Int ->
  IncreaseReplicaCountResponse
mkIncreaseReplicaCountResponse responseStatus =
  IncreaseReplicaCountResponse'
    { replicationGroup = Core.Nothing,
      responseStatus
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'replicationGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ircrrsReplicationGroup :: Lens.Lens' IncreaseReplicaCountResponse (Core.Maybe Types.ReplicationGroup)
ircrrsReplicationGroup = Lens.field @"replicationGroup"
{-# DEPRECATED ircrrsReplicationGroup "Use generic-lens or generic-optics with 'replicationGroup' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ircrrsResponseStatus :: Lens.Lens' IncreaseReplicaCountResponse Core.Int
ircrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ircrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
