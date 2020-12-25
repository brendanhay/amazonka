{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.IncreaseNodeGroupsInGlobalReplicationGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Increase the number of node groups in the Global Datastore
module Network.AWS.ElastiCache.IncreaseNodeGroupsInGlobalReplicationGroup
  ( -- * Creating a request
    IncreaseNodeGroupsInGlobalReplicationGroup (..),
    mkIncreaseNodeGroupsInGlobalReplicationGroup,

    -- ** Request lenses
    ingigrgGlobalReplicationGroupId,
    ingigrgNodeGroupCount,
    ingigrgApplyImmediately,
    ingigrgRegionalConfigurations,

    -- * Destructuring the response
    IncreaseNodeGroupsInGlobalReplicationGroupResponse (..),
    mkIncreaseNodeGroupsInGlobalReplicationGroupResponse,

    -- ** Response lenses
    ingigrgrrsGlobalReplicationGroup,
    ingigrgrrsResponseStatus,
  )
where

import qualified Network.AWS.ElastiCache.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkIncreaseNodeGroupsInGlobalReplicationGroup' smart constructor.
data IncreaseNodeGroupsInGlobalReplicationGroup = IncreaseNodeGroupsInGlobalReplicationGroup'
  { -- | The name of the Global Datastore
    globalReplicationGroupId :: Types.GlobalReplicationGroupId,
    -- | The number of node groups you wish to add
    nodeGroupCount :: Core.Int,
    -- | Indicates that the process begins immediately. At present, the only permitted value for this parameter is true.
    applyImmediately :: Core.Bool,
    -- | Describes the replication group IDs, the AWS regions where they are stored and the shard configuration for each that comprise the Global Datastore
    regionalConfigurations :: Core.Maybe [Types.RegionalConfiguration]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'IncreaseNodeGroupsInGlobalReplicationGroup' value with any optional fields omitted.
mkIncreaseNodeGroupsInGlobalReplicationGroup ::
  -- | 'globalReplicationGroupId'
  Types.GlobalReplicationGroupId ->
  -- | 'nodeGroupCount'
  Core.Int ->
  -- | 'applyImmediately'
  Core.Bool ->
  IncreaseNodeGroupsInGlobalReplicationGroup
mkIncreaseNodeGroupsInGlobalReplicationGroup
  globalReplicationGroupId
  nodeGroupCount
  applyImmediately =
    IncreaseNodeGroupsInGlobalReplicationGroup'
      { globalReplicationGroupId,
        nodeGroupCount,
        applyImmediately,
        regionalConfigurations = Core.Nothing
      }

-- | The name of the Global Datastore
--
-- /Note:/ Consider using 'globalReplicationGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ingigrgGlobalReplicationGroupId :: Lens.Lens' IncreaseNodeGroupsInGlobalReplicationGroup Types.GlobalReplicationGroupId
ingigrgGlobalReplicationGroupId = Lens.field @"globalReplicationGroupId"
{-# DEPRECATED ingigrgGlobalReplicationGroupId "Use generic-lens or generic-optics with 'globalReplicationGroupId' instead." #-}

-- | The number of node groups you wish to add
--
-- /Note:/ Consider using 'nodeGroupCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ingigrgNodeGroupCount :: Lens.Lens' IncreaseNodeGroupsInGlobalReplicationGroup Core.Int
ingigrgNodeGroupCount = Lens.field @"nodeGroupCount"
{-# DEPRECATED ingigrgNodeGroupCount "Use generic-lens or generic-optics with 'nodeGroupCount' instead." #-}

-- | Indicates that the process begins immediately. At present, the only permitted value for this parameter is true.
--
-- /Note:/ Consider using 'applyImmediately' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ingigrgApplyImmediately :: Lens.Lens' IncreaseNodeGroupsInGlobalReplicationGroup Core.Bool
ingigrgApplyImmediately = Lens.field @"applyImmediately"
{-# DEPRECATED ingigrgApplyImmediately "Use generic-lens or generic-optics with 'applyImmediately' instead." #-}

-- | Describes the replication group IDs, the AWS regions where they are stored and the shard configuration for each that comprise the Global Datastore
--
-- /Note:/ Consider using 'regionalConfigurations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ingigrgRegionalConfigurations :: Lens.Lens' IncreaseNodeGroupsInGlobalReplicationGroup (Core.Maybe [Types.RegionalConfiguration])
ingigrgRegionalConfigurations = Lens.field @"regionalConfigurations"
{-# DEPRECATED ingigrgRegionalConfigurations "Use generic-lens or generic-optics with 'regionalConfigurations' instead." #-}

instance Core.AWSRequest IncreaseNodeGroupsInGlobalReplicationGroup where
  type
    Rs IncreaseNodeGroupsInGlobalReplicationGroup =
      IncreaseNodeGroupsInGlobalReplicationGroupResponse
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
            ( Core.pure ("Action", "IncreaseNodeGroupsInGlobalReplicationGroup")
                Core.<> (Core.pure ("Version", "2015-02-02"))
                Core.<> ( Core.toQueryValue
                            "GlobalReplicationGroupId"
                            globalReplicationGroupId
                        )
                Core.<> (Core.toQueryValue "NodeGroupCount" nodeGroupCount)
                Core.<> (Core.toQueryValue "ApplyImmediately" applyImmediately)
                Core.<> ( Core.toQueryValue
                            "RegionalConfigurations"
                            ( Core.toQueryList "RegionalConfiguration"
                                Core.<$> regionalConfigurations
                            )
                        )
            )
      }
  response =
    Response.receiveXMLWrapper
      "IncreaseNodeGroupsInGlobalReplicationGroupResult"
      ( \s h x ->
          IncreaseNodeGroupsInGlobalReplicationGroupResponse'
            Core.<$> (x Core..@? "GlobalReplicationGroup")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkIncreaseNodeGroupsInGlobalReplicationGroupResponse' smart constructor.
data IncreaseNodeGroupsInGlobalReplicationGroupResponse = IncreaseNodeGroupsInGlobalReplicationGroupResponse'
  { globalReplicationGroup :: Core.Maybe Types.GlobalReplicationGroup,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'IncreaseNodeGroupsInGlobalReplicationGroupResponse' value with any optional fields omitted.
mkIncreaseNodeGroupsInGlobalReplicationGroupResponse ::
  -- | 'responseStatus'
  Core.Int ->
  IncreaseNodeGroupsInGlobalReplicationGroupResponse
mkIncreaseNodeGroupsInGlobalReplicationGroupResponse responseStatus =
  IncreaseNodeGroupsInGlobalReplicationGroupResponse'
    { globalReplicationGroup =
        Core.Nothing,
      responseStatus
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'globalReplicationGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ingigrgrrsGlobalReplicationGroup :: Lens.Lens' IncreaseNodeGroupsInGlobalReplicationGroupResponse (Core.Maybe Types.GlobalReplicationGroup)
ingigrgrrsGlobalReplicationGroup = Lens.field @"globalReplicationGroup"
{-# DEPRECATED ingigrgrrsGlobalReplicationGroup "Use generic-lens or generic-optics with 'globalReplicationGroup' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ingigrgrrsResponseStatus :: Lens.Lens' IncreaseNodeGroupsInGlobalReplicationGroupResponse Core.Int
ingigrgrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ingigrgrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
