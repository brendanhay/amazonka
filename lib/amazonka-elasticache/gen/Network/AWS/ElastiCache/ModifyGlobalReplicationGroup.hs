{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.ModifyGlobalReplicationGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the settings for a Global Datastore.
module Network.AWS.ElastiCache.ModifyGlobalReplicationGroup
  ( -- * Creating a request
    ModifyGlobalReplicationGroup (..),
    mkModifyGlobalReplicationGroup,

    -- ** Request lenses
    mgrgGlobalReplicationGroupId,
    mgrgApplyImmediately,
    mgrgAutomaticFailoverEnabled,
    mgrgCacheNodeType,
    mgrgEngineVersion,
    mgrgGlobalReplicationGroupDescription,

    -- * Destructuring the response
    ModifyGlobalReplicationGroupResponse (..),
    mkModifyGlobalReplicationGroupResponse,

    -- ** Response lenses
    mgrgrrsGlobalReplicationGroup,
    mgrgrrsResponseStatus,
  )
where

import qualified Network.AWS.ElastiCache.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkModifyGlobalReplicationGroup' smart constructor.
data ModifyGlobalReplicationGroup = ModifyGlobalReplicationGroup'
  { -- | The name of the Global Datastore
    globalReplicationGroupId :: Types.String,
    -- | This parameter causes the modifications in this request and any pending modifications to be applied, asynchronously and as soon as possible. Modifications to Global Replication Groups cannot be requested to be applied in PreferredMaintenceWindow.
    applyImmediately :: Core.Bool,
    -- | Determines whether a read replica is automatically promoted to read/write primary if the existing primary encounters a failure.
    automaticFailoverEnabled :: Core.Maybe Core.Bool,
    -- | A valid cache node type that you want to scale this Global Datastore to.
    cacheNodeType :: Core.Maybe Types.String,
    -- | The upgraded version of the cache engine to be run on the clusters in the Global Datastore.
    engineVersion :: Core.Maybe Types.String,
    -- | A description of the Global Datastore
    globalReplicationGroupDescription :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ModifyGlobalReplicationGroup' value with any optional fields omitted.
mkModifyGlobalReplicationGroup ::
  -- | 'globalReplicationGroupId'
  Types.String ->
  -- | 'applyImmediately'
  Core.Bool ->
  ModifyGlobalReplicationGroup
mkModifyGlobalReplicationGroup
  globalReplicationGroupId
  applyImmediately =
    ModifyGlobalReplicationGroup'
      { globalReplicationGroupId,
        applyImmediately,
        automaticFailoverEnabled = Core.Nothing,
        cacheNodeType = Core.Nothing,
        engineVersion = Core.Nothing,
        globalReplicationGroupDescription = Core.Nothing
      }

-- | The name of the Global Datastore
--
-- /Note:/ Consider using 'globalReplicationGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mgrgGlobalReplicationGroupId :: Lens.Lens' ModifyGlobalReplicationGroup Types.String
mgrgGlobalReplicationGroupId = Lens.field @"globalReplicationGroupId"
{-# DEPRECATED mgrgGlobalReplicationGroupId "Use generic-lens or generic-optics with 'globalReplicationGroupId' instead." #-}

-- | This parameter causes the modifications in this request and any pending modifications to be applied, asynchronously and as soon as possible. Modifications to Global Replication Groups cannot be requested to be applied in PreferredMaintenceWindow.
--
-- /Note:/ Consider using 'applyImmediately' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mgrgApplyImmediately :: Lens.Lens' ModifyGlobalReplicationGroup Core.Bool
mgrgApplyImmediately = Lens.field @"applyImmediately"
{-# DEPRECATED mgrgApplyImmediately "Use generic-lens or generic-optics with 'applyImmediately' instead." #-}

-- | Determines whether a read replica is automatically promoted to read/write primary if the existing primary encounters a failure.
--
-- /Note:/ Consider using 'automaticFailoverEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mgrgAutomaticFailoverEnabled :: Lens.Lens' ModifyGlobalReplicationGroup (Core.Maybe Core.Bool)
mgrgAutomaticFailoverEnabled = Lens.field @"automaticFailoverEnabled"
{-# DEPRECATED mgrgAutomaticFailoverEnabled "Use generic-lens or generic-optics with 'automaticFailoverEnabled' instead." #-}

-- | A valid cache node type that you want to scale this Global Datastore to.
--
-- /Note:/ Consider using 'cacheNodeType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mgrgCacheNodeType :: Lens.Lens' ModifyGlobalReplicationGroup (Core.Maybe Types.String)
mgrgCacheNodeType = Lens.field @"cacheNodeType"
{-# DEPRECATED mgrgCacheNodeType "Use generic-lens or generic-optics with 'cacheNodeType' instead." #-}

-- | The upgraded version of the cache engine to be run on the clusters in the Global Datastore.
--
-- /Note:/ Consider using 'engineVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mgrgEngineVersion :: Lens.Lens' ModifyGlobalReplicationGroup (Core.Maybe Types.String)
mgrgEngineVersion = Lens.field @"engineVersion"
{-# DEPRECATED mgrgEngineVersion "Use generic-lens or generic-optics with 'engineVersion' instead." #-}

-- | A description of the Global Datastore
--
-- /Note:/ Consider using 'globalReplicationGroupDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mgrgGlobalReplicationGroupDescription :: Lens.Lens' ModifyGlobalReplicationGroup (Core.Maybe Types.String)
mgrgGlobalReplicationGroupDescription = Lens.field @"globalReplicationGroupDescription"
{-# DEPRECATED mgrgGlobalReplicationGroupDescription "Use generic-lens or generic-optics with 'globalReplicationGroupDescription' instead." #-}

instance Core.AWSRequest ModifyGlobalReplicationGroup where
  type
    Rs ModifyGlobalReplicationGroup =
      ModifyGlobalReplicationGroupResponse
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
            ( Core.pure ("Action", "ModifyGlobalReplicationGroup")
                Core.<> (Core.pure ("Version", "2015-02-02"))
                Core.<> ( Core.toQueryValue
                            "GlobalReplicationGroupId"
                            globalReplicationGroupId
                        )
                Core.<> (Core.toQueryValue "ApplyImmediately" applyImmediately)
                Core.<> ( Core.toQueryValue "AutomaticFailoverEnabled"
                            Core.<$> automaticFailoverEnabled
                        )
                Core.<> (Core.toQueryValue "CacheNodeType" Core.<$> cacheNodeType)
                Core.<> (Core.toQueryValue "EngineVersion" Core.<$> engineVersion)
                Core.<> ( Core.toQueryValue "GlobalReplicationGroupDescription"
                            Core.<$> globalReplicationGroupDescription
                        )
            )
      }
  response =
    Response.receiveXMLWrapper
      "ModifyGlobalReplicationGroupResult"
      ( \s h x ->
          ModifyGlobalReplicationGroupResponse'
            Core.<$> (x Core..@? "GlobalReplicationGroup")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkModifyGlobalReplicationGroupResponse' smart constructor.
data ModifyGlobalReplicationGroupResponse = ModifyGlobalReplicationGroupResponse'
  { globalReplicationGroup :: Core.Maybe Types.GlobalReplicationGroup,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ModifyGlobalReplicationGroupResponse' value with any optional fields omitted.
mkModifyGlobalReplicationGroupResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ModifyGlobalReplicationGroupResponse
mkModifyGlobalReplicationGroupResponse responseStatus =
  ModifyGlobalReplicationGroupResponse'
    { globalReplicationGroup =
        Core.Nothing,
      responseStatus
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'globalReplicationGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mgrgrrsGlobalReplicationGroup :: Lens.Lens' ModifyGlobalReplicationGroupResponse (Core.Maybe Types.GlobalReplicationGroup)
mgrgrrsGlobalReplicationGroup = Lens.field @"globalReplicationGroup"
{-# DEPRECATED mgrgrrsGlobalReplicationGroup "Use generic-lens or generic-optics with 'globalReplicationGroup' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mgrgrrsResponseStatus :: Lens.Lens' ModifyGlobalReplicationGroupResponse Core.Int
mgrgrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED mgrgrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
