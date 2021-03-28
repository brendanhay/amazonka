{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      ModifyGlobalReplicationGroup (..)
    , mkModifyGlobalReplicationGroup
    -- ** Request lenses
    , mgrgGlobalReplicationGroupId
    , mgrgApplyImmediately
    , mgrgAutomaticFailoverEnabled
    , mgrgCacheNodeType
    , mgrgEngineVersion
    , mgrgGlobalReplicationGroupDescription

    -- * Destructuring the response
    , ModifyGlobalReplicationGroupResponse (..)
    , mkModifyGlobalReplicationGroupResponse
    -- ** Response lenses
    , mgrgrrsGlobalReplicationGroup
    , mgrgrrsResponseStatus
    ) where

import qualified Network.AWS.ElastiCache.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkModifyGlobalReplicationGroup' smart constructor.
data ModifyGlobalReplicationGroup = ModifyGlobalReplicationGroup'
  { globalReplicationGroupId :: Core.Text
    -- ^ The name of the Global Datastore
  , applyImmediately :: Core.Bool
    -- ^ This parameter causes the modifications in this request and any pending modifications to be applied, asynchronously and as soon as possible. Modifications to Global Replication Groups cannot be requested to be applied in PreferredMaintenceWindow. 
  , automaticFailoverEnabled :: Core.Maybe Core.Bool
    -- ^ Determines whether a read replica is automatically promoted to read/write primary if the existing primary encounters a failure. 
  , cacheNodeType :: Core.Maybe Core.Text
    -- ^ A valid cache node type that you want to scale this Global Datastore to.
  , engineVersion :: Core.Maybe Core.Text
    -- ^ The upgraded version of the cache engine to be run on the clusters in the Global Datastore. 
  , globalReplicationGroupDescription :: Core.Maybe Core.Text
    -- ^ A description of the Global Datastore
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ModifyGlobalReplicationGroup' value with any optional fields omitted.
mkModifyGlobalReplicationGroup
    :: Core.Text -- ^ 'globalReplicationGroupId'
    -> Core.Bool -- ^ 'applyImmediately'
    -> ModifyGlobalReplicationGroup
mkModifyGlobalReplicationGroup globalReplicationGroupId
  applyImmediately
  = ModifyGlobalReplicationGroup'{globalReplicationGroupId,
                                  applyImmediately, automaticFailoverEnabled = Core.Nothing,
                                  cacheNodeType = Core.Nothing, engineVersion = Core.Nothing,
                                  globalReplicationGroupDescription = Core.Nothing}

-- | The name of the Global Datastore
--
-- /Note:/ Consider using 'globalReplicationGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mgrgGlobalReplicationGroupId :: Lens.Lens' ModifyGlobalReplicationGroup Core.Text
mgrgGlobalReplicationGroupId = Lens.field @"globalReplicationGroupId"
{-# INLINEABLE mgrgGlobalReplicationGroupId #-}
{-# DEPRECATED globalReplicationGroupId "Use generic-lens or generic-optics with 'globalReplicationGroupId' instead"  #-}

-- | This parameter causes the modifications in this request and any pending modifications to be applied, asynchronously and as soon as possible. Modifications to Global Replication Groups cannot be requested to be applied in PreferredMaintenceWindow. 
--
-- /Note:/ Consider using 'applyImmediately' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mgrgApplyImmediately :: Lens.Lens' ModifyGlobalReplicationGroup Core.Bool
mgrgApplyImmediately = Lens.field @"applyImmediately"
{-# INLINEABLE mgrgApplyImmediately #-}
{-# DEPRECATED applyImmediately "Use generic-lens or generic-optics with 'applyImmediately' instead"  #-}

-- | Determines whether a read replica is automatically promoted to read/write primary if the existing primary encounters a failure. 
--
-- /Note:/ Consider using 'automaticFailoverEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mgrgAutomaticFailoverEnabled :: Lens.Lens' ModifyGlobalReplicationGroup (Core.Maybe Core.Bool)
mgrgAutomaticFailoverEnabled = Lens.field @"automaticFailoverEnabled"
{-# INLINEABLE mgrgAutomaticFailoverEnabled #-}
{-# DEPRECATED automaticFailoverEnabled "Use generic-lens or generic-optics with 'automaticFailoverEnabled' instead"  #-}

-- | A valid cache node type that you want to scale this Global Datastore to.
--
-- /Note:/ Consider using 'cacheNodeType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mgrgCacheNodeType :: Lens.Lens' ModifyGlobalReplicationGroup (Core.Maybe Core.Text)
mgrgCacheNodeType = Lens.field @"cacheNodeType"
{-# INLINEABLE mgrgCacheNodeType #-}
{-# DEPRECATED cacheNodeType "Use generic-lens or generic-optics with 'cacheNodeType' instead"  #-}

-- | The upgraded version of the cache engine to be run on the clusters in the Global Datastore. 
--
-- /Note:/ Consider using 'engineVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mgrgEngineVersion :: Lens.Lens' ModifyGlobalReplicationGroup (Core.Maybe Core.Text)
mgrgEngineVersion = Lens.field @"engineVersion"
{-# INLINEABLE mgrgEngineVersion #-}
{-# DEPRECATED engineVersion "Use generic-lens or generic-optics with 'engineVersion' instead"  #-}

-- | A description of the Global Datastore
--
-- /Note:/ Consider using 'globalReplicationGroupDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mgrgGlobalReplicationGroupDescription :: Lens.Lens' ModifyGlobalReplicationGroup (Core.Maybe Core.Text)
mgrgGlobalReplicationGroupDescription = Lens.field @"globalReplicationGroupDescription"
{-# INLINEABLE mgrgGlobalReplicationGroupDescription #-}
{-# DEPRECATED globalReplicationGroupDescription "Use generic-lens or generic-optics with 'globalReplicationGroupDescription' instead"  #-}

instance Core.ToQuery ModifyGlobalReplicationGroup where
        toQuery ModifyGlobalReplicationGroup{..}
          = Core.toQueryPair "Action"
              ("ModifyGlobalReplicationGroup" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2015-02-02" :: Core.Text)
              Core.<>
              Core.toQueryPair "GlobalReplicationGroupId"
                globalReplicationGroupId
              Core.<> Core.toQueryPair "ApplyImmediately" applyImmediately
              Core.<>
              Core.maybe Core.mempty
                (Core.toQueryPair "AutomaticFailoverEnabled")
                automaticFailoverEnabled
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "CacheNodeType")
                cacheNodeType
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "EngineVersion")
                engineVersion
              Core.<>
              Core.maybe Core.mempty
                (Core.toQueryPair "GlobalReplicationGroupDescription")
                globalReplicationGroupDescription

instance Core.ToHeaders ModifyGlobalReplicationGroup where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest ModifyGlobalReplicationGroup where
        type Rs ModifyGlobalReplicationGroup =
             ModifyGlobalReplicationGroupResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.mempty,
                         Core._rqHeaders =
                           Core.pure
                             ("Content-Type",
                              "application/x-www-form-urlencoded; charset=utf-8")
                             Core.<> Core.toHeaders x,
                         Core._rqBody = Core.toFormBody (Core.toQuery x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXMLWrapper "ModifyGlobalReplicationGroupResult"
              (\ s h x ->
                 ModifyGlobalReplicationGroupResponse' Core.<$>
                   (x Core..@? "GlobalReplicationGroup") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkModifyGlobalReplicationGroupResponse' smart constructor.
data ModifyGlobalReplicationGroupResponse = ModifyGlobalReplicationGroupResponse'
  { globalReplicationGroup :: Core.Maybe Types.GlobalReplicationGroup
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ModifyGlobalReplicationGroupResponse' value with any optional fields omitted.
mkModifyGlobalReplicationGroupResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ModifyGlobalReplicationGroupResponse
mkModifyGlobalReplicationGroupResponse responseStatus
  = ModifyGlobalReplicationGroupResponse'{globalReplicationGroup =
                                            Core.Nothing,
                                          responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'globalReplicationGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mgrgrrsGlobalReplicationGroup :: Lens.Lens' ModifyGlobalReplicationGroupResponse (Core.Maybe Types.GlobalReplicationGroup)
mgrgrrsGlobalReplicationGroup = Lens.field @"globalReplicationGroup"
{-# INLINEABLE mgrgrrsGlobalReplicationGroup #-}
{-# DEPRECATED globalReplicationGroup "Use generic-lens or generic-optics with 'globalReplicationGroup' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mgrgrrsResponseStatus :: Lens.Lens' ModifyGlobalReplicationGroupResponse Core.Int
mgrgrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE mgrgrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
