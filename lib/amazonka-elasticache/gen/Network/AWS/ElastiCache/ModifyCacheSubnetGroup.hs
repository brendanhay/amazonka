{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.ModifyCacheSubnetGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies an existing cache subnet group.
module Network.AWS.ElastiCache.ModifyCacheSubnetGroup
    (
    -- * Creating a request
      ModifyCacheSubnetGroup (..)
    , mkModifyCacheSubnetGroup
    -- ** Request lenses
    , mcsgCacheSubnetGroupName
    , mcsgCacheSubnetGroupDescription
    , mcsgSubnetIds

    -- * Destructuring the response
    , ModifyCacheSubnetGroupResponse (..)
    , mkModifyCacheSubnetGroupResponse
    -- ** Response lenses
    , mcsgrrsCacheSubnetGroup
    , mcsgrrsResponseStatus
    ) where

import qualified Network.AWS.ElastiCache.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a @ModifyCacheSubnetGroup@ operation.
--
-- /See:/ 'mkModifyCacheSubnetGroup' smart constructor.
data ModifyCacheSubnetGroup = ModifyCacheSubnetGroup'
  { cacheSubnetGroupName :: Core.Text
    -- ^ The name for the cache subnet group. This value is stored as a lowercase string.
--
-- Constraints: Must contain no more than 255 alphanumeric characters or hyphens.
-- Example: @mysubnetgroup@ 
  , cacheSubnetGroupDescription :: Core.Maybe Core.Text
    -- ^ A description of the cache subnet group.
  , subnetIds :: Core.Maybe [Core.Text]
    -- ^ The EC2 subnet IDs for the cache subnet group.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ModifyCacheSubnetGroup' value with any optional fields omitted.
mkModifyCacheSubnetGroup
    :: Core.Text -- ^ 'cacheSubnetGroupName'
    -> ModifyCacheSubnetGroup
mkModifyCacheSubnetGroup cacheSubnetGroupName
  = ModifyCacheSubnetGroup'{cacheSubnetGroupName,
                            cacheSubnetGroupDescription = Core.Nothing,
                            subnetIds = Core.Nothing}

-- | The name for the cache subnet group. This value is stored as a lowercase string.
--
-- Constraints: Must contain no more than 255 alphanumeric characters or hyphens.
-- Example: @mysubnetgroup@ 
--
-- /Note:/ Consider using 'cacheSubnetGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcsgCacheSubnetGroupName :: Lens.Lens' ModifyCacheSubnetGroup Core.Text
mcsgCacheSubnetGroupName = Lens.field @"cacheSubnetGroupName"
{-# INLINEABLE mcsgCacheSubnetGroupName #-}
{-# DEPRECATED cacheSubnetGroupName "Use generic-lens or generic-optics with 'cacheSubnetGroupName' instead"  #-}

-- | A description of the cache subnet group.
--
-- /Note:/ Consider using 'cacheSubnetGroupDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcsgCacheSubnetGroupDescription :: Lens.Lens' ModifyCacheSubnetGroup (Core.Maybe Core.Text)
mcsgCacheSubnetGroupDescription = Lens.field @"cacheSubnetGroupDescription"
{-# INLINEABLE mcsgCacheSubnetGroupDescription #-}
{-# DEPRECATED cacheSubnetGroupDescription "Use generic-lens or generic-optics with 'cacheSubnetGroupDescription' instead"  #-}

-- | The EC2 subnet IDs for the cache subnet group.
--
-- /Note:/ Consider using 'subnetIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcsgSubnetIds :: Lens.Lens' ModifyCacheSubnetGroup (Core.Maybe [Core.Text])
mcsgSubnetIds = Lens.field @"subnetIds"
{-# INLINEABLE mcsgSubnetIds #-}
{-# DEPRECATED subnetIds "Use generic-lens or generic-optics with 'subnetIds' instead"  #-}

instance Core.ToQuery ModifyCacheSubnetGroup where
        toQuery ModifyCacheSubnetGroup{..}
          = Core.toQueryPair "Action" ("ModifyCacheSubnetGroup" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2015-02-02" :: Core.Text)
              Core.<>
              Core.toQueryPair "CacheSubnetGroupName" cacheSubnetGroupName
              Core.<>
              Core.maybe Core.mempty
                (Core.toQueryPair "CacheSubnetGroupDescription")
                cacheSubnetGroupDescription
              Core.<>
              Core.toQueryPair "SubnetIds"
                (Core.maybe Core.mempty (Core.toQueryList "SubnetIdentifier")
                   subnetIds)

instance Core.ToHeaders ModifyCacheSubnetGroup where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest ModifyCacheSubnetGroup where
        type Rs ModifyCacheSubnetGroup = ModifyCacheSubnetGroupResponse
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
          = Response.receiveXMLWrapper "ModifyCacheSubnetGroupResult"
              (\ s h x ->
                 ModifyCacheSubnetGroupResponse' Core.<$>
                   (x Core..@? "CacheSubnetGroup") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkModifyCacheSubnetGroupResponse' smart constructor.
data ModifyCacheSubnetGroupResponse = ModifyCacheSubnetGroupResponse'
  { cacheSubnetGroup :: Core.Maybe Types.CacheSubnetGroup
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ModifyCacheSubnetGroupResponse' value with any optional fields omitted.
mkModifyCacheSubnetGroupResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ModifyCacheSubnetGroupResponse
mkModifyCacheSubnetGroupResponse responseStatus
  = ModifyCacheSubnetGroupResponse'{cacheSubnetGroup = Core.Nothing,
                                    responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'cacheSubnetGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcsgrrsCacheSubnetGroup :: Lens.Lens' ModifyCacheSubnetGroupResponse (Core.Maybe Types.CacheSubnetGroup)
mcsgrrsCacheSubnetGroup = Lens.field @"cacheSubnetGroup"
{-# INLINEABLE mcsgrrsCacheSubnetGroup #-}
{-# DEPRECATED cacheSubnetGroup "Use generic-lens or generic-optics with 'cacheSubnetGroup' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcsgrrsResponseStatus :: Lens.Lens' ModifyCacheSubnetGroupResponse Core.Int
mcsgrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE mcsgrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
