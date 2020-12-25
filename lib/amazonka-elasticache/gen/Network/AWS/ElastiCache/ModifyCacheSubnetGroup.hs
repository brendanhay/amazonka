{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    ModifyCacheSubnetGroup (..),
    mkModifyCacheSubnetGroup,

    -- ** Request lenses
    mcsgCacheSubnetGroupName,
    mcsgCacheSubnetGroupDescription,
    mcsgSubnetIds,

    -- * Destructuring the response
    ModifyCacheSubnetGroupResponse (..),
    mkModifyCacheSubnetGroupResponse,

    -- ** Response lenses
    mcsgrrsCacheSubnetGroup,
    mcsgrrsResponseStatus,
  )
where

import qualified Network.AWS.ElastiCache.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a @ModifyCacheSubnetGroup@ operation.
--
-- /See:/ 'mkModifyCacheSubnetGroup' smart constructor.
data ModifyCacheSubnetGroup = ModifyCacheSubnetGroup'
  { -- | The name for the cache subnet group. This value is stored as a lowercase string.
    --
    -- Constraints: Must contain no more than 255 alphanumeric characters or hyphens.
    -- Example: @mysubnetgroup@
    cacheSubnetGroupName :: Types.String,
    -- | A description of the cache subnet group.
    cacheSubnetGroupDescription :: Core.Maybe Types.String,
    -- | The EC2 subnet IDs for the cache subnet group.
    subnetIds :: Core.Maybe [Types.String]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ModifyCacheSubnetGroup' value with any optional fields omitted.
mkModifyCacheSubnetGroup ::
  -- | 'cacheSubnetGroupName'
  Types.String ->
  ModifyCacheSubnetGroup
mkModifyCacheSubnetGroup cacheSubnetGroupName =
  ModifyCacheSubnetGroup'
    { cacheSubnetGroupName,
      cacheSubnetGroupDescription = Core.Nothing,
      subnetIds = Core.Nothing
    }

-- | The name for the cache subnet group. This value is stored as a lowercase string.
--
-- Constraints: Must contain no more than 255 alphanumeric characters or hyphens.
-- Example: @mysubnetgroup@
--
-- /Note:/ Consider using 'cacheSubnetGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcsgCacheSubnetGroupName :: Lens.Lens' ModifyCacheSubnetGroup Types.String
mcsgCacheSubnetGroupName = Lens.field @"cacheSubnetGroupName"
{-# DEPRECATED mcsgCacheSubnetGroupName "Use generic-lens or generic-optics with 'cacheSubnetGroupName' instead." #-}

-- | A description of the cache subnet group.
--
-- /Note:/ Consider using 'cacheSubnetGroupDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcsgCacheSubnetGroupDescription :: Lens.Lens' ModifyCacheSubnetGroup (Core.Maybe Types.String)
mcsgCacheSubnetGroupDescription = Lens.field @"cacheSubnetGroupDescription"
{-# DEPRECATED mcsgCacheSubnetGroupDescription "Use generic-lens or generic-optics with 'cacheSubnetGroupDescription' instead." #-}

-- | The EC2 subnet IDs for the cache subnet group.
--
-- /Note:/ Consider using 'subnetIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcsgSubnetIds :: Lens.Lens' ModifyCacheSubnetGroup (Core.Maybe [Types.String])
mcsgSubnetIds = Lens.field @"subnetIds"
{-# DEPRECATED mcsgSubnetIds "Use generic-lens or generic-optics with 'subnetIds' instead." #-}

instance Core.AWSRequest ModifyCacheSubnetGroup where
  type Rs ModifyCacheSubnetGroup = ModifyCacheSubnetGroupResponse
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
            ( Core.pure ("Action", "ModifyCacheSubnetGroup")
                Core.<> (Core.pure ("Version", "2015-02-02"))
                Core.<> (Core.toQueryValue "CacheSubnetGroupName" cacheSubnetGroupName)
                Core.<> ( Core.toQueryValue "CacheSubnetGroupDescription"
                            Core.<$> cacheSubnetGroupDescription
                        )
                Core.<> ( Core.toQueryValue
                            "SubnetIds"
                            (Core.toQueryList "SubnetIdentifier" Core.<$> subnetIds)
                        )
            )
      }
  response =
    Response.receiveXMLWrapper
      "ModifyCacheSubnetGroupResult"
      ( \s h x ->
          ModifyCacheSubnetGroupResponse'
            Core.<$> (x Core..@? "CacheSubnetGroup")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkModifyCacheSubnetGroupResponse' smart constructor.
data ModifyCacheSubnetGroupResponse = ModifyCacheSubnetGroupResponse'
  { cacheSubnetGroup :: Core.Maybe Types.CacheSubnetGroup,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ModifyCacheSubnetGroupResponse' value with any optional fields omitted.
mkModifyCacheSubnetGroupResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ModifyCacheSubnetGroupResponse
mkModifyCacheSubnetGroupResponse responseStatus =
  ModifyCacheSubnetGroupResponse'
    { cacheSubnetGroup = Core.Nothing,
      responseStatus
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'cacheSubnetGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcsgrrsCacheSubnetGroup :: Lens.Lens' ModifyCacheSubnetGroupResponse (Core.Maybe Types.CacheSubnetGroup)
mcsgrrsCacheSubnetGroup = Lens.field @"cacheSubnetGroup"
{-# DEPRECATED mcsgrrsCacheSubnetGroup "Use generic-lens or generic-optics with 'cacheSubnetGroup' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mcsgrrsResponseStatus :: Lens.Lens' ModifyCacheSubnetGroupResponse Core.Int
mcsgrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED mcsgrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
