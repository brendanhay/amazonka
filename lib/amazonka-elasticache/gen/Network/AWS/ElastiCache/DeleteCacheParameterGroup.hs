{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.DeleteCacheParameterGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified cache parameter group. You cannot delete a cache parameter group if it is associated with any cache clusters.
module Network.AWS.ElastiCache.DeleteCacheParameterGroup
  ( -- * Creating a request
    DeleteCacheParameterGroup (..),
    mkDeleteCacheParameterGroup,

    -- ** Request lenses
    dCacheParameterGroupName,

    -- * Destructuring the response
    DeleteCacheParameterGroupResponse (..),
    mkDeleteCacheParameterGroupResponse,
  )
where

import qualified Network.AWS.ElastiCache.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a @DeleteCacheParameterGroup@ operation.
--
-- /See:/ 'mkDeleteCacheParameterGroup' smart constructor.
newtype DeleteCacheParameterGroup = DeleteCacheParameterGroup'
  { -- | The name of the cache parameter group to delete.
    cacheParameterGroupName :: Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteCacheParameterGroup' value with any optional fields omitted.
mkDeleteCacheParameterGroup ::
  -- | 'cacheParameterGroupName'
  Types.String ->
  DeleteCacheParameterGroup
mkDeleteCacheParameterGroup cacheParameterGroupName =
  DeleteCacheParameterGroup' {cacheParameterGroupName}

-- | The name of the cache parameter group to delete.
--
-- /Note:/ Consider using 'cacheParameterGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dCacheParameterGroupName :: Lens.Lens' DeleteCacheParameterGroup Types.String
dCacheParameterGroupName = Lens.field @"cacheParameterGroupName"
{-# DEPRECATED dCacheParameterGroupName "Use generic-lens or generic-optics with 'cacheParameterGroupName' instead." #-}

instance Core.AWSRequest DeleteCacheParameterGroup where
  type
    Rs DeleteCacheParameterGroup =
      DeleteCacheParameterGroupResponse
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
            ( Core.pure ("Action", "DeleteCacheParameterGroup")
                Core.<> (Core.pure ("Version", "2015-02-02"))
                Core.<> ( Core.toQueryValue
                            "CacheParameterGroupName"
                            cacheParameterGroupName
                        )
            )
      }
  response = Response.receiveNull DeleteCacheParameterGroupResponse'

-- | /See:/ 'mkDeleteCacheParameterGroupResponse' smart constructor.
data DeleteCacheParameterGroupResponse = DeleteCacheParameterGroupResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteCacheParameterGroupResponse' value with any optional fields omitted.
mkDeleteCacheParameterGroupResponse ::
  DeleteCacheParameterGroupResponse
mkDeleteCacheParameterGroupResponse =
  DeleteCacheParameterGroupResponse'
