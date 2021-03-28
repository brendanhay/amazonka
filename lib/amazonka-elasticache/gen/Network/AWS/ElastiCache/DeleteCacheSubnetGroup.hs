{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.DeleteCacheSubnetGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a cache subnet group.
module Network.AWS.ElastiCache.DeleteCacheSubnetGroup
    (
    -- * Creating a request
      DeleteCacheSubnetGroup (..)
    , mkDeleteCacheSubnetGroup
    -- ** Request lenses
    , dCacheSubnetGroupName

    -- * Destructuring the response
    , DeleteCacheSubnetGroupResponse (..)
    , mkDeleteCacheSubnetGroupResponse
    ) where

import qualified Network.AWS.ElastiCache.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a @DeleteCacheSubnetGroup@ operation.
--
-- /See:/ 'mkDeleteCacheSubnetGroup' smart constructor.
newtype DeleteCacheSubnetGroup = DeleteCacheSubnetGroup'
  { cacheSubnetGroupName :: Core.Text
    -- ^ The name of the cache subnet group to delete.
--
-- Constraints: Must contain no more than 255 alphanumeric characters or hyphens.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteCacheSubnetGroup' value with any optional fields omitted.
mkDeleteCacheSubnetGroup
    :: Core.Text -- ^ 'cacheSubnetGroupName'
    -> DeleteCacheSubnetGroup
mkDeleteCacheSubnetGroup cacheSubnetGroupName
  = DeleteCacheSubnetGroup'{cacheSubnetGroupName}

-- | The name of the cache subnet group to delete.
--
-- Constraints: Must contain no more than 255 alphanumeric characters or hyphens.
--
-- /Note:/ Consider using 'cacheSubnetGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dCacheSubnetGroupName :: Lens.Lens' DeleteCacheSubnetGroup Core.Text
dCacheSubnetGroupName = Lens.field @"cacheSubnetGroupName"
{-# INLINEABLE dCacheSubnetGroupName #-}
{-# DEPRECATED cacheSubnetGroupName "Use generic-lens or generic-optics with 'cacheSubnetGroupName' instead"  #-}

instance Core.ToQuery DeleteCacheSubnetGroup where
        toQuery DeleteCacheSubnetGroup{..}
          = Core.toQueryPair "Action" ("DeleteCacheSubnetGroup" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2015-02-02" :: Core.Text)
              Core.<>
              Core.toQueryPair "CacheSubnetGroupName" cacheSubnetGroupName

instance Core.ToHeaders DeleteCacheSubnetGroup where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DeleteCacheSubnetGroup where
        type Rs DeleteCacheSubnetGroup = DeleteCacheSubnetGroupResponse
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
          = Response.receiveNull DeleteCacheSubnetGroupResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteCacheSubnetGroupResponse' smart constructor.
data DeleteCacheSubnetGroupResponse = DeleteCacheSubnetGroupResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteCacheSubnetGroupResponse' value with any optional fields omitted.
mkDeleteCacheSubnetGroupResponse
    :: DeleteCacheSubnetGroupResponse
mkDeleteCacheSubnetGroupResponse = DeleteCacheSubnetGroupResponse'
