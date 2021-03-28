{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.DeleteCacheSecurityGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a cache security group.
module Network.AWS.ElastiCache.DeleteCacheSecurityGroup
    (
    -- * Creating a request
      DeleteCacheSecurityGroup (..)
    , mkDeleteCacheSecurityGroup
    -- ** Request lenses
    , dcsgCacheSecurityGroupName

    -- * Destructuring the response
    , DeleteCacheSecurityGroupResponse (..)
    , mkDeleteCacheSecurityGroupResponse
    ) where

import qualified Network.AWS.ElastiCache.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a @DeleteCacheSecurityGroup@ operation.
--
-- /See:/ 'mkDeleteCacheSecurityGroup' smart constructor.
newtype DeleteCacheSecurityGroup = DeleteCacheSecurityGroup'
  { cacheSecurityGroupName :: Core.Text
    -- ^ The name of the cache security group to delete.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteCacheSecurityGroup' value with any optional fields omitted.
mkDeleteCacheSecurityGroup
    :: Core.Text -- ^ 'cacheSecurityGroupName'
    -> DeleteCacheSecurityGroup
mkDeleteCacheSecurityGroup cacheSecurityGroupName
  = DeleteCacheSecurityGroup'{cacheSecurityGroupName}

-- | The name of the cache security group to delete.
--
-- /Note:/ Consider using 'cacheSecurityGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsgCacheSecurityGroupName :: Lens.Lens' DeleteCacheSecurityGroup Core.Text
dcsgCacheSecurityGroupName = Lens.field @"cacheSecurityGroupName"
{-# INLINEABLE dcsgCacheSecurityGroupName #-}
{-# DEPRECATED cacheSecurityGroupName "Use generic-lens or generic-optics with 'cacheSecurityGroupName' instead"  #-}

instance Core.ToQuery DeleteCacheSecurityGroup where
        toQuery DeleteCacheSecurityGroup{..}
          = Core.toQueryPair "Action"
              ("DeleteCacheSecurityGroup" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2015-02-02" :: Core.Text)
              Core.<>
              Core.toQueryPair "CacheSecurityGroupName" cacheSecurityGroupName

instance Core.ToHeaders DeleteCacheSecurityGroup where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DeleteCacheSecurityGroup where
        type Rs DeleteCacheSecurityGroup = DeleteCacheSecurityGroupResponse
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
          = Response.receiveNull DeleteCacheSecurityGroupResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteCacheSecurityGroupResponse' smart constructor.
data DeleteCacheSecurityGroupResponse = DeleteCacheSecurityGroupResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteCacheSecurityGroupResponse' value with any optional fields omitted.
mkDeleteCacheSecurityGroupResponse
    :: DeleteCacheSecurityGroupResponse
mkDeleteCacheSecurityGroupResponse
  = DeleteCacheSecurityGroupResponse'
