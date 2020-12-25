{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    DeleteCacheSecurityGroup (..),
    mkDeleteCacheSecurityGroup,

    -- ** Request lenses
    dcsgCacheSecurityGroupName,

    -- * Destructuring the response
    DeleteCacheSecurityGroupResponse (..),
    mkDeleteCacheSecurityGroupResponse,
  )
where

import qualified Network.AWS.ElastiCache.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a @DeleteCacheSecurityGroup@ operation.
--
-- /See:/ 'mkDeleteCacheSecurityGroup' smart constructor.
newtype DeleteCacheSecurityGroup = DeleteCacheSecurityGroup'
  { -- | The name of the cache security group to delete.
    cacheSecurityGroupName :: Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteCacheSecurityGroup' value with any optional fields omitted.
mkDeleteCacheSecurityGroup ::
  -- | 'cacheSecurityGroupName'
  Types.String ->
  DeleteCacheSecurityGroup
mkDeleteCacheSecurityGroup cacheSecurityGroupName =
  DeleteCacheSecurityGroup' {cacheSecurityGroupName}

-- | The name of the cache security group to delete.
--
-- /Note:/ Consider using 'cacheSecurityGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsgCacheSecurityGroupName :: Lens.Lens' DeleteCacheSecurityGroup Types.String
dcsgCacheSecurityGroupName = Lens.field @"cacheSecurityGroupName"
{-# DEPRECATED dcsgCacheSecurityGroupName "Use generic-lens or generic-optics with 'cacheSecurityGroupName' instead." #-}

instance Core.AWSRequest DeleteCacheSecurityGroup where
  type Rs DeleteCacheSecurityGroup = DeleteCacheSecurityGroupResponse
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
            ( Core.pure ("Action", "DeleteCacheSecurityGroup")
                Core.<> (Core.pure ("Version", "2015-02-02"))
                Core.<> ( Core.toQueryValue
                            "CacheSecurityGroupName"
                            cacheSecurityGroupName
                        )
            )
      }
  response = Response.receiveNull DeleteCacheSecurityGroupResponse'

-- | /See:/ 'mkDeleteCacheSecurityGroupResponse' smart constructor.
data DeleteCacheSecurityGroupResponse = DeleteCacheSecurityGroupResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteCacheSecurityGroupResponse' value with any optional fields omitted.
mkDeleteCacheSecurityGroupResponse ::
  DeleteCacheSecurityGroupResponse
mkDeleteCacheSecurityGroupResponse =
  DeleteCacheSecurityGroupResponse'
