{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.DeleteCachePolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a cache policy.
--
-- You cannot delete a cache policy if it’s attached to a cache behavior. First update your distributions to remove the cache policy from all cache behaviors, then delete the cache policy.
-- To delete a cache policy, you must provide the policy’s identifier and version. To get these values, you can use @ListCachePolicies@ or @GetCachePolicy@ .
module Network.AWS.CloudFront.DeleteCachePolicy
  ( -- * Creating a request
    DeleteCachePolicy (..),
    mkDeleteCachePolicy,

    -- ** Request lenses
    dcpId,
    dcpIfMatch,

    -- * Destructuring the response
    DeleteCachePolicyResponse (..),
    mkDeleteCachePolicyResponse,
  )
where

import qualified Network.AWS.CloudFront.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteCachePolicy' smart constructor.
data DeleteCachePolicy = DeleteCachePolicy'
  { -- | The unique identifier for the cache policy that you are deleting. To get the identifier, you can use @ListCachePolicies@ .
    id :: Types.Id,
    -- | The version of the cache policy that you are deleting. The version is the cache policy’s @ETag@ value, which you can get using @ListCachePolicies@ , @GetCachePolicy@ , or @GetCachePolicyConfig@ .
    ifMatch :: Core.Maybe Types.IfMatch
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteCachePolicy' value with any optional fields omitted.
mkDeleteCachePolicy ::
  -- | 'id'
  Types.Id ->
  DeleteCachePolicy
mkDeleteCachePolicy id =
  DeleteCachePolicy' {id, ifMatch = Core.Nothing}

-- | The unique identifier for the cache policy that you are deleting. To get the identifier, you can use @ListCachePolicies@ .
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcpId :: Lens.Lens' DeleteCachePolicy Types.Id
dcpId = Lens.field @"id"
{-# DEPRECATED dcpId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The version of the cache policy that you are deleting. The version is the cache policy’s @ETag@ value, which you can get using @ListCachePolicies@ , @GetCachePolicy@ , or @GetCachePolicyConfig@ .
--
-- /Note:/ Consider using 'ifMatch' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcpIfMatch :: Lens.Lens' DeleteCachePolicy (Core.Maybe Types.IfMatch)
dcpIfMatch = Lens.field @"ifMatch"
{-# DEPRECATED dcpIfMatch "Use generic-lens or generic-optics with 'ifMatch' instead." #-}

instance Core.AWSRequest DeleteCachePolicy where
  type Rs DeleteCachePolicy = DeleteCachePolicyResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.DELETE,
        Core._rqPath =
          Core.rawPath
            ("/2020-05-31/cache-policy/" Core.<> (Core.toText id)),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.toHeaders "If-Match" ifMatch,
        Core._rqBody = ""
      }
  response = Response.receiveNull DeleteCachePolicyResponse'

-- | /See:/ 'mkDeleteCachePolicyResponse' smart constructor.
data DeleteCachePolicyResponse = DeleteCachePolicyResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteCachePolicyResponse' value with any optional fields omitted.
mkDeleteCachePolicyResponse ::
  DeleteCachePolicyResponse
mkDeleteCachePolicyResponse = DeleteCachePolicyResponse'
