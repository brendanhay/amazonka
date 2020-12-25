{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.UpdateCachePolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a cache policy configuration.
--
-- When you update a cache policy configuration, all the fields are updated with the values provided in the request. You cannot update some fields independent of others. To update a cache policy configuration:
--
--     * Use @GetCachePolicyConfig@ to get the current configuration.
--
--
--     * Locally modify the fields in the cache policy configuration that you want to update.
--
--
--     * Call @UpdateCachePolicy@ by providing the entire cache policy configuration, including the fields that you modified and those that you didn’t.
module Network.AWS.CloudFront.UpdateCachePolicy
  ( -- * Creating a request
    UpdateCachePolicy (..),
    mkUpdateCachePolicy,

    -- ** Request lenses
    ucpCachePolicyConfig,
    ucpId,
    ucpIfMatch,

    -- * Destructuring the response
    UpdateCachePolicyResponse (..),
    mkUpdateCachePolicyResponse,

    -- ** Response lenses
    ucprrsCachePolicy,
    ucprrsETag,
    ucprrsResponseStatus,
  )
where

import qualified Network.AWS.CloudFront.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateCachePolicy' smart constructor.
data UpdateCachePolicy = UpdateCachePolicy'
  { -- | A cache policy configuration.
    cachePolicyConfig :: Types.CachePolicyConfig,
    -- | The unique identifier for the cache policy that you are updating. The identifier is returned in a cache behavior’s @CachePolicyId@ field in the response to @GetDistributionConfig@ .
    id :: Types.Id,
    -- | The version of the cache policy that you are updating. The version is returned in the cache policy’s @ETag@ field in the response to @GetCachePolicyConfig@ .
    ifMatch :: Core.Maybe Types.IfMatch
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateCachePolicy' value with any optional fields omitted.
mkUpdateCachePolicy ::
  -- | 'cachePolicyConfig'
  Types.CachePolicyConfig ->
  -- | 'id'
  Types.Id ->
  UpdateCachePolicy
mkUpdateCachePolicy cachePolicyConfig id =
  UpdateCachePolicy' {cachePolicyConfig, id, ifMatch = Core.Nothing}

-- | A cache policy configuration.
--
-- /Note:/ Consider using 'cachePolicyConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucpCachePolicyConfig :: Lens.Lens' UpdateCachePolicy Types.CachePolicyConfig
ucpCachePolicyConfig = Lens.field @"cachePolicyConfig"
{-# DEPRECATED ucpCachePolicyConfig "Use generic-lens or generic-optics with 'cachePolicyConfig' instead." #-}

-- | The unique identifier for the cache policy that you are updating. The identifier is returned in a cache behavior’s @CachePolicyId@ field in the response to @GetDistributionConfig@ .
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucpId :: Lens.Lens' UpdateCachePolicy Types.Id
ucpId = Lens.field @"id"
{-# DEPRECATED ucpId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The version of the cache policy that you are updating. The version is returned in the cache policy’s @ETag@ field in the response to @GetCachePolicyConfig@ .
--
-- /Note:/ Consider using 'ifMatch' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucpIfMatch :: Lens.Lens' UpdateCachePolicy (Core.Maybe Types.IfMatch)
ucpIfMatch = Lens.field @"ifMatch"
{-# DEPRECATED ucpIfMatch "Use generic-lens or generic-optics with 'ifMatch' instead." #-}

instance Core.AWSRequest UpdateCachePolicy where
  type Rs UpdateCachePolicy = UpdateCachePolicyResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.PUT,
        Core._rqPath =
          Core.rawPath
            ("/2020-05-31/cache-policy/" Core.<> (Core.toText id)),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.toHeaders "If-Match" ifMatch,
        Core._rqBody = Core.toXMLBody x
      }
  response =
    Response.receiveXML
      ( \s h x ->
          UpdateCachePolicyResponse'
            Core.<$> (Core.parseXML x)
            Core.<*> (Core.parseHeaderMaybe "ETag" h)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkUpdateCachePolicyResponse' smart constructor.
data UpdateCachePolicyResponse = UpdateCachePolicyResponse'
  { -- | A cache policy.
    cachePolicy :: Core.Maybe Types.CachePolicy,
    -- | The current version of the cache policy.
    eTag :: Core.Maybe Types.ETag,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'UpdateCachePolicyResponse' value with any optional fields omitted.
mkUpdateCachePolicyResponse ::
  -- | 'responseStatus'
  Core.Int ->
  UpdateCachePolicyResponse
mkUpdateCachePolicyResponse responseStatus =
  UpdateCachePolicyResponse'
    { cachePolicy = Core.Nothing,
      eTag = Core.Nothing,
      responseStatus
    }

-- | A cache policy.
--
-- /Note:/ Consider using 'cachePolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucprrsCachePolicy :: Lens.Lens' UpdateCachePolicyResponse (Core.Maybe Types.CachePolicy)
ucprrsCachePolicy = Lens.field @"cachePolicy"
{-# DEPRECATED ucprrsCachePolicy "Use generic-lens or generic-optics with 'cachePolicy' instead." #-}

-- | The current version of the cache policy.
--
-- /Note:/ Consider using 'eTag' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucprrsETag :: Lens.Lens' UpdateCachePolicyResponse (Core.Maybe Types.ETag)
ucprrsETag = Lens.field @"eTag"
{-# DEPRECATED ucprrsETag "Use generic-lens or generic-optics with 'eTag' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucprrsResponseStatus :: Lens.Lens' UpdateCachePolicyResponse Core.Int
ucprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ucprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
