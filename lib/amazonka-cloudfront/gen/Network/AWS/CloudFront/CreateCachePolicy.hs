{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.CreateCachePolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a cache policy.
--
-- After you create a cache policy, you can attach it to one or more cache behaviors. When it’s attached to a cache behavior, the cache policy determines the following:
--
--     * The values that CloudFront includes in the /cache key/ . These values can include HTTP headers, cookies, and URL query strings. CloudFront uses the cache key to find an object in its cache that it can return to the viewer.
--
--
--     * The default, minimum, and maximum time to live (TTL) values that you want objects to stay in the CloudFront cache.
--
--
-- The headers, cookies, and query strings that are included in the cache key are automatically included in requests that CloudFront sends to the origin. CloudFront sends a request when it can’t find an object in its cache that matches the request’s cache key. If you want to send values to the origin but /not/ include them in the cache key, use @OriginRequestPolicy@ .
-- For more information about cache policies, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/controlling-the-cache-key.html Controlling the cache key> in the /Amazon CloudFront Developer Guide/ .
module Network.AWS.CloudFront.CreateCachePolicy
  ( -- * Creating a request
    CreateCachePolicy (..),
    mkCreateCachePolicy,

    -- ** Request lenses
    ccpCachePolicyConfig,

    -- * Destructuring the response
    CreateCachePolicyResponse (..),
    mkCreateCachePolicyResponse,

    -- ** Response lenses
    ccprrsCachePolicy,
    ccprrsETag,
    ccprrsLocation,
    ccprrsResponseStatus,
  )
where

import qualified Network.AWS.CloudFront.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateCachePolicy' smart constructor.
newtype CreateCachePolicy = CreateCachePolicy'
  { -- | A cache policy configuration.
    cachePolicyConfig :: Types.CachePolicyConfig
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'CreateCachePolicy' value with any optional fields omitted.
mkCreateCachePolicy ::
  -- | 'cachePolicyConfig'
  Types.CachePolicyConfig ->
  CreateCachePolicy
mkCreateCachePolicy cachePolicyConfig =
  CreateCachePolicy' {cachePolicyConfig}

-- | A cache policy configuration.
--
-- /Note:/ Consider using 'cachePolicyConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccpCachePolicyConfig :: Lens.Lens' CreateCachePolicy Types.CachePolicyConfig
ccpCachePolicyConfig = Lens.field @"cachePolicyConfig"
{-# DEPRECATED ccpCachePolicyConfig "Use generic-lens or generic-optics with 'cachePolicyConfig' instead." #-}

instance Core.AWSRequest CreateCachePolicy where
  type Rs CreateCachePolicy = CreateCachePolicyResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/2020-05-31/cache-policy",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = Core.toXMLBody x
      }
  response =
    Response.receiveXML
      ( \s h x ->
          CreateCachePolicyResponse'
            Core.<$> (Core.parseXML x)
            Core.<*> (Core.parseHeaderMaybe "ETag" h)
            Core.<*> (Core.parseHeaderMaybe "Location" h)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreateCachePolicyResponse' smart constructor.
data CreateCachePolicyResponse = CreateCachePolicyResponse'
  { -- | A cache policy.
    cachePolicy :: Core.Maybe Types.CachePolicy,
    -- | The current version of the cache policy.
    eTag :: Core.Maybe Types.String,
    -- | The fully qualified URI of the cache policy just created.
    location :: Core.Maybe Types.String,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'CreateCachePolicyResponse' value with any optional fields omitted.
mkCreateCachePolicyResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateCachePolicyResponse
mkCreateCachePolicyResponse responseStatus =
  CreateCachePolicyResponse'
    { cachePolicy = Core.Nothing,
      eTag = Core.Nothing,
      location = Core.Nothing,
      responseStatus
    }

-- | A cache policy.
--
-- /Note:/ Consider using 'cachePolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccprrsCachePolicy :: Lens.Lens' CreateCachePolicyResponse (Core.Maybe Types.CachePolicy)
ccprrsCachePolicy = Lens.field @"cachePolicy"
{-# DEPRECATED ccprrsCachePolicy "Use generic-lens or generic-optics with 'cachePolicy' instead." #-}

-- | The current version of the cache policy.
--
-- /Note:/ Consider using 'eTag' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccprrsETag :: Lens.Lens' CreateCachePolicyResponse (Core.Maybe Types.String)
ccprrsETag = Lens.field @"eTag"
{-# DEPRECATED ccprrsETag "Use generic-lens or generic-optics with 'eTag' instead." #-}

-- | The fully qualified URI of the cache policy just created.
--
-- /Note:/ Consider using 'location' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccprrsLocation :: Lens.Lens' CreateCachePolicyResponse (Core.Maybe Types.String)
ccprrsLocation = Lens.field @"location"
{-# DEPRECATED ccprrsLocation "Use generic-lens or generic-optics with 'location' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccprrsResponseStatus :: Lens.Lens' CreateCachePolicyResponse Core.Int
ccprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ccprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
