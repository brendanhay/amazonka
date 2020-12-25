{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.GetCachePolicyConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a cache policy configuration.
--
-- To get a cache policy configuration, you must provide the policy’s identifier. If the cache policy is attached to a distribution’s cache behavior, you can get the policy’s identifier using @ListDistributions@ or @GetDistribution@ . If the cache policy is not attached to a cache behavior, you can get the identifier using @ListCachePolicies@ .
module Network.AWS.CloudFront.GetCachePolicyConfig
  ( -- * Creating a request
    GetCachePolicyConfig (..),
    mkGetCachePolicyConfig,

    -- ** Request lenses
    gcpcId,

    -- * Destructuring the response
    GetCachePolicyConfigResponse (..),
    mkGetCachePolicyConfigResponse,

    -- ** Response lenses
    gcpcrrsCachePolicyConfig,
    gcpcrrsETag,
    gcpcrrsResponseStatus,
  )
where

import qualified Network.AWS.CloudFront.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetCachePolicyConfig' smart constructor.
newtype GetCachePolicyConfig = GetCachePolicyConfig'
  { -- | The unique identifier for the cache policy. If the cache policy is attached to a distribution’s cache behavior, you can get the policy’s identifier using @ListDistributions@ or @GetDistribution@ . If the cache policy is not attached to a cache behavior, you can get the identifier using @ListCachePolicies@ .
    id :: Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetCachePolicyConfig' value with any optional fields omitted.
mkGetCachePolicyConfig ::
  -- | 'id'
  Types.String ->
  GetCachePolicyConfig
mkGetCachePolicyConfig id = GetCachePolicyConfig' {id}

-- | The unique identifier for the cache policy. If the cache policy is attached to a distribution’s cache behavior, you can get the policy’s identifier using @ListDistributions@ or @GetDistribution@ . If the cache policy is not attached to a cache behavior, you can get the identifier using @ListCachePolicies@ .
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcpcId :: Lens.Lens' GetCachePolicyConfig Types.String
gcpcId = Lens.field @"id"
{-# DEPRECATED gcpcId "Use generic-lens or generic-optics with 'id' instead." #-}

instance Core.AWSRequest GetCachePolicyConfig where
  type Rs GetCachePolicyConfig = GetCachePolicyConfigResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath =
          Core.rawPath
            ( "/2020-05-31/cache-policy/" Core.<> (Core.toText id)
                Core.<> ("/config")
            ),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = ""
      }
  response =
    Response.receiveXML
      ( \s h x ->
          GetCachePolicyConfigResponse'
            Core.<$> (Core.parseXML x)
            Core.<*> (Core.parseHeaderMaybe "ETag" h)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetCachePolicyConfigResponse' smart constructor.
data GetCachePolicyConfigResponse = GetCachePolicyConfigResponse'
  { -- | The cache policy configuration.
    cachePolicyConfig :: Core.Maybe Types.CachePolicyConfig,
    -- | The current version of the cache policy.
    eTag :: Core.Maybe Types.String,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetCachePolicyConfigResponse' value with any optional fields omitted.
mkGetCachePolicyConfigResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetCachePolicyConfigResponse
mkGetCachePolicyConfigResponse responseStatus =
  GetCachePolicyConfigResponse'
    { cachePolicyConfig = Core.Nothing,
      eTag = Core.Nothing,
      responseStatus
    }

-- | The cache policy configuration.
--
-- /Note:/ Consider using 'cachePolicyConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcpcrrsCachePolicyConfig :: Lens.Lens' GetCachePolicyConfigResponse (Core.Maybe Types.CachePolicyConfig)
gcpcrrsCachePolicyConfig = Lens.field @"cachePolicyConfig"
{-# DEPRECATED gcpcrrsCachePolicyConfig "Use generic-lens or generic-optics with 'cachePolicyConfig' instead." #-}

-- | The current version of the cache policy.
--
-- /Note:/ Consider using 'eTag' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcpcrrsETag :: Lens.Lens' GetCachePolicyConfigResponse (Core.Maybe Types.String)
gcpcrrsETag = Lens.field @"eTag"
{-# DEPRECATED gcpcrrsETag "Use generic-lens or generic-optics with 'eTag' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcpcrrsResponseStatus :: Lens.Lens' GetCachePolicyConfigResponse Core.Int
gcpcrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gcpcrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
