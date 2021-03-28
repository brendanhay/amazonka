{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      GetCachePolicyConfig (..)
    , mkGetCachePolicyConfig
    -- ** Request lenses
    , gcpcId

    -- * Destructuring the response
    , GetCachePolicyConfigResponse (..)
    , mkGetCachePolicyConfigResponse
    -- ** Response lenses
    , gcpcrrsCachePolicyConfig
    , gcpcrrsETag
    , gcpcrrsResponseStatus
    ) where

import qualified Network.AWS.CloudFront.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetCachePolicyConfig' smart constructor.
newtype GetCachePolicyConfig = GetCachePolicyConfig'
  { id :: Core.Text
    -- ^ The unique identifier for the cache policy. If the cache policy is attached to a distribution’s cache behavior, you can get the policy’s identifier using @ListDistributions@ or @GetDistribution@ . If the cache policy is not attached to a cache behavior, you can get the identifier using @ListCachePolicies@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetCachePolicyConfig' value with any optional fields omitted.
mkGetCachePolicyConfig
    :: Core.Text -- ^ 'id'
    -> GetCachePolicyConfig
mkGetCachePolicyConfig id = GetCachePolicyConfig'{id}

-- | The unique identifier for the cache policy. If the cache policy is attached to a distribution’s cache behavior, you can get the policy’s identifier using @ListDistributions@ or @GetDistribution@ . If the cache policy is not attached to a cache behavior, you can get the identifier using @ListCachePolicies@ .
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcpcId :: Lens.Lens' GetCachePolicyConfig Core.Text
gcpcId = Lens.field @"id"
{-# INLINEABLE gcpcId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

instance Core.ToQuery GetCachePolicyConfig where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetCachePolicyConfig where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest GetCachePolicyConfig where
        type Rs GetCachePolicyConfig = GetCachePolicyConfigResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath =
                           "/2020-05-31/cache-policy/" Core.<> Core.toText id Core.<>
                             "/config",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXML
              (\ s h x ->
                 GetCachePolicyConfigResponse' Core.<$>
                   (Core.parseXML x) Core.<*> Core.parseHeaderMaybe "ETag" h Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetCachePolicyConfigResponse' smart constructor.
data GetCachePolicyConfigResponse = GetCachePolicyConfigResponse'
  { cachePolicyConfig :: Core.Maybe Types.CachePolicyConfig
    -- ^ The cache policy configuration.
  , eTag :: Core.Maybe Core.Text
    -- ^ The current version of the cache policy.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetCachePolicyConfigResponse' value with any optional fields omitted.
mkGetCachePolicyConfigResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetCachePolicyConfigResponse
mkGetCachePolicyConfigResponse responseStatus
  = GetCachePolicyConfigResponse'{cachePolicyConfig = Core.Nothing,
                                  eTag = Core.Nothing, responseStatus}

-- | The cache policy configuration.
--
-- /Note:/ Consider using 'cachePolicyConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcpcrrsCachePolicyConfig :: Lens.Lens' GetCachePolicyConfigResponse (Core.Maybe Types.CachePolicyConfig)
gcpcrrsCachePolicyConfig = Lens.field @"cachePolicyConfig"
{-# INLINEABLE gcpcrrsCachePolicyConfig #-}
{-# DEPRECATED cachePolicyConfig "Use generic-lens or generic-optics with 'cachePolicyConfig' instead"  #-}

-- | The current version of the cache policy.
--
-- /Note:/ Consider using 'eTag' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcpcrrsETag :: Lens.Lens' GetCachePolicyConfigResponse (Core.Maybe Core.Text)
gcpcrrsETag = Lens.field @"eTag"
{-# INLINEABLE gcpcrrsETag #-}
{-# DEPRECATED eTag "Use generic-lens or generic-optics with 'eTag' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcpcrrsResponseStatus :: Lens.Lens' GetCachePolicyConfigResponse Core.Int
gcpcrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gcpcrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
