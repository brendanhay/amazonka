{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.GetCachePolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a cache policy, including the following metadata:
--
--
--     * The policy’s identifier.
--
--
--     * The date and time when the policy was last modified.
--
--
-- To get a cache policy, you must provide the policy’s identifier. If the cache policy is attached to a distribution’s cache behavior, you can get the policy’s identifier using @ListDistributions@ or @GetDistribution@ . If the cache policy is not attached to a cache behavior, you can get the identifier using @ListCachePolicies@ .
module Network.AWS.CloudFront.GetCachePolicy
    (
    -- * Creating a request
      GetCachePolicy (..)
    , mkGetCachePolicy
    -- ** Request lenses
    , gcpId

    -- * Destructuring the response
    , GetCachePolicyResponse (..)
    , mkGetCachePolicyResponse
    -- ** Response lenses
    , gcprrsCachePolicy
    , gcprrsETag
    , gcprrsResponseStatus
    ) where

import qualified Network.AWS.CloudFront.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetCachePolicy' smart constructor.
newtype GetCachePolicy = GetCachePolicy'
  { id :: Core.Text
    -- ^ The unique identifier for the cache policy. If the cache policy is attached to a distribution’s cache behavior, you can get the policy’s identifier using @ListDistributions@ or @GetDistribution@ . If the cache policy is not attached to a cache behavior, you can get the identifier using @ListCachePolicies@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetCachePolicy' value with any optional fields omitted.
mkGetCachePolicy
    :: Core.Text -- ^ 'id'
    -> GetCachePolicy
mkGetCachePolicy id = GetCachePolicy'{id}

-- | The unique identifier for the cache policy. If the cache policy is attached to a distribution’s cache behavior, you can get the policy’s identifier using @ListDistributions@ or @GetDistribution@ . If the cache policy is not attached to a cache behavior, you can get the identifier using @ListCachePolicies@ .
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcpId :: Lens.Lens' GetCachePolicy Core.Text
gcpId = Lens.field @"id"
{-# INLINEABLE gcpId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

instance Core.ToQuery GetCachePolicy where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetCachePolicy where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest GetCachePolicy where
        type Rs GetCachePolicy = GetCachePolicyResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath = "/2020-05-31/cache-policy/" Core.<> Core.toText id,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXML
              (\ s h x ->
                 GetCachePolicyResponse' Core.<$>
                   (Core.parseXML x) Core.<*> Core.parseHeaderMaybe "ETag" h Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetCachePolicyResponse' smart constructor.
data GetCachePolicyResponse = GetCachePolicyResponse'
  { cachePolicy :: Core.Maybe Types.CachePolicy
    -- ^ The cache policy.
  , eTag :: Core.Maybe Core.Text
    -- ^ The current version of the cache policy.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'GetCachePolicyResponse' value with any optional fields omitted.
mkGetCachePolicyResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetCachePolicyResponse
mkGetCachePolicyResponse responseStatus
  = GetCachePolicyResponse'{cachePolicy = Core.Nothing,
                            eTag = Core.Nothing, responseStatus}

-- | The cache policy.
--
-- /Note:/ Consider using 'cachePolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcprrsCachePolicy :: Lens.Lens' GetCachePolicyResponse (Core.Maybe Types.CachePolicy)
gcprrsCachePolicy = Lens.field @"cachePolicy"
{-# INLINEABLE gcprrsCachePolicy #-}
{-# DEPRECATED cachePolicy "Use generic-lens or generic-optics with 'cachePolicy' instead"  #-}

-- | The current version of the cache policy.
--
-- /Note:/ Consider using 'eTag' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcprrsETag :: Lens.Lens' GetCachePolicyResponse (Core.Maybe Core.Text)
gcprrsETag = Lens.field @"eTag"
{-# INLINEABLE gcprrsETag #-}
{-# DEPRECATED eTag "Use generic-lens or generic-optics with 'eTag' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcprrsResponseStatus :: Lens.Lens' GetCachePolicyResponse Core.Int
gcprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gcprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
