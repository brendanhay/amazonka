{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
--
--
module Network.AWS.CloudFront.UpdateCachePolicy
    (
    -- * Creating a request
      UpdateCachePolicy (..)
    , mkUpdateCachePolicy
    -- ** Request lenses
    , ucpCachePolicyConfig
    , ucpId
    , ucpIfMatch

    -- * Destructuring the response
    , UpdateCachePolicyResponse (..)
    , mkUpdateCachePolicyResponse
    -- ** Response lenses
    , ucprrsCachePolicy
    , ucprrsETag
    , ucprrsResponseStatus
    ) where

import qualified Network.AWS.CloudFront.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateCachePolicy' smart constructor.
data UpdateCachePolicy = UpdateCachePolicy'
  { cachePolicyConfig :: Types.CachePolicyConfig
    -- ^ A cache policy configuration.
  , id :: Core.Text
    -- ^ The unique identifier for the cache policy that you are updating. The identifier is returned in a cache behavior’s @CachePolicyId@ field in the response to @GetDistributionConfig@ .
  , ifMatch :: Core.Maybe Core.Text
    -- ^ The version of the cache policy that you are updating. The version is returned in the cache policy’s @ETag@ field in the response to @GetCachePolicyConfig@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateCachePolicy' value with any optional fields omitted.
mkUpdateCachePolicy
    :: Types.CachePolicyConfig -- ^ 'cachePolicyConfig'
    -> Core.Text -- ^ 'id'
    -> UpdateCachePolicy
mkUpdateCachePolicy cachePolicyConfig id
  = UpdateCachePolicy'{cachePolicyConfig, id, ifMatch = Core.Nothing}

-- | A cache policy configuration.
--
-- /Note:/ Consider using 'cachePolicyConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucpCachePolicyConfig :: Lens.Lens' UpdateCachePolicy Types.CachePolicyConfig
ucpCachePolicyConfig = Lens.field @"cachePolicyConfig"
{-# INLINEABLE ucpCachePolicyConfig #-}
{-# DEPRECATED cachePolicyConfig "Use generic-lens or generic-optics with 'cachePolicyConfig' instead"  #-}

-- | The unique identifier for the cache policy that you are updating. The identifier is returned in a cache behavior’s @CachePolicyId@ field in the response to @GetDistributionConfig@ .
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucpId :: Lens.Lens' UpdateCachePolicy Core.Text
ucpId = Lens.field @"id"
{-# INLINEABLE ucpId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

-- | The version of the cache policy that you are updating. The version is returned in the cache policy’s @ETag@ field in the response to @GetCachePolicyConfig@ .
--
-- /Note:/ Consider using 'ifMatch' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucpIfMatch :: Lens.Lens' UpdateCachePolicy (Core.Maybe Core.Text)
ucpIfMatch = Lens.field @"ifMatch"
{-# INLINEABLE ucpIfMatch #-}
{-# DEPRECATED ifMatch "Use generic-lens or generic-optics with 'ifMatch' instead"  #-}

instance Core.ToQuery UpdateCachePolicy where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdateCachePolicy where
        toHeaders UpdateCachePolicy{..} = Core.toHeaders "If-Match" ifMatch

instance Core.AWSRequest UpdateCachePolicy where
        type Rs UpdateCachePolicy = UpdateCachePolicyResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.PUT,
                         Core._rqPath = "/2020-05-31/cache-policy/" Core.<> Core.toText id,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toXMLBody (Core.toXMLDocument x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXML
              (\ s h x ->
                 UpdateCachePolicyResponse' Core.<$>
                   (Core.parseXML x) Core.<*> Core.parseHeaderMaybe "ETag" h Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkUpdateCachePolicyResponse' smart constructor.
data UpdateCachePolicyResponse = UpdateCachePolicyResponse'
  { cachePolicy :: Core.Maybe Types.CachePolicy
    -- ^ A cache policy.
  , eTag :: Core.Maybe Core.Text
    -- ^ The current version of the cache policy.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'UpdateCachePolicyResponse' value with any optional fields omitted.
mkUpdateCachePolicyResponse
    :: Core.Int -- ^ 'responseStatus'
    -> UpdateCachePolicyResponse
mkUpdateCachePolicyResponse responseStatus
  = UpdateCachePolicyResponse'{cachePolicy = Core.Nothing,
                               eTag = Core.Nothing, responseStatus}

-- | A cache policy.
--
-- /Note:/ Consider using 'cachePolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucprrsCachePolicy :: Lens.Lens' UpdateCachePolicyResponse (Core.Maybe Types.CachePolicy)
ucprrsCachePolicy = Lens.field @"cachePolicy"
{-# INLINEABLE ucprrsCachePolicy #-}
{-# DEPRECATED cachePolicy "Use generic-lens or generic-optics with 'cachePolicy' instead"  #-}

-- | The current version of the cache policy.
--
-- /Note:/ Consider using 'eTag' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucprrsETag :: Lens.Lens' UpdateCachePolicyResponse (Core.Maybe Core.Text)
ucprrsETag = Lens.field @"eTag"
{-# INLINEABLE ucprrsETag #-}
{-# DEPRECATED eTag "Use generic-lens or generic-optics with 'eTag' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucprrsResponseStatus :: Lens.Lens' UpdateCachePolicyResponse Core.Int
ucprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ucprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
