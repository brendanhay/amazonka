{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppSync.CreateApiCache
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a cache for the GraphQL API.
module Network.AWS.AppSync.CreateApiCache
    (
    -- * Creating a request
      CreateApiCache (..)
    , mkCreateApiCache
    -- ** Request lenses
    , cacApiId
    , cacTtl
    , cacApiCachingBehavior
    , cacType
    , cacAtRestEncryptionEnabled
    , cacTransitEncryptionEnabled

    -- * Destructuring the response
    , CreateApiCacheResponse (..)
    , mkCreateApiCacheResponse
    -- ** Response lenses
    , cacrrsApiCache
    , cacrrsResponseStatus
    ) where

import qualified Network.AWS.AppSync.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a @CreateApiCache@ operation.
--
-- /See:/ 'mkCreateApiCache' smart constructor.
data CreateApiCache = CreateApiCache'
  { apiId :: Core.Text
    -- ^ The GraphQL API Id.
  , ttl :: Core.Integer
    -- ^ TTL in seconds for cache entries.
--
-- Valid values are between 1 and 3600 seconds.
  , apiCachingBehavior :: Types.ApiCachingBehavior
    -- ^ Caching behavior.
--
--
--     * __FULL_REQUEST_CACHING__ : All requests are fully cached.
--
--
--     * __PER_RESOLVER_CACHING__ : Individual resolvers that you specify are cached.
--
--
  , type' :: Types.ApiCacheType
    -- ^ The cache instance type. Valid values are 
--
--
--     * @SMALL@ 
--
--
--     * @MEDIUM@ 
--
--
--     * @LARGE@ 
--
--
--     * @XLARGE@ 
--
--
--     * @LARGE_2X@ 
--
--
--     * @LARGE_4X@ 
--
--
--     * @LARGE_8X@ (not available in all regions)
--
--
--     * @LARGE_12X@ 
--
--
-- Historically, instance types were identified by an EC2-style value. As of July 2020, this is deprecated, and the generic identifiers above should be used.
-- The following legacy instance types are available, but their use is discouraged:
--
--     * __T2_SMALL__ : A t2.small instance type.
--
--
--     * __T2_MEDIUM__ : A t2.medium instance type.
--
--
--     * __R4_LARGE__ : A r4.large instance type.
--
--
--     * __R4_XLARGE__ : A r4.xlarge instance type.
--
--
--     * __R4_2XLARGE__ : A r4.2xlarge instance type.
--
--
--     * __R4_4XLARGE__ : A r4.4xlarge instance type.
--
--
--     * __R4_8XLARGE__ : A r4.8xlarge instance type.
--
--
  , atRestEncryptionEnabled :: Core.Maybe Core.Bool
    -- ^ At rest encryption flag for cache. This setting cannot be updated after creation.
  , transitEncryptionEnabled :: Core.Maybe Core.Bool
    -- ^ Transit encryption flag when connecting to cache. This setting cannot be updated after creation.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateApiCache' value with any optional fields omitted.
mkCreateApiCache
    :: Core.Text -- ^ 'apiId'
    -> Core.Integer -- ^ 'ttl'
    -> Types.ApiCachingBehavior -- ^ 'apiCachingBehavior'
    -> Types.ApiCacheType -- ^ 'type\''
    -> CreateApiCache
mkCreateApiCache apiId ttl apiCachingBehavior type'
  = CreateApiCache'{apiId, ttl, apiCachingBehavior, type',
                    atRestEncryptionEnabled = Core.Nothing,
                    transitEncryptionEnabled = Core.Nothing}

-- | The GraphQL API Id.
--
-- /Note:/ Consider using 'apiId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cacApiId :: Lens.Lens' CreateApiCache Core.Text
cacApiId = Lens.field @"apiId"
{-# INLINEABLE cacApiId #-}
{-# DEPRECATED apiId "Use generic-lens or generic-optics with 'apiId' instead"  #-}

-- | TTL in seconds for cache entries.
--
-- Valid values are between 1 and 3600 seconds.
--
-- /Note:/ Consider using 'ttl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cacTtl :: Lens.Lens' CreateApiCache Core.Integer
cacTtl = Lens.field @"ttl"
{-# INLINEABLE cacTtl #-}
{-# DEPRECATED ttl "Use generic-lens or generic-optics with 'ttl' instead"  #-}

-- | Caching behavior.
--
--
--     * __FULL_REQUEST_CACHING__ : All requests are fully cached.
--
--
--     * __PER_RESOLVER_CACHING__ : Individual resolvers that you specify are cached.
--
--
--
-- /Note:/ Consider using 'apiCachingBehavior' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cacApiCachingBehavior :: Lens.Lens' CreateApiCache Types.ApiCachingBehavior
cacApiCachingBehavior = Lens.field @"apiCachingBehavior"
{-# INLINEABLE cacApiCachingBehavior #-}
{-# DEPRECATED apiCachingBehavior "Use generic-lens or generic-optics with 'apiCachingBehavior' instead"  #-}

-- | The cache instance type. Valid values are 
--
--
--     * @SMALL@ 
--
--
--     * @MEDIUM@ 
--
--
--     * @LARGE@ 
--
--
--     * @XLARGE@ 
--
--
--     * @LARGE_2X@ 
--
--
--     * @LARGE_4X@ 
--
--
--     * @LARGE_8X@ (not available in all regions)
--
--
--     * @LARGE_12X@ 
--
--
-- Historically, instance types were identified by an EC2-style value. As of July 2020, this is deprecated, and the generic identifiers above should be used.
-- The following legacy instance types are available, but their use is discouraged:
--
--     * __T2_SMALL__ : A t2.small instance type.
--
--
--     * __T2_MEDIUM__ : A t2.medium instance type.
--
--
--     * __R4_LARGE__ : A r4.large instance type.
--
--
--     * __R4_XLARGE__ : A r4.xlarge instance type.
--
--
--     * __R4_2XLARGE__ : A r4.2xlarge instance type.
--
--
--     * __R4_4XLARGE__ : A r4.4xlarge instance type.
--
--
--     * __R4_8XLARGE__ : A r4.8xlarge instance type.
--
--
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cacType :: Lens.Lens' CreateApiCache Types.ApiCacheType
cacType = Lens.field @"type'"
{-# INLINEABLE cacType #-}
{-# DEPRECATED type' "Use generic-lens or generic-optics with 'type'' instead"  #-}

-- | At rest encryption flag for cache. This setting cannot be updated after creation.
--
-- /Note:/ Consider using 'atRestEncryptionEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cacAtRestEncryptionEnabled :: Lens.Lens' CreateApiCache (Core.Maybe Core.Bool)
cacAtRestEncryptionEnabled = Lens.field @"atRestEncryptionEnabled"
{-# INLINEABLE cacAtRestEncryptionEnabled #-}
{-# DEPRECATED atRestEncryptionEnabled "Use generic-lens or generic-optics with 'atRestEncryptionEnabled' instead"  #-}

-- | Transit encryption flag when connecting to cache. This setting cannot be updated after creation.
--
-- /Note:/ Consider using 'transitEncryptionEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cacTransitEncryptionEnabled :: Lens.Lens' CreateApiCache (Core.Maybe Core.Bool)
cacTransitEncryptionEnabled = Lens.field @"transitEncryptionEnabled"
{-# INLINEABLE cacTransitEncryptionEnabled #-}
{-# DEPRECATED transitEncryptionEnabled "Use generic-lens or generic-optics with 'transitEncryptionEnabled' instead"  #-}

instance Core.ToQuery CreateApiCache where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateApiCache where
        toHeaders CreateApiCache{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CreateApiCache where
        toJSON CreateApiCache{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("ttl" Core..= ttl),
                  Core.Just ("apiCachingBehavior" Core..= apiCachingBehavior),
                  Core.Just ("type" Core..= type'),
                  ("atRestEncryptionEnabled" Core..=) Core.<$>
                    atRestEncryptionEnabled,
                  ("transitEncryptionEnabled" Core..=) Core.<$>
                    transitEncryptionEnabled])

instance Core.AWSRequest CreateApiCache where
        type Rs CreateApiCache = CreateApiCacheResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST,
                         Core._rqPath =
                           "/v1/apis/" Core.<> Core.toText apiId Core.<> "/ApiCaches",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CreateApiCacheResponse' Core.<$>
                   (x Core..:? "apiCache") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Represents the output of a @CreateApiCache@ operation.
--
-- /See:/ 'mkCreateApiCacheResponse' smart constructor.
data CreateApiCacheResponse = CreateApiCacheResponse'
  { apiCache :: Core.Maybe Types.ApiCache
    -- ^ The @ApiCache@ object.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateApiCacheResponse' value with any optional fields omitted.
mkCreateApiCacheResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateApiCacheResponse
mkCreateApiCacheResponse responseStatus
  = CreateApiCacheResponse'{apiCache = Core.Nothing, responseStatus}

-- | The @ApiCache@ object.
--
-- /Note:/ Consider using 'apiCache' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cacrrsApiCache :: Lens.Lens' CreateApiCacheResponse (Core.Maybe Types.ApiCache)
cacrrsApiCache = Lens.field @"apiCache"
{-# INLINEABLE cacrrsApiCache #-}
{-# DEPRECATED apiCache "Use generic-lens or generic-optics with 'apiCache' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cacrrsResponseStatus :: Lens.Lens' CreateApiCacheResponse Core.Int
cacrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE cacrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
