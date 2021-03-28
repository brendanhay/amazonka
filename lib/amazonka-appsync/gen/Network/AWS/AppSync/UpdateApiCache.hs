{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppSync.UpdateApiCache
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the cache for the GraphQL API.
module Network.AWS.AppSync.UpdateApiCache
    (
    -- * Creating a request
      UpdateApiCache (..)
    , mkUpdateApiCache
    -- ** Request lenses
    , uacApiId
    , uacTtl
    , uacApiCachingBehavior
    , uacType

    -- * Destructuring the response
    , UpdateApiCacheResponse (..)
    , mkUpdateApiCacheResponse
    -- ** Response lenses
    , uacrrsApiCache
    , uacrrsResponseStatus
    ) where

import qualified Network.AWS.AppSync.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a @UpdateApiCache@ operation.
--
-- /See:/ 'mkUpdateApiCache' smart constructor.
data UpdateApiCache = UpdateApiCache'
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
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateApiCache' value with any optional fields omitted.
mkUpdateApiCache
    :: Core.Text -- ^ 'apiId'
    -> Core.Integer -- ^ 'ttl'
    -> Types.ApiCachingBehavior -- ^ 'apiCachingBehavior'
    -> Types.ApiCacheType -- ^ 'type\''
    -> UpdateApiCache
mkUpdateApiCache apiId ttl apiCachingBehavior type'
  = UpdateApiCache'{apiId, ttl, apiCachingBehavior, type'}

-- | The GraphQL API Id.
--
-- /Note:/ Consider using 'apiId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uacApiId :: Lens.Lens' UpdateApiCache Core.Text
uacApiId = Lens.field @"apiId"
{-# INLINEABLE uacApiId #-}
{-# DEPRECATED apiId "Use generic-lens or generic-optics with 'apiId' instead"  #-}

-- | TTL in seconds for cache entries.
--
-- Valid values are between 1 and 3600 seconds.
--
-- /Note:/ Consider using 'ttl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uacTtl :: Lens.Lens' UpdateApiCache Core.Integer
uacTtl = Lens.field @"ttl"
{-# INLINEABLE uacTtl #-}
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
uacApiCachingBehavior :: Lens.Lens' UpdateApiCache Types.ApiCachingBehavior
uacApiCachingBehavior = Lens.field @"apiCachingBehavior"
{-# INLINEABLE uacApiCachingBehavior #-}
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
uacType :: Lens.Lens' UpdateApiCache Types.ApiCacheType
uacType = Lens.field @"type'"
{-# INLINEABLE uacType #-}
{-# DEPRECATED type' "Use generic-lens or generic-optics with 'type'' instead"  #-}

instance Core.ToQuery UpdateApiCache where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdateApiCache where
        toHeaders UpdateApiCache{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON UpdateApiCache where
        toJSON UpdateApiCache{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("ttl" Core..= ttl),
                  Core.Just ("apiCachingBehavior" Core..= apiCachingBehavior),
                  Core.Just ("type" Core..= type')])

instance Core.AWSRequest UpdateApiCache where
        type Rs UpdateApiCache = UpdateApiCacheResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST,
                         Core._rqPath =
                           "/v1/apis/" Core.<> Core.toText apiId Core.<> "/ApiCaches/update",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 UpdateApiCacheResponse' Core.<$>
                   (x Core..:? "apiCache") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Represents the output of a @UpdateApiCache@ operation.
--
-- /See:/ 'mkUpdateApiCacheResponse' smart constructor.
data UpdateApiCacheResponse = UpdateApiCacheResponse'
  { apiCache :: Core.Maybe Types.ApiCache
    -- ^ The @ApiCache@ object.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateApiCacheResponse' value with any optional fields omitted.
mkUpdateApiCacheResponse
    :: Core.Int -- ^ 'responseStatus'
    -> UpdateApiCacheResponse
mkUpdateApiCacheResponse responseStatus
  = UpdateApiCacheResponse'{apiCache = Core.Nothing, responseStatus}

-- | The @ApiCache@ object.
--
-- /Note:/ Consider using 'apiCache' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uacrrsApiCache :: Lens.Lens' UpdateApiCacheResponse (Core.Maybe Types.ApiCache)
uacrrsApiCache = Lens.field @"apiCache"
{-# INLINEABLE uacrrsApiCache #-}
{-# DEPRECATED apiCache "Use generic-lens or generic-optics with 'apiCache' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uacrrsResponseStatus :: Lens.Lens' UpdateApiCacheResponse Core.Int
uacrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE uacrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
