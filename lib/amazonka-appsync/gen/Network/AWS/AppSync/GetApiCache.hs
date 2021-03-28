{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppSync.GetApiCache
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves an @ApiCache@ object.
module Network.AWS.AppSync.GetApiCache
    (
    -- * Creating a request
      GetApiCache (..)
    , mkGetApiCache
    -- ** Request lenses
    , gacApiId

    -- * Destructuring the response
    , GetApiCacheResponse (..)
    , mkGetApiCacheResponse
    -- ** Response lenses
    , gacrrsApiCache
    , gacrrsResponseStatus
    ) where

import qualified Network.AWS.AppSync.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a @GetApiCache@ operation.
--
-- /See:/ 'mkGetApiCache' smart constructor.
newtype GetApiCache = GetApiCache'
  { apiId :: Core.Text
    -- ^ The API ID.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetApiCache' value with any optional fields omitted.
mkGetApiCache
    :: Core.Text -- ^ 'apiId'
    -> GetApiCache
mkGetApiCache apiId = GetApiCache'{apiId}

-- | The API ID.
--
-- /Note:/ Consider using 'apiId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gacApiId :: Lens.Lens' GetApiCache Core.Text
gacApiId = Lens.field @"apiId"
{-# INLINEABLE gacApiId #-}
{-# DEPRECATED apiId "Use generic-lens or generic-optics with 'apiId' instead"  #-}

instance Core.ToQuery GetApiCache where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetApiCache where
        toHeaders GetApiCache{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.AWSRequest GetApiCache where
        type Rs GetApiCache = GetApiCacheResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath =
                           "/v1/apis/" Core.<> Core.toText apiId Core.<> "/ApiCaches",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetApiCacheResponse' Core.<$>
                   (x Core..:? "apiCache") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Represents the output of a @GetApiCache@ operation.
--
-- /See:/ 'mkGetApiCacheResponse' smart constructor.
data GetApiCacheResponse = GetApiCacheResponse'
  { apiCache :: Core.Maybe Types.ApiCache
    -- ^ The @ApiCache@ object.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetApiCacheResponse' value with any optional fields omitted.
mkGetApiCacheResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetApiCacheResponse
mkGetApiCacheResponse responseStatus
  = GetApiCacheResponse'{apiCache = Core.Nothing, responseStatus}

-- | The @ApiCache@ object.
--
-- /Note:/ Consider using 'apiCache' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gacrrsApiCache :: Lens.Lens' GetApiCacheResponse (Core.Maybe Types.ApiCache)
gacrrsApiCache = Lens.field @"apiCache"
{-# INLINEABLE gacrrsApiCache #-}
{-# DEPRECATED apiCache "Use generic-lens or generic-optics with 'apiCache' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gacrrsResponseStatus :: Lens.Lens' GetApiCacheResponse Core.Int
gacrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gacrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
