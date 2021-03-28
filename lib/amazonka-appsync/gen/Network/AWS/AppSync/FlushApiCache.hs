{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppSync.FlushApiCache
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Flushes an @ApiCache@ object.
module Network.AWS.AppSync.FlushApiCache
    (
    -- * Creating a request
      FlushApiCache (..)
    , mkFlushApiCache
    -- ** Request lenses
    , facApiId

    -- * Destructuring the response
    , FlushApiCacheResponse (..)
    , mkFlushApiCacheResponse
    -- ** Response lenses
    , facrrsResponseStatus
    ) where

import qualified Network.AWS.AppSync.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a @FlushApiCache@ operation.
--
-- /See:/ 'mkFlushApiCache' smart constructor.
newtype FlushApiCache = FlushApiCache'
  { apiId :: Core.Text
    -- ^ The API ID.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'FlushApiCache' value with any optional fields omitted.
mkFlushApiCache
    :: Core.Text -- ^ 'apiId'
    -> FlushApiCache
mkFlushApiCache apiId = FlushApiCache'{apiId}

-- | The API ID.
--
-- /Note:/ Consider using 'apiId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
facApiId :: Lens.Lens' FlushApiCache Core.Text
facApiId = Lens.field @"apiId"
{-# INLINEABLE facApiId #-}
{-# DEPRECATED apiId "Use generic-lens or generic-optics with 'apiId' instead"  #-}

instance Core.ToQuery FlushApiCache where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders FlushApiCache where
        toHeaders FlushApiCache{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.AWSRequest FlushApiCache where
        type Rs FlushApiCache = FlushApiCacheResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.DELETE,
                         Core._rqPath =
                           "/v1/apis/" Core.<> Core.toText apiId Core.<> "/FlushCache",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 FlushApiCacheResponse' Core.<$> (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | Represents the output of a @FlushApiCache@ operation.
--
-- /See:/ 'mkFlushApiCacheResponse' smart constructor.
newtype FlushApiCacheResponse = FlushApiCacheResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'FlushApiCacheResponse' value with any optional fields omitted.
mkFlushApiCacheResponse
    :: Core.Int -- ^ 'responseStatus'
    -> FlushApiCacheResponse
mkFlushApiCacheResponse responseStatus
  = FlushApiCacheResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
facrrsResponseStatus :: Lens.Lens' FlushApiCacheResponse Core.Int
facrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE facrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
