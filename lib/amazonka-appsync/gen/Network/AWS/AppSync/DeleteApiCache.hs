{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppSync.DeleteApiCache
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an @ApiCache@ object.
module Network.AWS.AppSync.DeleteApiCache
    (
    -- * Creating a request
      DeleteApiCache (..)
    , mkDeleteApiCache
    -- ** Request lenses
    , dacApiId

    -- * Destructuring the response
    , DeleteApiCacheResponse (..)
    , mkDeleteApiCacheResponse
    -- ** Response lenses
    , dacrrsResponseStatus
    ) where

import qualified Network.AWS.AppSync.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a @DeleteApiCache@ operation.
--
-- /See:/ 'mkDeleteApiCache' smart constructor.
newtype DeleteApiCache = DeleteApiCache'
  { apiId :: Core.Text
    -- ^ The API ID.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteApiCache' value with any optional fields omitted.
mkDeleteApiCache
    :: Core.Text -- ^ 'apiId'
    -> DeleteApiCache
mkDeleteApiCache apiId = DeleteApiCache'{apiId}

-- | The API ID.
--
-- /Note:/ Consider using 'apiId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dacApiId :: Lens.Lens' DeleteApiCache Core.Text
dacApiId = Lens.field @"apiId"
{-# INLINEABLE dacApiId #-}
{-# DEPRECATED apiId "Use generic-lens or generic-optics with 'apiId' instead"  #-}

instance Core.ToQuery DeleteApiCache where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteApiCache where
        toHeaders DeleteApiCache{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.AWSRequest DeleteApiCache where
        type Rs DeleteApiCache = DeleteApiCacheResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.DELETE,
                         Core._rqPath =
                           "/v1/apis/" Core.<> Core.toText apiId Core.<> "/ApiCaches",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 DeleteApiCacheResponse' Core.<$> (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | Represents the output of a @DeleteApiCache@ operation.
--
-- /See:/ 'mkDeleteApiCacheResponse' smart constructor.
newtype DeleteApiCacheResponse = DeleteApiCacheResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteApiCacheResponse' value with any optional fields omitted.
mkDeleteApiCacheResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteApiCacheResponse
mkDeleteApiCacheResponse responseStatus
  = DeleteApiCacheResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dacrrsResponseStatus :: Lens.Lens' DeleteApiCacheResponse Core.Int
dacrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dacrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
