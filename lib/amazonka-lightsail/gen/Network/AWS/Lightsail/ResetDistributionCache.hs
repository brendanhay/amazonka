{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.ResetDistributionCache
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes currently cached content from your Amazon Lightsail content delivery network (CDN) distribution.
--
-- After resetting the cache, the next time a content request is made, your distribution pulls, serves, and caches it from the origin.
module Network.AWS.Lightsail.ResetDistributionCache
    (
    -- * Creating a request
      ResetDistributionCache (..)
    , mkResetDistributionCache
    -- ** Request lenses
    , rdcDistributionName

    -- * Destructuring the response
    , ResetDistributionCacheResponse (..)
    , mkResetDistributionCacheResponse
    -- ** Response lenses
    , rdcrrsCreateTime
    , rdcrrsOperation
    , rdcrrsStatus
    , rdcrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Lightsail.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkResetDistributionCache' smart constructor.
newtype ResetDistributionCache = ResetDistributionCache'
  { distributionName :: Core.Maybe Types.ResourceName
    -- ^ The name of the distribution for which to reset cache.
--
-- Use the @GetDistributions@ action to get a list of distribution names that you can specify.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'ResetDistributionCache' value with any optional fields omitted.
mkResetDistributionCache
    :: ResetDistributionCache
mkResetDistributionCache
  = ResetDistributionCache'{distributionName = Core.Nothing}

-- | The name of the distribution for which to reset cache.
--
-- Use the @GetDistributions@ action to get a list of distribution names that you can specify.
--
-- /Note:/ Consider using 'distributionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdcDistributionName :: Lens.Lens' ResetDistributionCache (Core.Maybe Types.ResourceName)
rdcDistributionName = Lens.field @"distributionName"
{-# INLINEABLE rdcDistributionName #-}
{-# DEPRECATED distributionName "Use generic-lens or generic-optics with 'distributionName' instead"  #-}

instance Core.ToQuery ResetDistributionCache where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ResetDistributionCache where
        toHeaders ResetDistributionCache{..}
          = Core.pure
              ("X-Amz-Target", "Lightsail_20161128.ResetDistributionCache")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ResetDistributionCache where
        toJSON ResetDistributionCache{..}
          = Core.object
              (Core.catMaybes
                 [("distributionName" Core..=) Core.<$> distributionName])

instance Core.AWSRequest ResetDistributionCache where
        type Rs ResetDistributionCache = ResetDistributionCacheResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ResetDistributionCacheResponse' Core.<$>
                   (x Core..:? "createTime") Core.<*> x Core..:? "operation" Core.<*>
                     x Core..:? "status"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkResetDistributionCacheResponse' smart constructor.
data ResetDistributionCacheResponse = ResetDistributionCacheResponse'
  { createTime :: Core.Maybe Core.NominalDiffTime
    -- ^ The timestamp of the reset cache request (e.g., @1479734909.17@ ) in Unix time format.
  , operation :: Core.Maybe Types.Operation
    -- ^ An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
  , status :: Core.Maybe Core.Text
    -- ^ The status of the reset cache request.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ResetDistributionCacheResponse' value with any optional fields omitted.
mkResetDistributionCacheResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ResetDistributionCacheResponse
mkResetDistributionCacheResponse responseStatus
  = ResetDistributionCacheResponse'{createTime = Core.Nothing,
                                    operation = Core.Nothing, status = Core.Nothing, responseStatus}

-- | The timestamp of the reset cache request (e.g., @1479734909.17@ ) in Unix time format.
--
-- /Note:/ Consider using 'createTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdcrrsCreateTime :: Lens.Lens' ResetDistributionCacheResponse (Core.Maybe Core.NominalDiffTime)
rdcrrsCreateTime = Lens.field @"createTime"
{-# INLINEABLE rdcrrsCreateTime #-}
{-# DEPRECATED createTime "Use generic-lens or generic-optics with 'createTime' instead"  #-}

-- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
--
-- /Note:/ Consider using 'operation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdcrrsOperation :: Lens.Lens' ResetDistributionCacheResponse (Core.Maybe Types.Operation)
rdcrrsOperation = Lens.field @"operation"
{-# INLINEABLE rdcrrsOperation #-}
{-# DEPRECATED operation "Use generic-lens or generic-optics with 'operation' instead"  #-}

-- | The status of the reset cache request.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdcrrsStatus :: Lens.Lens' ResetDistributionCacheResponse (Core.Maybe Core.Text)
rdcrrsStatus = Lens.field @"status"
{-# INLINEABLE rdcrrsStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdcrrsResponseStatus :: Lens.Lens' ResetDistributionCacheResponse Core.Int
rdcrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE rdcrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
