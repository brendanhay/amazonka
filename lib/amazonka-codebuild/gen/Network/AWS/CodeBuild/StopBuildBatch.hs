{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.StopBuildBatch
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops a running batch build.
module Network.AWS.CodeBuild.StopBuildBatch
    (
    -- * Creating a request
      StopBuildBatch (..)
    , mkStopBuildBatch
    -- ** Request lenses
    , sbbId

    -- * Destructuring the response
    , StopBuildBatchResponse (..)
    , mkStopBuildBatchResponse
    -- ** Response lenses
    , sbbrrsBuildBatch
    , sbbrrsResponseStatus
    ) where

import qualified Network.AWS.CodeBuild.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkStopBuildBatch' smart constructor.
newtype StopBuildBatch = StopBuildBatch'
  { id :: Types.Id
    -- ^ The identifier of the batch build to stop.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'StopBuildBatch' value with any optional fields omitted.
mkStopBuildBatch
    :: Types.Id -- ^ 'id'
    -> StopBuildBatch
mkStopBuildBatch id = StopBuildBatch'{id}

-- | The identifier of the batch build to stop.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbbId :: Lens.Lens' StopBuildBatch Types.Id
sbbId = Lens.field @"id"
{-# INLINEABLE sbbId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

instance Core.ToQuery StopBuildBatch where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders StopBuildBatch where
        toHeaders StopBuildBatch{..}
          = Core.pure ("X-Amz-Target", "CodeBuild_20161006.StopBuildBatch")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON StopBuildBatch where
        toJSON StopBuildBatch{..}
          = Core.object (Core.catMaybes [Core.Just ("id" Core..= id)])

instance Core.AWSRequest StopBuildBatch where
        type Rs StopBuildBatch = StopBuildBatchResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 StopBuildBatchResponse' Core.<$>
                   (x Core..:? "buildBatch") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkStopBuildBatchResponse' smart constructor.
data StopBuildBatchResponse = StopBuildBatchResponse'
  { buildBatch :: Core.Maybe Types.BuildBatch
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'StopBuildBatchResponse' value with any optional fields omitted.
mkStopBuildBatchResponse
    :: Core.Int -- ^ 'responseStatus'
    -> StopBuildBatchResponse
mkStopBuildBatchResponse responseStatus
  = StopBuildBatchResponse'{buildBatch = Core.Nothing,
                            responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'buildBatch' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbbrrsBuildBatch :: Lens.Lens' StopBuildBatchResponse (Core.Maybe Types.BuildBatch)
sbbrrsBuildBatch = Lens.field @"buildBatch"
{-# INLINEABLE sbbrrsBuildBatch #-}
{-# DEPRECATED buildBatch "Use generic-lens or generic-optics with 'buildBatch' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbbrrsResponseStatus :: Lens.Lens' StopBuildBatchResponse Core.Int
sbbrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE sbbrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
