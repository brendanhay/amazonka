{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.BatchGetBuildBatches
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about one or more batch builds.
module Network.AWS.CodeBuild.BatchGetBuildBatches
    (
    -- * Creating a request
      BatchGetBuildBatches (..)
    , mkBatchGetBuildBatches
    -- ** Request lenses
    , bgbbIds

    -- * Destructuring the response
    , BatchGetBuildBatchesResponse (..)
    , mkBatchGetBuildBatchesResponse
    -- ** Response lenses
    , bgbbrrsBuildBatches
    , bgbbrrsBuildBatchesNotFound
    , bgbbrrsResponseStatus
    ) where

import qualified Network.AWS.CodeBuild.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkBatchGetBuildBatches' smart constructor.
newtype BatchGetBuildBatches = BatchGetBuildBatches'
  { ids :: [Types.NonEmptyString]
    -- ^ An array that contains the batch build identifiers to retrieve.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'BatchGetBuildBatches' value with any optional fields omitted.
mkBatchGetBuildBatches
    :: BatchGetBuildBatches
mkBatchGetBuildBatches = BatchGetBuildBatches'{ids = Core.mempty}

-- | An array that contains the batch build identifiers to retrieve.
--
-- /Note:/ Consider using 'ids' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgbbIds :: Lens.Lens' BatchGetBuildBatches [Types.NonEmptyString]
bgbbIds = Lens.field @"ids"
{-# INLINEABLE bgbbIds #-}
{-# DEPRECATED ids "Use generic-lens or generic-optics with 'ids' instead"  #-}

instance Core.ToQuery BatchGetBuildBatches where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders BatchGetBuildBatches where
        toHeaders BatchGetBuildBatches{..}
          = Core.pure
              ("X-Amz-Target", "CodeBuild_20161006.BatchGetBuildBatches")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON BatchGetBuildBatches where
        toJSON BatchGetBuildBatches{..}
          = Core.object (Core.catMaybes [Core.Just ("ids" Core..= ids)])

instance Core.AWSRequest BatchGetBuildBatches where
        type Rs BatchGetBuildBatches = BatchGetBuildBatchesResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 BatchGetBuildBatchesResponse' Core.<$>
                   (x Core..:? "buildBatches") Core.<*>
                     x Core..:? "buildBatchesNotFound"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkBatchGetBuildBatchesResponse' smart constructor.
data BatchGetBuildBatchesResponse = BatchGetBuildBatchesResponse'
  { buildBatches :: Core.Maybe [Types.BuildBatch]
    -- ^ An array of @BuildBatch@ objects that represent the retrieved batch builds.
  , buildBatchesNotFound :: Core.Maybe [Types.NonEmptyString]
    -- ^ An array that contains the identifiers of any batch builds that are not found.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'BatchGetBuildBatchesResponse' value with any optional fields omitted.
mkBatchGetBuildBatchesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> BatchGetBuildBatchesResponse
mkBatchGetBuildBatchesResponse responseStatus
  = BatchGetBuildBatchesResponse'{buildBatches = Core.Nothing,
                                  buildBatchesNotFound = Core.Nothing, responseStatus}

-- | An array of @BuildBatch@ objects that represent the retrieved batch builds.
--
-- /Note:/ Consider using 'buildBatches' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgbbrrsBuildBatches :: Lens.Lens' BatchGetBuildBatchesResponse (Core.Maybe [Types.BuildBatch])
bgbbrrsBuildBatches = Lens.field @"buildBatches"
{-# INLINEABLE bgbbrrsBuildBatches #-}
{-# DEPRECATED buildBatches "Use generic-lens or generic-optics with 'buildBatches' instead"  #-}

-- | An array that contains the identifiers of any batch builds that are not found.
--
-- /Note:/ Consider using 'buildBatchesNotFound' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgbbrrsBuildBatchesNotFound :: Lens.Lens' BatchGetBuildBatchesResponse (Core.Maybe [Types.NonEmptyString])
bgbbrrsBuildBatchesNotFound = Lens.field @"buildBatchesNotFound"
{-# INLINEABLE bgbbrrsBuildBatchesNotFound #-}
{-# DEPRECATED buildBatchesNotFound "Use generic-lens or generic-optics with 'buildBatchesNotFound' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgbbrrsResponseStatus :: Lens.Lens' BatchGetBuildBatchesResponse Core.Int
bgbbrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE bgbbrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
