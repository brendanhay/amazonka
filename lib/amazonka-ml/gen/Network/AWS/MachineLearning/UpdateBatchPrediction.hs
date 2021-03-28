{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MachineLearning.UpdateBatchPrediction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the @BatchPredictionName@ of a @BatchPrediction@ .
--
-- You can use the @GetBatchPrediction@ operation to view the contents of the updated data element.
module Network.AWS.MachineLearning.UpdateBatchPrediction
    (
    -- * Creating a request
      UpdateBatchPrediction (..)
    , mkUpdateBatchPrediction
    -- ** Request lenses
    , ubpBatchPredictionId
    , ubpBatchPredictionName

    -- * Destructuring the response
    , UpdateBatchPredictionResponse (..)
    , mkUpdateBatchPredictionResponse
    -- ** Response lenses
    , ubprrsBatchPredictionId
    , ubprrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MachineLearning.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateBatchPrediction' smart constructor.
data UpdateBatchPrediction = UpdateBatchPrediction'
  { batchPredictionId :: Types.BatchPredictionId
    -- ^ The ID assigned to the @BatchPrediction@ during creation.
  , batchPredictionName :: Types.EntityName
    -- ^ A new user-supplied name or description of the @BatchPrediction@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateBatchPrediction' value with any optional fields omitted.
mkUpdateBatchPrediction
    :: Types.BatchPredictionId -- ^ 'batchPredictionId'
    -> Types.EntityName -- ^ 'batchPredictionName'
    -> UpdateBatchPrediction
mkUpdateBatchPrediction batchPredictionId batchPredictionName
  = UpdateBatchPrediction'{batchPredictionId, batchPredictionName}

-- | The ID assigned to the @BatchPrediction@ during creation.
--
-- /Note:/ Consider using 'batchPredictionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ubpBatchPredictionId :: Lens.Lens' UpdateBatchPrediction Types.BatchPredictionId
ubpBatchPredictionId = Lens.field @"batchPredictionId"
{-# INLINEABLE ubpBatchPredictionId #-}
{-# DEPRECATED batchPredictionId "Use generic-lens or generic-optics with 'batchPredictionId' instead"  #-}

-- | A new user-supplied name or description of the @BatchPrediction@ .
--
-- /Note:/ Consider using 'batchPredictionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ubpBatchPredictionName :: Lens.Lens' UpdateBatchPrediction Types.EntityName
ubpBatchPredictionName = Lens.field @"batchPredictionName"
{-# INLINEABLE ubpBatchPredictionName #-}
{-# DEPRECATED batchPredictionName "Use generic-lens or generic-optics with 'batchPredictionName' instead"  #-}

instance Core.ToQuery UpdateBatchPrediction where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdateBatchPrediction where
        toHeaders UpdateBatchPrediction{..}
          = Core.pure
              ("X-Amz-Target", "AmazonML_20141212.UpdateBatchPrediction")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON UpdateBatchPrediction where
        toJSON UpdateBatchPrediction{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("BatchPredictionId" Core..= batchPredictionId),
                  Core.Just ("BatchPredictionName" Core..= batchPredictionName)])

instance Core.AWSRequest UpdateBatchPrediction where
        type Rs UpdateBatchPrediction = UpdateBatchPredictionResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 UpdateBatchPredictionResponse' Core.<$>
                   (x Core..:? "BatchPredictionId") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Represents the output of an @UpdateBatchPrediction@ operation.
--
-- You can see the updated content by using the @GetBatchPrediction@ operation.
--
-- /See:/ 'mkUpdateBatchPredictionResponse' smart constructor.
data UpdateBatchPredictionResponse = UpdateBatchPredictionResponse'
  { batchPredictionId :: Core.Maybe Types.BatchPredictionId
    -- ^ The ID assigned to the @BatchPrediction@ during creation. This value should be identical to the value of the @BatchPredictionId@ in the request.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateBatchPredictionResponse' value with any optional fields omitted.
mkUpdateBatchPredictionResponse
    :: Core.Int -- ^ 'responseStatus'
    -> UpdateBatchPredictionResponse
mkUpdateBatchPredictionResponse responseStatus
  = UpdateBatchPredictionResponse'{batchPredictionId = Core.Nothing,
                                   responseStatus}

-- | The ID assigned to the @BatchPrediction@ during creation. This value should be identical to the value of the @BatchPredictionId@ in the request.
--
-- /Note:/ Consider using 'batchPredictionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ubprrsBatchPredictionId :: Lens.Lens' UpdateBatchPredictionResponse (Core.Maybe Types.BatchPredictionId)
ubprrsBatchPredictionId = Lens.field @"batchPredictionId"
{-# INLINEABLE ubprrsBatchPredictionId #-}
{-# DEPRECATED batchPredictionId "Use generic-lens or generic-optics with 'batchPredictionId' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ubprrsResponseStatus :: Lens.Lens' UpdateBatchPredictionResponse Core.Int
ubprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ubprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
