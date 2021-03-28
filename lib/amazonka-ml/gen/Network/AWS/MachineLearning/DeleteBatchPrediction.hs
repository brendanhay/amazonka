{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MachineLearning.DeleteBatchPrediction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Assigns the DELETED status to a @BatchPrediction@ , rendering it unusable.
--
-- After using the @DeleteBatchPrediction@ operation, you can use the 'GetBatchPrediction' operation to verify that the status of the @BatchPrediction@ changed to DELETED.
-- __Caution:__ The result of the @DeleteBatchPrediction@ operation is irreversible.
module Network.AWS.MachineLearning.DeleteBatchPrediction
    (
    -- * Creating a request
      DeleteBatchPrediction (..)
    , mkDeleteBatchPrediction
    -- ** Request lenses
    , dbpBatchPredictionId

    -- * Destructuring the response
    , DeleteBatchPredictionResponse (..)
    , mkDeleteBatchPredictionResponse
    -- ** Response lenses
    , dbprrsBatchPredictionId
    , dbprrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MachineLearning.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteBatchPrediction' smart constructor.
newtype DeleteBatchPrediction = DeleteBatchPrediction'
  { batchPredictionId :: Types.BatchPredictionId
    -- ^ A user-supplied ID that uniquely identifies the @BatchPrediction@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteBatchPrediction' value with any optional fields omitted.
mkDeleteBatchPrediction
    :: Types.BatchPredictionId -- ^ 'batchPredictionId'
    -> DeleteBatchPrediction
mkDeleteBatchPrediction batchPredictionId
  = DeleteBatchPrediction'{batchPredictionId}

-- | A user-supplied ID that uniquely identifies the @BatchPrediction@ .
--
-- /Note:/ Consider using 'batchPredictionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbpBatchPredictionId :: Lens.Lens' DeleteBatchPrediction Types.BatchPredictionId
dbpBatchPredictionId = Lens.field @"batchPredictionId"
{-# INLINEABLE dbpBatchPredictionId #-}
{-# DEPRECATED batchPredictionId "Use generic-lens or generic-optics with 'batchPredictionId' instead"  #-}

instance Core.ToQuery DeleteBatchPrediction where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteBatchPrediction where
        toHeaders DeleteBatchPrediction{..}
          = Core.pure
              ("X-Amz-Target", "AmazonML_20141212.DeleteBatchPrediction")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DeleteBatchPrediction where
        toJSON DeleteBatchPrediction{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("BatchPredictionId" Core..= batchPredictionId)])

instance Core.AWSRequest DeleteBatchPrediction where
        type Rs DeleteBatchPrediction = DeleteBatchPredictionResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DeleteBatchPredictionResponse' Core.<$>
                   (x Core..:? "BatchPredictionId") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Represents the output of a @DeleteBatchPrediction@ operation.
--
-- You can use the @GetBatchPrediction@ operation and check the value of the @Status@ parameter to see whether a @BatchPrediction@ is marked as @DELETED@ .
--
-- /See:/ 'mkDeleteBatchPredictionResponse' smart constructor.
data DeleteBatchPredictionResponse = DeleteBatchPredictionResponse'
  { batchPredictionId :: Core.Maybe Types.BatchPredictionId
    -- ^ A user-supplied ID that uniquely identifies the @BatchPrediction@ . This value should be identical to the value of the @BatchPredictionID@ in the request.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteBatchPredictionResponse' value with any optional fields omitted.
mkDeleteBatchPredictionResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteBatchPredictionResponse
mkDeleteBatchPredictionResponse responseStatus
  = DeleteBatchPredictionResponse'{batchPredictionId = Core.Nothing,
                                   responseStatus}

-- | A user-supplied ID that uniquely identifies the @BatchPrediction@ . This value should be identical to the value of the @BatchPredictionID@ in the request.
--
-- /Note:/ Consider using 'batchPredictionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbprrsBatchPredictionId :: Lens.Lens' DeleteBatchPredictionResponse (Core.Maybe Types.BatchPredictionId)
dbprrsBatchPredictionId = Lens.field @"batchPredictionId"
{-# INLINEABLE dbprrsBatchPredictionId #-}
{-# DEPRECATED batchPredictionId "Use generic-lens or generic-optics with 'batchPredictionId' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbprrsResponseStatus :: Lens.Lens' DeleteBatchPredictionResponse Core.Int
dbprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dbprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
