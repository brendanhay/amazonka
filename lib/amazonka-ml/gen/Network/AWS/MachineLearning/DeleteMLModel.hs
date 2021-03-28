{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MachineLearning.DeleteMLModel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Assigns the @DELETED@ status to an @MLModel@ , rendering it unusable.
--
-- After using the @DeleteMLModel@ operation, you can use the @GetMLModel@ operation to verify that the status of the @MLModel@ changed to DELETED.
-- __Caution:__ The result of the @DeleteMLModel@ operation is irreversible.
module Network.AWS.MachineLearning.DeleteMLModel
    (
    -- * Creating a request
      DeleteMLModel (..)
    , mkDeleteMLModel
    -- ** Request lenses
    , dmlmMLModelId

    -- * Destructuring the response
    , DeleteMLModelResponse (..)
    , mkDeleteMLModelResponse
    -- ** Response lenses
    , dmlmrrsMLModelId
    , dmlmrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MachineLearning.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteMLModel' smart constructor.
newtype DeleteMLModel = DeleteMLModel'
  { mLModelId :: Types.EntityId
    -- ^ A user-supplied ID that uniquely identifies the @MLModel@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteMLModel' value with any optional fields omitted.
mkDeleteMLModel
    :: Types.EntityId -- ^ 'mLModelId'
    -> DeleteMLModel
mkDeleteMLModel mLModelId = DeleteMLModel'{mLModelId}

-- | A user-supplied ID that uniquely identifies the @MLModel@ .
--
-- /Note:/ Consider using 'mLModelId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmlmMLModelId :: Lens.Lens' DeleteMLModel Types.EntityId
dmlmMLModelId = Lens.field @"mLModelId"
{-# INLINEABLE dmlmMLModelId #-}
{-# DEPRECATED mLModelId "Use generic-lens or generic-optics with 'mLModelId' instead"  #-}

instance Core.ToQuery DeleteMLModel where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteMLModel where
        toHeaders DeleteMLModel{..}
          = Core.pure ("X-Amz-Target", "AmazonML_20141212.DeleteMLModel")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DeleteMLModel where
        toJSON DeleteMLModel{..}
          = Core.object
              (Core.catMaybes [Core.Just ("MLModelId" Core..= mLModelId)])

instance Core.AWSRequest DeleteMLModel where
        type Rs DeleteMLModel = DeleteMLModelResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DeleteMLModelResponse' Core.<$>
                   (x Core..:? "MLModelId") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Represents the output of a @DeleteMLModel@ operation.
--
-- You can use the @GetMLModel@ operation and check the value of the @Status@ parameter to see whether an @MLModel@ is marked as @DELETED@ .
--
-- /See:/ 'mkDeleteMLModelResponse' smart constructor.
data DeleteMLModelResponse = DeleteMLModelResponse'
  { mLModelId :: Core.Maybe Types.MLModelId
    -- ^ A user-supplied ID that uniquely identifies the @MLModel@ . This value should be identical to the value of the @MLModelID@ in the request.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteMLModelResponse' value with any optional fields omitted.
mkDeleteMLModelResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteMLModelResponse
mkDeleteMLModelResponse responseStatus
  = DeleteMLModelResponse'{mLModelId = Core.Nothing, responseStatus}

-- | A user-supplied ID that uniquely identifies the @MLModel@ . This value should be identical to the value of the @MLModelID@ in the request.
--
-- /Note:/ Consider using 'mLModelId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmlmrrsMLModelId :: Lens.Lens' DeleteMLModelResponse (Core.Maybe Types.MLModelId)
dmlmrrsMLModelId = Lens.field @"mLModelId"
{-# INLINEABLE dmlmrrsMLModelId #-}
{-# DEPRECATED mLModelId "Use generic-lens or generic-optics with 'mLModelId' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmlmrrsResponseStatus :: Lens.Lens' DeleteMLModelResponse Core.Int
dmlmrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dmlmrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
