{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MachineLearning.UpdateEvaluation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the @EvaluationName@ of an @Evaluation@ .
--
-- You can use the @GetEvaluation@ operation to view the contents of the updated data element.
module Network.AWS.MachineLearning.UpdateEvaluation
    (
    -- * Creating a request
      UpdateEvaluation (..)
    , mkUpdateEvaluation
    -- ** Request lenses
    , ueEvaluationId
    , ueEvaluationName

    -- * Destructuring the response
    , UpdateEvaluationResponse (..)
    , mkUpdateEvaluationResponse
    -- ** Response lenses
    , uerrsEvaluationId
    , uerrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MachineLearning.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateEvaluation' smart constructor.
data UpdateEvaluation = UpdateEvaluation'
  { evaluationId :: Types.EntityId
    -- ^ The ID assigned to the @Evaluation@ during creation.
  , evaluationName :: Types.EntityName
    -- ^ A new user-supplied name or description of the @Evaluation@ that will replace the current content. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateEvaluation' value with any optional fields omitted.
mkUpdateEvaluation
    :: Types.EntityId -- ^ 'evaluationId'
    -> Types.EntityName -- ^ 'evaluationName'
    -> UpdateEvaluation
mkUpdateEvaluation evaluationId evaluationName
  = UpdateEvaluation'{evaluationId, evaluationName}

-- | The ID assigned to the @Evaluation@ during creation.
--
-- /Note:/ Consider using 'evaluationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ueEvaluationId :: Lens.Lens' UpdateEvaluation Types.EntityId
ueEvaluationId = Lens.field @"evaluationId"
{-# INLINEABLE ueEvaluationId #-}
{-# DEPRECATED evaluationId "Use generic-lens or generic-optics with 'evaluationId' instead"  #-}

-- | A new user-supplied name or description of the @Evaluation@ that will replace the current content. 
--
-- /Note:/ Consider using 'evaluationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ueEvaluationName :: Lens.Lens' UpdateEvaluation Types.EntityName
ueEvaluationName = Lens.field @"evaluationName"
{-# INLINEABLE ueEvaluationName #-}
{-# DEPRECATED evaluationName "Use generic-lens or generic-optics with 'evaluationName' instead"  #-}

instance Core.ToQuery UpdateEvaluation where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdateEvaluation where
        toHeaders UpdateEvaluation{..}
          = Core.pure ("X-Amz-Target", "AmazonML_20141212.UpdateEvaluation")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON UpdateEvaluation where
        toJSON UpdateEvaluation{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("EvaluationId" Core..= evaluationId),
                  Core.Just ("EvaluationName" Core..= evaluationName)])

instance Core.AWSRequest UpdateEvaluation where
        type Rs UpdateEvaluation = UpdateEvaluationResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 UpdateEvaluationResponse' Core.<$>
                   (x Core..:? "EvaluationId") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Represents the output of an @UpdateEvaluation@ operation.
--
-- You can see the updated content by using the @GetEvaluation@ operation.
--
-- /See:/ 'mkUpdateEvaluationResponse' smart constructor.
data UpdateEvaluationResponse = UpdateEvaluationResponse'
  { evaluationId :: Core.Maybe Types.EvaluationId
    -- ^ The ID assigned to the @Evaluation@ during creation. This value should be identical to the value of the @Evaluation@ in the request.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateEvaluationResponse' value with any optional fields omitted.
mkUpdateEvaluationResponse
    :: Core.Int -- ^ 'responseStatus'
    -> UpdateEvaluationResponse
mkUpdateEvaluationResponse responseStatus
  = UpdateEvaluationResponse'{evaluationId = Core.Nothing,
                              responseStatus}

-- | The ID assigned to the @Evaluation@ during creation. This value should be identical to the value of the @Evaluation@ in the request.
--
-- /Note:/ Consider using 'evaluationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uerrsEvaluationId :: Lens.Lens' UpdateEvaluationResponse (Core.Maybe Types.EvaluationId)
uerrsEvaluationId = Lens.field @"evaluationId"
{-# INLINEABLE uerrsEvaluationId #-}
{-# DEPRECATED evaluationId "Use generic-lens or generic-optics with 'evaluationId' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uerrsResponseStatus :: Lens.Lens' UpdateEvaluationResponse Core.Int
uerrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE uerrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
