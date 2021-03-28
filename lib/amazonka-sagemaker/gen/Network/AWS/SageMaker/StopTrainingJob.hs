{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.StopTrainingJob
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops a training job. To stop a job, Amazon SageMaker sends the algorithm the @SIGTERM@ signal, which delays job termination for 120 seconds. Algorithms might use this 120-second window to save the model artifacts, so the results of the training is not lost. 
--
-- When it receives a @StopTrainingJob@ request, Amazon SageMaker changes the status of the job to @Stopping@ . After Amazon SageMaker stops the job, it sets the status to @Stopped@ .
module Network.AWS.SageMaker.StopTrainingJob
    (
    -- * Creating a request
      StopTrainingJob (..)
    , mkStopTrainingJob
    -- ** Request lenses
    , stjTrainingJobName

    -- * Destructuring the response
    , StopTrainingJobResponse (..)
    , mkStopTrainingJobResponse
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SageMaker.Types as Types

-- | /See:/ 'mkStopTrainingJob' smart constructor.
newtype StopTrainingJob = StopTrainingJob'
  { trainingJobName :: Types.TrainingJobName
    -- ^ The name of the training job to stop.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'StopTrainingJob' value with any optional fields omitted.
mkStopTrainingJob
    :: Types.TrainingJobName -- ^ 'trainingJobName'
    -> StopTrainingJob
mkStopTrainingJob trainingJobName
  = StopTrainingJob'{trainingJobName}

-- | The name of the training job to stop.
--
-- /Note:/ Consider using 'trainingJobName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stjTrainingJobName :: Lens.Lens' StopTrainingJob Types.TrainingJobName
stjTrainingJobName = Lens.field @"trainingJobName"
{-# INLINEABLE stjTrainingJobName #-}
{-# DEPRECATED trainingJobName "Use generic-lens or generic-optics with 'trainingJobName' instead"  #-}

instance Core.ToQuery StopTrainingJob where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders StopTrainingJob where
        toHeaders StopTrainingJob{..}
          = Core.pure ("X-Amz-Target", "SageMaker.StopTrainingJob") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON StopTrainingJob where
        toJSON StopTrainingJob{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("TrainingJobName" Core..= trainingJobName)])

instance Core.AWSRequest StopTrainingJob where
        type Rs StopTrainingJob = StopTrainingJobResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse = Response.receiveNull StopTrainingJobResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkStopTrainingJobResponse' smart constructor.
data StopTrainingJobResponse = StopTrainingJobResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StopTrainingJobResponse' value with any optional fields omitted.
mkStopTrainingJobResponse
    :: StopTrainingJobResponse
mkStopTrainingJobResponse = StopTrainingJobResponse'
