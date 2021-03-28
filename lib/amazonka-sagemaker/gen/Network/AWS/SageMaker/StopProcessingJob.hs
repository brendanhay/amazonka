{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.StopProcessingJob
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops a processing job.
module Network.AWS.SageMaker.StopProcessingJob
    (
    -- * Creating a request
      StopProcessingJob (..)
    , mkStopProcessingJob
    -- ** Request lenses
    , spjProcessingJobName

    -- * Destructuring the response
    , StopProcessingJobResponse (..)
    , mkStopProcessingJobResponse
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SageMaker.Types as Types

-- | /See:/ 'mkStopProcessingJob' smart constructor.
newtype StopProcessingJob = StopProcessingJob'
  { processingJobName :: Types.ProcessingJobName
    -- ^ The name of the processing job to stop.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'StopProcessingJob' value with any optional fields omitted.
mkStopProcessingJob
    :: Types.ProcessingJobName -- ^ 'processingJobName'
    -> StopProcessingJob
mkStopProcessingJob processingJobName
  = StopProcessingJob'{processingJobName}

-- | The name of the processing job to stop.
--
-- /Note:/ Consider using 'processingJobName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spjProcessingJobName :: Lens.Lens' StopProcessingJob Types.ProcessingJobName
spjProcessingJobName = Lens.field @"processingJobName"
{-# INLINEABLE spjProcessingJobName #-}
{-# DEPRECATED processingJobName "Use generic-lens or generic-optics with 'processingJobName' instead"  #-}

instance Core.ToQuery StopProcessingJob where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders StopProcessingJob where
        toHeaders StopProcessingJob{..}
          = Core.pure ("X-Amz-Target", "SageMaker.StopProcessingJob") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON StopProcessingJob where
        toJSON StopProcessingJob{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("ProcessingJobName" Core..= processingJobName)])

instance Core.AWSRequest StopProcessingJob where
        type Rs StopProcessingJob = StopProcessingJobResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse = Response.receiveNull StopProcessingJobResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkStopProcessingJobResponse' smart constructor.
data StopProcessingJobResponse = StopProcessingJobResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StopProcessingJobResponse' value with any optional fields omitted.
mkStopProcessingJobResponse
    :: StopProcessingJobResponse
mkStopProcessingJobResponse = StopProcessingJobResponse'
