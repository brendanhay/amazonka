{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.StopLabelingJob
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops a running labeling job. A job that is stopped cannot be restarted. Any results obtained before the job is stopped are placed in the Amazon S3 output bucket.
module Network.AWS.SageMaker.StopLabelingJob
    (
    -- * Creating a request
      StopLabelingJob (..)
    , mkStopLabelingJob
    -- ** Request lenses
    , sljLabelingJobName

    -- * Destructuring the response
    , StopLabelingJobResponse (..)
    , mkStopLabelingJobResponse
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SageMaker.Types as Types

-- | /See:/ 'mkStopLabelingJob' smart constructor.
newtype StopLabelingJob = StopLabelingJob'
  { labelingJobName :: Types.LabelingJobName
    -- ^ The name of the labeling job to stop.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'StopLabelingJob' value with any optional fields omitted.
mkStopLabelingJob
    :: Types.LabelingJobName -- ^ 'labelingJobName'
    -> StopLabelingJob
mkStopLabelingJob labelingJobName
  = StopLabelingJob'{labelingJobName}

-- | The name of the labeling job to stop.
--
-- /Note:/ Consider using 'labelingJobName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sljLabelingJobName :: Lens.Lens' StopLabelingJob Types.LabelingJobName
sljLabelingJobName = Lens.field @"labelingJobName"
{-# INLINEABLE sljLabelingJobName #-}
{-# DEPRECATED labelingJobName "Use generic-lens or generic-optics with 'labelingJobName' instead"  #-}

instance Core.ToQuery StopLabelingJob where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders StopLabelingJob where
        toHeaders StopLabelingJob{..}
          = Core.pure ("X-Amz-Target", "SageMaker.StopLabelingJob") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON StopLabelingJob where
        toJSON StopLabelingJob{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("LabelingJobName" Core..= labelingJobName)])

instance Core.AWSRequest StopLabelingJob where
        type Rs StopLabelingJob = StopLabelingJobResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse = Response.receiveNull StopLabelingJobResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkStopLabelingJobResponse' smart constructor.
data StopLabelingJobResponse = StopLabelingJobResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StopLabelingJobResponse' value with any optional fields omitted.
mkStopLabelingJobResponse
    :: StopLabelingJobResponse
mkStopLabelingJobResponse = StopLabelingJobResponse'
