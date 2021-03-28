{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchEvents.Types.BatchParameters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudWatchEvents.Types.BatchParameters
  ( BatchParameters (..)
  -- * Smart constructor
  , mkBatchParameters
  -- * Lenses
  , bpJobDefinition
  , bpJobName
  , bpArrayProperties
  , bpRetryStrategy
  ) where

import qualified Network.AWS.CloudWatchEvents.Types.BatchArrayProperties as Types
import qualified Network.AWS.CloudWatchEvents.Types.BatchRetryStrategy as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The custom parameters to be used when the target is an AWS Batch job.
--
-- /See:/ 'mkBatchParameters' smart constructor.
data BatchParameters = BatchParameters'
  { jobDefinition :: Core.Text
    -- ^ The ARN or name of the job definition to use if the event target is an AWS Batch job. This job definition must already exist.
  , jobName :: Core.Text
    -- ^ The name to use for this execution of the job, if the target is an AWS Batch job.
  , arrayProperties :: Core.Maybe Types.BatchArrayProperties
    -- ^ The array properties for the submitted job, such as the size of the array. The array size can be between 2 and 10,000. If you specify array properties for a job, it becomes an array job. This parameter is used only if the target is an AWS Batch job.
  , retryStrategy :: Core.Maybe Types.BatchRetryStrategy
    -- ^ The retry strategy to use for failed jobs, if the target is an AWS Batch job. The retry strategy is the number of times to retry the failed job execution. Valid values are 1–10. When you specify a retry strategy here, it overrides the retry strategy defined in the job definition.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'BatchParameters' value with any optional fields omitted.
mkBatchParameters
    :: Core.Text -- ^ 'jobDefinition'
    -> Core.Text -- ^ 'jobName'
    -> BatchParameters
mkBatchParameters jobDefinition jobName
  = BatchParameters'{jobDefinition, jobName,
                     arrayProperties = Core.Nothing, retryStrategy = Core.Nothing}

-- | The ARN or name of the job definition to use if the event target is an AWS Batch job. This job definition must already exist.
--
-- /Note:/ Consider using 'jobDefinition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bpJobDefinition :: Lens.Lens' BatchParameters Core.Text
bpJobDefinition = Lens.field @"jobDefinition"
{-# INLINEABLE bpJobDefinition #-}
{-# DEPRECATED jobDefinition "Use generic-lens or generic-optics with 'jobDefinition' instead"  #-}

-- | The name to use for this execution of the job, if the target is an AWS Batch job.
--
-- /Note:/ Consider using 'jobName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bpJobName :: Lens.Lens' BatchParameters Core.Text
bpJobName = Lens.field @"jobName"
{-# INLINEABLE bpJobName #-}
{-# DEPRECATED jobName "Use generic-lens or generic-optics with 'jobName' instead"  #-}

-- | The array properties for the submitted job, such as the size of the array. The array size can be between 2 and 10,000. If you specify array properties for a job, it becomes an array job. This parameter is used only if the target is an AWS Batch job.
--
-- /Note:/ Consider using 'arrayProperties' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bpArrayProperties :: Lens.Lens' BatchParameters (Core.Maybe Types.BatchArrayProperties)
bpArrayProperties = Lens.field @"arrayProperties"
{-# INLINEABLE bpArrayProperties #-}
{-# DEPRECATED arrayProperties "Use generic-lens or generic-optics with 'arrayProperties' instead"  #-}

-- | The retry strategy to use for failed jobs, if the target is an AWS Batch job. The retry strategy is the number of times to retry the failed job execution. Valid values are 1–10. When you specify a retry strategy here, it overrides the retry strategy defined in the job definition.
--
-- /Note:/ Consider using 'retryStrategy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bpRetryStrategy :: Lens.Lens' BatchParameters (Core.Maybe Types.BatchRetryStrategy)
bpRetryStrategy = Lens.field @"retryStrategy"
{-# INLINEABLE bpRetryStrategy #-}
{-# DEPRECATED retryStrategy "Use generic-lens or generic-optics with 'retryStrategy' instead"  #-}

instance Core.FromJSON BatchParameters where
        toJSON BatchParameters{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("JobDefinition" Core..= jobDefinition),
                  Core.Just ("JobName" Core..= jobName),
                  ("ArrayProperties" Core..=) Core.<$> arrayProperties,
                  ("RetryStrategy" Core..=) Core.<$> retryStrategy])

instance Core.FromJSON BatchParameters where
        parseJSON
          = Core.withObject "BatchParameters" Core.$
              \ x ->
                BatchParameters' Core.<$>
                  (x Core..: "JobDefinition") Core.<*> x Core..: "JobName" Core.<*>
                    x Core..:? "ArrayProperties"
                    Core.<*> x Core..:? "RetryStrategy"
