{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchEvents.Types.BatchParameters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatchEvents.Types.BatchParameters
  ( BatchParameters (..),

    -- * Smart constructor
    mkBatchParameters,

    -- * Lenses
    bpJobDefinition,
    bpJobName,
    bpArrayProperties,
    bpRetryStrategy,
  )
where

import qualified Network.AWS.CloudWatchEvents.Types.BatchArrayProperties as Types
import qualified Network.AWS.CloudWatchEvents.Types.BatchRetryStrategy as Types
import qualified Network.AWS.CloudWatchEvents.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The custom parameters to be used when the target is an AWS Batch job.
--
-- /See:/ 'mkBatchParameters' smart constructor.
data BatchParameters = BatchParameters'
  { -- | The ARN or name of the job definition to use if the event target is an AWS Batch job. This job definition must already exist.
    jobDefinition :: Types.String,
    -- | The name to use for this execution of the job, if the target is an AWS Batch job.
    jobName :: Types.String,
    -- | The array properties for the submitted job, such as the size of the array. The array size can be between 2 and 10,000. If you specify array properties for a job, it becomes an array job. This parameter is used only if the target is an AWS Batch job.
    arrayProperties :: Core.Maybe Types.BatchArrayProperties,
    -- | The retry strategy to use for failed jobs, if the target is an AWS Batch job. The retry strategy is the number of times to retry the failed job execution. Valid values are 1–10. When you specify a retry strategy here, it overrides the retry strategy defined in the job definition.
    retryStrategy :: Core.Maybe Types.BatchRetryStrategy
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'BatchParameters' value with any optional fields omitted.
mkBatchParameters ::
  -- | 'jobDefinition'
  Types.String ->
  -- | 'jobName'
  Types.String ->
  BatchParameters
mkBatchParameters jobDefinition jobName =
  BatchParameters'
    { jobDefinition,
      jobName,
      arrayProperties = Core.Nothing,
      retryStrategy = Core.Nothing
    }

-- | The ARN or name of the job definition to use if the event target is an AWS Batch job. This job definition must already exist.
--
-- /Note:/ Consider using 'jobDefinition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bpJobDefinition :: Lens.Lens' BatchParameters Types.String
bpJobDefinition = Lens.field @"jobDefinition"
{-# DEPRECATED bpJobDefinition "Use generic-lens or generic-optics with 'jobDefinition' instead." #-}

-- | The name to use for this execution of the job, if the target is an AWS Batch job.
--
-- /Note:/ Consider using 'jobName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bpJobName :: Lens.Lens' BatchParameters Types.String
bpJobName = Lens.field @"jobName"
{-# DEPRECATED bpJobName "Use generic-lens or generic-optics with 'jobName' instead." #-}

-- | The array properties for the submitted job, such as the size of the array. The array size can be between 2 and 10,000. If you specify array properties for a job, it becomes an array job. This parameter is used only if the target is an AWS Batch job.
--
-- /Note:/ Consider using 'arrayProperties' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bpArrayProperties :: Lens.Lens' BatchParameters (Core.Maybe Types.BatchArrayProperties)
bpArrayProperties = Lens.field @"arrayProperties"
{-# DEPRECATED bpArrayProperties "Use generic-lens or generic-optics with 'arrayProperties' instead." #-}

-- | The retry strategy to use for failed jobs, if the target is an AWS Batch job. The retry strategy is the number of times to retry the failed job execution. Valid values are 1–10. When you specify a retry strategy here, it overrides the retry strategy defined in the job definition.
--
-- /Note:/ Consider using 'retryStrategy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bpRetryStrategy :: Lens.Lens' BatchParameters (Core.Maybe Types.BatchRetryStrategy)
bpRetryStrategy = Lens.field @"retryStrategy"
{-# DEPRECATED bpRetryStrategy "Use generic-lens or generic-optics with 'retryStrategy' instead." #-}

instance Core.FromJSON BatchParameters where
  toJSON BatchParameters {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("JobDefinition" Core..= jobDefinition),
            Core.Just ("JobName" Core..= jobName),
            ("ArrayProperties" Core..=) Core.<$> arrayProperties,
            ("RetryStrategy" Core..=) Core.<$> retryStrategy
          ]
      )

instance Core.FromJSON BatchParameters where
  parseJSON =
    Core.withObject "BatchParameters" Core.$
      \x ->
        BatchParameters'
          Core.<$> (x Core..: "JobDefinition")
          Core.<*> (x Core..: "JobName")
          Core.<*> (x Core..:? "ArrayProperties")
          Core.<*> (x Core..:? "RetryStrategy")
