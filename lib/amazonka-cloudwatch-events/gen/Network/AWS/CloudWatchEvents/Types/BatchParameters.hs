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
    bpRetryStrategy,
    bpArrayProperties,
    bpJobDefinition,
    bpJobName,
  )
where

import Network.AWS.CloudWatchEvents.Types.BatchArrayProperties
import Network.AWS.CloudWatchEvents.Types.BatchRetryStrategy
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The custom parameters to be used when the target is an AWS Batch job.
--
-- /See:/ 'mkBatchParameters' smart constructor.
data BatchParameters = BatchParameters'
  { retryStrategy ::
      Lude.Maybe BatchRetryStrategy,
    arrayProperties :: Lude.Maybe BatchArrayProperties,
    jobDefinition :: Lude.Text,
    jobName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BatchParameters' with the minimum fields required to make a request.
--
-- * 'arrayProperties' - The array properties for the submitted job, such as the size of the array. The array size can be between 2 and 10,000. If you specify array properties for a job, it becomes an array job. This parameter is used only if the target is an AWS Batch job.
-- * 'jobDefinition' - The ARN or name of the job definition to use if the event target is an AWS Batch job. This job definition must already exist.
-- * 'jobName' - The name to use for this execution of the job, if the target is an AWS Batch job.
-- * 'retryStrategy' - The retry strategy to use for failed jobs, if the target is an AWS Batch job. The retry strategy is the number of times to retry the failed job execution. Valid values are 1–10. When you specify a retry strategy here, it overrides the retry strategy defined in the job definition.
mkBatchParameters ::
  -- | 'jobDefinition'
  Lude.Text ->
  -- | 'jobName'
  Lude.Text ->
  BatchParameters
mkBatchParameters pJobDefinition_ pJobName_ =
  BatchParameters'
    { retryStrategy = Lude.Nothing,
      arrayProperties = Lude.Nothing,
      jobDefinition = pJobDefinition_,
      jobName = pJobName_
    }

-- | The retry strategy to use for failed jobs, if the target is an AWS Batch job. The retry strategy is the number of times to retry the failed job execution. Valid values are 1–10. When you specify a retry strategy here, it overrides the retry strategy defined in the job definition.
--
-- /Note:/ Consider using 'retryStrategy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bpRetryStrategy :: Lens.Lens' BatchParameters (Lude.Maybe BatchRetryStrategy)
bpRetryStrategy = Lens.lens (retryStrategy :: BatchParameters -> Lude.Maybe BatchRetryStrategy) (\s a -> s {retryStrategy = a} :: BatchParameters)
{-# DEPRECATED bpRetryStrategy "Use generic-lens or generic-optics with 'retryStrategy' instead." #-}

-- | The array properties for the submitted job, such as the size of the array. The array size can be between 2 and 10,000. If you specify array properties for a job, it becomes an array job. This parameter is used only if the target is an AWS Batch job.
--
-- /Note:/ Consider using 'arrayProperties' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bpArrayProperties :: Lens.Lens' BatchParameters (Lude.Maybe BatchArrayProperties)
bpArrayProperties = Lens.lens (arrayProperties :: BatchParameters -> Lude.Maybe BatchArrayProperties) (\s a -> s {arrayProperties = a} :: BatchParameters)
{-# DEPRECATED bpArrayProperties "Use generic-lens or generic-optics with 'arrayProperties' instead." #-}

-- | The ARN or name of the job definition to use if the event target is an AWS Batch job. This job definition must already exist.
--
-- /Note:/ Consider using 'jobDefinition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bpJobDefinition :: Lens.Lens' BatchParameters Lude.Text
bpJobDefinition = Lens.lens (jobDefinition :: BatchParameters -> Lude.Text) (\s a -> s {jobDefinition = a} :: BatchParameters)
{-# DEPRECATED bpJobDefinition "Use generic-lens or generic-optics with 'jobDefinition' instead." #-}

-- | The name to use for this execution of the job, if the target is an AWS Batch job.
--
-- /Note:/ Consider using 'jobName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bpJobName :: Lens.Lens' BatchParameters Lude.Text
bpJobName = Lens.lens (jobName :: BatchParameters -> Lude.Text) (\s a -> s {jobName = a} :: BatchParameters)
{-# DEPRECATED bpJobName "Use generic-lens or generic-optics with 'jobName' instead." #-}

instance Lude.FromJSON BatchParameters where
  parseJSON =
    Lude.withObject
      "BatchParameters"
      ( \x ->
          BatchParameters'
            Lude.<$> (x Lude..:? "RetryStrategy")
            Lude.<*> (x Lude..:? "ArrayProperties")
            Lude.<*> (x Lude..: "JobDefinition")
            Lude.<*> (x Lude..: "JobName")
      )

instance Lude.ToJSON BatchParameters where
  toJSON BatchParameters' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("RetryStrategy" Lude..=) Lude.<$> retryStrategy,
            ("ArrayProperties" Lude..=) Lude.<$> arrayProperties,
            Lude.Just ("JobDefinition" Lude..= jobDefinition),
            Lude.Just ("JobName" Lude..= jobName)
          ]
      )
