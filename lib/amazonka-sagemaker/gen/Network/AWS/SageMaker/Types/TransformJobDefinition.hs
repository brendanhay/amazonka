{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.TransformJobDefinition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.TransformJobDefinition
  ( TransformJobDefinition (..),

    -- * Smart constructor
    mkTransformJobDefinition,

    -- * Lenses
    tjdTransformInput,
    tjdTransformOutput,
    tjdTransformResources,
    tjdBatchStrategy,
    tjdEnvironment,
    tjdMaxConcurrentTransforms,
    tjdMaxPayloadInMB,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SageMaker.Types.BatchStrategy as Types
import qualified Network.AWS.SageMaker.Types.TransformEnvironmentKey as Types
import qualified Network.AWS.SageMaker.Types.TransformEnvironmentValue as Types
import qualified Network.AWS.SageMaker.Types.TransformInput as Types
import qualified Network.AWS.SageMaker.Types.TransformOutput as Types
import qualified Network.AWS.SageMaker.Types.TransformResources as Types

-- | Defines the input needed to run a transform job using the inference specification specified in the algorithm.
--
-- /See:/ 'mkTransformJobDefinition' smart constructor.
data TransformJobDefinition = TransformJobDefinition'
  { -- | A description of the input source and the way the transform job consumes it.
    transformInput :: Types.TransformInput,
    -- | Identifies the Amazon S3 location where you want Amazon SageMaker to save the results from the transform job.
    transformOutput :: Types.TransformOutput,
    -- | Identifies the ML compute instances for the transform job.
    transformResources :: Types.TransformResources,
    -- | A string that determines the number of records included in a single mini-batch.
    --
    -- @SingleRecord@ means only one record is used per mini-batch. @MultiRecord@ means a mini-batch is set to contain as many records that can fit within the @MaxPayloadInMB@ limit.
    batchStrategy :: Core.Maybe Types.BatchStrategy,
    -- | The environment variables to set in the Docker container. We support up to 16 key and values entries in the map.
    environment :: Core.Maybe (Core.HashMap Types.TransformEnvironmentKey Types.TransformEnvironmentValue),
    -- | The maximum number of parallel requests that can be sent to each instance in a transform job. The default value is 1.
    maxConcurrentTransforms :: Core.Maybe Core.Natural,
    -- | The maximum payload size allowed, in MB. A payload is the data portion of a record (without metadata).
    maxPayloadInMB :: Core.Maybe Core.Natural
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TransformJobDefinition' value with any optional fields omitted.
mkTransformJobDefinition ::
  -- | 'transformInput'
  Types.TransformInput ->
  -- | 'transformOutput'
  Types.TransformOutput ->
  -- | 'transformResources'
  Types.TransformResources ->
  TransformJobDefinition
mkTransformJobDefinition
  transformInput
  transformOutput
  transformResources =
    TransformJobDefinition'
      { transformInput,
        transformOutput,
        transformResources,
        batchStrategy = Core.Nothing,
        environment = Core.Nothing,
        maxConcurrentTransforms = Core.Nothing,
        maxPayloadInMB = Core.Nothing
      }

-- | A description of the input source and the way the transform job consumes it.
--
-- /Note:/ Consider using 'transformInput' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tjdTransformInput :: Lens.Lens' TransformJobDefinition Types.TransformInput
tjdTransformInput = Lens.field @"transformInput"
{-# DEPRECATED tjdTransformInput "Use generic-lens or generic-optics with 'transformInput' instead." #-}

-- | Identifies the Amazon S3 location where you want Amazon SageMaker to save the results from the transform job.
--
-- /Note:/ Consider using 'transformOutput' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tjdTransformOutput :: Lens.Lens' TransformJobDefinition Types.TransformOutput
tjdTransformOutput = Lens.field @"transformOutput"
{-# DEPRECATED tjdTransformOutput "Use generic-lens or generic-optics with 'transformOutput' instead." #-}

-- | Identifies the ML compute instances for the transform job.
--
-- /Note:/ Consider using 'transformResources' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tjdTransformResources :: Lens.Lens' TransformJobDefinition Types.TransformResources
tjdTransformResources = Lens.field @"transformResources"
{-# DEPRECATED tjdTransformResources "Use generic-lens or generic-optics with 'transformResources' instead." #-}

-- | A string that determines the number of records included in a single mini-batch.
--
-- @SingleRecord@ means only one record is used per mini-batch. @MultiRecord@ means a mini-batch is set to contain as many records that can fit within the @MaxPayloadInMB@ limit.
--
-- /Note:/ Consider using 'batchStrategy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tjdBatchStrategy :: Lens.Lens' TransformJobDefinition (Core.Maybe Types.BatchStrategy)
tjdBatchStrategy = Lens.field @"batchStrategy"
{-# DEPRECATED tjdBatchStrategy "Use generic-lens or generic-optics with 'batchStrategy' instead." #-}

-- | The environment variables to set in the Docker container. We support up to 16 key and values entries in the map.
--
-- /Note:/ Consider using 'environment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tjdEnvironment :: Lens.Lens' TransformJobDefinition (Core.Maybe (Core.HashMap Types.TransformEnvironmentKey Types.TransformEnvironmentValue))
tjdEnvironment = Lens.field @"environment"
{-# DEPRECATED tjdEnvironment "Use generic-lens or generic-optics with 'environment' instead." #-}

-- | The maximum number of parallel requests that can be sent to each instance in a transform job. The default value is 1.
--
-- /Note:/ Consider using 'maxConcurrentTransforms' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tjdMaxConcurrentTransforms :: Lens.Lens' TransformJobDefinition (Core.Maybe Core.Natural)
tjdMaxConcurrentTransforms = Lens.field @"maxConcurrentTransforms"
{-# DEPRECATED tjdMaxConcurrentTransforms "Use generic-lens or generic-optics with 'maxConcurrentTransforms' instead." #-}

-- | The maximum payload size allowed, in MB. A payload is the data portion of a record (without metadata).
--
-- /Note:/ Consider using 'maxPayloadInMB' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tjdMaxPayloadInMB :: Lens.Lens' TransformJobDefinition (Core.Maybe Core.Natural)
tjdMaxPayloadInMB = Lens.field @"maxPayloadInMB"
{-# DEPRECATED tjdMaxPayloadInMB "Use generic-lens or generic-optics with 'maxPayloadInMB' instead." #-}

instance Core.FromJSON TransformJobDefinition where
  toJSON TransformJobDefinition {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("TransformInput" Core..= transformInput),
            Core.Just ("TransformOutput" Core..= transformOutput),
            Core.Just ("TransformResources" Core..= transformResources),
            ("BatchStrategy" Core..=) Core.<$> batchStrategy,
            ("Environment" Core..=) Core.<$> environment,
            ("MaxConcurrentTransforms" Core..=)
              Core.<$> maxConcurrentTransforms,
            ("MaxPayloadInMB" Core..=) Core.<$> maxPayloadInMB
          ]
      )

instance Core.FromJSON TransformJobDefinition where
  parseJSON =
    Core.withObject "TransformJobDefinition" Core.$
      \x ->
        TransformJobDefinition'
          Core.<$> (x Core..: "TransformInput")
          Core.<*> (x Core..: "TransformOutput")
          Core.<*> (x Core..: "TransformResources")
          Core.<*> (x Core..:? "BatchStrategy")
          Core.<*> (x Core..:? "Environment")
          Core.<*> (x Core..:? "MaxConcurrentTransforms")
          Core.<*> (x Core..:? "MaxPayloadInMB")
