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
    tjdBatchStrategy,
    tjdMaxPayloadInMB,
    tjdEnvironment,
    tjdTransformResources,
    tjdTransformInput,
    tjdMaxConcurrentTransforms,
    tjdTransformOutput,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SageMaker.Types.BatchStrategy
import Network.AWS.SageMaker.Types.TransformInput
import Network.AWS.SageMaker.Types.TransformOutput
import Network.AWS.SageMaker.Types.TransformResources

-- | Defines the input needed to run a transform job using the inference specification specified in the algorithm.
--
-- /See:/ 'mkTransformJobDefinition' smart constructor.
data TransformJobDefinition = TransformJobDefinition'
  { -- | A string that determines the number of records included in a single mini-batch.
    --
    -- @SingleRecord@ means only one record is used per mini-batch. @MultiRecord@ means a mini-batch is set to contain as many records that can fit within the @MaxPayloadInMB@ limit.
    batchStrategy :: Lude.Maybe BatchStrategy,
    -- | The maximum payload size allowed, in MB. A payload is the data portion of a record (without metadata).
    maxPayloadInMB :: Lude.Maybe Lude.Natural,
    -- | The environment variables to set in the Docker container. We support up to 16 key and values entries in the map.
    environment :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    -- | Identifies the ML compute instances for the transform job.
    transformResources :: TransformResources,
    -- | A description of the input source and the way the transform job consumes it.
    transformInput :: TransformInput,
    -- | The maximum number of parallel requests that can be sent to each instance in a transform job. The default value is 1.
    maxConcurrentTransforms :: Lude.Maybe Lude.Natural,
    -- | Identifies the Amazon S3 location where you want Amazon SageMaker to save the results from the transform job.
    transformOutput :: TransformOutput
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TransformJobDefinition' with the minimum fields required to make a request.
--
-- * 'batchStrategy' - A string that determines the number of records included in a single mini-batch.
--
-- @SingleRecord@ means only one record is used per mini-batch. @MultiRecord@ means a mini-batch is set to contain as many records that can fit within the @MaxPayloadInMB@ limit.
-- * 'maxPayloadInMB' - The maximum payload size allowed, in MB. A payload is the data portion of a record (without metadata).
-- * 'environment' - The environment variables to set in the Docker container. We support up to 16 key and values entries in the map.
-- * 'transformResources' - Identifies the ML compute instances for the transform job.
-- * 'transformInput' - A description of the input source and the way the transform job consumes it.
-- * 'maxConcurrentTransforms' - The maximum number of parallel requests that can be sent to each instance in a transform job. The default value is 1.
-- * 'transformOutput' - Identifies the Amazon S3 location where you want Amazon SageMaker to save the results from the transform job.
mkTransformJobDefinition ::
  -- | 'transformResources'
  TransformResources ->
  -- | 'transformInput'
  TransformInput ->
  -- | 'transformOutput'
  TransformOutput ->
  TransformJobDefinition
mkTransformJobDefinition
  pTransformResources_
  pTransformInput_
  pTransformOutput_ =
    TransformJobDefinition'
      { batchStrategy = Lude.Nothing,
        maxPayloadInMB = Lude.Nothing,
        environment = Lude.Nothing,
        transformResources = pTransformResources_,
        transformInput = pTransformInput_,
        maxConcurrentTransforms = Lude.Nothing,
        transformOutput = pTransformOutput_
      }

-- | A string that determines the number of records included in a single mini-batch.
--
-- @SingleRecord@ means only one record is used per mini-batch. @MultiRecord@ means a mini-batch is set to contain as many records that can fit within the @MaxPayloadInMB@ limit.
--
-- /Note:/ Consider using 'batchStrategy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tjdBatchStrategy :: Lens.Lens' TransformJobDefinition (Lude.Maybe BatchStrategy)
tjdBatchStrategy = Lens.lens (batchStrategy :: TransformJobDefinition -> Lude.Maybe BatchStrategy) (\s a -> s {batchStrategy = a} :: TransformJobDefinition)
{-# DEPRECATED tjdBatchStrategy "Use generic-lens or generic-optics with 'batchStrategy' instead." #-}

-- | The maximum payload size allowed, in MB. A payload is the data portion of a record (without metadata).
--
-- /Note:/ Consider using 'maxPayloadInMB' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tjdMaxPayloadInMB :: Lens.Lens' TransformJobDefinition (Lude.Maybe Lude.Natural)
tjdMaxPayloadInMB = Lens.lens (maxPayloadInMB :: TransformJobDefinition -> Lude.Maybe Lude.Natural) (\s a -> s {maxPayloadInMB = a} :: TransformJobDefinition)
{-# DEPRECATED tjdMaxPayloadInMB "Use generic-lens or generic-optics with 'maxPayloadInMB' instead." #-}

-- | The environment variables to set in the Docker container. We support up to 16 key and values entries in the map.
--
-- /Note:/ Consider using 'environment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tjdEnvironment :: Lens.Lens' TransformJobDefinition (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
tjdEnvironment = Lens.lens (environment :: TransformJobDefinition -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {environment = a} :: TransformJobDefinition)
{-# DEPRECATED tjdEnvironment "Use generic-lens or generic-optics with 'environment' instead." #-}

-- | Identifies the ML compute instances for the transform job.
--
-- /Note:/ Consider using 'transformResources' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tjdTransformResources :: Lens.Lens' TransformJobDefinition TransformResources
tjdTransformResources = Lens.lens (transformResources :: TransformJobDefinition -> TransformResources) (\s a -> s {transformResources = a} :: TransformJobDefinition)
{-# DEPRECATED tjdTransformResources "Use generic-lens or generic-optics with 'transformResources' instead." #-}

-- | A description of the input source and the way the transform job consumes it.
--
-- /Note:/ Consider using 'transformInput' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tjdTransformInput :: Lens.Lens' TransformJobDefinition TransformInput
tjdTransformInput = Lens.lens (transformInput :: TransformJobDefinition -> TransformInput) (\s a -> s {transformInput = a} :: TransformJobDefinition)
{-# DEPRECATED tjdTransformInput "Use generic-lens or generic-optics with 'transformInput' instead." #-}

-- | The maximum number of parallel requests that can be sent to each instance in a transform job. The default value is 1.
--
-- /Note:/ Consider using 'maxConcurrentTransforms' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tjdMaxConcurrentTransforms :: Lens.Lens' TransformJobDefinition (Lude.Maybe Lude.Natural)
tjdMaxConcurrentTransforms = Lens.lens (maxConcurrentTransforms :: TransformJobDefinition -> Lude.Maybe Lude.Natural) (\s a -> s {maxConcurrentTransforms = a} :: TransformJobDefinition)
{-# DEPRECATED tjdMaxConcurrentTransforms "Use generic-lens or generic-optics with 'maxConcurrentTransforms' instead." #-}

-- | Identifies the Amazon S3 location where you want Amazon SageMaker to save the results from the transform job.
--
-- /Note:/ Consider using 'transformOutput' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tjdTransformOutput :: Lens.Lens' TransformJobDefinition TransformOutput
tjdTransformOutput = Lens.lens (transformOutput :: TransformJobDefinition -> TransformOutput) (\s a -> s {transformOutput = a} :: TransformJobDefinition)
{-# DEPRECATED tjdTransformOutput "Use generic-lens or generic-optics with 'transformOutput' instead." #-}

instance Lude.FromJSON TransformJobDefinition where
  parseJSON =
    Lude.withObject
      "TransformJobDefinition"
      ( \x ->
          TransformJobDefinition'
            Lude.<$> (x Lude..:? "BatchStrategy")
            Lude.<*> (x Lude..:? "MaxPayloadInMB")
            Lude.<*> (x Lude..:? "Environment" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..: "TransformResources")
            Lude.<*> (x Lude..: "TransformInput")
            Lude.<*> (x Lude..:? "MaxConcurrentTransforms")
            Lude.<*> (x Lude..: "TransformOutput")
      )

instance Lude.ToJSON TransformJobDefinition where
  toJSON TransformJobDefinition' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("BatchStrategy" Lude..=) Lude.<$> batchStrategy,
            ("MaxPayloadInMB" Lude..=) Lude.<$> maxPayloadInMB,
            ("Environment" Lude..=) Lude.<$> environment,
            Lude.Just ("TransformResources" Lude..= transformResources),
            Lude.Just ("TransformInput" Lude..= transformInput),
            ("MaxConcurrentTransforms" Lude..=)
              Lude.<$> maxConcurrentTransforms,
            Lude.Just ("TransformOutput" Lude..= transformOutput)
          ]
      )
