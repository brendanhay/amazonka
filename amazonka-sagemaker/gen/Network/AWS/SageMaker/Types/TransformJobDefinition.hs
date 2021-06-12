{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.TransformJobDefinition
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.TransformJobDefinition where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.SageMaker.Types.BatchStrategy
import Network.AWS.SageMaker.Types.TransformInput
import Network.AWS.SageMaker.Types.TransformOutput
import Network.AWS.SageMaker.Types.TransformResources

-- | Defines the input needed to run a transform job using the inference
-- specification specified in the algorithm.
--
-- /See:/ 'newTransformJobDefinition' smart constructor.
data TransformJobDefinition = TransformJobDefinition'
  { -- | The maximum number of parallel requests that can be sent to each
    -- instance in a transform job. The default value is 1.
    maxConcurrentTransforms :: Core.Maybe Core.Natural,
    -- | The environment variables to set in the Docker container. We support up
    -- to 16 key and values entries in the map.
    environment :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | The maximum payload size allowed, in MB. A payload is the data portion
    -- of a record (without metadata).
    maxPayloadInMB :: Core.Maybe Core.Natural,
    -- | A string that determines the number of records included in a single
    -- mini-batch.
    --
    -- @SingleRecord@ means only one record is used per mini-batch.
    -- @MultiRecord@ means a mini-batch is set to contain as many records that
    -- can fit within the @MaxPayloadInMB@ limit.
    batchStrategy :: Core.Maybe BatchStrategy,
    -- | A description of the input source and the way the transform job consumes
    -- it.
    transformInput :: TransformInput,
    -- | Identifies the Amazon S3 location where you want Amazon SageMaker to
    -- save the results from the transform job.
    transformOutput :: TransformOutput,
    -- | Identifies the ML compute instances for the transform job.
    transformResources :: TransformResources
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'TransformJobDefinition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxConcurrentTransforms', 'transformJobDefinition_maxConcurrentTransforms' - The maximum number of parallel requests that can be sent to each
-- instance in a transform job. The default value is 1.
--
-- 'environment', 'transformJobDefinition_environment' - The environment variables to set in the Docker container. We support up
-- to 16 key and values entries in the map.
--
-- 'maxPayloadInMB', 'transformJobDefinition_maxPayloadInMB' - The maximum payload size allowed, in MB. A payload is the data portion
-- of a record (without metadata).
--
-- 'batchStrategy', 'transformJobDefinition_batchStrategy' - A string that determines the number of records included in a single
-- mini-batch.
--
-- @SingleRecord@ means only one record is used per mini-batch.
-- @MultiRecord@ means a mini-batch is set to contain as many records that
-- can fit within the @MaxPayloadInMB@ limit.
--
-- 'transformInput', 'transformJobDefinition_transformInput' - A description of the input source and the way the transform job consumes
-- it.
--
-- 'transformOutput', 'transformJobDefinition_transformOutput' - Identifies the Amazon S3 location where you want Amazon SageMaker to
-- save the results from the transform job.
--
-- 'transformResources', 'transformJobDefinition_transformResources' - Identifies the ML compute instances for the transform job.
newTransformJobDefinition ::
  -- | 'transformInput'
  TransformInput ->
  -- | 'transformOutput'
  TransformOutput ->
  -- | 'transformResources'
  TransformResources ->
  TransformJobDefinition
newTransformJobDefinition
  pTransformInput_
  pTransformOutput_
  pTransformResources_ =
    TransformJobDefinition'
      { maxConcurrentTransforms =
          Core.Nothing,
        environment = Core.Nothing,
        maxPayloadInMB = Core.Nothing,
        batchStrategy = Core.Nothing,
        transformInput = pTransformInput_,
        transformOutput = pTransformOutput_,
        transformResources = pTransformResources_
      }

-- | The maximum number of parallel requests that can be sent to each
-- instance in a transform job. The default value is 1.
transformJobDefinition_maxConcurrentTransforms :: Lens.Lens' TransformJobDefinition (Core.Maybe Core.Natural)
transformJobDefinition_maxConcurrentTransforms = Lens.lens (\TransformJobDefinition' {maxConcurrentTransforms} -> maxConcurrentTransforms) (\s@TransformJobDefinition' {} a -> s {maxConcurrentTransforms = a} :: TransformJobDefinition)

-- | The environment variables to set in the Docker container. We support up
-- to 16 key and values entries in the map.
transformJobDefinition_environment :: Lens.Lens' TransformJobDefinition (Core.Maybe (Core.HashMap Core.Text Core.Text))
transformJobDefinition_environment = Lens.lens (\TransformJobDefinition' {environment} -> environment) (\s@TransformJobDefinition' {} a -> s {environment = a} :: TransformJobDefinition) Core.. Lens.mapping Lens._Coerce

-- | The maximum payload size allowed, in MB. A payload is the data portion
-- of a record (without metadata).
transformJobDefinition_maxPayloadInMB :: Lens.Lens' TransformJobDefinition (Core.Maybe Core.Natural)
transformJobDefinition_maxPayloadInMB = Lens.lens (\TransformJobDefinition' {maxPayloadInMB} -> maxPayloadInMB) (\s@TransformJobDefinition' {} a -> s {maxPayloadInMB = a} :: TransformJobDefinition)

-- | A string that determines the number of records included in a single
-- mini-batch.
--
-- @SingleRecord@ means only one record is used per mini-batch.
-- @MultiRecord@ means a mini-batch is set to contain as many records that
-- can fit within the @MaxPayloadInMB@ limit.
transformJobDefinition_batchStrategy :: Lens.Lens' TransformJobDefinition (Core.Maybe BatchStrategy)
transformJobDefinition_batchStrategy = Lens.lens (\TransformJobDefinition' {batchStrategy} -> batchStrategy) (\s@TransformJobDefinition' {} a -> s {batchStrategy = a} :: TransformJobDefinition)

-- | A description of the input source and the way the transform job consumes
-- it.
transformJobDefinition_transformInput :: Lens.Lens' TransformJobDefinition TransformInput
transformJobDefinition_transformInput = Lens.lens (\TransformJobDefinition' {transformInput} -> transformInput) (\s@TransformJobDefinition' {} a -> s {transformInput = a} :: TransformJobDefinition)

-- | Identifies the Amazon S3 location where you want Amazon SageMaker to
-- save the results from the transform job.
transformJobDefinition_transformOutput :: Lens.Lens' TransformJobDefinition TransformOutput
transformJobDefinition_transformOutput = Lens.lens (\TransformJobDefinition' {transformOutput} -> transformOutput) (\s@TransformJobDefinition' {} a -> s {transformOutput = a} :: TransformJobDefinition)

-- | Identifies the ML compute instances for the transform job.
transformJobDefinition_transformResources :: Lens.Lens' TransformJobDefinition TransformResources
transformJobDefinition_transformResources = Lens.lens (\TransformJobDefinition' {transformResources} -> transformResources) (\s@TransformJobDefinition' {} a -> s {transformResources = a} :: TransformJobDefinition)

instance Core.FromJSON TransformJobDefinition where
  parseJSON =
    Core.withObject
      "TransformJobDefinition"
      ( \x ->
          TransformJobDefinition'
            Core.<$> (x Core..:? "MaxConcurrentTransforms")
            Core.<*> (x Core..:? "Environment" Core..!= Core.mempty)
            Core.<*> (x Core..:? "MaxPayloadInMB")
            Core.<*> (x Core..:? "BatchStrategy")
            Core.<*> (x Core..: "TransformInput")
            Core.<*> (x Core..: "TransformOutput")
            Core.<*> (x Core..: "TransformResources")
      )

instance Core.Hashable TransformJobDefinition

instance Core.NFData TransformJobDefinition

instance Core.ToJSON TransformJobDefinition where
  toJSON TransformJobDefinition' {..} =
    Core.object
      ( Core.catMaybes
          [ ("MaxConcurrentTransforms" Core..=)
              Core.<$> maxConcurrentTransforms,
            ("Environment" Core..=) Core.<$> environment,
            ("MaxPayloadInMB" Core..=) Core.<$> maxPayloadInMB,
            ("BatchStrategy" Core..=) Core.<$> batchStrategy,
            Core.Just ("TransformInput" Core..= transformInput),
            Core.Just
              ("TransformOutput" Core..= transformOutput),
            Core.Just
              ("TransformResources" Core..= transformResources)
          ]
      )
