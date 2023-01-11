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
-- Module      : Amazonka.SageMaker.Types.TransformJobDefinition
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.TransformJobDefinition where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.BatchStrategy
import Amazonka.SageMaker.Types.TransformInput
import Amazonka.SageMaker.Types.TransformOutput
import Amazonka.SageMaker.Types.TransformResources

-- | Defines the input needed to run a transform job using the inference
-- specification specified in the algorithm.
--
-- /See:/ 'newTransformJobDefinition' smart constructor.
data TransformJobDefinition = TransformJobDefinition'
  { -- | A string that determines the number of records included in a single
    -- mini-batch.
    --
    -- @SingleRecord@ means only one record is used per mini-batch.
    -- @MultiRecord@ means a mini-batch is set to contain as many records that
    -- can fit within the @MaxPayloadInMB@ limit.
    batchStrategy :: Prelude.Maybe BatchStrategy,
    -- | The environment variables to set in the Docker container. We support up
    -- to 16 key and values entries in the map.
    environment :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The maximum number of parallel requests that can be sent to each
    -- instance in a transform job. The default value is 1.
    maxConcurrentTransforms :: Prelude.Maybe Prelude.Natural,
    -- | The maximum payload size allowed, in MB. A payload is the data portion
    -- of a record (without metadata).
    maxPayloadInMB :: Prelude.Maybe Prelude.Natural,
    -- | A description of the input source and the way the transform job consumes
    -- it.
    transformInput :: TransformInput,
    -- | Identifies the Amazon S3 location where you want Amazon SageMaker to
    -- save the results from the transform job.
    transformOutput :: TransformOutput,
    -- | Identifies the ML compute instances for the transform job.
    transformResources :: TransformResources
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TransformJobDefinition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'batchStrategy', 'transformJobDefinition_batchStrategy' - A string that determines the number of records included in a single
-- mini-batch.
--
-- @SingleRecord@ means only one record is used per mini-batch.
-- @MultiRecord@ means a mini-batch is set to contain as many records that
-- can fit within the @MaxPayloadInMB@ limit.
--
-- 'environment', 'transformJobDefinition_environment' - The environment variables to set in the Docker container. We support up
-- to 16 key and values entries in the map.
--
-- 'maxConcurrentTransforms', 'transformJobDefinition_maxConcurrentTransforms' - The maximum number of parallel requests that can be sent to each
-- instance in a transform job. The default value is 1.
--
-- 'maxPayloadInMB', 'transformJobDefinition_maxPayloadInMB' - The maximum payload size allowed, in MB. A payload is the data portion
-- of a record (without metadata).
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
      { batchStrategy =
          Prelude.Nothing,
        environment = Prelude.Nothing,
        maxConcurrentTransforms = Prelude.Nothing,
        maxPayloadInMB = Prelude.Nothing,
        transformInput = pTransformInput_,
        transformOutput = pTransformOutput_,
        transformResources = pTransformResources_
      }

-- | A string that determines the number of records included in a single
-- mini-batch.
--
-- @SingleRecord@ means only one record is used per mini-batch.
-- @MultiRecord@ means a mini-batch is set to contain as many records that
-- can fit within the @MaxPayloadInMB@ limit.
transformJobDefinition_batchStrategy :: Lens.Lens' TransformJobDefinition (Prelude.Maybe BatchStrategy)
transformJobDefinition_batchStrategy = Lens.lens (\TransformJobDefinition' {batchStrategy} -> batchStrategy) (\s@TransformJobDefinition' {} a -> s {batchStrategy = a} :: TransformJobDefinition)

-- | The environment variables to set in the Docker container. We support up
-- to 16 key and values entries in the map.
transformJobDefinition_environment :: Lens.Lens' TransformJobDefinition (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
transformJobDefinition_environment = Lens.lens (\TransformJobDefinition' {environment} -> environment) (\s@TransformJobDefinition' {} a -> s {environment = a} :: TransformJobDefinition) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of parallel requests that can be sent to each
-- instance in a transform job. The default value is 1.
transformJobDefinition_maxConcurrentTransforms :: Lens.Lens' TransformJobDefinition (Prelude.Maybe Prelude.Natural)
transformJobDefinition_maxConcurrentTransforms = Lens.lens (\TransformJobDefinition' {maxConcurrentTransforms} -> maxConcurrentTransforms) (\s@TransformJobDefinition' {} a -> s {maxConcurrentTransforms = a} :: TransformJobDefinition)

-- | The maximum payload size allowed, in MB. A payload is the data portion
-- of a record (without metadata).
transformJobDefinition_maxPayloadInMB :: Lens.Lens' TransformJobDefinition (Prelude.Maybe Prelude.Natural)
transformJobDefinition_maxPayloadInMB = Lens.lens (\TransformJobDefinition' {maxPayloadInMB} -> maxPayloadInMB) (\s@TransformJobDefinition' {} a -> s {maxPayloadInMB = a} :: TransformJobDefinition)

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

instance Data.FromJSON TransformJobDefinition where
  parseJSON =
    Data.withObject
      "TransformJobDefinition"
      ( \x ->
          TransformJobDefinition'
            Prelude.<$> (x Data..:? "BatchStrategy")
            Prelude.<*> (x Data..:? "Environment" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "MaxConcurrentTransforms")
            Prelude.<*> (x Data..:? "MaxPayloadInMB")
            Prelude.<*> (x Data..: "TransformInput")
            Prelude.<*> (x Data..: "TransformOutput")
            Prelude.<*> (x Data..: "TransformResources")
      )

instance Prelude.Hashable TransformJobDefinition where
  hashWithSalt _salt TransformJobDefinition' {..} =
    _salt `Prelude.hashWithSalt` batchStrategy
      `Prelude.hashWithSalt` environment
      `Prelude.hashWithSalt` maxConcurrentTransforms
      `Prelude.hashWithSalt` maxPayloadInMB
      `Prelude.hashWithSalt` transformInput
      `Prelude.hashWithSalt` transformOutput
      `Prelude.hashWithSalt` transformResources

instance Prelude.NFData TransformJobDefinition where
  rnf TransformJobDefinition' {..} =
    Prelude.rnf batchStrategy
      `Prelude.seq` Prelude.rnf environment
      `Prelude.seq` Prelude.rnf maxConcurrentTransforms
      `Prelude.seq` Prelude.rnf maxPayloadInMB
      `Prelude.seq` Prelude.rnf transformInput
      `Prelude.seq` Prelude.rnf transformOutput
      `Prelude.seq` Prelude.rnf transformResources

instance Data.ToJSON TransformJobDefinition where
  toJSON TransformJobDefinition' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("BatchStrategy" Data..=) Prelude.<$> batchStrategy,
            ("Environment" Data..=) Prelude.<$> environment,
            ("MaxConcurrentTransforms" Data..=)
              Prelude.<$> maxConcurrentTransforms,
            ("MaxPayloadInMB" Data..=)
              Prelude.<$> maxPayloadInMB,
            Prelude.Just
              ("TransformInput" Data..= transformInput),
            Prelude.Just
              ("TransformOutput" Data..= transformOutput),
            Prelude.Just
              ("TransformResources" Data..= transformResources)
          ]
      )
