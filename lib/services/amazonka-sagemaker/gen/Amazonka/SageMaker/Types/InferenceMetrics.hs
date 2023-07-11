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
-- Module      : Amazonka.SageMaker.Types.InferenceMetrics
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.InferenceMetrics where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The metrics for an existing endpoint compared in an Inference
-- Recommender job.
--
-- /See:/ 'newInferenceMetrics' smart constructor.
data InferenceMetrics = InferenceMetrics'
  { -- | The expected maximum number of requests per minute for the instance.
    maxInvocations :: Prelude.Int,
    -- | The expected model latency at maximum invocations per minute for the
    -- instance.
    modelLatency :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InferenceMetrics' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxInvocations', 'inferenceMetrics_maxInvocations' - The expected maximum number of requests per minute for the instance.
--
-- 'modelLatency', 'inferenceMetrics_modelLatency' - The expected model latency at maximum invocations per minute for the
-- instance.
newInferenceMetrics ::
  -- | 'maxInvocations'
  Prelude.Int ->
  -- | 'modelLatency'
  Prelude.Int ->
  InferenceMetrics
newInferenceMetrics pMaxInvocations_ pModelLatency_ =
  InferenceMetrics'
    { maxInvocations =
        pMaxInvocations_,
      modelLatency = pModelLatency_
    }

-- | The expected maximum number of requests per minute for the instance.
inferenceMetrics_maxInvocations :: Lens.Lens' InferenceMetrics Prelude.Int
inferenceMetrics_maxInvocations = Lens.lens (\InferenceMetrics' {maxInvocations} -> maxInvocations) (\s@InferenceMetrics' {} a -> s {maxInvocations = a} :: InferenceMetrics)

-- | The expected model latency at maximum invocations per minute for the
-- instance.
inferenceMetrics_modelLatency :: Lens.Lens' InferenceMetrics Prelude.Int
inferenceMetrics_modelLatency = Lens.lens (\InferenceMetrics' {modelLatency} -> modelLatency) (\s@InferenceMetrics' {} a -> s {modelLatency = a} :: InferenceMetrics)

instance Data.FromJSON InferenceMetrics where
  parseJSON =
    Data.withObject
      "InferenceMetrics"
      ( \x ->
          InferenceMetrics'
            Prelude.<$> (x Data..: "MaxInvocations")
            Prelude.<*> (x Data..: "ModelLatency")
      )

instance Prelude.Hashable InferenceMetrics where
  hashWithSalt _salt InferenceMetrics' {..} =
    _salt
      `Prelude.hashWithSalt` maxInvocations
      `Prelude.hashWithSalt` modelLatency

instance Prelude.NFData InferenceMetrics where
  rnf InferenceMetrics' {..} =
    Prelude.rnf maxInvocations
      `Prelude.seq` Prelude.rnf modelLatency
