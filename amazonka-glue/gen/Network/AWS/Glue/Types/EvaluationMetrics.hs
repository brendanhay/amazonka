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
-- Module      : Network.AWS.Glue.Types.EvaluationMetrics
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.EvaluationMetrics where

import qualified Network.AWS.Core as Core
import Network.AWS.Glue.Types.FindMatchesMetrics
import Network.AWS.Glue.Types.TransformType
import qualified Network.AWS.Lens as Lens

-- | Evaluation metrics provide an estimate of the quality of your machine
-- learning transform.
--
-- /See:/ 'newEvaluationMetrics' smart constructor.
data EvaluationMetrics = EvaluationMetrics'
  { -- | The evaluation metrics for the find matches algorithm.
    findMatchesMetrics :: Core.Maybe FindMatchesMetrics,
    -- | The type of machine learning transform.
    transformType :: TransformType
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'EvaluationMetrics' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'findMatchesMetrics', 'evaluationMetrics_findMatchesMetrics' - The evaluation metrics for the find matches algorithm.
--
-- 'transformType', 'evaluationMetrics_transformType' - The type of machine learning transform.
newEvaluationMetrics ::
  -- | 'transformType'
  TransformType ->
  EvaluationMetrics
newEvaluationMetrics pTransformType_ =
  EvaluationMetrics'
    { findMatchesMetrics =
        Core.Nothing,
      transformType = pTransformType_
    }

-- | The evaluation metrics for the find matches algorithm.
evaluationMetrics_findMatchesMetrics :: Lens.Lens' EvaluationMetrics (Core.Maybe FindMatchesMetrics)
evaluationMetrics_findMatchesMetrics = Lens.lens (\EvaluationMetrics' {findMatchesMetrics} -> findMatchesMetrics) (\s@EvaluationMetrics' {} a -> s {findMatchesMetrics = a} :: EvaluationMetrics)

-- | The type of machine learning transform.
evaluationMetrics_transformType :: Lens.Lens' EvaluationMetrics TransformType
evaluationMetrics_transformType = Lens.lens (\EvaluationMetrics' {transformType} -> transformType) (\s@EvaluationMetrics' {} a -> s {transformType = a} :: EvaluationMetrics)

instance Core.FromJSON EvaluationMetrics where
  parseJSON =
    Core.withObject
      "EvaluationMetrics"
      ( \x ->
          EvaluationMetrics'
            Core.<$> (x Core..:? "FindMatchesMetrics")
            Core.<*> (x Core..: "TransformType")
      )

instance Core.Hashable EvaluationMetrics

instance Core.NFData EvaluationMetrics
