{-# LANGUAGE DeriveDataTypeable #-}
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

import Network.AWS.Glue.Types.FindMatchesMetrics
import Network.AWS.Glue.Types.TransformType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Evaluation metrics provide an estimate of the quality of your machine
-- learning transform.
--
-- /See:/ 'newEvaluationMetrics' smart constructor.
data EvaluationMetrics = EvaluationMetrics'
  { -- | The evaluation metrics for the find matches algorithm.
    findMatchesMetrics :: Prelude.Maybe FindMatchesMetrics,
    -- | The type of machine learning transform.
    transformType :: TransformType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
        Prelude.Nothing,
      transformType = pTransformType_
    }

-- | The evaluation metrics for the find matches algorithm.
evaluationMetrics_findMatchesMetrics :: Lens.Lens' EvaluationMetrics (Prelude.Maybe FindMatchesMetrics)
evaluationMetrics_findMatchesMetrics = Lens.lens (\EvaluationMetrics' {findMatchesMetrics} -> findMatchesMetrics) (\s@EvaluationMetrics' {} a -> s {findMatchesMetrics = a} :: EvaluationMetrics)

-- | The type of machine learning transform.
evaluationMetrics_transformType :: Lens.Lens' EvaluationMetrics TransformType
evaluationMetrics_transformType = Lens.lens (\EvaluationMetrics' {transformType} -> transformType) (\s@EvaluationMetrics' {} a -> s {transformType = a} :: EvaluationMetrics)

instance Prelude.FromJSON EvaluationMetrics where
  parseJSON =
    Prelude.withObject
      "EvaluationMetrics"
      ( \x ->
          EvaluationMetrics'
            Prelude.<$> (x Prelude..:? "FindMatchesMetrics")
            Prelude.<*> (x Prelude..: "TransformType")
      )

instance Prelude.Hashable EvaluationMetrics

instance Prelude.NFData EvaluationMetrics
