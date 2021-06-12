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
-- Module      : Network.AWS.Comprehend.Types.EntityTypesEvaluationMetrics
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Comprehend.Types.EntityTypesEvaluationMetrics where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Detailed information about the accuracy of an entity recognizer for a
-- specific entity type.
--
-- /See:/ 'newEntityTypesEvaluationMetrics' smart constructor.
data EntityTypesEvaluationMetrics = EntityTypesEvaluationMetrics'
  { -- | A measure of how accurate the recognizer results are for a specific
    -- entity type in the test data. It is derived from the @Precision@ and
    -- @Recall@ values. The @F1Score@ is the harmonic average of the two
    -- scores. The highest score is 1, and the worst score is 0.
    f1Score :: Core.Maybe Core.Double,
    -- | A measure of the usefulness of the recognizer results for a specific
    -- entity type in the test data. High precision means that the recognizer
    -- returned substantially more relevant results than irrelevant ones.
    precision :: Core.Maybe Core.Double,
    -- | A measure of how complete the recognizer results are for a specific
    -- entity type in the test data. High recall means that the recognizer
    -- returned most of the relevant results.
    recall :: Core.Maybe Core.Double
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'EntityTypesEvaluationMetrics' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'f1Score', 'entityTypesEvaluationMetrics_f1Score' - A measure of how accurate the recognizer results are for a specific
-- entity type in the test data. It is derived from the @Precision@ and
-- @Recall@ values. The @F1Score@ is the harmonic average of the two
-- scores. The highest score is 1, and the worst score is 0.
--
-- 'precision', 'entityTypesEvaluationMetrics_precision' - A measure of the usefulness of the recognizer results for a specific
-- entity type in the test data. High precision means that the recognizer
-- returned substantially more relevant results than irrelevant ones.
--
-- 'recall', 'entityTypesEvaluationMetrics_recall' - A measure of how complete the recognizer results are for a specific
-- entity type in the test data. High recall means that the recognizer
-- returned most of the relevant results.
newEntityTypesEvaluationMetrics ::
  EntityTypesEvaluationMetrics
newEntityTypesEvaluationMetrics =
  EntityTypesEvaluationMetrics'
    { f1Score =
        Core.Nothing,
      precision = Core.Nothing,
      recall = Core.Nothing
    }

-- | A measure of how accurate the recognizer results are for a specific
-- entity type in the test data. It is derived from the @Precision@ and
-- @Recall@ values. The @F1Score@ is the harmonic average of the two
-- scores. The highest score is 1, and the worst score is 0.
entityTypesEvaluationMetrics_f1Score :: Lens.Lens' EntityTypesEvaluationMetrics (Core.Maybe Core.Double)
entityTypesEvaluationMetrics_f1Score = Lens.lens (\EntityTypesEvaluationMetrics' {f1Score} -> f1Score) (\s@EntityTypesEvaluationMetrics' {} a -> s {f1Score = a} :: EntityTypesEvaluationMetrics)

-- | A measure of the usefulness of the recognizer results for a specific
-- entity type in the test data. High precision means that the recognizer
-- returned substantially more relevant results than irrelevant ones.
entityTypesEvaluationMetrics_precision :: Lens.Lens' EntityTypesEvaluationMetrics (Core.Maybe Core.Double)
entityTypesEvaluationMetrics_precision = Lens.lens (\EntityTypesEvaluationMetrics' {precision} -> precision) (\s@EntityTypesEvaluationMetrics' {} a -> s {precision = a} :: EntityTypesEvaluationMetrics)

-- | A measure of how complete the recognizer results are for a specific
-- entity type in the test data. High recall means that the recognizer
-- returned most of the relevant results.
entityTypesEvaluationMetrics_recall :: Lens.Lens' EntityTypesEvaluationMetrics (Core.Maybe Core.Double)
entityTypesEvaluationMetrics_recall = Lens.lens (\EntityTypesEvaluationMetrics' {recall} -> recall) (\s@EntityTypesEvaluationMetrics' {} a -> s {recall = a} :: EntityTypesEvaluationMetrics)

instance Core.FromJSON EntityTypesEvaluationMetrics where
  parseJSON =
    Core.withObject
      "EntityTypesEvaluationMetrics"
      ( \x ->
          EntityTypesEvaluationMetrics'
            Core.<$> (x Core..:? "F1Score")
            Core.<*> (x Core..:? "Precision")
            Core.<*> (x Core..:? "Recall")
      )

instance Core.Hashable EntityTypesEvaluationMetrics

instance Core.NFData EntityTypesEvaluationMetrics
