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
-- Module      : Network.AWS.Comprehend.Types.EntityRecognizerEvaluationMetrics
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Comprehend.Types.EntityRecognizerEvaluationMetrics where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Detailed information about the accuracy of an entity recognizer.
--
-- /See:/ 'newEntityRecognizerEvaluationMetrics' smart constructor.
data EntityRecognizerEvaluationMetrics = EntityRecognizerEvaluationMetrics'
  { -- | A measure of how accurate the recognizer results are for the test data.
    -- It is derived from the @Precision@ and @Recall@ values. The @F1Score@ is
    -- the harmonic average of the two scores. The highest score is 1, and the
    -- worst score is 0.
    f1Score :: Core.Maybe Core.Double,
    -- | A measure of the usefulness of the recognizer results in the test data.
    -- High precision means that the recognizer returned substantially more
    -- relevant results than irrelevant ones.
    precision :: Core.Maybe Core.Double,
    -- | A measure of how complete the recognizer results are for the test data.
    -- High recall means that the recognizer returned most of the relevant
    -- results.
    recall :: Core.Maybe Core.Double
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'EntityRecognizerEvaluationMetrics' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'f1Score', 'entityRecognizerEvaluationMetrics_f1Score' - A measure of how accurate the recognizer results are for the test data.
-- It is derived from the @Precision@ and @Recall@ values. The @F1Score@ is
-- the harmonic average of the two scores. The highest score is 1, and the
-- worst score is 0.
--
-- 'precision', 'entityRecognizerEvaluationMetrics_precision' - A measure of the usefulness of the recognizer results in the test data.
-- High precision means that the recognizer returned substantially more
-- relevant results than irrelevant ones.
--
-- 'recall', 'entityRecognizerEvaluationMetrics_recall' - A measure of how complete the recognizer results are for the test data.
-- High recall means that the recognizer returned most of the relevant
-- results.
newEntityRecognizerEvaluationMetrics ::
  EntityRecognizerEvaluationMetrics
newEntityRecognizerEvaluationMetrics =
  EntityRecognizerEvaluationMetrics'
    { f1Score =
        Core.Nothing,
      precision = Core.Nothing,
      recall = Core.Nothing
    }

-- | A measure of how accurate the recognizer results are for the test data.
-- It is derived from the @Precision@ and @Recall@ values. The @F1Score@ is
-- the harmonic average of the two scores. The highest score is 1, and the
-- worst score is 0.
entityRecognizerEvaluationMetrics_f1Score :: Lens.Lens' EntityRecognizerEvaluationMetrics (Core.Maybe Core.Double)
entityRecognizerEvaluationMetrics_f1Score = Lens.lens (\EntityRecognizerEvaluationMetrics' {f1Score} -> f1Score) (\s@EntityRecognizerEvaluationMetrics' {} a -> s {f1Score = a} :: EntityRecognizerEvaluationMetrics)

-- | A measure of the usefulness of the recognizer results in the test data.
-- High precision means that the recognizer returned substantially more
-- relevant results than irrelevant ones.
entityRecognizerEvaluationMetrics_precision :: Lens.Lens' EntityRecognizerEvaluationMetrics (Core.Maybe Core.Double)
entityRecognizerEvaluationMetrics_precision = Lens.lens (\EntityRecognizerEvaluationMetrics' {precision} -> precision) (\s@EntityRecognizerEvaluationMetrics' {} a -> s {precision = a} :: EntityRecognizerEvaluationMetrics)

-- | A measure of how complete the recognizer results are for the test data.
-- High recall means that the recognizer returned most of the relevant
-- results.
entityRecognizerEvaluationMetrics_recall :: Lens.Lens' EntityRecognizerEvaluationMetrics (Core.Maybe Core.Double)
entityRecognizerEvaluationMetrics_recall = Lens.lens (\EntityRecognizerEvaluationMetrics' {recall} -> recall) (\s@EntityRecognizerEvaluationMetrics' {} a -> s {recall = a} :: EntityRecognizerEvaluationMetrics)

instance
  Core.FromJSON
    EntityRecognizerEvaluationMetrics
  where
  parseJSON =
    Core.withObject
      "EntityRecognizerEvaluationMetrics"
      ( \x ->
          EntityRecognizerEvaluationMetrics'
            Core.<$> (x Core..:? "F1Score")
            Core.<*> (x Core..:? "Precision")
            Core.<*> (x Core..:? "Recall")
      )

instance
  Core.Hashable
    EntityRecognizerEvaluationMetrics

instance
  Core.NFData
    EntityRecognizerEvaluationMetrics
