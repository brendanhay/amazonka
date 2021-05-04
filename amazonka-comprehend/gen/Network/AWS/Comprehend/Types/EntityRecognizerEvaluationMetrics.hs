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
-- Module      : Network.AWS.Comprehend.Types.EntityRecognizerEvaluationMetrics
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Comprehend.Types.EntityRecognizerEvaluationMetrics where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Detailed information about the accuracy of an entity recognizer.
--
-- /See:/ 'newEntityRecognizerEvaluationMetrics' smart constructor.
data EntityRecognizerEvaluationMetrics = EntityRecognizerEvaluationMetrics'
  { -- | A measure of how accurate the recognizer results are for the test data.
    -- It is derived from the @Precision@ and @Recall@ values. The @F1Score@ is
    -- the harmonic average of the two scores. The highest score is 1, and the
    -- worst score is 0.
    f1Score :: Prelude.Maybe Prelude.Double,
    -- | A measure of the usefulness of the recognizer results in the test data.
    -- High precision means that the recognizer returned substantially more
    -- relevant results than irrelevant ones.
    precision :: Prelude.Maybe Prelude.Double,
    -- | A measure of how complete the recognizer results are for the test data.
    -- High recall means that the recognizer returned most of the relevant
    -- results.
    recall :: Prelude.Maybe Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
        Prelude.Nothing,
      precision = Prelude.Nothing,
      recall = Prelude.Nothing
    }

-- | A measure of how accurate the recognizer results are for the test data.
-- It is derived from the @Precision@ and @Recall@ values. The @F1Score@ is
-- the harmonic average of the two scores. The highest score is 1, and the
-- worst score is 0.
entityRecognizerEvaluationMetrics_f1Score :: Lens.Lens' EntityRecognizerEvaluationMetrics (Prelude.Maybe Prelude.Double)
entityRecognizerEvaluationMetrics_f1Score = Lens.lens (\EntityRecognizerEvaluationMetrics' {f1Score} -> f1Score) (\s@EntityRecognizerEvaluationMetrics' {} a -> s {f1Score = a} :: EntityRecognizerEvaluationMetrics)

-- | A measure of the usefulness of the recognizer results in the test data.
-- High precision means that the recognizer returned substantially more
-- relevant results than irrelevant ones.
entityRecognizerEvaluationMetrics_precision :: Lens.Lens' EntityRecognizerEvaluationMetrics (Prelude.Maybe Prelude.Double)
entityRecognizerEvaluationMetrics_precision = Lens.lens (\EntityRecognizerEvaluationMetrics' {precision} -> precision) (\s@EntityRecognizerEvaluationMetrics' {} a -> s {precision = a} :: EntityRecognizerEvaluationMetrics)

-- | A measure of how complete the recognizer results are for the test data.
-- High recall means that the recognizer returned most of the relevant
-- results.
entityRecognizerEvaluationMetrics_recall :: Lens.Lens' EntityRecognizerEvaluationMetrics (Prelude.Maybe Prelude.Double)
entityRecognizerEvaluationMetrics_recall = Lens.lens (\EntityRecognizerEvaluationMetrics' {recall} -> recall) (\s@EntityRecognizerEvaluationMetrics' {} a -> s {recall = a} :: EntityRecognizerEvaluationMetrics)

instance
  Prelude.FromJSON
    EntityRecognizerEvaluationMetrics
  where
  parseJSON =
    Prelude.withObject
      "EntityRecognizerEvaluationMetrics"
      ( \x ->
          EntityRecognizerEvaluationMetrics'
            Prelude.<$> (x Prelude..:? "F1Score")
            Prelude.<*> (x Prelude..:? "Precision")
            Prelude.<*> (x Prelude..:? "Recall")
      )

instance
  Prelude.Hashable
    EntityRecognizerEvaluationMetrics

instance
  Prelude.NFData
    EntityRecognizerEvaluationMetrics
