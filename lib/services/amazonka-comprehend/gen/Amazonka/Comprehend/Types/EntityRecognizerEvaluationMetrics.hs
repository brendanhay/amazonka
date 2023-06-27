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
-- Module      : Amazonka.Comprehend.Types.EntityRecognizerEvaluationMetrics
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Comprehend.Types.EntityRecognizerEvaluationMetrics where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Detailed information about the accuracy of an entity recognizer.
--
-- /See:/ 'newEntityRecognizerEvaluationMetrics' smart constructor.
data EntityRecognizerEvaluationMetrics = EntityRecognizerEvaluationMetrics'
  { -- | A measure of how accurate the recognizer results are for the test data.
    -- It is derived from the @Precision@ and @Recall@ values. The @F1Score@ is
    -- the harmonic average of the two scores. For plain text entity recognizer
    -- models, the range is 0 to 100, where 100 is the best score. For
    -- PDF\/Word entity recognizer models, the range is 0 to 1, where 1 is the
    -- best score.
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- the harmonic average of the two scores. For plain text entity recognizer
-- models, the range is 0 to 100, where 100 is the best score. For
-- PDF\/Word entity recognizer models, the range is 0 to 1, where 1 is the
-- best score.
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
-- the harmonic average of the two scores. For plain text entity recognizer
-- models, the range is 0 to 100, where 100 is the best score. For
-- PDF\/Word entity recognizer models, the range is 0 to 1, where 1 is the
-- best score.
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
  Data.FromJSON
    EntityRecognizerEvaluationMetrics
  where
  parseJSON =
    Data.withObject
      "EntityRecognizerEvaluationMetrics"
      ( \x ->
          EntityRecognizerEvaluationMetrics'
            Prelude.<$> (x Data..:? "F1Score")
            Prelude.<*> (x Data..:? "Precision")
            Prelude.<*> (x Data..:? "Recall")
      )

instance
  Prelude.Hashable
    EntityRecognizerEvaluationMetrics
  where
  hashWithSalt
    _salt
    EntityRecognizerEvaluationMetrics' {..} =
      _salt
        `Prelude.hashWithSalt` f1Score
        `Prelude.hashWithSalt` precision
        `Prelude.hashWithSalt` recall

instance
  Prelude.NFData
    EntityRecognizerEvaluationMetrics
  where
  rnf EntityRecognizerEvaluationMetrics' {..} =
    Prelude.rnf f1Score
      `Prelude.seq` Prelude.rnf precision
      `Prelude.seq` Prelude.rnf recall
