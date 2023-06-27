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
-- Module      : Amazonka.Comprehend.Types.ClassifierEvaluationMetrics
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Comprehend.Types.ClassifierEvaluationMetrics where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes the result metrics for the test data associated with an
-- documentation classifier.
--
-- /See:/ 'newClassifierEvaluationMetrics' smart constructor.
data ClassifierEvaluationMetrics = ClassifierEvaluationMetrics'
  { -- | The fraction of the labels that were correct recognized. It is computed
    -- by dividing the number of labels in the test documents that were
    -- correctly recognized by the total number of labels in the test
    -- documents.
    accuracy :: Prelude.Maybe Prelude.Double,
    -- | A measure of how accurate the classifier results are for the test data.
    -- It is derived from the @Precision@ and @Recall@ values. The @F1Score@ is
    -- the harmonic average of the two scores. The highest score is 1, and the
    -- worst score is 0.
    f1Score :: Prelude.Maybe Prelude.Double,
    -- | Indicates the fraction of labels that are incorrectly predicted. Also
    -- seen as the fraction of wrong labels compared to the total number of
    -- labels. Scores closer to zero are better.
    hammingLoss :: Prelude.Maybe Prelude.Double,
    -- | A measure of how accurate the classifier results are for the test data.
    -- It is a combination of the @Micro Precision@ and @Micro Recall@ values.
    -- The @Micro F1Score@ is the harmonic mean of the two scores. The highest
    -- score is 1, and the worst score is 0.
    microF1Score :: Prelude.Maybe Prelude.Double,
    -- | A measure of the usefulness of the recognizer results in the test data.
    -- High precision means that the recognizer returned substantially more
    -- relevant results than irrelevant ones. Unlike the Precision metric which
    -- comes from averaging the precision of all available labels, this is
    -- based on the overall score of all precision scores added together.
    microPrecision :: Prelude.Maybe Prelude.Double,
    -- | A measure of how complete the classifier results are for the test data.
    -- High recall means that the classifier returned most of the relevant
    -- results. Specifically, this indicates how many of the correct categories
    -- in the text that the model can predict. It is a percentage of correct
    -- categories in the text that can found. Instead of averaging the recall
    -- scores of all labels (as with Recall), micro Recall is based on the
    -- overall score of all recall scores added together.
    microRecall :: Prelude.Maybe Prelude.Double,
    -- | A measure of the usefulness of the classifier results in the test data.
    -- High precision means that the classifier returned substantially more
    -- relevant results than irrelevant ones.
    precision :: Prelude.Maybe Prelude.Double,
    -- | A measure of how complete the classifier results are for the test data.
    -- High recall means that the classifier returned most of the relevant
    -- results.
    recall :: Prelude.Maybe Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ClassifierEvaluationMetrics' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accuracy', 'classifierEvaluationMetrics_accuracy' - The fraction of the labels that were correct recognized. It is computed
-- by dividing the number of labels in the test documents that were
-- correctly recognized by the total number of labels in the test
-- documents.
--
-- 'f1Score', 'classifierEvaluationMetrics_f1Score' - A measure of how accurate the classifier results are for the test data.
-- It is derived from the @Precision@ and @Recall@ values. The @F1Score@ is
-- the harmonic average of the two scores. The highest score is 1, and the
-- worst score is 0.
--
-- 'hammingLoss', 'classifierEvaluationMetrics_hammingLoss' - Indicates the fraction of labels that are incorrectly predicted. Also
-- seen as the fraction of wrong labels compared to the total number of
-- labels. Scores closer to zero are better.
--
-- 'microF1Score', 'classifierEvaluationMetrics_microF1Score' - A measure of how accurate the classifier results are for the test data.
-- It is a combination of the @Micro Precision@ and @Micro Recall@ values.
-- The @Micro F1Score@ is the harmonic mean of the two scores. The highest
-- score is 1, and the worst score is 0.
--
-- 'microPrecision', 'classifierEvaluationMetrics_microPrecision' - A measure of the usefulness of the recognizer results in the test data.
-- High precision means that the recognizer returned substantially more
-- relevant results than irrelevant ones. Unlike the Precision metric which
-- comes from averaging the precision of all available labels, this is
-- based on the overall score of all precision scores added together.
--
-- 'microRecall', 'classifierEvaluationMetrics_microRecall' - A measure of how complete the classifier results are for the test data.
-- High recall means that the classifier returned most of the relevant
-- results. Specifically, this indicates how many of the correct categories
-- in the text that the model can predict. It is a percentage of correct
-- categories in the text that can found. Instead of averaging the recall
-- scores of all labels (as with Recall), micro Recall is based on the
-- overall score of all recall scores added together.
--
-- 'precision', 'classifierEvaluationMetrics_precision' - A measure of the usefulness of the classifier results in the test data.
-- High precision means that the classifier returned substantially more
-- relevant results than irrelevant ones.
--
-- 'recall', 'classifierEvaluationMetrics_recall' - A measure of how complete the classifier results are for the test data.
-- High recall means that the classifier returned most of the relevant
-- results.
newClassifierEvaluationMetrics ::
  ClassifierEvaluationMetrics
newClassifierEvaluationMetrics =
  ClassifierEvaluationMetrics'
    { accuracy =
        Prelude.Nothing,
      f1Score = Prelude.Nothing,
      hammingLoss = Prelude.Nothing,
      microF1Score = Prelude.Nothing,
      microPrecision = Prelude.Nothing,
      microRecall = Prelude.Nothing,
      precision = Prelude.Nothing,
      recall = Prelude.Nothing
    }

-- | The fraction of the labels that were correct recognized. It is computed
-- by dividing the number of labels in the test documents that were
-- correctly recognized by the total number of labels in the test
-- documents.
classifierEvaluationMetrics_accuracy :: Lens.Lens' ClassifierEvaluationMetrics (Prelude.Maybe Prelude.Double)
classifierEvaluationMetrics_accuracy = Lens.lens (\ClassifierEvaluationMetrics' {accuracy} -> accuracy) (\s@ClassifierEvaluationMetrics' {} a -> s {accuracy = a} :: ClassifierEvaluationMetrics)

-- | A measure of how accurate the classifier results are for the test data.
-- It is derived from the @Precision@ and @Recall@ values. The @F1Score@ is
-- the harmonic average of the two scores. The highest score is 1, and the
-- worst score is 0.
classifierEvaluationMetrics_f1Score :: Lens.Lens' ClassifierEvaluationMetrics (Prelude.Maybe Prelude.Double)
classifierEvaluationMetrics_f1Score = Lens.lens (\ClassifierEvaluationMetrics' {f1Score} -> f1Score) (\s@ClassifierEvaluationMetrics' {} a -> s {f1Score = a} :: ClassifierEvaluationMetrics)

-- | Indicates the fraction of labels that are incorrectly predicted. Also
-- seen as the fraction of wrong labels compared to the total number of
-- labels. Scores closer to zero are better.
classifierEvaluationMetrics_hammingLoss :: Lens.Lens' ClassifierEvaluationMetrics (Prelude.Maybe Prelude.Double)
classifierEvaluationMetrics_hammingLoss = Lens.lens (\ClassifierEvaluationMetrics' {hammingLoss} -> hammingLoss) (\s@ClassifierEvaluationMetrics' {} a -> s {hammingLoss = a} :: ClassifierEvaluationMetrics)

-- | A measure of how accurate the classifier results are for the test data.
-- It is a combination of the @Micro Precision@ and @Micro Recall@ values.
-- The @Micro F1Score@ is the harmonic mean of the two scores. The highest
-- score is 1, and the worst score is 0.
classifierEvaluationMetrics_microF1Score :: Lens.Lens' ClassifierEvaluationMetrics (Prelude.Maybe Prelude.Double)
classifierEvaluationMetrics_microF1Score = Lens.lens (\ClassifierEvaluationMetrics' {microF1Score} -> microF1Score) (\s@ClassifierEvaluationMetrics' {} a -> s {microF1Score = a} :: ClassifierEvaluationMetrics)

-- | A measure of the usefulness of the recognizer results in the test data.
-- High precision means that the recognizer returned substantially more
-- relevant results than irrelevant ones. Unlike the Precision metric which
-- comes from averaging the precision of all available labels, this is
-- based on the overall score of all precision scores added together.
classifierEvaluationMetrics_microPrecision :: Lens.Lens' ClassifierEvaluationMetrics (Prelude.Maybe Prelude.Double)
classifierEvaluationMetrics_microPrecision = Lens.lens (\ClassifierEvaluationMetrics' {microPrecision} -> microPrecision) (\s@ClassifierEvaluationMetrics' {} a -> s {microPrecision = a} :: ClassifierEvaluationMetrics)

-- | A measure of how complete the classifier results are for the test data.
-- High recall means that the classifier returned most of the relevant
-- results. Specifically, this indicates how many of the correct categories
-- in the text that the model can predict. It is a percentage of correct
-- categories in the text that can found. Instead of averaging the recall
-- scores of all labels (as with Recall), micro Recall is based on the
-- overall score of all recall scores added together.
classifierEvaluationMetrics_microRecall :: Lens.Lens' ClassifierEvaluationMetrics (Prelude.Maybe Prelude.Double)
classifierEvaluationMetrics_microRecall = Lens.lens (\ClassifierEvaluationMetrics' {microRecall} -> microRecall) (\s@ClassifierEvaluationMetrics' {} a -> s {microRecall = a} :: ClassifierEvaluationMetrics)

-- | A measure of the usefulness of the classifier results in the test data.
-- High precision means that the classifier returned substantially more
-- relevant results than irrelevant ones.
classifierEvaluationMetrics_precision :: Lens.Lens' ClassifierEvaluationMetrics (Prelude.Maybe Prelude.Double)
classifierEvaluationMetrics_precision = Lens.lens (\ClassifierEvaluationMetrics' {precision} -> precision) (\s@ClassifierEvaluationMetrics' {} a -> s {precision = a} :: ClassifierEvaluationMetrics)

-- | A measure of how complete the classifier results are for the test data.
-- High recall means that the classifier returned most of the relevant
-- results.
classifierEvaluationMetrics_recall :: Lens.Lens' ClassifierEvaluationMetrics (Prelude.Maybe Prelude.Double)
classifierEvaluationMetrics_recall = Lens.lens (\ClassifierEvaluationMetrics' {recall} -> recall) (\s@ClassifierEvaluationMetrics' {} a -> s {recall = a} :: ClassifierEvaluationMetrics)

instance Data.FromJSON ClassifierEvaluationMetrics where
  parseJSON =
    Data.withObject
      "ClassifierEvaluationMetrics"
      ( \x ->
          ClassifierEvaluationMetrics'
            Prelude.<$> (x Data..:? "Accuracy")
            Prelude.<*> (x Data..:? "F1Score")
            Prelude.<*> (x Data..:? "HammingLoss")
            Prelude.<*> (x Data..:? "MicroF1Score")
            Prelude.<*> (x Data..:? "MicroPrecision")
            Prelude.<*> (x Data..:? "MicroRecall")
            Prelude.<*> (x Data..:? "Precision")
            Prelude.<*> (x Data..:? "Recall")
      )

instance Prelude.Hashable ClassifierEvaluationMetrics where
  hashWithSalt _salt ClassifierEvaluationMetrics' {..} =
    _salt
      `Prelude.hashWithSalt` accuracy
      `Prelude.hashWithSalt` f1Score
      `Prelude.hashWithSalt` hammingLoss
      `Prelude.hashWithSalt` microF1Score
      `Prelude.hashWithSalt` microPrecision
      `Prelude.hashWithSalt` microRecall
      `Prelude.hashWithSalt` precision
      `Prelude.hashWithSalt` recall

instance Prelude.NFData ClassifierEvaluationMetrics where
  rnf ClassifierEvaluationMetrics' {..} =
    Prelude.rnf accuracy
      `Prelude.seq` Prelude.rnf f1Score
      `Prelude.seq` Prelude.rnf hammingLoss
      `Prelude.seq` Prelude.rnf microF1Score
      `Prelude.seq` Prelude.rnf microPrecision
      `Prelude.seq` Prelude.rnf microRecall
      `Prelude.seq` Prelude.rnf precision
      `Prelude.seq` Prelude.rnf recall
