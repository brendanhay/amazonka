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
-- Module      : Network.AWS.Glue.Types.FindMatchesMetrics
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.FindMatchesMetrics where

import Network.AWS.Glue.Types.ColumnImportance
import Network.AWS.Glue.Types.ConfusionMatrix
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The evaluation metrics for the find matches algorithm. The quality of
-- your machine learning transform is measured by getting your transform to
-- predict some matches and comparing the results to known matches from the
-- same dataset. The quality metrics are based on a subset of your data, so
-- they are not precise.
--
-- /See:/ 'newFindMatchesMetrics' smart constructor.
data FindMatchesMetrics = FindMatchesMetrics'
  { -- | The maximum F1 metric indicates the transform\'s accuracy between 0 and
    -- 1, where 1 is the best accuracy.
    --
    -- For more information, see
    -- <https://en.wikipedia.org/wiki/F1_score F1 score> in Wikipedia.
    f1 :: Prelude.Maybe Prelude.Double,
    -- | The confusion matrix shows you what your transform is predicting
    -- accurately and what types of errors it is making.
    --
    -- For more information, see
    -- <https://en.wikipedia.org/wiki/Confusion_matrix Confusion matrix> in
    -- Wikipedia.
    confusionMatrix :: Prelude.Maybe ConfusionMatrix,
    -- | A list of @ColumnImportance@ structures containing column importance
    -- metrics, sorted in order of descending importance.
    columnImportances :: Prelude.Maybe [ColumnImportance],
    -- | The precision metric indicates when often your transform is correct when
    -- it predicts a match. Specifically, it measures how well the transform
    -- finds true positives from the total true positives possible.
    --
    -- For more information, see
    -- <https://en.wikipedia.org/wiki/Precision_and_recall Precision and recall>
    -- in Wikipedia.
    precision :: Prelude.Maybe Prelude.Double,
    -- | The area under the precision\/recall curve (AUPRC) is a single number
    -- measuring the overall quality of the transform, that is independent of
    -- the choice made for precision vs. recall. Higher values indicate that
    -- you have a more attractive precision vs. recall tradeoff.
    --
    -- For more information, see
    -- <https://en.wikipedia.org/wiki/Precision_and_recall Precision and recall>
    -- in Wikipedia.
    areaUnderPRCurve :: Prelude.Maybe Prelude.Double,
    -- | The recall metric indicates that for an actual match, how often your
    -- transform predicts the match. Specifically, it measures how well the
    -- transform finds true positives from the total records in the source
    -- data.
    --
    -- For more information, see
    -- <https://en.wikipedia.org/wiki/Precision_and_recall Precision and recall>
    -- in Wikipedia.
    recall :: Prelude.Maybe Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'FindMatchesMetrics' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'f1', 'findMatchesMetrics_f1' - The maximum F1 metric indicates the transform\'s accuracy between 0 and
-- 1, where 1 is the best accuracy.
--
-- For more information, see
-- <https://en.wikipedia.org/wiki/F1_score F1 score> in Wikipedia.
--
-- 'confusionMatrix', 'findMatchesMetrics_confusionMatrix' - The confusion matrix shows you what your transform is predicting
-- accurately and what types of errors it is making.
--
-- For more information, see
-- <https://en.wikipedia.org/wiki/Confusion_matrix Confusion matrix> in
-- Wikipedia.
--
-- 'columnImportances', 'findMatchesMetrics_columnImportances' - A list of @ColumnImportance@ structures containing column importance
-- metrics, sorted in order of descending importance.
--
-- 'precision', 'findMatchesMetrics_precision' - The precision metric indicates when often your transform is correct when
-- it predicts a match. Specifically, it measures how well the transform
-- finds true positives from the total true positives possible.
--
-- For more information, see
-- <https://en.wikipedia.org/wiki/Precision_and_recall Precision and recall>
-- in Wikipedia.
--
-- 'areaUnderPRCurve', 'findMatchesMetrics_areaUnderPRCurve' - The area under the precision\/recall curve (AUPRC) is a single number
-- measuring the overall quality of the transform, that is independent of
-- the choice made for precision vs. recall. Higher values indicate that
-- you have a more attractive precision vs. recall tradeoff.
--
-- For more information, see
-- <https://en.wikipedia.org/wiki/Precision_and_recall Precision and recall>
-- in Wikipedia.
--
-- 'recall', 'findMatchesMetrics_recall' - The recall metric indicates that for an actual match, how often your
-- transform predicts the match. Specifically, it measures how well the
-- transform finds true positives from the total records in the source
-- data.
--
-- For more information, see
-- <https://en.wikipedia.org/wiki/Precision_and_recall Precision and recall>
-- in Wikipedia.
newFindMatchesMetrics ::
  FindMatchesMetrics
newFindMatchesMetrics =
  FindMatchesMetrics'
    { f1 = Prelude.Nothing,
      confusionMatrix = Prelude.Nothing,
      columnImportances = Prelude.Nothing,
      precision = Prelude.Nothing,
      areaUnderPRCurve = Prelude.Nothing,
      recall = Prelude.Nothing
    }

-- | The maximum F1 metric indicates the transform\'s accuracy between 0 and
-- 1, where 1 is the best accuracy.
--
-- For more information, see
-- <https://en.wikipedia.org/wiki/F1_score F1 score> in Wikipedia.
findMatchesMetrics_f1 :: Lens.Lens' FindMatchesMetrics (Prelude.Maybe Prelude.Double)
findMatchesMetrics_f1 = Lens.lens (\FindMatchesMetrics' {f1} -> f1) (\s@FindMatchesMetrics' {} a -> s {f1 = a} :: FindMatchesMetrics)

-- | The confusion matrix shows you what your transform is predicting
-- accurately and what types of errors it is making.
--
-- For more information, see
-- <https://en.wikipedia.org/wiki/Confusion_matrix Confusion matrix> in
-- Wikipedia.
findMatchesMetrics_confusionMatrix :: Lens.Lens' FindMatchesMetrics (Prelude.Maybe ConfusionMatrix)
findMatchesMetrics_confusionMatrix = Lens.lens (\FindMatchesMetrics' {confusionMatrix} -> confusionMatrix) (\s@FindMatchesMetrics' {} a -> s {confusionMatrix = a} :: FindMatchesMetrics)

-- | A list of @ColumnImportance@ structures containing column importance
-- metrics, sorted in order of descending importance.
findMatchesMetrics_columnImportances :: Lens.Lens' FindMatchesMetrics (Prelude.Maybe [ColumnImportance])
findMatchesMetrics_columnImportances = Lens.lens (\FindMatchesMetrics' {columnImportances} -> columnImportances) (\s@FindMatchesMetrics' {} a -> s {columnImportances = a} :: FindMatchesMetrics) Prelude.. Lens.mapping Prelude._Coerce

-- | The precision metric indicates when often your transform is correct when
-- it predicts a match. Specifically, it measures how well the transform
-- finds true positives from the total true positives possible.
--
-- For more information, see
-- <https://en.wikipedia.org/wiki/Precision_and_recall Precision and recall>
-- in Wikipedia.
findMatchesMetrics_precision :: Lens.Lens' FindMatchesMetrics (Prelude.Maybe Prelude.Double)
findMatchesMetrics_precision = Lens.lens (\FindMatchesMetrics' {precision} -> precision) (\s@FindMatchesMetrics' {} a -> s {precision = a} :: FindMatchesMetrics)

-- | The area under the precision\/recall curve (AUPRC) is a single number
-- measuring the overall quality of the transform, that is independent of
-- the choice made for precision vs. recall. Higher values indicate that
-- you have a more attractive precision vs. recall tradeoff.
--
-- For more information, see
-- <https://en.wikipedia.org/wiki/Precision_and_recall Precision and recall>
-- in Wikipedia.
findMatchesMetrics_areaUnderPRCurve :: Lens.Lens' FindMatchesMetrics (Prelude.Maybe Prelude.Double)
findMatchesMetrics_areaUnderPRCurve = Lens.lens (\FindMatchesMetrics' {areaUnderPRCurve} -> areaUnderPRCurve) (\s@FindMatchesMetrics' {} a -> s {areaUnderPRCurve = a} :: FindMatchesMetrics)

-- | The recall metric indicates that for an actual match, how often your
-- transform predicts the match. Specifically, it measures how well the
-- transform finds true positives from the total records in the source
-- data.
--
-- For more information, see
-- <https://en.wikipedia.org/wiki/Precision_and_recall Precision and recall>
-- in Wikipedia.
findMatchesMetrics_recall :: Lens.Lens' FindMatchesMetrics (Prelude.Maybe Prelude.Double)
findMatchesMetrics_recall = Lens.lens (\FindMatchesMetrics' {recall} -> recall) (\s@FindMatchesMetrics' {} a -> s {recall = a} :: FindMatchesMetrics)

instance Prelude.FromJSON FindMatchesMetrics where
  parseJSON =
    Prelude.withObject
      "FindMatchesMetrics"
      ( \x ->
          FindMatchesMetrics'
            Prelude.<$> (x Prelude..:? "F1")
            Prelude.<*> (x Prelude..:? "ConfusionMatrix")
            Prelude.<*> ( x Prelude..:? "ColumnImportances"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..:? "Precision")
            Prelude.<*> (x Prelude..:? "AreaUnderPRCurve")
            Prelude.<*> (x Prelude..:? "Recall")
      )

instance Prelude.Hashable FindMatchesMetrics

instance Prelude.NFData FindMatchesMetrics
