{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.FindMatchesMetrics
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.FindMatchesMetrics where

import Network.AWS.Glue.Types.ConfusionMatrix
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The evaluation metrics for the find matches algorithm. The quality of your machine learning transform is measured by getting your transform to predict some matches and comparing the results to known matches from the same dataset. The quality metrics are based on a subset of your data, so they are not precise.
--
--
--
-- /See:/ 'findMatchesMetrics' smart constructor.
data FindMatchesMetrics = FindMatchesMetrics'
  { _fmmF1 ::
      !(Maybe Double),
    _fmmAreaUnderPRCurve :: !(Maybe Double),
    _fmmRecall :: !(Maybe Double),
    _fmmPrecision :: !(Maybe Double),
    _fmmConfusionMatrix :: !(Maybe ConfusionMatrix)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'FindMatchesMetrics' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fmmF1' - The maximum F1 metric indicates the transform's accuracy between 0 and 1, where 1 is the best accuracy. For more information, see <https://en.wikipedia.org/wiki/F1_score F1 score> in Wikipedia.
--
-- * 'fmmAreaUnderPRCurve' - The area under the precision/recall curve (AUPRC) is a single number measuring the overall quality of the transform, that is independent of the choice made for precision vs. recall. Higher values indicate that you have a more attractive precision vs. recall tradeoff. For more information, see <https://en.wikipedia.org/wiki/Precision_and_recall Precision and recall> in Wikipedia.
--
-- * 'fmmRecall' - The recall metric indicates that for an actual match, how often your transform predicts the match. Specifically, it measures how well the transform finds true positives from the total records in the source data. For more information, see <https://en.wikipedia.org/wiki/Precision_and_recall Precision and recall> in Wikipedia.
--
-- * 'fmmPrecision' - The precision metric indicates when often your transform is correct when it predicts a match. Specifically, it measures how well the transform finds true positives from the total true positives possible. For more information, see <https://en.wikipedia.org/wiki/Precision_and_recall Precision and recall> in Wikipedia.
--
-- * 'fmmConfusionMatrix' - The confusion matrix shows you what your transform is predicting accurately and what types of errors it is making. For more information, see <https://en.wikipedia.org/wiki/Confusion_matrix Confusion matrix> in Wikipedia.
findMatchesMetrics ::
  FindMatchesMetrics
findMatchesMetrics =
  FindMatchesMetrics'
    { _fmmF1 = Nothing,
      _fmmAreaUnderPRCurve = Nothing,
      _fmmRecall = Nothing,
      _fmmPrecision = Nothing,
      _fmmConfusionMatrix = Nothing
    }

-- | The maximum F1 metric indicates the transform's accuracy between 0 and 1, where 1 is the best accuracy. For more information, see <https://en.wikipedia.org/wiki/F1_score F1 score> in Wikipedia.
fmmF1 :: Lens' FindMatchesMetrics (Maybe Double)
fmmF1 = lens _fmmF1 (\s a -> s {_fmmF1 = a})

-- | The area under the precision/recall curve (AUPRC) is a single number measuring the overall quality of the transform, that is independent of the choice made for precision vs. recall. Higher values indicate that you have a more attractive precision vs. recall tradeoff. For more information, see <https://en.wikipedia.org/wiki/Precision_and_recall Precision and recall> in Wikipedia.
fmmAreaUnderPRCurve :: Lens' FindMatchesMetrics (Maybe Double)
fmmAreaUnderPRCurve = lens _fmmAreaUnderPRCurve (\s a -> s {_fmmAreaUnderPRCurve = a})

-- | The recall metric indicates that for an actual match, how often your transform predicts the match. Specifically, it measures how well the transform finds true positives from the total records in the source data. For more information, see <https://en.wikipedia.org/wiki/Precision_and_recall Precision and recall> in Wikipedia.
fmmRecall :: Lens' FindMatchesMetrics (Maybe Double)
fmmRecall = lens _fmmRecall (\s a -> s {_fmmRecall = a})

-- | The precision metric indicates when often your transform is correct when it predicts a match. Specifically, it measures how well the transform finds true positives from the total true positives possible. For more information, see <https://en.wikipedia.org/wiki/Precision_and_recall Precision and recall> in Wikipedia.
fmmPrecision :: Lens' FindMatchesMetrics (Maybe Double)
fmmPrecision = lens _fmmPrecision (\s a -> s {_fmmPrecision = a})

-- | The confusion matrix shows you what your transform is predicting accurately and what types of errors it is making. For more information, see <https://en.wikipedia.org/wiki/Confusion_matrix Confusion matrix> in Wikipedia.
fmmConfusionMatrix :: Lens' FindMatchesMetrics (Maybe ConfusionMatrix)
fmmConfusionMatrix = lens _fmmConfusionMatrix (\s a -> s {_fmmConfusionMatrix = a})

instance FromJSON FindMatchesMetrics where
  parseJSON =
    withObject
      "FindMatchesMetrics"
      ( \x ->
          FindMatchesMetrics'
            <$> (x .:? "F1")
            <*> (x .:? "AreaUnderPRCurve")
            <*> (x .:? "Recall")
            <*> (x .:? "Precision")
            <*> (x .:? "ConfusionMatrix")
      )

instance Hashable FindMatchesMetrics

instance NFData FindMatchesMetrics
