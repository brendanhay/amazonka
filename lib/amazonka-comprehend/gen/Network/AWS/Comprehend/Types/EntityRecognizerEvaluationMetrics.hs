{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.Types.EntityRecognizerEvaluationMetrics
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Comprehend.Types.EntityRecognizerEvaluationMetrics where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Detailed information about the accuracy of an entity recognizer.
--
--
--
-- /See:/ 'entityRecognizerEvaluationMetrics' smart constructor.
data EntityRecognizerEvaluationMetrics = EntityRecognizerEvaluationMetrics'
  { _eremRecall ::
      !(Maybe Double),
    _eremPrecision ::
      !(Maybe Double),
    _eremF1Score ::
      !(Maybe Double)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'EntityRecognizerEvaluationMetrics' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'eremRecall' - A measure of how complete the recognizer results are for the test data. High recall means that the recognizer returned most of the relevant results.
--
-- * 'eremPrecision' - A measure of the usefulness of the recognizer results in the test data. High precision means that the recognizer returned substantially more relevant results than irrelevant ones.
--
-- * 'eremF1Score' - A measure of how accurate the recognizer results are for the test data. It is derived from the @Precision@ and @Recall@ values. The @F1Score@ is the harmonic average of the two scores. The highest score is 1, and the worst score is 0.
entityRecognizerEvaluationMetrics ::
  EntityRecognizerEvaluationMetrics
entityRecognizerEvaluationMetrics =
  EntityRecognizerEvaluationMetrics'
    { _eremRecall = Nothing,
      _eremPrecision = Nothing,
      _eremF1Score = Nothing
    }

-- | A measure of how complete the recognizer results are for the test data. High recall means that the recognizer returned most of the relevant results.
eremRecall :: Lens' EntityRecognizerEvaluationMetrics (Maybe Double)
eremRecall = lens _eremRecall (\s a -> s {_eremRecall = a})

-- | A measure of the usefulness of the recognizer results in the test data. High precision means that the recognizer returned substantially more relevant results than irrelevant ones.
eremPrecision :: Lens' EntityRecognizerEvaluationMetrics (Maybe Double)
eremPrecision = lens _eremPrecision (\s a -> s {_eremPrecision = a})

-- | A measure of how accurate the recognizer results are for the test data. It is derived from the @Precision@ and @Recall@ values. The @F1Score@ is the harmonic average of the two scores. The highest score is 1, and the worst score is 0.
eremF1Score :: Lens' EntityRecognizerEvaluationMetrics (Maybe Double)
eremF1Score = lens _eremF1Score (\s a -> s {_eremF1Score = a})

instance FromJSON EntityRecognizerEvaluationMetrics where
  parseJSON =
    withObject
      "EntityRecognizerEvaluationMetrics"
      ( \x ->
          EntityRecognizerEvaluationMetrics'
            <$> (x .:? "Recall") <*> (x .:? "Precision") <*> (x .:? "F1Score")
      )

instance Hashable EntityRecognizerEvaluationMetrics

instance NFData EntityRecognizerEvaluationMetrics
