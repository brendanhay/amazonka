{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.Types.EntityTypesEvaluationMetrics
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Comprehend.Types.EntityTypesEvaluationMetrics where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Detailed information about the accuracy of an entity recognizer for a specific entity type.
--
--
--
-- /See:/ 'entityTypesEvaluationMetrics' smart constructor.
data EntityTypesEvaluationMetrics = EntityTypesEvaluationMetrics'
  { _etemRecall ::
      !(Maybe Double),
    _etemPrecision :: !(Maybe Double),
    _etemF1Score :: !(Maybe Double)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'EntityTypesEvaluationMetrics' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'etemRecall' - A measure of how complete the recognizer results are for a specific entity type in the test data. High recall means that the recognizer returned most of the relevant results.
--
-- * 'etemPrecision' - A measure of the usefulness of the recognizer results for a specific entity type in the test data. High precision means that the recognizer returned substantially more relevant results than irrelevant ones.
--
-- * 'etemF1Score' - A measure of how accurate the recognizer results are for a specific entity type in the test data. It is derived from the @Precision@ and @Recall@ values. The @F1Score@ is the harmonic average of the two scores. The highest score is 1, and the worst score is 0.
entityTypesEvaluationMetrics ::
  EntityTypesEvaluationMetrics
entityTypesEvaluationMetrics =
  EntityTypesEvaluationMetrics'
    { _etemRecall = Nothing,
      _etemPrecision = Nothing,
      _etemF1Score = Nothing
    }

-- | A measure of how complete the recognizer results are for a specific entity type in the test data. High recall means that the recognizer returned most of the relevant results.
etemRecall :: Lens' EntityTypesEvaluationMetrics (Maybe Double)
etemRecall = lens _etemRecall (\s a -> s {_etemRecall = a})

-- | A measure of the usefulness of the recognizer results for a specific entity type in the test data. High precision means that the recognizer returned substantially more relevant results than irrelevant ones.
etemPrecision :: Lens' EntityTypesEvaluationMetrics (Maybe Double)
etemPrecision = lens _etemPrecision (\s a -> s {_etemPrecision = a})

-- | A measure of how accurate the recognizer results are for a specific entity type in the test data. It is derived from the @Precision@ and @Recall@ values. The @F1Score@ is the harmonic average of the two scores. The highest score is 1, and the worst score is 0.
etemF1Score :: Lens' EntityTypesEvaluationMetrics (Maybe Double)
etemF1Score = lens _etemF1Score (\s a -> s {_etemF1Score = a})

instance FromJSON EntityTypesEvaluationMetrics where
  parseJSON =
    withObject
      "EntityTypesEvaluationMetrics"
      ( \x ->
          EntityTypesEvaluationMetrics'
            <$> (x .:? "Recall") <*> (x .:? "Precision") <*> (x .:? "F1Score")
      )

instance Hashable EntityTypesEvaluationMetrics

instance NFData EntityTypesEvaluationMetrics
