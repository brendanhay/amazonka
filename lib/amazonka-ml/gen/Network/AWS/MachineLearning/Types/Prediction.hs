{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MachineLearning.Types.Prediction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MachineLearning.Types.Prediction where

import Network.AWS.Lens
import Network.AWS.MachineLearning.Types.DetailsAttributes
import Network.AWS.Prelude

-- | The output from a @Predict@ operation:
--
--
--     * @Details@ - Contains the following attributes: @DetailsAttributes.PREDICTIVE_MODEL_TYPE - REGRESSION | BINARY | MULTICLASS@ @DetailsAttributes.ALGORITHM - SGD@
--
--     * @PredictedLabel@ - Present for either a @BINARY@ or @MULTICLASS@ @MLModel@ request.
--
--     * @PredictedScores@ - Contains the raw classification score corresponding to each label.
--
--     * @PredictedValue@ - Present for a @REGRESSION@ @MLModel@ request.
--
--
--
--
-- /See:/ 'prediction' smart constructor.
data Prediction = Prediction'
  { _pPredictedValue :: !(Maybe Double),
    _pPredictedLabel :: !(Maybe Text),
    _pPredictedScores :: !(Maybe (Map Text (Double))),
    _pDetails :: !(Maybe (Map DetailsAttributes (Text)))
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Prediction' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pPredictedValue' - The prediction value for @REGRESSION@ @MLModel@ .
--
-- * 'pPredictedLabel' - The prediction label for either a @BINARY@ or @MULTICLASS@ @MLModel@ .
--
-- * 'pPredictedScores' - Undocumented member.
--
-- * 'pDetails' - Undocumented member.
prediction ::
  Prediction
prediction =
  Prediction'
    { _pPredictedValue = Nothing,
      _pPredictedLabel = Nothing,
      _pPredictedScores = Nothing,
      _pDetails = Nothing
    }

-- | The prediction value for @REGRESSION@ @MLModel@ .
pPredictedValue :: Lens' Prediction (Maybe Double)
pPredictedValue = lens _pPredictedValue (\s a -> s {_pPredictedValue = a})

-- | The prediction label for either a @BINARY@ or @MULTICLASS@ @MLModel@ .
pPredictedLabel :: Lens' Prediction (Maybe Text)
pPredictedLabel = lens _pPredictedLabel (\s a -> s {_pPredictedLabel = a})

-- | Undocumented member.
pPredictedScores :: Lens' Prediction (HashMap Text (Double))
pPredictedScores = lens _pPredictedScores (\s a -> s {_pPredictedScores = a}) . _Default . _Map

-- | Undocumented member.
pDetails :: Lens' Prediction (HashMap DetailsAttributes (Text))
pDetails = lens _pDetails (\s a -> s {_pDetails = a}) . _Default . _Map

instance FromJSON Prediction where
  parseJSON =
    withObject
      "Prediction"
      ( \x ->
          Prediction'
            <$> (x .:? "predictedValue")
            <*> (x .:? "predictedLabel")
            <*> (x .:? "predictedScores" .!= mempty)
            <*> (x .:? "details" .!= mempty)
      )

instance Hashable Prediction

instance NFData Prediction
