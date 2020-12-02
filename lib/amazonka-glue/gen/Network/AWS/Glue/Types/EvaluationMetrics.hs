{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.EvaluationMetrics
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.EvaluationMetrics where

import Network.AWS.Glue.Types.FindMatchesMetrics
import Network.AWS.Glue.Types.TransformType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Evaluation metrics provide an estimate of the quality of your machine learning transform.
--
--
--
-- /See:/ 'evaluationMetrics' smart constructor.
data EvaluationMetrics = EvaluationMetrics'
  { _emFindMatchesMetrics ::
      !(Maybe FindMatchesMetrics),
    _emTransformType :: !TransformType
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'EvaluationMetrics' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'emFindMatchesMetrics' - The evaluation metrics for the find matches algorithm.
--
-- * 'emTransformType' - The type of machine learning transform.
evaluationMetrics ::
  -- | 'emTransformType'
  TransformType ->
  EvaluationMetrics
evaluationMetrics pTransformType_ =
  EvaluationMetrics'
    { _emFindMatchesMetrics = Nothing,
      _emTransformType = pTransformType_
    }

-- | The evaluation metrics for the find matches algorithm.
emFindMatchesMetrics :: Lens' EvaluationMetrics (Maybe FindMatchesMetrics)
emFindMatchesMetrics = lens _emFindMatchesMetrics (\s a -> s {_emFindMatchesMetrics = a})

-- | The type of machine learning transform.
emTransformType :: Lens' EvaluationMetrics TransformType
emTransformType = lens _emTransformType (\s a -> s {_emTransformType = a})

instance FromJSON EvaluationMetrics where
  parseJSON =
    withObject
      "EvaluationMetrics"
      ( \x ->
          EvaluationMetrics'
            <$> (x .:? "FindMatchesMetrics") <*> (x .: "TransformType")
      )

instance Hashable EvaluationMetrics

instance NFData EvaluationMetrics
