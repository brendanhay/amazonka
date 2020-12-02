{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.EvaluationResult
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.EvaluationResult where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Rekognition.Types.Summary

-- | The evaluation results for the training of a model.
--
--
--
-- /See:/ 'evaluationResult' smart constructor.
data EvaluationResult = EvaluationResult'
  { _erSummary ::
      !(Maybe Summary),
    _erF1Score :: !(Maybe Double)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'EvaluationResult' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'erSummary' - The S3 bucket that contains the training summary.
--
-- * 'erF1Score' - The F1 score for the evaluation of all labels. The F1 score metric evaluates the overall precision and recall performance of the model as a single value. A higher value indicates better precision and recall performance. A lower score indicates that precision, recall, or both are performing poorly.
evaluationResult ::
  EvaluationResult
evaluationResult =
  EvaluationResult' {_erSummary = Nothing, _erF1Score = Nothing}

-- | The S3 bucket that contains the training summary.
erSummary :: Lens' EvaluationResult (Maybe Summary)
erSummary = lens _erSummary (\s a -> s {_erSummary = a})

-- | The F1 score for the evaluation of all labels. The F1 score metric evaluates the overall precision and recall performance of the model as a single value. A higher value indicates better precision and recall performance. A lower score indicates that precision, recall, or both are performing poorly.
erF1Score :: Lens' EvaluationResult (Maybe Double)
erF1Score = lens _erF1Score (\s a -> s {_erF1Score = a})

instance FromJSON EvaluationResult where
  parseJSON =
    withObject
      "EvaluationResult"
      ( \x ->
          EvaluationResult' <$> (x .:? "Summary") <*> (x .:? "F1Score")
      )

instance Hashable EvaluationResult

instance NFData EvaluationResult
