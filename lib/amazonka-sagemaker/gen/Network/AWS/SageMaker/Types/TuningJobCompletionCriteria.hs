{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.TuningJobCompletionCriteria
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.TuningJobCompletionCriteria where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The job completion criteria.
--
--
--
-- /See:/ 'tuningJobCompletionCriteria' smart constructor.
newtype TuningJobCompletionCriteria = TuningJobCompletionCriteria'
  { _tjccTargetObjectiveMetricValue ::
      Double
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TuningJobCompletionCriteria' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tjccTargetObjectiveMetricValue' - The value of the objective metric.
tuningJobCompletionCriteria ::
  -- | 'tjccTargetObjectiveMetricValue'
  Double ->
  TuningJobCompletionCriteria
tuningJobCompletionCriteria pTargetObjectiveMetricValue_ =
  TuningJobCompletionCriteria'
    { _tjccTargetObjectiveMetricValue =
        pTargetObjectiveMetricValue_
    }

-- | The value of the objective metric.
tjccTargetObjectiveMetricValue :: Lens' TuningJobCompletionCriteria Double
tjccTargetObjectiveMetricValue = lens _tjccTargetObjectiveMetricValue (\s a -> s {_tjccTargetObjectiveMetricValue = a})

instance FromJSON TuningJobCompletionCriteria where
  parseJSON =
    withObject
      "TuningJobCompletionCriteria"
      ( \x ->
          TuningJobCompletionCriteria'
            <$> (x .: "TargetObjectiveMetricValue")
      )

instance Hashable TuningJobCompletionCriteria

instance NFData TuningJobCompletionCriteria

instance ToJSON TuningJobCompletionCriteria where
  toJSON TuningJobCompletionCriteria' {..} =
    object
      ( catMaybes
          [ Just
              ("TargetObjectiveMetricValue" .= _tjccTargetObjectiveMetricValue)
          ]
      )
