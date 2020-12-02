{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MachineLearning.Types.PerformanceMetrics
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MachineLearning.Types.PerformanceMetrics where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Measurements of how well the @MLModel@ performed on known observations. One of the following metrics is returned, based on the type of the @MLModel@ :
--
--
--     * BinaryAUC: The binary @MLModel@ uses the Area Under the Curve (AUC) technique to measure performance.
--
--     * RegressionRMSE: The regression @MLModel@ uses the Root Mean Square Error (RMSE) technique to measure performance. RMSE measures the difference between predicted and actual values for a single variable.
--
--     * MulticlassAvgFScore: The multiclass @MLModel@ uses the F1 score technique to measure performance.
--
--
--
-- For more information about performance metrics, please see the <http://docs.aws.amazon.com/machine-learning/latest/dg Amazon Machine Learning Developer Guide> .
--
--
-- /See:/ 'performanceMetrics' smart constructor.
newtype PerformanceMetrics = PerformanceMetrics'
  { _pmProperties ::
      Maybe (Map Text (Text))
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PerformanceMetrics' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pmProperties' - Undocumented member.
performanceMetrics ::
  PerformanceMetrics
performanceMetrics = PerformanceMetrics' {_pmProperties = Nothing}

-- | Undocumented member.
pmProperties :: Lens' PerformanceMetrics (HashMap Text (Text))
pmProperties = lens _pmProperties (\s a -> s {_pmProperties = a}) . _Default . _Map

instance FromJSON PerformanceMetrics where
  parseJSON =
    withObject
      "PerformanceMetrics"
      (\x -> PerformanceMetrics' <$> (x .:? "Properties" .!= mempty))

instance Hashable PerformanceMetrics

instance NFData PerformanceMetrics
