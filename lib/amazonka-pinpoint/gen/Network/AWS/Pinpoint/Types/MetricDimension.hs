{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.MetricDimension
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.MetricDimension where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Specifies metric-based criteria for including or excluding endpoints from a segment. These criteria derive from custom metrics that you define for endpoints.
--
--
--
-- /See:/ 'metricDimension' smart constructor.
data MetricDimension = MetricDimension'
  { _mdComparisonOperator ::
      !Text,
    _mdValue :: !Double
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'MetricDimension' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mdComparisonOperator' - The operator to use when comparing metric values. Valid values are: GREATER_THAN, LESS_THAN, GREATER_THAN_OR_EQUAL, LESS_THAN_OR_EQUAL, and EQUAL.
--
-- * 'mdValue' - The value to compare.
metricDimension ::
  -- | 'mdComparisonOperator'
  Text ->
  -- | 'mdValue'
  Double ->
  MetricDimension
metricDimension pComparisonOperator_ pValue_ =
  MetricDimension'
    { _mdComparisonOperator = pComparisonOperator_,
      _mdValue = pValue_
    }

-- | The operator to use when comparing metric values. Valid values are: GREATER_THAN, LESS_THAN, GREATER_THAN_OR_EQUAL, LESS_THAN_OR_EQUAL, and EQUAL.
mdComparisonOperator :: Lens' MetricDimension Text
mdComparisonOperator = lens _mdComparisonOperator (\s a -> s {_mdComparisonOperator = a})

-- | The value to compare.
mdValue :: Lens' MetricDimension Double
mdValue = lens _mdValue (\s a -> s {_mdValue = a})

instance FromJSON MetricDimension where
  parseJSON =
    withObject
      "MetricDimension"
      ( \x ->
          MetricDimension'
            <$> (x .: "ComparisonOperator") <*> (x .: "Value")
      )

instance Hashable MetricDimension

instance NFData MetricDimension

instance ToJSON MetricDimension where
  toJSON MetricDimension' {..} =
    object
      ( catMaybes
          [ Just ("ComparisonOperator" .= _mdComparisonOperator),
            Just ("Value" .= _mdValue)
          ]
      )
