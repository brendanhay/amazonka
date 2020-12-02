{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.MetricDimension
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.MetricDimension where

import Network.AWS.IoT.Types.DimensionValueOperator
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The dimension of a metric.
--
--
--
-- /See:/ 'metricDimension' smart constructor.
data MetricDimension = MetricDimension'
  { _mdOperator ::
      !(Maybe DimensionValueOperator),
    _mdDimensionName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'MetricDimension' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mdOperator' - Defines how the @dimensionValues@ of a dimension are interpreted. For example, for dimension type TOPIC_FILTER, the @IN@ operator, a message will be counted only if its topic matches one of the topic filters. With @NOT_IN@ operator, a message will be counted only if it doesn't match any of the topic filters. The operator is optional: if it's not provided (is @null@ ), it will be interpreted as @IN@ .
--
-- * 'mdDimensionName' - A unique identifier for the dimension.
metricDimension ::
  -- | 'mdDimensionName'
  Text ->
  MetricDimension
metricDimension pDimensionName_ =
  MetricDimension'
    { _mdOperator = Nothing,
      _mdDimensionName = pDimensionName_
    }

-- | Defines how the @dimensionValues@ of a dimension are interpreted. For example, for dimension type TOPIC_FILTER, the @IN@ operator, a message will be counted only if its topic matches one of the topic filters. With @NOT_IN@ operator, a message will be counted only if it doesn't match any of the topic filters. The operator is optional: if it's not provided (is @null@ ), it will be interpreted as @IN@ .
mdOperator :: Lens' MetricDimension (Maybe DimensionValueOperator)
mdOperator = lens _mdOperator (\s a -> s {_mdOperator = a})

-- | A unique identifier for the dimension.
mdDimensionName :: Lens' MetricDimension Text
mdDimensionName = lens _mdDimensionName (\s a -> s {_mdDimensionName = a})

instance FromJSON MetricDimension where
  parseJSON =
    withObject
      "MetricDimension"
      ( \x ->
          MetricDimension' <$> (x .:? "operator") <*> (x .: "dimensionName")
      )

instance Hashable MetricDimension

instance NFData MetricDimension

instance ToJSON MetricDimension where
  toJSON MetricDimension' {..} =
    object
      ( catMaybes
          [ ("operator" .=) <$> _mdOperator,
            Just ("dimensionName" .= _mdDimensionName)
          ]
      )
