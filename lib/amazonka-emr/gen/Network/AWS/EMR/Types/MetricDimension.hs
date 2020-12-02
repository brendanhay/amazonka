{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.MetricDimension
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.MetricDimension where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | A CloudWatch dimension, which is specified using a @Key@ (known as a @Name@ in CloudWatch), @Value@ pair. By default, Amazon EMR uses one dimension whose @Key@ is @JobFlowID@ and @Value@ is a variable representing the cluster ID, which is @> {emr.clusterId}@ . This enables the rule to bootstrap when the cluster ID becomes available.
--
--
--
-- /See:/ 'metricDimension' smart constructor.
data MetricDimension = MetricDimension'
  { _mdValue :: !(Maybe Text),
    _mdKey :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'MetricDimension' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mdValue' - The dimension value.
--
-- * 'mdKey' - The dimension name.
metricDimension ::
  MetricDimension
metricDimension =
  MetricDimension' {_mdValue = Nothing, _mdKey = Nothing}

-- | The dimension value.
mdValue :: Lens' MetricDimension (Maybe Text)
mdValue = lens _mdValue (\s a -> s {_mdValue = a})

-- | The dimension name.
mdKey :: Lens' MetricDimension (Maybe Text)
mdKey = lens _mdKey (\s a -> s {_mdKey = a})

instance FromJSON MetricDimension where
  parseJSON =
    withObject
      "MetricDimension"
      (\x -> MetricDimension' <$> (x .:? "Value") <*> (x .:? "Key"))

instance Hashable MetricDimension

instance NFData MetricDimension

instance ToJSON MetricDimension where
  toJSON MetricDimension' {..} =
    object
      (catMaybes [("Value" .=) <$> _mdValue, ("Key" .=) <$> _mdKey])
