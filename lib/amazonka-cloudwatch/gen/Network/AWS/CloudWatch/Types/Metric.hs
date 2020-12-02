{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatch.Types.Metric
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatch.Types.Metric where

import Network.AWS.CloudWatch.Types.Dimension
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents a specific metric.
--
--
--
-- /See:/ 'metric' smart constructor.
data Metric = Metric'
  { _mMetricName :: !(Maybe Text),
    _mNamespace :: !(Maybe Text),
    _mDimensions :: !(Maybe [Dimension])
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Metric' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mMetricName' - The name of the metric. This is a required field.
--
-- * 'mNamespace' - The namespace of the metric.
--
-- * 'mDimensions' - The dimensions for the metric.
metric ::
  Metric
metric =
  Metric'
    { _mMetricName = Nothing,
      _mNamespace = Nothing,
      _mDimensions = Nothing
    }

-- | The name of the metric. This is a required field.
mMetricName :: Lens' Metric (Maybe Text)
mMetricName = lens _mMetricName (\s a -> s {_mMetricName = a})

-- | The namespace of the metric.
mNamespace :: Lens' Metric (Maybe Text)
mNamespace = lens _mNamespace (\s a -> s {_mNamespace = a})

-- | The dimensions for the metric.
mDimensions :: Lens' Metric [Dimension]
mDimensions = lens _mDimensions (\s a -> s {_mDimensions = a}) . _Default . _Coerce

instance FromXML Metric where
  parseXML x =
    Metric'
      <$> (x .@? "MetricName")
      <*> (x .@? "Namespace")
      <*> (x .@? "Dimensions" .!@ mempty >>= may (parseXMLList "member"))

instance Hashable Metric

instance NFData Metric

instance ToQuery Metric where
  toQuery Metric' {..} =
    mconcat
      [ "MetricName" =: _mMetricName,
        "Namespace" =: _mNamespace,
        "Dimensions" =: toQuery (toQueryList "member" <$> _mDimensions)
      ]
