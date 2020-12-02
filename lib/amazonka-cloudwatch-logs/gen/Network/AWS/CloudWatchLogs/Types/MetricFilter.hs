{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchLogs.Types.MetricFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatchLogs.Types.MetricFilter where

import Network.AWS.CloudWatchLogs.Types.MetricTransformation
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Metric filters express how CloudWatch Logs would extract metric observations from ingested log events and transform them into metric data in a CloudWatch metric.
--
--
--
-- /See:/ 'metricFilter' smart constructor.
data MetricFilter = MetricFilter'
  { _mfCreationTime :: !(Maybe Nat),
    _mfFilterName :: !(Maybe Text),
    _mfLogGroupName :: !(Maybe Text),
    _mfFilterPattern :: !(Maybe Text),
    _mfMetricTransformations :: !(Maybe (List1 MetricTransformation))
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'MetricFilter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mfCreationTime' - The creation time of the metric filter, expressed as the number of milliseconds after Jan 1, 1970 00:00:00 UTC.
--
-- * 'mfFilterName' - The name of the metric filter.
--
-- * 'mfLogGroupName' - The name of the log group.
--
-- * 'mfFilterPattern' - Undocumented member.
--
-- * 'mfMetricTransformations' - The metric transformations.
metricFilter ::
  MetricFilter
metricFilter =
  MetricFilter'
    { _mfCreationTime = Nothing,
      _mfFilterName = Nothing,
      _mfLogGroupName = Nothing,
      _mfFilterPattern = Nothing,
      _mfMetricTransformations = Nothing
    }

-- | The creation time of the metric filter, expressed as the number of milliseconds after Jan 1, 1970 00:00:00 UTC.
mfCreationTime :: Lens' MetricFilter (Maybe Natural)
mfCreationTime = lens _mfCreationTime (\s a -> s {_mfCreationTime = a}) . mapping _Nat

-- | The name of the metric filter.
mfFilterName :: Lens' MetricFilter (Maybe Text)
mfFilterName = lens _mfFilterName (\s a -> s {_mfFilterName = a})

-- | The name of the log group.
mfLogGroupName :: Lens' MetricFilter (Maybe Text)
mfLogGroupName = lens _mfLogGroupName (\s a -> s {_mfLogGroupName = a})

-- | Undocumented member.
mfFilterPattern :: Lens' MetricFilter (Maybe Text)
mfFilterPattern = lens _mfFilterPattern (\s a -> s {_mfFilterPattern = a})

-- | The metric transformations.
mfMetricTransformations :: Lens' MetricFilter (Maybe (NonEmpty MetricTransformation))
mfMetricTransformations = lens _mfMetricTransformations (\s a -> s {_mfMetricTransformations = a}) . mapping _List1

instance FromJSON MetricFilter where
  parseJSON =
    withObject
      "MetricFilter"
      ( \x ->
          MetricFilter'
            <$> (x .:? "creationTime")
            <*> (x .:? "filterName")
            <*> (x .:? "logGroupName")
            <*> (x .:? "filterPattern")
            <*> (x .:? "metricTransformations")
      )

instance Hashable MetricFilter

instance NFData MetricFilter
