{-# LANGUAGE TemplateHaskell             #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudWatch.V2010_08_01.Lenses
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.CloudWatch.V2010_08_01.Lenses where

import Control.Lens.TH
import Network.AWS.CloudWatch.V2010_08_01.Types
import Network.AWS.CloudWatch.V2010_08_01.EnableAlarmActions
import Network.AWS.CloudWatch.V2010_08_01.PutMetricData
import Network.AWS.CloudWatch.V2010_08_01.DescribeAlarms
import Network.AWS.CloudWatch.V2010_08_01.ListMetrics
import Network.AWS.CloudWatch.V2010_08_01.DeleteAlarms
import Network.AWS.CloudWatch.V2010_08_01.DescribeAlarmHistory
import Network.AWS.CloudWatch.V2010_08_01.GetMetricStatistics
import Network.AWS.CloudWatch.V2010_08_01.DescribeAlarmsForMetric
import Network.AWS.CloudWatch.V2010_08_01.DisableAlarmActions
import Network.AWS.CloudWatch.V2010_08_01.PutMetricAlarm
import Network.AWS.CloudWatch.V2010_08_01.SetAlarmState

-- Newtypes

-- Products
makeLenses ''AlarmHistoryItem
makeLenses ''Datapoint
makeLenses ''Dimension
makeLenses ''DimensionFilter
makeLenses ''Metric
makeLenses ''MetricAlarm
makeLenses ''MetricDatum
makeLenses ''StatisticSet

-- Requests
makeLenses ''EnableAlarmActions
makeLenses ''PutMetricData
makeLenses ''DescribeAlarms
makeLenses ''ListMetrics
makeLenses ''DeleteAlarms
makeLenses ''DescribeAlarmHistory
makeLenses ''GetMetricStatistics
makeLenses ''DescribeAlarmsForMetric
makeLenses ''DisableAlarmActions
makeLenses ''PutMetricAlarm
makeLenses ''SetAlarmState

-- Responses
makeLenses ''EnableAlarmActionsResponse
makeLenses ''PutMetricDataResponse
makeLenses ''DescribeAlarmsResponse
makeLenses ''ListMetricsResponse
makeLenses ''DeleteAlarmsResponse
makeLenses ''DescribeAlarmHistoryResponse
makeLenses ''GetMetricStatisticsResponse
makeLenses ''DescribeAlarmsForMetricResponse
makeLenses ''DisableAlarmActionsResponse
makeLenses ''PutMetricAlarmResponse
makeLenses ''SetAlarmStateResponse
