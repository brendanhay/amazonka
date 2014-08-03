{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.CloudWatch.V2010_08_01.DescribeAlarmsForMetric
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Retrieves all alarms for a single metric. Specify a statistic, period, or
-- unit to filter the set of alarms further.
module Network.AWS.CloudWatch.V2010_08_01.DescribeAlarmsForMetric where

import Control.Lens
import Network.AWS.Request.Query
import Network.AWS.CloudWatch.V2010_08_01.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'DescribeAlarmsForMetric' request.
describeAlarmsForMetric :: Text -- ^ '_dafmiMetricName'
                        -> Text -- ^ '_dafmiNamespace'
                        -> DescribeAlarmsForMetric
describeAlarmsForMetric p1 p2 = DescribeAlarmsForMetric
    { _dafmiMetricName = p1
    , _dafmiNamespace = p2
    , _dafmiDimensions = mempty
    , _dafmiPeriod = Nothing
    , _dafmiUnit = Nothing
    , _dafmiStatistic = Nothing
    }

data DescribeAlarmsForMetric = DescribeAlarmsForMetric
    { _dafmiMetricName :: Text
      -- ^ The name of the metric.
    , _dafmiNamespace :: Text
      -- ^ The namespace of the metric.
    , _dafmiDimensions :: [Dimension]
      -- ^ The list of dimensions associated with the metric.
    , _dafmiPeriod :: Maybe Integer
      -- ^ The period in seconds over which the statistic is applied.
    , _dafmiUnit :: Maybe StandardUnit
      -- ^ The unit for the metric.
    , _dafmiStatistic :: Maybe Statistic
      -- ^ The statistic for the metric.
    } deriving (Generic)

makeLenses ''DescribeAlarmsForMetric

instance ToQuery DescribeAlarmsForMetric where
    toQuery = genericToQuery def

data DescribeAlarmsForMetricResponse = DescribeAlarmsForMetricResponse
    { _dafmoMetricAlarms :: [MetricAlarm]
      -- ^ A list of information for each alarm with the specified metric.
    } deriving (Generic)

makeLenses ''DescribeAlarmsForMetricResponse

instance FromXML DescribeAlarmsForMetricResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeAlarmsForMetric where
    type Sv DescribeAlarmsForMetric = CloudWatch
    type Rs DescribeAlarmsForMetric = DescribeAlarmsForMetricResponse

    request = post "DescribeAlarmsForMetric"
    response _ = xmlResponse
