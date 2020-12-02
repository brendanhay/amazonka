{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatch.Types.MetricDataQuery
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatch.Types.MetricDataQuery where

import Network.AWS.CloudWatch.Types.MetricStat
import Network.AWS.Lens
import Network.AWS.Prelude

-- | This structure is used in both @GetMetricData@ and @PutMetricAlarm@ . The supported use of this structure is different for those two operations.
--
--
-- When used in @GetMetricData@ , it indicates the metric data to return, and whether this call is just retrieving a batch set of data for one metric, or is performing a math expression on metric data. A single @GetMetricData@ call can include up to 500 @MetricDataQuery@ structures.
--
-- When used in @PutMetricAlarm@ , it enables you to create an alarm based on a metric math expression. Each @MetricDataQuery@ in the array specifies either a metric to retrieve, or a math expression to be performed on retrieved metrics. A single @PutMetricAlarm@ call can include up to 20 @MetricDataQuery@ structures in the array. The 20 structures can include as many as 10 structures that contain a @MetricStat@ parameter to retrieve a metric, and as many as 10 structures that contain the @Expression@ parameter to perform a math expression. Of those @Expression@ structures, one must have @True@ as the value for @ReturnData@ . The result of this expression is the value the alarm watches.
--
-- Any expression used in a @PutMetricAlarm@ operation must return a single time series. For more information, see <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/using-metric-math.html#metric-math-syntax Metric Math Syntax and Functions> in the /Amazon CloudWatch User Guide/ .
--
-- Some of the parameters of this structure also have different uses whether you are using this structure in a @GetMetricData@ operation or a @PutMetricAlarm@ operation. These differences are explained in the following parameter list.
--
--
-- /See:/ 'metricDataQuery' smart constructor.
data MetricDataQuery = MetricDataQuery'
  { _mdqReturnData ::
      !(Maybe Bool),
    _mdqPeriod :: !(Maybe Nat),
    _mdqExpression :: !(Maybe Text),
    _mdqLabel :: !(Maybe Text),
    _mdqMetricStat :: !(Maybe MetricStat),
    _mdqId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'MetricDataQuery' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mdqReturnData' - When used in @GetMetricData@ , this option indicates whether to return the timestamps and raw data values of this metric. If you are performing this call just to do math expressions and do not also need the raw data returned, you can specify @False@ . If you omit this, the default of @True@ is used. When used in @PutMetricAlarm@ , specify @True@ for the one expression result to use as the alarm. For all other metrics and expressions in the same @PutMetricAlarm@ operation, specify @ReturnData@ as False.
--
-- * 'mdqPeriod' - The granularity, in seconds, of the returned data points. For metrics with regular resolution, a period can be as short as one minute (60 seconds) and must be a multiple of 60. For high-resolution metrics that are collected at intervals of less than one minute, the period can be 1, 5, 10, 30, 60, or any multiple of 60. High-resolution metrics are those metrics stored by a @PutMetricData@ operation that includes a @StorageResolution of 1 second@ .
--
-- * 'mdqExpression' - The math expression to be performed on the returned data, if this object is performing a math expression. This expression can use the @Id@ of the other metrics to refer to those metrics, and can also use the @Id@ of other expressions to use the result of those expressions. For more information about metric math expressions, see <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/using-metric-math.html#metric-math-syntax Metric Math Syntax and Functions> in the /Amazon CloudWatch User Guide/ . Within each MetricDataQuery object, you must specify either @Expression@ or @MetricStat@ but not both.
--
-- * 'mdqLabel' - A human-readable label for this metric or expression. This is especially useful if this is an expression, so that you know what the value represents. If the metric or expression is shown in a CloudWatch dashboard widget, the label is shown. If Label is omitted, CloudWatch generates a default.
--
-- * 'mdqMetricStat' - The metric to be returned, along with statistics, period, and units. Use this parameter only if this object is retrieving a metric and not performing a math expression on returned data. Within one MetricDataQuery object, you must specify either @Expression@ or @MetricStat@ but not both.
--
-- * 'mdqId' - A short name used to tie this object to the results in the response. This name must be unique within a single call to @GetMetricData@ . If you are performing math expressions on this set of data, this name represents that data and can serve as a variable in the mathematical expression. The valid characters are letters, numbers, and underscore. The first character must be a lowercase letter.
metricDataQuery ::
  -- | 'mdqId'
  Text ->
  MetricDataQuery
metricDataQuery pId_ =
  MetricDataQuery'
    { _mdqReturnData = Nothing,
      _mdqPeriod = Nothing,
      _mdqExpression = Nothing,
      _mdqLabel = Nothing,
      _mdqMetricStat = Nothing,
      _mdqId = pId_
    }

-- | When used in @GetMetricData@ , this option indicates whether to return the timestamps and raw data values of this metric. If you are performing this call just to do math expressions and do not also need the raw data returned, you can specify @False@ . If you omit this, the default of @True@ is used. When used in @PutMetricAlarm@ , specify @True@ for the one expression result to use as the alarm. For all other metrics and expressions in the same @PutMetricAlarm@ operation, specify @ReturnData@ as False.
mdqReturnData :: Lens' MetricDataQuery (Maybe Bool)
mdqReturnData = lens _mdqReturnData (\s a -> s {_mdqReturnData = a})

-- | The granularity, in seconds, of the returned data points. For metrics with regular resolution, a period can be as short as one minute (60 seconds) and must be a multiple of 60. For high-resolution metrics that are collected at intervals of less than one minute, the period can be 1, 5, 10, 30, 60, or any multiple of 60. High-resolution metrics are those metrics stored by a @PutMetricData@ operation that includes a @StorageResolution of 1 second@ .
mdqPeriod :: Lens' MetricDataQuery (Maybe Natural)
mdqPeriod = lens _mdqPeriod (\s a -> s {_mdqPeriod = a}) . mapping _Nat

-- | The math expression to be performed on the returned data, if this object is performing a math expression. This expression can use the @Id@ of the other metrics to refer to those metrics, and can also use the @Id@ of other expressions to use the result of those expressions. For more information about metric math expressions, see <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/using-metric-math.html#metric-math-syntax Metric Math Syntax and Functions> in the /Amazon CloudWatch User Guide/ . Within each MetricDataQuery object, you must specify either @Expression@ or @MetricStat@ but not both.
mdqExpression :: Lens' MetricDataQuery (Maybe Text)
mdqExpression = lens _mdqExpression (\s a -> s {_mdqExpression = a})

-- | A human-readable label for this metric or expression. This is especially useful if this is an expression, so that you know what the value represents. If the metric or expression is shown in a CloudWatch dashboard widget, the label is shown. If Label is omitted, CloudWatch generates a default.
mdqLabel :: Lens' MetricDataQuery (Maybe Text)
mdqLabel = lens _mdqLabel (\s a -> s {_mdqLabel = a})

-- | The metric to be returned, along with statistics, period, and units. Use this parameter only if this object is retrieving a metric and not performing a math expression on returned data. Within one MetricDataQuery object, you must specify either @Expression@ or @MetricStat@ but not both.
mdqMetricStat :: Lens' MetricDataQuery (Maybe MetricStat)
mdqMetricStat = lens _mdqMetricStat (\s a -> s {_mdqMetricStat = a})

-- | A short name used to tie this object to the results in the response. This name must be unique within a single call to @GetMetricData@ . If you are performing math expressions on this set of data, this name represents that data and can serve as a variable in the mathematical expression. The valid characters are letters, numbers, and underscore. The first character must be a lowercase letter.
mdqId :: Lens' MetricDataQuery Text
mdqId = lens _mdqId (\s a -> s {_mdqId = a})

instance FromXML MetricDataQuery where
  parseXML x =
    MetricDataQuery'
      <$> (x .@? "ReturnData")
      <*> (x .@? "Period")
      <*> (x .@? "Expression")
      <*> (x .@? "Label")
      <*> (x .@? "MetricStat")
      <*> (x .@ "Id")

instance Hashable MetricDataQuery

instance NFData MetricDataQuery

instance ToQuery MetricDataQuery where
  toQuery MetricDataQuery' {..} =
    mconcat
      [ "ReturnData" =: _mdqReturnData,
        "Period" =: _mdqPeriod,
        "Expression" =: _mdqExpression,
        "Label" =: _mdqLabel,
        "MetricStat" =: _mdqMetricStat,
        "Id" =: _mdqId
      ]
