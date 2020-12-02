{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatch.Types.MetricDataResult
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatch.Types.MetricDataResult where

import Network.AWS.CloudWatch.Types.MessageData
import Network.AWS.CloudWatch.Types.StatusCode
import Network.AWS.Lens
import Network.AWS.Prelude

-- | A @GetMetricData@ call returns an array of @MetricDataResult@ structures. Each of these structures includes the data points for that metric, along with the timestamps of those data points and other identifying information.
--
--
--
-- /See:/ 'metricDataResult' smart constructor.
data MetricDataResult = MetricDataResult'
  { _mdrValues ::
      !(Maybe [Double]),
    _mdrId :: !(Maybe Text),
    _mdrTimestamps :: !(Maybe [ISO8601]),
    _mdrMessages :: !(Maybe [MessageData]),
    _mdrLabel :: !(Maybe Text),
    _mdrStatusCode :: !(Maybe StatusCode)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'MetricDataResult' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mdrValues' - The data points for the metric corresponding to @Timestamps@ . The number of values always matches the number of timestamps and the timestamp for Values[x] is Timestamps[x].
--
-- * 'mdrId' - The short name you specified to represent this metric.
--
-- * 'mdrTimestamps' - The timestamps for the data points, formatted in Unix timestamp format. The number of timestamps always matches the number of values and the value for Timestamps[x] is Values[x].
--
-- * 'mdrMessages' - A list of messages with additional information about the data returned.
--
-- * 'mdrLabel' - The human-readable label associated with the data.
--
-- * 'mdrStatusCode' - The status of the returned data. @Complete@ indicates that all data points in the requested time range were returned. @PartialData@ means that an incomplete set of data points were returned. You can use the @NextToken@ value that was returned and repeat your request to get more data points. @NextToken@ is not returned if you are performing a math expression. @InternalError@ indicates that an error occurred. Retry your request using @NextToken@ , if present.
metricDataResult ::
  MetricDataResult
metricDataResult =
  MetricDataResult'
    { _mdrValues = Nothing,
      _mdrId = Nothing,
      _mdrTimestamps = Nothing,
      _mdrMessages = Nothing,
      _mdrLabel = Nothing,
      _mdrStatusCode = Nothing
    }

-- | The data points for the metric corresponding to @Timestamps@ . The number of values always matches the number of timestamps and the timestamp for Values[x] is Timestamps[x].
mdrValues :: Lens' MetricDataResult [Double]
mdrValues = lens _mdrValues (\s a -> s {_mdrValues = a}) . _Default . _Coerce

-- | The short name you specified to represent this metric.
mdrId :: Lens' MetricDataResult (Maybe Text)
mdrId = lens _mdrId (\s a -> s {_mdrId = a})

-- | The timestamps for the data points, formatted in Unix timestamp format. The number of timestamps always matches the number of values and the value for Timestamps[x] is Values[x].
mdrTimestamps :: Lens' MetricDataResult [UTCTime]
mdrTimestamps = lens _mdrTimestamps (\s a -> s {_mdrTimestamps = a}) . _Default . _Coerce

-- | A list of messages with additional information about the data returned.
mdrMessages :: Lens' MetricDataResult [MessageData]
mdrMessages = lens _mdrMessages (\s a -> s {_mdrMessages = a}) . _Default . _Coerce

-- | The human-readable label associated with the data.
mdrLabel :: Lens' MetricDataResult (Maybe Text)
mdrLabel = lens _mdrLabel (\s a -> s {_mdrLabel = a})

-- | The status of the returned data. @Complete@ indicates that all data points in the requested time range were returned. @PartialData@ means that an incomplete set of data points were returned. You can use the @NextToken@ value that was returned and repeat your request to get more data points. @NextToken@ is not returned if you are performing a math expression. @InternalError@ indicates that an error occurred. Retry your request using @NextToken@ , if present.
mdrStatusCode :: Lens' MetricDataResult (Maybe StatusCode)
mdrStatusCode = lens _mdrStatusCode (\s a -> s {_mdrStatusCode = a})

instance FromXML MetricDataResult where
  parseXML x =
    MetricDataResult'
      <$> (x .@? "Values" .!@ mempty >>= may (parseXMLList "member"))
      <*> (x .@? "Id")
      <*> (x .@? "Timestamps" .!@ mempty >>= may (parseXMLList "member"))
      <*> (x .@? "Messages" .!@ mempty >>= may (parseXMLList "member"))
      <*> (x .@? "Label")
      <*> (x .@? "StatusCode")

instance Hashable MetricDataResult

instance NFData MetricDataResult
