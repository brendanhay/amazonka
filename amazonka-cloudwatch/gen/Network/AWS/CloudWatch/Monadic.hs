{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.CloudWatch.Monadic
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | This module is provided for convenience. It offers an alternative to the
-- common idiom of supplying required fields to an operations's smart constructor,
-- using the operation's lenses to modify additional fields, and then sending
-- or paginating the request.
--
-- As an example: using "Network.AWS.CloudWatch" with the smart constructor and
-- basic lens syntax, before explicitly calling 'send':
--
-- @
-- import Control.Monad.Trans.AWS
-- import Network.AWS.CloudWatch
--
-- send $ (mkOperationName w x)
--      & onLensField1 .~ y
--      & onLensField2 .~ z
-- @
--
-- Versus using "Network.AWS.CloudWatch.Monadic" with the 'State' operator variants from
-- "Control.Lens.Setter" such as '.=' to modify any additional request
-- parameters before sending:
--
-- @
-- import Control.Applicative
-- import Network.AWS.CloudWatch.Monadic
--
-- operationName w x $ do
--     onLensField1 .= y
--     onLensField2 .= z
--
-- -- Or to void any additional parameters outside of those required using 'Control.Applicative.empty':
-- operationName w x empty
-- @
--
module Network.AWS.CloudWatch.Monadic
    (
    -- * DeleteAlarms
    -- $DeleteAlarms
      deleteAlarms
    , deleteAlarmsCatch

    -- * DescribeAlarmHistory
    -- $DescribeAlarmHistory
    , describeAlarmHistory
    , describeAlarmHistoryCatch

    -- * DescribeAlarms
    -- $DescribeAlarms
    , describeAlarms
    , describeAlarmsCatch

    -- * DescribeAlarmsForMetric
    -- $DescribeAlarmsForMetric
    , describeAlarmsForMetric
    , describeAlarmsForMetricCatch

    -- * DisableAlarmActions
    -- $DisableAlarmActions
    , disableAlarmActions
    , disableAlarmActionsCatch

    -- * EnableAlarmActions
    -- $EnableAlarmActions
    , enableAlarmActions
    , enableAlarmActionsCatch

    -- * GetMetricStatistics
    -- $GetMetricStatistics
    , getMetricStatistics
    , getMetricStatisticsCatch

    -- * ListMetrics
    -- $ListMetrics
    , listMetrics
    , listMetricsCatch

    -- * PutMetricAlarm
    -- $PutMetricAlarm
    , putMetricAlarm
    , putMetricAlarmCatch

    -- * PutMetricData
    -- $PutMetricData
    , putMetricData
    , putMetricDataCatch

    -- * SetAlarmState
    -- $SetAlarmState
    , setAlarmState
    , setAlarmStateCatch

    -- * Re-exported
    , module Network.AWS.CloudWatch

    , (.=)
    , (?=)
    , (<>=)
    , (%=)
    ) where

import Control.Monad.Trans.AWS as AWS
import Network.AWS.Prelude
import Network.AWS.CloudWatch

type ServiceEr = Er CloudWatch

-- $DeleteAlarms
-- Deletes all specified alarms. In the event of an error, no alarms are
-- deleted.
--
-- See: 'Network.AWS.CloudWatch.DeleteAlarms'

deleteAlarms :: ( MonadCatch m
                , MonadResource m
                , MonadError AWS.Error m
                , MonadReader Env m
                )
    => [Text] -- ^ 'daAlarmNames'
    -> m DeleteAlarmsResponse
deleteAlarms p1 =
    send (mkDeleteAlarms p1)

deleteAlarmsCatch :: ( MonadCatch m
                     , MonadResource m
                     , MonadReader Env m
                     )
    => [Text] -- ^ 'daAlarmNames'
    -> m (Either ServiceEr DeleteAlarmsResponse)
deleteAlarmsCatch p1 =
    sendCatch (mkDeleteAlarms p1)

-- $DescribeAlarmHistory
-- Retrieves history for the specified alarm. Filter alarms by date range or
-- item type. If an alarm name is not specified, Amazon CloudWatch returns
-- histories for all of the owner's alarms. Amazon CloudWatch retains the
-- history of an alarm for two weeks, whether or not you delete the alarm.
--
-- See: 'Network.AWS.CloudWatch.DescribeAlarmHistory'

describeAlarmHistory :: ( MonadCatch m
                        , MonadResource m
                        , MonadError AWS.Error m
                        , MonadReader Env m
                        )
    => State DescribeAlarmHistory a
    -> Source m DescribeAlarmHistoryResponse
describeAlarmHistory s =
    paginate (mkDescribeAlarmHistory &~ s)

describeAlarmHistoryCatch :: ( MonadCatch m
                             , MonadResource m
                             , MonadReader Env m
                             )
    => State DescribeAlarmHistory a
    -> Source m (Either ServiceEr DescribeAlarmHistoryResponse)
describeAlarmHistoryCatch s =
    paginateCatch (mkDescribeAlarmHistory &~ s)

-- $DescribeAlarms
-- Retrieves alarms with the specified names. If no name is specified, all
-- alarms for the user are returned. Alarms can be retrieved by using only a
-- prefix for the alarm name, the alarm state, or a prefix for any action.
--
-- See: 'Network.AWS.CloudWatch.DescribeAlarms'

describeAlarms :: ( MonadCatch m
                  , MonadResource m
                  , MonadError AWS.Error m
                  , MonadReader Env m
                  )
    => State DescribeAlarms a
    -> Source m DescribeAlarmsResponse
describeAlarms s =
    paginate (mkDescribeAlarms &~ s)

describeAlarmsCatch :: ( MonadCatch m
                       , MonadResource m
                       , MonadReader Env m
                       )
    => State DescribeAlarms a
    -> Source m (Either ServiceEr DescribeAlarmsResponse)
describeAlarmsCatch s =
    paginateCatch (mkDescribeAlarms &~ s)

-- $DescribeAlarmsForMetric
-- Retrieves all alarms for a single metric. Specify a statistic, period, or
-- unit to filter the set of alarms further.
--
-- See: 'Network.AWS.CloudWatch.DescribeAlarmsForMetric'

describeAlarmsForMetric :: ( MonadCatch m
                           , MonadResource m
                           , MonadError AWS.Error m
                           , MonadReader Env m
                           )
    => Text -- ^ 'dafmMetricName'
    -> Text -- ^ 'dafmNamespace'
    -> State DescribeAlarmsForMetric a
    -> m DescribeAlarmsForMetricResponse
describeAlarmsForMetric p1 p2 s =
    send $ (mkDescribeAlarmsForMetric p1 p2) &~ s

describeAlarmsForMetricCatch :: ( MonadCatch m
                                , MonadResource m
                                , MonadReader Env m
                                )
    => Text -- ^ 'dafmMetricName'
    -> Text -- ^ 'dafmNamespace'
    -> State DescribeAlarmsForMetric a
    -> m (Either ServiceEr DescribeAlarmsForMetricResponse)
describeAlarmsForMetricCatch p1 p2 s =
    sendCatch $ (mkDescribeAlarmsForMetric p1 p2) &~ s

-- $DisableAlarmActions
-- Disables actions for the specified alarms. When an alarm's actions are
-- disabled the alarm's state may change, but none of the alarm's actions will
-- execute.
--
-- See: 'Network.AWS.CloudWatch.DisableAlarmActions'

disableAlarmActions :: ( MonadCatch m
                       , MonadResource m
                       , MonadError AWS.Error m
                       , MonadReader Env m
                       )
    => [Text] -- ^ 'daaAlarmNames'
    -> m DisableAlarmActionsResponse
disableAlarmActions p1 =
    send (mkDisableAlarmActions p1)

disableAlarmActionsCatch :: ( MonadCatch m
                            , MonadResource m
                            , MonadReader Env m
                            )
    => [Text] -- ^ 'daaAlarmNames'
    -> m (Either ServiceEr DisableAlarmActionsResponse)
disableAlarmActionsCatch p1 =
    sendCatch (mkDisableAlarmActions p1)

-- $EnableAlarmActions
-- Enables actions for the specified alarms.
--
-- See: 'Network.AWS.CloudWatch.EnableAlarmActions'

enableAlarmActions :: ( MonadCatch m
                      , MonadResource m
                      , MonadError AWS.Error m
                      , MonadReader Env m
                      )
    => [Text] -- ^ 'eaaAlarmNames'
    -> m EnableAlarmActionsResponse
enableAlarmActions p1 =
    send (mkEnableAlarmActions p1)

enableAlarmActionsCatch :: ( MonadCatch m
                           , MonadResource m
                           , MonadReader Env m
                           )
    => [Text] -- ^ 'eaaAlarmNames'
    -> m (Either ServiceEr EnableAlarmActionsResponse)
enableAlarmActionsCatch p1 =
    sendCatch (mkEnableAlarmActions p1)

-- $GetMetricStatistics
-- Gets statistics for the specified metric. The maximum number of data points
-- returned from a single GetMetricStatistics request is 1,440. If a request
-- is made that generates more than 1,440 data points, Amazon CloudWatch
-- returns an error. In such a case, alter the request by narrowing the
-- specified time range or increasing the specified period. Alternatively,
-- make multiple requests across adjacent time ranges. Amazon CloudWatch
-- aggregates data points based on the length of the period that you specify.
-- For example, if you request statistics with a one-minute granularity,
-- Amazon CloudWatch aggregates data points with time stamps that fall within
-- the same one-minute period. In such a case, the data points queried can
-- greatly outnumber the data points returned. The maximum number of data
-- points that can be queried is 50,850; whereas the maximum number of data
-- points returned is 1,440. The following examples show various statistics
-- allowed by the data point query maximum of 50,850 when you call
-- GetMetricStatistics on Amazon EC2 instances with detailed (one-minute)
-- monitoring enabled: Statistics for up to 400 instances for a span of one
-- hour Statistics for up to 35 instances over a span of 24 hours Statistics
-- for up to 2 instances over a span of 2 weeks.
--
-- See: 'Network.AWS.CloudWatch.GetMetricStatistics'

getMetricStatistics :: ( MonadCatch m
                       , MonadResource m
                       , MonadError AWS.Error m
                       , MonadReader Env m
                       )
    => Text -- ^ 'gmsNamespace'
    -> Text -- ^ 'gmsMetricName'
    -> ISO8601 -- ^ 'gmsStartTime'
    -> ISO8601 -- ^ 'gmsEndTime'
    -> Integer -- ^ 'gmsPeriod'
    -> List1 Statistic -- ^ 'gmsStatistics'
    -> State GetMetricStatistics a
    -> m GetMetricStatisticsResponse
getMetricStatistics p1 p2 p4 p5 p6 p7 s =
    send $ (mkGetMetricStatistics p1 p2 p4 p5 p6 p7) &~ s

getMetricStatisticsCatch :: ( MonadCatch m
                            , MonadResource m
                            , MonadReader Env m
                            )
    => Text -- ^ 'gmsNamespace'
    -> Text -- ^ 'gmsMetricName'
    -> ISO8601 -- ^ 'gmsStartTime'
    -> ISO8601 -- ^ 'gmsEndTime'
    -> Integer -- ^ 'gmsPeriod'
    -> List1 Statistic -- ^ 'gmsStatistics'
    -> State GetMetricStatistics a
    -> m (Either ServiceEr GetMetricStatisticsResponse)
getMetricStatisticsCatch p1 p2 p4 p5 p6 p7 s =
    sendCatch $ (mkGetMetricStatistics p1 p2 p4 p5 p6 p7) &~ s

-- $ListMetrics
-- Returns a list of valid metrics stored for the AWS account owner. Returned
-- metrics can be used with GetMetricStatistics to obtain statistical data for
-- a given metric. Up to 500 results are returned for any one call. To
-- retrieve further results, use returned NextToken values with subsequent
-- ListMetrics operations. If you create a metric with the PutMetricData
-- action, allow up to fifteen minutes for the metric to appear in calls to
-- the ListMetrics action.
--
-- See: 'Network.AWS.CloudWatch.ListMetrics'

listMetrics :: ( MonadCatch m
               , MonadResource m
               , MonadError AWS.Error m
               , MonadReader Env m
               )
    => State ListMetrics a
    -> Source m ListMetricsResponse
listMetrics s =
    paginate (mkListMetrics &~ s)

listMetricsCatch :: ( MonadCatch m
                    , MonadResource m
                    , MonadReader Env m
                    )
    => State ListMetrics a
    -> Source m (Either ServiceEr ListMetricsResponse)
listMetricsCatch s =
    paginateCatch (mkListMetrics &~ s)

-- $PutMetricAlarm
-- Creates or updates an alarm and associates it with the specified Amazon
-- CloudWatch metric. Optionally, this operation can associate one or more
-- Amazon Simple Notification Service resources with the alarm. When this
-- operation creates an alarm, the alarm state is immediately set to
-- INSUFFICIENT_DATA. The alarm is evaluated and its StateValue is set
-- appropriately. Any actions associated with the StateValue is then executed.
-- When updating an existing alarm, its StateValue is left unchanged.
--
-- See: 'Network.AWS.CloudWatch.PutMetricAlarm'

putMetricAlarm :: ( MonadCatch m
                  , MonadResource m
                  , MonadError AWS.Error m
                  , MonadReader Env m
                  )
    => Text -- ^ 'pmaAlarmName'
    -> Integer -- ^ 'pmaPeriod'
    -> Integer -- ^ 'pmaEvaluationPeriods'
    -> Double -- ^ 'pmaThreshold'
    -> ComparisonOperator -- ^ 'pmaComparisonOperator'
    -> Text -- ^ 'pmaMetricName'
    -> Text -- ^ 'pmaNamespace'
    -> Statistic -- ^ 'pmaStatistic'
    -> State PutMetricAlarm a
    -> m PutMetricAlarmResponse
putMetricAlarm p1 p11 p13 p14 p15 p7 p8 p9 s =
    send $ (mkPutMetricAlarm p1 p11 p13 p14 p15 p7 p8 p9) &~ s

putMetricAlarmCatch :: ( MonadCatch m
                       , MonadResource m
                       , MonadReader Env m
                       )
    => Text -- ^ 'pmaAlarmName'
    -> Integer -- ^ 'pmaPeriod'
    -> Integer -- ^ 'pmaEvaluationPeriods'
    -> Double -- ^ 'pmaThreshold'
    -> ComparisonOperator -- ^ 'pmaComparisonOperator'
    -> Text -- ^ 'pmaMetricName'
    -> Text -- ^ 'pmaNamespace'
    -> Statistic -- ^ 'pmaStatistic'
    -> State PutMetricAlarm a
    -> m (Either ServiceEr PutMetricAlarmResponse)
putMetricAlarmCatch p1 p11 p13 p14 p15 p7 p8 p9 s =
    sendCatch $ (mkPutMetricAlarm p1 p11 p13 p14 p15 p7 p8 p9) &~ s

-- $PutMetricData
-- Publishes metric data points to Amazon CloudWatch. Amazon Cloudwatch
-- associates the data points with the specified metric. If the specified
-- metric does not exist, Amazon CloudWatch creates the metric. If you create
-- a metric with the PutMetricData action, allow up to fifteen minutes for the
-- metric to appear in calls to the ListMetrics action. The size of a
-- PutMetricData request is limited to 8 KB for HTTP GET requests and 40 KB
-- for HTTP POST requests. Although the Value parameter accepts numbers of
-- type Double, Amazon CloudWatch truncates values with very large exponents.
-- Values with base-10 exponents greater than 126 (1 x 10^126) are truncated.
-- Likewise, values with base-10 exponents less than -130 (1 x 10^-130) are
-- also truncated.
--
-- See: 'Network.AWS.CloudWatch.PutMetricData'

putMetricData :: ( MonadCatch m
                 , MonadResource m
                 , MonadError AWS.Error m
                 , MonadReader Env m
                 )
    => Text -- ^ 'pmdNamespace'
    -> [MetricDatum] -- ^ 'pmdMetricData'
    -> m PutMetricDataResponse
putMetricData p1 p2 =
    send (mkPutMetricData p1 p2)

putMetricDataCatch :: ( MonadCatch m
                      , MonadResource m
                      , MonadReader Env m
                      )
    => Text -- ^ 'pmdNamespace'
    -> [MetricDatum] -- ^ 'pmdMetricData'
    -> m (Either ServiceEr PutMetricDataResponse)
putMetricDataCatch p1 p2 =
    sendCatch (mkPutMetricData p1 p2)

-- $SetAlarmState
-- Temporarily sets the state of an alarm. When the updated StateValue differs
-- from the previous value, the action configured for the appropriate state is
-- invoked. This is not a permanent change. The next periodic alarm check (in
-- about a minute) will set the alarm to its actual state.
--
-- See: 'Network.AWS.CloudWatch.SetAlarmState'

setAlarmState :: ( MonadCatch m
                 , MonadResource m
                 , MonadError AWS.Error m
                 , MonadReader Env m
                 )
    => Text -- ^ 'sasAlarmName'
    -> StateValue -- ^ 'sasStateValue'
    -> Text -- ^ 'sasStateReason'
    -> State SetAlarmState a
    -> m SetAlarmStateResponse
setAlarmState p1 p2 p3 s =
    send $ (mkSetAlarmState p1 p2 p3) &~ s

setAlarmStateCatch :: ( MonadCatch m
                      , MonadResource m
                      , MonadReader Env m
                      )
    => Text -- ^ 'sasAlarmName'
    -> StateValue -- ^ 'sasStateValue'
    -> Text -- ^ 'sasStateReason'
    -> State SetAlarmState a
    -> m (Either ServiceEr SetAlarmStateResponse)
setAlarmStateCatch p1 p2 p3 s =
    sendCatch $ (mkSetAlarmState p1 p2 p3) &~ s
