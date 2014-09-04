{-# LANGUAGE DeriveDataTypeable          #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE StandaloneDeriving          #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudWatchLogs.V2014_03_28.Types
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Amazon CloudWatch Logs enables you to monitor, store, and access your
-- system, application, and custom log files.
module Network.AWS.CloudWatchLogs.V2014_03_28.Types
    (
    -- * Service
      CloudWatchLogs
    -- ** Errors
    , Er (..)
    -- * InputLogEvent
    , InputLogEvent (..)
    , ilfTimestamp
    , ilfMessage

    -- * LogGroup
    , LogGroup (..)
    , lhLogGroupName
    , lhCreationTime
    , lhRetentionInDays
    , lhMetricFilterCount
    , lhArn
    , lhStoredBytes

    -- * LogStream
    , LogStream (..)
    , ltLogStreamName
    , ltCreationTime
    , ltFirstEventTimestamp
    , ltLastEventTimestamp
    , ltLastIngestionTime
    , ltUploadSequenceToken
    , ltArn
    , ltStoredBytes

    -- * MetricFilter
    , MetricFilter (..)
    , mgFilterName
    , mgFilterPattern
    , mgMetricTransformations
    , mgCreationTime

    -- * MetricFilterMatchRecord
    , MetricFilterMatchRecord (..)
    , mfmrEventNumber
    , mfmrEventMessage
    , mfmrExtractedValues

    -- * MetricTransformation
    , MetricTransformation (..)
    , muMetricName
    , muMetricNamespace
    , muMetricValue

    -- * OutputLogEvent
    , OutputLogEvent (..)
    , olfTimestamp
    , olfMessage
    , olfIngestionTime

    ) where

import Network.AWS.Prelude
import Network.AWS.Signing.V4

-- | Supported version (@2014-03-28@) of the
-- @Amazon CloudWatch Logs@ service.
data CloudWatchLogs deriving (Typeable)

instance AWSService CloudWatchLogs where
    type Sg CloudWatchLogs = V4
    data Er CloudWatchLogs
        = CloudWatchLogsClient HttpException
        | CloudWatchLogsSerializer String
        | CloudWatchLogsService String
        | DataAlreadyAcceptedException
            { _daaeExpectedSequenceToken :: Maybe Text
            }
        | InvalidParameterException
        | InvalidSequenceTokenException
            { _isteExpectedSequenceToken :: Maybe Text
            }
        | LimitExceededException
        | OperationAbortedException
        | ResourceAlreadyExistsException
        | ResourceNotFoundException
        | ServiceUnavailableException

    service = Service'
        { _svcEndpoint = Regional
        , _svcPrefix   = "logs"
        , _svcVersion  = "2014-03-28"
        , _svcTarget   = Nothing
        }

deriving instance Show    (Er CloudWatchLogs)
deriving instance Generic (Er CloudWatchLogs)

instance AWSError (Er CloudWatchLogs) where
    awsError = const "CloudWatchLogsError"

instance AWSServiceError (Er CloudWatchLogs) where
    serviceError    = CloudWatchLogsService
    clientError     = CloudWatchLogsClient
    serializerError = CloudWatchLogsSerializer

instance Exception (Er CloudWatchLogs)

-- | A log event is a record of some activity that was recorded by the
-- application or resource being monitored. The log event record that Amazon
-- CloudWatch Logs understands contains two properties: the timestamp of when
-- the event occurred, and the raw event message.
data InputLogEvent = InputLogEvent
    { _ilfTimestamp :: Integer
      -- ^ A point in time expressed as the number milliseconds since Jan 1,
      -- 1970 00:00:00 UTC.
    , _ilfMessage :: Text
    } deriving (Show, Generic)

-- | A point in time expressed as the number milliseconds since Jan 1, 1970
-- 00:00:00 UTC.
ilfTimestamp :: Lens' InputLogEvent (Integer)
ilfTimestamp f x =
    f (_ilfTimestamp x)
        <&> \y -> x { _ilfTimestamp = y }
{-# INLINE ilfTimestamp #-}

ilfMessage :: Lens' InputLogEvent (Text)
ilfMessage f x =
    f (_ilfMessage x)
        <&> \y -> x { _ilfMessage = y }
{-# INLINE ilfMessage #-}

instance ToJSON InputLogEvent

data LogGroup = LogGroup
    { _lhLogGroupName :: Maybe Text
    , _lhCreationTime :: Maybe Integer
      -- ^ A point in time expressed as the number milliseconds since Jan 1,
      -- 1970 00:00:00 UTC.
    , _lhRetentionInDays :: Maybe Integer
      -- ^ Specifies the number of days you want to retain log events in the
      -- specified log group. Possible values are: 1, 3, 5, 7, 14, 30, 60,
      -- 90, 120, 150, 180, 365, 400, 547, 730.
    , _lhMetricFilterCount :: Maybe Integer
      -- ^ The number of metric filters associated with the log group.
    , _lhArn :: Maybe Text
    , _lhStoredBytes :: Maybe Integer
    } deriving (Show, Generic)

lhLogGroupName :: Lens' LogGroup (Maybe Text)
lhLogGroupName f x =
    f (_lhLogGroupName x)
        <&> \y -> x { _lhLogGroupName = y }
{-# INLINE lhLogGroupName #-}

-- | A point in time expressed as the number milliseconds since Jan 1, 1970
-- 00:00:00 UTC.
lhCreationTime :: Lens' LogGroup (Maybe Integer)
lhCreationTime f x =
    f (_lhCreationTime x)
        <&> \y -> x { _lhCreationTime = y }
{-# INLINE lhCreationTime #-}

-- | Specifies the number of days you want to retain log events in the specified
-- log group. Possible values are: 1, 3, 5, 7, 14, 30, 60, 90, 120, 150, 180,
-- 365, 400, 547, 730.
lhRetentionInDays :: Lens' LogGroup (Maybe Integer)
lhRetentionInDays f x =
    f (_lhRetentionInDays x)
        <&> \y -> x { _lhRetentionInDays = y }
{-# INLINE lhRetentionInDays #-}

-- | The number of metric filters associated with the log group.
lhMetricFilterCount :: Lens' LogGroup (Maybe Integer)
lhMetricFilterCount f x =
    f (_lhMetricFilterCount x)
        <&> \y -> x { _lhMetricFilterCount = y }
{-# INLINE lhMetricFilterCount #-}

lhArn :: Lens' LogGroup (Maybe Text)
lhArn f x =
    f (_lhArn x)
        <&> \y -> x { _lhArn = y }
{-# INLINE lhArn #-}

lhStoredBytes :: Lens' LogGroup (Maybe Integer)
lhStoredBytes f x =
    f (_lhStoredBytes x)
        <&> \y -> x { _lhStoredBytes = y }
{-# INLINE lhStoredBytes #-}

instance FromJSON LogGroup

-- | A log stream is sequence of log events that share the same emitter.
data LogStream = LogStream
    { _ltLogStreamName :: Maybe Text
    , _ltCreationTime :: Maybe Integer
      -- ^ A point in time expressed as the number milliseconds since Jan 1,
      -- 1970 00:00:00 UTC.
    , _ltFirstEventTimestamp :: Maybe Integer
      -- ^ A point in time expressed as the number milliseconds since Jan 1,
      -- 1970 00:00:00 UTC.
    , _ltLastEventTimestamp :: Maybe Integer
      -- ^ A point in time expressed as the number milliseconds since Jan 1,
      -- 1970 00:00:00 UTC.
    , _ltLastIngestionTime :: Maybe Integer
      -- ^ A point in time expressed as the number milliseconds since Jan 1,
      -- 1970 00:00:00 UTC.
    , _ltUploadSequenceToken :: Maybe Text
      -- ^ A string token used for making PutLogEvents requests. A
      -- sequenceToken can only be used once, and PutLogEvents requests
      -- must include the sequenceToken obtained from the response of the
      -- previous request.
    , _ltArn :: Maybe Text
    , _ltStoredBytes :: Maybe Integer
    } deriving (Show, Generic)

ltLogStreamName :: Lens' LogStream (Maybe Text)
ltLogStreamName f x =
    f (_ltLogStreamName x)
        <&> \y -> x { _ltLogStreamName = y }
{-# INLINE ltLogStreamName #-}

-- | A point in time expressed as the number milliseconds since Jan 1, 1970
-- 00:00:00 UTC.
ltCreationTime :: Lens' LogStream (Maybe Integer)
ltCreationTime f x =
    f (_ltCreationTime x)
        <&> \y -> x { _ltCreationTime = y }
{-# INLINE ltCreationTime #-}

-- | A point in time expressed as the number milliseconds since Jan 1, 1970
-- 00:00:00 UTC.
ltFirstEventTimestamp :: Lens' LogStream (Maybe Integer)
ltFirstEventTimestamp f x =
    f (_ltFirstEventTimestamp x)
        <&> \y -> x { _ltFirstEventTimestamp = y }
{-# INLINE ltFirstEventTimestamp #-}

-- | A point in time expressed as the number milliseconds since Jan 1, 1970
-- 00:00:00 UTC.
ltLastEventTimestamp :: Lens' LogStream (Maybe Integer)
ltLastEventTimestamp f x =
    f (_ltLastEventTimestamp x)
        <&> \y -> x { _ltLastEventTimestamp = y }
{-# INLINE ltLastEventTimestamp #-}

-- | A point in time expressed as the number milliseconds since Jan 1, 1970
-- 00:00:00 UTC.
ltLastIngestionTime :: Lens' LogStream (Maybe Integer)
ltLastIngestionTime f x =
    f (_ltLastIngestionTime x)
        <&> \y -> x { _ltLastIngestionTime = y }
{-# INLINE ltLastIngestionTime #-}

-- | A string token used for making PutLogEvents requests. A sequenceToken can
-- only be used once, and PutLogEvents requests must include the sequenceToken
-- obtained from the response of the previous request.
ltUploadSequenceToken :: Lens' LogStream (Maybe Text)
ltUploadSequenceToken f x =
    f (_ltUploadSequenceToken x)
        <&> \y -> x { _ltUploadSequenceToken = y }
{-# INLINE ltUploadSequenceToken #-}

ltArn :: Lens' LogStream (Maybe Text)
ltArn f x =
    f (_ltArn x)
        <&> \y -> x { _ltArn = y }
{-# INLINE ltArn #-}

ltStoredBytes :: Lens' LogStream (Maybe Integer)
ltStoredBytes f x =
    f (_ltStoredBytes x)
        <&> \y -> x { _ltStoredBytes = y }
{-# INLINE ltStoredBytes #-}

instance FromJSON LogStream

-- | Metric filters can be used to express how Amazon CloudWatch Logs would
-- extract metric observations from ingested log events and transform them to
-- metric data in a CloudWatch metric.
data MetricFilter = MetricFilter
    { _mgFilterName :: Maybe Text
      -- ^ The name of the metric filter.
    , _mgFilterPattern :: Maybe Text
    , _mgMetricTransformations :: Maybe [MetricTransformation]
    , _mgCreationTime :: Maybe Integer
      -- ^ A point in time expressed as the number milliseconds since Jan 1,
      -- 1970 00:00:00 UTC.
    } deriving (Show, Generic)

-- | The name of the metric filter.
mgFilterName :: Lens' MetricFilter (Maybe Text)
mgFilterName f x =
    f (_mgFilterName x)
        <&> \y -> x { _mgFilterName = y }
{-# INLINE mgFilterName #-}

mgFilterPattern :: Lens' MetricFilter (Maybe Text)
mgFilterPattern f x =
    f (_mgFilterPattern x)
        <&> \y -> x { _mgFilterPattern = y }
{-# INLINE mgFilterPattern #-}

mgMetricTransformations :: Lens' MetricFilter (Maybe [MetricTransformation])
mgMetricTransformations f x =
    f (_mgMetricTransformations x)
        <&> \y -> x { _mgMetricTransformations = y }
{-# INLINE mgMetricTransformations #-}

-- | A point in time expressed as the number milliseconds since Jan 1, 1970
-- 00:00:00 UTC.
mgCreationTime :: Lens' MetricFilter (Maybe Integer)
mgCreationTime f x =
    f (_mgCreationTime x)
        <&> \y -> x { _mgCreationTime = y }
{-# INLINE mgCreationTime #-}

instance FromJSON MetricFilter

data MetricFilterMatchRecord = MetricFilterMatchRecord
    { _mfmrEventNumber :: Maybe Integer
    , _mfmrEventMessage :: Maybe Text
    , _mfmrExtractedValues :: Map Text Text
    } deriving (Show, Generic)

mfmrEventNumber :: Lens' MetricFilterMatchRecord (Maybe Integer)
mfmrEventNumber f x =
    f (_mfmrEventNumber x)
        <&> \y -> x { _mfmrEventNumber = y }
{-# INLINE mfmrEventNumber #-}

mfmrEventMessage :: Lens' MetricFilterMatchRecord (Maybe Text)
mfmrEventMessage f x =
    f (_mfmrEventMessage x)
        <&> \y -> x { _mfmrEventMessage = y }
{-# INLINE mfmrEventMessage #-}

mfmrExtractedValues :: Lens' MetricFilterMatchRecord (Map Text Text)
mfmrExtractedValues f x =
    f (_mfmrExtractedValues x)
        <&> \y -> x { _mfmrExtractedValues = y }
{-# INLINE mfmrExtractedValues #-}

instance FromJSON MetricFilterMatchRecord

data MetricTransformation = MetricTransformation
    { _muMetricName :: Text
    , _muMetricNamespace :: Text
    , _muMetricValue :: Text
    } deriving (Show, Generic)

muMetricName :: Lens' MetricTransformation (Text)
muMetricName f x =
    f (_muMetricName x)
        <&> \y -> x { _muMetricName = y }
{-# INLINE muMetricName #-}

muMetricNamespace :: Lens' MetricTransformation (Text)
muMetricNamespace f x =
    f (_muMetricNamespace x)
        <&> \y -> x { _muMetricNamespace = y }
{-# INLINE muMetricNamespace #-}

muMetricValue :: Lens' MetricTransformation (Text)
muMetricValue f x =
    f (_muMetricValue x)
        <&> \y -> x { _muMetricValue = y }
{-# INLINE muMetricValue #-}

instance FromJSON MetricTransformation

instance ToJSON MetricTransformation

data OutputLogEvent = OutputLogEvent
    { _olfTimestamp :: Maybe Integer
      -- ^ A point in time expressed as the number milliseconds since Jan 1,
      -- 1970 00:00:00 UTC.
    , _olfMessage :: Maybe Text
    , _olfIngestionTime :: Maybe Integer
      -- ^ A point in time expressed as the number milliseconds since Jan 1,
      -- 1970 00:00:00 UTC.
    } deriving (Show, Generic)

-- | A point in time expressed as the number milliseconds since Jan 1, 1970
-- 00:00:00 UTC.
olfTimestamp :: Lens' OutputLogEvent (Maybe Integer)
olfTimestamp f x =
    f (_olfTimestamp x)
        <&> \y -> x { _olfTimestamp = y }
{-# INLINE olfTimestamp #-}

olfMessage :: Lens' OutputLogEvent (Maybe Text)
olfMessage f x =
    f (_olfMessage x)
        <&> \y -> x { _olfMessage = y }
{-# INLINE olfMessage #-}

-- | A point in time expressed as the number milliseconds since Jan 1, 1970
-- 00:00:00 UTC.
olfIngestionTime :: Lens' OutputLogEvent (Maybe Integer)
olfIngestionTime f x =
    f (_olfIngestionTime x)
        <&> \y -> x { _olfIngestionTime = y }
{-# INLINE olfIngestionTime #-}

instance FromJSON OutputLogEvent
