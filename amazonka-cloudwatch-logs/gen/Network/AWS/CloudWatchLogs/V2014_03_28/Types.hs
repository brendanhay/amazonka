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
ilfTimestamp
    :: Functor f
    => (Integer
    -> f (Integer))
    -> InputLogEvent
    -> f InputLogEvent
ilfTimestamp f x =
    (\y -> x { _ilfTimestamp = y })
       <$> f (_ilfTimestamp x)
{-# INLINE ilfTimestamp #-}

ilfMessage
    :: Functor f
    => (Text
    -> f (Text))
    -> InputLogEvent
    -> f InputLogEvent
ilfMessage f x =
    (\y -> x { _ilfMessage = y })
       <$> f (_ilfMessage x)
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

lhLogGroupName
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> LogGroup
    -> f LogGroup
lhLogGroupName f x =
    (\y -> x { _lhLogGroupName = y })
       <$> f (_lhLogGroupName x)
{-# INLINE lhLogGroupName #-}

-- | A point in time expressed as the number milliseconds since Jan 1, 1970
-- 00:00:00 UTC.
lhCreationTime
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> LogGroup
    -> f LogGroup
lhCreationTime f x =
    (\y -> x { _lhCreationTime = y })
       <$> f (_lhCreationTime x)
{-# INLINE lhCreationTime #-}

-- | Specifies the number of days you want to retain log events in the specified
-- log group. Possible values are: 1, 3, 5, 7, 14, 30, 60, 90, 120, 150, 180,
-- 365, 400, 547, 730.
lhRetentionInDays
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> LogGroup
    -> f LogGroup
lhRetentionInDays f x =
    (\y -> x { _lhRetentionInDays = y })
       <$> f (_lhRetentionInDays x)
{-# INLINE lhRetentionInDays #-}

-- | The number of metric filters associated with the log group.
lhMetricFilterCount
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> LogGroup
    -> f LogGroup
lhMetricFilterCount f x =
    (\y -> x { _lhMetricFilterCount = y })
       <$> f (_lhMetricFilterCount x)
{-# INLINE lhMetricFilterCount #-}

lhArn
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> LogGroup
    -> f LogGroup
lhArn f x =
    (\y -> x { _lhArn = y })
       <$> f (_lhArn x)
{-# INLINE lhArn #-}

lhStoredBytes
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> LogGroup
    -> f LogGroup
lhStoredBytes f x =
    (\y -> x { _lhStoredBytes = y })
       <$> f (_lhStoredBytes x)
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

ltLogStreamName
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> LogStream
    -> f LogStream
ltLogStreamName f x =
    (\y -> x { _ltLogStreamName = y })
       <$> f (_ltLogStreamName x)
{-# INLINE ltLogStreamName #-}

-- | A point in time expressed as the number milliseconds since Jan 1, 1970
-- 00:00:00 UTC.
ltCreationTime
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> LogStream
    -> f LogStream
ltCreationTime f x =
    (\y -> x { _ltCreationTime = y })
       <$> f (_ltCreationTime x)
{-# INLINE ltCreationTime #-}

-- | A point in time expressed as the number milliseconds since Jan 1, 1970
-- 00:00:00 UTC.
ltFirstEventTimestamp
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> LogStream
    -> f LogStream
ltFirstEventTimestamp f x =
    (\y -> x { _ltFirstEventTimestamp = y })
       <$> f (_ltFirstEventTimestamp x)
{-# INLINE ltFirstEventTimestamp #-}

-- | A point in time expressed as the number milliseconds since Jan 1, 1970
-- 00:00:00 UTC.
ltLastEventTimestamp
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> LogStream
    -> f LogStream
ltLastEventTimestamp f x =
    (\y -> x { _ltLastEventTimestamp = y })
       <$> f (_ltLastEventTimestamp x)
{-# INLINE ltLastEventTimestamp #-}

-- | A point in time expressed as the number milliseconds since Jan 1, 1970
-- 00:00:00 UTC.
ltLastIngestionTime
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> LogStream
    -> f LogStream
ltLastIngestionTime f x =
    (\y -> x { _ltLastIngestionTime = y })
       <$> f (_ltLastIngestionTime x)
{-# INLINE ltLastIngestionTime #-}

-- | A string token used for making PutLogEvents requests. A sequenceToken can
-- only be used once, and PutLogEvents requests must include the sequenceToken
-- obtained from the response of the previous request.
ltUploadSequenceToken
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> LogStream
    -> f LogStream
ltUploadSequenceToken f x =
    (\y -> x { _ltUploadSequenceToken = y })
       <$> f (_ltUploadSequenceToken x)
{-# INLINE ltUploadSequenceToken #-}

ltArn
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> LogStream
    -> f LogStream
ltArn f x =
    (\y -> x { _ltArn = y })
       <$> f (_ltArn x)
{-# INLINE ltArn #-}

ltStoredBytes
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> LogStream
    -> f LogStream
ltStoredBytes f x =
    (\y -> x { _ltStoredBytes = y })
       <$> f (_ltStoredBytes x)
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
mgFilterName
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> MetricFilter
    -> f MetricFilter
mgFilterName f x =
    (\y -> x { _mgFilterName = y })
       <$> f (_mgFilterName x)
{-# INLINE mgFilterName #-}

mgFilterPattern
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> MetricFilter
    -> f MetricFilter
mgFilterPattern f x =
    (\y -> x { _mgFilterPattern = y })
       <$> f (_mgFilterPattern x)
{-# INLINE mgFilterPattern #-}

mgMetricTransformations
    :: Functor f
    => (Maybe [MetricTransformation]
    -> f (Maybe [MetricTransformation]))
    -> MetricFilter
    -> f MetricFilter
mgMetricTransformations f x =
    (\y -> x { _mgMetricTransformations = y })
       <$> f (_mgMetricTransformations x)
{-# INLINE mgMetricTransformations #-}

-- | A point in time expressed as the number milliseconds since Jan 1, 1970
-- 00:00:00 UTC.
mgCreationTime
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> MetricFilter
    -> f MetricFilter
mgCreationTime f x =
    (\y -> x { _mgCreationTime = y })
       <$> f (_mgCreationTime x)
{-# INLINE mgCreationTime #-}

instance FromJSON MetricFilter

data MetricFilterMatchRecord = MetricFilterMatchRecord
    { _mfmrEventNumber :: Maybe Integer
    , _mfmrEventMessage :: Maybe Text
    , _mfmrExtractedValues :: Map Text Text
    } deriving (Show, Generic)

mfmrEventNumber
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> MetricFilterMatchRecord
    -> f MetricFilterMatchRecord
mfmrEventNumber f x =
    (\y -> x { _mfmrEventNumber = y })
       <$> f (_mfmrEventNumber x)
{-# INLINE mfmrEventNumber #-}

mfmrEventMessage
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> MetricFilterMatchRecord
    -> f MetricFilterMatchRecord
mfmrEventMessage f x =
    (\y -> x { _mfmrEventMessage = y })
       <$> f (_mfmrEventMessage x)
{-# INLINE mfmrEventMessage #-}

mfmrExtractedValues
    :: Functor f
    => (Map Text Text
    -> f (Map Text Text))
    -> MetricFilterMatchRecord
    -> f MetricFilterMatchRecord
mfmrExtractedValues f x =
    (\y -> x { _mfmrExtractedValues = y })
       <$> f (_mfmrExtractedValues x)
{-# INLINE mfmrExtractedValues #-}

instance FromJSON MetricFilterMatchRecord

data MetricTransformation = MetricTransformation
    { _muMetricName :: Text
    , _muMetricNamespace :: Text
    , _muMetricValue :: Text
    } deriving (Show, Generic)

muMetricName
    :: Functor f
    => (Text
    -> f (Text))
    -> MetricTransformation
    -> f MetricTransformation
muMetricName f x =
    (\y -> x { _muMetricName = y })
       <$> f (_muMetricName x)
{-# INLINE muMetricName #-}

muMetricNamespace
    :: Functor f
    => (Text
    -> f (Text))
    -> MetricTransformation
    -> f MetricTransformation
muMetricNamespace f x =
    (\y -> x { _muMetricNamespace = y })
       <$> f (_muMetricNamespace x)
{-# INLINE muMetricNamespace #-}

muMetricValue
    :: Functor f
    => (Text
    -> f (Text))
    -> MetricTransformation
    -> f MetricTransformation
muMetricValue f x =
    (\y -> x { _muMetricValue = y })
       <$> f (_muMetricValue x)
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
olfTimestamp
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> OutputLogEvent
    -> f OutputLogEvent
olfTimestamp f x =
    (\y -> x { _olfTimestamp = y })
       <$> f (_olfTimestamp x)
{-# INLINE olfTimestamp #-}

olfMessage
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> OutputLogEvent
    -> f OutputLogEvent
olfMessage f x =
    (\y -> x { _olfMessage = y })
       <$> f (_olfMessage x)
{-# INLINE olfMessage #-}

-- | A point in time expressed as the number milliseconds since Jan 1, 1970
-- 00:00:00 UTC.
olfIngestionTime
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> OutputLogEvent
    -> f OutputLogEvent
olfIngestionTime f x =
    (\y -> x { _olfIngestionTime = y })
       <$> f (_olfIngestionTime x)
{-# INLINE olfIngestionTime #-}

instance FromJSON OutputLogEvent
