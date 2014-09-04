{-# LANGUAGE DeriveDataTypeable          #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
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
    , InputLogEvent
    , mkInputLogEvent
    , ilfTimestamp
    , ilfMessage

    -- * LogGroup
    , LogGroup
    , lhLogGroupName
    , lhCreationTime
    , lhRetentionInDays
    , lhMetricFilterCount
    , lhArn
    , lhStoredBytes

    -- * LogStream
    , LogStream
    , ltLogStreamName
    , ltCreationTime
    , ltFirstEventTimestamp
    , ltLastEventTimestamp
    , ltLastIngestionTime
    , ltUploadSequenceToken
    , ltArn
    , ltStoredBytes

    -- * MetricFilter
    , MetricFilter
    , mgFilterName
    , mgFilterPattern
    , mgMetricTransformations
    , mgCreationTime

    -- * MetricFilterMatchRecord
    , MetricFilterMatchRecord
    , mfmrEventNumber
    , mfmrEventMessage
    , mfmrExtractedValues

    -- * MetricTransformation
    , MetricTransformation
    , mkMetricTransformation
    , muMetricName
    , muMetricNamespace
    , muMetricValue

    -- * OutputLogEvent
    , OutputLogEvent
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
ilfTimestamp = lens _ilfTimestamp (\s a -> s { _ilfTimestamp = a })
{-# INLINE ilfTimestamp #-}

ilfMessage :: Lens' InputLogEvent (Text)
ilfMessage = lens _ilfMessage (\s a -> s { _ilfMessage = a })
{-# INLINE ilfMessage #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'InputLogEvent' data type to populate a request.
mkInputLogEvent :: Integer -- ^ 'ilfTimestamp'
                -> Text -- ^ 'ilfMessage'
                -> InputLogEvent
mkInputLogEvent p1 p2 = InputLogEvent
    { _ilfTimestamp = p1
    , _ilfMessage = p2
    }
{-# INLINE mkInputLogEvent #-}

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
lhLogGroupName = lens _lhLogGroupName (\s a -> s { _lhLogGroupName = a })
{-# INLINE lhLogGroupName #-}

-- | A point in time expressed as the number milliseconds since Jan 1, 1970
-- 00:00:00 UTC.
lhCreationTime :: Lens' LogGroup (Maybe Integer)
lhCreationTime = lens _lhCreationTime (\s a -> s { _lhCreationTime = a })
{-# INLINE lhCreationTime #-}

-- | Specifies the number of days you want to retain log events in the specified
-- log group. Possible values are: 1, 3, 5, 7, 14, 30, 60, 90, 120, 150, 180,
-- 365, 400, 547, 730.
lhRetentionInDays :: Lens' LogGroup (Maybe Integer)
lhRetentionInDays = lens _lhRetentionInDays (\s a -> s { _lhRetentionInDays = a })
{-# INLINE lhRetentionInDays #-}

-- | The number of metric filters associated with the log group.
lhMetricFilterCount :: Lens' LogGroup (Maybe Integer)
lhMetricFilterCount = lens _lhMetricFilterCount (\s a -> s { _lhMetricFilterCount = a })
{-# INLINE lhMetricFilterCount #-}

lhArn :: Lens' LogGroup (Maybe Text)
lhArn = lens _lhArn (\s a -> s { _lhArn = a })
{-# INLINE lhArn #-}

lhStoredBytes :: Lens' LogGroup (Maybe Integer)
lhStoredBytes = lens _lhStoredBytes (\s a -> s { _lhStoredBytes = a })
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
ltLogStreamName = lens _ltLogStreamName (\s a -> s { _ltLogStreamName = a })
{-# INLINE ltLogStreamName #-}

-- | A point in time expressed as the number milliseconds since Jan 1, 1970
-- 00:00:00 UTC.
ltCreationTime :: Lens' LogStream (Maybe Integer)
ltCreationTime = lens _ltCreationTime (\s a -> s { _ltCreationTime = a })
{-# INLINE ltCreationTime #-}

-- | A point in time expressed as the number milliseconds since Jan 1, 1970
-- 00:00:00 UTC.
ltFirstEventTimestamp :: Lens' LogStream (Maybe Integer)
ltFirstEventTimestamp = lens _ltFirstEventTimestamp (\s a -> s { _ltFirstEventTimestamp = a })
{-# INLINE ltFirstEventTimestamp #-}

-- | A point in time expressed as the number milliseconds since Jan 1, 1970
-- 00:00:00 UTC.
ltLastEventTimestamp :: Lens' LogStream (Maybe Integer)
ltLastEventTimestamp = lens _ltLastEventTimestamp (\s a -> s { _ltLastEventTimestamp = a })
{-# INLINE ltLastEventTimestamp #-}

-- | A point in time expressed as the number milliseconds since Jan 1, 1970
-- 00:00:00 UTC.
ltLastIngestionTime :: Lens' LogStream (Maybe Integer)
ltLastIngestionTime = lens _ltLastIngestionTime (\s a -> s { _ltLastIngestionTime = a })
{-# INLINE ltLastIngestionTime #-}

-- | A string token used for making PutLogEvents requests. A sequenceToken can
-- only be used once, and PutLogEvents requests must include the sequenceToken
-- obtained from the response of the previous request.
ltUploadSequenceToken :: Lens' LogStream (Maybe Text)
ltUploadSequenceToken = lens _ltUploadSequenceToken (\s a -> s { _ltUploadSequenceToken = a })
{-# INLINE ltUploadSequenceToken #-}

ltArn :: Lens' LogStream (Maybe Text)
ltArn = lens _ltArn (\s a -> s { _ltArn = a })
{-# INLINE ltArn #-}

ltStoredBytes :: Lens' LogStream (Maybe Integer)
ltStoredBytes = lens _ltStoredBytes (\s a -> s { _ltStoredBytes = a })
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
mgFilterName = lens _mgFilterName (\s a -> s { _mgFilterName = a })
{-# INLINE mgFilterName #-}

mgFilterPattern :: Lens' MetricFilter (Maybe Text)
mgFilterPattern = lens _mgFilterPattern (\s a -> s { _mgFilterPattern = a })
{-# INLINE mgFilterPattern #-}

mgMetricTransformations :: Lens' MetricFilter (Maybe [MetricTransformation])
mgMetricTransformations = lens _mgMetricTransformations (\s a -> s { _mgMetricTransformations = a })
{-# INLINE mgMetricTransformations #-}

-- | A point in time expressed as the number milliseconds since Jan 1, 1970
-- 00:00:00 UTC.
mgCreationTime :: Lens' MetricFilter (Maybe Integer)
mgCreationTime = lens _mgCreationTime (\s a -> s { _mgCreationTime = a })
{-# INLINE mgCreationTime #-}

instance FromJSON MetricFilter

data MetricFilterMatchRecord = MetricFilterMatchRecord
    { _mfmrEventNumber :: Maybe Integer
    , _mfmrEventMessage :: Maybe Text
    , _mfmrExtractedValues :: Map Text Text
    } deriving (Show, Generic)

mfmrEventNumber :: Lens' MetricFilterMatchRecord (Maybe Integer)
mfmrEventNumber = lens _mfmrEventNumber (\s a -> s { _mfmrEventNumber = a })
{-# INLINE mfmrEventNumber #-}

mfmrEventMessage :: Lens' MetricFilterMatchRecord (Maybe Text)
mfmrEventMessage = lens _mfmrEventMessage (\s a -> s { _mfmrEventMessage = a })
{-# INLINE mfmrEventMessage #-}

mfmrExtractedValues :: Lens' MetricFilterMatchRecord (Map Text Text)
mfmrExtractedValues = lens _mfmrExtractedValues (\s a -> s { _mfmrExtractedValues = a })
{-# INLINE mfmrExtractedValues #-}

instance FromJSON MetricFilterMatchRecord

data MetricTransformation = MetricTransformation
    { _muMetricName :: Text
    , _muMetricNamespace :: Text
    , _muMetricValue :: Text
    } deriving (Show, Generic)

muMetricName :: Lens' MetricTransformation (Text)
muMetricName = lens _muMetricName (\s a -> s { _muMetricName = a })
{-# INLINE muMetricName #-}

muMetricNamespace :: Lens' MetricTransformation (Text)
muMetricNamespace = lens _muMetricNamespace (\s a -> s { _muMetricNamespace = a })
{-# INLINE muMetricNamespace #-}

muMetricValue :: Lens' MetricTransformation (Text)
muMetricValue = lens _muMetricValue (\s a -> s { _muMetricValue = a })
{-# INLINE muMetricValue #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'MetricTransformation' data type to populate a request.
mkMetricTransformation :: Text -- ^ 'muMetricName'
                       -> Text -- ^ 'muMetricNamespace'
                       -> Text -- ^ 'muMetricValue'
                       -> MetricTransformation
mkMetricTransformation p1 p2 p3 = MetricTransformation
    { _muMetricName = p1
    , _muMetricNamespace = p2
    , _muMetricValue = p3
    }
{-# INLINE mkMetricTransformation #-}

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
olfTimestamp = lens _olfTimestamp (\s a -> s { _olfTimestamp = a })
{-# INLINE olfTimestamp #-}

olfMessage :: Lens' OutputLogEvent (Maybe Text)
olfMessage = lens _olfMessage (\s a -> s { _olfMessage = a })
{-# INLINE olfMessage #-}

-- | A point in time expressed as the number milliseconds since Jan 1, 1970
-- 00:00:00 UTC.
olfIngestionTime :: Lens' OutputLogEvent (Maybe Integer)
olfIngestionTime = lens _olfIngestionTime (\s a -> s { _olfIngestionTime = a })
{-# INLINE olfIngestionTime #-}

instance FromJSON OutputLogEvent
