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
    , ileTimestamp
    , ileMessage

    -- * LogGroup
    , LogGroup
    , mkLogGroup
    , lgLogGroupName
    , lgCreationTime
    , lgRetentionInDays
    , lgMetricFilterCount
    , lgArn
    , lgStoredBytes

    -- * LogStream
    , LogStream
    , mkLogStream
    , lsLogStreamName
    , lsCreationTime
    , lsFirstEventTimestamp
    , lsLastEventTimestamp
    , lsLastIngestionTime
    , lsUploadSequenceToken
    , lsArn
    , lsStoredBytes

    -- * MetricFilter
    , MetricFilter
    , mkMetricFilter
    , mfFilterName
    , mfFilterPattern
    , mfMetricTransformations
    , mfCreationTime

    -- * MetricFilterMatchRecord
    , MetricFilterMatchRecord
    , mkMetricFilterMatchRecord
    , mfmrEventNumber
    , mfmrEventMessage
    , mfmrExtractedValues

    -- * MetricTransformation
    , MetricTransformation
    , mkMetricTransformation
    , mtMetricName
    , mtMetricNamespace
    , mtMetricValue

    -- * OutputLogEvent
    , OutputLogEvent
    , mkOutputLogEvent
    , oleTimestamp
    , oleMessage
    , oleIngestionTime
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
    { _ileTimestamp :: Integer
    , _ileMessage :: Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'InputLogEvent' data type to populate a request.
mkInputLogEvent :: Integer -- ^ 'ileTimestamp'
                -> Text -- ^ 'ileMessage'
                -> InputLogEvent
mkInputLogEvent p1 p2 = InputLogEvent
    { _ileTimestamp = p1
    , _ileMessage = p2
    }
{-# INLINE mkInputLogEvent #-}

-- | A point in time expressed as the number milliseconds since Jan 1, 1970
-- 00:00:00 UTC.
ileTimestamp :: Lens' InputLogEvent Integer
ileTimestamp = lens _ileTimestamp (\s a -> s { _ileTimestamp = a })
{-# INLINE ileTimestamp #-}

ileMessage :: Lens' InputLogEvent Text
ileMessage = lens _ileMessage (\s a -> s { _ileMessage = a })
{-# INLINE ileMessage #-}

instance ToJSON InputLogEvent

data LogGroup = LogGroup
    { _lgLogGroupName :: Maybe Text
    , _lgCreationTime :: Maybe Integer
    , _lgRetentionInDays :: Maybe Integer
    , _lgMetricFilterCount :: Maybe Integer
    , _lgArn :: Maybe Text
    , _lgStoredBytes :: Maybe Integer
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'LogGroup' data type to populate a request.
mkLogGroup :: LogGroup
mkLogGroup = LogGroup
    { _lgLogGroupName = Nothing
    , _lgCreationTime = Nothing
    , _lgRetentionInDays = Nothing
    , _lgMetricFilterCount = Nothing
    , _lgArn = Nothing
    , _lgStoredBytes = Nothing
    }
{-# INLINE mkLogGroup #-}

lgLogGroupName :: Lens' LogGroup (Maybe Text)
lgLogGroupName = lens _lgLogGroupName (\s a -> s { _lgLogGroupName = a })
{-# INLINE lgLogGroupName #-}

-- | A point in time expressed as the number milliseconds since Jan 1, 1970
-- 00:00:00 UTC.
lgCreationTime :: Lens' LogGroup (Maybe Integer)
lgCreationTime = lens _lgCreationTime (\s a -> s { _lgCreationTime = a })
{-# INLINE lgCreationTime #-}

-- | Specifies the number of days you want to retain log events in the specified
-- log group. Possible values are: 1, 3, 5, 7, 14, 30, 60, 90, 120, 150, 180,
-- 365, 400, 547, 730.
lgRetentionInDays :: Lens' LogGroup (Maybe Integer)
lgRetentionInDays =
    lens _lgRetentionInDays (\s a -> s { _lgRetentionInDays = a })
{-# INLINE lgRetentionInDays #-}

-- | The number of metric filters associated with the log group.
lgMetricFilterCount :: Lens' LogGroup (Maybe Integer)
lgMetricFilterCount =
    lens _lgMetricFilterCount (\s a -> s { _lgMetricFilterCount = a })
{-# INLINE lgMetricFilterCount #-}

lgArn :: Lens' LogGroup (Maybe Text)
lgArn = lens _lgArn (\s a -> s { _lgArn = a })
{-# INLINE lgArn #-}

lgStoredBytes :: Lens' LogGroup (Maybe Integer)
lgStoredBytes = lens _lgStoredBytes (\s a -> s { _lgStoredBytes = a })
{-# INLINE lgStoredBytes #-}

instance FromJSON LogGroup

-- | A log stream is sequence of log events that share the same emitter.
data LogStream = LogStream
    { _lsLogStreamName :: Maybe Text
    , _lsCreationTime :: Maybe Integer
    , _lsFirstEventTimestamp :: Maybe Integer
    , _lsLastEventTimestamp :: Maybe Integer
    , _lsLastIngestionTime :: Maybe Integer
    , _lsUploadSequenceToken :: Maybe Text
    , _lsArn :: Maybe Text
    , _lsStoredBytes :: Maybe Integer
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'LogStream' data type to populate a request.
mkLogStream :: LogStream
mkLogStream = LogStream
    { _lsLogStreamName = Nothing
    , _lsCreationTime = Nothing
    , _lsFirstEventTimestamp = Nothing
    , _lsLastEventTimestamp = Nothing
    , _lsLastIngestionTime = Nothing
    , _lsUploadSequenceToken = Nothing
    , _lsArn = Nothing
    , _lsStoredBytes = Nothing
    }
{-# INLINE mkLogStream #-}

lsLogStreamName :: Lens' LogStream (Maybe Text)
lsLogStreamName = lens _lsLogStreamName (\s a -> s { _lsLogStreamName = a })
{-# INLINE lsLogStreamName #-}

-- | A point in time expressed as the number milliseconds since Jan 1, 1970
-- 00:00:00 UTC.
lsCreationTime :: Lens' LogStream (Maybe Integer)
lsCreationTime = lens _lsCreationTime (\s a -> s { _lsCreationTime = a })
{-# INLINE lsCreationTime #-}

-- | A point in time expressed as the number milliseconds since Jan 1, 1970
-- 00:00:00 UTC.
lsFirstEventTimestamp :: Lens' LogStream (Maybe Integer)
lsFirstEventTimestamp =
    lens _lsFirstEventTimestamp (\s a -> s { _lsFirstEventTimestamp = a })
{-# INLINE lsFirstEventTimestamp #-}

-- | A point in time expressed as the number milliseconds since Jan 1, 1970
-- 00:00:00 UTC.
lsLastEventTimestamp :: Lens' LogStream (Maybe Integer)
lsLastEventTimestamp =
    lens _lsLastEventTimestamp (\s a -> s { _lsLastEventTimestamp = a })
{-# INLINE lsLastEventTimestamp #-}

-- | A point in time expressed as the number milliseconds since Jan 1, 1970
-- 00:00:00 UTC.
lsLastIngestionTime :: Lens' LogStream (Maybe Integer)
lsLastIngestionTime =
    lens _lsLastIngestionTime (\s a -> s { _lsLastIngestionTime = a })
{-# INLINE lsLastIngestionTime #-}

-- | A string token used for making PutLogEvents requests. A sequenceToken can
-- only be used once, and PutLogEvents requests must include the sequenceToken
-- obtained from the response of the previous request.
lsUploadSequenceToken :: Lens' LogStream (Maybe Text)
lsUploadSequenceToken =
    lens _lsUploadSequenceToken (\s a -> s { _lsUploadSequenceToken = a })
{-# INLINE lsUploadSequenceToken #-}

lsArn :: Lens' LogStream (Maybe Text)
lsArn = lens _lsArn (\s a -> s { _lsArn = a })
{-# INLINE lsArn #-}

lsStoredBytes :: Lens' LogStream (Maybe Integer)
lsStoredBytes = lens _lsStoredBytes (\s a -> s { _lsStoredBytes = a })
{-# INLINE lsStoredBytes #-}

instance FromJSON LogStream

-- | Metric filters can be used to express how Amazon CloudWatch Logs would
-- extract metric observations from ingested log events and transform them to
-- metric data in a CloudWatch metric.
data MetricFilter = MetricFilter
    { _mfFilterName :: Maybe Text
    , _mfFilterPattern :: Maybe Text
    , _mfMetricTransformations :: Maybe [MetricTransformation]
    , _mfCreationTime :: Maybe Integer
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'MetricFilter' data type to populate a request.
mkMetricFilter :: MetricFilter
mkMetricFilter = MetricFilter
    { _mfFilterName = Nothing
    , _mfFilterPattern = Nothing
    , _mfMetricTransformations = Nothing
    , _mfCreationTime = Nothing
    }
{-# INLINE mkMetricFilter #-}

-- | The name of the metric filter.
mfFilterName :: Lens' MetricFilter (Maybe Text)
mfFilterName = lens _mfFilterName (\s a -> s { _mfFilterName = a })
{-# INLINE mfFilterName #-}

mfFilterPattern :: Lens' MetricFilter (Maybe Text)
mfFilterPattern = lens _mfFilterPattern (\s a -> s { _mfFilterPattern = a })
{-# INLINE mfFilterPattern #-}

mfMetricTransformations :: Lens' MetricFilter (Maybe [MetricTransformation])
mfMetricTransformations =
    lens _mfMetricTransformations
         (\s a -> s { _mfMetricTransformations = a })
{-# INLINE mfMetricTransformations #-}

-- | A point in time expressed as the number milliseconds since Jan 1, 1970
-- 00:00:00 UTC.
mfCreationTime :: Lens' MetricFilter (Maybe Integer)
mfCreationTime = lens _mfCreationTime (\s a -> s { _mfCreationTime = a })
{-# INLINE mfCreationTime #-}

instance FromJSON MetricFilter

data MetricFilterMatchRecord = MetricFilterMatchRecord
    { _mfmrEventNumber :: Maybe Integer
    , _mfmrEventMessage :: Maybe Text
    , _mfmrExtractedValues :: Map Text Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'MetricFilterMatchRecord' data type to populate a request.
mkMetricFilterMatchRecord :: MetricFilterMatchRecord
mkMetricFilterMatchRecord = MetricFilterMatchRecord
    { _mfmrEventNumber = Nothing
    , _mfmrEventMessage = Nothing
    , _mfmrExtractedValues = mempty
    }
{-# INLINE mkMetricFilterMatchRecord #-}

mfmrEventNumber :: Lens' MetricFilterMatchRecord (Maybe Integer)
mfmrEventNumber = lens _mfmrEventNumber (\s a -> s { _mfmrEventNumber = a })
{-# INLINE mfmrEventNumber #-}

mfmrEventMessage :: Lens' MetricFilterMatchRecord (Maybe Text)
mfmrEventMessage =
    lens _mfmrEventMessage (\s a -> s { _mfmrEventMessage = a })
{-# INLINE mfmrEventMessage #-}

mfmrExtractedValues :: Lens' MetricFilterMatchRecord (Map Text Text)
mfmrExtractedValues =
    lens _mfmrExtractedValues (\s a -> s { _mfmrExtractedValues = a })
{-# INLINE mfmrExtractedValues #-}

instance FromJSON MetricFilterMatchRecord

data MetricTransformation = MetricTransformation
    { _mtMetricName :: Text
    , _mtMetricNamespace :: Text
    , _mtMetricValue :: Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'MetricTransformation' data type to populate a request.
mkMetricTransformation :: Text -- ^ 'mtMetricName'
                       -> Text -- ^ 'mtMetricNamespace'
                       -> Text -- ^ 'mtMetricValue'
                       -> MetricTransformation
mkMetricTransformation p1 p2 p3 = MetricTransformation
    { _mtMetricName = p1
    , _mtMetricNamespace = p2
    , _mtMetricValue = p3
    }
{-# INLINE mkMetricTransformation #-}

mtMetricName :: Lens' MetricTransformation Text
mtMetricName = lens _mtMetricName (\s a -> s { _mtMetricName = a })
{-# INLINE mtMetricName #-}

mtMetricNamespace :: Lens' MetricTransformation Text
mtMetricNamespace =
    lens _mtMetricNamespace (\s a -> s { _mtMetricNamespace = a })
{-# INLINE mtMetricNamespace #-}

mtMetricValue :: Lens' MetricTransformation Text
mtMetricValue = lens _mtMetricValue (\s a -> s { _mtMetricValue = a })
{-# INLINE mtMetricValue #-}

instance FromJSON MetricTransformation

instance ToJSON MetricTransformation

data OutputLogEvent = OutputLogEvent
    { _oleTimestamp :: Maybe Integer
    , _oleMessage :: Maybe Text
    , _oleIngestionTime :: Maybe Integer
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'OutputLogEvent' data type to populate a request.
mkOutputLogEvent :: OutputLogEvent
mkOutputLogEvent = OutputLogEvent
    { _oleTimestamp = Nothing
    , _oleMessage = Nothing
    , _oleIngestionTime = Nothing
    }
{-# INLINE mkOutputLogEvent #-}

-- | A point in time expressed as the number milliseconds since Jan 1, 1970
-- 00:00:00 UTC.
oleTimestamp :: Lens' OutputLogEvent (Maybe Integer)
oleTimestamp = lens _oleTimestamp (\s a -> s { _oleTimestamp = a })
{-# INLINE oleTimestamp #-}

oleMessage :: Lens' OutputLogEvent (Maybe Text)
oleMessage = lens _oleMessage (\s a -> s { _oleMessage = a })
{-# INLINE oleMessage #-}

-- | A point in time expressed as the number milliseconds since Jan 1, 1970
-- 00:00:00 UTC.
oleIngestionTime :: Lens' OutputLogEvent (Maybe Integer)
oleIngestionTime =
    lens _oleIngestionTime (\s a -> s { _oleIngestionTime = a })
{-# INLINE oleIngestionTime #-}

instance FromJSON OutputLogEvent
