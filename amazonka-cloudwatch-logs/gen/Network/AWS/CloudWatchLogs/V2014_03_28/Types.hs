{-# LANGUAGE DeriveDataTypeable          #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE StandaloneDeriving          #-}
{-# LANGUAGE TemplateHaskell             #-}
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
    ( module Network.AWS.CloudWatchLogs.V2014_03_28.Types
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
    { _ilfMessage :: Text
    , _ilfTimestamp :: Integer
      -- ^ A point in time expressed as the number milliseconds since Jan 1,
      -- 1970 00:00:00 UTC.
    } deriving (Show, Generic)

instance ToJSON InputLogEvent

data LogGroup = LogGroup
    { _lhArn :: Maybe Text
    , _lhCreationTime :: Maybe Integer
      -- ^ A point in time expressed as the number milliseconds since Jan 1,
      -- 1970 00:00:00 UTC.
    , _lhLogGroupName :: Maybe Text
    , _lhMetricFilterCount :: Maybe Integer
      -- ^ The number of metric filters associated with the log group.
    , _lhRetentionInDays :: Maybe Integer
      -- ^ Specifies the number of days you want to retain log events in the
      -- specified log group. Possible values are: 1, 3, 5, 7, 14, 30, 60,
      -- 90, 120, 150, 180, 365, 400, 547, 730.
    , _lhStoredBytes :: Maybe Integer
    } deriving (Show, Generic)

instance FromJSON LogGroup

-- | A log stream is sequence of log events that share the same emitter.
data LogStream = LogStream
    { _ltArn :: Maybe Text
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
    , _ltLogStreamName :: Maybe Text
    , _ltStoredBytes :: Maybe Integer
    , _ltUploadSequenceToken :: Maybe Text
      -- ^ A string token used for making PutLogEvents requests. A
      -- sequenceToken can only be used once, and PutLogEvents requests
      -- must include the sequenceToken obtained from the response of the
      -- previous request.
    } deriving (Show, Generic)

instance FromJSON LogStream

-- | Metric filters can be used to express how Amazon CloudWatch Logs would
-- extract metric observations from ingested log events and transform them to
-- metric data in a CloudWatch metric.
data MetricFilter = MetricFilter
    { _mgCreationTime :: Maybe Integer
      -- ^ A point in time expressed as the number milliseconds since Jan 1,
      -- 1970 00:00:00 UTC.
    , _mgFilterName :: Maybe Text
      -- ^ The name of the metric filter.
    , _mgFilterPattern :: Maybe Text
    , _mgMetricTransformations :: Maybe [MetricTransformation]
    } deriving (Show, Generic)

instance FromJSON MetricFilter

data MetricFilterMatchRecord = MetricFilterMatchRecord
    { _mfmrEventMessage :: Maybe Text
    , _mfmrEventNumber :: Maybe Integer
    , _mfmrExtractedValues :: Map Text Text
    } deriving (Show, Generic)

instance FromJSON MetricFilterMatchRecord

data MetricTransformation = MetricTransformation
    { _muMetricName :: Text
    , _muMetricNamespace :: Text
    , _muMetricValue :: Text
    } deriving (Show, Generic)

instance FromJSON MetricTransformation

instance ToJSON MetricTransformation

data OutputLogEvent = OutputLogEvent
    { _olfIngestionTime :: Maybe Integer
      -- ^ A point in time expressed as the number milliseconds since Jan 1,
      -- 1970 00:00:00 UTC.
    , _olfMessage :: Maybe Text
    , _olfTimestamp :: Maybe Integer
      -- ^ A point in time expressed as the number milliseconds since Jan 1,
      -- 1970 00:00:00 UTC.
    } deriving (Show, Generic)

instance FromJSON OutputLogEvent

makeLenses ''InputLogEvent
makeLenses ''LogGroup
makeLenses ''LogStream
makeLenses ''MetricFilter
makeLenses ''MetricFilterMatchRecord
makeLenses ''MetricTransformation
makeLenses ''OutputLogEvent
