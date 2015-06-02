{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}
{-# LANGUAGE ViewPatterns                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudWatchLogs.Types
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

module Network.AWS.CloudWatchLogs.Types
    (
    -- * Service
      CloudWatchLogs
    -- ** Error
    , JSONError

    -- * MetricFilter
    , MetricFilter
    , metricFilter
    , mfCreationTime
    , mfFilterName
    , mfFilterPattern
    , mfMetricTransformations

    -- * SearchedLogStream
    , SearchedLogStream
    , searchedLogStream
    , slsLogStreamName
    , slsSearchedCompletely

    -- * MetricFilterMatchRecord
    , MetricFilterMatchRecord
    , metricFilterMatchRecord
    , mfmrEventMessage
    , mfmrEventNumber
    , mfmrExtractedValues

    -- * MetricTransformation
    , MetricTransformation
    , metricTransformation
    , mtMetricName
    , mtMetricNamespace
    , mtMetricValue

    -- * OrderBy
    , OrderBy (..)

    -- * LogStream
    , LogStream
    , logStream
    , lsArn
    , lsCreationTime
    , lsFirstEventTimestamp
    , lsLastEventTimestamp
    , lsLastIngestionTime
    , lsLogStreamName
    , lsStoredBytes
    , lsUploadSequenceToken

    -- * LogGroup
    , LogGroup
    , logGroup
    , lgArn
    , lgCreationTime
    , lgLogGroupName
    , lgMetricFilterCount
    , lgRetentionInDays
    , lgStoredBytes

    -- * RejectedLogEventsInfo
    , RejectedLogEventsInfo
    , rejectedLogEventsInfo
    , rleiExpiredLogEventEndIndex
    , rleiTooNewLogEventStartIndex
    , rleiTooOldLogEventEndIndex

    -- * InputLogEvent
    , InputLogEvent
    , inputLogEvent
    , ileMessage
    , ileTimestamp

    -- * FilteredLogEvent
    , FilteredLogEvent
    , filteredLogEvent
    , fleEventId
    , fleIngestionTime
    , fleLogStreamName
    , fleMessage
    , fleTimestamp

    -- * OutputLogEvent
    , OutputLogEvent
    , outputLogEvent
    , oleIngestionTime
    , oleMessage
    , oleTimestamp
    ) where

import Network.AWS.Prelude
import Network.AWS.Signing
import qualified GHC.Exts

-- | Version @2014-03-28@ of the Amazon CloudWatch Logs service.
data CloudWatchLogs

instance AWSService CloudWatchLogs where
    type Sg CloudWatchLogs = V4
    type Er CloudWatchLogs = JSONError

    service = service'
      where
        service' :: Service CloudWatchLogs
        service' = Service
            { _svcAbbrev       = "CloudWatchLogs"
            , _svcPrefix       = "logs"
            , _svcVersion      = "2014-03-28"
            , _svcTargetPrefix = Just "Logs_20140328"
            , _svcJSONVersion  = Just "1.1"
            , _svcHandle       = handle
            , _svcRetry        = retry
            }

        handle :: Status
               -> Maybe (LazyByteString -> ServiceError JSONError)
        handle = jsonError statusSuccess service'

        retry :: Retry CloudWatchLogs
        retry = Exponential
            { _retryBase     = 0.05
            , _retryGrowth   = 2
            , _retryAttempts = 5
            , _retryCheck    = check
            }

        check :: Status
              -> JSONError
              -> Bool
        check (statusCode -> s) (awsErrorCode -> e)
            | s == 500  = True -- General Server Error
            | s == 509  = True -- Limit Exceeded
            | s == 503  = True -- Service Unavailable
            | otherwise = False

data MetricFilter = MetricFilter
    { _mfCreationTime          :: Maybe Nat
    , _mfFilterName            :: Maybe Text
    , _mfFilterPattern         :: Maybe Text
    , _mfMetricTransformations :: List1 "metricTransformations" MetricTransformation
    } deriving (Eq, Read, Show)

-- | 'MetricFilter' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'mfCreationTime' @::@ 'Maybe' 'Natural'
--
-- * 'mfFilterName' @::@ 'Maybe' 'Text'
--
-- * 'mfFilterPattern' @::@ 'Maybe' 'Text'
--
-- * 'mfMetricTransformations' @::@ 'NonEmpty' 'MetricTransformation'
--
metricFilter :: NonEmpty MetricTransformation -- ^ 'mfMetricTransformations'
             -> MetricFilter
metricFilter p1 = MetricFilter
    { _mfMetricTransformations = withIso _List1 (const id) p1
    , _mfFilterName            = Nothing
    , _mfFilterPattern         = Nothing
    , _mfCreationTime          = Nothing
    }

mfCreationTime :: Lens' MetricFilter (Maybe Natural)
mfCreationTime = lens _mfCreationTime (\s a -> s { _mfCreationTime = a }) . mapping _Nat

mfFilterName :: Lens' MetricFilter (Maybe Text)
mfFilterName = lens _mfFilterName (\s a -> s { _mfFilterName = a })

mfFilterPattern :: Lens' MetricFilter (Maybe Text)
mfFilterPattern = lens _mfFilterPattern (\s a -> s { _mfFilterPattern = a })

mfMetricTransformations :: Lens' MetricFilter (NonEmpty MetricTransformation)
mfMetricTransformations =
    lens _mfMetricTransformations (\s a -> s { _mfMetricTransformations = a })
        . _List1

instance FromJSON MetricFilter where
    parseJSON = withObject "MetricFilter" $ \o -> MetricFilter
        <$> o .:? "creationTime"
        <*> o .:? "filterName"
        <*> o .:? "filterPattern"
        <*> o .:  "metricTransformations"

instance ToJSON MetricFilter where
    toJSON MetricFilter{..} = object
        [ "filterName"            .= _mfFilterName
        , "filterPattern"         .= _mfFilterPattern
        , "metricTransformations" .= _mfMetricTransformations
        , "creationTime"          .= _mfCreationTime
        ]

data SearchedLogStream = SearchedLogStream
    { _slsLogStreamName      :: Maybe Text
    , _slsSearchedCompletely :: Maybe Bool
    } deriving (Eq, Ord, Read, Show)

-- | 'SearchedLogStream' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'slsLogStreamName' @::@ 'Maybe' 'Text'
--
-- * 'slsSearchedCompletely' @::@ 'Maybe' 'Bool'
--
searchedLogStream :: SearchedLogStream
searchedLogStream = SearchedLogStream
    { _slsLogStreamName      = Nothing
    , _slsSearchedCompletely = Nothing
    }

-- | The name of the log stream.
slsLogStreamName :: Lens' SearchedLogStream (Maybe Text)
slsLogStreamName = lens _slsLogStreamName (\s a -> s { _slsLogStreamName = a })

-- | Indicates whether all the events in this log stream were searched or more
-- data exists to search by paginating further.
slsSearchedCompletely :: Lens' SearchedLogStream (Maybe Bool)
slsSearchedCompletely =
    lens _slsSearchedCompletely (\s a -> s { _slsSearchedCompletely = a })

instance FromJSON SearchedLogStream where
    parseJSON = withObject "SearchedLogStream" $ \o -> SearchedLogStream
        <$> o .:? "logStreamName"
        <*> o .:? "searchedCompletely"

instance ToJSON SearchedLogStream where
    toJSON SearchedLogStream{..} = object
        [ "logStreamName"      .= _slsLogStreamName
        , "searchedCompletely" .= _slsSearchedCompletely
        ]

data MetricFilterMatchRecord = MetricFilterMatchRecord
    { _mfmrEventMessage    :: Maybe Text
    , _mfmrEventNumber     :: Maybe Integer
    , _mfmrExtractedValues :: Map Text Text
    } deriving (Eq, Read, Show)

-- | 'MetricFilterMatchRecord' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'mfmrEventMessage' @::@ 'Maybe' 'Text'
--
-- * 'mfmrEventNumber' @::@ 'Maybe' 'Integer'
--
-- * 'mfmrExtractedValues' @::@ 'HashMap' 'Text' 'Text'
--
metricFilterMatchRecord :: MetricFilterMatchRecord
metricFilterMatchRecord = MetricFilterMatchRecord
    { _mfmrEventNumber     = Nothing
    , _mfmrEventMessage    = Nothing
    , _mfmrExtractedValues = mempty
    }

mfmrEventMessage :: Lens' MetricFilterMatchRecord (Maybe Text)
mfmrEventMessage = lens _mfmrEventMessage (\s a -> s { _mfmrEventMessage = a })

mfmrEventNumber :: Lens' MetricFilterMatchRecord (Maybe Integer)
mfmrEventNumber = lens _mfmrEventNumber (\s a -> s { _mfmrEventNumber = a })

mfmrExtractedValues :: Lens' MetricFilterMatchRecord (HashMap Text Text)
mfmrExtractedValues =
    lens _mfmrExtractedValues (\s a -> s { _mfmrExtractedValues = a })
        . _Map

instance FromJSON MetricFilterMatchRecord where
    parseJSON = withObject "MetricFilterMatchRecord" $ \o -> MetricFilterMatchRecord
        <$> o .:? "eventMessage"
        <*> o .:? "eventNumber"
        <*> o .:? "extractedValues" .!= mempty

instance ToJSON MetricFilterMatchRecord where
    toJSON MetricFilterMatchRecord{..} = object
        [ "eventNumber"     .= _mfmrEventNumber
        , "eventMessage"    .= _mfmrEventMessage
        , "extractedValues" .= _mfmrExtractedValues
        ]

data MetricTransformation = MetricTransformation
    { _mtMetricName      :: Text
    , _mtMetricNamespace :: Text
    , _mtMetricValue     :: Text
    } deriving (Eq, Ord, Read, Show)

-- | 'MetricTransformation' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'mtMetricName' @::@ 'Text'
--
-- * 'mtMetricNamespace' @::@ 'Text'
--
-- * 'mtMetricValue' @::@ 'Text'
--
metricTransformation :: Text -- ^ 'mtMetricName'
                     -> Text -- ^ 'mtMetricNamespace'
                     -> Text -- ^ 'mtMetricValue'
                     -> MetricTransformation
metricTransformation p1 p2 p3 = MetricTransformation
    { _mtMetricName      = p1
    , _mtMetricNamespace = p2
    , _mtMetricValue     = p3
    }

mtMetricName :: Lens' MetricTransformation Text
mtMetricName = lens _mtMetricName (\s a -> s { _mtMetricName = a })

mtMetricNamespace :: Lens' MetricTransformation Text
mtMetricNamespace =
    lens _mtMetricNamespace (\s a -> s { _mtMetricNamespace = a })

mtMetricValue :: Lens' MetricTransformation Text
mtMetricValue = lens _mtMetricValue (\s a -> s { _mtMetricValue = a })

instance FromJSON MetricTransformation where
    parseJSON = withObject "MetricTransformation" $ \o -> MetricTransformation
        <$> o .:  "metricName"
        <*> o .:  "metricNamespace"
        <*> o .:  "metricValue"

instance ToJSON MetricTransformation where
    toJSON MetricTransformation{..} = object
        [ "metricName"      .= _mtMetricName
        , "metricNamespace" .= _mtMetricNamespace
        , "metricValue"     .= _mtMetricValue
        ]

data OrderBy
    = OBLastEventTime -- ^ LastEventTime
    | OBLogStreamName -- ^ LogStreamName
      deriving (Eq, Ord, Read, Show, Generic, Enum)

instance Hashable OrderBy

instance FromText OrderBy where
    parser = takeLowerText >>= \case
        "lasteventtime" -> pure OBLastEventTime
        "logstreamname" -> pure OBLogStreamName
        e               -> fail $
            "Failure parsing OrderBy from " ++ show e

instance ToText OrderBy where
    toText = \case
        OBLastEventTime -> "LastEventTime"
        OBLogStreamName -> "LogStreamName"

instance ToByteString OrderBy
instance ToHeader     OrderBy
instance ToQuery      OrderBy

instance FromJSON OrderBy where
    parseJSON = parseJSONText "OrderBy"

instance ToJSON OrderBy where
    toJSON = toJSONText

data LogStream = LogStream
    { _lsArn                 :: Maybe Text
    , _lsCreationTime        :: Maybe Nat
    , _lsFirstEventTimestamp :: Maybe Nat
    , _lsLastEventTimestamp  :: Maybe Nat
    , _lsLastIngestionTime   :: Maybe Nat
    , _lsLogStreamName       :: Maybe Text
    , _lsStoredBytes         :: Maybe Nat
    , _lsUploadSequenceToken :: Maybe Text
    } deriving (Eq, Ord, Read, Show)

-- | 'LogStream' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lsArn' @::@ 'Maybe' 'Text'
--
-- * 'lsCreationTime' @::@ 'Maybe' 'Natural'
--
-- * 'lsFirstEventTimestamp' @::@ 'Maybe' 'Natural'
--
-- * 'lsLastEventTimestamp' @::@ 'Maybe' 'Natural'
--
-- * 'lsLastIngestionTime' @::@ 'Maybe' 'Natural'
--
-- * 'lsLogStreamName' @::@ 'Maybe' 'Text'
--
-- * 'lsStoredBytes' @::@ 'Maybe' 'Natural'
--
-- * 'lsUploadSequenceToken' @::@ 'Maybe' 'Text'
--
logStream :: LogStream
logStream = LogStream
    { _lsLogStreamName       = Nothing
    , _lsCreationTime        = Nothing
    , _lsFirstEventTimestamp = Nothing
    , _lsLastEventTimestamp  = Nothing
    , _lsLastIngestionTime   = Nothing
    , _lsUploadSequenceToken = Nothing
    , _lsArn                 = Nothing
    , _lsStoredBytes         = Nothing
    }

lsArn :: Lens' LogStream (Maybe Text)
lsArn = lens _lsArn (\s a -> s { _lsArn = a })

lsCreationTime :: Lens' LogStream (Maybe Natural)
lsCreationTime = lens _lsCreationTime (\s a -> s { _lsCreationTime = a }) . mapping _Nat

lsFirstEventTimestamp :: Lens' LogStream (Maybe Natural)
lsFirstEventTimestamp =
    lens _lsFirstEventTimestamp (\s a -> s { _lsFirstEventTimestamp = a })
        . mapping _Nat

lsLastEventTimestamp :: Lens' LogStream (Maybe Natural)
lsLastEventTimestamp =
    lens _lsLastEventTimestamp (\s a -> s { _lsLastEventTimestamp = a })
        . mapping _Nat

lsLastIngestionTime :: Lens' LogStream (Maybe Natural)
lsLastIngestionTime =
    lens _lsLastIngestionTime (\s a -> s { _lsLastIngestionTime = a })
        . mapping _Nat

lsLogStreamName :: Lens' LogStream (Maybe Text)
lsLogStreamName = lens _lsLogStreamName (\s a -> s { _lsLogStreamName = a })

lsStoredBytes :: Lens' LogStream (Maybe Natural)
lsStoredBytes = lens _lsStoredBytes (\s a -> s { _lsStoredBytes = a }) . mapping _Nat

lsUploadSequenceToken :: Lens' LogStream (Maybe Text)
lsUploadSequenceToken =
    lens _lsUploadSequenceToken (\s a -> s { _lsUploadSequenceToken = a })

instance FromJSON LogStream where
    parseJSON = withObject "LogStream" $ \o -> LogStream
        <$> o .:? "arn"
        <*> o .:? "creationTime"
        <*> o .:? "firstEventTimestamp"
        <*> o .:? "lastEventTimestamp"
        <*> o .:? "lastIngestionTime"
        <*> o .:? "logStreamName"
        <*> o .:? "storedBytes"
        <*> o .:? "uploadSequenceToken"

instance ToJSON LogStream where
    toJSON LogStream{..} = object
        [ "logStreamName"       .= _lsLogStreamName
        , "creationTime"        .= _lsCreationTime
        , "firstEventTimestamp" .= _lsFirstEventTimestamp
        , "lastEventTimestamp"  .= _lsLastEventTimestamp
        , "lastIngestionTime"   .= _lsLastIngestionTime
        , "uploadSequenceToken" .= _lsUploadSequenceToken
        , "arn"                 .= _lsArn
        , "storedBytes"         .= _lsStoredBytes
        ]

data LogGroup = LogGroup
    { _lgArn               :: Maybe Text
    , _lgCreationTime      :: Maybe Nat
    , _lgLogGroupName      :: Maybe Text
    , _lgMetricFilterCount :: Maybe Int
    , _lgRetentionInDays   :: Maybe Int
    , _lgStoredBytes       :: Maybe Nat
    } deriving (Eq, Ord, Read, Show)

-- | 'LogGroup' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lgArn' @::@ 'Maybe' 'Text'
--
-- * 'lgCreationTime' @::@ 'Maybe' 'Natural'
--
-- * 'lgLogGroupName' @::@ 'Maybe' 'Text'
--
-- * 'lgMetricFilterCount' @::@ 'Maybe' 'Int'
--
-- * 'lgRetentionInDays' @::@ 'Maybe' 'Int'
--
-- * 'lgStoredBytes' @::@ 'Maybe' 'Natural'
--
logGroup :: LogGroup
logGroup = LogGroup
    { _lgLogGroupName      = Nothing
    , _lgCreationTime      = Nothing
    , _lgRetentionInDays   = Nothing
    , _lgMetricFilterCount = Nothing
    , _lgArn               = Nothing
    , _lgStoredBytes       = Nothing
    }

lgArn :: Lens' LogGroup (Maybe Text)
lgArn = lens _lgArn (\s a -> s { _lgArn = a })

lgCreationTime :: Lens' LogGroup (Maybe Natural)
lgCreationTime = lens _lgCreationTime (\s a -> s { _lgCreationTime = a }) . mapping _Nat

lgLogGroupName :: Lens' LogGroup (Maybe Text)
lgLogGroupName = lens _lgLogGroupName (\s a -> s { _lgLogGroupName = a })

lgMetricFilterCount :: Lens' LogGroup (Maybe Int)
lgMetricFilterCount =
    lens _lgMetricFilterCount (\s a -> s { _lgMetricFilterCount = a })

lgRetentionInDays :: Lens' LogGroup (Maybe Int)
lgRetentionInDays =
    lens _lgRetentionInDays (\s a -> s { _lgRetentionInDays = a })

lgStoredBytes :: Lens' LogGroup (Maybe Natural)
lgStoredBytes = lens _lgStoredBytes (\s a -> s { _lgStoredBytes = a }) . mapping _Nat

instance FromJSON LogGroup where
    parseJSON = withObject "LogGroup" $ \o -> LogGroup
        <$> o .:? "arn"
        <*> o .:? "creationTime"
        <*> o .:? "logGroupName"
        <*> o .:? "metricFilterCount"
        <*> o .:? "retentionInDays"
        <*> o .:? "storedBytes"

instance ToJSON LogGroup where
    toJSON LogGroup{..} = object
        [ "logGroupName"      .= _lgLogGroupName
        , "creationTime"      .= _lgCreationTime
        , "retentionInDays"   .= _lgRetentionInDays
        , "metricFilterCount" .= _lgMetricFilterCount
        , "arn"               .= _lgArn
        , "storedBytes"       .= _lgStoredBytes
        ]

data RejectedLogEventsInfo = RejectedLogEventsInfo
    { _rleiExpiredLogEventEndIndex  :: Maybe Int
    , _rleiTooNewLogEventStartIndex :: Maybe Int
    , _rleiTooOldLogEventEndIndex   :: Maybe Int
    } deriving (Eq, Ord, Read, Show)

-- | 'RejectedLogEventsInfo' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rleiExpiredLogEventEndIndex' @::@ 'Maybe' 'Int'
--
-- * 'rleiTooNewLogEventStartIndex' @::@ 'Maybe' 'Int'
--
-- * 'rleiTooOldLogEventEndIndex' @::@ 'Maybe' 'Int'
--
rejectedLogEventsInfo :: RejectedLogEventsInfo
rejectedLogEventsInfo = RejectedLogEventsInfo
    { _rleiTooNewLogEventStartIndex = Nothing
    , _rleiTooOldLogEventEndIndex   = Nothing
    , _rleiExpiredLogEventEndIndex  = Nothing
    }

rleiExpiredLogEventEndIndex :: Lens' RejectedLogEventsInfo (Maybe Int)
rleiExpiredLogEventEndIndex =
    lens _rleiExpiredLogEventEndIndex
        (\s a -> s { _rleiExpiredLogEventEndIndex = a })

rleiTooNewLogEventStartIndex :: Lens' RejectedLogEventsInfo (Maybe Int)
rleiTooNewLogEventStartIndex =
    lens _rleiTooNewLogEventStartIndex
        (\s a -> s { _rleiTooNewLogEventStartIndex = a })

rleiTooOldLogEventEndIndex :: Lens' RejectedLogEventsInfo (Maybe Int)
rleiTooOldLogEventEndIndex =
    lens _rleiTooOldLogEventEndIndex
        (\s a -> s { _rleiTooOldLogEventEndIndex = a })

instance FromJSON RejectedLogEventsInfo where
    parseJSON = withObject "RejectedLogEventsInfo" $ \o -> RejectedLogEventsInfo
        <$> o .:? "expiredLogEventEndIndex"
        <*> o .:? "tooNewLogEventStartIndex"
        <*> o .:? "tooOldLogEventEndIndex"

instance ToJSON RejectedLogEventsInfo where
    toJSON RejectedLogEventsInfo{..} = object
        [ "tooNewLogEventStartIndex" .= _rleiTooNewLogEventStartIndex
        , "tooOldLogEventEndIndex"   .= _rleiTooOldLogEventEndIndex
        , "expiredLogEventEndIndex"  .= _rleiExpiredLogEventEndIndex
        ]

data InputLogEvent = InputLogEvent
    { _ileMessage   :: Text
    , _ileTimestamp :: Nat
    } deriving (Eq, Ord, Read, Show)

-- | 'InputLogEvent' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ileMessage' @::@ 'Text'
--
-- * 'ileTimestamp' @::@ 'Natural'
--
inputLogEvent :: Natural -- ^ 'ileTimestamp'
              -> Text -- ^ 'ileMessage'
              -> InputLogEvent
inputLogEvent p1 p2 = InputLogEvent
    { _ileTimestamp = withIso _Nat (const id) p1
    , _ileMessage   = p2
    }

ileMessage :: Lens' InputLogEvent Text
ileMessage = lens _ileMessage (\s a -> s { _ileMessage = a })

ileTimestamp :: Lens' InputLogEvent Natural
ileTimestamp = lens _ileTimestamp (\s a -> s { _ileTimestamp = a }) . _Nat

instance FromJSON InputLogEvent where
    parseJSON = withObject "InputLogEvent" $ \o -> InputLogEvent
        <$> o .:  "message"
        <*> o .:  "timestamp"

instance ToJSON InputLogEvent where
    toJSON InputLogEvent{..} = object
        [ "timestamp" .= _ileTimestamp
        , "message"   .= _ileMessage
        ]

data FilteredLogEvent = FilteredLogEvent
    { _fleEventId       :: Maybe Text
    , _fleIngestionTime :: Maybe Nat
    , _fleLogStreamName :: Maybe Text
    , _fleMessage       :: Maybe Text
    , _fleTimestamp     :: Maybe Nat
    } deriving (Eq, Ord, Read, Show)

-- | 'FilteredLogEvent' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'fleEventId' @::@ 'Maybe' 'Text'
--
-- * 'fleIngestionTime' @::@ 'Maybe' 'Natural'
--
-- * 'fleLogStreamName' @::@ 'Maybe' 'Text'
--
-- * 'fleMessage' @::@ 'Maybe' 'Text'
--
-- * 'fleTimestamp' @::@ 'Maybe' 'Natural'
--
filteredLogEvent :: FilteredLogEvent
filteredLogEvent = FilteredLogEvent
    { _fleLogStreamName = Nothing
    , _fleTimestamp     = Nothing
    , _fleMessage       = Nothing
    , _fleIngestionTime = Nothing
    , _fleEventId       = Nothing
    }

-- | A unique identifier for this event.
fleEventId :: Lens' FilteredLogEvent (Maybe Text)
fleEventId = lens _fleEventId (\s a -> s { _fleEventId = a })

fleIngestionTime :: Lens' FilteredLogEvent (Maybe Natural)
fleIngestionTime = lens _fleIngestionTime (\s a -> s { _fleIngestionTime = a }) . mapping _Nat

-- | The name of the log stream this event belongs to.
fleLogStreamName :: Lens' FilteredLogEvent (Maybe Text)
fleLogStreamName = lens _fleLogStreamName (\s a -> s { _fleLogStreamName = a })

fleMessage :: Lens' FilteredLogEvent (Maybe Text)
fleMessage = lens _fleMessage (\s a -> s { _fleMessage = a })

fleTimestamp :: Lens' FilteredLogEvent (Maybe Natural)
fleTimestamp = lens _fleTimestamp (\s a -> s { _fleTimestamp = a }) . mapping _Nat

instance FromJSON FilteredLogEvent where
    parseJSON = withObject "FilteredLogEvent" $ \o -> FilteredLogEvent
        <$> o .:? "eventId"
        <*> o .:? "ingestionTime"
        <*> o .:? "logStreamName"
        <*> o .:? "message"
        <*> o .:? "timestamp"

instance ToJSON FilteredLogEvent where
    toJSON FilteredLogEvent{..} = object
        [ "logStreamName" .= _fleLogStreamName
        , "timestamp"     .= _fleTimestamp
        , "message"       .= _fleMessage
        , "ingestionTime" .= _fleIngestionTime
        , "eventId"       .= _fleEventId
        ]

data OutputLogEvent = OutputLogEvent
    { _oleIngestionTime :: Maybe Nat
    , _oleMessage       :: Maybe Text
    , _oleTimestamp     :: Maybe Nat
    } deriving (Eq, Ord, Read, Show)

-- | 'OutputLogEvent' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'oleIngestionTime' @::@ 'Maybe' 'Natural'
--
-- * 'oleMessage' @::@ 'Maybe' 'Text'
--
-- * 'oleTimestamp' @::@ 'Maybe' 'Natural'
--
outputLogEvent :: OutputLogEvent
outputLogEvent = OutputLogEvent
    { _oleTimestamp     = Nothing
    , _oleMessage       = Nothing
    , _oleIngestionTime = Nothing
    }

oleIngestionTime :: Lens' OutputLogEvent (Maybe Natural)
oleIngestionTime = lens _oleIngestionTime (\s a -> s { _oleIngestionTime = a }) . mapping _Nat

oleMessage :: Lens' OutputLogEvent (Maybe Text)
oleMessage = lens _oleMessage (\s a -> s { _oleMessage = a })

oleTimestamp :: Lens' OutputLogEvent (Maybe Natural)
oleTimestamp = lens _oleTimestamp (\s a -> s { _oleTimestamp = a }) . mapping _Nat

instance FromJSON OutputLogEvent where
    parseJSON = withObject "OutputLogEvent" $ \o -> OutputLogEvent
        <$> o .:? "ingestionTime"
        <*> o .:? "message"
        <*> o .:? "timestamp"

instance ToJSON OutputLogEvent where
    toJSON OutputLogEvent{..} = object
        [ "timestamp"     .= _oleTimestamp
        , "message"       .= _oleMessage
        , "ingestionTime" .= _oleIngestionTime
        ]
