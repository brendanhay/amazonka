{-# LANGUAGE DeriveDataTypeable          #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudWatchLogs.Types
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

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

    -- * InputLogEvent
    , InputLogEvent
    , inputLogEvent
    , ileMessage
    , ileTimestamp

    -- * OutputLogEvent
    , OutputLogEvent
    , outputLogEvent
    , oleIngestionTime
    , oleMessage
    , oleTimestamp
    ) where

import Network.AWS.Error
import Network.AWS.Prelude
import Network.AWS.Signing.V4

-- | Supported version (@2014-03-28@) of the Amazon CloudWatch Logs.
data CloudWatchLogs deriving (Typeable)

instance AWSService CloudWatchLogs where
    type Sg CloudWatchLogs = V4
    type Er CloudWatchLogs = JSONError

    service = Service
        { _svcEndpoint = regional
        , _svcAbbrev   = "CloudWatchLogs"
        , _svcPrefix   = "logs"
        , _svcVersion  = "2014-03-28"
        , _svcTarget   = Nothing
        }

    handle = jsonError alwaysFail

data MetricFilter = MetricFilter
    { _mfCreationTime          :: Maybe Natural
    , _mfFilterName            :: Maybe Text
    , _mfFilterPattern         :: Maybe Text
    , _mfMetricTransformations :: List1 MetricTransformation
    } deriving (Eq, Show, Generic)

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
mfCreationTime = lens _mfCreationTime (\s a -> s { _mfCreationTime = a })

mfFilterName :: Lens' MetricFilter (Maybe Text)
mfFilterName = lens _mfFilterName (\s a -> s { _mfFilterName = a })

mfFilterPattern :: Lens' MetricFilter (Maybe Text)
mfFilterPattern = lens _mfFilterPattern (\s a -> s { _mfFilterPattern = a })

mfMetricTransformations :: Lens' MetricFilter (NonEmpty MetricTransformation)
mfMetricTransformations =
    lens _mfMetricTransformations (\s a -> s { _mfMetricTransformations = a })
        . _List1

data MetricFilterMatchRecord = MetricFilterMatchRecord
    { _mfmrEventMessage    :: Maybe Text
    , _mfmrEventNumber     :: Maybe Integer
    , _mfmrExtractedValues :: Map Text Text
    } deriving (Eq, Show, Generic)

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

data MetricTransformation = MetricTransformation
    { _mtMetricName      :: Text
    , _mtMetricNamespace :: Text
    , _mtMetricValue     :: Text
    } deriving (Eq, Ord, Show, Generic)

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

data LogStream = LogStream
    { _lsArn                 :: Maybe Text
    , _lsCreationTime        :: Maybe Natural
    , _lsFirstEventTimestamp :: Maybe Natural
    , _lsLastEventTimestamp  :: Maybe Natural
    , _lsLastIngestionTime   :: Maybe Natural
    , _lsLogStreamName       :: Maybe Text
    , _lsStoredBytes         :: Maybe Natural
    , _lsUploadSequenceToken :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

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
lsCreationTime = lens _lsCreationTime (\s a -> s { _lsCreationTime = a })

lsFirstEventTimestamp :: Lens' LogStream (Maybe Natural)
lsFirstEventTimestamp =
    lens _lsFirstEventTimestamp (\s a -> s { _lsFirstEventTimestamp = a })

lsLastEventTimestamp :: Lens' LogStream (Maybe Natural)
lsLastEventTimestamp =
    lens _lsLastEventTimestamp (\s a -> s { _lsLastEventTimestamp = a })

lsLastIngestionTime :: Lens' LogStream (Maybe Natural)
lsLastIngestionTime =
    lens _lsLastIngestionTime (\s a -> s { _lsLastIngestionTime = a })

lsLogStreamName :: Lens' LogStream (Maybe Text)
lsLogStreamName = lens _lsLogStreamName (\s a -> s { _lsLogStreamName = a })

lsStoredBytes :: Lens' LogStream (Maybe Natural)
lsStoredBytes = lens _lsStoredBytes (\s a -> s { _lsStoredBytes = a })

lsUploadSequenceToken :: Lens' LogStream (Maybe Text)
lsUploadSequenceToken =
    lens _lsUploadSequenceToken (\s a -> s { _lsUploadSequenceToken = a })

data LogGroup = LogGroup
    { _lgArn               :: Maybe Text
    , _lgCreationTime      :: Maybe Natural
    , _lgLogGroupName      :: Maybe Text
    , _lgMetricFilterCount :: Maybe Int
    , _lgRetentionInDays   :: Maybe Int
    , _lgStoredBytes       :: Maybe Natural
    } deriving (Eq, Ord, Show, Generic)

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
lgCreationTime = lens _lgCreationTime (\s a -> s { _lgCreationTime = a })

lgLogGroupName :: Lens' LogGroup (Maybe Text)
lgLogGroupName = lens _lgLogGroupName (\s a -> s { _lgLogGroupName = a })

lgMetricFilterCount :: Lens' LogGroup (Maybe Int)
lgMetricFilterCount =
    lens _lgMetricFilterCount (\s a -> s { _lgMetricFilterCount = a })

lgRetentionInDays :: Lens' LogGroup (Maybe Int)
lgRetentionInDays =
    lens _lgRetentionInDays (\s a -> s { _lgRetentionInDays = a })

lgStoredBytes :: Lens' LogGroup (Maybe Natural)
lgStoredBytes = lens _lgStoredBytes (\s a -> s { _lgStoredBytes = a })

data InputLogEvent = InputLogEvent
    { _ileMessage   :: Text
    , _ileTimestamp :: Natural
    } deriving (Eq, Ord, Show, Generic)

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
    { _ileTimestamp = p1
    , _ileMessage   = p2
    }

ileMessage :: Lens' InputLogEvent Text
ileMessage = lens _ileMessage (\s a -> s { _ileMessage = a })

ileTimestamp :: Lens' InputLogEvent Natural
ileTimestamp = lens _ileTimestamp (\s a -> s { _ileTimestamp = a })

data OutputLogEvent = OutputLogEvent
    { _oleIngestionTime :: Maybe Natural
    , _oleMessage       :: Maybe Text
    , _oleTimestamp     :: Maybe Natural
    } deriving (Eq, Ord, Show, Generic)

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
oleIngestionTime = lens _oleIngestionTime (\s a -> s { _oleIngestionTime = a })

oleMessage :: Lens' OutputLogEvent (Maybe Text)
oleMessage = lens _oleMessage (\s a -> s { _oleMessage = a })

oleTimestamp :: Lens' OutputLogEvent (Maybe Natural)
oleTimestamp = lens _oleTimestamp (\s a -> s { _oleTimestamp = a })
