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

-- Module      : Network.AWS.CloudTrail.Types
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

module Network.AWS.CloudTrail.Types
    (
    -- * Service
      CloudTrail
    -- ** Error
    , JSONError

    -- * Event
    , Event
    , event
    , eCloudTrailEvent
    , eEventId
    , eEventName
    , eEventTime
    , eResources
    , eUsername

    -- * Trail
    , Trail
    , trail
    , tCloudWatchLogsLogGroupArn
    , tCloudWatchLogsRoleArn
    , tIncludeGlobalServiceEvents
    , tName
    , tS3BucketName
    , tS3KeyPrefix
    , tSnsTopicName

    -- * LookupAttribute
    , LookupAttribute
    , lookupAttribute
    , laAttributeKey
    , laAttributeValue

    -- * LookupAttributeKey
    , LookupAttributeKey (..)

    -- * Resource
    , Resource
    , resource
    , rResourceName
    , rResourceType
    ) where

import Network.AWS.Prelude
import Network.AWS.Signing
import qualified GHC.Exts

-- | Version @2013-11-01@ of the Amazon CloudTrail service.
data CloudTrail

instance AWSService CloudTrail where
    type Sg CloudTrail = V4
    type Er CloudTrail = JSONError

    service = service'
      where
        service' :: Service CloudTrail
        service' = Service
            { _svcAbbrev       = "CloudTrail"
            , _svcPrefix       = "cloudtrail"
            , _svcVersion      = "2013-11-01"
            , _svcTargetPrefix = Just "com.amazonaws.cloudtrail.v20131101.CloudTrail_20131101"
            , _svcJSONVersion  = Just "1.1"
            , _svcHandle       = handle
            , _svcRetry        = retry
            }

        handle :: Status
               -> Maybe (LazyByteString -> ServiceError JSONError)
        handle = jsonError statusSuccess service'

        retry :: Retry CloudTrail
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

data Event = Event
    { _eCloudTrailEvent :: Maybe Text
    , _eEventId         :: Maybe Text
    , _eEventName       :: Maybe Text
    , _eEventTime       :: Maybe POSIX
    , _eResources       :: List "Resources" Resource
    , _eUsername        :: Maybe Text
    } deriving (Eq, Read, Show)

-- | 'Event' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'eCloudTrailEvent' @::@ 'Maybe' 'Text'
--
-- * 'eEventId' @::@ 'Maybe' 'Text'
--
-- * 'eEventName' @::@ 'Maybe' 'Text'
--
-- * 'eEventTime' @::@ 'Maybe' 'UTCTime'
--
-- * 'eResources' @::@ ['Resource']
--
-- * 'eUsername' @::@ 'Maybe' 'Text'
--
event :: Event
event = Event
    { _eEventId         = Nothing
    , _eEventName       = Nothing
    , _eEventTime       = Nothing
    , _eUsername        = Nothing
    , _eResources       = mempty
    , _eCloudTrailEvent = Nothing
    }

-- | A JSON string that contains a representation of the event returned.
eCloudTrailEvent :: Lens' Event (Maybe Text)
eCloudTrailEvent = lens _eCloudTrailEvent (\s a -> s { _eCloudTrailEvent = a })

-- | The CloudTrail ID of the event returned.
eEventId :: Lens' Event (Maybe Text)
eEventId = lens _eEventId (\s a -> s { _eEventId = a })

-- | The name of the event returned.
eEventName :: Lens' Event (Maybe Text)
eEventName = lens _eEventName (\s a -> s { _eEventName = a })

-- | The date and time of the event returned.
eEventTime :: Lens' Event (Maybe UTCTime)
eEventTime = lens _eEventTime (\s a -> s { _eEventTime = a }) . mapping _Time

-- | A list of resources referenced by the event returned.
eResources :: Lens' Event [Resource]
eResources = lens _eResources (\s a -> s { _eResources = a }) . _List

-- | A user name or role name of the requester that called the API in the event
-- returned.
eUsername :: Lens' Event (Maybe Text)
eUsername = lens _eUsername (\s a -> s { _eUsername = a })

instance FromJSON Event where
    parseJSON = withObject "Event" $ \o -> Event
        <$> o .:? "CloudTrailEvent"
        <*> o .:? "EventId"
        <*> o .:? "EventName"
        <*> o .:? "EventTime"
        <*> o .:? "Resources" .!= mempty
        <*> o .:? "Username"

instance ToJSON Event where
    toJSON Event{..} = object
        [ "EventId"         .= _eEventId
        , "EventName"       .= _eEventName
        , "EventTime"       .= _eEventTime
        , "Username"        .= _eUsername
        , "Resources"       .= _eResources
        , "CloudTrailEvent" .= _eCloudTrailEvent
        ]

data Trail = Trail
    { _tCloudWatchLogsLogGroupArn  :: Maybe Text
    , _tCloudWatchLogsRoleArn      :: Maybe Text
    , _tIncludeGlobalServiceEvents :: Maybe Bool
    , _tName                       :: Maybe Text
    , _tS3BucketName               :: Maybe Text
    , _tS3KeyPrefix                :: Maybe Text
    , _tSnsTopicName               :: Maybe Text
    } deriving (Eq, Ord, Read, Show)

-- | 'Trail' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'tCloudWatchLogsLogGroupArn' @::@ 'Maybe' 'Text'
--
-- * 'tCloudWatchLogsRoleArn' @::@ 'Maybe' 'Text'
--
-- * 'tIncludeGlobalServiceEvents' @::@ 'Maybe' 'Bool'
--
-- * 'tName' @::@ 'Maybe' 'Text'
--
-- * 'tS3BucketName' @::@ 'Maybe' 'Text'
--
-- * 'tS3KeyPrefix' @::@ 'Maybe' 'Text'
--
-- * 'tSnsTopicName' @::@ 'Maybe' 'Text'
--
trail :: Trail
trail = Trail
    { _tName                       = Nothing
    , _tS3BucketName               = Nothing
    , _tS3KeyPrefix                = Nothing
    , _tSnsTopicName               = Nothing
    , _tIncludeGlobalServiceEvents = Nothing
    , _tCloudWatchLogsLogGroupArn  = Nothing
    , _tCloudWatchLogsRoleArn      = Nothing
    }

-- | Specifies an Amazon Resource Name (ARN), a unique identifier that represents
-- the log group to which CloudTrail logs will be delivered.
tCloudWatchLogsLogGroupArn :: Lens' Trail (Maybe Text)
tCloudWatchLogsLogGroupArn =
    lens _tCloudWatchLogsLogGroupArn
        (\s a -> s { _tCloudWatchLogsLogGroupArn = a })

-- | Specifies the role for the CloudWatch Logs endpoint to assume to write to a
-- user’s log group.
tCloudWatchLogsRoleArn :: Lens' Trail (Maybe Text)
tCloudWatchLogsRoleArn =
    lens _tCloudWatchLogsRoleArn (\s a -> s { _tCloudWatchLogsRoleArn = a })

-- | Set to True to include AWS API calls from AWS global services such as IAM.
-- Otherwise, False.
tIncludeGlobalServiceEvents :: Lens' Trail (Maybe Bool)
tIncludeGlobalServiceEvents =
    lens _tIncludeGlobalServiceEvents
        (\s a -> s { _tIncludeGlobalServiceEvents = a })

-- | Name of the trail set by calling 'CreateTrail'.
tName :: Lens' Trail (Maybe Text)
tName = lens _tName (\s a -> s { _tName = a })

-- | Name of the Amazon S3 bucket into which CloudTrail delivers your trail files.
tS3BucketName :: Lens' Trail (Maybe Text)
tS3BucketName = lens _tS3BucketName (\s a -> s { _tS3BucketName = a })

-- | Value of the Amazon S3 prefix.
tS3KeyPrefix :: Lens' Trail (Maybe Text)
tS3KeyPrefix = lens _tS3KeyPrefix (\s a -> s { _tS3KeyPrefix = a })

-- | Name of the existing Amazon SNS topic that CloudTrail uses to notify the
-- account owner when new CloudTrail log files have been delivered.
tSnsTopicName :: Lens' Trail (Maybe Text)
tSnsTopicName = lens _tSnsTopicName (\s a -> s { _tSnsTopicName = a })

instance FromJSON Trail where
    parseJSON = withObject "Trail" $ \o -> Trail
        <$> o .:? "CloudWatchLogsLogGroupArn"
        <*> o .:? "CloudWatchLogsRoleArn"
        <*> o .:? "IncludeGlobalServiceEvents"
        <*> o .:? "Name"
        <*> o .:? "S3BucketName"
        <*> o .:? "S3KeyPrefix"
        <*> o .:? "SnsTopicName"

instance ToJSON Trail where
    toJSON Trail{..} = object
        [ "Name"                       .= _tName
        , "S3BucketName"               .= _tS3BucketName
        , "S3KeyPrefix"                .= _tS3KeyPrefix
        , "SnsTopicName"               .= _tSnsTopicName
        , "IncludeGlobalServiceEvents" .= _tIncludeGlobalServiceEvents
        , "CloudWatchLogsLogGroupArn"  .= _tCloudWatchLogsLogGroupArn
        , "CloudWatchLogsRoleArn"      .= _tCloudWatchLogsRoleArn
        ]

data LookupAttribute = LookupAttribute
    { _laAttributeKey   :: LookupAttributeKey
    , _laAttributeValue :: Text
    } deriving (Eq, Read, Show)

-- | 'LookupAttribute' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'laAttributeKey' @::@ 'LookupAttributeKey'
--
-- * 'laAttributeValue' @::@ 'Text'
--
lookupAttribute :: LookupAttributeKey -- ^ 'laAttributeKey'
                -> Text -- ^ 'laAttributeValue'
                -> LookupAttribute
lookupAttribute p1 p2 = LookupAttribute
    { _laAttributeKey   = p1
    , _laAttributeValue = p2
    }

-- | Specifies an attribute on which to filter the events returned.
laAttributeKey :: Lens' LookupAttribute LookupAttributeKey
laAttributeKey = lens _laAttributeKey (\s a -> s { _laAttributeKey = a })

-- | Specifies a value for the specified AttributeKey.
laAttributeValue :: Lens' LookupAttribute Text
laAttributeValue = lens _laAttributeValue (\s a -> s { _laAttributeValue = a })

instance FromJSON LookupAttribute where
    parseJSON = withObject "LookupAttribute" $ \o -> LookupAttribute
        <$> o .:  "AttributeKey"
        <*> o .:  "AttributeValue"

instance ToJSON LookupAttribute where
    toJSON LookupAttribute{..} = object
        [ "AttributeKey"   .= _laAttributeKey
        , "AttributeValue" .= _laAttributeValue
        ]

data LookupAttributeKey
    = EventId      -- ^ EventId
    | EventName    -- ^ EventName
    | ResourceName -- ^ ResourceName
    | ResourceType -- ^ ResourceType
    | Username     -- ^ Username
      deriving (Eq, Ord, Read, Show, Generic, Enum)

instance Hashable LookupAttributeKey

instance FromText LookupAttributeKey where
    parser = takeLowerText >>= \case
        "eventid"      -> pure EventId
        "eventname"    -> pure EventName
        "resourcename" -> pure ResourceName
        "resourcetype" -> pure ResourceType
        "username"     -> pure Username
        e              -> fail $
            "Failure parsing LookupAttributeKey from " ++ show e

instance ToText LookupAttributeKey where
    toText = \case
        EventId      -> "EventId"
        EventName    -> "EventName"
        ResourceName -> "ResourceName"
        ResourceType -> "ResourceType"
        Username     -> "Username"

instance ToByteString LookupAttributeKey
instance ToHeader     LookupAttributeKey
instance ToQuery      LookupAttributeKey

instance FromJSON LookupAttributeKey where
    parseJSON = parseJSONText "LookupAttributeKey"

instance ToJSON LookupAttributeKey where
    toJSON = toJSONText

data Resource = Resource
    { _rResourceName :: Maybe Text
    , _rResourceType :: Maybe Text
    } deriving (Eq, Ord, Read, Show)

-- | 'Resource' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rResourceName' @::@ 'Maybe' 'Text'
--
-- * 'rResourceType' @::@ 'Maybe' 'Text'
--
resource :: Resource
resource = Resource
    { _rResourceType = Nothing
    , _rResourceName = Nothing
    }

-- | The name of the resource referenced by the event returned. These are
-- user-created names whose values will depend on the environment. For example,
-- the resource name might be "auto-scaling-test-group" for an Auto Scaling
-- Group or "i-1234567" for an EC2 Instance.
rResourceName :: Lens' Resource (Maybe Text)
rResourceName = lens _rResourceName (\s a -> s { _rResourceName = a })

-- | The type of a resource referenced by the event returned. When the resource
-- type cannot be determined, null is returned. Some examples of resource types
-- are: Instance for EC2, Trail for CloudTrail, DBInstance for RDS, and AccessKey
-- for IAM. For a list of resource types supported for event lookup, see <http://docs.aws.amazon.com/awscloudtrail/latest/userguide/lookup_supported_resourcetypes.html Resource Types Supported for Event Lookup>.
rResourceType :: Lens' Resource (Maybe Text)
rResourceType = lens _rResourceType (\s a -> s { _rResourceType = a })

instance FromJSON Resource where
    parseJSON = withObject "Resource" $ \o -> Resource
        <$> o .:? "ResourceName"
        <*> o .:? "ResourceType"

instance ToJSON Resource where
    toJSON Resource{..} = object
        [ "ResourceType" .= _rResourceType
        , "ResourceName" .= _rResourceName
        ]
