{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Config.Types
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

module Network.AWS.Config.Types
    (
    -- * Service
      Config
    -- ** Error
    , JSONError

    -- * ConfigExportDeliveryInfo
    , ConfigExportDeliveryInfo
    , configExportDeliveryInfo
    , cediLastAttemptTime
    , cediLastErrorCode
    , cediLastErrorMessage
    , cediLastStatus
    , cediLastSuccessfulTime

    -- * ConfigStreamDeliveryInfo
    , ConfigStreamDeliveryInfo
    , configStreamDeliveryInfo
    , csdiLastErrorCode
    , csdiLastErrorMessage
    , csdiLastStatus
    , csdiLastStatusChangeTime

    -- * Relationship
    , Relationship
    , relationship
    , rRelationshipName
    , rResourceId
    , rResourceType

    -- * DeliveryChannel
    , DeliveryChannel
    , deliveryChannel
    , dcName
    , dcS3BucketName
    , dcS3KeyPrefix
    , dcSnsTopicARN

    -- * ChronologicalOrder
    , ChronologicalOrder (..)

    -- * ResourceType
    , ResourceType (..)

    -- * ConfigurationItem
    , ConfigurationItem
    , configurationItem
    , ciAccountId
    , ciArn
    , ciAvailabilityZone
    , ciConfiguration
    , ciConfigurationItemCaptureTime
    , ciConfigurationItemMD5Hash
    , ciConfigurationItemStatus
    , ciConfigurationStateId
    , ciRelatedEvents
    , ciRelationships
    , ciResourceCreationTime
    , ciResourceId
    , ciResourceType
    , ciTags
    , ciVersion

    -- * DeliveryStatus
    , DeliveryStatus (..)

    -- * DeliveryChannelStatus
    , DeliveryChannelStatus
    , deliveryChannelStatus
    , dcsConfigHistoryDeliveryInfo
    , dcsConfigSnapshotDeliveryInfo
    , dcsConfigStreamDeliveryInfo
    , dcsName

    -- * ConfigurationRecorderStatus
    , ConfigurationRecorderStatus
    , configurationRecorderStatus
    , crsLastErrorCode
    , crsLastErrorMessage
    , crsLastStartTime
    , crsLastStatus
    , crsLastStatusChangeTime
    , crsLastStopTime
    , crsName
    , crsRecording

    -- * ConfigurationItemStatus
    , ConfigurationItemStatus (..)

    -- * ConfigurationRecorder
    , ConfigurationRecorder
    , configurationRecorder
    , crName
    , crRoleARN

    -- * RecorderStatus
    , RecorderStatus (..)
    ) where

import Data.Char (isUpper)
import Network.AWS.Error
import Network.AWS.Prelude
import Network.AWS.Signing.V4
import qualified GHC.Exts

-- | Version @2014-11-12@ of the Amazon Config service.
data Config

instance AWSService Config where
    type Sg Config = V4
    type Er Config = JSONError

    service = Service
        { _svcAbbrev       = "Config"
        , _svcPrefix       = "config"
        , _svcVersion      = "2014-11-12"
        , _svcTargetPrefix = Just "StarlingDoveService"
        , _svcJSONVersion  = Just "1.1"
        }

    handle = jsonError statusSuccess

data ConfigExportDeliveryInfo = ConfigExportDeliveryInfo
    { _cediLastAttemptTime    :: Maybe ISO8601
    , _cediLastErrorCode      :: Maybe Text
    , _cediLastErrorMessage   :: Maybe Text
    , _cediLastStatus         :: Maybe DeliveryStatus
    , _cediLastSuccessfulTime :: Maybe ISO8601
    } deriving (Eq, Show)

-- | 'ConfigExportDeliveryInfo' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cediLastAttemptTime' @::@ 'Maybe' 'UTCTime'
--
-- * 'cediLastErrorCode' @::@ 'Maybe' 'Text'
--
-- * 'cediLastErrorMessage' @::@ 'Maybe' 'Text'
--
-- * 'cediLastStatus' @::@ 'Maybe' 'DeliveryStatus'
--
-- * 'cediLastSuccessfulTime' @::@ 'Maybe' 'UTCTime'
--
configExportDeliveryInfo :: ConfigExportDeliveryInfo
configExportDeliveryInfo = ConfigExportDeliveryInfo
    { _cediLastStatus         = Nothing
    , _cediLastErrorCode      = Nothing
    , _cediLastErrorMessage   = Nothing
    , _cediLastAttemptTime    = Nothing
    , _cediLastSuccessfulTime = Nothing
    }

-- | The time of the last attempted delivery.
cediLastAttemptTime :: Lens' ConfigExportDeliveryInfo (Maybe UTCTime)
cediLastAttemptTime =
    lens _cediLastAttemptTime (\s a -> s { _cediLastAttemptTime = a })
        . mapping _Time

-- | The error code from the last attempted delivery.
cediLastErrorCode :: Lens' ConfigExportDeliveryInfo (Maybe Text)
cediLastErrorCode =
    lens _cediLastErrorCode (\s a -> s { _cediLastErrorCode = a })

-- | The error message from the last attempted delivery.
cediLastErrorMessage :: Lens' ConfigExportDeliveryInfo (Maybe Text)
cediLastErrorMessage =
    lens _cediLastErrorMessage (\s a -> s { _cediLastErrorMessage = a })

-- | Status of the last attempted delivery.
cediLastStatus :: Lens' ConfigExportDeliveryInfo (Maybe DeliveryStatus)
cediLastStatus = lens _cediLastStatus (\s a -> s { _cediLastStatus = a })

-- | The time of the last successful delivery.
cediLastSuccessfulTime :: Lens' ConfigExportDeliveryInfo (Maybe UTCTime)
cediLastSuccessfulTime =
    lens _cediLastSuccessfulTime (\s a -> s { _cediLastSuccessfulTime = a })
        . mapping _Time

instance FromJSON ConfigExportDeliveryInfo where
    parseJSON = withObject "ConfigExportDeliveryInfo" $ \o -> ConfigExportDeliveryInfo
        <$> o .:? "lastAttemptTime"
        <*> o .:? "lastErrorCode"
        <*> o .:? "lastErrorMessage"
        <*> o .:? "lastStatus"
        <*> o .:? "lastSuccessfulTime"

instance ToJSON ConfigExportDeliveryInfo where
    toJSON ConfigExportDeliveryInfo{..} = object
        [ "lastStatus"         .= _cediLastStatus
        , "lastErrorCode"      .= _cediLastErrorCode
        , "lastErrorMessage"   .= _cediLastErrorMessage
        , "lastAttemptTime"    .= _cediLastAttemptTime
        , "lastSuccessfulTime" .= _cediLastSuccessfulTime
        ]

data ConfigStreamDeliveryInfo = ConfigStreamDeliveryInfo
    { _csdiLastErrorCode        :: Maybe Text
    , _csdiLastErrorMessage     :: Maybe Text
    , _csdiLastStatus           :: Maybe DeliveryStatus
    , _csdiLastStatusChangeTime :: Maybe ISO8601
    } deriving (Eq, Show)

-- | 'ConfigStreamDeliveryInfo' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'csdiLastErrorCode' @::@ 'Maybe' 'Text'
--
-- * 'csdiLastErrorMessage' @::@ 'Maybe' 'Text'
--
-- * 'csdiLastStatus' @::@ 'Maybe' 'DeliveryStatus'
--
-- * 'csdiLastStatusChangeTime' @::@ 'Maybe' 'UTCTime'
--
configStreamDeliveryInfo :: ConfigStreamDeliveryInfo
configStreamDeliveryInfo = ConfigStreamDeliveryInfo
    { _csdiLastStatus           = Nothing
    , _csdiLastErrorCode        = Nothing
    , _csdiLastErrorMessage     = Nothing
    , _csdiLastStatusChangeTime = Nothing
    }

-- | The error code from the last attempted delivery.
csdiLastErrorCode :: Lens' ConfigStreamDeliveryInfo (Maybe Text)
csdiLastErrorCode =
    lens _csdiLastErrorCode (\s a -> s { _csdiLastErrorCode = a })

-- | The error message from the last attempted delivery.
csdiLastErrorMessage :: Lens' ConfigStreamDeliveryInfo (Maybe Text)
csdiLastErrorMessage =
    lens _csdiLastErrorMessage (\s a -> s { _csdiLastErrorMessage = a })

-- | Status of the last attempted delivery.
csdiLastStatus :: Lens' ConfigStreamDeliveryInfo (Maybe DeliveryStatus)
csdiLastStatus = lens _csdiLastStatus (\s a -> s { _csdiLastStatus = a })

-- | The time from the last status change.
csdiLastStatusChangeTime :: Lens' ConfigStreamDeliveryInfo (Maybe UTCTime)
csdiLastStatusChangeTime =
    lens _csdiLastStatusChangeTime
        (\s a -> s { _csdiLastStatusChangeTime = a })
            . mapping _Time

instance FromJSON ConfigStreamDeliveryInfo where
    parseJSON = withObject "ConfigStreamDeliveryInfo" $ \o -> ConfigStreamDeliveryInfo
        <$> o .:? "lastErrorCode"
        <*> o .:? "lastErrorMessage"
        <*> o .:? "lastStatus"
        <*> o .:? "lastStatusChangeTime"

instance ToJSON ConfigStreamDeliveryInfo where
    toJSON ConfigStreamDeliveryInfo{..} = object
        [ "lastStatus"           .= _csdiLastStatus
        , "lastErrorCode"        .= _csdiLastErrorCode
        , "lastErrorMessage"     .= _csdiLastErrorMessage
        , "lastStatusChangeTime" .= _csdiLastStatusChangeTime
        ]

data Relationship = Relationship
    { _rRelationshipName :: Maybe Text
    , _rResourceId       :: Maybe Text
    , _rResourceType     :: Maybe ResourceType
    } deriving (Eq, Show)

-- | 'Relationship' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rRelationshipName' @::@ 'Maybe' 'Text'
--
-- * 'rResourceId' @::@ 'Maybe' 'Text'
--
-- * 'rResourceType' @::@ 'Maybe' 'ResourceType'
--
relationship :: Relationship
relationship = Relationship
    { _rResourceType     = Nothing
    , _rResourceId       = Nothing
    , _rRelationshipName = Nothing
    }

-- | The name of the related resource.
rRelationshipName :: Lens' Relationship (Maybe Text)
rRelationshipName =
    lens _rRelationshipName (\s a -> s { _rRelationshipName = a })

-- | The resource ID of the related resource (for example, 'sg-xxxxxx'.
rResourceId :: Lens' Relationship (Maybe Text)
rResourceId = lens _rResourceId (\s a -> s { _rResourceId = a })

-- | The resource type of the related resource.
rResourceType :: Lens' Relationship (Maybe ResourceType)
rResourceType = lens _rResourceType (\s a -> s { _rResourceType = a })

instance FromJSON Relationship where
    parseJSON = withObject "Relationship" $ \o -> Relationship
        <$> o .:? "relationshipName"
        <*> o .:? "resourceId"
        <*> o .:? "resourceType"

instance ToJSON Relationship where
    toJSON Relationship{..} = object
        [ "resourceType"     .= _rResourceType
        , "resourceId"       .= _rResourceId
        , "relationshipName" .= _rRelationshipName
        ]

data DeliveryChannel = DeliveryChannel
    { _dcName         :: Maybe Text
    , _dcS3BucketName :: Maybe Text
    , _dcS3KeyPrefix  :: Maybe Text
    , _dcSnsTopicARN  :: Maybe Text
    } deriving (Eq, Ord, Show)

-- | 'DeliveryChannel' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dcName' @::@ 'Maybe' 'Text'
--
-- * 'dcS3BucketName' @::@ 'Maybe' 'Text'
--
-- * 'dcS3KeyPrefix' @::@ 'Maybe' 'Text'
--
-- * 'dcSnsTopicARN' @::@ 'Maybe' 'Text'
--
deliveryChannel :: DeliveryChannel
deliveryChannel = DeliveryChannel
    { _dcName         = Nothing
    , _dcS3BucketName = Nothing
    , _dcS3KeyPrefix  = Nothing
    , _dcSnsTopicARN  = Nothing
    }

-- | The name of the delivery channel. By default, AWS Config automatically
-- assigns the name "default" when creating the delivery channel. You cannot
-- change the assigned name.
dcName :: Lens' DeliveryChannel (Maybe Text)
dcName = lens _dcName (\s a -> s { _dcName = a })

-- | The name of the Amazon S3 bucket used to store configuration history for the
-- delivery channel.
dcS3BucketName :: Lens' DeliveryChannel (Maybe Text)
dcS3BucketName = lens _dcS3BucketName (\s a -> s { _dcS3BucketName = a })

-- | The prefix for the specified Amazon S3 bucket.
dcS3KeyPrefix :: Lens' DeliveryChannel (Maybe Text)
dcS3KeyPrefix = lens _dcS3KeyPrefix (\s a -> s { _dcS3KeyPrefix = a })

-- | The Amazon Resource Name (ARN) of the IAM role used for accessing the Amazon
-- S3 bucket and the Amazon SNS topic.
dcSnsTopicARN :: Lens' DeliveryChannel (Maybe Text)
dcSnsTopicARN = lens _dcSnsTopicARN (\s a -> s { _dcSnsTopicARN = a })

instance FromJSON DeliveryChannel where
    parseJSON = withObject "DeliveryChannel" $ \o -> DeliveryChannel
        <$> o .:? "name"
        <*> o .:? "s3BucketName"
        <*> o .:? "s3KeyPrefix"
        <*> o .:? "snsTopicARN"

instance ToJSON DeliveryChannel where
    toJSON DeliveryChannel{..} = object
        [ "name"         .= _dcName
        , "s3BucketName" .= _dcS3BucketName
        , "s3KeyPrefix"  .= _dcS3KeyPrefix
        , "snsTopicARN"  .= _dcSnsTopicARN
        ]

data ChronologicalOrder
    = Forward -- ^ Forward
    | Reverse -- ^ Reverse
      deriving (Eq, Ord, Show, Generic, Enum)

instance Hashable ChronologicalOrder

instance FromText ChronologicalOrder where
    parser = takeText >>= \case
        "Forward" -> pure Forward
        "Reverse" -> pure Reverse
        e         -> fail $
            "Failure parsing ChronologicalOrder from " ++ show e

instance ToText ChronologicalOrder where
    toText = \case
        Forward -> "Forward"
        Reverse -> "Reverse"

instance ToByteString ChronologicalOrder
instance ToHeader     ChronologicalOrder
instance ToQuery      ChronologicalOrder

instance FromJSON ChronologicalOrder where
    parseJSON = parseJSONText "ChronologicalOrder"

instance ToJSON ChronologicalOrder where
    toJSON = toJSONText

data ResourceType
    = AWSCloudTrailTrail     -- ^ AWS::CloudTrail::Trail
    | AWSEC2CustomerGateway  -- ^ AWS::EC2::CustomerGateway
    | AWSEC2EIP              -- ^ AWS::EC2::EIP
    | AWSEC2Instance         -- ^ AWS::EC2::Instance
    | AWSEC2InternetGateway  -- ^ AWS::EC2::InternetGateway
    | AWSEC2NetworkAcl       -- ^ AWS::EC2::NetworkAcl
    | AWSEC2NetworkInterface -- ^ AWS::EC2::NetworkInterface
    | AWSEC2RouteTable       -- ^ AWS::EC2::RouteTable
    | AWSEC2SecurityGroup    -- ^ AWS::EC2::SecurityGroup
    | AWSEC2Subnet           -- ^ AWS::EC2::Subnet
    | AWSEC2VPC              -- ^ AWS::EC2::VPC
    | AWSEC2VPNConnection    -- ^ AWS::EC2::VPNConnection
    | AWSEC2VPNGateway       -- ^ AWS::EC2::VPNGateway
    | AWSEC2Volume           -- ^ AWS::EC2::Volume
      deriving (Eq, Ord, Show, Generic, Enum)

instance Hashable ResourceType

instance FromText ResourceType where
    parser = takeText >>= \case
        "AWS::CloudTrail::Trail"     -> pure AWSCloudTrailTrail
        "AWS::EC2::CustomerGateway"  -> pure AWSEC2CustomerGateway
        "AWS::EC2::EIP"              -> pure AWSEC2EIP
        "AWS::EC2::Instance"         -> pure AWSEC2Instance
        "AWS::EC2::InternetGateway"  -> pure AWSEC2InternetGateway
        "AWS::EC2::NetworkAcl"       -> pure AWSEC2NetworkAcl
        "AWS::EC2::NetworkInterface" -> pure AWSEC2NetworkInterface
        "AWS::EC2::RouteTable"       -> pure AWSEC2RouteTable
        "AWS::EC2::SecurityGroup"    -> pure AWSEC2SecurityGroup
        "AWS::EC2::Subnet"           -> pure AWSEC2Subnet
        "AWS::EC2::VPC"              -> pure AWSEC2VPC
        "AWS::EC2::VPNConnection"    -> pure AWSEC2VPNConnection
        "AWS::EC2::VPNGateway"       -> pure AWSEC2VPNGateway
        "AWS::EC2::Volume"           -> pure AWSEC2Volume
        e                            -> fail $
            "Failure parsing ResourceType from " ++ show e

instance ToText ResourceType where
    toText = \case
        AWSCloudTrailTrail     -> "AWS::CloudTrail::Trail"
        AWSEC2CustomerGateway  -> "AWS::EC2::CustomerGateway"
        AWSEC2EIP              -> "AWS::EC2::EIP"
        AWSEC2Instance         -> "AWS::EC2::Instance"
        AWSEC2InternetGateway  -> "AWS::EC2::InternetGateway"
        AWSEC2NetworkAcl       -> "AWS::EC2::NetworkAcl"
        AWSEC2NetworkInterface -> "AWS::EC2::NetworkInterface"
        AWSEC2RouteTable       -> "AWS::EC2::RouteTable"
        AWSEC2SecurityGroup    -> "AWS::EC2::SecurityGroup"
        AWSEC2Subnet           -> "AWS::EC2::Subnet"
        AWSEC2VPC              -> "AWS::EC2::VPC"
        AWSEC2VPNConnection    -> "AWS::EC2::VPNConnection"
        AWSEC2VPNGateway       -> "AWS::EC2::VPNGateway"
        AWSEC2Volume           -> "AWS::EC2::Volume"

instance ToByteString ResourceType
instance ToHeader     ResourceType
instance ToQuery      ResourceType

instance FromJSON ResourceType where
    parseJSON = parseJSONText "ResourceType"

instance ToJSON ResourceType where
    toJSON = toJSONText

data ConfigurationItem = ConfigurationItem
    { _ciAccountId                    :: Maybe Text
    , _ciArn                          :: Maybe Text
    , _ciAvailabilityZone             :: Maybe Text
    , _ciConfiguration                :: Maybe Text
    , _ciConfigurationItemCaptureTime :: Maybe ISO8601
    , _ciConfigurationItemMD5Hash     :: Maybe Text
    , _ciConfigurationItemStatus      :: Maybe ConfigurationItemStatus
    , _ciConfigurationStateId         :: Maybe Text
    , _ciRelatedEvents                :: List "relatedEvents" Text
    , _ciRelationships                :: List "relationships" Relationship
    , _ciResourceCreationTime         :: Maybe ISO8601
    , _ciResourceId                   :: Maybe Text
    , _ciResourceType                 :: Maybe ResourceType
    , _ciTags                         :: Map Text Text
    , _ciVersion                      :: Maybe Text
    } deriving (Eq, Show)

-- | 'ConfigurationItem' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ciAccountId' @::@ 'Maybe' 'Text'
--
-- * 'ciArn' @::@ 'Maybe' 'Text'
--
-- * 'ciAvailabilityZone' @::@ 'Maybe' 'Text'
--
-- * 'ciConfiguration' @::@ 'Maybe' 'Text'
--
-- * 'ciConfigurationItemCaptureTime' @::@ 'Maybe' 'UTCTime'
--
-- * 'ciConfigurationItemMD5Hash' @::@ 'Maybe' 'Text'
--
-- * 'ciConfigurationItemStatus' @::@ 'Maybe' 'ConfigurationItemStatus'
--
-- * 'ciConfigurationStateId' @::@ 'Maybe' 'Text'
--
-- * 'ciRelatedEvents' @::@ ['Text']
--
-- * 'ciRelationships' @::@ ['Relationship']
--
-- * 'ciResourceCreationTime' @::@ 'Maybe' 'UTCTime'
--
-- * 'ciResourceId' @::@ 'Maybe' 'Text'
--
-- * 'ciResourceType' @::@ 'Maybe' 'ResourceType'
--
-- * 'ciTags' @::@ 'HashMap' 'Text' 'Text'
--
-- * 'ciVersion' @::@ 'Maybe' 'Text'
--
configurationItem :: ConfigurationItem
configurationItem = ConfigurationItem
    { _ciVersion                      = Nothing
    , _ciAccountId                    = Nothing
    , _ciConfigurationItemCaptureTime = Nothing
    , _ciConfigurationItemStatus      = Nothing
    , _ciConfigurationStateId         = Nothing
    , _ciConfigurationItemMD5Hash     = Nothing
    , _ciArn                          = Nothing
    , _ciResourceType                 = Nothing
    , _ciResourceId                   = Nothing
    , _ciAvailabilityZone             = Nothing
    , _ciResourceCreationTime         = Nothing
    , _ciTags                         = mempty
    , _ciRelatedEvents                = mempty
    , _ciRelationships                = mempty
    , _ciConfiguration                = Nothing
    }

-- | The 12 digit AWS account ID associated with the resource.
ciAccountId :: Lens' ConfigurationItem (Maybe Text)
ciAccountId = lens _ciAccountId (\s a -> s { _ciAccountId = a })

-- | The Amazon Resource Name (ARN) of the resource.
ciArn :: Lens' ConfigurationItem (Maybe Text)
ciArn = lens _ciArn (\s a -> s { _ciArn = a })

-- | The Availability Zone associated with the resource.
ciAvailabilityZone :: Lens' ConfigurationItem (Maybe Text)
ciAvailabilityZone =
    lens _ciAvailabilityZone (\s a -> s { _ciAvailabilityZone = a })

-- | The description of the resource configuration.
ciConfiguration :: Lens' ConfigurationItem (Maybe Text)
ciConfiguration = lens _ciConfiguration (\s a -> s { _ciConfiguration = a })

-- | The time when the configuration recording was initiated.
ciConfigurationItemCaptureTime :: Lens' ConfigurationItem (Maybe UTCTime)
ciConfigurationItemCaptureTime =
    lens _ciConfigurationItemCaptureTime
        (\s a -> s { _ciConfigurationItemCaptureTime = a })
            . mapping _Time

-- | Unique MD5 hash that represents the configuration item's state.
--
-- You can use MD5 hash to compare the states of two or more configuration
-- items that are associated with the same resource.
ciConfigurationItemMD5Hash :: Lens' ConfigurationItem (Maybe Text)
ciConfigurationItemMD5Hash =
    lens _ciConfigurationItemMD5Hash
        (\s a -> s { _ciConfigurationItemMD5Hash = a })

-- | The configuration item status.
ciConfigurationItemStatus :: Lens' ConfigurationItem (Maybe ConfigurationItemStatus)
ciConfigurationItemStatus =
    lens _ciConfigurationItemStatus
        (\s a -> s { _ciConfigurationItemStatus = a })

-- | An identifier that indicates the ordering of the configuration items of a
-- resource.
ciConfigurationStateId :: Lens' ConfigurationItem (Maybe Text)
ciConfigurationStateId =
    lens _ciConfigurationStateId (\s a -> s { _ciConfigurationStateId = a })

-- | A list of CloudTrail event IDs.
--
-- A populated field indicates that the current configuration was initiated by
-- the events recorded in the CloudTrail log. For more information about
-- CloudTrail, see <http://docs.aws.amazon.com/awscloudtrail/latest/userguide/what_is_cloud_trail_top_level.html What is AWS CloudTrail?>.
--
-- An empty field indicates that the current configuration was not initiated by
-- any event.
ciRelatedEvents :: Lens' ConfigurationItem [Text]
ciRelatedEvents = lens _ciRelatedEvents (\s a -> s { _ciRelatedEvents = a }) . _List

-- | A list of related AWS resources.
ciRelationships :: Lens' ConfigurationItem [Relationship]
ciRelationships = lens _ciRelationships (\s a -> s { _ciRelationships = a }) . _List

-- | The time stamp when the resource was created.
ciResourceCreationTime :: Lens' ConfigurationItem (Maybe UTCTime)
ciResourceCreationTime =
    lens _ciResourceCreationTime (\s a -> s { _ciResourceCreationTime = a })
        . mapping _Time

-- | The ID of the resource (for example., 'sg-xxxxxx').
ciResourceId :: Lens' ConfigurationItem (Maybe Text)
ciResourceId = lens _ciResourceId (\s a -> s { _ciResourceId = a })

-- | The type of AWS resource.
ciResourceType :: Lens' ConfigurationItem (Maybe ResourceType)
ciResourceType = lens _ciResourceType (\s a -> s { _ciResourceType = a })

-- | A mapping of key value tags associated with the resource.
ciTags :: Lens' ConfigurationItem (HashMap Text Text)
ciTags = lens _ciTags (\s a -> s { _ciTags = a }) . _Map

-- | The version number of the resource configuration.
ciVersion :: Lens' ConfigurationItem (Maybe Text)
ciVersion = lens _ciVersion (\s a -> s { _ciVersion = a })

instance FromJSON ConfigurationItem where
    parseJSON = withObject "ConfigurationItem" $ \o -> ConfigurationItem
        <$> o .:? "accountId"
        <*> o .:? "arn"
        <*> o .:? "availabilityZone"
        <*> o .:? "configuration"
        <*> o .:? "configurationItemCaptureTime"
        <*> o .:? "configurationItemMD5Hash"
        <*> o .:? "configurationItemStatus"
        <*> o .:? "configurationStateId"
        <*> o .:  "relatedEvents"
        <*> o .:  "relationships"
        <*> o .:? "resourceCreationTime"
        <*> o .:? "resourceId"
        <*> o .:? "resourceType"
        <*> o .:  "tags"
        <*> o .:? "version"

instance ToJSON ConfigurationItem where
    toJSON ConfigurationItem{..} = object
        [ "version"                      .= _ciVersion
        , "accountId"                    .= _ciAccountId
        , "configurationItemCaptureTime" .= _ciConfigurationItemCaptureTime
        , "configurationItemStatus"      .= _ciConfigurationItemStatus
        , "configurationStateId"         .= _ciConfigurationStateId
        , "configurationItemMD5Hash"     .= _ciConfigurationItemMD5Hash
        , "arn"                          .= _ciArn
        , "resourceType"                 .= _ciResourceType
        , "resourceId"                   .= _ciResourceId
        , "availabilityZone"             .= _ciAvailabilityZone
        , "resourceCreationTime"         .= _ciResourceCreationTime
        , "tags"                         .= _ciTags
        , "relatedEvents"                .= _ciRelatedEvents
        , "relationships"                .= _ciRelationships
        , "configuration"                .= _ciConfiguration
        ]

data DeliveryStatus
    = Failure -- ^ Failure
    | Success -- ^ Success
      deriving (Eq, Ord, Show, Generic, Enum)

instance Hashable DeliveryStatus

instance FromText DeliveryStatus where
    parser = takeText >>= \case
        "Failure" -> pure Failure
        "Success" -> pure Success
        e         -> fail $
            "Failure parsing DeliveryStatus from " ++ show e

instance ToText DeliveryStatus where
    toText = \case
        Failure -> "Failure"
        Success -> "Success"

instance ToByteString DeliveryStatus
instance ToHeader     DeliveryStatus
instance ToQuery      DeliveryStatus

instance FromJSON DeliveryStatus where
    parseJSON = parseJSONText "DeliveryStatus"

instance ToJSON DeliveryStatus where
    toJSON = toJSONText

data DeliveryChannelStatus = DeliveryChannelStatus
    { _dcsConfigHistoryDeliveryInfo  :: Maybe ConfigExportDeliveryInfo
    , _dcsConfigSnapshotDeliveryInfo :: Maybe ConfigExportDeliveryInfo
    , _dcsConfigStreamDeliveryInfo   :: Maybe ConfigStreamDeliveryInfo
    , _dcsName                       :: Maybe Text
    } deriving (Eq, Show)

-- | 'DeliveryChannelStatus' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dcsConfigHistoryDeliveryInfo' @::@ 'Maybe' 'ConfigExportDeliveryInfo'
--
-- * 'dcsConfigSnapshotDeliveryInfo' @::@ 'Maybe' 'ConfigExportDeliveryInfo'
--
-- * 'dcsConfigStreamDeliveryInfo' @::@ 'Maybe' 'ConfigStreamDeliveryInfo'
--
-- * 'dcsName' @::@ 'Maybe' 'Text'
--
deliveryChannelStatus :: DeliveryChannelStatus
deliveryChannelStatus = DeliveryChannelStatus
    { _dcsName                       = Nothing
    , _dcsConfigSnapshotDeliveryInfo = Nothing
    , _dcsConfigHistoryDeliveryInfo  = Nothing
    , _dcsConfigStreamDeliveryInfo   = Nothing
    }

-- | A list that contains the status of the delivery of the configuration history
-- to the specified Amazon S3 bucket.
dcsConfigHistoryDeliveryInfo :: Lens' DeliveryChannelStatus (Maybe ConfigExportDeliveryInfo)
dcsConfigHistoryDeliveryInfo =
    lens _dcsConfigHistoryDeliveryInfo
        (\s a -> s { _dcsConfigHistoryDeliveryInfo = a })

-- | A list containing the status of the delivery of the snapshot to the specified
-- Amazon S3 bucket.
dcsConfigSnapshotDeliveryInfo :: Lens' DeliveryChannelStatus (Maybe ConfigExportDeliveryInfo)
dcsConfigSnapshotDeliveryInfo =
    lens _dcsConfigSnapshotDeliveryInfo
        (\s a -> s { _dcsConfigSnapshotDeliveryInfo = a })

-- | A list containing the status of the delivery of the configuration stream
-- notification to the specified Amazon SNS topic.
dcsConfigStreamDeliveryInfo :: Lens' DeliveryChannelStatus (Maybe ConfigStreamDeliveryInfo)
dcsConfigStreamDeliveryInfo =
    lens _dcsConfigStreamDeliveryInfo
        (\s a -> s { _dcsConfigStreamDeliveryInfo = a })

-- | The name of the delivery channel.
dcsName :: Lens' DeliveryChannelStatus (Maybe Text)
dcsName = lens _dcsName (\s a -> s { _dcsName = a })

instance FromJSON DeliveryChannelStatus where
    parseJSON = withObject "DeliveryChannelStatus" $ \o -> DeliveryChannelStatus
        <$> o .:? "configHistoryDeliveryInfo"
        <*> o .:? "configSnapshotDeliveryInfo"
        <*> o .:? "configStreamDeliveryInfo"
        <*> o .:? "name"

instance ToJSON DeliveryChannelStatus where
    toJSON DeliveryChannelStatus{..} = object
        [ "name"                       .= _dcsName
        , "configSnapshotDeliveryInfo" .= _dcsConfigSnapshotDeliveryInfo
        , "configHistoryDeliveryInfo"  .= _dcsConfigHistoryDeliveryInfo
        , "configStreamDeliveryInfo"   .= _dcsConfigStreamDeliveryInfo
        ]

data ConfigurationRecorderStatus = ConfigurationRecorderStatus
    { _crsLastErrorCode        :: Maybe Text
    , _crsLastErrorMessage     :: Maybe Text
    , _crsLastStartTime        :: Maybe ISO8601
    , _crsLastStatus           :: Maybe RecorderStatus
    , _crsLastStatusChangeTime :: Maybe ISO8601
    , _crsLastStopTime         :: Maybe ISO8601
    , _crsName                 :: Maybe Text
    , _crsRecording            :: Maybe Bool
    } deriving (Eq, Show)

-- | 'ConfigurationRecorderStatus' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'crsLastErrorCode' @::@ 'Maybe' 'Text'
--
-- * 'crsLastErrorMessage' @::@ 'Maybe' 'Text'
--
-- * 'crsLastStartTime' @::@ 'Maybe' 'UTCTime'
--
-- * 'crsLastStatus' @::@ 'Maybe' 'RecorderStatus'
--
-- * 'crsLastStatusChangeTime' @::@ 'Maybe' 'UTCTime'
--
-- * 'crsLastStopTime' @::@ 'Maybe' 'UTCTime'
--
-- * 'crsName' @::@ 'Maybe' 'Text'
--
-- * 'crsRecording' @::@ 'Maybe' 'Bool'
--
configurationRecorderStatus :: ConfigurationRecorderStatus
configurationRecorderStatus = ConfigurationRecorderStatus
    { _crsName                 = Nothing
    , _crsLastStartTime        = Nothing
    , _crsLastStopTime         = Nothing
    , _crsRecording            = Nothing
    , _crsLastStatus           = Nothing
    , _crsLastErrorCode        = Nothing
    , _crsLastErrorMessage     = Nothing
    , _crsLastStatusChangeTime = Nothing
    }

-- | The error code indicating that the recording failed.
crsLastErrorCode :: Lens' ConfigurationRecorderStatus (Maybe Text)
crsLastErrorCode = lens _crsLastErrorCode (\s a -> s { _crsLastErrorCode = a })

-- | The message indicating that the recording failed due to an error.
crsLastErrorMessage :: Lens' ConfigurationRecorderStatus (Maybe Text)
crsLastErrorMessage =
    lens _crsLastErrorMessage (\s a -> s { _crsLastErrorMessage = a })

-- | The time the recorder was last started.
crsLastStartTime :: Lens' ConfigurationRecorderStatus (Maybe UTCTime)
crsLastStartTime = lens _crsLastStartTime (\s a -> s { _crsLastStartTime = a }) . mapping _Time

-- | The last (previous) status of the recorder.
crsLastStatus :: Lens' ConfigurationRecorderStatus (Maybe RecorderStatus)
crsLastStatus = lens _crsLastStatus (\s a -> s { _crsLastStatus = a })

-- | The time when the status was last changed.
crsLastStatusChangeTime :: Lens' ConfigurationRecorderStatus (Maybe UTCTime)
crsLastStatusChangeTime =
    lens _crsLastStatusChangeTime (\s a -> s { _crsLastStatusChangeTime = a })
        . mapping _Time

-- | The time the recorder was last stopped.
crsLastStopTime :: Lens' ConfigurationRecorderStatus (Maybe UTCTime)
crsLastStopTime = lens _crsLastStopTime (\s a -> s { _crsLastStopTime = a }) . mapping _Time

-- | The name of the configuration recorder.
crsName :: Lens' ConfigurationRecorderStatus (Maybe Text)
crsName = lens _crsName (\s a -> s { _crsName = a })

-- | Specifies whether the recorder is currently recording or not.
crsRecording :: Lens' ConfigurationRecorderStatus (Maybe Bool)
crsRecording = lens _crsRecording (\s a -> s { _crsRecording = a })

instance FromJSON ConfigurationRecorderStatus where
    parseJSON = withObject "ConfigurationRecorderStatus" $ \o -> ConfigurationRecorderStatus
        <$> o .:? "lastErrorCode"
        <*> o .:? "lastErrorMessage"
        <*> o .:? "lastStartTime"
        <*> o .:? "lastStatus"
        <*> o .:? "lastStatusChangeTime"
        <*> o .:? "lastStopTime"
        <*> o .:? "name"
        <*> o .:? "recording"

instance ToJSON ConfigurationRecorderStatus where
    toJSON ConfigurationRecorderStatus{..} = object
        [ "name"                 .= _crsName
        , "lastStartTime"        .= _crsLastStartTime
        , "lastStopTime"         .= _crsLastStopTime
        , "recording"            .= _crsRecording
        , "lastStatus"           .= _crsLastStatus
        , "lastErrorCode"        .= _crsLastErrorCode
        , "lastErrorMessage"     .= _crsLastErrorMessage
        , "lastStatusChangeTime" .= _crsLastStatusChangeTime
        ]

data ConfigurationItemStatus
    = Deleted    -- ^ Deleted
    | Discovered -- ^ Discovered
    | Failed     -- ^ Failed
    | Ok         -- ^ Ok
      deriving (Eq, Ord, Show, Generic, Enum)

instance Hashable ConfigurationItemStatus

instance FromText ConfigurationItemStatus where
    parser = takeText >>= \case
        "Deleted"    -> pure Deleted
        "Discovered" -> pure Discovered
        "Failed"     -> pure Failed
        "Ok"         -> pure Ok
        e            -> fail $
            "Failure parsing ConfigurationItemStatus from " ++ show e

instance ToText ConfigurationItemStatus where
    toText = \case
        Deleted    -> "Deleted"
        Discovered -> "Discovered"
        Failed     -> "Failed"
        Ok         -> "Ok"

instance ToByteString ConfigurationItemStatus
instance ToHeader     ConfigurationItemStatus
instance ToQuery      ConfigurationItemStatus

instance FromJSON ConfigurationItemStatus where
    parseJSON = parseJSONText "ConfigurationItemStatus"

instance ToJSON ConfigurationItemStatus where
    toJSON = toJSONText

data ConfigurationRecorder = ConfigurationRecorder
    { _crName    :: Maybe Text
    , _crRoleARN :: Maybe Text
    } deriving (Eq, Ord, Show)

-- | 'ConfigurationRecorder' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'crName' @::@ 'Maybe' 'Text'
--
-- * 'crRoleARN' @::@ 'Maybe' 'Text'
--
configurationRecorder :: ConfigurationRecorder
configurationRecorder = ConfigurationRecorder
    { _crName    = Nothing
    , _crRoleARN = Nothing
    }

-- | The name of the recorder. By default, AWS Config automatically assigns the
-- name "default" when creating the configuration recorder. You cannot change
-- the assigned name.
crName :: Lens' ConfigurationRecorder (Maybe Text)
crName = lens _crName (\s a -> s { _crName = a })

-- | Amazon Resource Name (ARN) of the IAM role used to describe the AWS resources
-- associated with the account.
crRoleARN :: Lens' ConfigurationRecorder (Maybe Text)
crRoleARN = lens _crRoleARN (\s a -> s { _crRoleARN = a })

instance FromJSON ConfigurationRecorder where
    parseJSON = withObject "ConfigurationRecorder" $ \o -> ConfigurationRecorder
        <$> o .:? "name"
        <*> o .:? "roleARN"

instance ToJSON ConfigurationRecorder where
    toJSON ConfigurationRecorder{..} = object
        [ "name"    .= _crName
        , "roleARN" .= _crRoleARN
        ]

data RecorderStatus
    = RSFailure -- ^ Failure
    | RSPending -- ^ Pending
    | RSSuccess -- ^ Success
      deriving (Eq, Ord, Show, Generic, Enum)

instance Hashable RecorderStatus

instance FromText RecorderStatus where
    parser = takeText >>= \case
        "Failure" -> pure RSFailure
        "Pending" -> pure RSPending
        "Success" -> pure RSSuccess
        e         -> fail $
            "Failure parsing RecorderStatus from " ++ show e

instance ToText RecorderStatus where
    toText = \case
        RSFailure -> "Failure"
        RSPending -> "Pending"
        RSSuccess -> "Success"

instance ToByteString RecorderStatus
instance ToHeader     RecorderStatus
instance ToQuery      RecorderStatus

instance FromJSON RecorderStatus where
    parseJSON = parseJSONText "RecorderStatus"

instance ToJSON RecorderStatus where
    toJSON = toJSONText
