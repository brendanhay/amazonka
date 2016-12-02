{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.Sum
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Pinpoint.Types.Sum where

import           Network.AWS.Prelude

data Action
    = DeepLink
    | OpenApp
    | URL
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText Action where
    parser = takeLowerText >>= \case
        "deep_link" -> pure DeepLink
        "open_app" -> pure OpenApp
        "url" -> pure URL
        e -> fromTextError $ "Failure parsing Action from value: '" <> e
           <> "'. Accepted values: deep_link, open_app, url"

instance ToText Action where
    toText = \case
        DeepLink -> "DEEP_LINK"
        OpenApp -> "OPEN_APP"
        URL -> "URL"

instance Hashable     Action
instance NFData       Action
instance ToByteString Action
instance ToQuery      Action
instance ToHeader     Action

instance ToJSON Action where
    toJSON = toJSONText

instance FromJSON Action where
    parseJSON = parseJSONText "Action"

data AttributeType
    = Exclusive
    | Inclusive
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText AttributeType where
    parser = takeLowerText >>= \case
        "exclusive" -> pure Exclusive
        "inclusive" -> pure Inclusive
        e -> fromTextError $ "Failure parsing AttributeType from value: '" <> e
           <> "'. Accepted values: exclusive, inclusive"

instance ToText AttributeType where
    toText = \case
        Exclusive -> "EXCLUSIVE"
        Inclusive -> "INCLUSIVE"

instance Hashable     AttributeType
instance NFData       AttributeType
instance ToByteString AttributeType
instance ToQuery      AttributeType
instance ToHeader     AttributeType

instance ToJSON AttributeType where
    toJSON = toJSONText

instance FromJSON AttributeType where
    parseJSON = parseJSONText "AttributeType"

data CampaignStatus
    = Completed
    | Executing
    | Paused
    | PendingNextRun
    | Scheduled
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText CampaignStatus where
    parser = takeLowerText >>= \case
        "completed" -> pure Completed
        "executing" -> pure Executing
        "paused" -> pure Paused
        "pending_next_run" -> pure PendingNextRun
        "scheduled" -> pure Scheduled
        e -> fromTextError $ "Failure parsing CampaignStatus from value: '" <> e
           <> "'. Accepted values: completed, executing, paused, pending_next_run, scheduled"

instance ToText CampaignStatus where
    toText = \case
        Completed -> "COMPLETED"
        Executing -> "EXECUTING"
        Paused -> "PAUSED"
        PendingNextRun -> "PENDING_NEXT_RUN"
        Scheduled -> "SCHEDULED"

instance Hashable     CampaignStatus
instance NFData       CampaignStatus
instance ToByteString CampaignStatus
instance ToQuery      CampaignStatus
instance ToHeader     CampaignStatus

instance FromJSON CampaignStatus where
    parseJSON = parseJSONText "CampaignStatus"

data ChannelType
    = APNS
    | GCM
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText ChannelType where
    parser = takeLowerText >>= \case
        "apns" -> pure APNS
        "gcm" -> pure GCM
        e -> fromTextError $ "Failure parsing ChannelType from value: '" <> e
           <> "'. Accepted values: apns, gcm"

instance ToText ChannelType where
    toText = \case
        APNS -> "APNS"
        GCM -> "GCM"

instance Hashable     ChannelType
instance NFData       ChannelType
instance ToByteString ChannelType
instance ToQuery      ChannelType
instance ToHeader     ChannelType

instance ToJSON ChannelType where
    toJSON = toJSONText

instance FromJSON ChannelType where
    parseJSON = parseJSONText "ChannelType"

data DefinitionFormat
    = CSV
    | JSON
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText DefinitionFormat where
    parser = takeLowerText >>= \case
        "csv" -> pure CSV
        "json" -> pure JSON
        e -> fromTextError $ "Failure parsing DefinitionFormat from value: '" <> e
           <> "'. Accepted values: csv, json"

instance ToText DefinitionFormat where
    toText = \case
        CSV -> "CSV"
        JSON -> "JSON"

instance Hashable     DefinitionFormat
instance NFData       DefinitionFormat
instance ToByteString DefinitionFormat
instance ToQuery      DefinitionFormat
instance ToHeader     DefinitionFormat

instance ToJSON DefinitionFormat where
    toJSON = toJSONText

instance FromJSON DefinitionFormat where
    parseJSON = parseJSONText "DefinitionFormat"

data DimensionType
    = DTExclusive
    | DTInclusive
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText DimensionType where
    parser = takeLowerText >>= \case
        "exclusive" -> pure DTExclusive
        "inclusive" -> pure DTInclusive
        e -> fromTextError $ "Failure parsing DimensionType from value: '" <> e
           <> "'. Accepted values: exclusive, inclusive"

instance ToText DimensionType where
    toText = \case
        DTExclusive -> "EXCLUSIVE"
        DTInclusive -> "INCLUSIVE"

instance Hashable     DimensionType
instance NFData       DimensionType
instance ToByteString DimensionType
instance ToQuery      DimensionType
instance ToHeader     DimensionType

instance ToJSON DimensionType where
    toJSON = toJSONText

instance FromJSON DimensionType where
    parseJSON = parseJSONText "DimensionType"

data Duration
    = Day14
    | Day30
    | Day7
    | Hr24
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText Duration where
    parser = takeLowerText >>= \case
        "day_14" -> pure Day14
        "day_30" -> pure Day30
        "day_7" -> pure Day7
        "hr_24" -> pure Hr24
        e -> fromTextError $ "Failure parsing Duration from value: '" <> e
           <> "'. Accepted values: day_14, day_30, day_7, hr_24"

instance ToText Duration where
    toText = \case
        Day14 -> "DAY_14"
        Day30 -> "DAY_30"
        Day7 -> "DAY_7"
        Hr24 -> "HR_24"

instance Hashable     Duration
instance NFData       Duration
instance ToByteString Duration
instance ToQuery      Duration
instance ToHeader     Duration

instance ToJSON Duration where
    toJSON = toJSONText

instance FromJSON Duration where
    parseJSON = parseJSONText "Duration"

data Frequency
    = Daily
    | Hourly
    | Monthly
    | Once
    | Weekly
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText Frequency where
    parser = takeLowerText >>= \case
        "daily" -> pure Daily
        "hourly" -> pure Hourly
        "monthly" -> pure Monthly
        "once" -> pure Once
        "weekly" -> pure Weekly
        e -> fromTextError $ "Failure parsing Frequency from value: '" <> e
           <> "'. Accepted values: daily, hourly, monthly, once, weekly"

instance ToText Frequency where
    toText = \case
        Daily -> "DAILY"
        Hourly -> "HOURLY"
        Monthly -> "MONTHLY"
        Once -> "ONCE"
        Weekly -> "WEEKLY"

instance Hashable     Frequency
instance NFData       Frequency
instance ToByteString Frequency
instance ToQuery      Frequency
instance ToHeader     Frequency

instance ToJSON Frequency where
    toJSON = toJSONText

instance FromJSON Frequency where
    parseJSON = parseJSONText "Frequency"

data JobStatus
    = JSCompleted
    | JSCompleting
    | JSCreated
    | JSFailed
    | JSFailing
    | JSInitializing
    | JSProcessing
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText JobStatus where
    parser = takeLowerText >>= \case
        "completed" -> pure JSCompleted
        "completing" -> pure JSCompleting
        "created" -> pure JSCreated
        "failed" -> pure JSFailed
        "failing" -> pure JSFailing
        "initializing" -> pure JSInitializing
        "processing" -> pure JSProcessing
        e -> fromTextError $ "Failure parsing JobStatus from value: '" <> e
           <> "'. Accepted values: completed, completing, created, failed, failing, initializing, processing"

instance ToText JobStatus where
    toText = \case
        JSCompleted -> "COMPLETED"
        JSCompleting -> "COMPLETING"
        JSCreated -> "CREATED"
        JSFailed -> "FAILED"
        JSFailing -> "FAILING"
        JSInitializing -> "INITIALIZING"
        JSProcessing -> "PROCESSING"

instance Hashable     JobStatus
instance NFData       JobStatus
instance ToByteString JobStatus
instance ToQuery      JobStatus
instance ToHeader     JobStatus

instance FromJSON JobStatus where
    parseJSON = parseJSONText "JobStatus"

data RecencyType
    = Active
    | Inactive
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText RecencyType where
    parser = takeLowerText >>= \case
        "active" -> pure Active
        "inactive" -> pure Inactive
        e -> fromTextError $ "Failure parsing RecencyType from value: '" <> e
           <> "'. Accepted values: active, inactive"

instance ToText RecencyType where
    toText = \case
        Active -> "ACTIVE"
        Inactive -> "INACTIVE"

instance Hashable     RecencyType
instance NFData       RecencyType
instance ToByteString RecencyType
instance ToQuery      RecencyType
instance ToHeader     RecencyType

instance ToJSON RecencyType where
    toJSON = toJSONText

instance FromJSON RecencyType where
    parseJSON = parseJSONText "RecencyType"

data SegmentType
    = Dimensional
    | Import
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText SegmentType where
    parser = takeLowerText >>= \case
        "dimensional" -> pure Dimensional
        "import" -> pure Import
        e -> fromTextError $ "Failure parsing SegmentType from value: '" <> e
           <> "'. Accepted values: dimensional, import"

instance ToText SegmentType where
    toText = \case
        Dimensional -> "DIMENSIONAL"
        Import -> "IMPORT"

instance Hashable     SegmentType
instance NFData       SegmentType
instance ToByteString SegmentType
instance ToQuery      SegmentType
instance ToHeader     SegmentType

instance FromJSON SegmentType where
    parseJSON = parseJSONText "SegmentType"
