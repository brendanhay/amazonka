{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexModels.Types.Sum
-- Copyright   : (c) 2013-2017 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.LexModels.Types.Sum where

import Network.AWS.Prelude

data ChannelType
  = Facebook
  | Slack
  | TwilioSms
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ChannelType where
    parser = takeLowerText >>= \case
        "facebook" -> pure Facebook
        "slack" -> pure Slack
        "twilio-sms" -> pure TwilioSms
        e -> fromTextError $ "Failure parsing ChannelType from value: '" <> e
           <> "'. Accepted values: facebook, slack, twilio-sms"

instance ToText ChannelType where
    toText = \case
        Facebook -> "Facebook"
        Slack -> "Slack"
        TwilioSms -> "Twilio-Sms"

instance Hashable     ChannelType
instance NFData       ChannelType
instance ToByteString ChannelType
instance ToQuery      ChannelType
instance ToHeader     ChannelType

instance FromJSON ChannelType where
    parseJSON = parseJSONText "ChannelType"

data ContentType
  = PlainText
  | Ssml
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ContentType where
    parser = takeLowerText >>= \case
        "plaintext" -> pure PlainText
        "ssml" -> pure Ssml
        e -> fromTextError $ "Failure parsing ContentType from value: '" <> e
           <> "'. Accepted values: plaintext, ssml"

instance ToText ContentType where
    toText = \case
        PlainText -> "PlainText"
        Ssml -> "SSML"

instance Hashable     ContentType
instance NFData       ContentType
instance ToByteString ContentType
instance ToQuery      ContentType
instance ToHeader     ContentType

instance ToJSON ContentType where
    toJSON = toJSONText

instance FromJSON ContentType where
    parseJSON = parseJSONText "ContentType"

data ExportStatus
  = Failed
  | InProgress
  | Ready
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ExportStatus where
    parser = takeLowerText >>= \case
        "failed" -> pure Failed
        "in_progress" -> pure InProgress
        "ready" -> pure Ready
        e -> fromTextError $ "Failure parsing ExportStatus from value: '" <> e
           <> "'. Accepted values: failed, in_progress, ready"

instance ToText ExportStatus where
    toText = \case
        Failed -> "FAILED"
        InProgress -> "IN_PROGRESS"
        Ready -> "READY"

instance Hashable     ExportStatus
instance NFData       ExportStatus
instance ToByteString ExportStatus
instance ToQuery      ExportStatus
instance ToHeader     ExportStatus

instance FromJSON ExportStatus where
    parseJSON = parseJSONText "ExportStatus"

data ExportType =
  AlexaSkillsKit
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ExportType where
    parser = takeLowerText >>= \case
        "alexa_skills_kit" -> pure AlexaSkillsKit
        e -> fromTextError $ "Failure parsing ExportType from value: '" <> e
           <> "'. Accepted values: alexa_skills_kit"

instance ToText ExportType where
    toText = \case
        AlexaSkillsKit -> "ALEXA_SKILLS_KIT"

instance Hashable     ExportType
instance NFData       ExportType
instance ToByteString ExportType
instance ToQuery      ExportType
instance ToHeader     ExportType

instance ToJSON ExportType where
    toJSON = toJSONText

instance FromJSON ExportType where
    parseJSON = parseJSONText "ExportType"

data FulfillmentActivityType
  = CodeHook
  | ReturnIntent
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText FulfillmentActivityType where
    parser = takeLowerText >>= \case
        "codehook" -> pure CodeHook
        "returnintent" -> pure ReturnIntent
        e -> fromTextError $ "Failure parsing FulfillmentActivityType from value: '" <> e
           <> "'. Accepted values: codehook, returnintent"

instance ToText FulfillmentActivityType where
    toText = \case
        CodeHook -> "CodeHook"
        ReturnIntent -> "ReturnIntent"

instance Hashable     FulfillmentActivityType
instance NFData       FulfillmentActivityType
instance ToByteString FulfillmentActivityType
instance ToQuery      FulfillmentActivityType
instance ToHeader     FulfillmentActivityType

instance ToJSON FulfillmentActivityType where
    toJSON = toJSONText

instance FromJSON FulfillmentActivityType where
    parseJSON = parseJSONText "FulfillmentActivityType"

data LexStatus
  = LSBuilding
  | LSFailed
  | LSNotBuilt
  | LSReady
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText LexStatus where
    parser = takeLowerText >>= \case
        "building" -> pure LSBuilding
        "failed" -> pure LSFailed
        "not_built" -> pure LSNotBuilt
        "ready" -> pure LSReady
        e -> fromTextError $ "Failure parsing LexStatus from value: '" <> e
           <> "'. Accepted values: building, failed, not_built, ready"

instance ToText LexStatus where
    toText = \case
        LSBuilding -> "BUILDING"
        LSFailed -> "FAILED"
        LSNotBuilt -> "NOT_BUILT"
        LSReady -> "READY"

instance Hashable     LexStatus
instance NFData       LexStatus
instance ToByteString LexStatus
instance ToQuery      LexStatus
instance ToHeader     LexStatus

instance FromJSON LexStatus where
    parseJSON = parseJSONText "LexStatus"

data Locale =
  EnUs
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText Locale where
    parser = takeLowerText >>= \case
        "en-us" -> pure EnUs
        e -> fromTextError $ "Failure parsing Locale from value: '" <> e
           <> "'. Accepted values: en-us"

instance ToText Locale where
    toText = \case
        EnUs -> "en-US"

instance Hashable     Locale
instance NFData       Locale
instance ToByteString Locale
instance ToQuery      Locale
instance ToHeader     Locale

instance ToJSON Locale where
    toJSON = toJSONText

instance FromJSON Locale where
    parseJSON = parseJSONText "Locale"

data ProcessBehavior
  = Build
  | Save
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ProcessBehavior where
    parser = takeLowerText >>= \case
        "build" -> pure Build
        "save" -> pure Save
        e -> fromTextError $ "Failure parsing ProcessBehavior from value: '" <> e
           <> "'. Accepted values: build, save"

instance ToText ProcessBehavior where
    toText = \case
        Build -> "BUILD"
        Save -> "SAVE"

instance Hashable     ProcessBehavior
instance NFData       ProcessBehavior
instance ToByteString ProcessBehavior
instance ToQuery      ProcessBehavior
instance ToHeader     ProcessBehavior

instance ToJSON ProcessBehavior where
    toJSON = toJSONText

data ResourceType =
  Bot
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ResourceType where
    parser = takeLowerText >>= \case
        "bot" -> pure Bot
        e -> fromTextError $ "Failure parsing ResourceType from value: '" <> e
           <> "'. Accepted values: bot"

instance ToText ResourceType where
    toText = \case
        Bot -> "BOT"

instance Hashable     ResourceType
instance NFData       ResourceType
instance ToByteString ResourceType
instance ToQuery      ResourceType
instance ToHeader     ResourceType

instance ToJSON ResourceType where
    toJSON = toJSONText

instance FromJSON ResourceType where
    parseJSON = parseJSONText "ResourceType"

data SlotConstraint
  = Optional
  | Required
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText SlotConstraint where
    parser = takeLowerText >>= \case
        "optional" -> pure Optional
        "required" -> pure Required
        e -> fromTextError $ "Failure parsing SlotConstraint from value: '" <> e
           <> "'. Accepted values: optional, required"

instance ToText SlotConstraint where
    toText = \case
        Optional -> "Optional"
        Required -> "Required"

instance Hashable     SlotConstraint
instance NFData       SlotConstraint
instance ToByteString SlotConstraint
instance ToQuery      SlotConstraint
instance ToHeader     SlotConstraint

instance ToJSON SlotConstraint where
    toJSON = toJSONText

instance FromJSON SlotConstraint where
    parseJSON = parseJSONText "SlotConstraint"

data SlotValueSelectionStrategy
  = OriginalValue
  | TopResolution
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText SlotValueSelectionStrategy where
    parser = takeLowerText >>= \case
        "original_value" -> pure OriginalValue
        "top_resolution" -> pure TopResolution
        e -> fromTextError $ "Failure parsing SlotValueSelectionStrategy from value: '" <> e
           <> "'. Accepted values: original_value, top_resolution"

instance ToText SlotValueSelectionStrategy where
    toText = \case
        OriginalValue -> "ORIGINAL_VALUE"
        TopResolution -> "TOP_RESOLUTION"

instance Hashable     SlotValueSelectionStrategy
instance NFData       SlotValueSelectionStrategy
instance ToByteString SlotValueSelectionStrategy
instance ToQuery      SlotValueSelectionStrategy
instance ToHeader     SlotValueSelectionStrategy

instance ToJSON SlotValueSelectionStrategy where
    toJSON = toJSONText

instance FromJSON SlotValueSelectionStrategy where
    parseJSON = parseJSONText "SlotValueSelectionStrategy"

data StatusType
  = Detected
  | Missed
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText StatusType where
    parser = takeLowerText >>= \case
        "detected" -> pure Detected
        "missed" -> pure Missed
        e -> fromTextError $ "Failure parsing StatusType from value: '" <> e
           <> "'. Accepted values: detected, missed"

instance ToText StatusType where
    toText = \case
        Detected -> "Detected"
        Missed -> "Missed"

instance Hashable     StatusType
instance NFData       StatusType
instance ToByteString StatusType
instance ToQuery      StatusType
instance ToHeader     StatusType

instance ToJSON StatusType where
    toJSON = toJSONText
