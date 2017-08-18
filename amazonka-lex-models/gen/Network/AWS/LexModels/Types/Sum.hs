{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexModels.Types.Sum
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.LexModels.Types.Sum where

import           Network.AWS.Prelude

data ChannelType
    = Facebook
    | Slack
    | TwilioSms
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

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
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

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

data FulfillmentActivityType
    = CodeHook
    | ReturnIntent
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

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
    = Building
    | Failed
    | NotBuilt
    | Ready
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText LexStatus where
    parser = takeLowerText >>= \case
        "building" -> pure Building
        "failed" -> pure Failed
        "not_built" -> pure NotBuilt
        "ready" -> pure Ready
        e -> fromTextError $ "Failure parsing LexStatus from value: '" <> e
           <> "'. Accepted values: building, failed, not_built, ready"

instance ToText LexStatus where
    toText = \case
        Building -> "BUILDING"
        Failed -> "FAILED"
        NotBuilt -> "NOT_BUILT"
        Ready -> "READY"

instance Hashable     LexStatus
instance NFData       LexStatus
instance ToByteString LexStatus
instance ToQuery      LexStatus
instance ToHeader     LexStatus

instance FromJSON LexStatus where
    parseJSON = parseJSONText "LexStatus"

data Locale =
    EnUs
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

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
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

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

data SlotConstraint
    = Optional
    | Required
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

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

data StatusType
    = Detected
    | Missed
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

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
