{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.Sum
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CognitoIdentityProvider.Types.Sum where

import           Network.AWS.Prelude

data AliasAttributeType
    = AATEmail
    | AATPhoneNumber
    | AATPreferredUsername
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText AliasAttributeType where
    parser = takeLowerText >>= \case
        "email" -> pure AATEmail
        "phone_number" -> pure AATPhoneNumber
        "preferred_username" -> pure AATPreferredUsername
        e -> fromTextError $ "Failure parsing AliasAttributeType from value: '" <> e
           <> "'. Accepted values: email, phone_number, preferred_username"

instance ToText AliasAttributeType where
    toText = \case
        AATEmail -> "email"
        AATPhoneNumber -> "phone_number"
        AATPreferredUsername -> "preferred_username"

instance Hashable     AliasAttributeType
instance NFData       AliasAttributeType
instance ToByteString AliasAttributeType
instance ToQuery      AliasAttributeType
instance ToHeader     AliasAttributeType

instance ToJSON AliasAttributeType where
    toJSON = toJSONText

instance FromJSON AliasAttributeType where
    parseJSON = parseJSONText "AliasAttributeType"

data AttributeDataType
    = Boolean
    | DateTime
    | Number
    | String
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText AttributeDataType where
    parser = takeLowerText >>= \case
        "boolean" -> pure Boolean
        "datetime" -> pure DateTime
        "number" -> pure Number
        "string" -> pure String
        e -> fromTextError $ "Failure parsing AttributeDataType from value: '" <> e
           <> "'. Accepted values: Boolean, DateTime, Number, String"

instance ToText AttributeDataType where
    toText = \case
        Boolean -> "Boolean"
        DateTime -> "DateTime"
        Number -> "Number"
        String -> "String"

instance Hashable     AttributeDataType
instance NFData       AttributeDataType
instance ToByteString AttributeDataType
instance ToQuery      AttributeDataType
instance ToHeader     AttributeDataType

instance ToJSON AttributeDataType where
    toJSON = toJSONText

instance FromJSON AttributeDataType where
    parseJSON = parseJSONText "AttributeDataType"

data DeliveryMediumType
    = DMTEmail
    | DMTSms
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText DeliveryMediumType where
    parser = takeLowerText >>= \case
        "email" -> pure DMTEmail
        "sms" -> pure DMTSms
        e -> fromTextError $ "Failure parsing DeliveryMediumType from value: '" <> e
           <> "'. Accepted values: EMAIL, SMS"

instance ToText DeliveryMediumType where
    toText = \case
        DMTEmail -> "EMAIL"
        DMTSms -> "SMS"

instance Hashable     DeliveryMediumType
instance NFData       DeliveryMediumType
instance ToByteString DeliveryMediumType
instance ToQuery      DeliveryMediumType
instance ToHeader     DeliveryMediumType

instance ToJSON DeliveryMediumType where
    toJSON = toJSONText

instance FromJSON DeliveryMediumType where
    parseJSON = parseJSONText "DeliveryMediumType"

data StatusType
    = Disabled
    | Enabled
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText StatusType where
    parser = takeLowerText >>= \case
        "disabled" -> pure Disabled
        "enabled" -> pure Enabled
        e -> fromTextError $ "Failure parsing StatusType from value: '" <> e
           <> "'. Accepted values: Disabled, Enabled"

instance ToText StatusType where
    toText = \case
        Disabled -> "Disabled"
        Enabled -> "Enabled"

instance Hashable     StatusType
instance NFData       StatusType
instance ToByteString StatusType
instance ToQuery      StatusType
instance ToHeader     StatusType

instance FromJSON StatusType where
    parseJSON = parseJSONText "StatusType"

data UserPoolMFAType
    = ON
    | Off
    | Optional
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText UserPoolMFAType where
    parser = takeLowerText >>= \case
        "on" -> pure ON
        "off" -> pure Off
        "optional" -> pure Optional
        e -> fromTextError $ "Failure parsing UserPoolMFAType from value: '" <> e
           <> "'. Accepted values: ON, OFF, OPTIONAL"

instance ToText UserPoolMFAType where
    toText = \case
        ON -> "ON"
        Off -> "OFF"
        Optional -> "OPTIONAL"

instance Hashable     UserPoolMFAType
instance NFData       UserPoolMFAType
instance ToByteString UserPoolMFAType
instance ToQuery      UserPoolMFAType
instance ToHeader     UserPoolMFAType

instance ToJSON UserPoolMFAType where
    toJSON = toJSONText

instance FromJSON UserPoolMFAType where
    parseJSON = parseJSONText "UserPoolMFAType"

data UserStatusType
    = Archived
    | Compromised
    | Confirmed
    | Unconfirmed
    | Unknown
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText UserStatusType where
    parser = takeLowerText >>= \case
        "archived" -> pure Archived
        "compromised" -> pure Compromised
        "confirmed" -> pure Confirmed
        "unconfirmed" -> pure Unconfirmed
        "unknown" -> pure Unknown
        e -> fromTextError $ "Failure parsing UserStatusType from value: '" <> e
           <> "'. Accepted values: ARCHIVED, COMPROMISED, CONFIRMED, UNCONFIRMED, UNKNOWN"

instance ToText UserStatusType where
    toText = \case
        Archived -> "ARCHIVED"
        Compromised -> "COMPROMISED"
        Confirmed -> "CONFIRMED"
        Unconfirmed -> "UNCONFIRMED"
        Unknown -> "UNKNOWN"

instance Hashable     UserStatusType
instance NFData       UserStatusType
instance ToByteString UserStatusType
instance ToQuery      UserStatusType
instance ToHeader     UserStatusType

instance ToJSON UserStatusType where
    toJSON = toJSONText

instance FromJSON UserStatusType where
    parseJSON = parseJSONText "UserStatusType"

data VerifiedAttributeType
    = Email
    | PhoneNumber
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText VerifiedAttributeType where
    parser = takeLowerText >>= \case
        "email" -> pure Email
        "phone_number" -> pure PhoneNumber
        e -> fromTextError $ "Failure parsing VerifiedAttributeType from value: '" <> e
           <> "'. Accepted values: email, phone_number"

instance ToText VerifiedAttributeType where
    toText = \case
        Email -> "email"
        PhoneNumber -> "phone_number"

instance Hashable     VerifiedAttributeType
instance NFData       VerifiedAttributeType
instance ToByteString VerifiedAttributeType
instance ToQuery      VerifiedAttributeType
instance ToHeader     VerifiedAttributeType

instance ToJSON VerifiedAttributeType where
    toJSON = toJSONText

instance FromJSON VerifiedAttributeType where
    parseJSON = parseJSONText "VerifiedAttributeType"
