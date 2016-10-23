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
           <> "'. Accepted values: boolean, datetime, number, string"

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

data AuthFlowType
    = AdminNoSrpAuth
    | CustomAuth
    | RefreshToken
    | RefreshTokenAuth
    | UserSrpAuth
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText AuthFlowType where
    parser = takeLowerText >>= \case
        "admin_no_srp_auth" -> pure AdminNoSrpAuth
        "custom_auth" -> pure CustomAuth
        "refresh_token" -> pure RefreshToken
        "refresh_token_auth" -> pure RefreshTokenAuth
        "user_srp_auth" -> pure UserSrpAuth
        e -> fromTextError $ "Failure parsing AuthFlowType from value: '" <> e
           <> "'. Accepted values: admin_no_srp_auth, custom_auth, refresh_token, refresh_token_auth, user_srp_auth"

instance ToText AuthFlowType where
    toText = \case
        AdminNoSrpAuth -> "ADMIN_NO_SRP_AUTH"
        CustomAuth -> "CUSTOM_AUTH"
        RefreshToken -> "REFRESH_TOKEN"
        RefreshTokenAuth -> "REFRESH_TOKEN_AUTH"
        UserSrpAuth -> "USER_SRP_AUTH"

instance Hashable     AuthFlowType
instance NFData       AuthFlowType
instance ToByteString AuthFlowType
instance ToQuery      AuthFlowType
instance ToHeader     AuthFlowType

instance ToJSON AuthFlowType where
    toJSON = toJSONText

data ChallengeNameType
    = CNTAdminNoSrpAuth
    | CNTCustomChallenge
    | CNTDevicePasswordVerifier
    | CNTDeviceSrpAuth
    | CNTPasswordVerifier
    | CNTSmsMFA
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText ChallengeNameType where
    parser = takeLowerText >>= \case
        "admin_no_srp_auth" -> pure CNTAdminNoSrpAuth
        "custom_challenge" -> pure CNTCustomChallenge
        "device_password_verifier" -> pure CNTDevicePasswordVerifier
        "device_srp_auth" -> pure CNTDeviceSrpAuth
        "password_verifier" -> pure CNTPasswordVerifier
        "sms_mfa" -> pure CNTSmsMFA
        e -> fromTextError $ "Failure parsing ChallengeNameType from value: '" <> e
           <> "'. Accepted values: admin_no_srp_auth, custom_challenge, device_password_verifier, device_srp_auth, password_verifier, sms_mfa"

instance ToText ChallengeNameType where
    toText = \case
        CNTAdminNoSrpAuth -> "ADMIN_NO_SRP_AUTH"
        CNTCustomChallenge -> "CUSTOM_CHALLENGE"
        CNTDevicePasswordVerifier -> "DEVICE_PASSWORD_VERIFIER"
        CNTDeviceSrpAuth -> "DEVICE_SRP_AUTH"
        CNTPasswordVerifier -> "PASSWORD_VERIFIER"
        CNTSmsMFA -> "SMS_MFA"

instance Hashable     ChallengeNameType
instance NFData       ChallengeNameType
instance ToByteString ChallengeNameType
instance ToQuery      ChallengeNameType
instance ToHeader     ChallengeNameType

instance ToJSON ChallengeNameType where
    toJSON = toJSONText

instance FromJSON ChallengeNameType where
    parseJSON = parseJSONText "ChallengeNameType"

data DeliveryMediumType
    = DMTEmail
    | DMTSms
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText DeliveryMediumType where
    parser = takeLowerText >>= \case
        "email" -> pure DMTEmail
        "sms" -> pure DMTSms
        e -> fromTextError $ "Failure parsing DeliveryMediumType from value: '" <> e
           <> "'. Accepted values: email, sms"

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

data DeviceRememberedStatusType
    = NotRemembered
    | Remembered
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText DeviceRememberedStatusType where
    parser = takeLowerText >>= \case
        "not_remembered" -> pure NotRemembered
        "remembered" -> pure Remembered
        e -> fromTextError $ "Failure parsing DeviceRememberedStatusType from value: '" <> e
           <> "'. Accepted values: not_remembered, remembered"

instance ToText DeviceRememberedStatusType where
    toText = \case
        NotRemembered -> "not_remembered"
        Remembered -> "remembered"

instance Hashable     DeviceRememberedStatusType
instance NFData       DeviceRememberedStatusType
instance ToByteString DeviceRememberedStatusType
instance ToQuery      DeviceRememberedStatusType
instance ToHeader     DeviceRememberedStatusType

instance ToJSON DeviceRememberedStatusType where
    toJSON = toJSONText

data ExplicitAuthFlowsType
    = EAFTAdminNoSrpAuth
    | EAFTCustomAuthFlowOnly
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText ExplicitAuthFlowsType where
    parser = takeLowerText >>= \case
        "admin_no_srp_auth" -> pure EAFTAdminNoSrpAuth
        "custom_auth_flow_only" -> pure EAFTCustomAuthFlowOnly
        e -> fromTextError $ "Failure parsing ExplicitAuthFlowsType from value: '" <> e
           <> "'. Accepted values: admin_no_srp_auth, custom_auth_flow_only"

instance ToText ExplicitAuthFlowsType where
    toText = \case
        EAFTAdminNoSrpAuth -> "ADMIN_NO_SRP_AUTH"
        EAFTCustomAuthFlowOnly -> "CUSTOM_AUTH_FLOW_ONLY"

instance Hashable     ExplicitAuthFlowsType
instance NFData       ExplicitAuthFlowsType
instance ToByteString ExplicitAuthFlowsType
instance ToQuery      ExplicitAuthFlowsType
instance ToHeader     ExplicitAuthFlowsType

instance ToJSON ExplicitAuthFlowsType where
    toJSON = toJSONText

instance FromJSON ExplicitAuthFlowsType where
    parseJSON = parseJSONText "ExplicitAuthFlowsType"

data StatusType
    = Disabled
    | Enabled
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText StatusType where
    parser = takeLowerText >>= \case
        "disabled" -> pure Disabled
        "enabled" -> pure Enabled
        e -> fromTextError $ "Failure parsing StatusType from value: '" <> e
           <> "'. Accepted values: disabled, enabled"

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

data UserImportJobStatusType
    = Created
    | Expired
    | Failed
    | InProgress
    | Pending
    | Stopped
    | Stopping
    | Succeeded
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText UserImportJobStatusType where
    parser = takeLowerText >>= \case
        "created" -> pure Created
        "expired" -> pure Expired
        "failed" -> pure Failed
        "inprogress" -> pure InProgress
        "pending" -> pure Pending
        "stopped" -> pure Stopped
        "stopping" -> pure Stopping
        "succeeded" -> pure Succeeded
        e -> fromTextError $ "Failure parsing UserImportJobStatusType from value: '" <> e
           <> "'. Accepted values: created, expired, failed, inprogress, pending, stopped, stopping, succeeded"

instance ToText UserImportJobStatusType where
    toText = \case
        Created -> "Created"
        Expired -> "Expired"
        Failed -> "Failed"
        InProgress -> "InProgress"
        Pending -> "Pending"
        Stopped -> "Stopped"
        Stopping -> "Stopping"
        Succeeded -> "Succeeded"

instance Hashable     UserImportJobStatusType
instance NFData       UserImportJobStatusType
instance ToByteString UserImportJobStatusType
instance ToQuery      UserImportJobStatusType
instance ToHeader     UserImportJobStatusType

instance FromJSON UserImportJobStatusType where
    parseJSON = parseJSONText "UserImportJobStatusType"

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
           <> "'. Accepted values: on, off, optional"

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
    | ResetRequired
    | Unconfirmed
    | Unknown
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText UserStatusType where
    parser = takeLowerText >>= \case
        "archived" -> pure Archived
        "compromised" -> pure Compromised
        "confirmed" -> pure Confirmed
        "reset_required" -> pure ResetRequired
        "unconfirmed" -> pure Unconfirmed
        "unknown" -> pure Unknown
        e -> fromTextError $ "Failure parsing UserStatusType from value: '" <> e
           <> "'. Accepted values: archived, compromised, confirmed, reset_required, unconfirmed, unknown"

instance ToText UserStatusType where
    toText = \case
        Archived -> "ARCHIVED"
        Compromised -> "COMPROMISED"
        Confirmed -> "CONFIRMED"
        ResetRequired -> "RESET_REQUIRED"
        Unconfirmed -> "UNCONFIRMED"
        Unknown -> "UNKNOWN"

instance Hashable     UserStatusType
instance NFData       UserStatusType
instance ToByteString UserStatusType
instance ToQuery      UserStatusType
instance ToHeader     UserStatusType

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
