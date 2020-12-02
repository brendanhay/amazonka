{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.Sum
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CognitoIdentityProvider.Types.Sum where

import Network.AWS.Prelude

data AccountTakeoverEventActionType
  = ATEATBlock
  | ATEATMFAIfConfigured
  | ATEATMFARequired
  | ATEATNoAction
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText AccountTakeoverEventActionType where
    parser = takeLowerText >>= \case
        "block" -> pure ATEATBlock
        "mfa_if_configured" -> pure ATEATMFAIfConfigured
        "mfa_required" -> pure ATEATMFARequired
        "no_action" -> pure ATEATNoAction
        e -> fromTextError $ "Failure parsing AccountTakeoverEventActionType from value: '" <> e
           <> "'. Accepted values: block, mfa_if_configured, mfa_required, no_action"

instance ToText AccountTakeoverEventActionType where
    toText = \case
        ATEATBlock -> "BLOCK"
        ATEATMFAIfConfigured -> "MFA_IF_CONFIGURED"
        ATEATMFARequired -> "MFA_REQUIRED"
        ATEATNoAction -> "NO_ACTION"

instance Hashable     AccountTakeoverEventActionType
instance NFData       AccountTakeoverEventActionType
instance ToByteString AccountTakeoverEventActionType
instance ToQuery      AccountTakeoverEventActionType
instance ToHeader     AccountTakeoverEventActionType

instance ToJSON AccountTakeoverEventActionType where
    toJSON = toJSONText

instance FromJSON AccountTakeoverEventActionType where
    parseJSON = parseJSONText "AccountTakeoverEventActionType"

data AdvancedSecurityModeType
  = Audit
  | Enforced
  | Off
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText AdvancedSecurityModeType where
    parser = takeLowerText >>= \case
        "audit" -> pure Audit
        "enforced" -> pure Enforced
        "off" -> pure Off
        e -> fromTextError $ "Failure parsing AdvancedSecurityModeType from value: '" <> e
           <> "'. Accepted values: audit, enforced, off"

instance ToText AdvancedSecurityModeType where
    toText = \case
        Audit -> "AUDIT"
        Enforced -> "ENFORCED"
        Off -> "OFF"

instance Hashable     AdvancedSecurityModeType
instance NFData       AdvancedSecurityModeType
instance ToByteString AdvancedSecurityModeType
instance ToQuery      AdvancedSecurityModeType
instance ToHeader     AdvancedSecurityModeType

instance ToJSON AdvancedSecurityModeType where
    toJSON = toJSONText

instance FromJSON AdvancedSecurityModeType where
    parseJSON = parseJSONText "AdvancedSecurityModeType"

data AliasAttributeType
  = AATEmail
  | AATPhoneNumber
  | AATPreferredUsername
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


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
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


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
  | UserPasswordAuth
  | UserSrpAuth
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText AuthFlowType where
    parser = takeLowerText >>= \case
        "admin_no_srp_auth" -> pure AdminNoSrpAuth
        "custom_auth" -> pure CustomAuth
        "refresh_token" -> pure RefreshToken
        "refresh_token_auth" -> pure RefreshTokenAuth
        "user_password_auth" -> pure UserPasswordAuth
        "user_srp_auth" -> pure UserSrpAuth
        e -> fromTextError $ "Failure parsing AuthFlowType from value: '" <> e
           <> "'. Accepted values: admin_no_srp_auth, custom_auth, refresh_token, refresh_token_auth, user_password_auth, user_srp_auth"

instance ToText AuthFlowType where
    toText = \case
        AdminNoSrpAuth -> "ADMIN_NO_SRP_AUTH"
        CustomAuth -> "CUSTOM_AUTH"
        RefreshToken -> "REFRESH_TOKEN"
        RefreshTokenAuth -> "REFRESH_TOKEN_AUTH"
        UserPasswordAuth -> "USER_PASSWORD_AUTH"
        UserSrpAuth -> "USER_SRP_AUTH"

instance Hashable     AuthFlowType
instance NFData       AuthFlowType
instance ToByteString AuthFlowType
instance ToQuery      AuthFlowType
instance ToHeader     AuthFlowType

instance ToJSON AuthFlowType where
    toJSON = toJSONText

data ChallengeName
  = MFA
  | Password
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ChallengeName where
    parser = takeLowerText >>= \case
        "mfa" -> pure MFA
        "password" -> pure Password
        e -> fromTextError $ "Failure parsing ChallengeName from value: '" <> e
           <> "'. Accepted values: mfa, password"

instance ToText ChallengeName where
    toText = \case
        MFA -> "Mfa"
        Password -> "Password"

instance Hashable     ChallengeName
instance NFData       ChallengeName
instance ToByteString ChallengeName
instance ToQuery      ChallengeName
instance ToHeader     ChallengeName

instance FromJSON ChallengeName where
    parseJSON = parseJSONText "ChallengeName"

data ChallengeNameType
  = CNTAdminNoSrpAuth
  | CNTCustomChallenge
  | CNTDevicePasswordVerifier
  | CNTDeviceSrpAuth
  | CNTMFASetup
  | CNTNewPasswordRequired
  | CNTPasswordVerifier
  | CNTSelectMFAType
  | CNTSmsMFA
  | CNTSoftwareTokenMFA
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ChallengeNameType where
    parser = takeLowerText >>= \case
        "admin_no_srp_auth" -> pure CNTAdminNoSrpAuth
        "custom_challenge" -> pure CNTCustomChallenge
        "device_password_verifier" -> pure CNTDevicePasswordVerifier
        "device_srp_auth" -> pure CNTDeviceSrpAuth
        "mfa_setup" -> pure CNTMFASetup
        "new_password_required" -> pure CNTNewPasswordRequired
        "password_verifier" -> pure CNTPasswordVerifier
        "select_mfa_type" -> pure CNTSelectMFAType
        "sms_mfa" -> pure CNTSmsMFA
        "software_token_mfa" -> pure CNTSoftwareTokenMFA
        e -> fromTextError $ "Failure parsing ChallengeNameType from value: '" <> e
           <> "'. Accepted values: admin_no_srp_auth, custom_challenge, device_password_verifier, device_srp_auth, mfa_setup, new_password_required, password_verifier, select_mfa_type, sms_mfa, software_token_mfa"

instance ToText ChallengeNameType where
    toText = \case
        CNTAdminNoSrpAuth -> "ADMIN_NO_SRP_AUTH"
        CNTCustomChallenge -> "CUSTOM_CHALLENGE"
        CNTDevicePasswordVerifier -> "DEVICE_PASSWORD_VERIFIER"
        CNTDeviceSrpAuth -> "DEVICE_SRP_AUTH"
        CNTMFASetup -> "MFA_SETUP"
        CNTNewPasswordRequired -> "NEW_PASSWORD_REQUIRED"
        CNTPasswordVerifier -> "PASSWORD_VERIFIER"
        CNTSelectMFAType -> "SELECT_MFA_TYPE"
        CNTSmsMFA -> "SMS_MFA"
        CNTSoftwareTokenMFA -> "SOFTWARE_TOKEN_MFA"

instance Hashable     ChallengeNameType
instance NFData       ChallengeNameType
instance ToByteString ChallengeNameType
instance ToQuery      ChallengeNameType
instance ToHeader     ChallengeNameType

instance ToJSON ChallengeNameType where
    toJSON = toJSONText

instance FromJSON ChallengeNameType where
    parseJSON = parseJSONText "ChallengeNameType"

data ChallengeResponse
  = CFailure
  | CSuccess
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ChallengeResponse where
    parser = takeLowerText >>= \case
        "failure" -> pure CFailure
        "success" -> pure CSuccess
        e -> fromTextError $ "Failure parsing ChallengeResponse from value: '" <> e
           <> "'. Accepted values: failure, success"

instance ToText ChallengeResponse where
    toText = \case
        CFailure -> "Failure"
        CSuccess -> "Success"

instance Hashable     ChallengeResponse
instance NFData       ChallengeResponse
instance ToByteString ChallengeResponse
instance ToQuery      ChallengeResponse
instance ToHeader     ChallengeResponse

instance FromJSON ChallengeResponse where
    parseJSON = parseJSONText "ChallengeResponse"

data CompromisedCredentialsEventActionType
  = CCEATBlock
  | CCEATNoAction
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText CompromisedCredentialsEventActionType where
    parser = takeLowerText >>= \case
        "block" -> pure CCEATBlock
        "no_action" -> pure CCEATNoAction
        e -> fromTextError $ "Failure parsing CompromisedCredentialsEventActionType from value: '" <> e
           <> "'. Accepted values: block, no_action"

instance ToText CompromisedCredentialsEventActionType where
    toText = \case
        CCEATBlock -> "BLOCK"
        CCEATNoAction -> "NO_ACTION"

instance Hashable     CompromisedCredentialsEventActionType
instance NFData       CompromisedCredentialsEventActionType
instance ToByteString CompromisedCredentialsEventActionType
instance ToQuery      CompromisedCredentialsEventActionType
instance ToHeader     CompromisedCredentialsEventActionType

instance ToJSON CompromisedCredentialsEventActionType where
    toJSON = toJSONText

instance FromJSON CompromisedCredentialsEventActionType where
    parseJSON = parseJSONText "CompromisedCredentialsEventActionType"

data DefaultEmailOptionType
  = ConfirmWithCode
  | ConfirmWithLink
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText DefaultEmailOptionType where
    parser = takeLowerText >>= \case
        "confirm_with_code" -> pure ConfirmWithCode
        "confirm_with_link" -> pure ConfirmWithLink
        e -> fromTextError $ "Failure parsing DefaultEmailOptionType from value: '" <> e
           <> "'. Accepted values: confirm_with_code, confirm_with_link"

instance ToText DefaultEmailOptionType where
    toText = \case
        ConfirmWithCode -> "CONFIRM_WITH_CODE"
        ConfirmWithLink -> "CONFIRM_WITH_LINK"

instance Hashable     DefaultEmailOptionType
instance NFData       DefaultEmailOptionType
instance ToByteString DefaultEmailOptionType
instance ToQuery      DefaultEmailOptionType
instance ToHeader     DefaultEmailOptionType

instance ToJSON DefaultEmailOptionType where
    toJSON = toJSONText

instance FromJSON DefaultEmailOptionType where
    parseJSON = parseJSONText "DefaultEmailOptionType"

data DeliveryMediumType
  = DMTEmail
  | DMTSms
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


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
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


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

data DomainStatusType
  = DSTActive
  | DSTCreating
  | DSTDeleting
  | DSTFailed
  | DSTUpdating
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText DomainStatusType where
    parser = takeLowerText >>= \case
        "active" -> pure DSTActive
        "creating" -> pure DSTCreating
        "deleting" -> pure DSTDeleting
        "failed" -> pure DSTFailed
        "updating" -> pure DSTUpdating
        e -> fromTextError $ "Failure parsing DomainStatusType from value: '" <> e
           <> "'. Accepted values: active, creating, deleting, failed, updating"

instance ToText DomainStatusType where
    toText = \case
        DSTActive -> "ACTIVE"
        DSTCreating -> "CREATING"
        DSTDeleting -> "DELETING"
        DSTFailed -> "FAILED"
        DSTUpdating -> "UPDATING"

instance Hashable     DomainStatusType
instance NFData       DomainStatusType
instance ToByteString DomainStatusType
instance ToQuery      DomainStatusType
instance ToHeader     DomainStatusType

instance FromJSON DomainStatusType where
    parseJSON = parseJSONText "DomainStatusType"

data EventFilterType
  = PasswordChange
  | SignIn
  | SignUp
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText EventFilterType where
    parser = takeLowerText >>= \case
        "password_change" -> pure PasswordChange
        "sign_in" -> pure SignIn
        "sign_up" -> pure SignUp
        e -> fromTextError $ "Failure parsing EventFilterType from value: '" <> e
           <> "'. Accepted values: password_change, sign_in, sign_up"

instance ToText EventFilterType where
    toText = \case
        PasswordChange -> "PASSWORD_CHANGE"
        SignIn -> "SIGN_IN"
        SignUp -> "SIGN_UP"

instance Hashable     EventFilterType
instance NFData       EventFilterType
instance ToByteString EventFilterType
instance ToQuery      EventFilterType
instance ToHeader     EventFilterType

instance ToJSON EventFilterType where
    toJSON = toJSONText

instance FromJSON EventFilterType where
    parseJSON = parseJSONText "EventFilterType"

data EventResponseType
  = Failure
  | Success
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText EventResponseType where
    parser = takeLowerText >>= \case
        "failure" -> pure Failure
        "success" -> pure Success
        e -> fromTextError $ "Failure parsing EventResponseType from value: '" <> e
           <> "'. Accepted values: failure, success"

instance ToText EventResponseType where
    toText = \case
        Failure -> "Failure"
        Success -> "Success"

instance Hashable     EventResponseType
instance NFData       EventResponseType
instance ToByteString EventResponseType
instance ToQuery      EventResponseType
instance ToHeader     EventResponseType

instance FromJSON EventResponseType where
    parseJSON = parseJSONText "EventResponseType"

data EventType
  = ETForgotPassword
  | ETSignIn
  | ETSignUp
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText EventType where
    parser = takeLowerText >>= \case
        "forgotpassword" -> pure ETForgotPassword
        "signin" -> pure ETSignIn
        "signup" -> pure ETSignUp
        e -> fromTextError $ "Failure parsing EventType from value: '" <> e
           <> "'. Accepted values: forgotpassword, signin, signup"

instance ToText EventType where
    toText = \case
        ETForgotPassword -> "ForgotPassword"
        ETSignIn -> "SignIn"
        ETSignUp -> "SignUp"

instance Hashable     EventType
instance NFData       EventType
instance ToByteString EventType
instance ToQuery      EventType
instance ToHeader     EventType

instance FromJSON EventType where
    parseJSON = parseJSONText "EventType"

data ExplicitAuthFlowsType
  = EAFTAdminNoSrpAuth
  | EAFTCustomAuthFlowOnly
  | EAFTUserPasswordAuth
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ExplicitAuthFlowsType where
    parser = takeLowerText >>= \case
        "admin_no_srp_auth" -> pure EAFTAdminNoSrpAuth
        "custom_auth_flow_only" -> pure EAFTCustomAuthFlowOnly
        "user_password_auth" -> pure EAFTUserPasswordAuth
        e -> fromTextError $ "Failure parsing ExplicitAuthFlowsType from value: '" <> e
           <> "'. Accepted values: admin_no_srp_auth, custom_auth_flow_only, user_password_auth"

instance ToText ExplicitAuthFlowsType where
    toText = \case
        EAFTAdminNoSrpAuth -> "ADMIN_NO_SRP_AUTH"
        EAFTCustomAuthFlowOnly -> "CUSTOM_AUTH_FLOW_ONLY"
        EAFTUserPasswordAuth -> "USER_PASSWORD_AUTH"

instance Hashable     ExplicitAuthFlowsType
instance NFData       ExplicitAuthFlowsType
instance ToByteString ExplicitAuthFlowsType
instance ToQuery      ExplicitAuthFlowsType
instance ToHeader     ExplicitAuthFlowsType

instance ToJSON ExplicitAuthFlowsType where
    toJSON = toJSONText

instance FromJSON ExplicitAuthFlowsType where
    parseJSON = parseJSONText "ExplicitAuthFlowsType"

data FeedbackValueType
  = Invalid
  | Valid
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText FeedbackValueType where
    parser = takeLowerText >>= \case
        "invalid" -> pure Invalid
        "valid" -> pure Valid
        e -> fromTextError $ "Failure parsing FeedbackValueType from value: '" <> e
           <> "'. Accepted values: invalid, valid"

instance ToText FeedbackValueType where
    toText = \case
        Invalid -> "Invalid"
        Valid -> "Valid"

instance Hashable     FeedbackValueType
instance NFData       FeedbackValueType
instance ToByteString FeedbackValueType
instance ToQuery      FeedbackValueType
instance ToHeader     FeedbackValueType

instance ToJSON FeedbackValueType where
    toJSON = toJSONText

instance FromJSON FeedbackValueType where
    parseJSON = parseJSONText "FeedbackValueType"

data IdentityProviderTypeType
  = Facebook
  | Google
  | LoginWithAmazon
  | Saml
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText IdentityProviderTypeType where
    parser = takeLowerText >>= \case
        "facebook" -> pure Facebook
        "google" -> pure Google
        "loginwithamazon" -> pure LoginWithAmazon
        "saml" -> pure Saml
        e -> fromTextError $ "Failure parsing IdentityProviderTypeType from value: '" <> e
           <> "'. Accepted values: facebook, google, loginwithamazon, saml"

instance ToText IdentityProviderTypeType where
    toText = \case
        Facebook -> "Facebook"
        Google -> "Google"
        LoginWithAmazon -> "LoginWithAmazon"
        Saml -> "SAML"

instance Hashable     IdentityProviderTypeType
instance NFData       IdentityProviderTypeType
instance ToByteString IdentityProviderTypeType
instance ToQuery      IdentityProviderTypeType
instance ToHeader     IdentityProviderTypeType

instance ToJSON IdentityProviderTypeType where
    toJSON = toJSONText

instance FromJSON IdentityProviderTypeType where
    parseJSON = parseJSONText "IdentityProviderTypeType"

data MessageActionType
  = Resend
  | Suppress
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText MessageActionType where
    parser = takeLowerText >>= \case
        "resend" -> pure Resend
        "suppress" -> pure Suppress
        e -> fromTextError $ "Failure parsing MessageActionType from value: '" <> e
           <> "'. Accepted values: resend, suppress"

instance ToText MessageActionType where
    toText = \case
        Resend -> "RESEND"
        Suppress -> "SUPPRESS"

instance Hashable     MessageActionType
instance NFData       MessageActionType
instance ToByteString MessageActionType
instance ToQuery      MessageActionType
instance ToHeader     MessageActionType

instance ToJSON MessageActionType where
    toJSON = toJSONText

data OAuthFlowType
  = ClientCredentials
  | Code
  | Implicit
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText OAuthFlowType where
    parser = takeLowerText >>= \case
        "client_credentials" -> pure ClientCredentials
        "code" -> pure Code
        "implicit" -> pure Implicit
        e -> fromTextError $ "Failure parsing OAuthFlowType from value: '" <> e
           <> "'. Accepted values: client_credentials, code, implicit"

instance ToText OAuthFlowType where
    toText = \case
        ClientCredentials -> "client_credentials"
        Code -> "code"
        Implicit -> "implicit"

instance Hashable     OAuthFlowType
instance NFData       OAuthFlowType
instance ToByteString OAuthFlowType
instance ToQuery      OAuthFlowType
instance ToHeader     OAuthFlowType

instance ToJSON OAuthFlowType where
    toJSON = toJSONText

instance FromJSON OAuthFlowType where
    parseJSON = parseJSONText "OAuthFlowType"

data RiskDecisionType
  = AccountTakeover
  | Block
  | NoRisk
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText RiskDecisionType where
    parser = takeLowerText >>= \case
        "accounttakeover" -> pure AccountTakeover
        "block" -> pure Block
        "norisk" -> pure NoRisk
        e -> fromTextError $ "Failure parsing RiskDecisionType from value: '" <> e
           <> "'. Accepted values: accounttakeover, block, norisk"

instance ToText RiskDecisionType where
    toText = \case
        AccountTakeover -> "AccountTakeover"
        Block -> "Block"
        NoRisk -> "NoRisk"

instance Hashable     RiskDecisionType
instance NFData       RiskDecisionType
instance ToByteString RiskDecisionType
instance ToQuery      RiskDecisionType
instance ToHeader     RiskDecisionType

instance FromJSON RiskDecisionType where
    parseJSON = parseJSONText "RiskDecisionType"

data RiskLevelType
  = High
  | Low
  | Medium
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText RiskLevelType where
    parser = takeLowerText >>= \case
        "high" -> pure High
        "low" -> pure Low
        "medium" -> pure Medium
        e -> fromTextError $ "Failure parsing RiskLevelType from value: '" <> e
           <> "'. Accepted values: high, low, medium"

instance ToText RiskLevelType where
    toText = \case
        High -> "High"
        Low -> "Low"
        Medium -> "Medium"

instance Hashable     RiskLevelType
instance NFData       RiskLevelType
instance ToByteString RiskLevelType
instance ToQuery      RiskLevelType
instance ToHeader     RiskLevelType

instance FromJSON RiskLevelType where
    parseJSON = parseJSONText "RiskLevelType"

data StatusType
  = Disabled
  | Enabled
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


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
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


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
  = UPMTON
  | UPMTOff
  | UPMTOptional
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText UserPoolMFAType where
    parser = takeLowerText >>= \case
        "on" -> pure UPMTON
        "off" -> pure UPMTOff
        "optional" -> pure UPMTOptional
        e -> fromTextError $ "Failure parsing UserPoolMFAType from value: '" <> e
           <> "'. Accepted values: on, off, optional"

instance ToText UserPoolMFAType where
    toText = \case
        UPMTON -> "ON"
        UPMTOff -> "OFF"
        UPMTOptional -> "OPTIONAL"

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
  | ForceChangePassword
  | ResetRequired
  | Unconfirmed
  | Unknown
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText UserStatusType where
    parser = takeLowerText >>= \case
        "archived" -> pure Archived
        "compromised" -> pure Compromised
        "confirmed" -> pure Confirmed
        "force_change_password" -> pure ForceChangePassword
        "reset_required" -> pure ResetRequired
        "unconfirmed" -> pure Unconfirmed
        "unknown" -> pure Unknown
        e -> fromTextError $ "Failure parsing UserStatusType from value: '" <> e
           <> "'. Accepted values: archived, compromised, confirmed, force_change_password, reset_required, unconfirmed, unknown"

instance ToText UserStatusType where
    toText = \case
        Archived -> "ARCHIVED"
        Compromised -> "COMPROMISED"
        Confirmed -> "CONFIRMED"
        ForceChangePassword -> "FORCE_CHANGE_PASSWORD"
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

data UsernameAttributeType
  = UATEmail
  | UATPhoneNumber
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText UsernameAttributeType where
    parser = takeLowerText >>= \case
        "email" -> pure UATEmail
        "phone_number" -> pure UATPhoneNumber
        e -> fromTextError $ "Failure parsing UsernameAttributeType from value: '" <> e
           <> "'. Accepted values: email, phone_number"

instance ToText UsernameAttributeType where
    toText = \case
        UATEmail -> "email"
        UATPhoneNumber -> "phone_number"

instance Hashable     UsernameAttributeType
instance NFData       UsernameAttributeType
instance ToByteString UsernameAttributeType
instance ToQuery      UsernameAttributeType
instance ToHeader     UsernameAttributeType

instance ToJSON UsernameAttributeType where
    toJSON = toJSONText

instance FromJSON UsernameAttributeType where
    parseJSON = parseJSONText "UsernameAttributeType"

data VerifiedAttributeType
  = Email
  | PhoneNumber
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


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

data VerifySoftwareTokenResponseType
  = VSTRTError'
  | VSTRTSuccess
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText VerifySoftwareTokenResponseType where
    parser = takeLowerText >>= \case
        "error" -> pure VSTRTError'
        "success" -> pure VSTRTSuccess
        e -> fromTextError $ "Failure parsing VerifySoftwareTokenResponseType from value: '" <> e
           <> "'. Accepted values: error, success"

instance ToText VerifySoftwareTokenResponseType where
    toText = \case
        VSTRTError' -> "ERROR"
        VSTRTSuccess -> "SUCCESS"

instance Hashable     VerifySoftwareTokenResponseType
instance NFData       VerifySoftwareTokenResponseType
instance ToByteString VerifySoftwareTokenResponseType
instance ToQuery      VerifySoftwareTokenResponseType
instance ToHeader     VerifySoftwareTokenResponseType

instance FromJSON VerifySoftwareTokenResponseType where
    parseJSON = parseJSONText "VerifySoftwareTokenResponseType"
