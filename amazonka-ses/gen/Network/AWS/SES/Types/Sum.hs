{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.Types.Sum
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SES.Types.Sum where

import Network.AWS.Prelude

data BehaviorOnMXFailure
  = RejectMessage
  | UseDefaultValue
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText BehaviorOnMXFailure where
    parser = takeLowerText >>= \case
        "rejectmessage" -> pure RejectMessage
        "usedefaultvalue" -> pure UseDefaultValue
        e -> fromTextError $ "Failure parsing BehaviorOnMXFailure from value: '" <> e
           <> "'. Accepted values: rejectmessage, usedefaultvalue"

instance ToText BehaviorOnMXFailure where
    toText = \case
        RejectMessage -> "RejectMessage"
        UseDefaultValue -> "UseDefaultValue"

instance Hashable     BehaviorOnMXFailure
instance NFData       BehaviorOnMXFailure
instance ToByteString BehaviorOnMXFailure
instance ToQuery      BehaviorOnMXFailure
instance ToHeader     BehaviorOnMXFailure

instance FromXML BehaviorOnMXFailure where
    parseXML = parseXMLText "BehaviorOnMXFailure"

data BounceType
  = ContentRejected
  | DoesNotExist
  | ExceededQuota
  | MessageTooLarge
  | TemporaryFailure
  | Undefined
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText BounceType where
    parser = takeLowerText >>= \case
        "contentrejected" -> pure ContentRejected
        "doesnotexist" -> pure DoesNotExist
        "exceededquota" -> pure ExceededQuota
        "messagetoolarge" -> pure MessageTooLarge
        "temporaryfailure" -> pure TemporaryFailure
        "undefined" -> pure Undefined
        e -> fromTextError $ "Failure parsing BounceType from value: '" <> e
           <> "'. Accepted values: contentrejected, doesnotexist, exceededquota, messagetoolarge, temporaryfailure, undefined"

instance ToText BounceType where
    toText = \case
        ContentRejected -> "ContentRejected"
        DoesNotExist -> "DoesNotExist"
        ExceededQuota -> "ExceededQuota"
        MessageTooLarge -> "MessageTooLarge"
        TemporaryFailure -> "TemporaryFailure"
        Undefined -> "Undefined"

instance Hashable     BounceType
instance NFData       BounceType
instance ToByteString BounceType
instance ToQuery      BounceType
instance ToHeader     BounceType

data BulkEmailStatus
  = AccountDailyQuotaExceeded
  | AccountSendingPaused
  | AccountSuspended
  | AccountThrottled
  | ConfigurationSetDoesNotExist
  | ConfigurationSetSendingPaused
  | Failed
  | InvalidParameterValue
  | InvalidSendingPoolName
  | MailFromDomainNotVerified
  | MessageRejected
  | Success
  | TemplateDoesNotExist
  | TransientFailure
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText BulkEmailStatus where
    parser = takeLowerText >>= \case
        "accountdailyquotaexceeded" -> pure AccountDailyQuotaExceeded
        "accountsendingpaused" -> pure AccountSendingPaused
        "accountsuspended" -> pure AccountSuspended
        "accountthrottled" -> pure AccountThrottled
        "configurationsetdoesnotexist" -> pure ConfigurationSetDoesNotExist
        "configurationsetsendingpaused" -> pure ConfigurationSetSendingPaused
        "failed" -> pure Failed
        "invalidparametervalue" -> pure InvalidParameterValue
        "invalidsendingpoolname" -> pure InvalidSendingPoolName
        "mailfromdomainnotverified" -> pure MailFromDomainNotVerified
        "messagerejected" -> pure MessageRejected
        "success" -> pure Success
        "templatedoesnotexist" -> pure TemplateDoesNotExist
        "transientfailure" -> pure TransientFailure
        e -> fromTextError $ "Failure parsing BulkEmailStatus from value: '" <> e
           <> "'. Accepted values: accountdailyquotaexceeded, accountsendingpaused, accountsuspended, accountthrottled, configurationsetdoesnotexist, configurationsetsendingpaused, failed, invalidparametervalue, invalidsendingpoolname, mailfromdomainnotverified, messagerejected, success, templatedoesnotexist, transientfailure"

instance ToText BulkEmailStatus where
    toText = \case
        AccountDailyQuotaExceeded -> "AccountDailyQuotaExceeded"
        AccountSendingPaused -> "AccountSendingPaused"
        AccountSuspended -> "AccountSuspended"
        AccountThrottled -> "AccountThrottled"
        ConfigurationSetDoesNotExist -> "ConfigurationSetDoesNotExist"
        ConfigurationSetSendingPaused -> "ConfigurationSetSendingPaused"
        Failed -> "Failed"
        InvalidParameterValue -> "InvalidParameterValue"
        InvalidSendingPoolName -> "InvalidSendingPoolName"
        MailFromDomainNotVerified -> "MailFromDomainNotVerified"
        MessageRejected -> "MessageRejected"
        Success -> "Success"
        TemplateDoesNotExist -> "TemplateDoesNotExist"
        TransientFailure -> "TransientFailure"

instance Hashable     BulkEmailStatus
instance NFData       BulkEmailStatus
instance ToByteString BulkEmailStatus
instance ToQuery      BulkEmailStatus
instance ToHeader     BulkEmailStatus

instance FromXML BulkEmailStatus where
    parseXML = parseXMLText "BulkEmailStatus"

data ConfigurationSetAttribute
  = EventDestinations
  | ReputationOptions
  | TrackingOptions
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ConfigurationSetAttribute where
    parser = takeLowerText >>= \case
        "eventdestinations" -> pure EventDestinations
        "reputationoptions" -> pure ReputationOptions
        "trackingoptions" -> pure TrackingOptions
        e -> fromTextError $ "Failure parsing ConfigurationSetAttribute from value: '" <> e
           <> "'. Accepted values: eventdestinations, reputationoptions, trackingoptions"

instance ToText ConfigurationSetAttribute where
    toText = \case
        EventDestinations -> "eventDestinations"
        ReputationOptions -> "reputationOptions"
        TrackingOptions -> "trackingOptions"

instance Hashable     ConfigurationSetAttribute
instance NFData       ConfigurationSetAttribute
instance ToByteString ConfigurationSetAttribute
instance ToQuery      ConfigurationSetAttribute
instance ToHeader     ConfigurationSetAttribute

data CustomMailFromStatus
  = CMFSFailed
  | CMFSPending
  | CMFSSuccess
  | CMFSTemporaryFailure
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText CustomMailFromStatus where
    parser = takeLowerText >>= \case
        "failed" -> pure CMFSFailed
        "pending" -> pure CMFSPending
        "success" -> pure CMFSSuccess
        "temporaryfailure" -> pure CMFSTemporaryFailure
        e -> fromTextError $ "Failure parsing CustomMailFromStatus from value: '" <> e
           <> "'. Accepted values: failed, pending, success, temporaryfailure"

instance ToText CustomMailFromStatus where
    toText = \case
        CMFSFailed -> "Failed"
        CMFSPending -> "Pending"
        CMFSSuccess -> "Success"
        CMFSTemporaryFailure -> "TemporaryFailure"

instance Hashable     CustomMailFromStatus
instance NFData       CustomMailFromStatus
instance ToByteString CustomMailFromStatus
instance ToQuery      CustomMailFromStatus
instance ToHeader     CustomMailFromStatus

instance FromXML CustomMailFromStatus where
    parseXML = parseXMLText "CustomMailFromStatus"

data DimensionValueSource
  = EmailHeader
  | LinkTag
  | MessageTag
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText DimensionValueSource where
    parser = takeLowerText >>= \case
        "emailheader" -> pure EmailHeader
        "linktag" -> pure LinkTag
        "messagetag" -> pure MessageTag
        e -> fromTextError $ "Failure parsing DimensionValueSource from value: '" <> e
           <> "'. Accepted values: emailheader, linktag, messagetag"

instance ToText DimensionValueSource where
    toText = \case
        EmailHeader -> "emailHeader"
        LinkTag -> "linkTag"
        MessageTag -> "messageTag"

instance Hashable     DimensionValueSource
instance NFData       DimensionValueSource
instance ToByteString DimensionValueSource
instance ToQuery      DimensionValueSource
instance ToHeader     DimensionValueSource

instance FromXML DimensionValueSource where
    parseXML = parseXMLText "DimensionValueSource"

data DsnAction
  = DADelayed
  | DADelivered
  | DAExpanded
  | DAFailed
  | DARelayed
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText DsnAction where
    parser = takeLowerText >>= \case
        "delayed" -> pure DADelayed
        "delivered" -> pure DADelivered
        "expanded" -> pure DAExpanded
        "failed" -> pure DAFailed
        "relayed" -> pure DARelayed
        e -> fromTextError $ "Failure parsing DsnAction from value: '" <> e
           <> "'. Accepted values: delayed, delivered, expanded, failed, relayed"

instance ToText DsnAction where
    toText = \case
        DADelayed -> "delayed"
        DADelivered -> "delivered"
        DAExpanded -> "expanded"
        DAFailed -> "failed"
        DARelayed -> "relayed"

instance Hashable     DsnAction
instance NFData       DsnAction
instance ToByteString DsnAction
instance ToQuery      DsnAction
instance ToHeader     DsnAction

data EventType
  = ETBounce
  | ETClick
  | ETComplaint
  | ETDelivery
  | ETOpen
  | ETReject
  | ETRenderingFailure
  | ETSend
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText EventType where
    parser = takeLowerText >>= \case
        "bounce" -> pure ETBounce
        "click" -> pure ETClick
        "complaint" -> pure ETComplaint
        "delivery" -> pure ETDelivery
        "open" -> pure ETOpen
        "reject" -> pure ETReject
        "renderingfailure" -> pure ETRenderingFailure
        "send" -> pure ETSend
        e -> fromTextError $ "Failure parsing EventType from value: '" <> e
           <> "'. Accepted values: bounce, click, complaint, delivery, open, reject, renderingfailure, send"

instance ToText EventType where
    toText = \case
        ETBounce -> "bounce"
        ETClick -> "click"
        ETComplaint -> "complaint"
        ETDelivery -> "delivery"
        ETOpen -> "open"
        ETReject -> "reject"
        ETRenderingFailure -> "renderingFailure"
        ETSend -> "send"

instance Hashable     EventType
instance NFData       EventType
instance ToByteString EventType
instance ToQuery      EventType
instance ToHeader     EventType

instance FromXML EventType where
    parseXML = parseXMLText "EventType"

data IdentityType
  = Domain
  | EmailAddress
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText IdentityType where
    parser = takeLowerText >>= \case
        "domain" -> pure Domain
        "emailaddress" -> pure EmailAddress
        e -> fromTextError $ "Failure parsing IdentityType from value: '" <> e
           <> "'. Accepted values: domain, emailaddress"

instance ToText IdentityType where
    toText = \case
        Domain -> "Domain"
        EmailAddress -> "EmailAddress"

instance Hashable     IdentityType
instance NFData       IdentityType
instance ToByteString IdentityType
instance ToQuery      IdentityType
instance ToHeader     IdentityType

data InvocationType
  = Event
  | RequestResponse
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText InvocationType where
    parser = takeLowerText >>= \case
        "event" -> pure Event
        "requestresponse" -> pure RequestResponse
        e -> fromTextError $ "Failure parsing InvocationType from value: '" <> e
           <> "'. Accepted values: event, requestresponse"

instance ToText InvocationType where
    toText = \case
        Event -> "Event"
        RequestResponse -> "RequestResponse"

instance Hashable     InvocationType
instance NFData       InvocationType
instance ToByteString InvocationType
instance ToQuery      InvocationType
instance ToHeader     InvocationType

instance FromXML InvocationType where
    parseXML = parseXMLText "InvocationType"

data NotificationType
  = Bounce
  | Complaint
  | Delivery
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText NotificationType where
    parser = takeLowerText >>= \case
        "bounce" -> pure Bounce
        "complaint" -> pure Complaint
        "delivery" -> pure Delivery
        e -> fromTextError $ "Failure parsing NotificationType from value: '" <> e
           <> "'. Accepted values: bounce, complaint, delivery"

instance ToText NotificationType where
    toText = \case
        Bounce -> "Bounce"
        Complaint -> "Complaint"
        Delivery -> "Delivery"

instance Hashable     NotificationType
instance NFData       NotificationType
instance ToByteString NotificationType
instance ToQuery      NotificationType
instance ToHeader     NotificationType

data ReceiptFilterPolicy
  = Allow
  | Block
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ReceiptFilterPolicy where
    parser = takeLowerText >>= \case
        "allow" -> pure Allow
        "block" -> pure Block
        e -> fromTextError $ "Failure parsing ReceiptFilterPolicy from value: '" <> e
           <> "'. Accepted values: allow, block"

instance ToText ReceiptFilterPolicy where
    toText = \case
        Allow -> "Allow"
        Block -> "Block"

instance Hashable     ReceiptFilterPolicy
instance NFData       ReceiptFilterPolicy
instance ToByteString ReceiptFilterPolicy
instance ToQuery      ReceiptFilterPolicy
instance ToHeader     ReceiptFilterPolicy

instance FromXML ReceiptFilterPolicy where
    parseXML = parseXMLText "ReceiptFilterPolicy"

data SNSActionEncoding
  = BASE64
  | Utf8
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText SNSActionEncoding where
    parser = takeLowerText >>= \case
        "base64" -> pure BASE64
        "utf-8" -> pure Utf8
        e -> fromTextError $ "Failure parsing SNSActionEncoding from value: '" <> e
           <> "'. Accepted values: base64, utf-8"

instance ToText SNSActionEncoding where
    toText = \case
        BASE64 -> "Base64"
        Utf8 -> "UTF-8"

instance Hashable     SNSActionEncoding
instance NFData       SNSActionEncoding
instance ToByteString SNSActionEncoding
instance ToQuery      SNSActionEncoding
instance ToHeader     SNSActionEncoding

instance FromXML SNSActionEncoding where
    parseXML = parseXMLText "SNSActionEncoding"

data StopScope =
  RuleSet
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText StopScope where
    parser = takeLowerText >>= \case
        "ruleset" -> pure RuleSet
        e -> fromTextError $ "Failure parsing StopScope from value: '" <> e
           <> "'. Accepted values: ruleset"

instance ToText StopScope where
    toText = \case
        RuleSet -> "RuleSet"

instance Hashable     StopScope
instance NFData       StopScope
instance ToByteString StopScope
instance ToQuery      StopScope
instance ToHeader     StopScope

instance FromXML StopScope where
    parseXML = parseXMLText "StopScope"

data TLSPolicy
  = Optional
  | Require
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText TLSPolicy where
    parser = takeLowerText >>= \case
        "optional" -> pure Optional
        "require" -> pure Require
        e -> fromTextError $ "Failure parsing TLSPolicy from value: '" <> e
           <> "'. Accepted values: optional, require"

instance ToText TLSPolicy where
    toText = \case
        Optional -> "Optional"
        Require -> "Require"

instance Hashable     TLSPolicy
instance NFData       TLSPolicy
instance ToByteString TLSPolicy
instance ToQuery      TLSPolicy
instance ToHeader     TLSPolicy

instance FromXML TLSPolicy where
    parseXML = parseXMLText "TLSPolicy"

data VerificationStatus
  = VSFailed
  | VSNotStarted
  | VSPending
  | VSSuccess
  | VSTemporaryFailure
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText VerificationStatus where
    parser = takeLowerText >>= \case
        "failed" -> pure VSFailed
        "notstarted" -> pure VSNotStarted
        "pending" -> pure VSPending
        "success" -> pure VSSuccess
        "temporaryfailure" -> pure VSTemporaryFailure
        e -> fromTextError $ "Failure parsing VerificationStatus from value: '" <> e
           <> "'. Accepted values: failed, notstarted, pending, success, temporaryfailure"

instance ToText VerificationStatus where
    toText = \case
        VSFailed -> "Failed"
        VSNotStarted -> "NotStarted"
        VSPending -> "Pending"
        VSSuccess -> "Success"
        VSTemporaryFailure -> "TemporaryFailure"

instance Hashable     VerificationStatus
instance NFData       VerificationStatus
instance ToByteString VerificationStatus
instance ToQuery      VerificationStatus
instance ToHeader     VerificationStatus

instance FromXML VerificationStatus where
    parseXML = parseXMLText "VerificationStatus"
