{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.Types.Sum
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SES.Types.Sum where

import           Network.AWS.Prelude

data BehaviorOnMXFailure
    = RejectMessage
    | UseDefaultValue
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText BehaviorOnMXFailure where
    parser = takeLowerText >>= \case
        "rejectmessage" -> pure RejectMessage
        "usedefaultvalue" -> pure UseDefaultValue
        e -> fromTextError $ "Failure parsing BehaviorOnMXFailure from value: '" <> e
           <> "'. Accepted values: RejectMessage, UseDefaultValue"

instance ToText BehaviorOnMXFailure where
    toText = \case
        RejectMessage -> "RejectMessage"
        UseDefaultValue -> "UseDefaultValue"

instance Hashable     BehaviorOnMXFailure
instance ToByteString BehaviorOnMXFailure
instance ToQuery      BehaviorOnMXFailure
instance ToHeader     BehaviorOnMXFailure

instance FromXML BehaviorOnMXFailure where
    parseXML = parseXMLText "BehaviorOnMXFailure"

data BounceType
    = BTContentRejected
    | BTDoesNotExist
    | BTExceededQuota
    | BTMessageTooLarge
    | BTTemporaryFailure
    | BTUndefined
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText BounceType where
    parser = takeLowerText >>= \case
        "contentrejected" -> pure BTContentRejected
        "doesnotexist" -> pure BTDoesNotExist
        "exceededquota" -> pure BTExceededQuota
        "messagetoolarge" -> pure BTMessageTooLarge
        "temporaryfailure" -> pure BTTemporaryFailure
        "undefined" -> pure BTUndefined
        e -> fromTextError $ "Failure parsing BounceType from value: '" <> e
           <> "'. Accepted values: ContentRejected, DoesNotExist, ExceededQuota, MessageTooLarge, TemporaryFailure, Undefined"

instance ToText BounceType where
    toText = \case
        BTContentRejected -> "ContentRejected"
        BTDoesNotExist -> "DoesNotExist"
        BTExceededQuota -> "ExceededQuota"
        BTMessageTooLarge -> "MessageTooLarge"
        BTTemporaryFailure -> "TemporaryFailure"
        BTUndefined -> "Undefined"

instance Hashable     BounceType
instance ToByteString BounceType
instance ToQuery      BounceType
instance ToHeader     BounceType

data CustomMailFromStatus
    = CMFSFailed
    | CMFSPending
    | CMFSSuccess
    | CMFSTemporaryFailure
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText CustomMailFromStatus where
    parser = takeLowerText >>= \case
        "failed" -> pure CMFSFailed
        "pending" -> pure CMFSPending
        "success" -> pure CMFSSuccess
        "temporaryfailure" -> pure CMFSTemporaryFailure
        e -> fromTextError $ "Failure parsing CustomMailFromStatus from value: '" <> e
           <> "'. Accepted values: Failed, Pending, Success, TemporaryFailure"

instance ToText CustomMailFromStatus where
    toText = \case
        CMFSFailed -> "Failed"
        CMFSPending -> "Pending"
        CMFSSuccess -> "Success"
        CMFSTemporaryFailure -> "TemporaryFailure"

instance Hashable     CustomMailFromStatus
instance ToByteString CustomMailFromStatus
instance ToQuery      CustomMailFromStatus
instance ToHeader     CustomMailFromStatus

instance FromXML CustomMailFromStatus where
    parseXML = parseXMLText "CustomMailFromStatus"

data DsnAction
    = DADelayed
    | DADelivered
    | DAExpanded
    | DAFailed
    | DARelayed
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

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
instance ToByteString DsnAction
instance ToQuery      DsnAction
instance ToHeader     DsnAction

data IdentityType
    = Domain
    | EmailAddress
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText IdentityType where
    parser = takeLowerText >>= \case
        "domain" -> pure Domain
        "emailaddress" -> pure EmailAddress
        e -> fromTextError $ "Failure parsing IdentityType from value: '" <> e
           <> "'. Accepted values: Domain, EmailAddress"

instance ToText IdentityType where
    toText = \case
        Domain -> "Domain"
        EmailAddress -> "EmailAddress"

instance Hashable     IdentityType
instance ToByteString IdentityType
instance ToQuery      IdentityType
instance ToHeader     IdentityType

data InvocationType
    = Event
    | RequestResponse
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText InvocationType where
    parser = takeLowerText >>= \case
        "event" -> pure Event
        "requestresponse" -> pure RequestResponse
        e -> fromTextError $ "Failure parsing InvocationType from value: '" <> e
           <> "'. Accepted values: Event, RequestResponse"

instance ToText InvocationType where
    toText = \case
        Event -> "Event"
        RequestResponse -> "RequestResponse"

instance Hashable     InvocationType
instance ToByteString InvocationType
instance ToQuery      InvocationType
instance ToHeader     InvocationType

instance FromXML InvocationType where
    parseXML = parseXMLText "InvocationType"

data NotificationType
    = Bounce
    | Complaint
    | Delivery
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText NotificationType where
    parser = takeLowerText >>= \case
        "bounce" -> pure Bounce
        "complaint" -> pure Complaint
        "delivery" -> pure Delivery
        e -> fromTextError $ "Failure parsing NotificationType from value: '" <> e
           <> "'. Accepted values: Bounce, Complaint, Delivery"

instance ToText NotificationType where
    toText = \case
        Bounce -> "Bounce"
        Complaint -> "Complaint"
        Delivery -> "Delivery"

instance Hashable     NotificationType
instance ToByteString NotificationType
instance ToQuery      NotificationType
instance ToHeader     NotificationType

data ReceiptFilterPolicy
    = Allow
    | Block
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText ReceiptFilterPolicy where
    parser = takeLowerText >>= \case
        "allow" -> pure Allow
        "block" -> pure Block
        e -> fromTextError $ "Failure parsing ReceiptFilterPolicy from value: '" <> e
           <> "'. Accepted values: Allow, Block"

instance ToText ReceiptFilterPolicy where
    toText = \case
        Allow -> "Allow"
        Block -> "Block"

instance Hashable     ReceiptFilterPolicy
instance ToByteString ReceiptFilterPolicy
instance ToQuery      ReceiptFilterPolicy
instance ToHeader     ReceiptFilterPolicy

instance FromXML ReceiptFilterPolicy where
    parseXML = parseXMLText "ReceiptFilterPolicy"

data SNSActionEncoding
    = BASE64
    | Utf8
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText SNSActionEncoding where
    parser = takeLowerText >>= \case
        "base64" -> pure BASE64
        "utf-8" -> pure Utf8
        e -> fromTextError $ "Failure parsing SNSActionEncoding from value: '" <> e
           <> "'. Accepted values: Base64, UTF-8"

instance ToText SNSActionEncoding where
    toText = \case
        BASE64 -> "Base64"
        Utf8 -> "UTF-8"

instance Hashable     SNSActionEncoding
instance ToByteString SNSActionEncoding
instance ToQuery      SNSActionEncoding
instance ToHeader     SNSActionEncoding

instance FromXML SNSActionEncoding where
    parseXML = parseXMLText "SNSActionEncoding"

data StopScope =
    RuleSet
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText StopScope where
    parser = takeLowerText >>= \case
        "ruleset" -> pure RuleSet
        e -> fromTextError $ "Failure parsing StopScope from value: '" <> e
           <> "'. Accepted values: RuleSet"

instance ToText StopScope where
    toText = \case
        RuleSet -> "RuleSet"

instance Hashable     StopScope
instance ToByteString StopScope
instance ToQuery      StopScope
instance ToHeader     StopScope

instance FromXML StopScope where
    parseXML = parseXMLText "StopScope"

data TLSPolicy
    = Optional
    | Require
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText TLSPolicy where
    parser = takeLowerText >>= \case
        "optional" -> pure Optional
        "require" -> pure Require
        e -> fromTextError $ "Failure parsing TLSPolicy from value: '" <> e
           <> "'. Accepted values: Optional, Require"

instance ToText TLSPolicy where
    toText = \case
        Optional -> "Optional"
        Require -> "Require"

instance Hashable     TLSPolicy
instance ToByteString TLSPolicy
instance ToQuery      TLSPolicy
instance ToHeader     TLSPolicy

instance FromXML TLSPolicy where
    parseXML = parseXMLText "TLSPolicy"

data VerificationStatus
    = Failed
    | NotStarted
    | Pending
    | Success
    | TemporaryFailure
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText VerificationStatus where
    parser = takeLowerText >>= \case
        "failed" -> pure Failed
        "notstarted" -> pure NotStarted
        "pending" -> pure Pending
        "success" -> pure Success
        "temporaryfailure" -> pure TemporaryFailure
        e -> fromTextError $ "Failure parsing VerificationStatus from value: '" <> e
           <> "'. Accepted values: Failed, NotStarted, Pending, Success, TemporaryFailure"

instance ToText VerificationStatus where
    toText = \case
        Failed -> "Failed"
        NotStarted -> "NotStarted"
        Pending -> "Pending"
        Success -> "Success"
        TemporaryFailure -> "TemporaryFailure"

instance Hashable     VerificationStatus
instance ToByteString VerificationStatus
instance ToQuery      VerificationStatus
instance ToHeader     VerificationStatus

instance FromXML VerificationStatus where
    parseXML = parseXMLText "VerificationStatus"
