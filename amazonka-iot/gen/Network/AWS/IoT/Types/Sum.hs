{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.Sum
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IoT.Types.Sum where

import           Network.AWS.Prelude

data AutoRegistrationStatus
    = Disable
    | Enable
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText AutoRegistrationStatus where
    parser = takeLowerText >>= \case
        "disable" -> pure Disable
        "enable" -> pure Enable
        e -> fromTextError $ "Failure parsing AutoRegistrationStatus from value: '" <> e
           <> "'. Accepted values: DISABLE, ENABLE"

instance ToText AutoRegistrationStatus where
    toText = \case
        Disable -> "DISABLE"
        Enable -> "ENABLE"

instance Hashable     AutoRegistrationStatus
instance NFData       AutoRegistrationStatus
instance ToByteString AutoRegistrationStatus
instance ToQuery      AutoRegistrationStatus
instance ToHeader     AutoRegistrationStatus

instance ToJSON AutoRegistrationStatus where
    toJSON = toJSONText

instance FromJSON AutoRegistrationStatus where
    parseJSON = parseJSONText "AutoRegistrationStatus"

data CACertificateStatus
    = CACSActive
    | CACSInactive
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText CACertificateStatus where
    parser = takeLowerText >>= \case
        "active" -> pure CACSActive
        "inactive" -> pure CACSInactive
        e -> fromTextError $ "Failure parsing CACertificateStatus from value: '" <> e
           <> "'. Accepted values: ACTIVE, INACTIVE"

instance ToText CACertificateStatus where
    toText = \case
        CACSActive -> "ACTIVE"
        CACSInactive -> "INACTIVE"

instance Hashable     CACertificateStatus
instance NFData       CACertificateStatus
instance ToByteString CACertificateStatus
instance ToQuery      CACertificateStatus
instance ToHeader     CACertificateStatus

instance ToJSON CACertificateStatus where
    toJSON = toJSONText

instance FromJSON CACertificateStatus where
    parseJSON = parseJSONText "CACertificateStatus"

data CertificateStatus
    = Active
    | Inactive
    | PendingActivation
    | PendingTransfer
    | RegisterInactive
    | Revoked
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText CertificateStatus where
    parser = takeLowerText >>= \case
        "active" -> pure Active
        "inactive" -> pure Inactive
        "pending_activation" -> pure PendingActivation
        "pending_transfer" -> pure PendingTransfer
        "register_inactive" -> pure RegisterInactive
        "revoked" -> pure Revoked
        e -> fromTextError $ "Failure parsing CertificateStatus from value: '" <> e
           <> "'. Accepted values: ACTIVE, INACTIVE, PENDING_ACTIVATION, PENDING_TRANSFER, REGISTER_INACTIVE, REVOKED"

instance ToText CertificateStatus where
    toText = \case
        Active -> "ACTIVE"
        Inactive -> "INACTIVE"
        PendingActivation -> "PENDING_ACTIVATION"
        PendingTransfer -> "PENDING_TRANSFER"
        RegisterInactive -> "REGISTER_INACTIVE"
        Revoked -> "REVOKED"

instance Hashable     CertificateStatus
instance NFData       CertificateStatus
instance ToByteString CertificateStatus
instance ToQuery      CertificateStatus
instance ToHeader     CertificateStatus

instance ToJSON CertificateStatus where
    toJSON = toJSONText

instance FromJSON CertificateStatus where
    parseJSON = parseJSONText "CertificateStatus"

data DynamoKeyType
    = Number
    | String
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText DynamoKeyType where
    parser = takeLowerText >>= \case
        "number" -> pure Number
        "string" -> pure String
        e -> fromTextError $ "Failure parsing DynamoKeyType from value: '" <> e
           <> "'. Accepted values: NUMBER, STRING"

instance ToText DynamoKeyType where
    toText = \case
        Number -> "NUMBER"
        String -> "STRING"

instance Hashable     DynamoKeyType
instance NFData       DynamoKeyType
instance ToByteString DynamoKeyType
instance ToQuery      DynamoKeyType
instance ToHeader     DynamoKeyType

instance ToJSON DynamoKeyType where
    toJSON = toJSONText

instance FromJSON DynamoKeyType where
    parseJSON = parseJSONText "DynamoKeyType"

data LogLevel
    = Debug
    | Disabled
    | Error'
    | Info
    | Warn
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText LogLevel where
    parser = takeLowerText >>= \case
        "debug" -> pure Debug
        "disabled" -> pure Disabled
        "error" -> pure Error'
        "info" -> pure Info
        "warn" -> pure Warn
        e -> fromTextError $ "Failure parsing LogLevel from value: '" <> e
           <> "'. Accepted values: DEBUG, DISABLED, ERROR, INFO, WARN"

instance ToText LogLevel where
    toText = \case
        Debug -> "DEBUG"
        Disabled -> "DISABLED"
        Error' -> "ERROR"
        Info -> "INFO"
        Warn -> "WARN"

instance Hashable     LogLevel
instance NFData       LogLevel
instance ToByteString LogLevel
instance ToQuery      LogLevel
instance ToHeader     LogLevel

instance ToJSON LogLevel where
    toJSON = toJSONText

instance FromJSON LogLevel where
    parseJSON = parseJSONText "LogLevel"

data MessageFormat
    = JSON
    | Raw
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText MessageFormat where
    parser = takeLowerText >>= \case
        "json" -> pure JSON
        "raw" -> pure Raw
        e -> fromTextError $ "Failure parsing MessageFormat from value: '" <> e
           <> "'. Accepted values: JSON, RAW"

instance ToText MessageFormat where
    toText = \case
        JSON -> "JSON"
        Raw -> "RAW"

instance Hashable     MessageFormat
instance NFData       MessageFormat
instance ToByteString MessageFormat
instance ToQuery      MessageFormat
instance ToHeader     MessageFormat

instance ToJSON MessageFormat where
    toJSON = toJSONText

instance FromJSON MessageFormat where
    parseJSON = parseJSONText "MessageFormat"
