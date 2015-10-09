{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.Sum
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IoT.Types.Sum where

import           Network.AWS.Prelude

data CertificateStatus
    = Active
    | Inactive
    | PendingTransfer
    | Revoked
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText CertificateStatus where
    parser = takeLowerText >>= \case
        "active" -> pure Active
        "inactive" -> pure Inactive
        "pending_transfer" -> pure PendingTransfer
        "revoked" -> pure Revoked
        e -> fromTextError $ "Failure parsing CertificateStatus from value: '" <> e
           <> "'. Accepted values: ACTIVE, INACTIVE, PENDING_TRANSFER, REVOKED"

instance ToText CertificateStatus where
    toText = \case
        Active -> "ACTIVE"
        Inactive -> "INACTIVE"
        PendingTransfer -> "PENDING_TRANSFER"
        Revoked -> "REVOKED"

instance Hashable     CertificateStatus
instance ToByteString CertificateStatus
instance ToQuery      CertificateStatus
instance ToHeader     CertificateStatus

instance ToJSON CertificateStatus where
    toJSON = toJSONText

instance FromJSON CertificateStatus where
    parseJSON = parseJSONText "CertificateStatus"

data LogLevel
    = Debug
    | Disabled
    | Error'
    | Info
    | Warn
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

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
instance ToByteString LogLevel
instance ToQuery      LogLevel
instance ToHeader     LogLevel

instance ToJSON LogLevel where
    toJSON = toJSONText

instance FromJSON LogLevel where
    parseJSON = parseJSONText "LogLevel"
