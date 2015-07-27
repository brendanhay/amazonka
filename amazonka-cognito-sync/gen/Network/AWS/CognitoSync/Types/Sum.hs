{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoSync.Types.Sum
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CognitoSync.Types.Sum where

import           Network.AWS.Prelude

data BulkPublishStatus
    = NotStarted
    | InProgress
    | Succeeded
    | Failed
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText BulkPublishStatus where
    parser = takeLowerText >>= \case
        "failed" -> pure Failed
        "in_progress" -> pure InProgress
        "not_started" -> pure NotStarted
        "succeeded" -> pure Succeeded
        e -> fromTextError $ "Failure parsing BulkPublishStatus from value: '" <> e
           <> "'. Accepted values: failed, in_progress, not_started, succeeded"

instance ToText BulkPublishStatus where
    toText = \case
        Failed -> "failed"
        InProgress -> "in_progress"
        NotStarted -> "not_started"
        Succeeded -> "succeeded"

instance Hashable     BulkPublishStatus
instance ToByteString BulkPublishStatus
instance ToPath       BulkPublishStatus
instance ToQuery      BulkPublishStatus
instance ToHeader     BulkPublishStatus

instance FromJSON BulkPublishStatus where
    parseJSON = parseJSONText "BulkPublishStatus"

data Operation
    = Replace
    | Remove
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText Operation where
    parser = takeLowerText >>= \case
        "remove" -> pure Remove
        "replace" -> pure Replace
        e -> fromTextError $ "Failure parsing Operation from value: '" <> e
           <> "'. Accepted values: remove, replace"

instance ToText Operation where
    toText = \case
        Remove -> "remove"
        Replace -> "replace"

instance Hashable     Operation
instance ToByteString Operation
instance ToPath       Operation
instance ToQuery      Operation
instance ToHeader     Operation

instance ToJSON Operation where
    toJSON = toJSONText

data Platform
    = GCM
    | APNS
    | ADM
    | APNSSandbox
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText Platform where
    parser = takeLowerText >>= \case
        "adm" -> pure ADM
        "apns" -> pure APNS
        "apns_sandbox" -> pure APNSSandbox
        "gcm" -> pure GCM
        e -> fromTextError $ "Failure parsing Platform from value: '" <> e
           <> "'. Accepted values: adm, apns, apns_sandbox, gcm"

instance ToText Platform where
    toText = \case
        ADM -> "adm"
        APNS -> "apns"
        APNSSandbox -> "apns_sandbox"
        GCM -> "gcm"

instance Hashable     Platform
instance ToByteString Platform
instance ToPath       Platform
instance ToQuery      Platform
instance ToHeader     Platform

instance ToJSON Platform where
    toJSON = toJSONText

data StreamingStatus
    = Enabled
    | Disabled
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText StreamingStatus where
    parser = takeLowerText >>= \case
        "disabled" -> pure Disabled
        "enabled" -> pure Enabled
        e -> fromTextError $ "Failure parsing StreamingStatus from value: '" <> e
           <> "'. Accepted values: disabled, enabled"

instance ToText StreamingStatus where
    toText = \case
        Disabled -> "disabled"
        Enabled -> "enabled"

instance Hashable     StreamingStatus
instance ToByteString StreamingStatus
instance ToPath       StreamingStatus
instance ToQuery      StreamingStatus
instance ToHeader     StreamingStatus

instance ToJSON StreamingStatus where
    toJSON = toJSONText

instance FromJSON StreamingStatus where
    parseJSON = parseJSONText "StreamingStatus"
