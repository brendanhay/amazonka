{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoSync.Types.Sum
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CognitoSync.Types.Sum where

import Network.AWS.Prelude

data BulkPublishStatus
  = Failed
  | InProgress
  | NotStarted
  | Succeeded
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


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
        Failed -> "FAILED"
        InProgress -> "IN_PROGRESS"
        NotStarted -> "NOT_STARTED"
        Succeeded -> "SUCCEEDED"

instance Hashable     BulkPublishStatus
instance NFData       BulkPublishStatus
instance ToByteString BulkPublishStatus
instance ToQuery      BulkPublishStatus
instance ToHeader     BulkPublishStatus

instance FromJSON BulkPublishStatus where
    parseJSON = parseJSONText "BulkPublishStatus"

data Operation
  = Remove
  | Replace
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


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
instance NFData       Operation
instance ToByteString Operation
instance ToQuery      Operation
instance ToHeader     Operation

instance ToJSON Operation where
    toJSON = toJSONText

data Platform
  = ADM
  | APNS
  | APNSSandbox
  | GCM
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


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
        ADM -> "ADM"
        APNS -> "APNS"
        APNSSandbox -> "APNS_SANDBOX"
        GCM -> "GCM"

instance Hashable     Platform
instance NFData       Platform
instance ToByteString Platform
instance ToQuery      Platform
instance ToHeader     Platform

instance ToJSON Platform where
    toJSON = toJSONText

data StreamingStatus
  = Disabled
  | Enabled
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText StreamingStatus where
    parser = takeLowerText >>= \case
        "disabled" -> pure Disabled
        "enabled" -> pure Enabled
        e -> fromTextError $ "Failure parsing StreamingStatus from value: '" <> e
           <> "'. Accepted values: disabled, enabled"

instance ToText StreamingStatus where
    toText = \case
        Disabled -> "DISABLED"
        Enabled -> "ENABLED"

instance Hashable     StreamingStatus
instance NFData       StreamingStatus
instance ToByteString StreamingStatus
instance ToQuery      StreamingStatus
instance ToHeader     StreamingStatus

instance ToJSON StreamingStatus where
    toJSON = toJSONText

instance FromJSON StreamingStatus where
    parseJSON = parseJSONText "StreamingStatus"
