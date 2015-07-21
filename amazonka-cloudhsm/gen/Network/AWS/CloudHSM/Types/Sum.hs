{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudHSM.Types.Sum
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudHSM.Types.Sum where

import           Network.AWS.Prelude

data ClientVersion
    = V5_3
    | V5_1
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText ClientVersion where
    parser = takeLowerText >>= \case
        "5.1" -> pure V5_1
        "5.3" -> pure V5_3
        e -> fromTextError $ "Failure parsing ClientVersion from value: '" <> e
           <> "'. Accepted values: 5.1, 5.3"

instance ToText ClientVersion where
    toText = \case
        V5_1 -> "5.1"
        V5_3 -> "5.3"

instance Hashable ClientVersion
instance ToQuery ClientVersion
instance ToHeader ClientVersion

instance ToJSON ClientVersion where
    toJSON = toJSONText

data CloudHSMObjectState
    = Updating
    | Degraded
    | Ready
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText CloudHSMObjectState where
    parser = takeLowerText >>= \case
        "degraded" -> pure Degraded
        "ready" -> pure Ready
        "updating" -> pure Updating
        e -> fromTextError $ "Failure parsing CloudHSMObjectState from value: '" <> e
           <> "'. Accepted values: degraded, ready, updating"

instance ToText CloudHSMObjectState where
    toText = \case
        Degraded -> "degraded"
        Ready -> "ready"
        Updating -> "updating"

instance Hashable CloudHSMObjectState
instance ToQuery CloudHSMObjectState
instance ToHeader CloudHSMObjectState

instance FromJSON CloudHSMObjectState where
    parseJSON = parseJSONText "CloudHSMObjectState"

data HSMStatus
    = HSRunning
    | HSUpdating
    | HSTerminated
    | HSPending
    | HSTerminating
    | HSSuspended
    | HSDegraded
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText HSMStatus where
    parser = takeLowerText >>= \case
        "degraded" -> pure HSDegraded
        "pending" -> pure HSPending
        "running" -> pure HSRunning
        "suspended" -> pure HSSuspended
        "terminated" -> pure HSTerminated
        "terminating" -> pure HSTerminating
        "updating" -> pure HSUpdating
        e -> fromTextError $ "Failure parsing HSMStatus from value: '" <> e
           <> "'. Accepted values: degraded, pending, running, suspended, terminated, terminating, updating"

instance ToText HSMStatus where
    toText = \case
        HSDegraded -> "degraded"
        HSPending -> "pending"
        HSRunning -> "running"
        HSSuspended -> "suspended"
        HSTerminated -> "terminated"
        HSTerminating -> "terminating"
        HSUpdating -> "updating"

instance Hashable HSMStatus
instance ToQuery HSMStatus
instance ToHeader HSMStatus

instance FromJSON HSMStatus where
    parseJSON = parseJSONText "HSMStatus"

data SubscriptionType =
    Production
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText SubscriptionType where
    parser = takeLowerText >>= \case
        "production" -> pure Production
        e -> fromTextError $ "Failure parsing SubscriptionType from value: '" <> e
           <> "'. Accepted values: production"

instance ToText SubscriptionType where
    toText = \case
        Production -> "production"

instance Hashable SubscriptionType
instance ToQuery SubscriptionType
instance ToHeader SubscriptionType

instance ToJSON SubscriptionType where
    toJSON = toJSONText

instance FromJSON SubscriptionType where
    parseJSON = parseJSONText "SubscriptionType"
