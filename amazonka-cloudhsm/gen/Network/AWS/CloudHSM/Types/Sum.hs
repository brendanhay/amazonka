{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudHSM.Types.Sum
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudHSM.Types.Sum where

import Network.AWS.Prelude

data ClientVersion
  = VD5_1
  | VD5_3
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ClientVersion where
    parser = takeLowerText >>= \case
        "5.1" -> pure VD5_1
        "5.3" -> pure VD5_3
        e -> fromTextError $ "Failure parsing ClientVersion from value: '" <> e
           <> "'. Accepted values: 5.1, 5.3"

instance ToText ClientVersion where
    toText = \case
        VD5_1 -> "5.1"
        VD5_3 -> "5.3"

instance Hashable     ClientVersion
instance NFData       ClientVersion
instance ToByteString ClientVersion
instance ToQuery      ClientVersion
instance ToHeader     ClientVersion

instance ToJSON ClientVersion where
    toJSON = toJSONText

data CloudHSMObjectState
  = Degraded
  | Ready
  | Updating
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText CloudHSMObjectState where
    parser = takeLowerText >>= \case
        "degraded" -> pure Degraded
        "ready" -> pure Ready
        "updating" -> pure Updating
        e -> fromTextError $ "Failure parsing CloudHSMObjectState from value: '" <> e
           <> "'. Accepted values: degraded, ready, updating"

instance ToText CloudHSMObjectState where
    toText = \case
        Degraded -> "DEGRADED"
        Ready -> "READY"
        Updating -> "UPDATING"

instance Hashable     CloudHSMObjectState
instance NFData       CloudHSMObjectState
instance ToByteString CloudHSMObjectState
instance ToQuery      CloudHSMObjectState
instance ToHeader     CloudHSMObjectState

instance FromJSON CloudHSMObjectState where
    parseJSON = parseJSONText "CloudHSMObjectState"

data HSMStatus
  = HSDegraded
  | HSPending
  | HSRunning
  | HSSuspended
  | HSTerminated
  | HSTerminating
  | HSUpdating
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


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
        HSDegraded -> "DEGRADED"
        HSPending -> "PENDING"
        HSRunning -> "RUNNING"
        HSSuspended -> "SUSPENDED"
        HSTerminated -> "TERMINATED"
        HSTerminating -> "TERMINATING"
        HSUpdating -> "UPDATING"

instance Hashable     HSMStatus
instance NFData       HSMStatus
instance ToByteString HSMStatus
instance ToQuery      HSMStatus
instance ToHeader     HSMStatus

instance FromJSON HSMStatus where
    parseJSON = parseJSONText "HSMStatus"

-- | Specifies the type of subscription for the HSM.
--
--
--     * __PRODUCTION__ - The HSM is being used in a production environment.
--
--     * __TRIAL__ - The HSM is being used in a product trial.
--
--
--
data SubscriptionType =
  Production
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText SubscriptionType where
    parser = takeLowerText >>= \case
        "production" -> pure Production
        e -> fromTextError $ "Failure parsing SubscriptionType from value: '" <> e
           <> "'. Accepted values: production"

instance ToText SubscriptionType where
    toText = \case
        Production -> "PRODUCTION"

instance Hashable     SubscriptionType
instance NFData       SubscriptionType
instance ToByteString SubscriptionType
instance ToQuery      SubscriptionType
instance ToHeader     SubscriptionType

instance ToJSON SubscriptionType where
    toJSON = toJSONText

instance FromJSON SubscriptionType where
    parseJSON = parseJSONText "SubscriptionType"
