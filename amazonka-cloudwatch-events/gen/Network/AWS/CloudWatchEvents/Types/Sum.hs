{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchEvents.Types.Sum
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudWatchEvents.Types.Sum where

import Network.AWS.Prelude

data AssignPublicIP
  = APIDisabled
  | APIEnabled
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText AssignPublicIP where
    parser = takeLowerText >>= \case
        "disabled" -> pure APIDisabled
        "enabled" -> pure APIEnabled
        e -> fromTextError $ "Failure parsing AssignPublicIP from value: '" <> e
           <> "'. Accepted values: disabled, enabled"

instance ToText AssignPublicIP where
    toText = \case
        APIDisabled -> "DISABLED"
        APIEnabled -> "ENABLED"

instance Hashable     AssignPublicIP
instance NFData       AssignPublicIP
instance ToByteString AssignPublicIP
instance ToQuery      AssignPublicIP
instance ToHeader     AssignPublicIP

instance ToJSON AssignPublicIP where
    toJSON = toJSONText

instance FromJSON AssignPublicIP where
    parseJSON = parseJSONText "AssignPublicIP"

data EventSourceState
  = Active
  | Deleted
  | Pending
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText EventSourceState where
    parser = takeLowerText >>= \case
        "active" -> pure Active
        "deleted" -> pure Deleted
        "pending" -> pure Pending
        e -> fromTextError $ "Failure parsing EventSourceState from value: '" <> e
           <> "'. Accepted values: active, deleted, pending"

instance ToText EventSourceState where
    toText = \case
        Active -> "ACTIVE"
        Deleted -> "DELETED"
        Pending -> "PENDING"

instance Hashable     EventSourceState
instance NFData       EventSourceState
instance ToByteString EventSourceState
instance ToQuery      EventSourceState
instance ToHeader     EventSourceState

instance FromJSON EventSourceState where
    parseJSON = parseJSONText "EventSourceState"

data LaunchType
  = EC2
  | Fargate
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText LaunchType where
    parser = takeLowerText >>= \case
        "ec2" -> pure EC2
        "fargate" -> pure Fargate
        e -> fromTextError $ "Failure parsing LaunchType from value: '" <> e
           <> "'. Accepted values: ec2, fargate"

instance ToText LaunchType where
    toText = \case
        EC2 -> "EC2"
        Fargate -> "FARGATE"

instance Hashable     LaunchType
instance NFData       LaunchType
instance ToByteString LaunchType
instance ToQuery      LaunchType
instance ToHeader     LaunchType

instance ToJSON LaunchType where
    toJSON = toJSONText

instance FromJSON LaunchType where
    parseJSON = parseJSONText "LaunchType"

data RuleState
  = Disabled
  | Enabled
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText RuleState where
    parser = takeLowerText >>= \case
        "disabled" -> pure Disabled
        "enabled" -> pure Enabled
        e -> fromTextError $ "Failure parsing RuleState from value: '" <> e
           <> "'. Accepted values: disabled, enabled"

instance ToText RuleState where
    toText = \case
        Disabled -> "DISABLED"
        Enabled -> "ENABLED"

instance Hashable     RuleState
instance NFData       RuleState
instance ToByteString RuleState
instance ToQuery      RuleState
instance ToHeader     RuleState

instance ToJSON RuleState where
    toJSON = toJSONText

instance FromJSON RuleState where
    parseJSON = parseJSONText "RuleState"
