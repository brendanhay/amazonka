{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Snowball.Types.Sum
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Snowball.Types.Sum where

import Network.AWS.Prelude

data ClusterState
  = AwaitingQuorum
  | Cancelled
  | Complete
  | InUse
  | Pending
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ClusterState where
    parser = takeLowerText >>= \case
        "awaitingquorum" -> pure AwaitingQuorum
        "cancelled" -> pure Cancelled
        "complete" -> pure Complete
        "inuse" -> pure InUse
        "pending" -> pure Pending
        e -> fromTextError $ "Failure parsing ClusterState from value: '" <> e
           <> "'. Accepted values: awaitingquorum, cancelled, complete, inuse, pending"

instance ToText ClusterState where
    toText = \case
        AwaitingQuorum -> "AwaitingQuorum"
        Cancelled -> "Cancelled"
        Complete -> "Complete"
        InUse -> "InUse"
        Pending -> "Pending"

instance Hashable     ClusterState
instance NFData       ClusterState
instance ToByteString ClusterState
instance ToQuery      ClusterState
instance ToHeader     ClusterState

instance FromJSON ClusterState where
    parseJSON = parseJSONText "ClusterState"

data JobState
  = JSCancelled
  | JSComplete
  | JSInProgress
  | JSInTransitToAWS
  | JSInTransitToCustomer
  | JSListing
  | JSNew
  | JSPending
  | JSPreparingAppliance
  | JSPreparingShipment
  | JSWithAWS
  | JSWithCustomer
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText JobState where
    parser = takeLowerText >>= \case
        "cancelled" -> pure JSCancelled
        "complete" -> pure JSComplete
        "inprogress" -> pure JSInProgress
        "intransittoaws" -> pure JSInTransitToAWS
        "intransittocustomer" -> pure JSInTransitToCustomer
        "listing" -> pure JSListing
        "new" -> pure JSNew
        "pending" -> pure JSPending
        "preparingappliance" -> pure JSPreparingAppliance
        "preparingshipment" -> pure JSPreparingShipment
        "withaws" -> pure JSWithAWS
        "withcustomer" -> pure JSWithCustomer
        e -> fromTextError $ "Failure parsing JobState from value: '" <> e
           <> "'. Accepted values: cancelled, complete, inprogress, intransittoaws, intransittocustomer, listing, new, pending, preparingappliance, preparingshipment, withaws, withcustomer"

instance ToText JobState where
    toText = \case
        JSCancelled -> "Cancelled"
        JSComplete -> "Complete"
        JSInProgress -> "InProgress"
        JSInTransitToAWS -> "InTransitToAWS"
        JSInTransitToCustomer -> "InTransitToCustomer"
        JSListing -> "Listing"
        JSNew -> "New"
        JSPending -> "Pending"
        JSPreparingAppliance -> "PreparingAppliance"
        JSPreparingShipment -> "PreparingShipment"
        JSWithAWS -> "WithAWS"
        JSWithCustomer -> "WithCustomer"

instance Hashable     JobState
instance NFData       JobState
instance ToByteString JobState
instance ToQuery      JobState
instance ToHeader     JobState

instance ToJSON JobState where
    toJSON = toJSONText

instance FromJSON JobState where
    parseJSON = parseJSONText "JobState"

data JobType
  = Export
  | Import
  | LocalUse
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText JobType where
    parser = takeLowerText >>= \case
        "export" -> pure Export
        "import" -> pure Import
        "local_use" -> pure LocalUse
        e -> fromTextError $ "Failure parsing JobType from value: '" <> e
           <> "'. Accepted values: export, import, local_use"

instance ToText JobType where
    toText = \case
        Export -> "EXPORT"
        Import -> "IMPORT"
        LocalUse -> "LOCAL_USE"

instance Hashable     JobType
instance NFData       JobType
instance ToByteString JobType
instance ToQuery      JobType
instance ToHeader     JobType

instance ToJSON JobType where
    toJSON = toJSONText

instance FromJSON JobType where
    parseJSON = parseJSONText "JobType"

data ShippingOption
  = SOExpress
  | SONextDay
  | SOSecondDay
  | SOStandard
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ShippingOption where
    parser = takeLowerText >>= \case
        "express" -> pure SOExpress
        "next_day" -> pure SONextDay
        "second_day" -> pure SOSecondDay
        "standard" -> pure SOStandard
        e -> fromTextError $ "Failure parsing ShippingOption from value: '" <> e
           <> "'. Accepted values: express, next_day, second_day, standard"

instance ToText ShippingOption where
    toText = \case
        SOExpress -> "EXPRESS"
        SONextDay -> "NEXT_DAY"
        SOSecondDay -> "SECOND_DAY"
        SOStandard -> "STANDARD"

instance Hashable     ShippingOption
instance NFData       ShippingOption
instance ToByteString ShippingOption
instance ToQuery      ShippingOption
instance ToHeader     ShippingOption

instance ToJSON ShippingOption where
    toJSON = toJSONText

instance FromJSON ShippingOption where
    parseJSON = parseJSONText "ShippingOption"

data SnowballCapacity
  = NoPreference
  | T100
  | T50
  | T80
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText SnowballCapacity where
    parser = takeLowerText >>= \case
        "nopreference" -> pure NoPreference
        "t100" -> pure T100
        "t50" -> pure T50
        "t80" -> pure T80
        e -> fromTextError $ "Failure parsing SnowballCapacity from value: '" <> e
           <> "'. Accepted values: nopreference, t100, t50, t80"

instance ToText SnowballCapacity where
    toText = \case
        NoPreference -> "NoPreference"
        T100 -> "T100"
        T50 -> "T50"
        T80 -> "T80"

instance Hashable     SnowballCapacity
instance NFData       SnowballCapacity
instance ToByteString SnowballCapacity
instance ToQuery      SnowballCapacity
instance ToHeader     SnowballCapacity

instance ToJSON SnowballCapacity where
    toJSON = toJSONText

instance FromJSON SnowballCapacity where
    parseJSON = parseJSONText "SnowballCapacity"

data SnowballType
  = Edge
  | Standard
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText SnowballType where
    parser = takeLowerText >>= \case
        "edge" -> pure Edge
        "standard" -> pure Standard
        e -> fromTextError $ "Failure parsing SnowballType from value: '" <> e
           <> "'. Accepted values: edge, standard"

instance ToText SnowballType where
    toText = \case
        Edge -> "EDGE"
        Standard -> "STANDARD"

instance Hashable     SnowballType
instance NFData       SnowballType
instance ToByteString SnowballType
instance ToQuery      SnowballType
instance ToHeader     SnowballType

instance ToJSON SnowballType where
    toJSON = toJSONText

instance FromJSON SnowballType where
    parseJSON = parseJSONText "SnowballType"
