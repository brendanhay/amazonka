{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Snowball.Types.Sum
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Snowball.Types.Sum where

import           Network.AWS.Prelude

data JobState
    = Cancelled
    | Complete
    | InProgress
    | InTransitToAWS
    | InTransitToCustomer
    | Listing
    | New
    | Pending
    | PreparingAppliance
    | PreparingShipment
    | WithAWS
    | WithCustomer
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText JobState where
    parser = takeLowerText >>= \case
        "cancelled" -> pure Cancelled
        "complete" -> pure Complete
        "inprogress" -> pure InProgress
        "intransittoaws" -> pure InTransitToAWS
        "intransittocustomer" -> pure InTransitToCustomer
        "listing" -> pure Listing
        "new" -> pure New
        "pending" -> pure Pending
        "preparingappliance" -> pure PreparingAppliance
        "preparingshipment" -> pure PreparingShipment
        "withaws" -> pure WithAWS
        "withcustomer" -> pure WithCustomer
        e -> fromTextError $ "Failure parsing JobState from value: '" <> e
           <> "'. Accepted values: Cancelled, Complete, InProgress, InTransitToAWS, InTransitToCustomer, Listing, New, Pending, PreparingAppliance, PreparingShipment, WithAWS, WithCustomer"

instance ToText JobState where
    toText = \case
        Cancelled -> "Cancelled"
        Complete -> "Complete"
        InProgress -> "InProgress"
        InTransitToAWS -> "InTransitToAWS"
        InTransitToCustomer -> "InTransitToCustomer"
        Listing -> "Listing"
        New -> "New"
        Pending -> "Pending"
        PreparingAppliance -> "PreparingAppliance"
        PreparingShipment -> "PreparingShipment"
        WithAWS -> "WithAWS"
        WithCustomer -> "WithCustomer"

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
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText JobType where
    parser = takeLowerText >>= \case
        "export" -> pure Export
        "import" -> pure Import
        e -> fromTextError $ "Failure parsing JobType from value: '" <> e
           <> "'. Accepted values: EXPORT, IMPORT"

instance ToText JobType where
    toText = \case
        Export -> "EXPORT"
        Import -> "IMPORT"

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
    = Express
    | NextDay
    | SecondDay
    | Standard
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText ShippingOption where
    parser = takeLowerText >>= \case
        "express" -> pure Express
        "next_day" -> pure NextDay
        "second_day" -> pure SecondDay
        "standard" -> pure Standard
        e -> fromTextError $ "Failure parsing ShippingOption from value: '" <> e
           <> "'. Accepted values: EXPRESS, NEXT_DAY, SECOND_DAY, STANDARD"

instance ToText ShippingOption where
    toText = \case
        Express -> "EXPRESS"
        NextDay -> "NEXT_DAY"
        SecondDay -> "SECOND_DAY"
        Standard -> "STANDARD"

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
    | T50
    | T80
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText SnowballCapacity where
    parser = takeLowerText >>= \case
        "nopreference" -> pure NoPreference
        "t50" -> pure T50
        "t80" -> pure T80
        e -> fromTextError $ "Failure parsing SnowballCapacity from value: '" <> e
           <> "'. Accepted values: NoPreference, T50, T80"

instance ToText SnowballCapacity where
    toText = \case
        NoPreference -> "NoPreference"
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
