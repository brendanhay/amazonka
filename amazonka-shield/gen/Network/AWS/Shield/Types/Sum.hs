{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Shield.Types.Sum
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Shield.Types.Sum where

import Network.AWS.Prelude

data AttackLayer
  = Application
  | Network
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText AttackLayer where
    parser = takeLowerText >>= \case
        "application" -> pure Application
        "network" -> pure Network
        e -> fromTextError $ "Failure parsing AttackLayer from value: '" <> e
           <> "'. Accepted values: application, network"

instance ToText AttackLayer where
    toText = \case
        Application -> "APPLICATION"
        Network -> "NETWORK"

instance Hashable     AttackLayer
instance NFData       AttackLayer
instance ToByteString AttackLayer
instance ToQuery      AttackLayer
instance ToHeader     AttackLayer

instance FromJSON AttackLayer where
    parseJSON = parseJSONText "AttackLayer"

data AttackPropertyIdentifier
  = DestinationURL
  | Referrer
  | SourceASN
  | SourceCountry
  | SourceIPAddress
  | SourceUserAgent
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText AttackPropertyIdentifier where
    parser = takeLowerText >>= \case
        "destination_url" -> pure DestinationURL
        "referrer" -> pure Referrer
        "source_asn" -> pure SourceASN
        "source_country" -> pure SourceCountry
        "source_ip_address" -> pure SourceIPAddress
        "source_user_agent" -> pure SourceUserAgent
        e -> fromTextError $ "Failure parsing AttackPropertyIdentifier from value: '" <> e
           <> "'. Accepted values: destination_url, referrer, source_asn, source_country, source_ip_address, source_user_agent"

instance ToText AttackPropertyIdentifier where
    toText = \case
        DestinationURL -> "DESTINATION_URL"
        Referrer -> "REFERRER"
        SourceASN -> "SOURCE_ASN"
        SourceCountry -> "SOURCE_COUNTRY"
        SourceIPAddress -> "SOURCE_IP_ADDRESS"
        SourceUserAgent -> "SOURCE_USER_AGENT"

instance Hashable     AttackPropertyIdentifier
instance NFData       AttackPropertyIdentifier
instance ToByteString AttackPropertyIdentifier
instance ToQuery      AttackPropertyIdentifier
instance ToHeader     AttackPropertyIdentifier

instance FromJSON AttackPropertyIdentifier where
    parseJSON = parseJSONText "AttackPropertyIdentifier"

data SubResourceType
  = IP
  | URL
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText SubResourceType where
    parser = takeLowerText >>= \case
        "ip" -> pure IP
        "url" -> pure URL
        e -> fromTextError $ "Failure parsing SubResourceType from value: '" <> e
           <> "'. Accepted values: ip, url"

instance ToText SubResourceType where
    toText = \case
        IP -> "IP"
        URL -> "URL"

instance Hashable     SubResourceType
instance NFData       SubResourceType
instance ToByteString SubResourceType
instance ToQuery      SubResourceType
instance ToHeader     SubResourceType

instance FromJSON SubResourceType where
    parseJSON = parseJSONText "SubResourceType"

data SubscriptionState
  = Active
  | Inactive
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText SubscriptionState where
    parser = takeLowerText >>= \case
        "active" -> pure Active
        "inactive" -> pure Inactive
        e -> fromTextError $ "Failure parsing SubscriptionState from value: '" <> e
           <> "'. Accepted values: active, inactive"

instance ToText SubscriptionState where
    toText = \case
        Active -> "ACTIVE"
        Inactive -> "INACTIVE"

instance Hashable     SubscriptionState
instance NFData       SubscriptionState
instance ToByteString SubscriptionState
instance ToQuery      SubscriptionState
instance ToHeader     SubscriptionState

instance FromJSON SubscriptionState where
    parseJSON = parseJSONText "SubscriptionState"

data Unit
  = Bits
  | Bytes
  | Packets
  | Requests
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText Unit where
    parser = takeLowerText >>= \case
        "bits" -> pure Bits
        "bytes" -> pure Bytes
        "packets" -> pure Packets
        "requests" -> pure Requests
        e -> fromTextError $ "Failure parsing Unit from value: '" <> e
           <> "'. Accepted values: bits, bytes, packets, requests"

instance ToText Unit where
    toText = \case
        Bits -> "BITS"
        Bytes -> "BYTES"
        Packets -> "PACKETS"
        Requests -> "REQUESTS"

instance Hashable     Unit
instance NFData       Unit
instance ToByteString Unit
instance ToQuery      Unit
instance ToHeader     Unit

instance FromJSON Unit where
    parseJSON = parseJSONText "Unit"
