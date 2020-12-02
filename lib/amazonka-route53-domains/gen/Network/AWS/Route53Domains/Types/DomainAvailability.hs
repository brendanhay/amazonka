{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53Domains.Types.DomainAvailability
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53Domains.Types.DomainAvailability where

import Network.AWS.Prelude

data DomainAvailability
  = DAAvailable
  | DAAvailablePreorder
  | DAAvailableReserved
  | DADontKnow
  | DAReserved
  | DAUnavailable
  | DAUnavailablePremium
  | DAUnavailableRestricted
  deriving
    ( Eq,
      Ord,
      Read,
      Show,
      Enum,
      Bounded,
      Data,
      Typeable,
      Generic
    )

instance FromText DomainAvailability where
  parser =
    takeLowerText >>= \case
      "available" -> pure DAAvailable
      "available_preorder" -> pure DAAvailablePreorder
      "available_reserved" -> pure DAAvailableReserved
      "dont_know" -> pure DADontKnow
      "reserved" -> pure DAReserved
      "unavailable" -> pure DAUnavailable
      "unavailable_premium" -> pure DAUnavailablePremium
      "unavailable_restricted" -> pure DAUnavailableRestricted
      e ->
        fromTextError $
          "Failure parsing DomainAvailability from value: '" <> e
            <> "'. Accepted values: available, available_preorder, available_reserved, dont_know, reserved, unavailable, unavailable_premium, unavailable_restricted"

instance ToText DomainAvailability where
  toText = \case
    DAAvailable -> "AVAILABLE"
    DAAvailablePreorder -> "AVAILABLE_PREORDER"
    DAAvailableReserved -> "AVAILABLE_RESERVED"
    DADontKnow -> "DONT_KNOW"
    DAReserved -> "RESERVED"
    DAUnavailable -> "UNAVAILABLE"
    DAUnavailablePremium -> "UNAVAILABLE_PREMIUM"
    DAUnavailableRestricted -> "UNAVAILABLE_RESTRICTED"

instance Hashable DomainAvailability

instance NFData DomainAvailability

instance ToByteString DomainAvailability

instance ToQuery DomainAvailability

instance ToHeader DomainAvailability

instance FromJSON DomainAvailability where
  parseJSON = parseJSONText "DomainAvailability"
