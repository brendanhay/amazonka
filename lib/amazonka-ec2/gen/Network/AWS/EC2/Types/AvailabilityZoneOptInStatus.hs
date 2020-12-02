{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.AvailabilityZoneOptInStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.AvailabilityZoneOptInStatus where

import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data AvailabilityZoneOptInStatus
  = AZOISNotOptedIn
  | AZOISOptInNotRequired
  | AZOISOptedIn
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

instance FromText AvailabilityZoneOptInStatus where
  parser =
    takeLowerText >>= \case
      "not-opted-in" -> pure AZOISNotOptedIn
      "opt-in-not-required" -> pure AZOISOptInNotRequired
      "opted-in" -> pure AZOISOptedIn
      e ->
        fromTextError $
          "Failure parsing AvailabilityZoneOptInStatus from value: '" <> e
            <> "'. Accepted values: not-opted-in, opt-in-not-required, opted-in"

instance ToText AvailabilityZoneOptInStatus where
  toText = \case
    AZOISNotOptedIn -> "not-opted-in"
    AZOISOptInNotRequired -> "opt-in-not-required"
    AZOISOptedIn -> "opted-in"

instance Hashable AvailabilityZoneOptInStatus

instance NFData AvailabilityZoneOptInStatus

instance ToByteString AvailabilityZoneOptInStatus

instance ToQuery AvailabilityZoneOptInStatus

instance ToHeader AvailabilityZoneOptInStatus

instance FromXML AvailabilityZoneOptInStatus where
  parseXML = parseXMLText "AvailabilityZoneOptInStatus"
