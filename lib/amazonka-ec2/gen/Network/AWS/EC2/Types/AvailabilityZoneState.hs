{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.AvailabilityZoneState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.AvailabilityZoneState where

import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data AvailabilityZoneState
  = AZSAvailable
  | AZSImpaired
  | AZSInformation
  | AZSUnavailable
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

instance FromText AvailabilityZoneState where
  parser =
    takeLowerText >>= \case
      "available" -> pure AZSAvailable
      "impaired" -> pure AZSImpaired
      "information" -> pure AZSInformation
      "unavailable" -> pure AZSUnavailable
      e ->
        fromTextError $
          "Failure parsing AvailabilityZoneState from value: '" <> e
            <> "'. Accepted values: available, impaired, information, unavailable"

instance ToText AvailabilityZoneState where
  toText = \case
    AZSAvailable -> "available"
    AZSImpaired -> "impaired"
    AZSInformation -> "information"
    AZSUnavailable -> "unavailable"

instance Hashable AvailabilityZoneState

instance NFData AvailabilityZoneState

instance ToByteString AvailabilityZoneState

instance ToQuery AvailabilityZoneState

instance ToHeader AvailabilityZoneState

instance FromXML AvailabilityZoneState where
  parseXML = parseXMLText "AvailabilityZoneState"
