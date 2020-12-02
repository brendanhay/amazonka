{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.LocationType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.LocationType where

import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data LocationType
  = AvailabilityZone
  | AvailabilityZoneId
  | Region
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

instance FromText LocationType where
  parser =
    takeLowerText >>= \case
      "availability-zone" -> pure AvailabilityZone
      "availability-zone-id" -> pure AvailabilityZoneId
      "region" -> pure Region
      e ->
        fromTextError $
          "Failure parsing LocationType from value: '" <> e
            <> "'. Accepted values: availability-zone, availability-zone-id, region"

instance ToText LocationType where
  toText = \case
    AvailabilityZone -> "availability-zone"
    AvailabilityZoneId -> "availability-zone-id"
    Region -> "region"

instance Hashable LocationType

instance NFData LocationType

instance ToByteString LocationType

instance ToQuery LocationType

instance ToHeader LocationType

instance FromXML LocationType where
  parseXML = parseXMLText "LocationType"
