{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.Scte35DeviceRestrictions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.Scte35DeviceRestrictions where

import Network.AWS.Prelude

-- | Corresponds to the device_restrictions parameter in a segmentation_descriptor. If you include one of the "restriction" flags then you must include all four of them.
data Scte35DeviceRestrictions
  = SDRNone
  | SDRRestrictGROUP0
  | SDRRestrictGROUP1
  | SDRRestrictGROUP2
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

instance FromText Scte35DeviceRestrictions where
  parser =
    takeLowerText >>= \case
      "none" -> pure SDRNone
      "restrict_group0" -> pure SDRRestrictGROUP0
      "restrict_group1" -> pure SDRRestrictGROUP1
      "restrict_group2" -> pure SDRRestrictGROUP2
      e ->
        fromTextError $
          "Failure parsing Scte35DeviceRestrictions from value: '" <> e
            <> "'. Accepted values: none, restrict_group0, restrict_group1, restrict_group2"

instance ToText Scte35DeviceRestrictions where
  toText = \case
    SDRNone -> "NONE"
    SDRRestrictGROUP0 -> "RESTRICT_GROUP0"
    SDRRestrictGROUP1 -> "RESTRICT_GROUP1"
    SDRRestrictGROUP2 -> "RESTRICT_GROUP2"

instance Hashable Scte35DeviceRestrictions

instance NFData Scte35DeviceRestrictions

instance ToByteString Scte35DeviceRestrictions

instance ToQuery Scte35DeviceRestrictions

instance ToHeader Scte35DeviceRestrictions

instance ToJSON Scte35DeviceRestrictions where
  toJSON = toJSONText

instance FromJSON Scte35DeviceRestrictions where
  parseJSON = parseJSONText "Scte35DeviceRestrictions"
