{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.Types.DeviceAvailability
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DeviceFarm.Types.DeviceAvailability where

import Network.AWS.Prelude

data DeviceAvailability
  = Available
  | Busy
  | HighlyAvailable
  | TemporaryNotAvailable
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

instance FromText DeviceAvailability where
  parser =
    takeLowerText >>= \case
      "available" -> pure Available
      "busy" -> pure Busy
      "highly_available" -> pure HighlyAvailable
      "temporary_not_available" -> pure TemporaryNotAvailable
      e ->
        fromTextError $
          "Failure parsing DeviceAvailability from value: '" <> e
            <> "'. Accepted values: available, busy, highly_available, temporary_not_available"

instance ToText DeviceAvailability where
  toText = \case
    Available -> "AVAILABLE"
    Busy -> "BUSY"
    HighlyAvailable -> "HIGHLY_AVAILABLE"
    TemporaryNotAvailable -> "TEMPORARY_NOT_AVAILABLE"

instance Hashable DeviceAvailability

instance NFData DeviceAvailability

instance ToByteString DeviceAvailability

instance ToQuery DeviceAvailability

instance ToHeader DeviceAvailability

instance FromJSON DeviceAvailability where
  parseJSON = parseJSONText "DeviceAvailability"
