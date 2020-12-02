{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatch.Types.StandardUnit
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatch.Types.StandardUnit where

import Network.AWS.Prelude

data StandardUnit
  = Bits
  | BitsSecond
  | Bytes
  | BytesSecond
  | Count
  | CountSecond
  | Gigabits
  | GigabitsSecond
  | Gigabytes
  | GigabytesSecond
  | Kilobits
  | KilobitsSecond
  | Kilobytes
  | KilobytesSecond
  | Megabits
  | MegabitsSecond
  | Megabytes
  | MegabytesSecond
  | Microseconds
  | Milliseconds
  | None
  | Percent
  | Seconds
  | Terabits
  | TerabitsSecond
  | Terabytes
  | TerabytesSecond
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

instance FromText StandardUnit where
  parser =
    takeLowerText >>= \case
      "bits" -> pure Bits
      "bits/second" -> pure BitsSecond
      "bytes" -> pure Bytes
      "bytes/second" -> pure BytesSecond
      "count" -> pure Count
      "count/second" -> pure CountSecond
      "gigabits" -> pure Gigabits
      "gigabits/second" -> pure GigabitsSecond
      "gigabytes" -> pure Gigabytes
      "gigabytes/second" -> pure GigabytesSecond
      "kilobits" -> pure Kilobits
      "kilobits/second" -> pure KilobitsSecond
      "kilobytes" -> pure Kilobytes
      "kilobytes/second" -> pure KilobytesSecond
      "megabits" -> pure Megabits
      "megabits/second" -> pure MegabitsSecond
      "megabytes" -> pure Megabytes
      "megabytes/second" -> pure MegabytesSecond
      "microseconds" -> pure Microseconds
      "milliseconds" -> pure Milliseconds
      "none" -> pure None
      "percent" -> pure Percent
      "seconds" -> pure Seconds
      "terabits" -> pure Terabits
      "terabits/second" -> pure TerabitsSecond
      "terabytes" -> pure Terabytes
      "terabytes/second" -> pure TerabytesSecond
      e ->
        fromTextError $
          "Failure parsing StandardUnit from value: '" <> e
            <> "'. Accepted values: bits, bits/second, bytes, bytes/second, count, count/second, gigabits, gigabits/second, gigabytes, gigabytes/second, kilobits, kilobits/second, kilobytes, kilobytes/second, megabits, megabits/second, megabytes, megabytes/second, microseconds, milliseconds, none, percent, seconds, terabits, terabits/second, terabytes, terabytes/second"

instance ToText StandardUnit where
  toText = \case
    Bits -> "Bits"
    BitsSecond -> "Bits/Second"
    Bytes -> "Bytes"
    BytesSecond -> "Bytes/Second"
    Count -> "Count"
    CountSecond -> "Count/Second"
    Gigabits -> "Gigabits"
    GigabitsSecond -> "Gigabits/Second"
    Gigabytes -> "Gigabytes"
    GigabytesSecond -> "Gigabytes/Second"
    Kilobits -> "Kilobits"
    KilobitsSecond -> "Kilobits/Second"
    Kilobytes -> "Kilobytes"
    KilobytesSecond -> "Kilobytes/Second"
    Megabits -> "Megabits"
    MegabitsSecond -> "Megabits/Second"
    Megabytes -> "Megabytes"
    MegabytesSecond -> "Megabytes/Second"
    Microseconds -> "Microseconds"
    Milliseconds -> "Milliseconds"
    None -> "None"
    Percent -> "Percent"
    Seconds -> "Seconds"
    Terabits -> "Terabits"
    TerabitsSecond -> "Terabits/Second"
    Terabytes -> "Terabytes"
    TerabytesSecond -> "Terabytes/Second"

instance Hashable StandardUnit

instance NFData StandardUnit

instance ToByteString StandardUnit

instance ToQuery StandardUnit

instance ToHeader StandardUnit

instance FromXML StandardUnit where
  parseXML = parseXMLText "StandardUnit"
