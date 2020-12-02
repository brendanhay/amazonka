{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.Unit
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.Unit where

import Network.AWS.Prelude

data Unit
  = Bits
  | BitsPerSecond
  | Bytes
  | BytesPerSecond
  | Count
  | CountPerSecond
  | GigaBits
  | GigaBitsPerSecond
  | GigaBytes
  | GigaBytesPerSecond
  | KiloBits
  | KiloBitsPerSecond
  | KiloBytes
  | KiloBytesPerSecond
  | MegaBits
  | MegaBitsPerSecond
  | MegaBytes
  | MegaBytesPerSecond
  | MicroSeconds
  | MilliSeconds
  | None
  | Percent
  | Seconds
  | TeraBits
  | TeraBitsPerSecond
  | TeraBytes
  | TeraBytesPerSecond
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

instance FromText Unit where
  parser =
    takeLowerText >>= \case
      "bits" -> pure Bits
      "bits_per_second" -> pure BitsPerSecond
      "bytes" -> pure Bytes
      "bytes_per_second" -> pure BytesPerSecond
      "count" -> pure Count
      "count_per_second" -> pure CountPerSecond
      "giga_bits" -> pure GigaBits
      "giga_bits_per_second" -> pure GigaBitsPerSecond
      "giga_bytes" -> pure GigaBytes
      "giga_bytes_per_second" -> pure GigaBytesPerSecond
      "kilo_bits" -> pure KiloBits
      "kilo_bits_per_second" -> pure KiloBitsPerSecond
      "kilo_bytes" -> pure KiloBytes
      "kilo_bytes_per_second" -> pure KiloBytesPerSecond
      "mega_bits" -> pure MegaBits
      "mega_bits_per_second" -> pure MegaBitsPerSecond
      "mega_bytes" -> pure MegaBytes
      "mega_bytes_per_second" -> pure MegaBytesPerSecond
      "micro_seconds" -> pure MicroSeconds
      "milli_seconds" -> pure MilliSeconds
      "none" -> pure None
      "percent" -> pure Percent
      "seconds" -> pure Seconds
      "tera_bits" -> pure TeraBits
      "tera_bits_per_second" -> pure TeraBitsPerSecond
      "tera_bytes" -> pure TeraBytes
      "tera_bytes_per_second" -> pure TeraBytesPerSecond
      e ->
        fromTextError $
          "Failure parsing Unit from value: '" <> e
            <> "'. Accepted values: bits, bits_per_second, bytes, bytes_per_second, count, count_per_second, giga_bits, giga_bits_per_second, giga_bytes, giga_bytes_per_second, kilo_bits, kilo_bits_per_second, kilo_bytes, kilo_bytes_per_second, mega_bits, mega_bits_per_second, mega_bytes, mega_bytes_per_second, micro_seconds, milli_seconds, none, percent, seconds, tera_bits, tera_bits_per_second, tera_bytes, tera_bytes_per_second"

instance ToText Unit where
  toText = \case
    Bits -> "BITS"
    BitsPerSecond -> "BITS_PER_SECOND"
    Bytes -> "BYTES"
    BytesPerSecond -> "BYTES_PER_SECOND"
    Count -> "COUNT"
    CountPerSecond -> "COUNT_PER_SECOND"
    GigaBits -> "GIGA_BITS"
    GigaBitsPerSecond -> "GIGA_BITS_PER_SECOND"
    GigaBytes -> "GIGA_BYTES"
    GigaBytesPerSecond -> "GIGA_BYTES_PER_SECOND"
    KiloBits -> "KILO_BITS"
    KiloBitsPerSecond -> "KILO_BITS_PER_SECOND"
    KiloBytes -> "KILO_BYTES"
    KiloBytesPerSecond -> "KILO_BYTES_PER_SECOND"
    MegaBits -> "MEGA_BITS"
    MegaBitsPerSecond -> "MEGA_BITS_PER_SECOND"
    MegaBytes -> "MEGA_BYTES"
    MegaBytesPerSecond -> "MEGA_BYTES_PER_SECOND"
    MicroSeconds -> "MICRO_SECONDS"
    MilliSeconds -> "MILLI_SECONDS"
    None -> "NONE"
    Percent -> "PERCENT"
    Seconds -> "SECONDS"
    TeraBits -> "TERA_BITS"
    TeraBitsPerSecond -> "TERA_BITS_PER_SECOND"
    TeraBytes -> "TERA_BYTES"
    TeraBytesPerSecond -> "TERA_BYTES_PER_SECOND"

instance Hashable Unit

instance NFData Unit

instance ToByteString Unit

instance ToQuery Unit

instance ToHeader Unit

instance ToJSON Unit where
  toJSON = toJSONText

instance FromJSON Unit where
  parseJSON = parseJSONText "Unit"
