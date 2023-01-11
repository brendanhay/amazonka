{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.LicenseManager.Types.EntitlementDataUnit
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LicenseManager.Types.EntitlementDataUnit
  ( EntitlementDataUnit
      ( ..,
        EntitlementDataUnit_Bits,
        EntitlementDataUnit_Bits_Second,
        EntitlementDataUnit_Bytes,
        EntitlementDataUnit_Bytes_Second,
        EntitlementDataUnit_Count,
        EntitlementDataUnit_Count_Second,
        EntitlementDataUnit_Gigabits,
        EntitlementDataUnit_Gigabits_Second,
        EntitlementDataUnit_Gigabytes,
        EntitlementDataUnit_Gigabytes_Second,
        EntitlementDataUnit_Kilobits,
        EntitlementDataUnit_Kilobits_Second,
        EntitlementDataUnit_Kilobytes,
        EntitlementDataUnit_Kilobytes_Second,
        EntitlementDataUnit_Megabits,
        EntitlementDataUnit_Megabits_Second,
        EntitlementDataUnit_Megabytes,
        EntitlementDataUnit_Megabytes_Second,
        EntitlementDataUnit_Microseconds,
        EntitlementDataUnit_Milliseconds,
        EntitlementDataUnit_None,
        EntitlementDataUnit_Percent,
        EntitlementDataUnit_Seconds,
        EntitlementDataUnit_Terabits,
        EntitlementDataUnit_Terabits_Second,
        EntitlementDataUnit_Terabytes,
        EntitlementDataUnit_Terabytes_Second
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype EntitlementDataUnit = EntitlementDataUnit'
  { fromEntitlementDataUnit ::
      Data.Text
  }
  deriving stock
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Generic
    )
  deriving newtype
    ( Prelude.Hashable,
      Prelude.NFData,
      Data.FromText,
      Data.ToText,
      Data.ToByteString,
      Data.ToLog,
      Data.ToHeader,
      Data.ToQuery,
      Data.FromJSON,
      Data.FromJSONKey,
      Data.ToJSON,
      Data.ToJSONKey,
      Data.FromXML,
      Data.ToXML
    )

pattern EntitlementDataUnit_Bits :: EntitlementDataUnit
pattern EntitlementDataUnit_Bits = EntitlementDataUnit' "Bits"

pattern EntitlementDataUnit_Bits_Second :: EntitlementDataUnit
pattern EntitlementDataUnit_Bits_Second = EntitlementDataUnit' "Bits/Second"

pattern EntitlementDataUnit_Bytes :: EntitlementDataUnit
pattern EntitlementDataUnit_Bytes = EntitlementDataUnit' "Bytes"

pattern EntitlementDataUnit_Bytes_Second :: EntitlementDataUnit
pattern EntitlementDataUnit_Bytes_Second = EntitlementDataUnit' "Bytes/Second"

pattern EntitlementDataUnit_Count :: EntitlementDataUnit
pattern EntitlementDataUnit_Count = EntitlementDataUnit' "Count"

pattern EntitlementDataUnit_Count_Second :: EntitlementDataUnit
pattern EntitlementDataUnit_Count_Second = EntitlementDataUnit' "Count/Second"

pattern EntitlementDataUnit_Gigabits :: EntitlementDataUnit
pattern EntitlementDataUnit_Gigabits = EntitlementDataUnit' "Gigabits"

pattern EntitlementDataUnit_Gigabits_Second :: EntitlementDataUnit
pattern EntitlementDataUnit_Gigabits_Second = EntitlementDataUnit' "Gigabits/Second"

pattern EntitlementDataUnit_Gigabytes :: EntitlementDataUnit
pattern EntitlementDataUnit_Gigabytes = EntitlementDataUnit' "Gigabytes"

pattern EntitlementDataUnit_Gigabytes_Second :: EntitlementDataUnit
pattern EntitlementDataUnit_Gigabytes_Second = EntitlementDataUnit' "Gigabytes/Second"

pattern EntitlementDataUnit_Kilobits :: EntitlementDataUnit
pattern EntitlementDataUnit_Kilobits = EntitlementDataUnit' "Kilobits"

pattern EntitlementDataUnit_Kilobits_Second :: EntitlementDataUnit
pattern EntitlementDataUnit_Kilobits_Second = EntitlementDataUnit' "Kilobits/Second"

pattern EntitlementDataUnit_Kilobytes :: EntitlementDataUnit
pattern EntitlementDataUnit_Kilobytes = EntitlementDataUnit' "Kilobytes"

pattern EntitlementDataUnit_Kilobytes_Second :: EntitlementDataUnit
pattern EntitlementDataUnit_Kilobytes_Second = EntitlementDataUnit' "Kilobytes/Second"

pattern EntitlementDataUnit_Megabits :: EntitlementDataUnit
pattern EntitlementDataUnit_Megabits = EntitlementDataUnit' "Megabits"

pattern EntitlementDataUnit_Megabits_Second :: EntitlementDataUnit
pattern EntitlementDataUnit_Megabits_Second = EntitlementDataUnit' "Megabits/Second"

pattern EntitlementDataUnit_Megabytes :: EntitlementDataUnit
pattern EntitlementDataUnit_Megabytes = EntitlementDataUnit' "Megabytes"

pattern EntitlementDataUnit_Megabytes_Second :: EntitlementDataUnit
pattern EntitlementDataUnit_Megabytes_Second = EntitlementDataUnit' "Megabytes/Second"

pattern EntitlementDataUnit_Microseconds :: EntitlementDataUnit
pattern EntitlementDataUnit_Microseconds = EntitlementDataUnit' "Microseconds"

pattern EntitlementDataUnit_Milliseconds :: EntitlementDataUnit
pattern EntitlementDataUnit_Milliseconds = EntitlementDataUnit' "Milliseconds"

pattern EntitlementDataUnit_None :: EntitlementDataUnit
pattern EntitlementDataUnit_None = EntitlementDataUnit' "None"

pattern EntitlementDataUnit_Percent :: EntitlementDataUnit
pattern EntitlementDataUnit_Percent = EntitlementDataUnit' "Percent"

pattern EntitlementDataUnit_Seconds :: EntitlementDataUnit
pattern EntitlementDataUnit_Seconds = EntitlementDataUnit' "Seconds"

pattern EntitlementDataUnit_Terabits :: EntitlementDataUnit
pattern EntitlementDataUnit_Terabits = EntitlementDataUnit' "Terabits"

pattern EntitlementDataUnit_Terabits_Second :: EntitlementDataUnit
pattern EntitlementDataUnit_Terabits_Second = EntitlementDataUnit' "Terabits/Second"

pattern EntitlementDataUnit_Terabytes :: EntitlementDataUnit
pattern EntitlementDataUnit_Terabytes = EntitlementDataUnit' "Terabytes"

pattern EntitlementDataUnit_Terabytes_Second :: EntitlementDataUnit
pattern EntitlementDataUnit_Terabytes_Second = EntitlementDataUnit' "Terabytes/Second"

{-# COMPLETE
  EntitlementDataUnit_Bits,
  EntitlementDataUnit_Bits_Second,
  EntitlementDataUnit_Bytes,
  EntitlementDataUnit_Bytes_Second,
  EntitlementDataUnit_Count,
  EntitlementDataUnit_Count_Second,
  EntitlementDataUnit_Gigabits,
  EntitlementDataUnit_Gigabits_Second,
  EntitlementDataUnit_Gigabytes,
  EntitlementDataUnit_Gigabytes_Second,
  EntitlementDataUnit_Kilobits,
  EntitlementDataUnit_Kilobits_Second,
  EntitlementDataUnit_Kilobytes,
  EntitlementDataUnit_Kilobytes_Second,
  EntitlementDataUnit_Megabits,
  EntitlementDataUnit_Megabits_Second,
  EntitlementDataUnit_Megabytes,
  EntitlementDataUnit_Megabytes_Second,
  EntitlementDataUnit_Microseconds,
  EntitlementDataUnit_Milliseconds,
  EntitlementDataUnit_None,
  EntitlementDataUnit_Percent,
  EntitlementDataUnit_Seconds,
  EntitlementDataUnit_Terabits,
  EntitlementDataUnit_Terabits_Second,
  EntitlementDataUnit_Terabytes,
  EntitlementDataUnit_Terabytes_Second,
  EntitlementDataUnit'
  #-}
