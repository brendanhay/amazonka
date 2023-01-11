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
-- Module      : Amazonka.LicenseManager.Types.EntitlementUnit
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LicenseManager.Types.EntitlementUnit
  ( EntitlementUnit
      ( ..,
        EntitlementUnit_Bits,
        EntitlementUnit_Bits_Second,
        EntitlementUnit_Bytes,
        EntitlementUnit_Bytes_Second,
        EntitlementUnit_Count,
        EntitlementUnit_Count_Second,
        EntitlementUnit_Gigabits,
        EntitlementUnit_Gigabits_Second,
        EntitlementUnit_Gigabytes,
        EntitlementUnit_Gigabytes_Second,
        EntitlementUnit_Kilobits,
        EntitlementUnit_Kilobits_Second,
        EntitlementUnit_Kilobytes,
        EntitlementUnit_Kilobytes_Second,
        EntitlementUnit_Megabits,
        EntitlementUnit_Megabits_Second,
        EntitlementUnit_Megabytes,
        EntitlementUnit_Megabytes_Second,
        EntitlementUnit_Microseconds,
        EntitlementUnit_Milliseconds,
        EntitlementUnit_None,
        EntitlementUnit_Percent,
        EntitlementUnit_Seconds,
        EntitlementUnit_Terabits,
        EntitlementUnit_Terabits_Second,
        EntitlementUnit_Terabytes,
        EntitlementUnit_Terabytes_Second
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype EntitlementUnit = EntitlementUnit'
  { fromEntitlementUnit ::
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

pattern EntitlementUnit_Bits :: EntitlementUnit
pattern EntitlementUnit_Bits = EntitlementUnit' "Bits"

pattern EntitlementUnit_Bits_Second :: EntitlementUnit
pattern EntitlementUnit_Bits_Second = EntitlementUnit' "Bits/Second"

pattern EntitlementUnit_Bytes :: EntitlementUnit
pattern EntitlementUnit_Bytes = EntitlementUnit' "Bytes"

pattern EntitlementUnit_Bytes_Second :: EntitlementUnit
pattern EntitlementUnit_Bytes_Second = EntitlementUnit' "Bytes/Second"

pattern EntitlementUnit_Count :: EntitlementUnit
pattern EntitlementUnit_Count = EntitlementUnit' "Count"

pattern EntitlementUnit_Count_Second :: EntitlementUnit
pattern EntitlementUnit_Count_Second = EntitlementUnit' "Count/Second"

pattern EntitlementUnit_Gigabits :: EntitlementUnit
pattern EntitlementUnit_Gigabits = EntitlementUnit' "Gigabits"

pattern EntitlementUnit_Gigabits_Second :: EntitlementUnit
pattern EntitlementUnit_Gigabits_Second = EntitlementUnit' "Gigabits/Second"

pattern EntitlementUnit_Gigabytes :: EntitlementUnit
pattern EntitlementUnit_Gigabytes = EntitlementUnit' "Gigabytes"

pattern EntitlementUnit_Gigabytes_Second :: EntitlementUnit
pattern EntitlementUnit_Gigabytes_Second = EntitlementUnit' "Gigabytes/Second"

pattern EntitlementUnit_Kilobits :: EntitlementUnit
pattern EntitlementUnit_Kilobits = EntitlementUnit' "Kilobits"

pattern EntitlementUnit_Kilobits_Second :: EntitlementUnit
pattern EntitlementUnit_Kilobits_Second = EntitlementUnit' "Kilobits/Second"

pattern EntitlementUnit_Kilobytes :: EntitlementUnit
pattern EntitlementUnit_Kilobytes = EntitlementUnit' "Kilobytes"

pattern EntitlementUnit_Kilobytes_Second :: EntitlementUnit
pattern EntitlementUnit_Kilobytes_Second = EntitlementUnit' "Kilobytes/Second"

pattern EntitlementUnit_Megabits :: EntitlementUnit
pattern EntitlementUnit_Megabits = EntitlementUnit' "Megabits"

pattern EntitlementUnit_Megabits_Second :: EntitlementUnit
pattern EntitlementUnit_Megabits_Second = EntitlementUnit' "Megabits/Second"

pattern EntitlementUnit_Megabytes :: EntitlementUnit
pattern EntitlementUnit_Megabytes = EntitlementUnit' "Megabytes"

pattern EntitlementUnit_Megabytes_Second :: EntitlementUnit
pattern EntitlementUnit_Megabytes_Second = EntitlementUnit' "Megabytes/Second"

pattern EntitlementUnit_Microseconds :: EntitlementUnit
pattern EntitlementUnit_Microseconds = EntitlementUnit' "Microseconds"

pattern EntitlementUnit_Milliseconds :: EntitlementUnit
pattern EntitlementUnit_Milliseconds = EntitlementUnit' "Milliseconds"

pattern EntitlementUnit_None :: EntitlementUnit
pattern EntitlementUnit_None = EntitlementUnit' "None"

pattern EntitlementUnit_Percent :: EntitlementUnit
pattern EntitlementUnit_Percent = EntitlementUnit' "Percent"

pattern EntitlementUnit_Seconds :: EntitlementUnit
pattern EntitlementUnit_Seconds = EntitlementUnit' "Seconds"

pattern EntitlementUnit_Terabits :: EntitlementUnit
pattern EntitlementUnit_Terabits = EntitlementUnit' "Terabits"

pattern EntitlementUnit_Terabits_Second :: EntitlementUnit
pattern EntitlementUnit_Terabits_Second = EntitlementUnit' "Terabits/Second"

pattern EntitlementUnit_Terabytes :: EntitlementUnit
pattern EntitlementUnit_Terabytes = EntitlementUnit' "Terabytes"

pattern EntitlementUnit_Terabytes_Second :: EntitlementUnit
pattern EntitlementUnit_Terabytes_Second = EntitlementUnit' "Terabytes/Second"

{-# COMPLETE
  EntitlementUnit_Bits,
  EntitlementUnit_Bits_Second,
  EntitlementUnit_Bytes,
  EntitlementUnit_Bytes_Second,
  EntitlementUnit_Count,
  EntitlementUnit_Count_Second,
  EntitlementUnit_Gigabits,
  EntitlementUnit_Gigabits_Second,
  EntitlementUnit_Gigabytes,
  EntitlementUnit_Gigabytes_Second,
  EntitlementUnit_Kilobits,
  EntitlementUnit_Kilobits_Second,
  EntitlementUnit_Kilobytes,
  EntitlementUnit_Kilobytes_Second,
  EntitlementUnit_Megabits,
  EntitlementUnit_Megabits_Second,
  EntitlementUnit_Megabytes,
  EntitlementUnit_Megabytes_Second,
  EntitlementUnit_Microseconds,
  EntitlementUnit_Milliseconds,
  EntitlementUnit_None,
  EntitlementUnit_Percent,
  EntitlementUnit_Seconds,
  EntitlementUnit_Terabits,
  EntitlementUnit_Terabits_Second,
  EntitlementUnit_Terabytes,
  EntitlementUnit_Terabytes_Second,
  EntitlementUnit'
  #-}
