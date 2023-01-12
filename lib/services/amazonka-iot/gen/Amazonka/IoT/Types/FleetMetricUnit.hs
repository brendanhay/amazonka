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
-- Module      : Amazonka.IoT.Types.FleetMetricUnit
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT.Types.FleetMetricUnit
  ( FleetMetricUnit
      ( ..,
        FleetMetricUnit_Bits,
        FleetMetricUnit_Bits_Second,
        FleetMetricUnit_Bytes,
        FleetMetricUnit_Bytes_Second,
        FleetMetricUnit_Count,
        FleetMetricUnit_Count_Second,
        FleetMetricUnit_Gigabits,
        FleetMetricUnit_Gigabits_Second,
        FleetMetricUnit_Gigabytes,
        FleetMetricUnit_Gigabytes_Second,
        FleetMetricUnit_Kilobits,
        FleetMetricUnit_Kilobits_Second,
        FleetMetricUnit_Kilobytes,
        FleetMetricUnit_Kilobytes_Second,
        FleetMetricUnit_Megabits,
        FleetMetricUnit_Megabits_Second,
        FleetMetricUnit_Megabytes,
        FleetMetricUnit_Megabytes_Second,
        FleetMetricUnit_Microseconds,
        FleetMetricUnit_Milliseconds,
        FleetMetricUnit_None,
        FleetMetricUnit_Percent,
        FleetMetricUnit_Seconds,
        FleetMetricUnit_Terabits,
        FleetMetricUnit_Terabits_Second,
        FleetMetricUnit_Terabytes,
        FleetMetricUnit_Terabytes_Second
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype FleetMetricUnit = FleetMetricUnit'
  { fromFleetMetricUnit ::
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

pattern FleetMetricUnit_Bits :: FleetMetricUnit
pattern FleetMetricUnit_Bits = FleetMetricUnit' "Bits"

pattern FleetMetricUnit_Bits_Second :: FleetMetricUnit
pattern FleetMetricUnit_Bits_Second = FleetMetricUnit' "Bits/Second"

pattern FleetMetricUnit_Bytes :: FleetMetricUnit
pattern FleetMetricUnit_Bytes = FleetMetricUnit' "Bytes"

pattern FleetMetricUnit_Bytes_Second :: FleetMetricUnit
pattern FleetMetricUnit_Bytes_Second = FleetMetricUnit' "Bytes/Second"

pattern FleetMetricUnit_Count :: FleetMetricUnit
pattern FleetMetricUnit_Count = FleetMetricUnit' "Count"

pattern FleetMetricUnit_Count_Second :: FleetMetricUnit
pattern FleetMetricUnit_Count_Second = FleetMetricUnit' "Count/Second"

pattern FleetMetricUnit_Gigabits :: FleetMetricUnit
pattern FleetMetricUnit_Gigabits = FleetMetricUnit' "Gigabits"

pattern FleetMetricUnit_Gigabits_Second :: FleetMetricUnit
pattern FleetMetricUnit_Gigabits_Second = FleetMetricUnit' "Gigabits/Second"

pattern FleetMetricUnit_Gigabytes :: FleetMetricUnit
pattern FleetMetricUnit_Gigabytes = FleetMetricUnit' "Gigabytes"

pattern FleetMetricUnit_Gigabytes_Second :: FleetMetricUnit
pattern FleetMetricUnit_Gigabytes_Second = FleetMetricUnit' "Gigabytes/Second"

pattern FleetMetricUnit_Kilobits :: FleetMetricUnit
pattern FleetMetricUnit_Kilobits = FleetMetricUnit' "Kilobits"

pattern FleetMetricUnit_Kilobits_Second :: FleetMetricUnit
pattern FleetMetricUnit_Kilobits_Second = FleetMetricUnit' "Kilobits/Second"

pattern FleetMetricUnit_Kilobytes :: FleetMetricUnit
pattern FleetMetricUnit_Kilobytes = FleetMetricUnit' "Kilobytes"

pattern FleetMetricUnit_Kilobytes_Second :: FleetMetricUnit
pattern FleetMetricUnit_Kilobytes_Second = FleetMetricUnit' "Kilobytes/Second"

pattern FleetMetricUnit_Megabits :: FleetMetricUnit
pattern FleetMetricUnit_Megabits = FleetMetricUnit' "Megabits"

pattern FleetMetricUnit_Megabits_Second :: FleetMetricUnit
pattern FleetMetricUnit_Megabits_Second = FleetMetricUnit' "Megabits/Second"

pattern FleetMetricUnit_Megabytes :: FleetMetricUnit
pattern FleetMetricUnit_Megabytes = FleetMetricUnit' "Megabytes"

pattern FleetMetricUnit_Megabytes_Second :: FleetMetricUnit
pattern FleetMetricUnit_Megabytes_Second = FleetMetricUnit' "Megabytes/Second"

pattern FleetMetricUnit_Microseconds :: FleetMetricUnit
pattern FleetMetricUnit_Microseconds = FleetMetricUnit' "Microseconds"

pattern FleetMetricUnit_Milliseconds :: FleetMetricUnit
pattern FleetMetricUnit_Milliseconds = FleetMetricUnit' "Milliseconds"

pattern FleetMetricUnit_None :: FleetMetricUnit
pattern FleetMetricUnit_None = FleetMetricUnit' "None"

pattern FleetMetricUnit_Percent :: FleetMetricUnit
pattern FleetMetricUnit_Percent = FleetMetricUnit' "Percent"

pattern FleetMetricUnit_Seconds :: FleetMetricUnit
pattern FleetMetricUnit_Seconds = FleetMetricUnit' "Seconds"

pattern FleetMetricUnit_Terabits :: FleetMetricUnit
pattern FleetMetricUnit_Terabits = FleetMetricUnit' "Terabits"

pattern FleetMetricUnit_Terabits_Second :: FleetMetricUnit
pattern FleetMetricUnit_Terabits_Second = FleetMetricUnit' "Terabits/Second"

pattern FleetMetricUnit_Terabytes :: FleetMetricUnit
pattern FleetMetricUnit_Terabytes = FleetMetricUnit' "Terabytes"

pattern FleetMetricUnit_Terabytes_Second :: FleetMetricUnit
pattern FleetMetricUnit_Terabytes_Second = FleetMetricUnit' "Terabytes/Second"

{-# COMPLETE
  FleetMetricUnit_Bits,
  FleetMetricUnit_Bits_Second,
  FleetMetricUnit_Bytes,
  FleetMetricUnit_Bytes_Second,
  FleetMetricUnit_Count,
  FleetMetricUnit_Count_Second,
  FleetMetricUnit_Gigabits,
  FleetMetricUnit_Gigabits_Second,
  FleetMetricUnit_Gigabytes,
  FleetMetricUnit_Gigabytes_Second,
  FleetMetricUnit_Kilobits,
  FleetMetricUnit_Kilobits_Second,
  FleetMetricUnit_Kilobytes,
  FleetMetricUnit_Kilobytes_Second,
  FleetMetricUnit_Megabits,
  FleetMetricUnit_Megabits_Second,
  FleetMetricUnit_Megabytes,
  FleetMetricUnit_Megabytes_Second,
  FleetMetricUnit_Microseconds,
  FleetMetricUnit_Milliseconds,
  FleetMetricUnit_None,
  FleetMetricUnit_Percent,
  FleetMetricUnit_Seconds,
  FleetMetricUnit_Terabits,
  FleetMetricUnit_Terabits_Second,
  FleetMetricUnit_Terabytes,
  FleetMetricUnit_Terabytes_Second,
  FleetMetricUnit'
  #-}
