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
-- Module      : Amazonka.Lightsail.Types.MetricUnit
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Lightsail.Types.MetricUnit
  ( MetricUnit
      ( ..,
        MetricUnit_Bits,
        MetricUnit_Bits_Second,
        MetricUnit_Bytes,
        MetricUnit_Bytes_Second,
        MetricUnit_Count,
        MetricUnit_Count_Second,
        MetricUnit_Gigabits,
        MetricUnit_Gigabits_Second,
        MetricUnit_Gigabytes,
        MetricUnit_Gigabytes_Second,
        MetricUnit_Kilobits,
        MetricUnit_Kilobits_Second,
        MetricUnit_Kilobytes,
        MetricUnit_Kilobytes_Second,
        MetricUnit_Megabits,
        MetricUnit_Megabits_Second,
        MetricUnit_Megabytes,
        MetricUnit_Megabytes_Second,
        MetricUnit_Microseconds,
        MetricUnit_Milliseconds,
        MetricUnit_None,
        MetricUnit_Percent,
        MetricUnit_Seconds,
        MetricUnit_Terabits,
        MetricUnit_Terabits_Second,
        MetricUnit_Terabytes,
        MetricUnit_Terabytes_Second
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype MetricUnit = MetricUnit'
  { fromMetricUnit ::
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

pattern MetricUnit_Bits :: MetricUnit
pattern MetricUnit_Bits = MetricUnit' "Bits"

pattern MetricUnit_Bits_Second :: MetricUnit
pattern MetricUnit_Bits_Second = MetricUnit' "Bits/Second"

pattern MetricUnit_Bytes :: MetricUnit
pattern MetricUnit_Bytes = MetricUnit' "Bytes"

pattern MetricUnit_Bytes_Second :: MetricUnit
pattern MetricUnit_Bytes_Second = MetricUnit' "Bytes/Second"

pattern MetricUnit_Count :: MetricUnit
pattern MetricUnit_Count = MetricUnit' "Count"

pattern MetricUnit_Count_Second :: MetricUnit
pattern MetricUnit_Count_Second = MetricUnit' "Count/Second"

pattern MetricUnit_Gigabits :: MetricUnit
pattern MetricUnit_Gigabits = MetricUnit' "Gigabits"

pattern MetricUnit_Gigabits_Second :: MetricUnit
pattern MetricUnit_Gigabits_Second = MetricUnit' "Gigabits/Second"

pattern MetricUnit_Gigabytes :: MetricUnit
pattern MetricUnit_Gigabytes = MetricUnit' "Gigabytes"

pattern MetricUnit_Gigabytes_Second :: MetricUnit
pattern MetricUnit_Gigabytes_Second = MetricUnit' "Gigabytes/Second"

pattern MetricUnit_Kilobits :: MetricUnit
pattern MetricUnit_Kilobits = MetricUnit' "Kilobits"

pattern MetricUnit_Kilobits_Second :: MetricUnit
pattern MetricUnit_Kilobits_Second = MetricUnit' "Kilobits/Second"

pattern MetricUnit_Kilobytes :: MetricUnit
pattern MetricUnit_Kilobytes = MetricUnit' "Kilobytes"

pattern MetricUnit_Kilobytes_Second :: MetricUnit
pattern MetricUnit_Kilobytes_Second = MetricUnit' "Kilobytes/Second"

pattern MetricUnit_Megabits :: MetricUnit
pattern MetricUnit_Megabits = MetricUnit' "Megabits"

pattern MetricUnit_Megabits_Second :: MetricUnit
pattern MetricUnit_Megabits_Second = MetricUnit' "Megabits/Second"

pattern MetricUnit_Megabytes :: MetricUnit
pattern MetricUnit_Megabytes = MetricUnit' "Megabytes"

pattern MetricUnit_Megabytes_Second :: MetricUnit
pattern MetricUnit_Megabytes_Second = MetricUnit' "Megabytes/Second"

pattern MetricUnit_Microseconds :: MetricUnit
pattern MetricUnit_Microseconds = MetricUnit' "Microseconds"

pattern MetricUnit_Milliseconds :: MetricUnit
pattern MetricUnit_Milliseconds = MetricUnit' "Milliseconds"

pattern MetricUnit_None :: MetricUnit
pattern MetricUnit_None = MetricUnit' "None"

pattern MetricUnit_Percent :: MetricUnit
pattern MetricUnit_Percent = MetricUnit' "Percent"

pattern MetricUnit_Seconds :: MetricUnit
pattern MetricUnit_Seconds = MetricUnit' "Seconds"

pattern MetricUnit_Terabits :: MetricUnit
pattern MetricUnit_Terabits = MetricUnit' "Terabits"

pattern MetricUnit_Terabits_Second :: MetricUnit
pattern MetricUnit_Terabits_Second = MetricUnit' "Terabits/Second"

pattern MetricUnit_Terabytes :: MetricUnit
pattern MetricUnit_Terabytes = MetricUnit' "Terabytes"

pattern MetricUnit_Terabytes_Second :: MetricUnit
pattern MetricUnit_Terabytes_Second = MetricUnit' "Terabytes/Second"

{-# COMPLETE
  MetricUnit_Bits,
  MetricUnit_Bits_Second,
  MetricUnit_Bytes,
  MetricUnit_Bytes_Second,
  MetricUnit_Count,
  MetricUnit_Count_Second,
  MetricUnit_Gigabits,
  MetricUnit_Gigabits_Second,
  MetricUnit_Gigabytes,
  MetricUnit_Gigabytes_Second,
  MetricUnit_Kilobits,
  MetricUnit_Kilobits_Second,
  MetricUnit_Kilobytes,
  MetricUnit_Kilobytes_Second,
  MetricUnit_Megabits,
  MetricUnit_Megabits_Second,
  MetricUnit_Megabytes,
  MetricUnit_Megabytes_Second,
  MetricUnit_Microseconds,
  MetricUnit_Milliseconds,
  MetricUnit_None,
  MetricUnit_Percent,
  MetricUnit_Seconds,
  MetricUnit_Terabits,
  MetricUnit_Terabits_Second,
  MetricUnit_Terabytes,
  MetricUnit_Terabytes_Second,
  MetricUnit'
  #-}
