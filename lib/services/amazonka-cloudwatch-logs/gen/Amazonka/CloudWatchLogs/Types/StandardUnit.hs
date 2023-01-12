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
-- Module      : Amazonka.CloudWatchLogs.Types.StandardUnit
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudWatchLogs.Types.StandardUnit
  ( StandardUnit
      ( ..,
        StandardUnit_Bits,
        StandardUnit_Bits_Second,
        StandardUnit_Bytes,
        StandardUnit_Bytes_Second,
        StandardUnit_Count,
        StandardUnit_Count_Second,
        StandardUnit_Gigabits,
        StandardUnit_Gigabits_Second,
        StandardUnit_Gigabytes,
        StandardUnit_Gigabytes_Second,
        StandardUnit_Kilobits,
        StandardUnit_Kilobits_Second,
        StandardUnit_Kilobytes,
        StandardUnit_Kilobytes_Second,
        StandardUnit_Megabits,
        StandardUnit_Megabits_Second,
        StandardUnit_Megabytes,
        StandardUnit_Megabytes_Second,
        StandardUnit_Microseconds,
        StandardUnit_Milliseconds,
        StandardUnit_None,
        StandardUnit_Percent,
        StandardUnit_Seconds,
        StandardUnit_Terabits,
        StandardUnit_Terabits_Second,
        StandardUnit_Terabytes,
        StandardUnit_Terabytes_Second
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype StandardUnit = StandardUnit'
  { fromStandardUnit ::
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

pattern StandardUnit_Bits :: StandardUnit
pattern StandardUnit_Bits = StandardUnit' "Bits"

pattern StandardUnit_Bits_Second :: StandardUnit
pattern StandardUnit_Bits_Second = StandardUnit' "Bits/Second"

pattern StandardUnit_Bytes :: StandardUnit
pattern StandardUnit_Bytes = StandardUnit' "Bytes"

pattern StandardUnit_Bytes_Second :: StandardUnit
pattern StandardUnit_Bytes_Second = StandardUnit' "Bytes/Second"

pattern StandardUnit_Count :: StandardUnit
pattern StandardUnit_Count = StandardUnit' "Count"

pattern StandardUnit_Count_Second :: StandardUnit
pattern StandardUnit_Count_Second = StandardUnit' "Count/Second"

pattern StandardUnit_Gigabits :: StandardUnit
pattern StandardUnit_Gigabits = StandardUnit' "Gigabits"

pattern StandardUnit_Gigabits_Second :: StandardUnit
pattern StandardUnit_Gigabits_Second = StandardUnit' "Gigabits/Second"

pattern StandardUnit_Gigabytes :: StandardUnit
pattern StandardUnit_Gigabytes = StandardUnit' "Gigabytes"

pattern StandardUnit_Gigabytes_Second :: StandardUnit
pattern StandardUnit_Gigabytes_Second = StandardUnit' "Gigabytes/Second"

pattern StandardUnit_Kilobits :: StandardUnit
pattern StandardUnit_Kilobits = StandardUnit' "Kilobits"

pattern StandardUnit_Kilobits_Second :: StandardUnit
pattern StandardUnit_Kilobits_Second = StandardUnit' "Kilobits/Second"

pattern StandardUnit_Kilobytes :: StandardUnit
pattern StandardUnit_Kilobytes = StandardUnit' "Kilobytes"

pattern StandardUnit_Kilobytes_Second :: StandardUnit
pattern StandardUnit_Kilobytes_Second = StandardUnit' "Kilobytes/Second"

pattern StandardUnit_Megabits :: StandardUnit
pattern StandardUnit_Megabits = StandardUnit' "Megabits"

pattern StandardUnit_Megabits_Second :: StandardUnit
pattern StandardUnit_Megabits_Second = StandardUnit' "Megabits/Second"

pattern StandardUnit_Megabytes :: StandardUnit
pattern StandardUnit_Megabytes = StandardUnit' "Megabytes"

pattern StandardUnit_Megabytes_Second :: StandardUnit
pattern StandardUnit_Megabytes_Second = StandardUnit' "Megabytes/Second"

pattern StandardUnit_Microseconds :: StandardUnit
pattern StandardUnit_Microseconds = StandardUnit' "Microseconds"

pattern StandardUnit_Milliseconds :: StandardUnit
pattern StandardUnit_Milliseconds = StandardUnit' "Milliseconds"

pattern StandardUnit_None :: StandardUnit
pattern StandardUnit_None = StandardUnit' "None"

pattern StandardUnit_Percent :: StandardUnit
pattern StandardUnit_Percent = StandardUnit' "Percent"

pattern StandardUnit_Seconds :: StandardUnit
pattern StandardUnit_Seconds = StandardUnit' "Seconds"

pattern StandardUnit_Terabits :: StandardUnit
pattern StandardUnit_Terabits = StandardUnit' "Terabits"

pattern StandardUnit_Terabits_Second :: StandardUnit
pattern StandardUnit_Terabits_Second = StandardUnit' "Terabits/Second"

pattern StandardUnit_Terabytes :: StandardUnit
pattern StandardUnit_Terabytes = StandardUnit' "Terabytes"

pattern StandardUnit_Terabytes_Second :: StandardUnit
pattern StandardUnit_Terabytes_Second = StandardUnit' "Terabytes/Second"

{-# COMPLETE
  StandardUnit_Bits,
  StandardUnit_Bits_Second,
  StandardUnit_Bytes,
  StandardUnit_Bytes_Second,
  StandardUnit_Count,
  StandardUnit_Count_Second,
  StandardUnit_Gigabits,
  StandardUnit_Gigabits_Second,
  StandardUnit_Gigabytes,
  StandardUnit_Gigabytes_Second,
  StandardUnit_Kilobits,
  StandardUnit_Kilobits_Second,
  StandardUnit_Kilobytes,
  StandardUnit_Kilobytes_Second,
  StandardUnit_Megabits,
  StandardUnit_Megabits_Second,
  StandardUnit_Megabytes,
  StandardUnit_Megabytes_Second,
  StandardUnit_Microseconds,
  StandardUnit_Milliseconds,
  StandardUnit_None,
  StandardUnit_Percent,
  StandardUnit_Seconds,
  StandardUnit_Terabits,
  StandardUnit_Terabits_Second,
  StandardUnit_Terabytes,
  StandardUnit_Terabytes_Second,
  StandardUnit'
  #-}
