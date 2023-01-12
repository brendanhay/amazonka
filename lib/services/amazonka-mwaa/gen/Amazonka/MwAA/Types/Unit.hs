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
-- Module      : Amazonka.MwAA.Types.Unit
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MwAA.Types.Unit
  ( Unit
      ( ..,
        Unit_Bits,
        Unit_Bits_Second,
        Unit_Bytes,
        Unit_Bytes_Second,
        Unit_Count,
        Unit_Count_Second,
        Unit_Gigabits,
        Unit_Gigabits_Second,
        Unit_Gigabytes,
        Unit_Gigabytes_Second,
        Unit_Kilobits,
        Unit_Kilobits_Second,
        Unit_Kilobytes,
        Unit_Kilobytes_Second,
        Unit_Megabits,
        Unit_Megabits_Second,
        Unit_Megabytes,
        Unit_Megabytes_Second,
        Unit_Microseconds,
        Unit_Milliseconds,
        Unit_None,
        Unit_Percent,
        Unit_Seconds,
        Unit_Terabits,
        Unit_Terabits_Second,
        Unit_Terabytes,
        Unit_Terabytes_Second
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype Unit = Unit' {fromUnit :: Data.Text}
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

pattern Unit_Bits :: Unit
pattern Unit_Bits = Unit' "Bits"

pattern Unit_Bits_Second :: Unit
pattern Unit_Bits_Second = Unit' "Bits/Second"

pattern Unit_Bytes :: Unit
pattern Unit_Bytes = Unit' "Bytes"

pattern Unit_Bytes_Second :: Unit
pattern Unit_Bytes_Second = Unit' "Bytes/Second"

pattern Unit_Count :: Unit
pattern Unit_Count = Unit' "Count"

pattern Unit_Count_Second :: Unit
pattern Unit_Count_Second = Unit' "Count/Second"

pattern Unit_Gigabits :: Unit
pattern Unit_Gigabits = Unit' "Gigabits"

pattern Unit_Gigabits_Second :: Unit
pattern Unit_Gigabits_Second = Unit' "Gigabits/Second"

pattern Unit_Gigabytes :: Unit
pattern Unit_Gigabytes = Unit' "Gigabytes"

pattern Unit_Gigabytes_Second :: Unit
pattern Unit_Gigabytes_Second = Unit' "Gigabytes/Second"

pattern Unit_Kilobits :: Unit
pattern Unit_Kilobits = Unit' "Kilobits"

pattern Unit_Kilobits_Second :: Unit
pattern Unit_Kilobits_Second = Unit' "Kilobits/Second"

pattern Unit_Kilobytes :: Unit
pattern Unit_Kilobytes = Unit' "Kilobytes"

pattern Unit_Kilobytes_Second :: Unit
pattern Unit_Kilobytes_Second = Unit' "Kilobytes/Second"

pattern Unit_Megabits :: Unit
pattern Unit_Megabits = Unit' "Megabits"

pattern Unit_Megabits_Second :: Unit
pattern Unit_Megabits_Second = Unit' "Megabits/Second"

pattern Unit_Megabytes :: Unit
pattern Unit_Megabytes = Unit' "Megabytes"

pattern Unit_Megabytes_Second :: Unit
pattern Unit_Megabytes_Second = Unit' "Megabytes/Second"

pattern Unit_Microseconds :: Unit
pattern Unit_Microseconds = Unit' "Microseconds"

pattern Unit_Milliseconds :: Unit
pattern Unit_Milliseconds = Unit' "Milliseconds"

pattern Unit_None :: Unit
pattern Unit_None = Unit' "None"

pattern Unit_Percent :: Unit
pattern Unit_Percent = Unit' "Percent"

pattern Unit_Seconds :: Unit
pattern Unit_Seconds = Unit' "Seconds"

pattern Unit_Terabits :: Unit
pattern Unit_Terabits = Unit' "Terabits"

pattern Unit_Terabits_Second :: Unit
pattern Unit_Terabits_Second = Unit' "Terabits/Second"

pattern Unit_Terabytes :: Unit
pattern Unit_Terabytes = Unit' "Terabytes"

pattern Unit_Terabytes_Second :: Unit
pattern Unit_Terabytes_Second = Unit' "Terabytes/Second"

{-# COMPLETE
  Unit_Bits,
  Unit_Bits_Second,
  Unit_Bytes,
  Unit_Bytes_Second,
  Unit_Count,
  Unit_Count_Second,
  Unit_Gigabits,
  Unit_Gigabits_Second,
  Unit_Gigabytes,
  Unit_Gigabytes_Second,
  Unit_Kilobits,
  Unit_Kilobits_Second,
  Unit_Kilobytes,
  Unit_Kilobytes_Second,
  Unit_Megabits,
  Unit_Megabits_Second,
  Unit_Megabytes,
  Unit_Megabytes_Second,
  Unit_Microseconds,
  Unit_Milliseconds,
  Unit_None,
  Unit_Percent,
  Unit_Seconds,
  Unit_Terabits,
  Unit_Terabits_Second,
  Unit_Terabytes,
  Unit_Terabytes_Second,
  Unit'
  #-}
