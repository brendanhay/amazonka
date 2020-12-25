{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatch.Types.StandardUnit
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatch.Types.StandardUnit
  ( StandardUnit
      ( StandardUnit',
        StandardUnitSeconds,
        StandardUnitMicroseconds,
        StandardUnitMilliseconds,
        StandardUnitBytes,
        StandardUnitKilobytes,
        StandardUnitMegabytes,
        StandardUnitGigabytes,
        StandardUnitTerabytes,
        StandardUnitBits,
        StandardUnitKilobits,
        StandardUnitMegabits,
        StandardUnitGigabits,
        StandardUnitTerabits,
        StandardUnitPercent,
        StandardUnitCount,
        StandardUnitBytesSecond,
        StandardUnitKilobytesSecond,
        StandardUnitMegabytesSecond,
        StandardUnitGigabytesSecond,
        StandardUnitTerabytesSecond,
        StandardUnitBitsSecond,
        StandardUnitKilobitsSecond,
        StandardUnitMegabitsSecond,
        StandardUnitGigabitsSecond,
        StandardUnitTerabitsSecond,
        StandardUnitCountSecond,
        StandardUnitNone,
        fromStandardUnit
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype StandardUnit = StandardUnit' {fromStandardUnit :: Core.Text}
  deriving stock
    ( Core.Eq,
      Core.Ord,
      Core.Read,
      Core.Show,
      Core.Generic
    )
  deriving newtype
    ( Core.IsString,
      Core.Hashable,
      Core.NFData,
      Core.ToJSONKey,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.FromJSON,
      Core.ToXML,
      Core.FromXML,
      Core.ToText,
      Core.FromText,
      Core.ToByteString,
      Core.ToQuery,
      Core.ToHeader
    )

pattern StandardUnitSeconds :: StandardUnit
pattern StandardUnitSeconds = StandardUnit' "Seconds"

pattern StandardUnitMicroseconds :: StandardUnit
pattern StandardUnitMicroseconds = StandardUnit' "Microseconds"

pattern StandardUnitMilliseconds :: StandardUnit
pattern StandardUnitMilliseconds = StandardUnit' "Milliseconds"

pattern StandardUnitBytes :: StandardUnit
pattern StandardUnitBytes = StandardUnit' "Bytes"

pattern StandardUnitKilobytes :: StandardUnit
pattern StandardUnitKilobytes = StandardUnit' "Kilobytes"

pattern StandardUnitMegabytes :: StandardUnit
pattern StandardUnitMegabytes = StandardUnit' "Megabytes"

pattern StandardUnitGigabytes :: StandardUnit
pattern StandardUnitGigabytes = StandardUnit' "Gigabytes"

pattern StandardUnitTerabytes :: StandardUnit
pattern StandardUnitTerabytes = StandardUnit' "Terabytes"

pattern StandardUnitBits :: StandardUnit
pattern StandardUnitBits = StandardUnit' "Bits"

pattern StandardUnitKilobits :: StandardUnit
pattern StandardUnitKilobits = StandardUnit' "Kilobits"

pattern StandardUnitMegabits :: StandardUnit
pattern StandardUnitMegabits = StandardUnit' "Megabits"

pattern StandardUnitGigabits :: StandardUnit
pattern StandardUnitGigabits = StandardUnit' "Gigabits"

pattern StandardUnitTerabits :: StandardUnit
pattern StandardUnitTerabits = StandardUnit' "Terabits"

pattern StandardUnitPercent :: StandardUnit
pattern StandardUnitPercent = StandardUnit' "Percent"

pattern StandardUnitCount :: StandardUnit
pattern StandardUnitCount = StandardUnit' "Count"

pattern StandardUnitBytesSecond :: StandardUnit
pattern StandardUnitBytesSecond = StandardUnit' "Bytes/Second"

pattern StandardUnitKilobytesSecond :: StandardUnit
pattern StandardUnitKilobytesSecond = StandardUnit' "Kilobytes/Second"

pattern StandardUnitMegabytesSecond :: StandardUnit
pattern StandardUnitMegabytesSecond = StandardUnit' "Megabytes/Second"

pattern StandardUnitGigabytesSecond :: StandardUnit
pattern StandardUnitGigabytesSecond = StandardUnit' "Gigabytes/Second"

pattern StandardUnitTerabytesSecond :: StandardUnit
pattern StandardUnitTerabytesSecond = StandardUnit' "Terabytes/Second"

pattern StandardUnitBitsSecond :: StandardUnit
pattern StandardUnitBitsSecond = StandardUnit' "Bits/Second"

pattern StandardUnitKilobitsSecond :: StandardUnit
pattern StandardUnitKilobitsSecond = StandardUnit' "Kilobits/Second"

pattern StandardUnitMegabitsSecond :: StandardUnit
pattern StandardUnitMegabitsSecond = StandardUnit' "Megabits/Second"

pattern StandardUnitGigabitsSecond :: StandardUnit
pattern StandardUnitGigabitsSecond = StandardUnit' "Gigabits/Second"

pattern StandardUnitTerabitsSecond :: StandardUnit
pattern StandardUnitTerabitsSecond = StandardUnit' "Terabits/Second"

pattern StandardUnitCountSecond :: StandardUnit
pattern StandardUnitCountSecond = StandardUnit' "Count/Second"

pattern StandardUnitNone :: StandardUnit
pattern StandardUnitNone = StandardUnit' "None"

{-# COMPLETE
  StandardUnitSeconds,
  StandardUnitMicroseconds,
  StandardUnitMilliseconds,
  StandardUnitBytes,
  StandardUnitKilobytes,
  StandardUnitMegabytes,
  StandardUnitGigabytes,
  StandardUnitTerabytes,
  StandardUnitBits,
  StandardUnitKilobits,
  StandardUnitMegabits,
  StandardUnitGigabits,
  StandardUnitTerabits,
  StandardUnitPercent,
  StandardUnitCount,
  StandardUnitBytesSecond,
  StandardUnitKilobytesSecond,
  StandardUnitMegabytesSecond,
  StandardUnitGigabytesSecond,
  StandardUnitTerabytesSecond,
  StandardUnitBitsSecond,
  StandardUnitKilobitsSecond,
  StandardUnitMegabitsSecond,
  StandardUnitGigabitsSecond,
  StandardUnitTerabitsSecond,
  StandardUnitCountSecond,
  StandardUnitNone,
  StandardUnit'
  #-}
