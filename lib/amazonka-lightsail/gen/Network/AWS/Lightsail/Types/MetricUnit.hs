{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.MetricUnit
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.MetricUnit
  ( MetricUnit
      ( MetricUnit',
        MetricUnitSeconds,
        MetricUnitMicroseconds,
        MetricUnitMilliseconds,
        MetricUnitBytes,
        MetricUnitKilobytes,
        MetricUnitMegabytes,
        MetricUnitGigabytes,
        MetricUnitTerabytes,
        MetricUnitBits,
        MetricUnitKilobits,
        MetricUnitMegabits,
        MetricUnitGigabits,
        MetricUnitTerabits,
        MetricUnitPercent,
        MetricUnitCount,
        MetricUnitBytesSecond,
        MetricUnitKilobytesSecond,
        MetricUnitMegabytesSecond,
        MetricUnitGigabytesSecond,
        MetricUnitTerabytesSecond,
        MetricUnitBitsSecond,
        MetricUnitKilobitsSecond,
        MetricUnitMegabitsSecond,
        MetricUnitGigabitsSecond,
        MetricUnitTerabitsSecond,
        MetricUnitCountSecond,
        MetricUnitNone,
        fromMetricUnit
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype MetricUnit = MetricUnit' {fromMetricUnit :: Core.Text}
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

pattern MetricUnitSeconds :: MetricUnit
pattern MetricUnitSeconds = MetricUnit' "Seconds"

pattern MetricUnitMicroseconds :: MetricUnit
pattern MetricUnitMicroseconds = MetricUnit' "Microseconds"

pattern MetricUnitMilliseconds :: MetricUnit
pattern MetricUnitMilliseconds = MetricUnit' "Milliseconds"

pattern MetricUnitBytes :: MetricUnit
pattern MetricUnitBytes = MetricUnit' "Bytes"

pattern MetricUnitKilobytes :: MetricUnit
pattern MetricUnitKilobytes = MetricUnit' "Kilobytes"

pattern MetricUnitMegabytes :: MetricUnit
pattern MetricUnitMegabytes = MetricUnit' "Megabytes"

pattern MetricUnitGigabytes :: MetricUnit
pattern MetricUnitGigabytes = MetricUnit' "Gigabytes"

pattern MetricUnitTerabytes :: MetricUnit
pattern MetricUnitTerabytes = MetricUnit' "Terabytes"

pattern MetricUnitBits :: MetricUnit
pattern MetricUnitBits = MetricUnit' "Bits"

pattern MetricUnitKilobits :: MetricUnit
pattern MetricUnitKilobits = MetricUnit' "Kilobits"

pattern MetricUnitMegabits :: MetricUnit
pattern MetricUnitMegabits = MetricUnit' "Megabits"

pattern MetricUnitGigabits :: MetricUnit
pattern MetricUnitGigabits = MetricUnit' "Gigabits"

pattern MetricUnitTerabits :: MetricUnit
pattern MetricUnitTerabits = MetricUnit' "Terabits"

pattern MetricUnitPercent :: MetricUnit
pattern MetricUnitPercent = MetricUnit' "Percent"

pattern MetricUnitCount :: MetricUnit
pattern MetricUnitCount = MetricUnit' "Count"

pattern MetricUnitBytesSecond :: MetricUnit
pattern MetricUnitBytesSecond = MetricUnit' "Bytes/Second"

pattern MetricUnitKilobytesSecond :: MetricUnit
pattern MetricUnitKilobytesSecond = MetricUnit' "Kilobytes/Second"

pattern MetricUnitMegabytesSecond :: MetricUnit
pattern MetricUnitMegabytesSecond = MetricUnit' "Megabytes/Second"

pattern MetricUnitGigabytesSecond :: MetricUnit
pattern MetricUnitGigabytesSecond = MetricUnit' "Gigabytes/Second"

pattern MetricUnitTerabytesSecond :: MetricUnit
pattern MetricUnitTerabytesSecond = MetricUnit' "Terabytes/Second"

pattern MetricUnitBitsSecond :: MetricUnit
pattern MetricUnitBitsSecond = MetricUnit' "Bits/Second"

pattern MetricUnitKilobitsSecond :: MetricUnit
pattern MetricUnitKilobitsSecond = MetricUnit' "Kilobits/Second"

pattern MetricUnitMegabitsSecond :: MetricUnit
pattern MetricUnitMegabitsSecond = MetricUnit' "Megabits/Second"

pattern MetricUnitGigabitsSecond :: MetricUnit
pattern MetricUnitGigabitsSecond = MetricUnit' "Gigabits/Second"

pattern MetricUnitTerabitsSecond :: MetricUnit
pattern MetricUnitTerabitsSecond = MetricUnit' "Terabits/Second"

pattern MetricUnitCountSecond :: MetricUnit
pattern MetricUnitCountSecond = MetricUnit' "Count/Second"

pattern MetricUnitNone :: MetricUnit
pattern MetricUnitNone = MetricUnit' "None"

{-# COMPLETE
  MetricUnitSeconds,
  MetricUnitMicroseconds,
  MetricUnitMilliseconds,
  MetricUnitBytes,
  MetricUnitKilobytes,
  MetricUnitMegabytes,
  MetricUnitGigabytes,
  MetricUnitTerabytes,
  MetricUnitBits,
  MetricUnitKilobits,
  MetricUnitMegabits,
  MetricUnitGigabits,
  MetricUnitTerabits,
  MetricUnitPercent,
  MetricUnitCount,
  MetricUnitBytesSecond,
  MetricUnitKilobytesSecond,
  MetricUnitMegabytesSecond,
  MetricUnitGigabytesSecond,
  MetricUnitTerabytesSecond,
  MetricUnitBitsSecond,
  MetricUnitKilobitsSecond,
  MetricUnitMegabitsSecond,
  MetricUnitGigabitsSecond,
  MetricUnitTerabitsSecond,
  MetricUnitCountSecond,
  MetricUnitNone,
  MetricUnit'
  #-}
