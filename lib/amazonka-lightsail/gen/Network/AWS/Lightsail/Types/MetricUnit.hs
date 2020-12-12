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
        Bits,
        BitsSecond,
        Bytes,
        BytesSecond,
        Count,
        CountSecond,
        Gigabits,
        GigabitsSecond,
        Gigabytes,
        GigabytesSecond,
        Kilobits,
        KilobitsSecond,
        Kilobytes,
        KilobytesSecond,
        Megabits,
        MegabitsSecond,
        Megabytes,
        MegabytesSecond,
        Microseconds,
        Milliseconds,
        None,
        Percent,
        Seconds,
        Terabits,
        TerabitsSecond,
        Terabytes,
        TerabytesSecond
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype MetricUnit = MetricUnit' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern Bits :: MetricUnit
pattern Bits = MetricUnit' "Bits"

pattern BitsSecond :: MetricUnit
pattern BitsSecond = MetricUnit' "Bits/Second"

pattern Bytes :: MetricUnit
pattern Bytes = MetricUnit' "Bytes"

pattern BytesSecond :: MetricUnit
pattern BytesSecond = MetricUnit' "Bytes/Second"

pattern Count :: MetricUnit
pattern Count = MetricUnit' "Count"

pattern CountSecond :: MetricUnit
pattern CountSecond = MetricUnit' "Count/Second"

pattern Gigabits :: MetricUnit
pattern Gigabits = MetricUnit' "Gigabits"

pattern GigabitsSecond :: MetricUnit
pattern GigabitsSecond = MetricUnit' "Gigabits/Second"

pattern Gigabytes :: MetricUnit
pattern Gigabytes = MetricUnit' "Gigabytes"

pattern GigabytesSecond :: MetricUnit
pattern GigabytesSecond = MetricUnit' "Gigabytes/Second"

pattern Kilobits :: MetricUnit
pattern Kilobits = MetricUnit' "Kilobits"

pattern KilobitsSecond :: MetricUnit
pattern KilobitsSecond = MetricUnit' "Kilobits/Second"

pattern Kilobytes :: MetricUnit
pattern Kilobytes = MetricUnit' "Kilobytes"

pattern KilobytesSecond :: MetricUnit
pattern KilobytesSecond = MetricUnit' "Kilobytes/Second"

pattern Megabits :: MetricUnit
pattern Megabits = MetricUnit' "Megabits"

pattern MegabitsSecond :: MetricUnit
pattern MegabitsSecond = MetricUnit' "Megabits/Second"

pattern Megabytes :: MetricUnit
pattern Megabytes = MetricUnit' "Megabytes"

pattern MegabytesSecond :: MetricUnit
pattern MegabytesSecond = MetricUnit' "Megabytes/Second"

pattern Microseconds :: MetricUnit
pattern Microseconds = MetricUnit' "Microseconds"

pattern Milliseconds :: MetricUnit
pattern Milliseconds = MetricUnit' "Milliseconds"

pattern None :: MetricUnit
pattern None = MetricUnit' "None"

pattern Percent :: MetricUnit
pattern Percent = MetricUnit' "Percent"

pattern Seconds :: MetricUnit
pattern Seconds = MetricUnit' "Seconds"

pattern Terabits :: MetricUnit
pattern Terabits = MetricUnit' "Terabits"

pattern TerabitsSecond :: MetricUnit
pattern TerabitsSecond = MetricUnit' "Terabits/Second"

pattern Terabytes :: MetricUnit
pattern Terabytes = MetricUnit' "Terabytes"

pattern TerabytesSecond :: MetricUnit
pattern TerabytesSecond = MetricUnit' "Terabytes/Second"

{-# COMPLETE
  Bits,
  BitsSecond,
  Bytes,
  BytesSecond,
  Count,
  CountSecond,
  Gigabits,
  GigabitsSecond,
  Gigabytes,
  GigabytesSecond,
  Kilobits,
  KilobitsSecond,
  Kilobytes,
  KilobytesSecond,
  Megabits,
  MegabitsSecond,
  Megabytes,
  MegabytesSecond,
  Microseconds,
  Milliseconds,
  None,
  Percent,
  Seconds,
  Terabits,
  TerabitsSecond,
  Terabytes,
  TerabytesSecond,
  MetricUnit'
  #-}
