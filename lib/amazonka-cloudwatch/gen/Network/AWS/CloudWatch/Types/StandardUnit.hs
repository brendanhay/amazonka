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
        Seconds,
        Microseconds,
        Milliseconds,
        Bytes,
        Kilobytes,
        Megabytes,
        Gigabytes,
        Terabytes,
        Bits,
        Kilobits,
        Megabits,
        Gigabits,
        Terabits,
        Percent,
        Count,
        BytesSecond,
        KilobytesSecond,
        MegabytesSecond,
        GigabytesSecond,
        TerabytesSecond,
        BitsSecond,
        KilobitsSecond,
        MegabitsSecond,
        GigabitsSecond,
        TerabitsSecond,
        CountSecond,
        None
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype StandardUnit = StandardUnit' Lude.Text
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

pattern Seconds :: StandardUnit
pattern Seconds = StandardUnit' "Seconds"

pattern Microseconds :: StandardUnit
pattern Microseconds = StandardUnit' "Microseconds"

pattern Milliseconds :: StandardUnit
pattern Milliseconds = StandardUnit' "Milliseconds"

pattern Bytes :: StandardUnit
pattern Bytes = StandardUnit' "Bytes"

pattern Kilobytes :: StandardUnit
pattern Kilobytes = StandardUnit' "Kilobytes"

pattern Megabytes :: StandardUnit
pattern Megabytes = StandardUnit' "Megabytes"

pattern Gigabytes :: StandardUnit
pattern Gigabytes = StandardUnit' "Gigabytes"

pattern Terabytes :: StandardUnit
pattern Terabytes = StandardUnit' "Terabytes"

pattern Bits :: StandardUnit
pattern Bits = StandardUnit' "Bits"

pattern Kilobits :: StandardUnit
pattern Kilobits = StandardUnit' "Kilobits"

pattern Megabits :: StandardUnit
pattern Megabits = StandardUnit' "Megabits"

pattern Gigabits :: StandardUnit
pattern Gigabits = StandardUnit' "Gigabits"

pattern Terabits :: StandardUnit
pattern Terabits = StandardUnit' "Terabits"

pattern Percent :: StandardUnit
pattern Percent = StandardUnit' "Percent"

pattern Count :: StandardUnit
pattern Count = StandardUnit' "Count"

pattern BytesSecond :: StandardUnit
pattern BytesSecond = StandardUnit' "Bytes/Second"

pattern KilobytesSecond :: StandardUnit
pattern KilobytesSecond = StandardUnit' "Kilobytes/Second"

pattern MegabytesSecond :: StandardUnit
pattern MegabytesSecond = StandardUnit' "Megabytes/Second"

pattern GigabytesSecond :: StandardUnit
pattern GigabytesSecond = StandardUnit' "Gigabytes/Second"

pattern TerabytesSecond :: StandardUnit
pattern TerabytesSecond = StandardUnit' "Terabytes/Second"

pattern BitsSecond :: StandardUnit
pattern BitsSecond = StandardUnit' "Bits/Second"

pattern KilobitsSecond :: StandardUnit
pattern KilobitsSecond = StandardUnit' "Kilobits/Second"

pattern MegabitsSecond :: StandardUnit
pattern MegabitsSecond = StandardUnit' "Megabits/Second"

pattern GigabitsSecond :: StandardUnit
pattern GigabitsSecond = StandardUnit' "Gigabits/Second"

pattern TerabitsSecond :: StandardUnit
pattern TerabitsSecond = StandardUnit' "Terabits/Second"

pattern CountSecond :: StandardUnit
pattern CountSecond = StandardUnit' "Count/Second"

pattern None :: StandardUnit
pattern None = StandardUnit' "None"

{-# COMPLETE
  Seconds,
  Microseconds,
  Milliseconds,
  Bytes,
  Kilobytes,
  Megabytes,
  Gigabytes,
  Terabytes,
  Bits,
  Kilobits,
  Megabits,
  Gigabits,
  Terabits,
  Percent,
  Count,
  BytesSecond,
  KilobytesSecond,
  MegabytesSecond,
  GigabytesSecond,
  TerabytesSecond,
  BitsSecond,
  KilobitsSecond,
  MegabitsSecond,
  GigabitsSecond,
  TerabitsSecond,
  CountSecond,
  None,
  StandardUnit'
  #-}
