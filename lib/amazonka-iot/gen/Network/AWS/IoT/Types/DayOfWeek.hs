{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.DayOfWeek
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.DayOfWeek
  ( DayOfWeek
      ( DayOfWeek',
        DayOfWeekSun,
        DayOfWeekMon,
        DayOfWeekTue,
        DayOfWeekWed,
        DayOfWeekThu,
        DayOfWeekFri,
        DayOfWeekSat,
        fromDayOfWeek
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype DayOfWeek = DayOfWeek' {fromDayOfWeek :: Core.Text}
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

pattern DayOfWeekSun :: DayOfWeek
pattern DayOfWeekSun = DayOfWeek' "SUN"

pattern DayOfWeekMon :: DayOfWeek
pattern DayOfWeekMon = DayOfWeek' "MON"

pattern DayOfWeekTue :: DayOfWeek
pattern DayOfWeekTue = DayOfWeek' "TUE"

pattern DayOfWeekWed :: DayOfWeek
pattern DayOfWeekWed = DayOfWeek' "WED"

pattern DayOfWeekThu :: DayOfWeek
pattern DayOfWeekThu = DayOfWeek' "THU"

pattern DayOfWeekFri :: DayOfWeek
pattern DayOfWeekFri = DayOfWeek' "FRI"

pattern DayOfWeekSat :: DayOfWeek
pattern DayOfWeekSat = DayOfWeek' "SAT"

{-# COMPLETE
  DayOfWeekSun,
  DayOfWeekMon,
  DayOfWeekTue,
  DayOfWeekWed,
  DayOfWeekThu,
  DayOfWeekFri,
  DayOfWeekSat,
  DayOfWeek'
  #-}
