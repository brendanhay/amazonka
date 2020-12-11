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
        Fri,
        Mon,
        Sat,
        Sun,
        Thu,
        Tue,
        Wed
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype DayOfWeek = DayOfWeek' Lude.Text
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

pattern Fri :: DayOfWeek
pattern Fri = DayOfWeek' "FRI"

pattern Mon :: DayOfWeek
pattern Mon = DayOfWeek' "MON"

pattern Sat :: DayOfWeek
pattern Sat = DayOfWeek' "SAT"

pattern Sun :: DayOfWeek
pattern Sun = DayOfWeek' "SUN"

pattern Thu :: DayOfWeek
pattern Thu = DayOfWeek' "THU"

pattern Tue :: DayOfWeek
pattern Tue = DayOfWeek' "TUE"

pattern Wed :: DayOfWeek
pattern Wed = DayOfWeek' "WED"

{-# COMPLETE
  Fri,
  Mon,
  Sat,
  Sun,
  Thu,
  Tue,
  Wed,
  DayOfWeek'
  #-}
