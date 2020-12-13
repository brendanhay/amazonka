{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MQ.Types.DayOfWeek
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MQ.Types.DayOfWeek
  ( DayOfWeek
      ( DayOfWeek',
        Monday,
        Tuesday,
        Wednesday,
        Thursday,
        Friday,
        Saturday,
        Sunday
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

pattern Monday :: DayOfWeek
pattern Monday = DayOfWeek' "MONDAY"

pattern Tuesday :: DayOfWeek
pattern Tuesday = DayOfWeek' "TUESDAY"

pattern Wednesday :: DayOfWeek
pattern Wednesday = DayOfWeek' "WEDNESDAY"

pattern Thursday :: DayOfWeek
pattern Thursday = DayOfWeek' "THURSDAY"

pattern Friday :: DayOfWeek
pattern Friday = DayOfWeek' "FRIDAY"

pattern Saturday :: DayOfWeek
pattern Saturday = DayOfWeek' "SATURDAY"

pattern Sunday :: DayOfWeek
pattern Sunday = DayOfWeek' "SUNDAY"

{-# COMPLETE
  Monday,
  Tuesday,
  Wednesday,
  Thursday,
  Friday,
  Saturday,
  Sunday,
  DayOfWeek'
  #-}
