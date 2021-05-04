{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MQ.Types.DayOfWeek
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MQ.Types.DayOfWeek
  ( DayOfWeek
      ( ..,
        DayOfWeek_FRIDAY,
        DayOfWeek_MONDAY,
        DayOfWeek_SATURDAY,
        DayOfWeek_SUNDAY,
        DayOfWeek_THURSDAY,
        DayOfWeek_TUESDAY,
        DayOfWeek_WEDNESDAY
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype DayOfWeek = DayOfWeek'
  { fromDayOfWeek ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
    )

pattern DayOfWeek_FRIDAY :: DayOfWeek
pattern DayOfWeek_FRIDAY = DayOfWeek' "FRIDAY"

pattern DayOfWeek_MONDAY :: DayOfWeek
pattern DayOfWeek_MONDAY = DayOfWeek' "MONDAY"

pattern DayOfWeek_SATURDAY :: DayOfWeek
pattern DayOfWeek_SATURDAY = DayOfWeek' "SATURDAY"

pattern DayOfWeek_SUNDAY :: DayOfWeek
pattern DayOfWeek_SUNDAY = DayOfWeek' "SUNDAY"

pattern DayOfWeek_THURSDAY :: DayOfWeek
pattern DayOfWeek_THURSDAY = DayOfWeek' "THURSDAY"

pattern DayOfWeek_TUESDAY :: DayOfWeek
pattern DayOfWeek_TUESDAY = DayOfWeek' "TUESDAY"

pattern DayOfWeek_WEDNESDAY :: DayOfWeek
pattern DayOfWeek_WEDNESDAY = DayOfWeek' "WEDNESDAY"

{-# COMPLETE
  DayOfWeek_FRIDAY,
  DayOfWeek_MONDAY,
  DayOfWeek_SATURDAY,
  DayOfWeek_SUNDAY,
  DayOfWeek_THURSDAY,
  DayOfWeek_TUESDAY,
  DayOfWeek_WEDNESDAY,
  DayOfWeek'
  #-}
