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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype DayOfWeek = DayOfWeek'
  { fromDayOfWeek ::
      Core.Text
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
      Core.FromText,
      Core.ToText,
      Core.ToByteString,
      Core.ToLog,
      Core.ToHeader,
      Core.ToQuery,
      Core.FromJSON,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.ToJSONKey,
      Core.FromXML,
      Core.ToXML
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
