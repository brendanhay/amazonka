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
-- Module      : Amazonka.Pinpoint.Types.DayOfWeek
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pinpoint.Types.DayOfWeek
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype DayOfWeek = DayOfWeek'
  { fromDayOfWeek ::
      Data.Text
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
