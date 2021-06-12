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
-- Module      : Network.AWS.IoT.Types.DayOfWeek
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.DayOfWeek
  ( DayOfWeek
      ( ..,
        DayOfWeek_FRI,
        DayOfWeek_MON,
        DayOfWeek_SAT,
        DayOfWeek_SUN,
        DayOfWeek_THU,
        DayOfWeek_TUE,
        DayOfWeek_WED
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype DayOfWeek = DayOfWeek'
  { fromDayOfWeek ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
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

pattern DayOfWeek_FRI :: DayOfWeek
pattern DayOfWeek_FRI = DayOfWeek' "FRI"

pattern DayOfWeek_MON :: DayOfWeek
pattern DayOfWeek_MON = DayOfWeek' "MON"

pattern DayOfWeek_SAT :: DayOfWeek
pattern DayOfWeek_SAT = DayOfWeek' "SAT"

pattern DayOfWeek_SUN :: DayOfWeek
pattern DayOfWeek_SUN = DayOfWeek' "SUN"

pattern DayOfWeek_THU :: DayOfWeek
pattern DayOfWeek_THU = DayOfWeek' "THU"

pattern DayOfWeek_TUE :: DayOfWeek
pattern DayOfWeek_TUE = DayOfWeek' "TUE"

pattern DayOfWeek_WED :: DayOfWeek
pattern DayOfWeek_WED = DayOfWeek' "WED"

{-# COMPLETE
  DayOfWeek_FRI,
  DayOfWeek_MON,
  DayOfWeek_SAT,
  DayOfWeek_SUN,
  DayOfWeek_THU,
  DayOfWeek_TUE,
  DayOfWeek_WED,
  DayOfWeek'
  #-}
