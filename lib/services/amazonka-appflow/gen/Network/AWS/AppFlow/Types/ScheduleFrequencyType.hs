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
-- Module      : Network.AWS.AppFlow.Types.ScheduleFrequencyType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppFlow.Types.ScheduleFrequencyType
  ( ScheduleFrequencyType
      ( ..,
        ScheduleFrequencyType_BYMINUTE,
        ScheduleFrequencyType_DAILY,
        ScheduleFrequencyType_HOURLY,
        ScheduleFrequencyType_MONTHLY,
        ScheduleFrequencyType_ONCE,
        ScheduleFrequencyType_WEEKLY
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype ScheduleFrequencyType = ScheduleFrequencyType'
  { fromScheduleFrequencyType ::
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

pattern ScheduleFrequencyType_BYMINUTE :: ScheduleFrequencyType
pattern ScheduleFrequencyType_BYMINUTE = ScheduleFrequencyType' "BYMINUTE"

pattern ScheduleFrequencyType_DAILY :: ScheduleFrequencyType
pattern ScheduleFrequencyType_DAILY = ScheduleFrequencyType' "DAILY"

pattern ScheduleFrequencyType_HOURLY :: ScheduleFrequencyType
pattern ScheduleFrequencyType_HOURLY = ScheduleFrequencyType' "HOURLY"

pattern ScheduleFrequencyType_MONTHLY :: ScheduleFrequencyType
pattern ScheduleFrequencyType_MONTHLY = ScheduleFrequencyType' "MONTHLY"

pattern ScheduleFrequencyType_ONCE :: ScheduleFrequencyType
pattern ScheduleFrequencyType_ONCE = ScheduleFrequencyType' "ONCE"

pattern ScheduleFrequencyType_WEEKLY :: ScheduleFrequencyType
pattern ScheduleFrequencyType_WEEKLY = ScheduleFrequencyType' "WEEKLY"

{-# COMPLETE
  ScheduleFrequencyType_BYMINUTE,
  ScheduleFrequencyType_DAILY,
  ScheduleFrequencyType_HOURLY,
  ScheduleFrequencyType_MONTHLY,
  ScheduleFrequencyType_ONCE,
  ScheduleFrequencyType_WEEKLY,
  ScheduleFrequencyType'
  #-}
