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
-- Module      : Amazonka.AppFlow.Types.ScheduleFrequencyType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppFlow.Types.ScheduleFrequencyType
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ScheduleFrequencyType = ScheduleFrequencyType'
  { fromScheduleFrequencyType ::
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
