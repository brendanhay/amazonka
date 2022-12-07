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
-- Module      : Amazonka.Glue.Types.ScheduleState
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.ScheduleState
  ( ScheduleState
      ( ..,
        ScheduleState_NOT_SCHEDULED,
        ScheduleState_SCHEDULED,
        ScheduleState_TRANSITIONING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ScheduleState = ScheduleState'
  { fromScheduleState ::
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

pattern ScheduleState_NOT_SCHEDULED :: ScheduleState
pattern ScheduleState_NOT_SCHEDULED = ScheduleState' "NOT_SCHEDULED"

pattern ScheduleState_SCHEDULED :: ScheduleState
pattern ScheduleState_SCHEDULED = ScheduleState' "SCHEDULED"

pattern ScheduleState_TRANSITIONING :: ScheduleState
pattern ScheduleState_TRANSITIONING = ScheduleState' "TRANSITIONING"

{-# COMPLETE
  ScheduleState_NOT_SCHEDULED,
  ScheduleState_SCHEDULED,
  ScheduleState_TRANSITIONING,
  ScheduleState'
  #-}
