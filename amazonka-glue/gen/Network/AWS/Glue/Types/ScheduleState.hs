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
-- Module      : Network.AWS.Glue.Types.ScheduleState
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.ScheduleState
  ( ScheduleState
      ( ..,
        ScheduleState_NOT_SCHEDULED,
        ScheduleState_SCHEDULED,
        ScheduleState_TRANSITIONING
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype ScheduleState = ScheduleState'
  { fromScheduleState ::
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
