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

import qualified Network.AWS.Prelude as Prelude

newtype ScheduleState = ScheduleState'
  { fromScheduleState ::
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
