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
-- Module      : Network.AWS.SWF.Types.ScheduleActivityTaskFailedCause
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.ScheduleActivityTaskFailedCause
  ( ScheduleActivityTaskFailedCause
      ( ..,
        ScheduleActivityTaskFailedCause_ACTIVITY_CREATION_RATE_EXCEEDED,
        ScheduleActivityTaskFailedCause_ACTIVITY_ID_ALREADY_IN_USE,
        ScheduleActivityTaskFailedCause_ACTIVITY_TYPE_DEPRECATED,
        ScheduleActivityTaskFailedCause_ACTIVITY_TYPE_DOES_NOT_EXIST,
        ScheduleActivityTaskFailedCause_DEFAULT_HEARTBEAT_TIMEOUT_UNDEFINED,
        ScheduleActivityTaskFailedCause_DEFAULT_SCHEDULE_TO_CLOSE_TIMEOUT_UNDEFINED,
        ScheduleActivityTaskFailedCause_DEFAULT_SCHEDULE_TO_START_TIMEOUT_UNDEFINED,
        ScheduleActivityTaskFailedCause_DEFAULT_START_TO_CLOSE_TIMEOUT_UNDEFINED,
        ScheduleActivityTaskFailedCause_DEFAULT_TASK_LIST_UNDEFINED,
        ScheduleActivityTaskFailedCause_OPEN_ACTIVITIES_LIMIT_EXCEEDED,
        ScheduleActivityTaskFailedCause_OPERATION_NOT_PERMITTED
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype ScheduleActivityTaskFailedCause = ScheduleActivityTaskFailedCause'
  { fromScheduleActivityTaskFailedCause ::
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

pattern ScheduleActivityTaskFailedCause_ACTIVITY_CREATION_RATE_EXCEEDED :: ScheduleActivityTaskFailedCause
pattern ScheduleActivityTaskFailedCause_ACTIVITY_CREATION_RATE_EXCEEDED = ScheduleActivityTaskFailedCause' "ACTIVITY_CREATION_RATE_EXCEEDED"

pattern ScheduleActivityTaskFailedCause_ACTIVITY_ID_ALREADY_IN_USE :: ScheduleActivityTaskFailedCause
pattern ScheduleActivityTaskFailedCause_ACTIVITY_ID_ALREADY_IN_USE = ScheduleActivityTaskFailedCause' "ACTIVITY_ID_ALREADY_IN_USE"

pattern ScheduleActivityTaskFailedCause_ACTIVITY_TYPE_DEPRECATED :: ScheduleActivityTaskFailedCause
pattern ScheduleActivityTaskFailedCause_ACTIVITY_TYPE_DEPRECATED = ScheduleActivityTaskFailedCause' "ACTIVITY_TYPE_DEPRECATED"

pattern ScheduleActivityTaskFailedCause_ACTIVITY_TYPE_DOES_NOT_EXIST :: ScheduleActivityTaskFailedCause
pattern ScheduleActivityTaskFailedCause_ACTIVITY_TYPE_DOES_NOT_EXIST = ScheduleActivityTaskFailedCause' "ACTIVITY_TYPE_DOES_NOT_EXIST"

pattern ScheduleActivityTaskFailedCause_DEFAULT_HEARTBEAT_TIMEOUT_UNDEFINED :: ScheduleActivityTaskFailedCause
pattern ScheduleActivityTaskFailedCause_DEFAULT_HEARTBEAT_TIMEOUT_UNDEFINED = ScheduleActivityTaskFailedCause' "DEFAULT_HEARTBEAT_TIMEOUT_UNDEFINED"

pattern ScheduleActivityTaskFailedCause_DEFAULT_SCHEDULE_TO_CLOSE_TIMEOUT_UNDEFINED :: ScheduleActivityTaskFailedCause
pattern ScheduleActivityTaskFailedCause_DEFAULT_SCHEDULE_TO_CLOSE_TIMEOUT_UNDEFINED = ScheduleActivityTaskFailedCause' "DEFAULT_SCHEDULE_TO_CLOSE_TIMEOUT_UNDEFINED"

pattern ScheduleActivityTaskFailedCause_DEFAULT_SCHEDULE_TO_START_TIMEOUT_UNDEFINED :: ScheduleActivityTaskFailedCause
pattern ScheduleActivityTaskFailedCause_DEFAULT_SCHEDULE_TO_START_TIMEOUT_UNDEFINED = ScheduleActivityTaskFailedCause' "DEFAULT_SCHEDULE_TO_START_TIMEOUT_UNDEFINED"

pattern ScheduleActivityTaskFailedCause_DEFAULT_START_TO_CLOSE_TIMEOUT_UNDEFINED :: ScheduleActivityTaskFailedCause
pattern ScheduleActivityTaskFailedCause_DEFAULT_START_TO_CLOSE_TIMEOUT_UNDEFINED = ScheduleActivityTaskFailedCause' "DEFAULT_START_TO_CLOSE_TIMEOUT_UNDEFINED"

pattern ScheduleActivityTaskFailedCause_DEFAULT_TASK_LIST_UNDEFINED :: ScheduleActivityTaskFailedCause
pattern ScheduleActivityTaskFailedCause_DEFAULT_TASK_LIST_UNDEFINED = ScheduleActivityTaskFailedCause' "DEFAULT_TASK_LIST_UNDEFINED"

pattern ScheduleActivityTaskFailedCause_OPEN_ACTIVITIES_LIMIT_EXCEEDED :: ScheduleActivityTaskFailedCause
pattern ScheduleActivityTaskFailedCause_OPEN_ACTIVITIES_LIMIT_EXCEEDED = ScheduleActivityTaskFailedCause' "OPEN_ACTIVITIES_LIMIT_EXCEEDED"

pattern ScheduleActivityTaskFailedCause_OPERATION_NOT_PERMITTED :: ScheduleActivityTaskFailedCause
pattern ScheduleActivityTaskFailedCause_OPERATION_NOT_PERMITTED = ScheduleActivityTaskFailedCause' "OPERATION_NOT_PERMITTED"

{-# COMPLETE
  ScheduleActivityTaskFailedCause_ACTIVITY_CREATION_RATE_EXCEEDED,
  ScheduleActivityTaskFailedCause_ACTIVITY_ID_ALREADY_IN_USE,
  ScheduleActivityTaskFailedCause_ACTIVITY_TYPE_DEPRECATED,
  ScheduleActivityTaskFailedCause_ACTIVITY_TYPE_DOES_NOT_EXIST,
  ScheduleActivityTaskFailedCause_DEFAULT_HEARTBEAT_TIMEOUT_UNDEFINED,
  ScheduleActivityTaskFailedCause_DEFAULT_SCHEDULE_TO_CLOSE_TIMEOUT_UNDEFINED,
  ScheduleActivityTaskFailedCause_DEFAULT_SCHEDULE_TO_START_TIMEOUT_UNDEFINED,
  ScheduleActivityTaskFailedCause_DEFAULT_START_TO_CLOSE_TIMEOUT_UNDEFINED,
  ScheduleActivityTaskFailedCause_DEFAULT_TASK_LIST_UNDEFINED,
  ScheduleActivityTaskFailedCause_OPEN_ACTIVITIES_LIMIT_EXCEEDED,
  ScheduleActivityTaskFailedCause_OPERATION_NOT_PERMITTED,
  ScheduleActivityTaskFailedCause'
  #-}
