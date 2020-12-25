{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.ScheduleActivityTaskFailedCause
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.ScheduleActivityTaskFailedCause
  ( ScheduleActivityTaskFailedCause
      ( ScheduleActivityTaskFailedCause',
        ScheduleActivityTaskFailedCauseActivityTypeDeprecated,
        ScheduleActivityTaskFailedCauseActivityTypeDoesNotExist,
        ScheduleActivityTaskFailedCauseActivityIdAlreadyInUse,
        ScheduleActivityTaskFailedCauseOpenActivitiesLimitExceeded,
        ScheduleActivityTaskFailedCauseActivityCreationRateExceeded,
        ScheduleActivityTaskFailedCauseDefaultScheduleToCloseTimeoutUndefined,
        ScheduleActivityTaskFailedCauseDefaultTaskListUndefined,
        ScheduleActivityTaskFailedCauseDefaultScheduleToStartTimeoutUndefined,
        ScheduleActivityTaskFailedCauseDefaultStartToCloseTimeoutUndefined,
        ScheduleActivityTaskFailedCauseDefaultHeartbeatTimeoutUndefined,
        ScheduleActivityTaskFailedCauseOperationNotPermitted,
        fromScheduleActivityTaskFailedCause
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype ScheduleActivityTaskFailedCause = ScheduleActivityTaskFailedCause'
  { fromScheduleActivityTaskFailedCause ::
      Core.Text
  }
  deriving stock
    ( Core.Eq,
      Core.Ord,
      Core.Read,
      Core.Show,
      Core.Generic
    )
  deriving newtype
    ( Core.IsString,
      Core.Hashable,
      Core.NFData,
      Core.ToJSONKey,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.FromJSON,
      Core.ToXML,
      Core.FromXML,
      Core.ToText,
      Core.FromText,
      Core.ToByteString,
      Core.ToQuery,
      Core.ToHeader
    )

pattern ScheduleActivityTaskFailedCauseActivityTypeDeprecated :: ScheduleActivityTaskFailedCause
pattern ScheduleActivityTaskFailedCauseActivityTypeDeprecated = ScheduleActivityTaskFailedCause' "ACTIVITY_TYPE_DEPRECATED"

pattern ScheduleActivityTaskFailedCauseActivityTypeDoesNotExist :: ScheduleActivityTaskFailedCause
pattern ScheduleActivityTaskFailedCauseActivityTypeDoesNotExist = ScheduleActivityTaskFailedCause' "ACTIVITY_TYPE_DOES_NOT_EXIST"

pattern ScheduleActivityTaskFailedCauseActivityIdAlreadyInUse :: ScheduleActivityTaskFailedCause
pattern ScheduleActivityTaskFailedCauseActivityIdAlreadyInUse = ScheduleActivityTaskFailedCause' "ACTIVITY_ID_ALREADY_IN_USE"

pattern ScheduleActivityTaskFailedCauseOpenActivitiesLimitExceeded :: ScheduleActivityTaskFailedCause
pattern ScheduleActivityTaskFailedCauseOpenActivitiesLimitExceeded = ScheduleActivityTaskFailedCause' "OPEN_ACTIVITIES_LIMIT_EXCEEDED"

pattern ScheduleActivityTaskFailedCauseActivityCreationRateExceeded :: ScheduleActivityTaskFailedCause
pattern ScheduleActivityTaskFailedCauseActivityCreationRateExceeded = ScheduleActivityTaskFailedCause' "ACTIVITY_CREATION_RATE_EXCEEDED"

pattern ScheduleActivityTaskFailedCauseDefaultScheduleToCloseTimeoutUndefined :: ScheduleActivityTaskFailedCause
pattern ScheduleActivityTaskFailedCauseDefaultScheduleToCloseTimeoutUndefined = ScheduleActivityTaskFailedCause' "DEFAULT_SCHEDULE_TO_CLOSE_TIMEOUT_UNDEFINED"

pattern ScheduleActivityTaskFailedCauseDefaultTaskListUndefined :: ScheduleActivityTaskFailedCause
pattern ScheduleActivityTaskFailedCauseDefaultTaskListUndefined = ScheduleActivityTaskFailedCause' "DEFAULT_TASK_LIST_UNDEFINED"

pattern ScheduleActivityTaskFailedCauseDefaultScheduleToStartTimeoutUndefined :: ScheduleActivityTaskFailedCause
pattern ScheduleActivityTaskFailedCauseDefaultScheduleToStartTimeoutUndefined = ScheduleActivityTaskFailedCause' "DEFAULT_SCHEDULE_TO_START_TIMEOUT_UNDEFINED"

pattern ScheduleActivityTaskFailedCauseDefaultStartToCloseTimeoutUndefined :: ScheduleActivityTaskFailedCause
pattern ScheduleActivityTaskFailedCauseDefaultStartToCloseTimeoutUndefined = ScheduleActivityTaskFailedCause' "DEFAULT_START_TO_CLOSE_TIMEOUT_UNDEFINED"

pattern ScheduleActivityTaskFailedCauseDefaultHeartbeatTimeoutUndefined :: ScheduleActivityTaskFailedCause
pattern ScheduleActivityTaskFailedCauseDefaultHeartbeatTimeoutUndefined = ScheduleActivityTaskFailedCause' "DEFAULT_HEARTBEAT_TIMEOUT_UNDEFINED"

pattern ScheduleActivityTaskFailedCauseOperationNotPermitted :: ScheduleActivityTaskFailedCause
pattern ScheduleActivityTaskFailedCauseOperationNotPermitted = ScheduleActivityTaskFailedCause' "OPERATION_NOT_PERMITTED"

{-# COMPLETE
  ScheduleActivityTaskFailedCauseActivityTypeDeprecated,
  ScheduleActivityTaskFailedCauseActivityTypeDoesNotExist,
  ScheduleActivityTaskFailedCauseActivityIdAlreadyInUse,
  ScheduleActivityTaskFailedCauseOpenActivitiesLimitExceeded,
  ScheduleActivityTaskFailedCauseActivityCreationRateExceeded,
  ScheduleActivityTaskFailedCauseDefaultScheduleToCloseTimeoutUndefined,
  ScheduleActivityTaskFailedCauseDefaultTaskListUndefined,
  ScheduleActivityTaskFailedCauseDefaultScheduleToStartTimeoutUndefined,
  ScheduleActivityTaskFailedCauseDefaultStartToCloseTimeoutUndefined,
  ScheduleActivityTaskFailedCauseDefaultHeartbeatTimeoutUndefined,
  ScheduleActivityTaskFailedCauseOperationNotPermitted,
  ScheduleActivityTaskFailedCause'
  #-}
