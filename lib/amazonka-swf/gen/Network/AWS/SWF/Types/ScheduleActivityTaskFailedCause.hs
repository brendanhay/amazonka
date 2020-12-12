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
        SATFCActivityCreationRateExceeded,
        SATFCActivityIdAlreadyInUse,
        SATFCActivityTypeDeprecated,
        SATFCActivityTypeDoesNotExist,
        SATFCDefaultHeartbeatTimeoutUndefined,
        SATFCDefaultScheduleToCloseTimeoutUndefined,
        SATFCDefaultScheduleToStartTimeoutUndefined,
        SATFCDefaultStartToCloseTimeoutUndefined,
        SATFCDefaultTaskListUndefined,
        SATFCOpenActivitiesLimitExceeded,
        SATFCOperationNotPermitted
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype ScheduleActivityTaskFailedCause = ScheduleActivityTaskFailedCause' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern SATFCActivityCreationRateExceeded :: ScheduleActivityTaskFailedCause
pattern SATFCActivityCreationRateExceeded = ScheduleActivityTaskFailedCause' "ACTIVITY_CREATION_RATE_EXCEEDED"

pattern SATFCActivityIdAlreadyInUse :: ScheduleActivityTaskFailedCause
pattern SATFCActivityIdAlreadyInUse = ScheduleActivityTaskFailedCause' "ACTIVITY_ID_ALREADY_IN_USE"

pattern SATFCActivityTypeDeprecated :: ScheduleActivityTaskFailedCause
pattern SATFCActivityTypeDeprecated = ScheduleActivityTaskFailedCause' "ACTIVITY_TYPE_DEPRECATED"

pattern SATFCActivityTypeDoesNotExist :: ScheduleActivityTaskFailedCause
pattern SATFCActivityTypeDoesNotExist = ScheduleActivityTaskFailedCause' "ACTIVITY_TYPE_DOES_NOT_EXIST"

pattern SATFCDefaultHeartbeatTimeoutUndefined :: ScheduleActivityTaskFailedCause
pattern SATFCDefaultHeartbeatTimeoutUndefined = ScheduleActivityTaskFailedCause' "DEFAULT_HEARTBEAT_TIMEOUT_UNDEFINED"

pattern SATFCDefaultScheduleToCloseTimeoutUndefined :: ScheduleActivityTaskFailedCause
pattern SATFCDefaultScheduleToCloseTimeoutUndefined = ScheduleActivityTaskFailedCause' "DEFAULT_SCHEDULE_TO_CLOSE_TIMEOUT_UNDEFINED"

pattern SATFCDefaultScheduleToStartTimeoutUndefined :: ScheduleActivityTaskFailedCause
pattern SATFCDefaultScheduleToStartTimeoutUndefined = ScheduleActivityTaskFailedCause' "DEFAULT_SCHEDULE_TO_START_TIMEOUT_UNDEFINED"

pattern SATFCDefaultStartToCloseTimeoutUndefined :: ScheduleActivityTaskFailedCause
pattern SATFCDefaultStartToCloseTimeoutUndefined = ScheduleActivityTaskFailedCause' "DEFAULT_START_TO_CLOSE_TIMEOUT_UNDEFINED"

pattern SATFCDefaultTaskListUndefined :: ScheduleActivityTaskFailedCause
pattern SATFCDefaultTaskListUndefined = ScheduleActivityTaskFailedCause' "DEFAULT_TASK_LIST_UNDEFINED"

pattern SATFCOpenActivitiesLimitExceeded :: ScheduleActivityTaskFailedCause
pattern SATFCOpenActivitiesLimitExceeded = ScheduleActivityTaskFailedCause' "OPEN_ACTIVITIES_LIMIT_EXCEEDED"

pattern SATFCOperationNotPermitted :: ScheduleActivityTaskFailedCause
pattern SATFCOperationNotPermitted = ScheduleActivityTaskFailedCause' "OPERATION_NOT_PERMITTED"

{-# COMPLETE
  SATFCActivityCreationRateExceeded,
  SATFCActivityIdAlreadyInUse,
  SATFCActivityTypeDeprecated,
  SATFCActivityTypeDoesNotExist,
  SATFCDefaultHeartbeatTimeoutUndefined,
  SATFCDefaultScheduleToCloseTimeoutUndefined,
  SATFCDefaultScheduleToStartTimeoutUndefined,
  SATFCDefaultStartToCloseTimeoutUndefined,
  SATFCDefaultTaskListUndefined,
  SATFCOpenActivitiesLimitExceeded,
  SATFCOperationNotPermitted,
  ScheduleActivityTaskFailedCause'
  #-}
