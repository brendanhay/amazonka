{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.ScheduleActivityTaskFailedCause
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.ScheduleActivityTaskFailedCause where

import Network.AWS.Prelude

data ScheduleActivityTaskFailedCause
  = SATFCActivityCreationRateExceeded
  | SATFCActivityIdAlreadyInUse
  | SATFCActivityTypeDeprecated
  | SATFCActivityTypeDoesNotExist
  | SATFCDefaultHeartbeatTimeoutUndefined
  | SATFCDefaultScheduleToCloseTimeoutUndefined
  | SATFCDefaultScheduleToStartTimeoutUndefined
  | SATFCDefaultStartToCloseTimeoutUndefined
  | SATFCDefaultTaskListUndefined
  | SATFCOpenActivitiesLimitExceeded
  | SATFCOperationNotPermitted
  deriving
    ( Eq,
      Ord,
      Read,
      Show,
      Enum,
      Bounded,
      Data,
      Typeable,
      Generic
    )

instance FromText ScheduleActivityTaskFailedCause where
  parser =
    takeLowerText >>= \case
      "activity_creation_rate_exceeded" -> pure SATFCActivityCreationRateExceeded
      "activity_id_already_in_use" -> pure SATFCActivityIdAlreadyInUse
      "activity_type_deprecated" -> pure SATFCActivityTypeDeprecated
      "activity_type_does_not_exist" -> pure SATFCActivityTypeDoesNotExist
      "default_heartbeat_timeout_undefined" -> pure SATFCDefaultHeartbeatTimeoutUndefined
      "default_schedule_to_close_timeout_undefined" -> pure SATFCDefaultScheduleToCloseTimeoutUndefined
      "default_schedule_to_start_timeout_undefined" -> pure SATFCDefaultScheduleToStartTimeoutUndefined
      "default_start_to_close_timeout_undefined" -> pure SATFCDefaultStartToCloseTimeoutUndefined
      "default_task_list_undefined" -> pure SATFCDefaultTaskListUndefined
      "open_activities_limit_exceeded" -> pure SATFCOpenActivitiesLimitExceeded
      "operation_not_permitted" -> pure SATFCOperationNotPermitted
      e ->
        fromTextError $
          "Failure parsing ScheduleActivityTaskFailedCause from value: '" <> e
            <> "'. Accepted values: activity_creation_rate_exceeded, activity_id_already_in_use, activity_type_deprecated, activity_type_does_not_exist, default_heartbeat_timeout_undefined, default_schedule_to_close_timeout_undefined, default_schedule_to_start_timeout_undefined, default_start_to_close_timeout_undefined, default_task_list_undefined, open_activities_limit_exceeded, operation_not_permitted"

instance ToText ScheduleActivityTaskFailedCause where
  toText = \case
    SATFCActivityCreationRateExceeded -> "ACTIVITY_CREATION_RATE_EXCEEDED"
    SATFCActivityIdAlreadyInUse -> "ACTIVITY_ID_ALREADY_IN_USE"
    SATFCActivityTypeDeprecated -> "ACTIVITY_TYPE_DEPRECATED"
    SATFCActivityTypeDoesNotExist -> "ACTIVITY_TYPE_DOES_NOT_EXIST"
    SATFCDefaultHeartbeatTimeoutUndefined -> "DEFAULT_HEARTBEAT_TIMEOUT_UNDEFINED"
    SATFCDefaultScheduleToCloseTimeoutUndefined -> "DEFAULT_SCHEDULE_TO_CLOSE_TIMEOUT_UNDEFINED"
    SATFCDefaultScheduleToStartTimeoutUndefined -> "DEFAULT_SCHEDULE_TO_START_TIMEOUT_UNDEFINED"
    SATFCDefaultStartToCloseTimeoutUndefined -> "DEFAULT_START_TO_CLOSE_TIMEOUT_UNDEFINED"
    SATFCDefaultTaskListUndefined -> "DEFAULT_TASK_LIST_UNDEFINED"
    SATFCOpenActivitiesLimitExceeded -> "OPEN_ACTIVITIES_LIMIT_EXCEEDED"
    SATFCOperationNotPermitted -> "OPERATION_NOT_PERMITTED"

instance Hashable ScheduleActivityTaskFailedCause

instance NFData ScheduleActivityTaskFailedCause

instance ToByteString ScheduleActivityTaskFailedCause

instance ToQuery ScheduleActivityTaskFailedCause

instance ToHeader ScheduleActivityTaskFailedCause

instance FromJSON ScheduleActivityTaskFailedCause where
  parseJSON = parseJSONText "ScheduleActivityTaskFailedCause"
