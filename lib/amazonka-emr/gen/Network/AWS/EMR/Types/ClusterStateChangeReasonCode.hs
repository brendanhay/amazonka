{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.ClusterStateChangeReasonCode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.ClusterStateChangeReasonCode where

import Network.AWS.Prelude

data ClusterStateChangeReasonCode
  = CSCRCAllStepsCompleted
  | CSCRCBootstrapFailure
  | CSCRCInstanceFailure
  | CSCRCInstanceFleetTimeout
  | CSCRCInternalError
  | CSCRCStepFailure
  | CSCRCUserRequest
  | CSCRCValidationError
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

instance FromText ClusterStateChangeReasonCode where
  parser =
    takeLowerText >>= \case
      "all_steps_completed" -> pure CSCRCAllStepsCompleted
      "bootstrap_failure" -> pure CSCRCBootstrapFailure
      "instance_failure" -> pure CSCRCInstanceFailure
      "instance_fleet_timeout" -> pure CSCRCInstanceFleetTimeout
      "internal_error" -> pure CSCRCInternalError
      "step_failure" -> pure CSCRCStepFailure
      "user_request" -> pure CSCRCUserRequest
      "validation_error" -> pure CSCRCValidationError
      e ->
        fromTextError $
          "Failure parsing ClusterStateChangeReasonCode from value: '" <> e
            <> "'. Accepted values: all_steps_completed, bootstrap_failure, instance_failure, instance_fleet_timeout, internal_error, step_failure, user_request, validation_error"

instance ToText ClusterStateChangeReasonCode where
  toText = \case
    CSCRCAllStepsCompleted -> "ALL_STEPS_COMPLETED"
    CSCRCBootstrapFailure -> "BOOTSTRAP_FAILURE"
    CSCRCInstanceFailure -> "INSTANCE_FAILURE"
    CSCRCInstanceFleetTimeout -> "INSTANCE_FLEET_TIMEOUT"
    CSCRCInternalError -> "INTERNAL_ERROR"
    CSCRCStepFailure -> "STEP_FAILURE"
    CSCRCUserRequest -> "USER_REQUEST"
    CSCRCValidationError -> "VALIDATION_ERROR"

instance Hashable ClusterStateChangeReasonCode

instance NFData ClusterStateChangeReasonCode

instance ToByteString ClusterStateChangeReasonCode

instance ToQuery ClusterStateChangeReasonCode

instance ToHeader ClusterStateChangeReasonCode

instance FromJSON ClusterStateChangeReasonCode where
  parseJSON = parseJSONText "ClusterStateChangeReasonCode"
