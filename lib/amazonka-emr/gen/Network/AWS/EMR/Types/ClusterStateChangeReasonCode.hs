{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.ClusterStateChangeReasonCode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EMR.Types.ClusterStateChangeReasonCode
  ( ClusterStateChangeReasonCode
    ( ClusterStateChangeReasonCode'
    , ClusterStateChangeReasonCodeInternalError
    , ClusterStateChangeReasonCodeValidationError
    , ClusterStateChangeReasonCodeInstanceFailure
    , ClusterStateChangeReasonCodeInstanceFleetTimeout
    , ClusterStateChangeReasonCodeBootstrapFailure
    , ClusterStateChangeReasonCodeUserRequest
    , ClusterStateChangeReasonCodeStepFailure
    , ClusterStateChangeReasonCodeAllStepsCompleted
    , fromClusterStateChangeReasonCode
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype ClusterStateChangeReasonCode = ClusterStateChangeReasonCode'{fromClusterStateChangeReasonCode
                                                                     :: Core.Text}
                                         deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                                         Core.Generic)
                                         deriving newtype (Core.IsString, Core.Hashable,
                                                           Core.NFData, Core.ToJSONKey,
                                                           Core.FromJSONKey, Core.ToJSON,
                                                           Core.FromJSON, Core.ToXML, Core.FromXML,
                                                           Core.ToText, Core.FromText,
                                                           Core.ToByteString, Core.ToQuery,
                                                           Core.ToHeader)

pattern ClusterStateChangeReasonCodeInternalError :: ClusterStateChangeReasonCode
pattern ClusterStateChangeReasonCodeInternalError = ClusterStateChangeReasonCode' "INTERNAL_ERROR"

pattern ClusterStateChangeReasonCodeValidationError :: ClusterStateChangeReasonCode
pattern ClusterStateChangeReasonCodeValidationError = ClusterStateChangeReasonCode' "VALIDATION_ERROR"

pattern ClusterStateChangeReasonCodeInstanceFailure :: ClusterStateChangeReasonCode
pattern ClusterStateChangeReasonCodeInstanceFailure = ClusterStateChangeReasonCode' "INSTANCE_FAILURE"

pattern ClusterStateChangeReasonCodeInstanceFleetTimeout :: ClusterStateChangeReasonCode
pattern ClusterStateChangeReasonCodeInstanceFleetTimeout = ClusterStateChangeReasonCode' "INSTANCE_FLEET_TIMEOUT"

pattern ClusterStateChangeReasonCodeBootstrapFailure :: ClusterStateChangeReasonCode
pattern ClusterStateChangeReasonCodeBootstrapFailure = ClusterStateChangeReasonCode' "BOOTSTRAP_FAILURE"

pattern ClusterStateChangeReasonCodeUserRequest :: ClusterStateChangeReasonCode
pattern ClusterStateChangeReasonCodeUserRequest = ClusterStateChangeReasonCode' "USER_REQUEST"

pattern ClusterStateChangeReasonCodeStepFailure :: ClusterStateChangeReasonCode
pattern ClusterStateChangeReasonCodeStepFailure = ClusterStateChangeReasonCode' "STEP_FAILURE"

pattern ClusterStateChangeReasonCodeAllStepsCompleted :: ClusterStateChangeReasonCode
pattern ClusterStateChangeReasonCodeAllStepsCompleted = ClusterStateChangeReasonCode' "ALL_STEPS_COMPLETED"

{-# COMPLETE 
  ClusterStateChangeReasonCodeInternalError,

  ClusterStateChangeReasonCodeValidationError,

  ClusterStateChangeReasonCodeInstanceFailure,

  ClusterStateChangeReasonCodeInstanceFleetTimeout,

  ClusterStateChangeReasonCodeBootstrapFailure,

  ClusterStateChangeReasonCodeUserRequest,

  ClusterStateChangeReasonCodeStepFailure,

  ClusterStateChangeReasonCodeAllStepsCompleted,
  ClusterStateChangeReasonCode'
  #-}
