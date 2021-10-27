{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DLM.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DLM.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _InvalidRequestException,
    _InternalServerException,
    _ResourceNotFoundException,
    _LimitExceededException,

    -- * EventSourceValues
    EventSourceValues (..),

    -- * EventTypeValues
    EventTypeValues (..),

    -- * GettablePolicyStateValues
    GettablePolicyStateValues (..),

    -- * IntervalUnitValues
    IntervalUnitValues (..),

    -- * LocationValues
    LocationValues (..),

    -- * PolicyTypeValues
    PolicyTypeValues (..),

    -- * ResourceLocationValues
    ResourceLocationValues (..),

    -- * ResourceTypeValues
    ResourceTypeValues (..),

    -- * RetentionIntervalUnitValues
    RetentionIntervalUnitValues (..),

    -- * SettablePolicyStateValues
    SettablePolicyStateValues (..),

    -- * Action
    Action (..),
    newAction,
    action_name,
    action_crossRegionCopy,

    -- * CreateRule
    CreateRule (..),
    newCreateRule,
    createRule_location,
    createRule_interval,
    createRule_cronExpression,
    createRule_times,
    createRule_intervalUnit,

    -- * CrossRegionCopyAction
    CrossRegionCopyAction (..),
    newCrossRegionCopyAction,
    crossRegionCopyAction_retainRule,
    crossRegionCopyAction_target,
    crossRegionCopyAction_encryptionConfiguration,

    -- * CrossRegionCopyDeprecateRule
    CrossRegionCopyDeprecateRule (..),
    newCrossRegionCopyDeprecateRule,
    crossRegionCopyDeprecateRule_interval,
    crossRegionCopyDeprecateRule_intervalUnit,

    -- * CrossRegionCopyRetainRule
    CrossRegionCopyRetainRule (..),
    newCrossRegionCopyRetainRule,
    crossRegionCopyRetainRule_interval,
    crossRegionCopyRetainRule_intervalUnit,

    -- * CrossRegionCopyRule
    CrossRegionCopyRule (..),
    newCrossRegionCopyRule,
    crossRegionCopyRule_deprecateRule,
    crossRegionCopyRule_targetRegion,
    crossRegionCopyRule_copyTags,
    crossRegionCopyRule_cmkArn,
    crossRegionCopyRule_retainRule,
    crossRegionCopyRule_target,
    crossRegionCopyRule_encrypted,

    -- * DeprecateRule
    DeprecateRule (..),
    newDeprecateRule,
    deprecateRule_count,
    deprecateRule_interval,
    deprecateRule_intervalUnit,

    -- * EncryptionConfiguration
    EncryptionConfiguration (..),
    newEncryptionConfiguration,
    encryptionConfiguration_cmkArn,
    encryptionConfiguration_encrypted,

    -- * EventParameters
    EventParameters (..),
    newEventParameters,
    eventParameters_eventType,
    eventParameters_snapshotOwner,
    eventParameters_descriptionRegex,

    -- * EventSource
    EventSource (..),
    newEventSource,
    eventSource_parameters,
    eventSource_type,

    -- * FastRestoreRule
    FastRestoreRule (..),
    newFastRestoreRule,
    fastRestoreRule_count,
    fastRestoreRule_interval,
    fastRestoreRule_intervalUnit,
    fastRestoreRule_availabilityZones,

    -- * LifecyclePolicy
    LifecyclePolicy (..),
    newLifecyclePolicy,
    lifecyclePolicy_state,
    lifecyclePolicy_policyDetails,
    lifecyclePolicy_policyId,
    lifecyclePolicy_executionRoleArn,
    lifecyclePolicy_dateCreated,
    lifecyclePolicy_statusMessage,
    lifecyclePolicy_dateModified,
    lifecyclePolicy_policyArn,
    lifecyclePolicy_description,
    lifecyclePolicy_tags,

    -- * LifecyclePolicySummary
    LifecyclePolicySummary (..),
    newLifecyclePolicySummary,
    lifecyclePolicySummary_state,
    lifecyclePolicySummary_policyId,
    lifecyclePolicySummary_policyType,
    lifecyclePolicySummary_description,
    lifecyclePolicySummary_tags,

    -- * Parameters
    Parameters (..),
    newParameters,
    parameters_noReboot,
    parameters_excludeBootVolume,

    -- * PolicyDetails
    PolicyDetails (..),
    newPolicyDetails,
    policyDetails_actions,
    policyDetails_targetTags,
    policyDetails_policyType,
    policyDetails_resourceLocations,
    policyDetails_parameters,
    policyDetails_schedules,
    policyDetails_eventSource,
    policyDetails_resourceTypes,

    -- * RetainRule
    RetainRule (..),
    newRetainRule,
    retainRule_count,
    retainRule_interval,
    retainRule_intervalUnit,

    -- * Schedule
    Schedule (..),
    newSchedule,
    schedule_variableTags,
    schedule_createRule,
    schedule_deprecateRule,
    schedule_copyTags,
    schedule_name,
    schedule_shareRules,
    schedule_tagsToAdd,
    schedule_retainRule,
    schedule_crossRegionCopyRules,
    schedule_fastRestoreRule,

    -- * ShareRule
    ShareRule (..),
    newShareRule,
    shareRule_unshareIntervalUnit,
    shareRule_unshareInterval,
    shareRule_targetAccounts,

    -- * Tag
    Tag (..),
    newTag,
    tag_key,
    tag_value,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.DLM.Types.Action
import Network.AWS.DLM.Types.CreateRule
import Network.AWS.DLM.Types.CrossRegionCopyAction
import Network.AWS.DLM.Types.CrossRegionCopyDeprecateRule
import Network.AWS.DLM.Types.CrossRegionCopyRetainRule
import Network.AWS.DLM.Types.CrossRegionCopyRule
import Network.AWS.DLM.Types.DeprecateRule
import Network.AWS.DLM.Types.EncryptionConfiguration
import Network.AWS.DLM.Types.EventParameters
import Network.AWS.DLM.Types.EventSource
import Network.AWS.DLM.Types.EventSourceValues
import Network.AWS.DLM.Types.EventTypeValues
import Network.AWS.DLM.Types.FastRestoreRule
import Network.AWS.DLM.Types.GettablePolicyStateValues
import Network.AWS.DLM.Types.IntervalUnitValues
import Network.AWS.DLM.Types.LifecyclePolicy
import Network.AWS.DLM.Types.LifecyclePolicySummary
import Network.AWS.DLM.Types.LocationValues
import Network.AWS.DLM.Types.Parameters
import Network.AWS.DLM.Types.PolicyDetails
import Network.AWS.DLM.Types.PolicyTypeValues
import Network.AWS.DLM.Types.ResourceLocationValues
import Network.AWS.DLM.Types.ResourceTypeValues
import Network.AWS.DLM.Types.RetainRule
import Network.AWS.DLM.Types.RetentionIntervalUnitValues
import Network.AWS.DLM.Types.Schedule
import Network.AWS.DLM.Types.SettablePolicyStateValues
import Network.AWS.DLM.Types.ShareRule
import Network.AWS.DLM.Types.Tag
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2018-01-12@ of the Amazon Data Lifecycle Manager SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core._serviceAbbrev = "DLM",
      Core._serviceSigner = Sign.v4,
      Core._serviceEndpointPrefix = "dlm",
      Core._serviceSigningName = "dlm",
      Core._serviceVersion = "2018-01-12",
      Core._serviceEndpoint =
        Core.defaultEndpoint defaultService,
      Core._serviceTimeout = Prelude.Just 70,
      Core._serviceCheck = Core.statusSuccess,
      Core._serviceError = Core.parseJSONError "DLM",
      Core._serviceRetry = retry
    }
  where
    retry =
      Core.Exponential
        { Core._retryBase = 5.0e-2,
          Core._retryGrowth = 2,
          Core._retryAttempts = 5,
          Core._retryCheck = check
        }
    check e
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has (Core.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Core.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Prelude.otherwise = Prelude.Nothing

-- | Bad request. The request is missing required parameters or has invalid
-- parameters.
_InvalidRequestException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidRequestException =
  Core._MatchServiceError
    defaultService
    "InvalidRequestException"
    Prelude.. Core.hasStatus 400

-- | The service failed in an unexpected way.
_InternalServerException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InternalServerException =
  Core._MatchServiceError
    defaultService
    "InternalServerException"
    Prelude.. Core.hasStatus 500

-- | A requested resource was not found.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"
    Prelude.. Core.hasStatus 404

-- | The request failed because a limit was exceeded.
_LimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_LimitExceededException =
  Core._MatchServiceError
    defaultService
    "LimitExceededException"
    Prelude.. Core.hasStatus 429
