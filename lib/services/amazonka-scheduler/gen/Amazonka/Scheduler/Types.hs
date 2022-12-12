{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Scheduler.Types
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Scheduler.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _ConflictException,
    _InternalServerException,
    _ResourceNotFoundException,
    _ServiceQuotaExceededException,
    _ThrottlingException,
    _ValidationException,

    -- * AssignPublicIp
    AssignPublicIp (..),

    -- * FlexibleTimeWindowMode
    FlexibleTimeWindowMode (..),

    -- * LaunchType
    LaunchType (..),

    -- * PlacementConstraintType
    PlacementConstraintType (..),

    -- * PlacementStrategyType
    PlacementStrategyType (..),

    -- * PropagateTags
    PropagateTags (..),

    -- * ScheduleGroupState
    ScheduleGroupState (..),

    -- * ScheduleState
    ScheduleState (..),

    -- * AwsVpcConfiguration
    AwsVpcConfiguration (..),
    newAwsVpcConfiguration,
    awsVpcConfiguration_assignPublicIp,
    awsVpcConfiguration_securityGroups,
    awsVpcConfiguration_subnets,

    -- * CapacityProviderStrategyItem
    CapacityProviderStrategyItem (..),
    newCapacityProviderStrategyItem,
    capacityProviderStrategyItem_base,
    capacityProviderStrategyItem_weight,
    capacityProviderStrategyItem_capacityProvider,

    -- * DeadLetterConfig
    DeadLetterConfig (..),
    newDeadLetterConfig,
    deadLetterConfig_arn,

    -- * EcsParameters
    EcsParameters (..),
    newEcsParameters,
    ecsParameters_capacityProviderStrategy,
    ecsParameters_enableECSManagedTags,
    ecsParameters_enableExecuteCommand,
    ecsParameters_group,
    ecsParameters_launchType,
    ecsParameters_networkConfiguration,
    ecsParameters_placementConstraints,
    ecsParameters_placementStrategy,
    ecsParameters_platformVersion,
    ecsParameters_propagateTags,
    ecsParameters_referenceId,
    ecsParameters_tags,
    ecsParameters_taskCount,
    ecsParameters_taskDefinitionArn,

    -- * EventBridgeParameters
    EventBridgeParameters (..),
    newEventBridgeParameters,
    eventBridgeParameters_detailType,
    eventBridgeParameters_source,

    -- * FlexibleTimeWindow
    FlexibleTimeWindow (..),
    newFlexibleTimeWindow,
    flexibleTimeWindow_maximumWindowInMinutes,
    flexibleTimeWindow_mode,

    -- * KinesisParameters
    KinesisParameters (..),
    newKinesisParameters,
    kinesisParameters_partitionKey,

    -- * NetworkConfiguration
    NetworkConfiguration (..),
    newNetworkConfiguration,
    networkConfiguration_awsvpcConfiguration,

    -- * PlacementConstraint
    PlacementConstraint (..),
    newPlacementConstraint,
    placementConstraint_expression,
    placementConstraint_type,

    -- * PlacementStrategy
    PlacementStrategy (..),
    newPlacementStrategy,
    placementStrategy_field,
    placementStrategy_type,

    -- * RetryPolicy
    RetryPolicy (..),
    newRetryPolicy,
    retryPolicy_maximumEventAgeInSeconds,
    retryPolicy_maximumRetryAttempts,

    -- * SageMakerPipelineParameter
    SageMakerPipelineParameter (..),
    newSageMakerPipelineParameter,
    sageMakerPipelineParameter_name,
    sageMakerPipelineParameter_value,

    -- * SageMakerPipelineParameters
    SageMakerPipelineParameters (..),
    newSageMakerPipelineParameters,
    sageMakerPipelineParameters_pipelineParameterList,

    -- * ScheduleGroupSummary
    ScheduleGroupSummary (..),
    newScheduleGroupSummary,
    scheduleGroupSummary_arn,
    scheduleGroupSummary_creationDate,
    scheduleGroupSummary_lastModificationDate,
    scheduleGroupSummary_name,
    scheduleGroupSummary_state,

    -- * ScheduleSummary
    ScheduleSummary (..),
    newScheduleSummary,
    scheduleSummary_arn,
    scheduleSummary_creationDate,
    scheduleSummary_groupName,
    scheduleSummary_lastModificationDate,
    scheduleSummary_name,
    scheduleSummary_state,
    scheduleSummary_target,

    -- * SqsParameters
    SqsParameters (..),
    newSqsParameters,
    sqsParameters_messageGroupId,

    -- * Tag
    Tag (..),
    newTag,
    tag_key,
    tag_value,

    -- * Target
    Target (..),
    newTarget,
    target_deadLetterConfig,
    target_ecsParameters,
    target_eventBridgeParameters,
    target_input,
    target_kinesisParameters,
    target_retryPolicy,
    target_sageMakerPipelineParameters,
    target_sqsParameters,
    target_arn,
    target_roleArn,

    -- * TargetSummary
    TargetSummary (..),
    newTargetSummary,
    targetSummary_arn,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.Scheduler.Types.AssignPublicIp
import Amazonka.Scheduler.Types.AwsVpcConfiguration
import Amazonka.Scheduler.Types.CapacityProviderStrategyItem
import Amazonka.Scheduler.Types.DeadLetterConfig
import Amazonka.Scheduler.Types.EcsParameters
import Amazonka.Scheduler.Types.EventBridgeParameters
import Amazonka.Scheduler.Types.FlexibleTimeWindow
import Amazonka.Scheduler.Types.FlexibleTimeWindowMode
import Amazonka.Scheduler.Types.KinesisParameters
import Amazonka.Scheduler.Types.LaunchType
import Amazonka.Scheduler.Types.NetworkConfiguration
import Amazonka.Scheduler.Types.PlacementConstraint
import Amazonka.Scheduler.Types.PlacementConstraintType
import Amazonka.Scheduler.Types.PlacementStrategy
import Amazonka.Scheduler.Types.PlacementStrategyType
import Amazonka.Scheduler.Types.PropagateTags
import Amazonka.Scheduler.Types.RetryPolicy
import Amazonka.Scheduler.Types.SageMakerPipelineParameter
import Amazonka.Scheduler.Types.SageMakerPipelineParameters
import Amazonka.Scheduler.Types.ScheduleGroupState
import Amazonka.Scheduler.Types.ScheduleGroupSummary
import Amazonka.Scheduler.Types.ScheduleState
import Amazonka.Scheduler.Types.ScheduleSummary
import Amazonka.Scheduler.Types.SqsParameters
import Amazonka.Scheduler.Types.Tag
import Amazonka.Scheduler.Types.Target
import Amazonka.Scheduler.Types.TargetSummary
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2021-06-30@ of the Amazon EventBridge Scheduler SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "Scheduler",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "scheduler",
      Core.signingName = "scheduler",
      Core.version = "2021-06-30",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error = Core.parseJSONError "Scheduler",
      Core.retry = retry
    }
  where
    retry =
      Core.Exponential
        { Core.base = 5.0e-2,
          Core.growth = 2,
          Core.attempts = 5,
          Core.check = check
        }
    check e
      | Lens.has (Core.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has (Core.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Prelude.otherwise = Prelude.Nothing

-- | Updating or deleting the resource can cause an inconsistent state.
_ConflictException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ConflictException =
  Core._MatchServiceError
    defaultService
    "ConflictException"
    Prelude.. Core.hasStatus 409

-- | Unexpected error encountered while processing the request.
_InternalServerException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InternalServerException =
  Core._MatchServiceError
    defaultService
    "InternalServerException"
    Prelude.. Core.hasStatus 500

-- | The request references a resource which does not exist.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"
    Prelude.. Core.hasStatus 404

-- | The request exceeds a service quota.
_ServiceQuotaExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ServiceQuotaExceededException =
  Core._MatchServiceError
    defaultService
    "ServiceQuotaExceededException"
    Prelude.. Core.hasStatus 402

-- | The request was denied due to request throttling.
_ThrottlingException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ThrottlingException =
  Core._MatchServiceError
    defaultService
    "ThrottlingException"
    Prelude.. Core.hasStatus 429

-- | The input fails to satisfy the constraints specified by an AWS service.
_ValidationException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ValidationException =
  Core._MatchServiceError
    defaultService
    "ValidationException"
    Prelude.. Core.hasStatus 400
