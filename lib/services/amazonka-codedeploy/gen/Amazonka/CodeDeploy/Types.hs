{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.CodeDeploy.Types
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeDeploy.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _AlarmsLimitExceededException,
    _InvalidDeploymentStyleException,
    _InvalidIamUserArnException,
    _DeploymentIsNotInReadyStateException,
    _InvalidDeploymentConfigNameException,
    _InvalidDeploymentWaitTypeException,
    _ApplicationDoesNotExistException,
    _InvalidApplicationNameException,
    _InvalidAlarmConfigException,
    _IamUserArnRequiredException,
    _TagSetListLimitExceededException,
    _InvalidInputException,
    _ApplicationNameRequiredException,
    _InvalidLifecycleEventHookExecutionIdException,
    _InvalidMinimumHealthyHostValueException,
    _InvalidDeploymentStatusException,
    _MultipleIamArnsProvidedException,
    _InvalidOperationException,
    _InvalidUpdateOutdatedInstancesOnlyValueException,
    _DeploymentGroupLimitExceededException,
    _InvalidECSServiceException,
    _ArnNotSupportedException,
    _DeploymentNotStartedException,
    _InvalidComputePlatformException,
    _InvalidOnPremisesTagCombinationException,
    _RevisionRequiredException,
    _DeploymentTargetIdRequiredException,
    _ResourceValidationException,
    _TagRequiredException,
    _ECSServiceMappingLimitExceededException,
    _ResourceArnRequiredException,
    _LifecycleHookLimitExceededException,
    _InstanceNotRegisteredException,
    _DeploymentAlreadyStartedException,
    _InvalidRoleException,
    _InvalidFileExistsBehaviorException,
    _InstanceDoesNotExistException,
    _DeploymentConfigInUseException,
    _LifecycleEventAlreadyCompletedException,
    _IamArnRequiredException,
    _InstanceNameRequiredException,
    _IamUserArnAlreadyRegisteredException,
    _DeploymentTargetListSizeExceededException,
    _InvalidKeyPrefixFilterException,
    _RoleRequiredException,
    _InvalidTagsToAddException,
    _InvalidDeployedStateFilterException,
    _IamSessionArnAlreadyRegisteredException,
    _InvalidAutoRollbackConfigException,
    _DeploymentTargetDoesNotExistException,
    _InvalidBucketNameFilterException,
    _DeploymentLimitExceededException,
    _BucketNameFilterRequiredException,
    _InvalidTargetFilterNameException,
    _InvalidSortOrderException,
    _InvalidNextTokenException,
    _InstanceLimitExceededException,
    _InvalidIgnoreApplicationStopFailuresValueException,
    _InvalidLifecycleEventHookExecutionStatusException,
    _InvalidTrafficRoutingConfigurationException,
    _InvalidEC2TagException,
    _InvalidEC2TagCombinationException,
    _DeploymentGroupDoesNotExistException,
    _DescriptionTooLongException,
    _InvalidIamSessionArnException,
    _ThrottlingException,
    _InvalidTagFilterException,
    _InvalidInstanceTypeException,
    _DeploymentGroupNameRequiredException,
    _DeploymentAlreadyCompletedException,
    _TagLimitExceededException,
    _DeploymentGroupAlreadyExistsException,
    _InvalidExternalIdException,
    _DeploymentConfigDoesNotExistException,
    _InvalidSortByException,
    _InvalidDeploymentTargetIdException,
    _InvalidTargetException,
    _InvalidRegistrationStatusException,
    _DeploymentConfigNameRequiredException,
    _InvalidArnException,
    _InvalidTargetGroupPairException,
    _InvalidTriggerConfigException,
    _UnsupportedActionForDeploymentTypeException,
    _InvalidTargetInstancesException,
    _InvalidRevisionException,
    _InvalidTagException,
    _InvalidInstanceStatusException,
    _InvalidDeploymentGroupNameException,
    _GitHubAccountTokenDoesNotExistException,
    _InvalidDeploymentIdException,
    _GitHubAccountTokenNameRequiredException,
    _InvalidTimeRangeException,
    _InstanceIdRequiredException,
    _InvalidGitHubAccountTokenException,
    _InvalidAutoScalingGroupException,
    _BatchLimitExceededException,
    _DeploymentConfigLimitExceededException,
    _ApplicationLimitExceededException,
    _ApplicationAlreadyExistsException,
    _RevisionDoesNotExistException,
    _DeploymentConfigAlreadyExistsException,
    _InvalidInstanceNameException,
    _DeploymentIdRequiredException,
    _InstanceNameAlreadyRegisteredException,
    _InvalidLoadBalancerInfoException,
    _InvalidBlueGreenDeploymentConfigurationException,
    _OperationNotSupportedException,
    _TriggerTargetsLimitExceededException,
    _InvalidDeploymentInstanceTypeException,
    _InvalidGitHubAccountTokenNameException,
    _DeploymentDoesNotExistException,
    _InvalidInstanceIdException,

    -- * ApplicationRevisionSortBy
    ApplicationRevisionSortBy (..),

    -- * AutoRollbackEvent
    AutoRollbackEvent (..),

    -- * BundleType
    BundleType (..),

    -- * ComputePlatform
    ComputePlatform (..),

    -- * DeployErrorCode
    DeployErrorCode (..),

    -- * DeploymentCreator
    DeploymentCreator (..),

    -- * DeploymentOption
    DeploymentOption (..),

    -- * DeploymentReadyAction
    DeploymentReadyAction (..),

    -- * DeploymentStatus
    DeploymentStatus (..),

    -- * DeploymentTargetType
    DeploymentTargetType (..),

    -- * DeploymentType
    DeploymentType (..),

    -- * DeploymentWaitType
    DeploymentWaitType (..),

    -- * EC2TagFilterType
    EC2TagFilterType (..),

    -- * FileExistsBehavior
    FileExistsBehavior (..),

    -- * GreenFleetProvisioningAction
    GreenFleetProvisioningAction (..),

    -- * InstanceAction
    InstanceAction (..),

    -- * LifecycleErrorCode
    LifecycleErrorCode (..),

    -- * LifecycleEventStatus
    LifecycleEventStatus (..),

    -- * ListStateFilterAction
    ListStateFilterAction (..),

    -- * MinimumHealthyHostsType
    MinimumHealthyHostsType (..),

    -- * OutdatedInstancesStrategy
    OutdatedInstancesStrategy (..),

    -- * RegistrationStatus
    RegistrationStatus (..),

    -- * RevisionLocationType
    RevisionLocationType (..),

    -- * SortOrder
    SortOrder (..),

    -- * StopStatus
    StopStatus (..),

    -- * TagFilterType
    TagFilterType (..),

    -- * TargetFilterName
    TargetFilterName (..),

    -- * TargetLabel
    TargetLabel (..),

    -- * TargetStatus
    TargetStatus (..),

    -- * TrafficRoutingType
    TrafficRoutingType (..),

    -- * TriggerEventType
    TriggerEventType (..),

    -- * Alarm
    Alarm (..),
    newAlarm,
    alarm_name,

    -- * AlarmConfiguration
    AlarmConfiguration (..),
    newAlarmConfiguration,
    alarmConfiguration_alarms,
    alarmConfiguration_ignorePollAlarmFailure,
    alarmConfiguration_enabled,

    -- * AppSpecContent
    AppSpecContent (..),
    newAppSpecContent,
    appSpecContent_sha256,
    appSpecContent_content,

    -- * ApplicationInfo
    ApplicationInfo (..),
    newApplicationInfo,
    applicationInfo_linkedToGitHub,
    applicationInfo_gitHubAccountName,
    applicationInfo_computePlatform,
    applicationInfo_createTime,
    applicationInfo_applicationId,
    applicationInfo_applicationName,

    -- * AutoRollbackConfiguration
    AutoRollbackConfiguration (..),
    newAutoRollbackConfiguration,
    autoRollbackConfiguration_enabled,
    autoRollbackConfiguration_events,

    -- * AutoScalingGroup
    AutoScalingGroup (..),
    newAutoScalingGroup,
    autoScalingGroup_name,
    autoScalingGroup_hook,

    -- * BlueGreenDeploymentConfiguration
    BlueGreenDeploymentConfiguration (..),
    newBlueGreenDeploymentConfiguration,
    blueGreenDeploymentConfiguration_greenFleetProvisioningOption,
    blueGreenDeploymentConfiguration_terminateBlueInstancesOnDeploymentSuccess,
    blueGreenDeploymentConfiguration_deploymentReadyOption,

    -- * BlueInstanceTerminationOption
    BlueInstanceTerminationOption (..),
    newBlueInstanceTerminationOption,
    blueInstanceTerminationOption_terminationWaitTimeInMinutes,
    blueInstanceTerminationOption_action,

    -- * CloudFormationTarget
    CloudFormationTarget (..),
    newCloudFormationTarget,
    cloudFormationTarget_resourceType,
    cloudFormationTarget_targetId,
    cloudFormationTarget_lifecycleEvents,
    cloudFormationTarget_lastUpdatedAt,
    cloudFormationTarget_targetVersionWeight,
    cloudFormationTarget_deploymentId,
    cloudFormationTarget_status,

    -- * DeploymentConfigInfo
    DeploymentConfigInfo (..),
    newDeploymentConfigInfo,
    deploymentConfigInfo_deploymentConfigId,
    deploymentConfigInfo_computePlatform,
    deploymentConfigInfo_trafficRoutingConfig,
    deploymentConfigInfo_createTime,
    deploymentConfigInfo_minimumHealthyHosts,
    deploymentConfigInfo_deploymentConfigName,

    -- * DeploymentGroupInfo
    DeploymentGroupInfo (..),
    newDeploymentGroupInfo,
    deploymentGroupInfo_targetRevision,
    deploymentGroupInfo_loadBalancerInfo,
    deploymentGroupInfo_autoScalingGroups,
    deploymentGroupInfo_serviceRoleArn,
    deploymentGroupInfo_lastSuccessfulDeployment,
    deploymentGroupInfo_triggerConfigurations,
    deploymentGroupInfo_deploymentGroupName,
    deploymentGroupInfo_computePlatform,
    deploymentGroupInfo_ec2TagFilters,
    deploymentGroupInfo_autoRollbackConfiguration,
    deploymentGroupInfo_deploymentStyle,
    deploymentGroupInfo_blueGreenDeploymentConfiguration,
    deploymentGroupInfo_alarmConfiguration,
    deploymentGroupInfo_ecsServices,
    deploymentGroupInfo_onPremisesTagSet,
    deploymentGroupInfo_outdatedInstancesStrategy,
    deploymentGroupInfo_onPremisesInstanceTagFilters,
    deploymentGroupInfo_ec2TagSet,
    deploymentGroupInfo_deploymentGroupId,
    deploymentGroupInfo_lastAttemptedDeployment,
    deploymentGroupInfo_deploymentConfigName,
    deploymentGroupInfo_applicationName,

    -- * DeploymentInfo
    DeploymentInfo (..),
    newDeploymentInfo,
    deploymentInfo_overrideAlarmConfiguration,
    deploymentInfo_loadBalancerInfo,
    deploymentInfo_deploymentGroupName,
    deploymentInfo_deploymentId,
    deploymentInfo_fileExistsBehavior,
    deploymentInfo_deploymentOverview,
    deploymentInfo_revision,
    deploymentInfo_rollbackInfo,
    deploymentInfo_externalId,
    deploymentInfo_status,
    deploymentInfo_description,
    deploymentInfo_computePlatform,
    deploymentInfo_updateOutdatedInstancesOnly,
    deploymentInfo_completeTime,
    deploymentInfo_deploymentStatusMessages,
    deploymentInfo_autoRollbackConfiguration,
    deploymentInfo_deploymentStyle,
    deploymentInfo_blueGreenDeploymentConfiguration,
    deploymentInfo_errorInformation,
    deploymentInfo_relatedDeployments,
    deploymentInfo_targetInstances,
    deploymentInfo_createTime,
    deploymentInfo_additionalDeploymentStatusInfo,
    deploymentInfo_creator,
    deploymentInfo_previousRevision,
    deploymentInfo_startTime,
    deploymentInfo_ignoreApplicationStopFailures,
    deploymentInfo_instanceTerminationWaitTimeStarted,
    deploymentInfo_deploymentConfigName,
    deploymentInfo_applicationName,

    -- * DeploymentOverview
    DeploymentOverview (..),
    newDeploymentOverview,
    deploymentOverview_failed,
    deploymentOverview_succeeded,
    deploymentOverview_ready,
    deploymentOverview_skipped,
    deploymentOverview_pending,
    deploymentOverview_inProgress,

    -- * DeploymentReadyOption
    DeploymentReadyOption (..),
    newDeploymentReadyOption,
    deploymentReadyOption_waitTimeInMinutes,
    deploymentReadyOption_actionOnTimeout,

    -- * DeploymentStyle
    DeploymentStyle (..),
    newDeploymentStyle,
    deploymentStyle_deploymentType,
    deploymentStyle_deploymentOption,

    -- * DeploymentTarget
    DeploymentTarget (..),
    newDeploymentTarget,
    deploymentTarget_instanceTarget,
    deploymentTarget_ecsTarget,
    deploymentTarget_deploymentTargetType,
    deploymentTarget_lambdaTarget,
    deploymentTarget_cloudFormationTarget,

    -- * Diagnostics
    Diagnostics (..),
    newDiagnostics,
    diagnostics_message,
    diagnostics_logTail,
    diagnostics_errorCode,
    diagnostics_scriptName,

    -- * EC2TagFilter
    EC2TagFilter (..),
    newEC2TagFilter,
    eC2TagFilter_key,
    eC2TagFilter_type,
    eC2TagFilter_value,

    -- * EC2TagSet
    EC2TagSet (..),
    newEC2TagSet,
    eC2TagSet_ec2TagSetList,

    -- * ECSService
    ECSService (..),
    newECSService,
    eCSService_serviceName,
    eCSService_clusterName,

    -- * ECSTarget
    ECSTarget (..),
    newECSTarget,
    eCSTarget_targetId,
    eCSTarget_lifecycleEvents,
    eCSTarget_lastUpdatedAt,
    eCSTarget_deploymentId,
    eCSTarget_targetArn,
    eCSTarget_status,
    eCSTarget_taskSetsInfo,

    -- * ECSTaskSet
    ECSTaskSet (..),
    newECSTaskSet,
    eCSTaskSet_identifer,
    eCSTaskSet_status,
    eCSTaskSet_targetGroup,
    eCSTaskSet_desiredCount,
    eCSTaskSet_pendingCount,
    eCSTaskSet_runningCount,
    eCSTaskSet_taskSetLabel,
    eCSTaskSet_trafficWeight,

    -- * ELBInfo
    ELBInfo (..),
    newELBInfo,
    eLBInfo_name,

    -- * ErrorInformation
    ErrorInformation (..),
    newErrorInformation,
    errorInformation_message,
    errorInformation_code,

    -- * GenericRevisionInfo
    GenericRevisionInfo (..),
    newGenericRevisionInfo,
    genericRevisionInfo_deploymentGroups,
    genericRevisionInfo_registerTime,
    genericRevisionInfo_description,
    genericRevisionInfo_firstUsedTime,
    genericRevisionInfo_lastUsedTime,

    -- * GitHubLocation
    GitHubLocation (..),
    newGitHubLocation,
    gitHubLocation_commitId,
    gitHubLocation_repository,

    -- * GreenFleetProvisioningOption
    GreenFleetProvisioningOption (..),
    newGreenFleetProvisioningOption,
    greenFleetProvisioningOption_action,

    -- * InstanceInfo
    InstanceInfo (..),
    newInstanceInfo,
    instanceInfo_tags,
    instanceInfo_iamSessionArn,
    instanceInfo_deregisterTime,
    instanceInfo_iamUserArn,
    instanceInfo_instanceName,
    instanceInfo_registerTime,
    instanceInfo_instanceArn,

    -- * InstanceTarget
    InstanceTarget (..),
    newInstanceTarget,
    instanceTarget_targetId,
    instanceTarget_instanceLabel,
    instanceTarget_lifecycleEvents,
    instanceTarget_lastUpdatedAt,
    instanceTarget_deploymentId,
    instanceTarget_targetArn,
    instanceTarget_status,

    -- * LambdaFunctionInfo
    LambdaFunctionInfo (..),
    newLambdaFunctionInfo,
    lambdaFunctionInfo_targetVersionWeight,
    lambdaFunctionInfo_functionName,
    lambdaFunctionInfo_targetVersion,
    lambdaFunctionInfo_currentVersion,
    lambdaFunctionInfo_functionAlias,

    -- * LambdaTarget
    LambdaTarget (..),
    newLambdaTarget,
    lambdaTarget_targetId,
    lambdaTarget_lifecycleEvents,
    lambdaTarget_lambdaFunctionInfo,
    lambdaTarget_lastUpdatedAt,
    lambdaTarget_deploymentId,
    lambdaTarget_targetArn,
    lambdaTarget_status,

    -- * LastDeploymentInfo
    LastDeploymentInfo (..),
    newLastDeploymentInfo,
    lastDeploymentInfo_deploymentId,
    lastDeploymentInfo_status,
    lastDeploymentInfo_endTime,
    lastDeploymentInfo_createTime,

    -- * LifecycleEvent
    LifecycleEvent (..),
    newLifecycleEvent,
    lifecycleEvent_lifecycleEventName,
    lifecycleEvent_status,
    lifecycleEvent_endTime,
    lifecycleEvent_diagnostics,
    lifecycleEvent_startTime,

    -- * LoadBalancerInfo
    LoadBalancerInfo (..),
    newLoadBalancerInfo,
    loadBalancerInfo_targetGroupPairInfoList,
    loadBalancerInfo_elbInfoList,
    loadBalancerInfo_targetGroupInfoList,

    -- * MinimumHealthyHosts
    MinimumHealthyHosts (..),
    newMinimumHealthyHosts,
    minimumHealthyHosts_type,
    minimumHealthyHosts_value,

    -- * OnPremisesTagSet
    OnPremisesTagSet (..),
    newOnPremisesTagSet,
    onPremisesTagSet_onPremisesTagSetList,

    -- * RawString
    RawString (..),
    newRawString,
    rawString_sha256,
    rawString_content,

    -- * RelatedDeployments
    RelatedDeployments (..),
    newRelatedDeployments,
    relatedDeployments_autoUpdateOutdatedInstancesDeploymentIds,
    relatedDeployments_autoUpdateOutdatedInstancesRootDeploymentId,

    -- * RevisionInfo
    RevisionInfo (..),
    newRevisionInfo,
    revisionInfo_revisionLocation,
    revisionInfo_genericRevisionInfo,

    -- * RevisionLocation
    RevisionLocation (..),
    newRevisionLocation,
    revisionLocation_string,
    revisionLocation_appSpecContent,
    revisionLocation_s3Location,
    revisionLocation_gitHubLocation,
    revisionLocation_revisionType,

    -- * RollbackInfo
    RollbackInfo (..),
    newRollbackInfo,
    rollbackInfo_rollbackMessage,
    rollbackInfo_rollbackDeploymentId,
    rollbackInfo_rollbackTriggeringDeploymentId,

    -- * S3Location
    S3Location (..),
    newS3Location,
    s3Location_key,
    s3Location_bundleType,
    s3Location_bucket,
    s3Location_eTag,
    s3Location_version,

    -- * Tag
    Tag (..),
    newTag,
    tag_key,
    tag_value,

    -- * TagFilter
    TagFilter (..),
    newTagFilter,
    tagFilter_key,
    tagFilter_type,
    tagFilter_value,

    -- * TargetGroupInfo
    TargetGroupInfo (..),
    newTargetGroupInfo,
    targetGroupInfo_name,

    -- * TargetGroupPairInfo
    TargetGroupPairInfo (..),
    newTargetGroupPairInfo,
    targetGroupPairInfo_testTrafficRoute,
    targetGroupPairInfo_prodTrafficRoute,
    targetGroupPairInfo_targetGroups,

    -- * TargetInstances
    TargetInstances (..),
    newTargetInstances,
    targetInstances_autoScalingGroups,
    targetInstances_tagFilters,
    targetInstances_ec2TagSet,

    -- * TimeBasedCanary
    TimeBasedCanary (..),
    newTimeBasedCanary,
    timeBasedCanary_canaryPercentage,
    timeBasedCanary_canaryInterval,

    -- * TimeBasedLinear
    TimeBasedLinear (..),
    newTimeBasedLinear,
    timeBasedLinear_linearInterval,
    timeBasedLinear_linearPercentage,

    -- * TimeRange
    TimeRange (..),
    newTimeRange,
    timeRange_start,
    timeRange_end,

    -- * TrafficRoute
    TrafficRoute (..),
    newTrafficRoute,
    trafficRoute_listenerArns,

    -- * TrafficRoutingConfig
    TrafficRoutingConfig (..),
    newTrafficRoutingConfig,
    trafficRoutingConfig_type,
    trafficRoutingConfig_timeBasedLinear,
    trafficRoutingConfig_timeBasedCanary,

    -- * TriggerConfig
    TriggerConfig (..),
    newTriggerConfig,
    triggerConfig_triggerName,
    triggerConfig_triggerEvents,
    triggerConfig_triggerTargetArn,
  )
where

import Amazonka.CodeDeploy.Types.Alarm
import Amazonka.CodeDeploy.Types.AlarmConfiguration
import Amazonka.CodeDeploy.Types.AppSpecContent
import Amazonka.CodeDeploy.Types.ApplicationInfo
import Amazonka.CodeDeploy.Types.ApplicationRevisionSortBy
import Amazonka.CodeDeploy.Types.AutoRollbackConfiguration
import Amazonka.CodeDeploy.Types.AutoRollbackEvent
import Amazonka.CodeDeploy.Types.AutoScalingGroup
import Amazonka.CodeDeploy.Types.BlueGreenDeploymentConfiguration
import Amazonka.CodeDeploy.Types.BlueInstanceTerminationOption
import Amazonka.CodeDeploy.Types.BundleType
import Amazonka.CodeDeploy.Types.CloudFormationTarget
import Amazonka.CodeDeploy.Types.ComputePlatform
import Amazonka.CodeDeploy.Types.DeployErrorCode
import Amazonka.CodeDeploy.Types.DeploymentConfigInfo
import Amazonka.CodeDeploy.Types.DeploymentCreator
import Amazonka.CodeDeploy.Types.DeploymentGroupInfo
import Amazonka.CodeDeploy.Types.DeploymentInfo
import Amazonka.CodeDeploy.Types.DeploymentOption
import Amazonka.CodeDeploy.Types.DeploymentOverview
import Amazonka.CodeDeploy.Types.DeploymentReadyAction
import Amazonka.CodeDeploy.Types.DeploymentReadyOption
import Amazonka.CodeDeploy.Types.DeploymentStatus
import Amazonka.CodeDeploy.Types.DeploymentStyle
import Amazonka.CodeDeploy.Types.DeploymentTarget
import Amazonka.CodeDeploy.Types.DeploymentTargetType
import Amazonka.CodeDeploy.Types.DeploymentType
import Amazonka.CodeDeploy.Types.DeploymentWaitType
import Amazonka.CodeDeploy.Types.Diagnostics
import Amazonka.CodeDeploy.Types.EC2TagFilter
import Amazonka.CodeDeploy.Types.EC2TagFilterType
import Amazonka.CodeDeploy.Types.EC2TagSet
import Amazonka.CodeDeploy.Types.ECSService
import Amazonka.CodeDeploy.Types.ECSTarget
import Amazonka.CodeDeploy.Types.ECSTaskSet
import Amazonka.CodeDeploy.Types.ELBInfo
import Amazonka.CodeDeploy.Types.ErrorInformation
import Amazonka.CodeDeploy.Types.FileExistsBehavior
import Amazonka.CodeDeploy.Types.GenericRevisionInfo
import Amazonka.CodeDeploy.Types.GitHubLocation
import Amazonka.CodeDeploy.Types.GreenFleetProvisioningAction
import Amazonka.CodeDeploy.Types.GreenFleetProvisioningOption
import Amazonka.CodeDeploy.Types.InstanceAction
import Amazonka.CodeDeploy.Types.InstanceInfo
import Amazonka.CodeDeploy.Types.InstanceTarget
import Amazonka.CodeDeploy.Types.LambdaFunctionInfo
import Amazonka.CodeDeploy.Types.LambdaTarget
import Amazonka.CodeDeploy.Types.LastDeploymentInfo
import Amazonka.CodeDeploy.Types.LifecycleErrorCode
import Amazonka.CodeDeploy.Types.LifecycleEvent
import Amazonka.CodeDeploy.Types.LifecycleEventStatus
import Amazonka.CodeDeploy.Types.ListStateFilterAction
import Amazonka.CodeDeploy.Types.LoadBalancerInfo
import Amazonka.CodeDeploy.Types.MinimumHealthyHosts
import Amazonka.CodeDeploy.Types.MinimumHealthyHostsType
import Amazonka.CodeDeploy.Types.OnPremisesTagSet
import Amazonka.CodeDeploy.Types.OutdatedInstancesStrategy
import Amazonka.CodeDeploy.Types.RawString
import Amazonka.CodeDeploy.Types.RegistrationStatus
import Amazonka.CodeDeploy.Types.RelatedDeployments
import Amazonka.CodeDeploy.Types.RevisionInfo
import Amazonka.CodeDeploy.Types.RevisionLocation
import Amazonka.CodeDeploy.Types.RevisionLocationType
import Amazonka.CodeDeploy.Types.RollbackInfo
import Amazonka.CodeDeploy.Types.S3Location
import Amazonka.CodeDeploy.Types.SortOrder
import Amazonka.CodeDeploy.Types.StopStatus
import Amazonka.CodeDeploy.Types.Tag
import Amazonka.CodeDeploy.Types.TagFilter
import Amazonka.CodeDeploy.Types.TagFilterType
import Amazonka.CodeDeploy.Types.TargetFilterName
import Amazonka.CodeDeploy.Types.TargetGroupInfo
import Amazonka.CodeDeploy.Types.TargetGroupPairInfo
import Amazonka.CodeDeploy.Types.TargetInstances
import Amazonka.CodeDeploy.Types.TargetLabel
import Amazonka.CodeDeploy.Types.TargetStatus
import Amazonka.CodeDeploy.Types.TimeBasedCanary
import Amazonka.CodeDeploy.Types.TimeBasedLinear
import Amazonka.CodeDeploy.Types.TimeRange
import Amazonka.CodeDeploy.Types.TrafficRoute
import Amazonka.CodeDeploy.Types.TrafficRoutingConfig
import Amazonka.CodeDeploy.Types.TrafficRoutingType
import Amazonka.CodeDeploy.Types.TriggerConfig
import Amazonka.CodeDeploy.Types.TriggerEventType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2014-10-06@ of the Amazon CodeDeploy SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "CodeDeploy",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "codedeploy",
      Core.signingName = "codedeploy",
      Core.version = "2014-10-06",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error = Core.parseJSONError "CodeDeploy",
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
      | Lens.has (Core.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Lens.has (Core.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Core.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has (Core.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Prelude.otherwise = Prelude.Nothing

-- | The maximum number of alarms for a deployment group (10) was exceeded.
_AlarmsLimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AlarmsLimitExceededException =
  Core._MatchServiceError
    defaultService
    "AlarmsLimitExceededException"

-- | An invalid deployment style was specified. Valid deployment types
-- include \"IN_PLACE\" and \"BLUE_GREEN.\" Valid deployment options
-- include \"WITH_TRAFFIC_CONTROL\" and \"WITHOUT_TRAFFIC_CONTROL.\"
_InvalidDeploymentStyleException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidDeploymentStyleException =
  Core._MatchServiceError
    defaultService
    "InvalidDeploymentStyleException"

-- | The IAM user ARN was specified in an invalid format.
_InvalidIamUserArnException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidIamUserArnException =
  Core._MatchServiceError
    defaultService
    "InvalidIamUserArnException"

-- | The deployment does not have a status of Ready and can\'t continue yet.
_DeploymentIsNotInReadyStateException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DeploymentIsNotInReadyStateException =
  Core._MatchServiceError
    defaultService
    "DeploymentIsNotInReadyStateException"

-- | The deployment configuration name was specified in an invalid format.
_InvalidDeploymentConfigNameException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidDeploymentConfigNameException =
  Core._MatchServiceError
    defaultService
    "InvalidDeploymentConfigNameException"

-- | The wait type is invalid.
_InvalidDeploymentWaitTypeException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidDeploymentWaitTypeException =
  Core._MatchServiceError
    defaultService
    "InvalidDeploymentWaitTypeException"

-- | The application does not exist with the IAM user or Amazon Web Services
-- account.
_ApplicationDoesNotExistException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ApplicationDoesNotExistException =
  Core._MatchServiceError
    defaultService
    "ApplicationDoesNotExistException"

-- | The application name was specified in an invalid format.
_InvalidApplicationNameException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidApplicationNameException =
  Core._MatchServiceError
    defaultService
    "InvalidApplicationNameException"

-- | The format of the alarm configuration is invalid. Possible causes
-- include:
--
-- -   The alarm list is null.
--
-- -   The alarm object is null.
--
-- -   The alarm name is empty or null or exceeds the limit of 255
--     characters.
--
-- -   Two alarms with the same name have been specified.
--
-- -   The alarm configuration is enabled, but the alarm list is empty.
_InvalidAlarmConfigException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidAlarmConfigException =
  Core._MatchServiceError
    defaultService
    "InvalidAlarmConfigException"

-- | An IAM user ARN was not specified.
_IamUserArnRequiredException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_IamUserArnRequiredException =
  Core._MatchServiceError
    defaultService
    "IamUserArnRequiredException"

-- | The number of tag groups included in the tag set list exceeded the
-- maximum allowed limit of 3.
_TagSetListLimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TagSetListLimitExceededException =
  Core._MatchServiceError
    defaultService
    "TagSetListLimitExceededException"

-- | The input was specified in an invalid format.
_InvalidInputException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidInputException =
  Core._MatchServiceError
    defaultService
    "InvalidInputException"

-- | The minimum number of required application names was not specified.
_ApplicationNameRequiredException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ApplicationNameRequiredException =
  Core._MatchServiceError
    defaultService
    "ApplicationNameRequiredException"

-- | A lifecycle event hook is invalid. Review the @hooks@ section in your
-- AppSpec file to ensure the lifecycle events and @hooks@ functions are
-- valid.
_InvalidLifecycleEventHookExecutionIdException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidLifecycleEventHookExecutionIdException =
  Core._MatchServiceError
    defaultService
    "InvalidLifecycleEventHookExecutionIdException"

-- | The minimum healthy instance value was specified in an invalid format.
_InvalidMinimumHealthyHostValueException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidMinimumHealthyHostValueException =
  Core._MatchServiceError
    defaultService
    "InvalidMinimumHealthyHostValueException"

-- | The specified deployment status doesn\'t exist or cannot be determined.
_InvalidDeploymentStatusException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidDeploymentStatusException =
  Core._MatchServiceError
    defaultService
    "InvalidDeploymentStatusException"

-- | Both an IAM user ARN and an IAM session ARN were included in the
-- request. Use only one ARN type.
_MultipleIamArnsProvidedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_MultipleIamArnsProvidedException =
  Core._MatchServiceError
    defaultService
    "MultipleIamArnsProvidedException"

-- | An invalid operation was detected.
_InvalidOperationException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidOperationException =
  Core._MatchServiceError
    defaultService
    "InvalidOperationException"

-- | The UpdateOutdatedInstancesOnly value is invalid. For Lambda
-- deployments, @false@ is expected. For EC2\/On-premises deployments,
-- @true@ or @false@ is expected.
_InvalidUpdateOutdatedInstancesOnlyValueException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidUpdateOutdatedInstancesOnlyValueException =
  Core._MatchServiceError
    defaultService
    "InvalidUpdateOutdatedInstancesOnlyValueException"

-- | The deployment groups limit was exceeded.
_DeploymentGroupLimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DeploymentGroupLimitExceededException =
  Core._MatchServiceError
    defaultService
    "DeploymentGroupLimitExceededException"

-- | The Amazon ECS service identifier is not valid.
_InvalidECSServiceException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidECSServiceException =
  Core._MatchServiceError
    defaultService
    "InvalidECSServiceException"

-- | The specified ARN is not supported. For example, it might be an ARN for
-- a resource that is not expected.
_ArnNotSupportedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ArnNotSupportedException =
  Core._MatchServiceError
    defaultService
    "ArnNotSupportedException"

-- | The specified deployment has not started.
_DeploymentNotStartedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DeploymentNotStartedException =
  Core._MatchServiceError
    defaultService
    "DeploymentNotStartedException"

-- | The computePlatform is invalid. The computePlatform should be @Lambda@,
-- @Server@, or @ECS@.
_InvalidComputePlatformException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidComputePlatformException =
  Core._MatchServiceError
    defaultService
    "InvalidComputePlatformException"

-- | A call was submitted that specified both OnPremisesTagFilters and
-- OnPremisesTagSet, but only one of these data types can be used in a
-- single call.
_InvalidOnPremisesTagCombinationException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidOnPremisesTagCombinationException =
  Core._MatchServiceError
    defaultService
    "InvalidOnPremisesTagCombinationException"

-- | The revision ID was not specified.
_RevisionRequiredException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_RevisionRequiredException =
  Core._MatchServiceError
    defaultService
    "RevisionRequiredException"

-- | A deployment target ID was not provided.
_DeploymentTargetIdRequiredException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DeploymentTargetIdRequiredException =
  Core._MatchServiceError
    defaultService
    "DeploymentTargetIdRequiredException"

-- | The specified resource could not be validated.
_ResourceValidationException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceValidationException =
  Core._MatchServiceError
    defaultService
    "ResourceValidationException"

-- | A tag was not specified.
_TagRequiredException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TagRequiredException =
  Core._MatchServiceError
    defaultService
    "TagRequiredException"

-- | The Amazon ECS service is associated with more than one deployment
-- groups. An Amazon ECS service can be associated with only one deployment
-- group.
_ECSServiceMappingLimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ECSServiceMappingLimitExceededException =
  Core._MatchServiceError
    defaultService
    "ECSServiceMappingLimitExceededException"

-- | The ARN of a resource is required, but was not found.
_ResourceArnRequiredException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceArnRequiredException =
  Core._MatchServiceError
    defaultService
    "ResourceArnRequiredException"

-- | The limit for lifecycle hooks was exceeded.
_LifecycleHookLimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_LifecycleHookLimitExceededException =
  Core._MatchServiceError
    defaultService
    "LifecycleHookLimitExceededException"

-- | The specified on-premises instance is not registered.
_InstanceNotRegisteredException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InstanceNotRegisteredException =
  Core._MatchServiceError
    defaultService
    "InstanceNotRegisteredException"

-- | A deployment to a target was attempted while another deployment was in
-- progress.
_DeploymentAlreadyStartedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DeploymentAlreadyStartedException =
  Core._MatchServiceError
    defaultService
    "DeploymentAlreadyStartedException"

-- | The service role ARN was specified in an invalid format. Or, if an Auto
-- Scaling group was specified, the specified service role does not grant
-- the appropriate permissions to Amazon EC2 Auto Scaling.
_InvalidRoleException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidRoleException =
  Core._MatchServiceError
    defaultService
    "InvalidRoleException"

-- | An invalid fileExistsBehavior option was specified to determine how
-- CodeDeploy handles files or directories that already exist in a
-- deployment target location, but weren\'t part of the previous successful
-- deployment. Valid values include \"DISALLOW,\" \"OVERWRITE,\" and
-- \"RETAIN.\"
_InvalidFileExistsBehaviorException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidFileExistsBehaviorException =
  Core._MatchServiceError
    defaultService
    "InvalidFileExistsBehaviorException"

-- | The specified instance does not exist in the deployment group.
_InstanceDoesNotExistException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InstanceDoesNotExistException =
  Core._MatchServiceError
    defaultService
    "InstanceDoesNotExistException"

-- | The deployment configuration is still in use.
_DeploymentConfigInUseException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DeploymentConfigInUseException =
  Core._MatchServiceError
    defaultService
    "DeploymentConfigInUseException"

-- | An attempt to return the status of an already completed lifecycle event
-- occurred.
_LifecycleEventAlreadyCompletedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_LifecycleEventAlreadyCompletedException =
  Core._MatchServiceError
    defaultService
    "LifecycleEventAlreadyCompletedException"

-- | No IAM ARN was included in the request. You must use an IAM session ARN
-- or IAM user ARN in the request.
_IamArnRequiredException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_IamArnRequiredException =
  Core._MatchServiceError
    defaultService
    "IamArnRequiredException"

-- | An on-premises instance name was not specified.
_InstanceNameRequiredException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InstanceNameRequiredException =
  Core._MatchServiceError
    defaultService
    "InstanceNameRequiredException"

-- | The specified IAM user ARN is already registered with an on-premises
-- instance.
_IamUserArnAlreadyRegisteredException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_IamUserArnAlreadyRegisteredException =
  Core._MatchServiceError
    defaultService
    "IamUserArnAlreadyRegisteredException"

-- | The maximum number of targets that can be associated with an Amazon ECS
-- or Lambda deployment was exceeded. The target list of both types of
-- deployments must have exactly one item. This exception does not apply to
-- EC2\/On-premises deployments.
_DeploymentTargetListSizeExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DeploymentTargetListSizeExceededException =
  Core._MatchServiceError
    defaultService
    "DeploymentTargetListSizeExceededException"

-- | The specified key prefix filter was specified in an invalid format.
_InvalidKeyPrefixFilterException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidKeyPrefixFilterException =
  Core._MatchServiceError
    defaultService
    "InvalidKeyPrefixFilterException"

-- | The role ID was not specified.
_RoleRequiredException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_RoleRequiredException =
  Core._MatchServiceError
    defaultService
    "RoleRequiredException"

-- | The specified tags are not valid.
_InvalidTagsToAddException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidTagsToAddException =
  Core._MatchServiceError
    defaultService
    "InvalidTagsToAddException"

-- | The deployed state filter was specified in an invalid format.
_InvalidDeployedStateFilterException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidDeployedStateFilterException =
  Core._MatchServiceError
    defaultService
    "InvalidDeployedStateFilterException"

-- | The request included an IAM session ARN that has already been used to
-- register a different instance.
_IamSessionArnAlreadyRegisteredException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_IamSessionArnAlreadyRegisteredException =
  Core._MatchServiceError
    defaultService
    "IamSessionArnAlreadyRegisteredException"

-- | The automatic rollback configuration was specified in an invalid format.
-- For example, automatic rollback is enabled, but an invalid triggering
-- event type or no event types were listed.
_InvalidAutoRollbackConfigException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidAutoRollbackConfigException =
  Core._MatchServiceError
    defaultService
    "InvalidAutoRollbackConfigException"

-- | The provided target ID does not belong to the attempted deployment.
_DeploymentTargetDoesNotExistException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DeploymentTargetDoesNotExistException =
  Core._MatchServiceError
    defaultService
    "DeploymentTargetDoesNotExistException"

-- | The bucket name either doesn\'t exist or was specified in an invalid
-- format.
_InvalidBucketNameFilterException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidBucketNameFilterException =
  Core._MatchServiceError
    defaultService
    "InvalidBucketNameFilterException"

-- | The number of allowed deployments was exceeded.
_DeploymentLimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DeploymentLimitExceededException =
  Core._MatchServiceError
    defaultService
    "DeploymentLimitExceededException"

-- | A bucket name is required, but was not provided.
_BucketNameFilterRequiredException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_BucketNameFilterRequiredException =
  Core._MatchServiceError
    defaultService
    "BucketNameFilterRequiredException"

-- | The target filter name is invalid.
_InvalidTargetFilterNameException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidTargetFilterNameException =
  Core._MatchServiceError
    defaultService
    "InvalidTargetFilterNameException"

-- | The sort order was specified in an invalid format.
_InvalidSortOrderException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidSortOrderException =
  Core._MatchServiceError
    defaultService
    "InvalidSortOrderException"

-- | The next token was specified in an invalid format.
_InvalidNextTokenException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidNextTokenException =
  Core._MatchServiceError
    defaultService
    "InvalidNextTokenException"

-- | The maximum number of allowed on-premises instances in a single call was
-- exceeded.
_InstanceLimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InstanceLimitExceededException =
  Core._MatchServiceError
    defaultService
    "InstanceLimitExceededException"

-- | The IgnoreApplicationStopFailures value is invalid. For Lambda
-- deployments, @false@ is expected. For EC2\/On-premises deployments,
-- @true@ or @false@ is expected.
_InvalidIgnoreApplicationStopFailuresValueException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidIgnoreApplicationStopFailuresValueException =
  Core._MatchServiceError
    defaultService
    "InvalidIgnoreApplicationStopFailuresValueException"

-- | The result of a Lambda validation function that verifies a lifecycle
-- event is invalid. It should return @Succeeded@ or @Failed@.
_InvalidLifecycleEventHookExecutionStatusException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidLifecycleEventHookExecutionStatusException =
  Core._MatchServiceError
    defaultService
    "InvalidLifecycleEventHookExecutionStatusException"

-- | The configuration that specifies how traffic is routed during a
-- deployment is invalid.
_InvalidTrafficRoutingConfigurationException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidTrafficRoutingConfigurationException =
  Core._MatchServiceError
    defaultService
    "InvalidTrafficRoutingConfigurationException"

-- | The tag was specified in an invalid format.
_InvalidEC2TagException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidEC2TagException =
  Core._MatchServiceError
    defaultService
    "InvalidEC2TagException"

-- | A call was submitted that specified both Ec2TagFilters and Ec2TagSet,
-- but only one of these data types can be used in a single call.
_InvalidEC2TagCombinationException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidEC2TagCombinationException =
  Core._MatchServiceError
    defaultService
    "InvalidEC2TagCombinationException"

-- | The named deployment group with the IAM user or Amazon Web Services
-- account does not exist.
_DeploymentGroupDoesNotExistException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DeploymentGroupDoesNotExistException =
  Core._MatchServiceError
    defaultService
    "DeploymentGroupDoesNotExistException"

-- | The description is too long.
_DescriptionTooLongException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DescriptionTooLongException =
  Core._MatchServiceError
    defaultService
    "DescriptionTooLongException"

-- | The IAM session ARN was specified in an invalid format.
_InvalidIamSessionArnException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidIamSessionArnException =
  Core._MatchServiceError
    defaultService
    "InvalidIamSessionArnException"

-- | An API function was called too frequently.
_ThrottlingException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ThrottlingException =
  Core._MatchServiceError
    defaultService
    "ThrottlingException"

-- | The tag filter was specified in an invalid format.
_InvalidTagFilterException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidTagFilterException =
  Core._MatchServiceError
    defaultService
    "InvalidTagFilterException"

-- | An invalid instance type was specified for instances in a blue\/green
-- deployment. Valid values include \"Blue\" for an original environment
-- and \"Green\" for a replacement environment.
_InvalidInstanceTypeException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidInstanceTypeException =
  Core._MatchServiceError
    defaultService
    "InvalidInstanceTypeException"

-- | The deployment group name was not specified.
_DeploymentGroupNameRequiredException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DeploymentGroupNameRequiredException =
  Core._MatchServiceError
    defaultService
    "DeploymentGroupNameRequiredException"

-- | The deployment is already complete.
_DeploymentAlreadyCompletedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DeploymentAlreadyCompletedException =
  Core._MatchServiceError
    defaultService
    "DeploymentAlreadyCompletedException"

-- | The maximum allowed number of tags was exceeded.
_TagLimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TagLimitExceededException =
  Core._MatchServiceError
    defaultService
    "TagLimitExceededException"

-- | A deployment group with the specified name with the IAM user or Amazon
-- Web Services account already exists.
_DeploymentGroupAlreadyExistsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DeploymentGroupAlreadyExistsException =
  Core._MatchServiceError
    defaultService
    "DeploymentGroupAlreadyExistsException"

-- | The external ID was specified in an invalid format.
_InvalidExternalIdException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidExternalIdException =
  Core._MatchServiceError
    defaultService
    "InvalidExternalIdException"

-- | The deployment configuration does not exist with the IAM user or Amazon
-- Web Services account.
_DeploymentConfigDoesNotExistException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DeploymentConfigDoesNotExistException =
  Core._MatchServiceError
    defaultService
    "DeploymentConfigDoesNotExistException"

-- | The column name to sort by is either not present or was specified in an
-- invalid format.
_InvalidSortByException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidSortByException =
  Core._MatchServiceError
    defaultService
    "InvalidSortByException"

-- | The target ID provided was not valid.
_InvalidDeploymentTargetIdException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidDeploymentTargetIdException =
  Core._MatchServiceError
    defaultService
    "InvalidDeploymentTargetIdException"

-- | A target is not valid.
_InvalidTargetException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidTargetException =
  Core._MatchServiceError
    defaultService
    "InvalidTargetException"

-- | The registration status was specified in an invalid format.
_InvalidRegistrationStatusException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidRegistrationStatusException =
  Core._MatchServiceError
    defaultService
    "InvalidRegistrationStatusException"

-- | The deployment configuration name was not specified.
_DeploymentConfigNameRequiredException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DeploymentConfigNameRequiredException =
  Core._MatchServiceError
    defaultService
    "DeploymentConfigNameRequiredException"

-- | The specified ARN is not in a valid format.
_InvalidArnException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidArnException =
  Core._MatchServiceError
    defaultService
    "InvalidArnException"

-- | A target group pair associated with this deployment is not valid.
_InvalidTargetGroupPairException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidTargetGroupPairException =
  Core._MatchServiceError
    defaultService
    "InvalidTargetGroupPairException"

-- | The trigger was specified in an invalid format.
_InvalidTriggerConfigException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidTriggerConfigException =
  Core._MatchServiceError
    defaultService
    "InvalidTriggerConfigException"

-- | A call was submitted that is not supported for the specified deployment
-- type.
_UnsupportedActionForDeploymentTypeException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_UnsupportedActionForDeploymentTypeException =
  Core._MatchServiceError
    defaultService
    "UnsupportedActionForDeploymentTypeException"

-- | The target instance configuration is invalid. Possible causes include:
--
-- -   Configuration data for target instances was entered for an in-place
--     deployment.
--
-- -   The limit of 10 tags for a tag type was exceeded.
--
-- -   The combined length of the tag names exceeded the limit.
--
-- -   A specified tag is not currently applied to any instances.
_InvalidTargetInstancesException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidTargetInstancesException =
  Core._MatchServiceError
    defaultService
    "InvalidTargetInstancesException"

-- | The revision was specified in an invalid format.
_InvalidRevisionException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidRevisionException =
  Core._MatchServiceError
    defaultService
    "InvalidRevisionException"

-- | The tag was specified in an invalid format.
_InvalidTagException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidTagException =
  Core._MatchServiceError
    defaultService
    "InvalidTagException"

-- | The specified instance status does not exist.
_InvalidInstanceStatusException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidInstanceStatusException =
  Core._MatchServiceError
    defaultService
    "InvalidInstanceStatusException"

-- | The deployment group name was specified in an invalid format.
_InvalidDeploymentGroupNameException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidDeploymentGroupNameException =
  Core._MatchServiceError
    defaultService
    "InvalidDeploymentGroupNameException"

-- | No GitHub account connection exists with the named specified in the
-- call.
_GitHubAccountTokenDoesNotExistException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_GitHubAccountTokenDoesNotExistException =
  Core._MatchServiceError
    defaultService
    "GitHubAccountTokenDoesNotExistException"

-- | At least one of the deployment IDs was specified in an invalid format.
_InvalidDeploymentIdException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidDeploymentIdException =
  Core._MatchServiceError
    defaultService
    "InvalidDeploymentIdException"

-- | The call is missing a required GitHub account connection name.
_GitHubAccountTokenNameRequiredException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_GitHubAccountTokenNameRequiredException =
  Core._MatchServiceError
    defaultService
    "GitHubAccountTokenNameRequiredException"

-- | The specified time range was specified in an invalid format.
_InvalidTimeRangeException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidTimeRangeException =
  Core._MatchServiceError
    defaultService
    "InvalidTimeRangeException"

-- | The instance ID was not specified.
_InstanceIdRequiredException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InstanceIdRequiredException =
  Core._MatchServiceError
    defaultService
    "InstanceIdRequiredException"

-- | The GitHub token is not valid.
_InvalidGitHubAccountTokenException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidGitHubAccountTokenException =
  Core._MatchServiceError
    defaultService
    "InvalidGitHubAccountTokenException"

-- | The Auto Scaling group was specified in an invalid format or does not
-- exist.
_InvalidAutoScalingGroupException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidAutoScalingGroupException =
  Core._MatchServiceError
    defaultService
    "InvalidAutoScalingGroupException"

-- | The maximum number of names or IDs allowed for this request (100) was
-- exceeded.
_BatchLimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_BatchLimitExceededException =
  Core._MatchServiceError
    defaultService
    "BatchLimitExceededException"

-- | The deployment configurations limit was exceeded.
_DeploymentConfigLimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DeploymentConfigLimitExceededException =
  Core._MatchServiceError
    defaultService
    "DeploymentConfigLimitExceededException"

-- | More applications were attempted to be created than are allowed.
_ApplicationLimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ApplicationLimitExceededException =
  Core._MatchServiceError
    defaultService
    "ApplicationLimitExceededException"

-- | An application with the specified name with the IAM user or Amazon Web
-- Services account already exists.
_ApplicationAlreadyExistsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ApplicationAlreadyExistsException =
  Core._MatchServiceError
    defaultService
    "ApplicationAlreadyExistsException"

-- | The named revision does not exist with the IAM user or Amazon Web
-- Services account.
_RevisionDoesNotExistException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_RevisionDoesNotExistException =
  Core._MatchServiceError
    defaultService
    "RevisionDoesNotExistException"

-- | A deployment configuration with the specified name with the IAM user or
-- Amazon Web Services account already exists.
_DeploymentConfigAlreadyExistsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DeploymentConfigAlreadyExistsException =
  Core._MatchServiceError
    defaultService
    "DeploymentConfigAlreadyExistsException"

-- | The on-premises instance name was specified in an invalid format.
_InvalidInstanceNameException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidInstanceNameException =
  Core._MatchServiceError
    defaultService
    "InvalidInstanceNameException"

-- | At least one deployment ID must be specified.
_DeploymentIdRequiredException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DeploymentIdRequiredException =
  Core._MatchServiceError
    defaultService
    "DeploymentIdRequiredException"

-- | The specified on-premises instance name is already registered.
_InstanceNameAlreadyRegisteredException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InstanceNameAlreadyRegisteredException =
  Core._MatchServiceError
    defaultService
    "InstanceNameAlreadyRegisteredException"

-- | An invalid load balancer name, or no load balancer name, was specified.
_InvalidLoadBalancerInfoException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidLoadBalancerInfoException =
  Core._MatchServiceError
    defaultService
    "InvalidLoadBalancerInfoException"

-- | The configuration for the blue\/green deployment group was provided in
-- an invalid format. For information about deployment configuration
-- format, see CreateDeploymentConfig.
_InvalidBlueGreenDeploymentConfigurationException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidBlueGreenDeploymentConfigurationException =
  Core._MatchServiceError
    defaultService
    "InvalidBlueGreenDeploymentConfigurationException"

-- | The API used does not support the deployment.
_OperationNotSupportedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_OperationNotSupportedException =
  Core._MatchServiceError
    defaultService
    "OperationNotSupportedException"

-- | The maximum allowed number of triggers was exceeded.
_TriggerTargetsLimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TriggerTargetsLimitExceededException =
  Core._MatchServiceError
    defaultService
    "TriggerTargetsLimitExceededException"

-- | An instance type was specified for an in-place deployment. Instance
-- types are supported for blue\/green deployments only.
_InvalidDeploymentInstanceTypeException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidDeploymentInstanceTypeException =
  Core._MatchServiceError
    defaultService
    "InvalidDeploymentInstanceTypeException"

-- | The format of the specified GitHub account connection name is invalid.
_InvalidGitHubAccountTokenNameException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidGitHubAccountTokenNameException =
  Core._MatchServiceError
    defaultService
    "InvalidGitHubAccountTokenNameException"

-- | The deployment with the IAM user or Amazon Web Services account does not
-- exist.
_DeploymentDoesNotExistException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DeploymentDoesNotExistException =
  Core._MatchServiceError
    defaultService
    "DeploymentDoesNotExistException"

-- |
_InvalidInstanceIdException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidInstanceIdException =
  Core._MatchServiceError
    defaultService
    "InvalidInstanceIdException"
