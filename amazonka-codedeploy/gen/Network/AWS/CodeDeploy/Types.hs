{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeDeploy.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _DeploymentGroupLimitExceededException,
    _InstanceNameAlreadyRegisteredException,
    _DeploymentGroupAlreadyExistsException,
    _BucketNameFilterRequiredException,
    _RevisionDoesNotExistException,
    _DeploymentConfigDoesNotExistException,
    _InvalidInstanceTypeException,
    _InvalidIamUserArnException,
    _InvalidFileExistsBehaviorException,
    _InvalidTagFilterException,
    _DeploymentTargetDoesNotExistException,
    _InvalidTriggerConfigException,
    _AlarmsLimitExceededException,
    _DeploymentAlreadyStartedException,
    _InvalidKeyPrefixFilterException,
    _ArnNotSupportedException,
    _OperationNotSupportedException,
    _InvalidGitHubAccountTokenException,
    _InvalidExternalIdException,
    _InvalidMinimumHealthyHostValueException,
    _UnsupportedActionForDeploymentTypeException,
    _InvalidECSServiceException,
    _ResourceValidationException,
    _InvalidDeploymentInstanceTypeException,
    _InvalidGitHubAccountTokenNameException,
    _InvalidOperationException,
    _ApplicationAlreadyExistsException,
    _DeploymentTargetIdRequiredException,
    _InvalidArnException,
    _TagLimitExceededException,
    _InvalidAutoScalingGroupException,
    _ApplicationLimitExceededException,
    _InvalidInputException,
    _InstanceLimitExceededException,
    _IamUserArnAlreadyRegisteredException,
    _InstanceNameRequiredException,
    _DeploymentLimitExceededException,
    _InvalidTargetFilterNameException,
    _InvalidTargetInstancesException,
    _InvalidRevisionException,
    _InvalidTagsToAddException,
    _InvalidAutoRollbackConfigException,
    _DeploymentIdRequiredException,
    _InvalidRoleException,
    _DeploymentConfigAlreadyExistsException,
    _InvalidNextTokenException,
    _InstanceIdRequiredException,
    _InvalidBlueGreenDeploymentConfigurationException,
    _DeploymentConfigLimitExceededException,
    _InvalidLoadBalancerInfoException,
    _InvalidSortOrderException,
    _ThrottlingException,
    _InvalidTargetGroupPairException,
    _DeploymentConfigNameRequiredException,
    _InvalidOnPremisesTagCombinationException,
    _InvalidAlarmConfigException,
    _InvalidSortByException,
    _InvalidTrafficRoutingConfigurationException,
    _DescriptionTooLongException,
    _IamUserArnRequiredException,
    _IamSessionArnAlreadyRegisteredException,
    _InvalidDeploymentGroupNameException,
    _InvalidBucketNameFilterException,
    _InvalidTargetException,
    _DeploymentGroupNameRequiredException,
    _InvalidTimeRangeException,
    _TagRequiredException,
    _InvalidIgnoreApplicationStopFailuresValueException,
    _InvalidUpdateOutdatedInstancesOnlyValueException,
    _InvalidTagException,
    _InvalidDeploymentWaitTypeException,
    _InvalidComputePlatformException,
    _LifecycleHookLimitExceededException,
    _InvalidLifecycleEventHookExecutionStatusException,
    _ResourceArnRequiredException,
    _InvalidEC2TagCombinationException,
    _IamArnRequiredException,
    _GitHubAccountTokenNameRequiredException,
    _LifecycleEventAlreadyCompletedException,
    _ApplicationDoesNotExistException,
    _MultipleIamArnsProvidedException,
    _InvalidDeploymentTargetIdException,
    _InvalidRegistrationStatusException,
    _InstanceNotRegisteredException,
    _InvalidDeployedStateFilterException,
    _InvalidInstanceStatusException,
    _ApplicationNameRequiredException,
    _InvalidDeploymentStatusException,
    _TriggerTargetsLimitExceededException,
    _TagSetListLimitExceededException,
    _GitHubAccountTokenDoesNotExistException,
    _InvalidApplicationNameException,
    _DeploymentTargetListSizeExceededException,
    _DeploymentConfigInUseException,
    _InvalidInstanceNameException,
    _InvalidIamSessionArnException,
    _InvalidLifecycleEventHookExecutionIdException,
    _InvalidEC2TagException,
    _InvalidDeploymentConfigIdException,
    _InvalidDeploymentStyleException,
    _RevisionRequiredException,
    _InstanceDoesNotExistException,
    _DeploymentAlreadyCompletedException,
    _ECSServiceMappingLimitExceededException,
    _DeploymentDoesNotExistException,
    _BatchLimitExceededException,
    _InvalidDeploymentIdException,
    _InvalidDeploymentConfigNameException,
    _DeploymentNotStartedException,
    _DeploymentIsNotInReadyStateException,
    _InvalidInstanceIdException,
    _DeploymentGroupDoesNotExistException,
    _RoleRequiredException,

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
    alarmConfiguration_ignorePollAlarmFailure,
    alarmConfiguration_enabled,
    alarmConfiguration_alarms,

    -- * AppSpecContent
    AppSpecContent (..),
    newAppSpecContent,
    appSpecContent_content,
    appSpecContent_sha256,

    -- * ApplicationInfo
    ApplicationInfo (..),
    newApplicationInfo,
    applicationInfo_applicationId,
    applicationInfo_linkedToGitHub,
    applicationInfo_gitHubAccountName,
    applicationInfo_createTime,
    applicationInfo_applicationName,
    applicationInfo_computePlatform,

    -- * AutoRollbackConfiguration
    AutoRollbackConfiguration (..),
    newAutoRollbackConfiguration,
    autoRollbackConfiguration_enabled,
    autoRollbackConfiguration_events,

    -- * AutoScalingGroup
    AutoScalingGroup (..),
    newAutoScalingGroup,
    autoScalingGroup_hook,
    autoScalingGroup_name,

    -- * BlueGreenDeploymentConfiguration
    BlueGreenDeploymentConfiguration (..),
    newBlueGreenDeploymentConfiguration,
    blueGreenDeploymentConfiguration_greenFleetProvisioningOption,
    blueGreenDeploymentConfiguration_deploymentReadyOption,
    blueGreenDeploymentConfiguration_terminateBlueInstancesOnDeploymentSuccess,

    -- * BlueInstanceTerminationOption
    BlueInstanceTerminationOption (..),
    newBlueInstanceTerminationOption,
    blueInstanceTerminationOption_action,
    blueInstanceTerminationOption_terminationWaitTimeInMinutes,

    -- * CloudFormationTarget
    CloudFormationTarget (..),
    newCloudFormationTarget,
    cloudFormationTarget_deploymentId,
    cloudFormationTarget_status,
    cloudFormationTarget_targetId,
    cloudFormationTarget_targetVersionWeight,
    cloudFormationTarget_resourceType,
    cloudFormationTarget_lifecycleEvents,
    cloudFormationTarget_lastUpdatedAt,

    -- * DeploymentConfigInfo
    DeploymentConfigInfo (..),
    newDeploymentConfigInfo,
    deploymentConfigInfo_deploymentConfigName,
    deploymentConfigInfo_deploymentConfigId,
    deploymentConfigInfo_createTime,
    deploymentConfigInfo_trafficRoutingConfig,
    deploymentConfigInfo_minimumHealthyHosts,
    deploymentConfigInfo_computePlatform,

    -- * DeploymentGroupInfo
    DeploymentGroupInfo (..),
    newDeploymentGroupInfo,
    deploymentGroupInfo_onPremisesTagSet,
    deploymentGroupInfo_serviceRoleArn,
    deploymentGroupInfo_deploymentConfigName,
    deploymentGroupInfo_autoRollbackConfiguration,
    deploymentGroupInfo_deploymentGroupName,
    deploymentGroupInfo_triggerConfigurations,
    deploymentGroupInfo_deploymentGroupId,
    deploymentGroupInfo_ec2TagFilters,
    deploymentGroupInfo_targetRevision,
    deploymentGroupInfo_lastSuccessfulDeployment,
    deploymentGroupInfo_lastAttemptedDeployment,
    deploymentGroupInfo_onPremisesInstanceTagFilters,
    deploymentGroupInfo_loadBalancerInfo,
    deploymentGroupInfo_ec2TagSet,
    deploymentGroupInfo_blueGreenDeploymentConfiguration,
    deploymentGroupInfo_autoScalingGroups,
    deploymentGroupInfo_deploymentStyle,
    deploymentGroupInfo_alarmConfiguration,
    deploymentGroupInfo_ecsServices,
    deploymentGroupInfo_applicationName,
    deploymentGroupInfo_computePlatform,

    -- * DeploymentInfo
    DeploymentInfo (..),
    newDeploymentInfo,
    deploymentInfo_deploymentId,
    deploymentInfo_status,
    deploymentInfo_deploymentConfigName,
    deploymentInfo_ignoreApplicationStopFailures,
    deploymentInfo_updateOutdatedInstancesOnly,
    deploymentInfo_autoRollbackConfiguration,
    deploymentInfo_deploymentGroupName,
    deploymentInfo_targetInstances,
    deploymentInfo_startTime,
    deploymentInfo_instanceTerminationWaitTimeStarted,
    deploymentInfo_previousRevision,
    deploymentInfo_loadBalancerInfo,
    deploymentInfo_completeTime,
    deploymentInfo_errorInformation,
    deploymentInfo_blueGreenDeploymentConfiguration,
    deploymentInfo_creator,
    deploymentInfo_createTime,
    deploymentInfo_description,
    deploymentInfo_deploymentStyle,
    deploymentInfo_revision,
    deploymentInfo_rollbackInfo,
    deploymentInfo_externalId,
    deploymentInfo_applicationName,
    deploymentInfo_deploymentStatusMessages,
    deploymentInfo_fileExistsBehavior,
    deploymentInfo_additionalDeploymentStatusInfo,
    deploymentInfo_deploymentOverview,
    deploymentInfo_computePlatform,

    -- * DeploymentOverview
    DeploymentOverview (..),
    newDeploymentOverview,
    deploymentOverview_succeeded,
    deploymentOverview_ready,
    deploymentOverview_pending,
    deploymentOverview_failed,
    deploymentOverview_skipped,
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
    deploymentTarget_ecsTarget,
    deploymentTarget_lambdaTarget,
    deploymentTarget_cloudFormationTarget,
    deploymentTarget_instanceTarget,
    deploymentTarget_deploymentTargetType,

    -- * Diagnostics
    Diagnostics (..),
    newDiagnostics,
    diagnostics_logTail,
    diagnostics_message,
    diagnostics_scriptName,
    diagnostics_errorCode,

    -- * EC2TagFilter
    EC2TagFilter (..),
    newEC2TagFilter,
    eC2TagFilter_key,
    eC2TagFilter_value,
    eC2TagFilter_type,

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
    eCSTarget_deploymentId,
    eCSTarget_status,
    eCSTarget_targetId,
    eCSTarget_taskSetsInfo,
    eCSTarget_targetArn,
    eCSTarget_lifecycleEvents,
    eCSTarget_lastUpdatedAt,

    -- * ECSTaskSet
    ECSTaskSet (..),
    newECSTaskSet,
    eCSTaskSet_status,
    eCSTaskSet_runningCount,
    eCSTaskSet_desiredCount,
    eCSTaskSet_pendingCount,
    eCSTaskSet_taskSetLabel,
    eCSTaskSet_targetGroup,
    eCSTaskSet_trafficWeight,
    eCSTaskSet_identifer,

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
    genericRevisionInfo_registerTime,
    genericRevisionInfo_deploymentGroups,
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
    instanceInfo_registerTime,
    instanceInfo_iamUserArn,
    instanceInfo_instanceName,
    instanceInfo_instanceArn,
    instanceInfo_tags,
    instanceInfo_iamSessionArn,
    instanceInfo_deregisterTime,

    -- * InstanceTarget
    InstanceTarget (..),
    newInstanceTarget,
    instanceTarget_deploymentId,
    instanceTarget_status,
    instanceTarget_targetId,
    instanceTarget_instanceLabel,
    instanceTarget_targetArn,
    instanceTarget_lifecycleEvents,
    instanceTarget_lastUpdatedAt,

    -- * LambdaFunctionInfo
    LambdaFunctionInfo (..),
    newLambdaFunctionInfo,
    lambdaFunctionInfo_functionAlias,
    lambdaFunctionInfo_targetVersion,
    lambdaFunctionInfo_targetVersionWeight,
    lambdaFunctionInfo_currentVersion,
    lambdaFunctionInfo_functionName,

    -- * LambdaTarget
    LambdaTarget (..),
    newLambdaTarget,
    lambdaTarget_deploymentId,
    lambdaTarget_status,
    lambdaTarget_targetId,
    lambdaTarget_targetArn,
    lambdaTarget_lifecycleEvents,
    lambdaTarget_lambdaFunctionInfo,
    lambdaTarget_lastUpdatedAt,

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
    lifecycleEvent_status,
    lifecycleEvent_diagnostics,
    lifecycleEvent_startTime,
    lifecycleEvent_endTime,
    lifecycleEvent_lifecycleEventName,

    -- * LoadBalancerInfo
    LoadBalancerInfo (..),
    newLoadBalancerInfo,
    loadBalancerInfo_targetGroupPairInfoList,
    loadBalancerInfo_elbInfoList,
    loadBalancerInfo_targetGroupInfoList,

    -- * MinimumHealthyHosts
    MinimumHealthyHosts (..),
    newMinimumHealthyHosts,
    minimumHealthyHosts_value,
    minimumHealthyHosts_type,

    -- * OnPremisesTagSet
    OnPremisesTagSet (..),
    newOnPremisesTagSet,
    onPremisesTagSet_onPremisesTagSetList,

    -- * RawString
    RawString (..),
    newRawString,
    rawString_content,
    rawString_sha256,

    -- * RevisionInfo
    RevisionInfo (..),
    newRevisionInfo,
    revisionInfo_genericRevisionInfo,
    revisionInfo_revisionLocation,

    -- * RevisionLocation
    RevisionLocation (..),
    newRevisionLocation,
    revisionLocation_revisionType,
    revisionLocation_s3Location,
    revisionLocation_appSpecContent,
    revisionLocation_gitHubLocation,
    revisionLocation_string,

    -- * RollbackInfo
    RollbackInfo (..),
    newRollbackInfo,
    rollbackInfo_rollbackMessage,
    rollbackInfo_rollbackTriggeringDeploymentId,
    rollbackInfo_rollbackDeploymentId,

    -- * S3Location
    S3Location (..),
    newS3Location,
    s3Location_eTag,
    s3Location_key,
    s3Location_bundleType,
    s3Location_version,
    s3Location_bucket,

    -- * Tag
    Tag (..),
    newTag,
    tag_key,
    tag_value,

    -- * TagFilter
    TagFilter (..),
    newTagFilter,
    tagFilter_key,
    tagFilter_value,
    tagFilter_type,

    -- * TargetGroupInfo
    TargetGroupInfo (..),
    newTargetGroupInfo,
    targetGroupInfo_name,

    -- * TargetGroupPairInfo
    TargetGroupPairInfo (..),
    newTargetGroupPairInfo,
    targetGroupPairInfo_targetGroups,
    targetGroupPairInfo_prodTrafficRoute,
    targetGroupPairInfo_testTrafficRoute,

    -- * TargetInstances
    TargetInstances (..),
    newTargetInstances,
    targetInstances_tagFilters,
    targetInstances_ec2TagSet,
    targetInstances_autoScalingGroups,

    -- * TimeBasedCanary
    TimeBasedCanary (..),
    newTimeBasedCanary,
    timeBasedCanary_canaryInterval,
    timeBasedCanary_canaryPercentage,

    -- * TimeBasedLinear
    TimeBasedLinear (..),
    newTimeBasedLinear,
    timeBasedLinear_linearInterval,
    timeBasedLinear_linearPercentage,

    -- * TimeRange
    TimeRange (..),
    newTimeRange,
    timeRange_end,
    timeRange_start,

    -- * TrafficRoute
    TrafficRoute (..),
    newTrafficRoute,
    trafficRoute_listenerArns,

    -- * TrafficRoutingConfig
    TrafficRoutingConfig (..),
    newTrafficRoutingConfig,
    trafficRoutingConfig_timeBasedLinear,
    trafficRoutingConfig_type,
    trafficRoutingConfig_timeBasedCanary,

    -- * TriggerConfig
    TriggerConfig (..),
    newTriggerConfig,
    triggerConfig_triggerEvents,
    triggerConfig_triggerName,
    triggerConfig_triggerTargetArn,
  )
where

import Network.AWS.CodeDeploy.Types.Alarm
import Network.AWS.CodeDeploy.Types.AlarmConfiguration
import Network.AWS.CodeDeploy.Types.AppSpecContent
import Network.AWS.CodeDeploy.Types.ApplicationInfo
import Network.AWS.CodeDeploy.Types.ApplicationRevisionSortBy
import Network.AWS.CodeDeploy.Types.AutoRollbackConfiguration
import Network.AWS.CodeDeploy.Types.AutoRollbackEvent
import Network.AWS.CodeDeploy.Types.AutoScalingGroup
import Network.AWS.CodeDeploy.Types.BlueGreenDeploymentConfiguration
import Network.AWS.CodeDeploy.Types.BlueInstanceTerminationOption
import Network.AWS.CodeDeploy.Types.BundleType
import Network.AWS.CodeDeploy.Types.CloudFormationTarget
import Network.AWS.CodeDeploy.Types.ComputePlatform
import Network.AWS.CodeDeploy.Types.DeployErrorCode
import Network.AWS.CodeDeploy.Types.DeploymentConfigInfo
import Network.AWS.CodeDeploy.Types.DeploymentCreator
import Network.AWS.CodeDeploy.Types.DeploymentGroupInfo
import Network.AWS.CodeDeploy.Types.DeploymentInfo
import Network.AWS.CodeDeploy.Types.DeploymentOption
import Network.AWS.CodeDeploy.Types.DeploymentOverview
import Network.AWS.CodeDeploy.Types.DeploymentReadyAction
import Network.AWS.CodeDeploy.Types.DeploymentReadyOption
import Network.AWS.CodeDeploy.Types.DeploymentStatus
import Network.AWS.CodeDeploy.Types.DeploymentStyle
import Network.AWS.CodeDeploy.Types.DeploymentTarget
import Network.AWS.CodeDeploy.Types.DeploymentTargetType
import Network.AWS.CodeDeploy.Types.DeploymentType
import Network.AWS.CodeDeploy.Types.DeploymentWaitType
import Network.AWS.CodeDeploy.Types.Diagnostics
import Network.AWS.CodeDeploy.Types.EC2TagFilter
import Network.AWS.CodeDeploy.Types.EC2TagFilterType
import Network.AWS.CodeDeploy.Types.EC2TagSet
import Network.AWS.CodeDeploy.Types.ECSService
import Network.AWS.CodeDeploy.Types.ECSTarget
import Network.AWS.CodeDeploy.Types.ECSTaskSet
import Network.AWS.CodeDeploy.Types.ELBInfo
import Network.AWS.CodeDeploy.Types.ErrorInformation
import Network.AWS.CodeDeploy.Types.FileExistsBehavior
import Network.AWS.CodeDeploy.Types.GenericRevisionInfo
import Network.AWS.CodeDeploy.Types.GitHubLocation
import Network.AWS.CodeDeploy.Types.GreenFleetProvisioningAction
import Network.AWS.CodeDeploy.Types.GreenFleetProvisioningOption
import Network.AWS.CodeDeploy.Types.InstanceAction
import Network.AWS.CodeDeploy.Types.InstanceInfo
import Network.AWS.CodeDeploy.Types.InstanceTarget
import Network.AWS.CodeDeploy.Types.LambdaFunctionInfo
import Network.AWS.CodeDeploy.Types.LambdaTarget
import Network.AWS.CodeDeploy.Types.LastDeploymentInfo
import Network.AWS.CodeDeploy.Types.LifecycleErrorCode
import Network.AWS.CodeDeploy.Types.LifecycleEvent
import Network.AWS.CodeDeploy.Types.LifecycleEventStatus
import Network.AWS.CodeDeploy.Types.ListStateFilterAction
import Network.AWS.CodeDeploy.Types.LoadBalancerInfo
import Network.AWS.CodeDeploy.Types.MinimumHealthyHosts
import Network.AWS.CodeDeploy.Types.MinimumHealthyHostsType
import Network.AWS.CodeDeploy.Types.OnPremisesTagSet
import Network.AWS.CodeDeploy.Types.RawString
import Network.AWS.CodeDeploy.Types.RegistrationStatus
import Network.AWS.CodeDeploy.Types.RevisionInfo
import Network.AWS.CodeDeploy.Types.RevisionLocation
import Network.AWS.CodeDeploy.Types.RevisionLocationType
import Network.AWS.CodeDeploy.Types.RollbackInfo
import Network.AWS.CodeDeploy.Types.S3Location
import Network.AWS.CodeDeploy.Types.SortOrder
import Network.AWS.CodeDeploy.Types.StopStatus
import Network.AWS.CodeDeploy.Types.Tag
import Network.AWS.CodeDeploy.Types.TagFilter
import Network.AWS.CodeDeploy.Types.TagFilterType
import Network.AWS.CodeDeploy.Types.TargetFilterName
import Network.AWS.CodeDeploy.Types.TargetGroupInfo
import Network.AWS.CodeDeploy.Types.TargetGroupPairInfo
import Network.AWS.CodeDeploy.Types.TargetInstances
import Network.AWS.CodeDeploy.Types.TargetLabel
import Network.AWS.CodeDeploy.Types.TargetStatus
import Network.AWS.CodeDeploy.Types.TimeBasedCanary
import Network.AWS.CodeDeploy.Types.TimeBasedLinear
import Network.AWS.CodeDeploy.Types.TimeRange
import Network.AWS.CodeDeploy.Types.TrafficRoute
import Network.AWS.CodeDeploy.Types.TrafficRoutingConfig
import Network.AWS.CodeDeploy.Types.TrafficRoutingType
import Network.AWS.CodeDeploy.Types.TriggerConfig
import Network.AWS.CodeDeploy.Types.TriggerEventType
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2014-10-06@ of the Amazon CodeDeploy SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core._serviceAbbrev = "CodeDeploy",
      Core._serviceSigner = Sign.v4,
      Core._serviceEndpointPrefix = "codedeploy",
      Core._serviceSigningName = "codedeploy",
      Core._serviceVersion = "2014-10-06",
      Core._serviceEndpoint =
        Core.defaultEndpoint defaultService,
      Core._serviceTimeout = Prelude.Just 70,
      Core._serviceCheck = Core.statusSuccess,
      Core._serviceError =
        Core.parseJSONError "CodeDeploy",
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
      | Lens.has (Core.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Core.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has (Core.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Lens.has (Core.hasStatus 500) e =
        Prelude.Just "general_server_error"
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
      | Prelude.otherwise = Prelude.Nothing

-- | The deployment groups limit was exceeded.
_DeploymentGroupLimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DeploymentGroupLimitExceededException =
  Core._MatchServiceError
    defaultService
    "DeploymentGroupLimitExceededException"

-- | The specified on-premises instance name is already registered.
_InstanceNameAlreadyRegisteredException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InstanceNameAlreadyRegisteredException =
  Core._MatchServiceError
    defaultService
    "InstanceNameAlreadyRegisteredException"

-- | A deployment group with the specified name with the IAM user or AWS
-- account already exists.
_DeploymentGroupAlreadyExistsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DeploymentGroupAlreadyExistsException =
  Core._MatchServiceError
    defaultService
    "DeploymentGroupAlreadyExistsException"

-- | A bucket name is required, but was not provided.
_BucketNameFilterRequiredException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_BucketNameFilterRequiredException =
  Core._MatchServiceError
    defaultService
    "BucketNameFilterRequiredException"

-- | The named revision does not exist with the IAM user or AWS account.
_RevisionDoesNotExistException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_RevisionDoesNotExistException =
  Core._MatchServiceError
    defaultService
    "RevisionDoesNotExistException"

-- | The deployment configuration does not exist with the IAM user or AWS
-- account.
_DeploymentConfigDoesNotExistException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DeploymentConfigDoesNotExistException =
  Core._MatchServiceError
    defaultService
    "DeploymentConfigDoesNotExistException"

-- | An invalid instance type was specified for instances in a blue\/green
-- deployment. Valid values include \"Blue\" for an original environment
-- and \"Green\" for a replacement environment.
_InvalidInstanceTypeException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidInstanceTypeException =
  Core._MatchServiceError
    defaultService
    "InvalidInstanceTypeException"

-- | The IAM user ARN was specified in an invalid format.
_InvalidIamUserArnException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidIamUserArnException =
  Core._MatchServiceError
    defaultService
    "InvalidIamUserArnException"

-- | An invalid fileExistsBehavior option was specified to determine how AWS
-- CodeDeploy handles files or directories that already exist in a
-- deployment target location, but weren\'t part of the previous successful
-- deployment. Valid values include \"DISALLOW,\" \"OVERWRITE,\" and
-- \"RETAIN.\"
_InvalidFileExistsBehaviorException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidFileExistsBehaviorException =
  Core._MatchServiceError
    defaultService
    "InvalidFileExistsBehaviorException"

-- | The tag filter was specified in an invalid format.
_InvalidTagFilterException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidTagFilterException =
  Core._MatchServiceError
    defaultService
    "InvalidTagFilterException"

-- | The provided target ID does not belong to the attempted deployment.
_DeploymentTargetDoesNotExistException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DeploymentTargetDoesNotExistException =
  Core._MatchServiceError
    defaultService
    "DeploymentTargetDoesNotExistException"

-- | The trigger was specified in an invalid format.
_InvalidTriggerConfigException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidTriggerConfigException =
  Core._MatchServiceError
    defaultService
    "InvalidTriggerConfigException"

-- | The maximum number of alarms for a deployment group (10) was exceeded.
_AlarmsLimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AlarmsLimitExceededException =
  Core._MatchServiceError
    defaultService
    "AlarmsLimitExceededException"

-- | A deployment to a target was attempted while another deployment was in
-- progress.
_DeploymentAlreadyStartedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DeploymentAlreadyStartedException =
  Core._MatchServiceError
    defaultService
    "DeploymentAlreadyStartedException"

-- | The specified key prefix filter was specified in an invalid format.
_InvalidKeyPrefixFilterException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidKeyPrefixFilterException =
  Core._MatchServiceError
    defaultService
    "InvalidKeyPrefixFilterException"

-- | The specified ARN is not supported. For example, it might be an ARN for
-- a resource that is not expected.
_ArnNotSupportedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ArnNotSupportedException =
  Core._MatchServiceError
    defaultService
    "ArnNotSupportedException"

-- | The API used does not support the deployment.
_OperationNotSupportedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_OperationNotSupportedException =
  Core._MatchServiceError
    defaultService
    "OperationNotSupportedException"

-- | The GitHub token is not valid.
_InvalidGitHubAccountTokenException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidGitHubAccountTokenException =
  Core._MatchServiceError
    defaultService
    "InvalidGitHubAccountTokenException"

-- | The external ID was specified in an invalid format.
_InvalidExternalIdException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidExternalIdException =
  Core._MatchServiceError
    defaultService
    "InvalidExternalIdException"

-- | The minimum healthy instance value was specified in an invalid format.
_InvalidMinimumHealthyHostValueException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidMinimumHealthyHostValueException =
  Core._MatchServiceError
    defaultService
    "InvalidMinimumHealthyHostValueException"

-- | A call was submitted that is not supported for the specified deployment
-- type.
_UnsupportedActionForDeploymentTypeException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_UnsupportedActionForDeploymentTypeException =
  Core._MatchServiceError
    defaultService
    "UnsupportedActionForDeploymentTypeException"

-- | The Amazon ECS service identifier is not valid.
_InvalidECSServiceException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidECSServiceException =
  Core._MatchServiceError
    defaultService
    "InvalidECSServiceException"

-- | The specified resource could not be validated.
_ResourceValidationException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceValidationException =
  Core._MatchServiceError
    defaultService
    "ResourceValidationException"

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

-- | An invalid operation was detected.
_InvalidOperationException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidOperationException =
  Core._MatchServiceError
    defaultService
    "InvalidOperationException"

-- | An application with the specified name with the IAM user or AWS account
-- already exists.
_ApplicationAlreadyExistsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ApplicationAlreadyExistsException =
  Core._MatchServiceError
    defaultService
    "ApplicationAlreadyExistsException"

-- | A deployment target ID was not provided.
_DeploymentTargetIdRequiredException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DeploymentTargetIdRequiredException =
  Core._MatchServiceError
    defaultService
    "DeploymentTargetIdRequiredException"

-- | The specified ARN is not in a valid format.
_InvalidArnException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidArnException =
  Core._MatchServiceError
    defaultService
    "InvalidArnException"

-- | The maximum allowed number of tags was exceeded.
_TagLimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TagLimitExceededException =
  Core._MatchServiceError
    defaultService
    "TagLimitExceededException"

-- | The Auto Scaling group was specified in an invalid format or does not
-- exist.
_InvalidAutoScalingGroupException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidAutoScalingGroupException =
  Core._MatchServiceError
    defaultService
    "InvalidAutoScalingGroupException"

-- | More applications were attempted to be created than are allowed.
_ApplicationLimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ApplicationLimitExceededException =
  Core._MatchServiceError
    defaultService
    "ApplicationLimitExceededException"

-- | The input was specified in an invalid format.
_InvalidInputException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidInputException =
  Core._MatchServiceError
    defaultService
    "InvalidInputException"

-- | The maximum number of allowed on-premises instances in a single call was
-- exceeded.
_InstanceLimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InstanceLimitExceededException =
  Core._MatchServiceError
    defaultService
    "InstanceLimitExceededException"

-- | The specified IAM user ARN is already registered with an on-premises
-- instance.
_IamUserArnAlreadyRegisteredException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_IamUserArnAlreadyRegisteredException =
  Core._MatchServiceError
    defaultService
    "IamUserArnAlreadyRegisteredException"

-- | An on-premises instance name was not specified.
_InstanceNameRequiredException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InstanceNameRequiredException =
  Core._MatchServiceError
    defaultService
    "InstanceNameRequiredException"

-- | The number of allowed deployments was exceeded.
_DeploymentLimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DeploymentLimitExceededException =
  Core._MatchServiceError
    defaultService
    "DeploymentLimitExceededException"

-- | The target filter name is invalid.
_InvalidTargetFilterNameException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidTargetFilterNameException =
  Core._MatchServiceError
    defaultService
    "InvalidTargetFilterNameException"

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

-- | The specified tags are not valid.
_InvalidTagsToAddException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidTagsToAddException =
  Core._MatchServiceError
    defaultService
    "InvalidTagsToAddException"

-- | The automatic rollback configuration was specified in an invalid format.
-- For example, automatic rollback is enabled, but an invalid triggering
-- event type or no event types were listed.
_InvalidAutoRollbackConfigException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidAutoRollbackConfigException =
  Core._MatchServiceError
    defaultService
    "InvalidAutoRollbackConfigException"

-- | At least one deployment ID must be specified.
_DeploymentIdRequiredException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DeploymentIdRequiredException =
  Core._MatchServiceError
    defaultService
    "DeploymentIdRequiredException"

-- | The service role ARN was specified in an invalid format. Or, if an Auto
-- Scaling group was specified, the specified service role does not grant
-- the appropriate permissions to Amazon EC2 Auto Scaling.
_InvalidRoleException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidRoleException =
  Core._MatchServiceError
    defaultService
    "InvalidRoleException"

-- | A deployment configuration with the specified name with the IAM user or
-- AWS account already exists.
_DeploymentConfigAlreadyExistsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DeploymentConfigAlreadyExistsException =
  Core._MatchServiceError
    defaultService
    "DeploymentConfigAlreadyExistsException"

-- | The next token was specified in an invalid format.
_InvalidNextTokenException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidNextTokenException =
  Core._MatchServiceError
    defaultService
    "InvalidNextTokenException"

-- | The instance ID was not specified.
_InstanceIdRequiredException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InstanceIdRequiredException =
  Core._MatchServiceError
    defaultService
    "InstanceIdRequiredException"

-- | The configuration for the blue\/green deployment group was provided in
-- an invalid format. For information about deployment configuration
-- format, see CreateDeploymentConfig.
_InvalidBlueGreenDeploymentConfigurationException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidBlueGreenDeploymentConfigurationException =
  Core._MatchServiceError
    defaultService
    "InvalidBlueGreenDeploymentConfigurationException"

-- | The deployment configurations limit was exceeded.
_DeploymentConfigLimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DeploymentConfigLimitExceededException =
  Core._MatchServiceError
    defaultService
    "DeploymentConfigLimitExceededException"

-- | An invalid load balancer name, or no load balancer name, was specified.
_InvalidLoadBalancerInfoException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidLoadBalancerInfoException =
  Core._MatchServiceError
    defaultService
    "InvalidLoadBalancerInfoException"

-- | The sort order was specified in an invalid format.
_InvalidSortOrderException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidSortOrderException =
  Core._MatchServiceError
    defaultService
    "InvalidSortOrderException"

-- | An API function was called too frequently.
_ThrottlingException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ThrottlingException =
  Core._MatchServiceError
    defaultService
    "ThrottlingException"

-- | A target group pair associated with this deployment is not valid.
_InvalidTargetGroupPairException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidTargetGroupPairException =
  Core._MatchServiceError
    defaultService
    "InvalidTargetGroupPairException"

-- | The deployment configuration name was not specified.
_DeploymentConfigNameRequiredException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DeploymentConfigNameRequiredException =
  Core._MatchServiceError
    defaultService
    "DeploymentConfigNameRequiredException"

-- | A call was submitted that specified both OnPremisesTagFilters and
-- OnPremisesTagSet, but only one of these data types can be used in a
-- single call.
_InvalidOnPremisesTagCombinationException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidOnPremisesTagCombinationException =
  Core._MatchServiceError
    defaultService
    "InvalidOnPremisesTagCombinationException"

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

-- | The column name to sort by is either not present or was specified in an
-- invalid format.
_InvalidSortByException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidSortByException =
  Core._MatchServiceError
    defaultService
    "InvalidSortByException"

-- | The configuration that specifies how traffic is routed during a
-- deployment is invalid.
_InvalidTrafficRoutingConfigurationException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidTrafficRoutingConfigurationException =
  Core._MatchServiceError
    defaultService
    "InvalidTrafficRoutingConfigurationException"

-- | The description is too long.
_DescriptionTooLongException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DescriptionTooLongException =
  Core._MatchServiceError
    defaultService
    "DescriptionTooLongException"

-- | An IAM user ARN was not specified.
_IamUserArnRequiredException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_IamUserArnRequiredException =
  Core._MatchServiceError
    defaultService
    "IamUserArnRequiredException"

-- | The request included an IAM session ARN that has already been used to
-- register a different instance.
_IamSessionArnAlreadyRegisteredException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_IamSessionArnAlreadyRegisteredException =
  Core._MatchServiceError
    defaultService
    "IamSessionArnAlreadyRegisteredException"

-- | The deployment group name was specified in an invalid format.
_InvalidDeploymentGroupNameException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidDeploymentGroupNameException =
  Core._MatchServiceError
    defaultService
    "InvalidDeploymentGroupNameException"

-- | The bucket name either doesn\'t exist or was specified in an invalid
-- format.
_InvalidBucketNameFilterException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidBucketNameFilterException =
  Core._MatchServiceError
    defaultService
    "InvalidBucketNameFilterException"

-- | A target is not valid.
_InvalidTargetException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidTargetException =
  Core._MatchServiceError
    defaultService
    "InvalidTargetException"

-- | The deployment group name was not specified.
_DeploymentGroupNameRequiredException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DeploymentGroupNameRequiredException =
  Core._MatchServiceError
    defaultService
    "DeploymentGroupNameRequiredException"

-- | The specified time range was specified in an invalid format.
_InvalidTimeRangeException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidTimeRangeException =
  Core._MatchServiceError
    defaultService
    "InvalidTimeRangeException"

-- | A tag was not specified.
_TagRequiredException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TagRequiredException =
  Core._MatchServiceError
    defaultService
    "TagRequiredException"

-- | The IgnoreApplicationStopFailures value is invalid. For AWS Lambda
-- deployments, @false@ is expected. For EC2\/On-premises deployments,
-- @true@ or @false@ is expected.
_InvalidIgnoreApplicationStopFailuresValueException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidIgnoreApplicationStopFailuresValueException =
  Core._MatchServiceError
    defaultService
    "InvalidIgnoreApplicationStopFailuresValueException"

-- | The UpdateOutdatedInstancesOnly value is invalid. For AWS Lambda
-- deployments, @false@ is expected. For EC2\/On-premises deployments,
-- @true@ or @false@ is expected.
_InvalidUpdateOutdatedInstancesOnlyValueException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidUpdateOutdatedInstancesOnlyValueException =
  Core._MatchServiceError
    defaultService
    "InvalidUpdateOutdatedInstancesOnlyValueException"

-- | The tag was specified in an invalid format.
_InvalidTagException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidTagException =
  Core._MatchServiceError
    defaultService
    "InvalidTagException"

-- | The wait type is invalid.
_InvalidDeploymentWaitTypeException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidDeploymentWaitTypeException =
  Core._MatchServiceError
    defaultService
    "InvalidDeploymentWaitTypeException"

-- | The computePlatform is invalid. The computePlatform should be @Lambda@,
-- @Server@, or @ECS@.
_InvalidComputePlatformException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidComputePlatformException =
  Core._MatchServiceError
    defaultService
    "InvalidComputePlatformException"

-- | The limit for lifecycle hooks was exceeded.
_LifecycleHookLimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_LifecycleHookLimitExceededException =
  Core._MatchServiceError
    defaultService
    "LifecycleHookLimitExceededException"

-- | The result of a Lambda validation function that verifies a lifecycle
-- event is invalid. It should return @Succeeded@ or @Failed@.
_InvalidLifecycleEventHookExecutionStatusException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidLifecycleEventHookExecutionStatusException =
  Core._MatchServiceError
    defaultService
    "InvalidLifecycleEventHookExecutionStatusException"

-- | The ARN of a resource is required, but was not found.
_ResourceArnRequiredException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceArnRequiredException =
  Core._MatchServiceError
    defaultService
    "ResourceArnRequiredException"

-- | A call was submitted that specified both Ec2TagFilters and Ec2TagSet,
-- but only one of these data types can be used in a single call.
_InvalidEC2TagCombinationException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidEC2TagCombinationException =
  Core._MatchServiceError
    defaultService
    "InvalidEC2TagCombinationException"

-- | No IAM ARN was included in the request. You must use an IAM session ARN
-- or IAM user ARN in the request.
_IamArnRequiredException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_IamArnRequiredException =
  Core._MatchServiceError
    defaultService
    "IamArnRequiredException"

-- | The call is missing a required GitHub account connection name.
_GitHubAccountTokenNameRequiredException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_GitHubAccountTokenNameRequiredException =
  Core._MatchServiceError
    defaultService
    "GitHubAccountTokenNameRequiredException"

-- | An attempt to return the status of an already completed lifecycle event
-- occurred.
_LifecycleEventAlreadyCompletedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_LifecycleEventAlreadyCompletedException =
  Core._MatchServiceError
    defaultService
    "LifecycleEventAlreadyCompletedException"

-- | The application does not exist with the IAM user or AWS account.
_ApplicationDoesNotExistException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ApplicationDoesNotExistException =
  Core._MatchServiceError
    defaultService
    "ApplicationDoesNotExistException"

-- | Both an IAM user ARN and an IAM session ARN were included in the
-- request. Use only one ARN type.
_MultipleIamArnsProvidedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_MultipleIamArnsProvidedException =
  Core._MatchServiceError
    defaultService
    "MultipleIamArnsProvidedException"

-- | The target ID provided was not valid.
_InvalidDeploymentTargetIdException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidDeploymentTargetIdException =
  Core._MatchServiceError
    defaultService
    "InvalidDeploymentTargetIdException"

-- | The registration status was specified in an invalid format.
_InvalidRegistrationStatusException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidRegistrationStatusException =
  Core._MatchServiceError
    defaultService
    "InvalidRegistrationStatusException"

-- | The specified on-premises instance is not registered.
_InstanceNotRegisteredException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InstanceNotRegisteredException =
  Core._MatchServiceError
    defaultService
    "InstanceNotRegisteredException"

-- | The deployed state filter was specified in an invalid format.
_InvalidDeployedStateFilterException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidDeployedStateFilterException =
  Core._MatchServiceError
    defaultService
    "InvalidDeployedStateFilterException"

-- | The specified instance status does not exist.
_InvalidInstanceStatusException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidInstanceStatusException =
  Core._MatchServiceError
    defaultService
    "InvalidInstanceStatusException"

-- | The minimum number of required application names was not specified.
_ApplicationNameRequiredException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ApplicationNameRequiredException =
  Core._MatchServiceError
    defaultService
    "ApplicationNameRequiredException"

-- | The specified deployment status doesn\'t exist or cannot be determined.
_InvalidDeploymentStatusException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidDeploymentStatusException =
  Core._MatchServiceError
    defaultService
    "InvalidDeploymentStatusException"

-- | The maximum allowed number of triggers was exceeded.
_TriggerTargetsLimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TriggerTargetsLimitExceededException =
  Core._MatchServiceError
    defaultService
    "TriggerTargetsLimitExceededException"

-- | The number of tag groups included in the tag set list exceeded the
-- maximum allowed limit of 3.
_TagSetListLimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TagSetListLimitExceededException =
  Core._MatchServiceError
    defaultService
    "TagSetListLimitExceededException"

-- | No GitHub account connection exists with the named specified in the
-- call.
_GitHubAccountTokenDoesNotExistException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_GitHubAccountTokenDoesNotExistException =
  Core._MatchServiceError
    defaultService
    "GitHubAccountTokenDoesNotExistException"

-- | The application name was specified in an invalid format.
_InvalidApplicationNameException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidApplicationNameException =
  Core._MatchServiceError
    defaultService
    "InvalidApplicationNameException"

-- | The maximum number of targets that can be associated with an Amazon ECS
-- or AWS Lambda deployment was exceeded. The target list of both types of
-- deployments must have exactly one item. This exception does not apply to
-- EC2\/On-premises deployments.
_DeploymentTargetListSizeExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DeploymentTargetListSizeExceededException =
  Core._MatchServiceError
    defaultService
    "DeploymentTargetListSizeExceededException"

-- | The deployment configuration is still in use.
_DeploymentConfigInUseException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DeploymentConfigInUseException =
  Core._MatchServiceError
    defaultService
    "DeploymentConfigInUseException"

-- | The on-premises instance name was specified in an invalid format.
_InvalidInstanceNameException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidInstanceNameException =
  Core._MatchServiceError
    defaultService
    "InvalidInstanceNameException"

-- | The IAM session ARN was specified in an invalid format.
_InvalidIamSessionArnException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidIamSessionArnException =
  Core._MatchServiceError
    defaultService
    "InvalidIamSessionArnException"

-- | A lifecycle event hook is invalid. Review the @hooks@ section in your
-- AppSpec file to ensure the lifecycle events and @hooks@ functions are
-- valid.
_InvalidLifecycleEventHookExecutionIdException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidLifecycleEventHookExecutionIdException =
  Core._MatchServiceError
    defaultService
    "InvalidLifecycleEventHookExecutionIdException"

-- | The tag was specified in an invalid format.
_InvalidEC2TagException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidEC2TagException =
  Core._MatchServiceError
    defaultService
    "InvalidEC2TagException"

-- | The ID of the deployment configuration is invalid.
_InvalidDeploymentConfigIdException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidDeploymentConfigIdException =
  Core._MatchServiceError
    defaultService
    "InvalidDeploymentConfigIdException"

-- | An invalid deployment style was specified. Valid deployment types
-- include \"IN_PLACE\" and \"BLUE_GREEN.\" Valid deployment options
-- include \"WITH_TRAFFIC_CONTROL\" and \"WITHOUT_TRAFFIC_CONTROL.\"
_InvalidDeploymentStyleException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidDeploymentStyleException =
  Core._MatchServiceError
    defaultService
    "InvalidDeploymentStyleException"

-- | The revision ID was not specified.
_RevisionRequiredException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_RevisionRequiredException =
  Core._MatchServiceError
    defaultService
    "RevisionRequiredException"

-- | The specified instance does not exist in the deployment group.
_InstanceDoesNotExistException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InstanceDoesNotExistException =
  Core._MatchServiceError
    defaultService
    "InstanceDoesNotExistException"

-- | The deployment is already complete.
_DeploymentAlreadyCompletedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DeploymentAlreadyCompletedException =
  Core._MatchServiceError
    defaultService
    "DeploymentAlreadyCompletedException"

-- | The Amazon ECS service is associated with more than one deployment
-- groups. An Amazon ECS service can be associated with only one deployment
-- group.
_ECSServiceMappingLimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ECSServiceMappingLimitExceededException =
  Core._MatchServiceError
    defaultService
    "ECSServiceMappingLimitExceededException"

-- | The deployment with the IAM user or AWS account does not exist.
_DeploymentDoesNotExistException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DeploymentDoesNotExistException =
  Core._MatchServiceError
    defaultService
    "DeploymentDoesNotExistException"

-- | The maximum number of names or IDs allowed for this request (100) was
-- exceeded.
_BatchLimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_BatchLimitExceededException =
  Core._MatchServiceError
    defaultService
    "BatchLimitExceededException"

-- | At least one of the deployment IDs was specified in an invalid format.
_InvalidDeploymentIdException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidDeploymentIdException =
  Core._MatchServiceError
    defaultService
    "InvalidDeploymentIdException"

-- | The deployment configuration name was specified in an invalid format.
_InvalidDeploymentConfigNameException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidDeploymentConfigNameException =
  Core._MatchServiceError
    defaultService
    "InvalidDeploymentConfigNameException"

-- | The specified deployment has not started.
_DeploymentNotStartedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DeploymentNotStartedException =
  Core._MatchServiceError
    defaultService
    "DeploymentNotStartedException"

-- | The deployment does not have a status of Ready and can\'t continue yet.
_DeploymentIsNotInReadyStateException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DeploymentIsNotInReadyStateException =
  Core._MatchServiceError
    defaultService
    "DeploymentIsNotInReadyStateException"

-- |
_InvalidInstanceIdException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidInstanceIdException =
  Core._MatchServiceError
    defaultService
    "InvalidInstanceIdException"

-- | The named deployment group with the IAM user or AWS account does not
-- exist.
_DeploymentGroupDoesNotExistException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DeploymentGroupDoesNotExistException =
  Core._MatchServiceError
    defaultService
    "DeploymentGroupDoesNotExistException"

-- | The role ID was not specified.
_RoleRequiredException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_RoleRequiredException =
  Core._MatchServiceError
    defaultService
    "RoleRequiredException"
