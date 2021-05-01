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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2014-10-06@ of the Amazon CodeDeploy SDK configuration.
defaultService :: Prelude.Service
defaultService =
  Prelude.Service
    { Prelude._svcAbbrev = "CodeDeploy",
      Prelude._svcSigner = Sign.v4,
      Prelude._svcPrefix = "codedeploy",
      Prelude._svcVersion = "2014-10-06",
      Prelude._svcEndpoint =
        Prelude.defaultEndpoint defaultService,
      Prelude._svcTimeout = Prelude.Just 70,
      Prelude._svcCheck = Prelude.statusSuccess,
      Prelude._svcError =
        Prelude.parseJSONError "CodeDeploy",
      Prelude._svcRetry = retry
    }
  where
    retry =
      Prelude.Exponential
        { Prelude._retryBase = 5.0e-2,
          Prelude._retryGrowth = 2,
          Prelude._retryAttempts = 5,
          Prelude._retryCheck = check
        }
    check e
      | Lens.has (Prelude.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Prelude.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Lens.has (Prelude.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Prelude.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Prelude.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Lens.has
          ( Prelude.hasCode "RequestThrottledException"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has
          ( Prelude.hasCode "ThrottledException"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has (Prelude.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Lens.has (Prelude.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has
          ( Prelude.hasCode "ThrottlingException"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has
          ( Prelude.hasCode "Throttling"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Prelude.otherwise = Prelude.Nothing

-- | The deployment groups limit was exceeded.
_DeploymentGroupLimitExceededException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_DeploymentGroupLimitExceededException =
  Prelude._MatchServiceError
    defaultService
    "DeploymentGroupLimitExceededException"

-- | The specified on-premises instance name is already registered.
_InstanceNameAlreadyRegisteredException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InstanceNameAlreadyRegisteredException =
  Prelude._MatchServiceError
    defaultService
    "InstanceNameAlreadyRegisteredException"

-- | A deployment group with the specified name with the IAM user or AWS
-- account already exists.
_DeploymentGroupAlreadyExistsException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_DeploymentGroupAlreadyExistsException =
  Prelude._MatchServiceError
    defaultService
    "DeploymentGroupAlreadyExistsException"

-- | A bucket name is required, but was not provided.
_BucketNameFilterRequiredException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_BucketNameFilterRequiredException =
  Prelude._MatchServiceError
    defaultService
    "BucketNameFilterRequiredException"

-- | The named revision does not exist with the IAM user or AWS account.
_RevisionDoesNotExistException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_RevisionDoesNotExistException =
  Prelude._MatchServiceError
    defaultService
    "RevisionDoesNotExistException"

-- | The deployment configuration does not exist with the IAM user or AWS
-- account.
_DeploymentConfigDoesNotExistException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_DeploymentConfigDoesNotExistException =
  Prelude._MatchServiceError
    defaultService
    "DeploymentConfigDoesNotExistException"

-- | An invalid instance type was specified for instances in a blue\/green
-- deployment. Valid values include \"Blue\" for an original environment
-- and \"Green\" for a replacement environment.
_InvalidInstanceTypeException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidInstanceTypeException =
  Prelude._MatchServiceError
    defaultService
    "InvalidInstanceTypeException"

-- | The IAM user ARN was specified in an invalid format.
_InvalidIamUserArnException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidIamUserArnException =
  Prelude._MatchServiceError
    defaultService
    "InvalidIamUserArnException"

-- | An invalid fileExistsBehavior option was specified to determine how AWS
-- CodeDeploy handles files or directories that already exist in a
-- deployment target location, but weren\'t part of the previous successful
-- deployment. Valid values include \"DISALLOW,\" \"OVERWRITE,\" and
-- \"RETAIN.\"
_InvalidFileExistsBehaviorException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidFileExistsBehaviorException =
  Prelude._MatchServiceError
    defaultService
    "InvalidFileExistsBehaviorException"

-- | The tag filter was specified in an invalid format.
_InvalidTagFilterException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidTagFilterException =
  Prelude._MatchServiceError
    defaultService
    "InvalidTagFilterException"

-- | The provided target ID does not belong to the attempted deployment.
_DeploymentTargetDoesNotExistException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_DeploymentTargetDoesNotExistException =
  Prelude._MatchServiceError
    defaultService
    "DeploymentTargetDoesNotExistException"

-- | The trigger was specified in an invalid format.
_InvalidTriggerConfigException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidTriggerConfigException =
  Prelude._MatchServiceError
    defaultService
    "InvalidTriggerConfigException"

-- | The maximum number of alarms for a deployment group (10) was exceeded.
_AlarmsLimitExceededException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_AlarmsLimitExceededException =
  Prelude._MatchServiceError
    defaultService
    "AlarmsLimitExceededException"

-- | A deployment to a target was attempted while another deployment was in
-- progress.
_DeploymentAlreadyStartedException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_DeploymentAlreadyStartedException =
  Prelude._MatchServiceError
    defaultService
    "DeploymentAlreadyStartedException"

-- | The specified key prefix filter was specified in an invalid format.
_InvalidKeyPrefixFilterException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidKeyPrefixFilterException =
  Prelude._MatchServiceError
    defaultService
    "InvalidKeyPrefixFilterException"

-- | The specified ARN is not supported. For example, it might be an ARN for
-- a resource that is not expected.
_ArnNotSupportedException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ArnNotSupportedException =
  Prelude._MatchServiceError
    defaultService
    "ArnNotSupportedException"

-- | The API used does not support the deployment.
_OperationNotSupportedException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_OperationNotSupportedException =
  Prelude._MatchServiceError
    defaultService
    "OperationNotSupportedException"

-- | The GitHub token is not valid.
_InvalidGitHubAccountTokenException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidGitHubAccountTokenException =
  Prelude._MatchServiceError
    defaultService
    "InvalidGitHubAccountTokenException"

-- | The external ID was specified in an invalid format.
_InvalidExternalIdException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidExternalIdException =
  Prelude._MatchServiceError
    defaultService
    "InvalidExternalIdException"

-- | The minimum healthy instance value was specified in an invalid format.
_InvalidMinimumHealthyHostValueException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidMinimumHealthyHostValueException =
  Prelude._MatchServiceError
    defaultService
    "InvalidMinimumHealthyHostValueException"

-- | A call was submitted that is not supported for the specified deployment
-- type.
_UnsupportedActionForDeploymentTypeException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_UnsupportedActionForDeploymentTypeException =
  Prelude._MatchServiceError
    defaultService
    "UnsupportedActionForDeploymentTypeException"

-- | The Amazon ECS service identifier is not valid.
_InvalidECSServiceException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidECSServiceException =
  Prelude._MatchServiceError
    defaultService
    "InvalidECSServiceException"

-- | The specified resource could not be validated.
_ResourceValidationException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ResourceValidationException =
  Prelude._MatchServiceError
    defaultService
    "ResourceValidationException"

-- | An instance type was specified for an in-place deployment. Instance
-- types are supported for blue\/green deployments only.
_InvalidDeploymentInstanceTypeException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidDeploymentInstanceTypeException =
  Prelude._MatchServiceError
    defaultService
    "InvalidDeploymentInstanceTypeException"

-- | The format of the specified GitHub account connection name is invalid.
_InvalidGitHubAccountTokenNameException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidGitHubAccountTokenNameException =
  Prelude._MatchServiceError
    defaultService
    "InvalidGitHubAccountTokenNameException"

-- | An invalid operation was detected.
_InvalidOperationException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidOperationException =
  Prelude._MatchServiceError
    defaultService
    "InvalidOperationException"

-- | An application with the specified name with the IAM user or AWS account
-- already exists.
_ApplicationAlreadyExistsException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ApplicationAlreadyExistsException =
  Prelude._MatchServiceError
    defaultService
    "ApplicationAlreadyExistsException"

-- | A deployment target ID was not provided.
_DeploymentTargetIdRequiredException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_DeploymentTargetIdRequiredException =
  Prelude._MatchServiceError
    defaultService
    "DeploymentTargetIdRequiredException"

-- | The specified ARN is not in a valid format.
_InvalidArnException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidArnException =
  Prelude._MatchServiceError
    defaultService
    "InvalidArnException"

-- | The maximum allowed number of tags was exceeded.
_TagLimitExceededException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_TagLimitExceededException =
  Prelude._MatchServiceError
    defaultService
    "TagLimitExceededException"

-- | The Auto Scaling group was specified in an invalid format or does not
-- exist.
_InvalidAutoScalingGroupException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidAutoScalingGroupException =
  Prelude._MatchServiceError
    defaultService
    "InvalidAutoScalingGroupException"

-- | More applications were attempted to be created than are allowed.
_ApplicationLimitExceededException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ApplicationLimitExceededException =
  Prelude._MatchServiceError
    defaultService
    "ApplicationLimitExceededException"

-- | The input was specified in an invalid format.
_InvalidInputException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidInputException =
  Prelude._MatchServiceError
    defaultService
    "InvalidInputException"

-- | The maximum number of allowed on-premises instances in a single call was
-- exceeded.
_InstanceLimitExceededException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InstanceLimitExceededException =
  Prelude._MatchServiceError
    defaultService
    "InstanceLimitExceededException"

-- | The specified IAM user ARN is already registered with an on-premises
-- instance.
_IamUserArnAlreadyRegisteredException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_IamUserArnAlreadyRegisteredException =
  Prelude._MatchServiceError
    defaultService
    "IamUserArnAlreadyRegisteredException"

-- | An on-premises instance name was not specified.
_InstanceNameRequiredException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InstanceNameRequiredException =
  Prelude._MatchServiceError
    defaultService
    "InstanceNameRequiredException"

-- | The number of allowed deployments was exceeded.
_DeploymentLimitExceededException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_DeploymentLimitExceededException =
  Prelude._MatchServiceError
    defaultService
    "DeploymentLimitExceededException"

-- | The target filter name is invalid.
_InvalidTargetFilterNameException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidTargetFilterNameException =
  Prelude._MatchServiceError
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
_InvalidTargetInstancesException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidTargetInstancesException =
  Prelude._MatchServiceError
    defaultService
    "InvalidTargetInstancesException"

-- | The revision was specified in an invalid format.
_InvalidRevisionException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidRevisionException =
  Prelude._MatchServiceError
    defaultService
    "InvalidRevisionException"

-- | The specified tags are not valid.
_InvalidTagsToAddException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidTagsToAddException =
  Prelude._MatchServiceError
    defaultService
    "InvalidTagsToAddException"

-- | The automatic rollback configuration was specified in an invalid format.
-- For example, automatic rollback is enabled, but an invalid triggering
-- event type or no event types were listed.
_InvalidAutoRollbackConfigException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidAutoRollbackConfigException =
  Prelude._MatchServiceError
    defaultService
    "InvalidAutoRollbackConfigException"

-- | At least one deployment ID must be specified.
_DeploymentIdRequiredException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_DeploymentIdRequiredException =
  Prelude._MatchServiceError
    defaultService
    "DeploymentIdRequiredException"

-- | The service role ARN was specified in an invalid format. Or, if an Auto
-- Scaling group was specified, the specified service role does not grant
-- the appropriate permissions to Amazon EC2 Auto Scaling.
_InvalidRoleException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidRoleException =
  Prelude._MatchServiceError
    defaultService
    "InvalidRoleException"

-- | A deployment configuration with the specified name with the IAM user or
-- AWS account already exists.
_DeploymentConfigAlreadyExistsException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_DeploymentConfigAlreadyExistsException =
  Prelude._MatchServiceError
    defaultService
    "DeploymentConfigAlreadyExistsException"

-- | The next token was specified in an invalid format.
_InvalidNextTokenException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidNextTokenException =
  Prelude._MatchServiceError
    defaultService
    "InvalidNextTokenException"

-- | The instance ID was not specified.
_InstanceIdRequiredException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InstanceIdRequiredException =
  Prelude._MatchServiceError
    defaultService
    "InstanceIdRequiredException"

-- | The configuration for the blue\/green deployment group was provided in
-- an invalid format. For information about deployment configuration
-- format, see CreateDeploymentConfig.
_InvalidBlueGreenDeploymentConfigurationException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidBlueGreenDeploymentConfigurationException =
  Prelude._MatchServiceError
    defaultService
    "InvalidBlueGreenDeploymentConfigurationException"

-- | The deployment configurations limit was exceeded.
_DeploymentConfigLimitExceededException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_DeploymentConfigLimitExceededException =
  Prelude._MatchServiceError
    defaultService
    "DeploymentConfigLimitExceededException"

-- | An invalid load balancer name, or no load balancer name, was specified.
_InvalidLoadBalancerInfoException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidLoadBalancerInfoException =
  Prelude._MatchServiceError
    defaultService
    "InvalidLoadBalancerInfoException"

-- | The sort order was specified in an invalid format.
_InvalidSortOrderException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidSortOrderException =
  Prelude._MatchServiceError
    defaultService
    "InvalidSortOrderException"

-- | An API function was called too frequently.
_ThrottlingException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ThrottlingException =
  Prelude._MatchServiceError
    defaultService
    "ThrottlingException"

-- | A target group pair associated with this deployment is not valid.
_InvalidTargetGroupPairException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidTargetGroupPairException =
  Prelude._MatchServiceError
    defaultService
    "InvalidTargetGroupPairException"

-- | The deployment configuration name was not specified.
_DeploymentConfigNameRequiredException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_DeploymentConfigNameRequiredException =
  Prelude._MatchServiceError
    defaultService
    "DeploymentConfigNameRequiredException"

-- | A call was submitted that specified both OnPremisesTagFilters and
-- OnPremisesTagSet, but only one of these data types can be used in a
-- single call.
_InvalidOnPremisesTagCombinationException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidOnPremisesTagCombinationException =
  Prelude._MatchServiceError
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
_InvalidAlarmConfigException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidAlarmConfigException =
  Prelude._MatchServiceError
    defaultService
    "InvalidAlarmConfigException"

-- | The column name to sort by is either not present or was specified in an
-- invalid format.
_InvalidSortByException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidSortByException =
  Prelude._MatchServiceError
    defaultService
    "InvalidSortByException"

-- | The configuration that specifies how traffic is routed during a
-- deployment is invalid.
_InvalidTrafficRoutingConfigurationException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidTrafficRoutingConfigurationException =
  Prelude._MatchServiceError
    defaultService
    "InvalidTrafficRoutingConfigurationException"

-- | The description is too long.
_DescriptionTooLongException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_DescriptionTooLongException =
  Prelude._MatchServiceError
    defaultService
    "DescriptionTooLongException"

-- | An IAM user ARN was not specified.
_IamUserArnRequiredException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_IamUserArnRequiredException =
  Prelude._MatchServiceError
    defaultService
    "IamUserArnRequiredException"

-- | The request included an IAM session ARN that has already been used to
-- register a different instance.
_IamSessionArnAlreadyRegisteredException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_IamSessionArnAlreadyRegisteredException =
  Prelude._MatchServiceError
    defaultService
    "IamSessionArnAlreadyRegisteredException"

-- | The deployment group name was specified in an invalid format.
_InvalidDeploymentGroupNameException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidDeploymentGroupNameException =
  Prelude._MatchServiceError
    defaultService
    "InvalidDeploymentGroupNameException"

-- | The bucket name either doesn\'t exist or was specified in an invalid
-- format.
_InvalidBucketNameFilterException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidBucketNameFilterException =
  Prelude._MatchServiceError
    defaultService
    "InvalidBucketNameFilterException"

-- | A target is not valid.
_InvalidTargetException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidTargetException =
  Prelude._MatchServiceError
    defaultService
    "InvalidTargetException"

-- | The deployment group name was not specified.
_DeploymentGroupNameRequiredException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_DeploymentGroupNameRequiredException =
  Prelude._MatchServiceError
    defaultService
    "DeploymentGroupNameRequiredException"

-- | The specified time range was specified in an invalid format.
_InvalidTimeRangeException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidTimeRangeException =
  Prelude._MatchServiceError
    defaultService
    "InvalidTimeRangeException"

-- | A tag was not specified.
_TagRequiredException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_TagRequiredException =
  Prelude._MatchServiceError
    defaultService
    "TagRequiredException"

-- | The IgnoreApplicationStopFailures value is invalid. For AWS Lambda
-- deployments, @false@ is expected. For EC2\/On-premises deployments,
-- @true@ or @false@ is expected.
_InvalidIgnoreApplicationStopFailuresValueException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidIgnoreApplicationStopFailuresValueException =
  Prelude._MatchServiceError
    defaultService
    "InvalidIgnoreApplicationStopFailuresValueException"

-- | The UpdateOutdatedInstancesOnly value is invalid. For AWS Lambda
-- deployments, @false@ is expected. For EC2\/On-premises deployments,
-- @true@ or @false@ is expected.
_InvalidUpdateOutdatedInstancesOnlyValueException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidUpdateOutdatedInstancesOnlyValueException =
  Prelude._MatchServiceError
    defaultService
    "InvalidUpdateOutdatedInstancesOnlyValueException"

-- | The tag was specified in an invalid format.
_InvalidTagException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidTagException =
  Prelude._MatchServiceError
    defaultService
    "InvalidTagException"

-- | The wait type is invalid.
_InvalidDeploymentWaitTypeException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidDeploymentWaitTypeException =
  Prelude._MatchServiceError
    defaultService
    "InvalidDeploymentWaitTypeException"

-- | The computePlatform is invalid. The computePlatform should be @Lambda@,
-- @Server@, or @ECS@.
_InvalidComputePlatformException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidComputePlatformException =
  Prelude._MatchServiceError
    defaultService
    "InvalidComputePlatformException"

-- | The limit for lifecycle hooks was exceeded.
_LifecycleHookLimitExceededException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_LifecycleHookLimitExceededException =
  Prelude._MatchServiceError
    defaultService
    "LifecycleHookLimitExceededException"

-- | The result of a Lambda validation function that verifies a lifecycle
-- event is invalid. It should return @Succeeded@ or @Failed@.
_InvalidLifecycleEventHookExecutionStatusException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidLifecycleEventHookExecutionStatusException =
  Prelude._MatchServiceError
    defaultService
    "InvalidLifecycleEventHookExecutionStatusException"

-- | The ARN of a resource is required, but was not found.
_ResourceArnRequiredException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ResourceArnRequiredException =
  Prelude._MatchServiceError
    defaultService
    "ResourceArnRequiredException"

-- | A call was submitted that specified both Ec2TagFilters and Ec2TagSet,
-- but only one of these data types can be used in a single call.
_InvalidEC2TagCombinationException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidEC2TagCombinationException =
  Prelude._MatchServiceError
    defaultService
    "InvalidEC2TagCombinationException"

-- | No IAM ARN was included in the request. You must use an IAM session ARN
-- or IAM user ARN in the request.
_IamArnRequiredException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_IamArnRequiredException =
  Prelude._MatchServiceError
    defaultService
    "IamArnRequiredException"

-- | The call is missing a required GitHub account connection name.
_GitHubAccountTokenNameRequiredException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_GitHubAccountTokenNameRequiredException =
  Prelude._MatchServiceError
    defaultService
    "GitHubAccountTokenNameRequiredException"

-- | An attempt to return the status of an already completed lifecycle event
-- occurred.
_LifecycleEventAlreadyCompletedException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_LifecycleEventAlreadyCompletedException =
  Prelude._MatchServiceError
    defaultService
    "LifecycleEventAlreadyCompletedException"

-- | The application does not exist with the IAM user or AWS account.
_ApplicationDoesNotExistException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ApplicationDoesNotExistException =
  Prelude._MatchServiceError
    defaultService
    "ApplicationDoesNotExistException"

-- | Both an IAM user ARN and an IAM session ARN were included in the
-- request. Use only one ARN type.
_MultipleIamArnsProvidedException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_MultipleIamArnsProvidedException =
  Prelude._MatchServiceError
    defaultService
    "MultipleIamArnsProvidedException"

-- | The target ID provided was not valid.
_InvalidDeploymentTargetIdException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidDeploymentTargetIdException =
  Prelude._MatchServiceError
    defaultService
    "InvalidDeploymentTargetIdException"

-- | The registration status was specified in an invalid format.
_InvalidRegistrationStatusException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidRegistrationStatusException =
  Prelude._MatchServiceError
    defaultService
    "InvalidRegistrationStatusException"

-- | The specified on-premises instance is not registered.
_InstanceNotRegisteredException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InstanceNotRegisteredException =
  Prelude._MatchServiceError
    defaultService
    "InstanceNotRegisteredException"

-- | The deployed state filter was specified in an invalid format.
_InvalidDeployedStateFilterException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidDeployedStateFilterException =
  Prelude._MatchServiceError
    defaultService
    "InvalidDeployedStateFilterException"

-- | The specified instance status does not exist.
_InvalidInstanceStatusException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidInstanceStatusException =
  Prelude._MatchServiceError
    defaultService
    "InvalidInstanceStatusException"

-- | The minimum number of required application names was not specified.
_ApplicationNameRequiredException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ApplicationNameRequiredException =
  Prelude._MatchServiceError
    defaultService
    "ApplicationNameRequiredException"

-- | The specified deployment status doesn\'t exist or cannot be determined.
_InvalidDeploymentStatusException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidDeploymentStatusException =
  Prelude._MatchServiceError
    defaultService
    "InvalidDeploymentStatusException"

-- | The maximum allowed number of triggers was exceeded.
_TriggerTargetsLimitExceededException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_TriggerTargetsLimitExceededException =
  Prelude._MatchServiceError
    defaultService
    "TriggerTargetsLimitExceededException"

-- | The number of tag groups included in the tag set list exceeded the
-- maximum allowed limit of 3.
_TagSetListLimitExceededException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_TagSetListLimitExceededException =
  Prelude._MatchServiceError
    defaultService
    "TagSetListLimitExceededException"

-- | No GitHub account connection exists with the named specified in the
-- call.
_GitHubAccountTokenDoesNotExistException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_GitHubAccountTokenDoesNotExistException =
  Prelude._MatchServiceError
    defaultService
    "GitHubAccountTokenDoesNotExistException"

-- | The application name was specified in an invalid format.
_InvalidApplicationNameException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidApplicationNameException =
  Prelude._MatchServiceError
    defaultService
    "InvalidApplicationNameException"

-- | The maximum number of targets that can be associated with an Amazon ECS
-- or AWS Lambda deployment was exceeded. The target list of both types of
-- deployments must have exactly one item. This exception does not apply to
-- EC2\/On-premises deployments.
_DeploymentTargetListSizeExceededException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_DeploymentTargetListSizeExceededException =
  Prelude._MatchServiceError
    defaultService
    "DeploymentTargetListSizeExceededException"

-- | The deployment configuration is still in use.
_DeploymentConfigInUseException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_DeploymentConfigInUseException =
  Prelude._MatchServiceError
    defaultService
    "DeploymentConfigInUseException"

-- | The on-premises instance name was specified in an invalid format.
_InvalidInstanceNameException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidInstanceNameException =
  Prelude._MatchServiceError
    defaultService
    "InvalidInstanceNameException"

-- | The IAM session ARN was specified in an invalid format.
_InvalidIamSessionArnException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidIamSessionArnException =
  Prelude._MatchServiceError
    defaultService
    "InvalidIamSessionArnException"

-- | A lifecycle event hook is invalid. Review the @hooks@ section in your
-- AppSpec file to ensure the lifecycle events and @hooks@ functions are
-- valid.
_InvalidLifecycleEventHookExecutionIdException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidLifecycleEventHookExecutionIdException =
  Prelude._MatchServiceError
    defaultService
    "InvalidLifecycleEventHookExecutionIdException"

-- | The tag was specified in an invalid format.
_InvalidEC2TagException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidEC2TagException =
  Prelude._MatchServiceError
    defaultService
    "InvalidEC2TagException"

-- | The ID of the deployment configuration is invalid.
_InvalidDeploymentConfigIdException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidDeploymentConfigIdException =
  Prelude._MatchServiceError
    defaultService
    "InvalidDeploymentConfigIdException"

-- | An invalid deployment style was specified. Valid deployment types
-- include \"IN_PLACE\" and \"BLUE_GREEN.\" Valid deployment options
-- include \"WITH_TRAFFIC_CONTROL\" and \"WITHOUT_TRAFFIC_CONTROL.\"
_InvalidDeploymentStyleException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidDeploymentStyleException =
  Prelude._MatchServiceError
    defaultService
    "InvalidDeploymentStyleException"

-- | The revision ID was not specified.
_RevisionRequiredException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_RevisionRequiredException =
  Prelude._MatchServiceError
    defaultService
    "RevisionRequiredException"

-- | The specified instance does not exist in the deployment group.
_InstanceDoesNotExistException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InstanceDoesNotExistException =
  Prelude._MatchServiceError
    defaultService
    "InstanceDoesNotExistException"

-- | The deployment is already complete.
_DeploymentAlreadyCompletedException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_DeploymentAlreadyCompletedException =
  Prelude._MatchServiceError
    defaultService
    "DeploymentAlreadyCompletedException"

-- | The Amazon ECS service is associated with more than one deployment
-- groups. An Amazon ECS service can be associated with only one deployment
-- group.
_ECSServiceMappingLimitExceededException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ECSServiceMappingLimitExceededException =
  Prelude._MatchServiceError
    defaultService
    "ECSServiceMappingLimitExceededException"

-- | The deployment with the IAM user or AWS account does not exist.
_DeploymentDoesNotExistException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_DeploymentDoesNotExistException =
  Prelude._MatchServiceError
    defaultService
    "DeploymentDoesNotExistException"

-- | The maximum number of names or IDs allowed for this request (100) was
-- exceeded.
_BatchLimitExceededException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_BatchLimitExceededException =
  Prelude._MatchServiceError
    defaultService
    "BatchLimitExceededException"

-- | At least one of the deployment IDs was specified in an invalid format.
_InvalidDeploymentIdException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidDeploymentIdException =
  Prelude._MatchServiceError
    defaultService
    "InvalidDeploymentIdException"

-- | The deployment configuration name was specified in an invalid format.
_InvalidDeploymentConfigNameException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidDeploymentConfigNameException =
  Prelude._MatchServiceError
    defaultService
    "InvalidDeploymentConfigNameException"

-- | The specified deployment has not started.
_DeploymentNotStartedException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_DeploymentNotStartedException =
  Prelude._MatchServiceError
    defaultService
    "DeploymentNotStartedException"

-- | The deployment does not have a status of Ready and can\'t continue yet.
_DeploymentIsNotInReadyStateException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_DeploymentIsNotInReadyStateException =
  Prelude._MatchServiceError
    defaultService
    "DeploymentIsNotInReadyStateException"

-- |
_InvalidInstanceIdException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidInstanceIdException =
  Prelude._MatchServiceError
    defaultService
    "InvalidInstanceIdException"

-- | The named deployment group with the IAM user or AWS account does not
-- exist.
_DeploymentGroupDoesNotExistException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_DeploymentGroupDoesNotExistException =
  Prelude._MatchServiceError
    defaultService
    "DeploymentGroupDoesNotExistException"

-- | The role ID was not specified.
_RoleRequiredException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_RoleRequiredException =
  Prelude._MatchServiceError
    defaultService
    "RoleRequiredException"
