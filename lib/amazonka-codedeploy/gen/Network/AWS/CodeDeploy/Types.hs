{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeDeploy.Types
  ( -- * Service Configuration
    codeDeploy,

    -- * Errors

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
    Alarm,
    alarm,
    aName,

    -- * AlarmConfiguration
    AlarmConfiguration,
    alarmConfiguration,
    acIgnorePollAlarmFailure,
    acEnabled,
    acAlarms,

    -- * AppSpecContent
    AppSpecContent,
    appSpecContent,
    ascContent,
    ascSha256,

    -- * ApplicationInfo
    ApplicationInfo,
    applicationInfo,
    aiLinkedToGitHub,
    aiComputePlatform,
    aiApplicationId,
    aiApplicationName,
    aiGitHubAccountName,
    aiCreateTime,

    -- * AutoRollbackConfiguration
    AutoRollbackConfiguration,
    autoRollbackConfiguration,
    arcEnabled,
    arcEvents,

    -- * AutoScalingGroup
    AutoScalingGroup,
    autoScalingGroup,
    asgHook,
    asgName,

    -- * BlueGreenDeploymentConfiguration
    BlueGreenDeploymentConfiguration,
    blueGreenDeploymentConfiguration,
    bgdcDeploymentReadyOption,
    bgdcGreenFleetProvisioningOption,
    bgdcTerminateBlueInstancesOnDeploymentSuccess,

    -- * BlueInstanceTerminationOption
    BlueInstanceTerminationOption,
    blueInstanceTerminationOption,
    bitoAction,
    bitoTerminationWaitTimeInMinutes,

    -- * CloudFormationTarget
    CloudFormationTarget,
    cloudFormationTarget,
    cftTargetId,
    cftStatus,
    cftDeploymentId,
    cftResourceType,
    cftLastUpdatedAt,
    cftLifecycleEvents,
    cftTargetVersionWeight,

    -- * DeploymentConfigInfo
    DeploymentConfigInfo,
    deploymentConfigInfo,
    dciDeploymentConfigName,
    dciComputePlatform,
    dciMinimumHealthyHosts,
    dciTrafficRoutingConfig,
    dciDeploymentConfigId,
    dciCreateTime,

    -- * DeploymentGroupInfo
    DeploymentGroupInfo,
    deploymentGroupInfo,
    dgiServiceRoleARN,
    dgiEc2TagSet,
    dgiDeploymentConfigName,
    dgiLastAttemptedDeployment,
    dgiOnPremisesTagSet,
    dgiComputePlatform,
    dgiTargetRevision,
    dgiEc2TagFilters,
    dgiEcsServices,
    dgiBlueGreenDeploymentConfiguration,
    dgiLoadBalancerInfo,
    dgiOnPremisesInstanceTagFilters,
    dgiLastSuccessfulDeployment,
    dgiApplicationName,
    dgiAlarmConfiguration,
    dgiTriggerConfigurations,
    dgiDeploymentGroupId,
    dgiAutoScalingGroups,
    dgiDeploymentStyle,
    dgiAutoRollbackConfiguration,
    dgiDeploymentGroupName,

    -- * DeploymentInfo
    DeploymentInfo,
    deploymentInfo,
    diCreator,
    diStatus,
    diDeploymentId,
    diDeploymentConfigName,
    diComputePlatform,
    diPreviousRevision,
    diInstanceTerminationWaitTimeStarted,
    diDeploymentStatusMessages,
    diStartTime,
    diCompleteTime,
    diBlueGreenDeploymentConfiguration,
    diErrorInformation,
    diLoadBalancerInfo,
    diAdditionalDeploymentStatusInfo,
    diDeploymentOverview,
    diFileExistsBehavior,
    diApplicationName,
    diRollbackInfo,
    diExternalId,
    diTargetInstances,
    diRevision,
    diDescription,
    diDeploymentStyle,
    diCreateTime,
    diAutoRollbackConfiguration,
    diUpdateOutdatedInstancesOnly,
    diDeploymentGroupName,
    diIgnoreApplicationStopFailures,

    -- * DeploymentOverview
    DeploymentOverview,
    deploymentOverview,
    doPending,
    doSkipped,
    doInProgress,
    doSucceeded,
    doReady,
    doFailed,

    -- * DeploymentReadyOption
    DeploymentReadyOption,
    deploymentReadyOption,
    droActionOnTimeout,
    droWaitTimeInMinutes,

    -- * DeploymentStyle
    DeploymentStyle,
    deploymentStyle,
    dsDeploymentOption,
    dsDeploymentType,

    -- * DeploymentTarget
    DeploymentTarget,
    deploymentTarget,
    dtInstanceTarget,
    dtCloudFormationTarget,
    dtEcsTarget,
    dtDeploymentTargetType,
    dtLambdaTarget,

    -- * Diagnostics
    Diagnostics,
    diagnostics,
    dLogTail,
    dErrorCode,
    dScriptName,
    dMessage,

    -- * EC2TagFilter
    EC2TagFilter,
    ec2TagFilter,
    etfValue,
    etfKey,
    etfType,

    -- * EC2TagSet
    EC2TagSet,
    ec2TagSet,
    etsEc2TagSetList,

    -- * ECSService
    ECSService,
    eCSService,
    ecssServiceName,
    ecssClusterName,

    -- * ECSTarget
    ECSTarget,
    eCSTarget,
    ecstTargetARN,
    ecstTargetId,
    ecstStatus,
    ecstDeploymentId,
    ecstLastUpdatedAt,
    ecstTaskSetsInfo,
    ecstLifecycleEvents,

    -- * ECSTaskSet
    ECSTaskSet,
    eCSTaskSet,
    ecstsRunningCount,
    ecstsStatus,
    ecstsIdentifer,
    ecstsDesiredCount,
    ecstsPendingCount,
    ecstsTrafficWeight,
    ecstsTargetGroup,
    ecstsTaskSetLabel,

    -- * ELBInfo
    ELBInfo,
    eLBInfo,
    elbiName,

    -- * ErrorInformation
    ErrorInformation,
    errorInformation,
    eiCode,
    eiMessage,

    -- * GenericRevisionInfo
    GenericRevisionInfo,
    genericRevisionInfo,
    griRegisterTime,
    griFirstUsedTime,
    griDeploymentGroups,
    griLastUsedTime,
    griDescription,

    -- * GitHubLocation
    GitHubLocation,
    gitHubLocation,
    ghlCommitId,
    ghlRepository,

    -- * GreenFleetProvisioningOption
    GreenFleetProvisioningOption,
    greenFleetProvisioningOption,
    gfpoAction,

    -- * InstanceInfo
    InstanceInfo,
    instanceInfo,
    iiRegisterTime,
    iiInstanceARN,
    iiDeregisterTime,
    iiIamUserARN,
    iiInstanceName,
    iiIamSessionARN,
    iiTags,

    -- * InstanceTarget
    InstanceTarget,
    instanceTarget,
    itTargetARN,
    itTargetId,
    itStatus,
    itDeploymentId,
    itInstanceLabel,
    itLastUpdatedAt,
    itLifecycleEvents,

    -- * LambdaFunctionInfo
    LambdaFunctionInfo,
    lambdaFunctionInfo,
    lfiCurrentVersion,
    lfiFunctionAlias,
    lfiFunctionName,
    lfiTargetVersion,
    lfiTargetVersionWeight,

    -- * LambdaTarget
    LambdaTarget,
    lambdaTarget,
    ltTargetARN,
    ltTargetId,
    ltStatus,
    ltDeploymentId,
    ltLastUpdatedAt,
    ltLifecycleEvents,
    ltLambdaFunctionInfo,

    -- * LastDeploymentInfo
    LastDeploymentInfo,
    lastDeploymentInfo,
    ldiStatus,
    ldiDeploymentId,
    ldiEndTime,
    ldiCreateTime,

    -- * LifecycleEvent
    LifecycleEvent,
    lifecycleEvent,
    leStatus,
    leLifecycleEventName,
    leStartTime,
    leDiagnostics,
    leEndTime,

    -- * LoadBalancerInfo
    LoadBalancerInfo,
    loadBalancerInfo,
    lbiElbInfoList,
    lbiTargetGroupInfoList,
    lbiTargetGroupPairInfoList,

    -- * MinimumHealthyHosts
    MinimumHealthyHosts,
    minimumHealthyHosts,
    mhhValue,
    mhhType,

    -- * OnPremisesTagSet
    OnPremisesTagSet,
    onPremisesTagSet,
    optsOnPremisesTagSetList,

    -- * RawString
    RawString,
    rawString,
    rsContent,
    rsSha256,

    -- * RevisionInfo
    RevisionInfo,
    revisionInfo,
    riGenericRevisionInfo,
    riRevisionLocation,

    -- * RevisionLocation
    RevisionLocation,
    revisionLocation,
    rlString,
    rlRevisionType,
    rlS3Location,
    rlAppSpecContent,
    rlGitHubLocation,

    -- * RollbackInfo
    RollbackInfo,
    rollbackInfo,
    riRollbackTriggeringDeploymentId,
    riRollbackMessage,
    riRollbackDeploymentId,

    -- * S3Location
    S3Location,
    s3Location,
    slBundleType,
    slETag,
    slBucket,
    slKey,
    slVersion,

    -- * Tag
    Tag,
    tag,
    tagValue,
    tagKey,

    -- * TagFilter
    TagFilter,
    tagFilter,
    tfValue,
    tfKey,
    tfType,

    -- * TargetGroupInfo
    TargetGroupInfo,
    targetGroupInfo,
    tgiName,

    -- * TargetGroupPairInfo
    TargetGroupPairInfo,
    targetGroupPairInfo,
    tgpiProdTrafficRoute,
    tgpiTestTrafficRoute,
    tgpiTargetGroups,

    -- * TargetInstances
    TargetInstances,
    targetInstances,
    tiEc2TagSet,
    tiTagFilters,
    tiAutoScalingGroups,

    -- * TimeBasedCanary
    TimeBasedCanary,
    timeBasedCanary,
    tbcCanaryInterval,
    tbcCanaryPercentage,

    -- * TimeBasedLinear
    TimeBasedLinear,
    timeBasedLinear,
    tblLinearInterval,
    tblLinearPercentage,

    -- * TimeRange
    TimeRange,
    timeRange,
    trStart,
    trEnd,

    -- * TrafficRoute
    TrafficRoute,
    trafficRoute,
    trListenerARNs,

    -- * TrafficRoutingConfig
    TrafficRoutingConfig,
    trafficRoutingConfig,
    trcTimeBasedCanary,
    trcTimeBasedLinear,
    trcType,

    -- * TriggerConfig
    TriggerConfig,
    triggerConfig,
    tcTriggerName,
    tcTriggerEvents,
    tcTriggerTargetARN,
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
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Sign.V4

-- | API version @2014-10-06@ of the Amazon CodeDeploy SDK configuration.
codeDeploy :: Service
codeDeploy =
  Service
    { _svcAbbrev = "CodeDeploy",
      _svcSigner = v4,
      _svcPrefix = "codedeploy",
      _svcVersion = "2014-10-06",
      _svcEndpoint = defaultEndpoint codeDeploy,
      _svcTimeout = Just 70,
      _svcCheck = statusSuccess,
      _svcError = parseJSONError "CodeDeploy",
      _svcRetry = retry
    }
  where
    retry =
      Exponential
        { _retryBase = 5.0e-2,
          _retryGrowth = 2,
          _retryAttempts = 5,
          _retryCheck = check
        }
    check e
      | has (hasCode "ThrottledException" . hasStatus 400) e =
        Just "throttled_exception"
      | has (hasStatus 429) e = Just "too_many_requests"
      | has (hasCode "ThrottlingException" . hasStatus 400) e =
        Just "throttling_exception"
      | has (hasCode "Throttling" . hasStatus 400) e = Just "throttling"
      | has
          (hasCode "ProvisionedThroughputExceededException" . hasStatus 400)
          e =
        Just "throughput_exceeded"
      | has (hasStatus 504) e = Just "gateway_timeout"
      | has (hasCode "RequestThrottledException" . hasStatus 400) e =
        Just "request_throttled_exception"
      | has (hasStatus 502) e = Just "bad_gateway"
      | has (hasStatus 503) e = Just "service_unavailable"
      | has (hasStatus 500) e = Just "general_server_error"
      | has (hasStatus 509) e = Just "limit_exceeded"
      | otherwise = Nothing
