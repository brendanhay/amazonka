-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeDeploy.Types
  ( -- * Service configuration
    codeDeployService,

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
    Alarm (..),
    mkAlarm,
    aName,

    -- * AlarmConfiguration
    AlarmConfiguration (..),
    mkAlarmConfiguration,
    acIgnorePollAlarmFailure,
    acEnabled,
    acAlarms,

    -- * AppSpecContent
    AppSpecContent (..),
    mkAppSpecContent,
    ascContent,
    ascSha256,

    -- * ApplicationInfo
    ApplicationInfo (..),
    mkApplicationInfo,
    aiLinkedToGitHub,
    aiComputePlatform,
    aiApplicationId,
    aiApplicationName,
    aiGitHubAccountName,
    aiCreateTime,

    -- * AutoRollbackConfiguration
    AutoRollbackConfiguration (..),
    mkAutoRollbackConfiguration,
    arcEnabled,
    arcEvents,

    -- * AutoScalingGroup
    AutoScalingGroup (..),
    mkAutoScalingGroup,
    asgHook,
    asgName,

    -- * BlueGreenDeploymentConfiguration
    BlueGreenDeploymentConfiguration (..),
    mkBlueGreenDeploymentConfiguration,
    bgdcDeploymentReadyOption,
    bgdcGreenFleetProvisioningOption,
    bgdcTerminateBlueInstancesOnDeploymentSuccess,

    -- * BlueInstanceTerminationOption
    BlueInstanceTerminationOption (..),
    mkBlueInstanceTerminationOption,
    bitoAction,
    bitoTerminationWaitTimeInMinutes,

    -- * CloudFormationTarget
    CloudFormationTarget (..),
    mkCloudFormationTarget,
    cftTargetId,
    cftStatus,
    cftDeploymentId,
    cftResourceType,
    cftLastUpdatedAt,
    cftLifecycleEvents,
    cftTargetVersionWeight,

    -- * DeploymentConfigInfo
    DeploymentConfigInfo (..),
    mkDeploymentConfigInfo,
    dciDeploymentConfigName,
    dciComputePlatform,
    dciMinimumHealthyHosts,
    dciTrafficRoutingConfig,
    dciDeploymentConfigId,
    dciCreateTime,

    -- * DeploymentGroupInfo
    DeploymentGroupInfo (..),
    mkDeploymentGroupInfo,
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
    DeploymentInfo (..),
    mkDeploymentInfo,
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
    DeploymentOverview (..),
    mkDeploymentOverview,
    doPending,
    doSkipped,
    doInProgress,
    doSucceeded,
    doReady,
    doFailed,

    -- * DeploymentReadyOption
    DeploymentReadyOption (..),
    mkDeploymentReadyOption,
    droActionOnTimeout,
    droWaitTimeInMinutes,

    -- * DeploymentStyle
    DeploymentStyle (..),
    mkDeploymentStyle,
    dsDeploymentOption,
    dsDeploymentType,

    -- * DeploymentTarget
    DeploymentTarget (..),
    mkDeploymentTarget,
    dtInstanceTarget,
    dtCloudFormationTarget,
    dtEcsTarget,
    dtDeploymentTargetType,
    dtLambdaTarget,

    -- * Diagnostics
    Diagnostics (..),
    mkDiagnostics,
    dLogTail,
    dErrorCode,
    dScriptName,
    dMessage,

    -- * EC2TagFilter
    EC2TagFilter (..),
    mkEC2TagFilter,
    etfValue,
    etfKey,
    etfType,

    -- * EC2TagSet
    EC2TagSet (..),
    mkEC2TagSet,
    etsEc2TagSetList,

    -- * ECSService
    ECSService (..),
    mkECSService,
    ecssServiceName,
    ecssClusterName,

    -- * ECSTarget
    ECSTarget (..),
    mkECSTarget,
    ecstTargetARN,
    ecstTargetId,
    ecstStatus,
    ecstDeploymentId,
    ecstLastUpdatedAt,
    ecstTaskSetsInfo,
    ecstLifecycleEvents,

    -- * ECSTaskSet
    ECSTaskSet (..),
    mkECSTaskSet,
    ecstsRunningCount,
    ecstsStatus,
    ecstsIdentifer,
    ecstsDesiredCount,
    ecstsPendingCount,
    ecstsTrafficWeight,
    ecstsTargetGroup,
    ecstsTaskSetLabel,

    -- * ELBInfo
    ELBInfo (..),
    mkELBInfo,
    elbiName,

    -- * ErrorInformation
    ErrorInformation (..),
    mkErrorInformation,
    eiCode,
    eiMessage,

    -- * GenericRevisionInfo
    GenericRevisionInfo (..),
    mkGenericRevisionInfo,
    griRegisterTime,
    griFirstUsedTime,
    griDeploymentGroups,
    griLastUsedTime,
    griDescription,

    -- * GitHubLocation
    GitHubLocation (..),
    mkGitHubLocation,
    ghlCommitId,
    ghlRepository,

    -- * GreenFleetProvisioningOption
    GreenFleetProvisioningOption (..),
    mkGreenFleetProvisioningOption,
    gfpoAction,

    -- * InstanceInfo
    InstanceInfo (..),
    mkInstanceInfo,
    iiRegisterTime,
    iiInstanceARN,
    iiDeregisterTime,
    iiIamUserARN,
    iiInstanceName,
    iiIamSessionARN,
    iiTags,

    -- * InstanceTarget
    InstanceTarget (..),
    mkInstanceTarget,
    itTargetARN,
    itTargetId,
    itStatus,
    itDeploymentId,
    itInstanceLabel,
    itLastUpdatedAt,
    itLifecycleEvents,

    -- * LambdaFunctionInfo
    LambdaFunctionInfo (..),
    mkLambdaFunctionInfo,
    lfiCurrentVersion,
    lfiFunctionAlias,
    lfiFunctionName,
    lfiTargetVersion,
    lfiTargetVersionWeight,

    -- * LambdaTarget
    LambdaTarget (..),
    mkLambdaTarget,
    ltTargetARN,
    ltTargetId,
    ltStatus,
    ltDeploymentId,
    ltLastUpdatedAt,
    ltLifecycleEvents,
    ltLambdaFunctionInfo,

    -- * LastDeploymentInfo
    LastDeploymentInfo (..),
    mkLastDeploymentInfo,
    ldiStatus,
    ldiDeploymentId,
    ldiEndTime,
    ldiCreateTime,

    -- * LifecycleEvent
    LifecycleEvent (..),
    mkLifecycleEvent,
    leStatus,
    leLifecycleEventName,
    leStartTime,
    leDiagnostics,
    leEndTime,

    -- * LoadBalancerInfo
    LoadBalancerInfo (..),
    mkLoadBalancerInfo,
    lbiElbInfoList,
    lbiTargetGroupInfoList,
    lbiTargetGroupPairInfoList,

    -- * MinimumHealthyHosts
    MinimumHealthyHosts (..),
    mkMinimumHealthyHosts,
    mhhValue,
    mhhType,

    -- * OnPremisesTagSet
    OnPremisesTagSet (..),
    mkOnPremisesTagSet,
    optsOnPremisesTagSetList,

    -- * RawString
    RawString (..),
    mkRawString,
    rsContent,
    rsSha256,

    -- * RevisionInfo
    RevisionInfo (..),
    mkRevisionInfo,
    riGenericRevisionInfo,
    riRevisionLocation,

    -- * RevisionLocation
    RevisionLocation (..),
    mkRevisionLocation,
    rlString,
    rlRevisionType,
    rlS3Location,
    rlAppSpecContent,
    rlGitHubLocation,

    -- * RollbackInfo
    RollbackInfo (..),
    mkRollbackInfo,
    riRollbackTriggeringDeploymentId,
    riRollbackMessage,
    riRollbackDeploymentId,

    -- * S3Location
    S3Location (..),
    mkS3Location,
    slBundleType,
    slETag,
    slBucket,
    slKey,
    slVersion,

    -- * Tag
    Tag (..),
    mkTag,
    tValue,
    tKey,

    -- * TagFilter
    TagFilter (..),
    mkTagFilter,
    tfValue,
    tfKey,
    tfType,

    -- * TargetGroupInfo
    TargetGroupInfo (..),
    mkTargetGroupInfo,
    tgiName,

    -- * TargetGroupPairInfo
    TargetGroupPairInfo (..),
    mkTargetGroupPairInfo,
    tgpiProdTrafficRoute,
    tgpiTestTrafficRoute,
    tgpiTargetGroups,

    -- * TargetInstances
    TargetInstances (..),
    mkTargetInstances,
    tiEc2TagSet,
    tiTagFilters,
    tiAutoScalingGroups,

    -- * TimeBasedCanary
    TimeBasedCanary (..),
    mkTimeBasedCanary,
    tbcCanaryInterval,
    tbcCanaryPercentage,

    -- * TimeBasedLinear
    TimeBasedLinear (..),
    mkTimeBasedLinear,
    tblLinearInterval,
    tblLinearPercentage,

    -- * TimeRange
    TimeRange (..),
    mkTimeRange,
    trStart,
    trEnd,

    -- * TrafficRoute
    TrafficRoute (..),
    mkTrafficRoute,
    trListenerARNs,

    -- * TrafficRoutingConfig
    TrafficRoutingConfig (..),
    mkTrafficRoutingConfig,
    trcTimeBasedCanary,
    trcTimeBasedLinear,
    trcType,

    -- * TriggerConfig
    TriggerConfig (..),
    mkTriggerConfig,
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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2014-10-06@ of the Amazon CodeDeploy SDK configuration.
codeDeployService :: Lude.Service
codeDeployService =
  Lude.Service
    { Lude._svcAbbrev = "CodeDeploy",
      Lude._svcSigner = Sign.v4,
      Lude._svcPrefix = "codedeploy",
      Lude._svcVersion = "2014-10-06",
      Lude._svcEndpoint = Lude.defaultEndpoint codeDeployService,
      Lude._svcTimeout = Lude.Just 70,
      Lude._svcCheck = Lude.statusSuccess,
      Lude._svcError = Lude.parseJSONError "CodeDeploy",
      Lude._svcRetry = retry
    }
  where
    retry =
      Lude.Exponential
        { Lude._retryBase = 5.0e-2,
          Lude._retryGrowth = 2,
          Lude._retryAttempts = 5,
          Lude._retryCheck = check
        }
    check e
      | Lens.has
          (Lude.hasCode "ThrottledException" Lude.. Lude.hasStatus 400)
          e =
        Lude.Just "throttled_exception"
      | Lens.has (Lude.hasStatus 429) e = Lude.Just "too_many_requests"
      | Lens.has
          (Lude.hasCode "ThrottlingException" Lude.. Lude.hasStatus 400)
          e =
        Lude.Just "throttling_exception"
      | Lens.has (Lude.hasCode "Throttling" Lude.. Lude.hasStatus 400) e =
        Lude.Just "throttling"
      | Lens.has
          ( Lude.hasCode "ProvisionedThroughputExceededException"
              Lude.. Lude.hasStatus 400
          )
          e =
        Lude.Just "throughput_exceeded"
      | Lens.has (Lude.hasStatus 504) e = Lude.Just "gateway_timeout"
      | Lens.has
          ( Lude.hasCode "RequestThrottledException"
              Lude.. Lude.hasStatus 400
          )
          e =
        Lude.Just "request_throttled_exception"
      | Lens.has (Lude.hasStatus 502) e = Lude.Just "bad_gateway"
      | Lens.has (Lude.hasStatus 503) e = Lude.Just "service_unavailable"
      | Lens.has (Lude.hasStatus 500) e =
        Lude.Just "general_server_error"
      | Lens.has (Lude.hasStatus 509) e = Lude.Just "limit_exceeded"
      | Lude.otherwise = Lude.Nothing
