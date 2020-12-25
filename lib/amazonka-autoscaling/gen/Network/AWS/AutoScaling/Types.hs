-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AutoScaling.Types
  ( -- * Service configuration
    mkServiceConfig,

    -- * Errors
    _InstanceRefreshInProgressFault,
    _AlreadyExistsFault,
    _LimitExceededFault,
    _ResourceInUseFault,
    _InvalidNextToken,
    _ScalingActivityInProgressFault,
    _ResourceContentionFault,
    _ServiceLinkedRoleFailure,
    _ActiveInstanceRefreshNotFoundFault,

    -- * InstanceRefresh
    InstanceRefresh (..),
    mkInstanceRefresh,
    irAutoScalingGroupName,
    irEndTime,
    irInstanceRefreshId,
    irInstancesToUpdate,
    irPercentageComplete,
    irStartTime,
    irStatus,
    irStatusReason,

    -- * XmlString
    XmlString (..),

    -- * MetricType
    MetricType (..),

    -- * TagDescription
    TagDescription (..),
    mkTagDescription,
    tdKey,
    tdPropagateAtLaunch,
    tdResourceId,
    tdResourceType,
    tdValue,

    -- * XmlStringMaxLen1023
    XmlStringMaxLen1023 (..),

    -- * PredefinedMetricSpecification
    PredefinedMetricSpecification (..),
    mkPredefinedMetricSpecification,
    pmsPredefinedMetricType,
    pmsResourceLabel,

    -- * Tag
    Tag (..),
    mkTag,
    tKey,
    tPropagateAtLaunch,
    tResourceId,
    tResourceType,
    tValue,

    -- * XmlStringMaxLen511
    XmlStringMaxLen511 (..),

    -- * LaunchTemplateOverrides
    LaunchTemplateOverrides (..),
    mkLaunchTemplateOverrides,
    ltoInstanceType,
    ltoLaunchTemplateSpecification,
    ltoWeightedCapacity,

    -- * LaunchTemplateName
    LaunchTemplateName (..),

    -- * NotificationConfiguration
    NotificationConfiguration (..),
    mkNotificationConfiguration,
    ncAutoScalingGroupName,
    ncNotificationType,
    ncTopicARN,

    -- * BlockDeviceMapping
    BlockDeviceMapping (..),
    mkBlockDeviceMapping,
    bdmDeviceName,
    bdmEbs,
    bdmNoDevice,
    bdmVirtualName,

    -- * LaunchConfiguration
    LaunchConfiguration (..),
    mkLaunchConfiguration,
    lcLaunchConfigurationName,
    lcImageId,
    lcInstanceType,
    lcCreatedTime,
    lcAssociatePublicIpAddress,
    lcBlockDeviceMappings,
    lcClassicLinkVPCId,
    lcClassicLinkVPCSecurityGroups,
    lcEbsOptimized,
    lcIamInstanceProfile,
    lcInstanceMonitoring,
    lcKernelId,
    lcKeyName,
    lcLaunchConfigurationARN,
    lcMetadataOptions,
    lcPlacementTenancy,
    lcRamdiskId,
    lcSecurityGroups,
    lcSpotPrice,
    lcUserData,

    -- * XmlStringMaxLen64
    XmlStringMaxLen64 (..),

    -- * AutoScalingGroup
    AutoScalingGroup (..),
    mkAutoScalingGroup,
    asgAutoScalingGroupName,
    asgMinSize,
    asgMaxSize,
    asgDesiredCapacity,
    asgDefaultCooldown,
    asgAvailabilityZones,
    asgHealthCheckType,
    asgCreatedTime,
    asgAutoScalingGroupARN,
    asgCapacityRebalance,
    asgEnabledMetrics,
    asgHealthCheckGracePeriod,
    asgInstances,
    asgLaunchConfigurationName,
    asgLaunchTemplate,
    asgLoadBalancerNames,
    asgMaxInstanceLifetime,
    asgMixedInstancesPolicy,
    asgNewInstancesProtectedFromScaleIn,
    asgPlacementGroup,
    asgServiceLinkedRoleARN,
    asgStatus,
    asgSuspendedProcesses,
    asgTags,
    asgTargetGroupARNs,
    asgTerminationPolicies,
    asgVPCZoneIdentifier,

    -- * ResourceName
    ResourceName (..),

    -- * SpotPrice
    SpotPrice (..),

    -- * ScalingPolicy
    ScalingPolicy (..),
    mkScalingPolicy,
    spAdjustmentType,
    spAlarms,
    spAutoScalingGroupName,
    spCooldown,
    spEnabled,
    spEstimatedInstanceWarmup,
    spMetricAggregationType,
    spMinAdjustmentMagnitude,
    spMinAdjustmentStep,
    spPolicyARN,
    spPolicyName,
    spPolicyType,
    spScalingAdjustment,
    spStepAdjustments,
    spTargetTrackingConfiguration,

    -- * XmlStringUserData
    XmlStringUserData (..),

    -- * XmlStringMaxLen255
    XmlStringMaxLen255 (..),

    -- * InstanceMonitoring
    InstanceMonitoring (..),
    mkInstanceMonitoring,
    imEnabled,

    -- * AsciiStringMaxLen255
    AsciiStringMaxLen255 (..),

    -- * ScheduledUpdateGroupActionRequest
    ScheduledUpdateGroupActionRequest (..),
    mkScheduledUpdateGroupActionRequest,
    sugarScheduledActionName,
    sugarDesiredCapacity,
    sugarEndTime,
    sugarMaxSize,
    sugarMinSize,
    sugarRecurrence,
    sugarStartTime,

    -- * CustomizedMetricSpecification
    CustomizedMetricSpecification (..),
    mkCustomizedMetricSpecification,
    cmsMetricName,
    cmsNamespace,
    cmsStatistic,
    cmsDimensions,
    cmsUnit,

    -- * InstanceMetadataEndpointState
    InstanceMetadataEndpointState (..),

    -- * InstanceRefreshStatus
    InstanceRefreshStatus (..),

    -- * XmlStringMaxLen1600
    XmlStringMaxLen1600 (..),

    -- * MetricDimensionName
    MetricDimensionName (..),

    -- * LifecycleActionToken
    LifecycleActionToken (..),

    -- * MetricName
    MetricName (..),

    -- * FailedScheduledUpdateGroupActionRequest
    FailedScheduledUpdateGroupActionRequest (..),
    mkFailedScheduledUpdateGroupActionRequest,
    fsugarScheduledActionName,
    fsugarErrorCode,
    fsugarErrorMessage,

    -- * TargetTrackingConfiguration
    TargetTrackingConfiguration (..),
    mkTargetTrackingConfiguration,
    ttcTargetValue,
    ttcCustomizedMetricSpecification,
    ttcDisableScaleIn,
    ttcPredefinedMetricSpecification,

    -- * XmlStringMaxLen2047
    XmlStringMaxLen2047 (..),

    -- * ScheduledUpdateGroupAction
    ScheduledUpdateGroupAction (..),
    mkScheduledUpdateGroupAction,
    sugaAutoScalingGroupName,
    sugaDesiredCapacity,
    sugaEndTime,
    sugaMaxSize,
    sugaMinSize,
    sugaRecurrence,
    sugaScheduledActionARN,
    sugaScheduledActionName,
    sugaStartTime,
    sugaTime,

    -- * LifecycleActionResult
    LifecycleActionResult (..),

    -- * MetricDimension
    MetricDimension (..),
    mkMetricDimension,
    mdName,
    mdValue,

    -- * ScalingProcessQuery
    ScalingProcessQuery (..),
    mkScalingProcessQuery,
    spqAutoScalingGroupName,
    spqScalingProcesses,

    -- * InstanceMetadataOptions
    InstanceMetadataOptions (..),
    mkInstanceMetadataOptions,
    imoHttpEndpoint,
    imoHttpPutResponseHopLimit,
    imoHttpTokens,

    -- * Ebs
    Ebs (..),
    mkEbs,
    eDeleteOnTermination,
    eEncrypted,
    eIops,
    eSnapshotId,
    eVolumeSize,
    eVolumeType,

    -- * MixedInstancesPolicy
    MixedInstancesPolicy (..),
    mkMixedInstancesPolicy,
    mipInstancesDistribution,
    mipLaunchTemplate,

    -- * AdjustmentType
    AdjustmentType (..),
    mkAdjustmentType,
    atAdjustmentType,

    -- * XmlStringMaxLen19
    XmlStringMaxLen19 (..),

    -- * MetricStatistic
    MetricStatistic (..),

    -- * MetricCollectionType
    MetricCollectionType (..),
    mkMetricCollectionType,
    mctMetric,

    -- * LoadBalancerState
    LoadBalancerState (..),
    mkLoadBalancerState,
    lbsLoadBalancerName,
    lbsState,

    -- * LaunchTemplate
    LaunchTemplate (..),
    mkLaunchTemplate,
    ltLaunchTemplateSpecification,
    ltOverrides,

    -- * LifecycleHook
    LifecycleHook (..),
    mkLifecycleHook,
    lhAutoScalingGroupName,
    lhDefaultResult,
    lhGlobalTimeout,
    lhHeartbeatTimeout,
    lhLifecycleHookName,
    lhLifecycleTransition,
    lhNotificationMetadata,
    lhNotificationTargetARN,
    lhRoleARN,

    -- * StepAdjustment
    StepAdjustment (..),
    mkStepAdjustment,
    saScalingAdjustment,
    saMetricIntervalLowerBound,
    saMetricIntervalUpperBound,

    -- * RefreshPreferences
    RefreshPreferences (..),
    mkRefreshPreferences,
    rpInstanceWarmup,
    rpMinHealthyPercentage,

    -- * Activity
    Activity (..),
    mkActivity,
    aActivityId,
    aAutoScalingGroupName,
    aCause,
    aStartTime,
    aStatusCode,
    aDescription,
    aDetails,
    aEndTime,
    aProgress,
    aStatusMessage,

    -- * SuspendedProcess
    SuspendedProcess (..),
    mkSuspendedProcess,
    spProcessName,
    spSuspensionReason,

    -- * LaunchTemplateSpecification
    LaunchTemplateSpecification (..),
    mkLaunchTemplateSpecification,
    ltsLaunchTemplateId,
    ltsLaunchTemplateName,
    ltsVersion,

    -- * MetricGranularityType
    MetricGranularityType (..),
    mkMetricGranularityType,
    mgtGranularity,

    -- * Filter
    Filter (..),
    mkFilter,
    fName,
    fValues,

    -- * LifecycleHookSpecification
    LifecycleHookSpecification (..),
    mkLifecycleHookSpecification,
    lhsLifecycleHookName,
    lhsLifecycleTransition,
    lhsDefaultResult,
    lhsHeartbeatTimeout,
    lhsNotificationMetadata,
    lhsNotificationTargetARN,
    lhsRoleARN,

    -- * InstancesDistribution
    InstancesDistribution (..),
    mkInstancesDistribution,
    idOnDemandAllocationStrategy,
    idOnDemandBaseCapacity,
    idOnDemandPercentageAboveBaseCapacity,
    idSpotAllocationStrategy,
    idSpotInstancePools,
    idSpotMaxPrice,

    -- * InstanceMetadataHttpTokensState
    InstanceMetadataHttpTokensState (..),

    -- * ProcessType
    ProcessType (..),
    mkProcessType,
    ptProcessName,

    -- * LoadBalancerTargetGroupState
    LoadBalancerTargetGroupState (..),
    mkLoadBalancerTargetGroupState,
    lbtgsLoadBalancerTargetGroupARN,
    lbtgsState,

    -- * Alarm
    Alarm (..),
    mkAlarm,
    aAlarmARN,
    aAlarmName,

    -- * RefreshStrategy
    RefreshStrategy (..),

    -- * XmlStringMaxLen32
    XmlStringMaxLen32 (..),

    -- * EnabledMetric
    EnabledMetric (..),
    mkEnabledMetric,
    emGranularity,
    emMetric,

    -- * LifecycleTransition
    LifecycleTransition (..),

    -- * Instance
    Instance (..),
    mkInstance,
    iInstanceId,
    iAvailabilityZone,
    iLifecycleState,
    iHealthStatus,
    iProtectedFromScaleIn,
    iInstanceType,
    iLaunchConfigurationName,
    iLaunchTemplate,
    iWeightedCapacity,

    -- * LifecycleState
    LifecycleState (..),

    -- * AutoScalingInstanceDetails
    AutoScalingInstanceDetails (..),
    mkAutoScalingInstanceDetails,
    asidInstanceId,
    asidAutoScalingGroupName,
    asidAvailabilityZone,
    asidLifecycleState,
    asidHealthStatus,
    asidProtectedFromScaleIn,
    asidInstanceType,
    asidLaunchConfigurationName,
    asidLaunchTemplate,
    asidWeightedCapacity,

    -- * ScalingActivityStatusCode
    ScalingActivityStatusCode (..),

    -- * LifecycleHookName
    LifecycleHookName (..),

    -- * AutoScalingGroupName
    AutoScalingGroupName (..),

    -- * InstanceRefreshId
    InstanceRefreshId (..),

    -- * StatusReason
    StatusReason (..),

    -- * HealthCheckType
    HealthCheckType (..),

    -- * LaunchConfigurationName
    LaunchConfigurationName (..),

    -- * PlacementGroup
    PlacementGroup (..),

    -- * ServiceLinkedRoleARN
    ServiceLinkedRoleARN (..),

    -- * VPCZoneIdentifier
    VPCZoneIdentifier (..),

    -- * Key
    Key (..),

    -- * Value
    Value (..),

    -- * DefaultResult
    DefaultResult (..),

    -- * NotificationMetadata
    NotificationMetadata (..),

    -- * NotificationTargetARN
    NotificationTargetARN (..),

    -- * RoleARN
    RoleARN (..),

    -- * PolicyName
    PolicyName (..),

    -- * InstanceType
    InstanceType (..),

    -- * WeightedCapacity
    WeightedCapacity (..),

    -- * InstanceId
    InstanceId (..),

    -- * NotificationType
    NotificationType (..),

    -- * TopicARN
    TopicARN (..),

    -- * DeviceName
    DeviceName (..),

    -- * VirtualName
    VirtualName (..),

    -- * ImageId
    ImageId (..),

    -- * ClassicLinkVPCId
    ClassicLinkVPCId (..),

    -- * IamInstanceProfile
    IamInstanceProfile (..),

    -- * KernelId
    KernelId (..),

    -- * KeyName
    KeyName (..),

    -- * LaunchConfigurationARN
    LaunchConfigurationARN (..),

    -- * PlacementTenancy
    PlacementTenancy (..),

    -- * RamdiskId
    RamdiskId (..),

    -- * UserData
    UserData (..),

    -- * AutoScalingGroupARN
    AutoScalingGroupARN (..),

    -- * Status
    Status (..),

    -- * HealthStatus
    HealthStatus (..),

    -- * MetricAggregationType
    MetricAggregationType (..),

    -- * Namespace
    Namespace (..),

    -- * Unit
    Unit (..),

    -- * VolumeType
    VolumeType (..),

    -- * SpotMaxPrice
    SpotMaxPrice (..),
  )
where

import Network.AWS.AutoScaling.Types.Activity
import Network.AWS.AutoScaling.Types.AdjustmentType
import Network.AWS.AutoScaling.Types.Alarm
import Network.AWS.AutoScaling.Types.AsciiStringMaxLen255
import Network.AWS.AutoScaling.Types.AutoScalingGroup
import Network.AWS.AutoScaling.Types.AutoScalingGroupARN
import Network.AWS.AutoScaling.Types.AutoScalingGroupName
import Network.AWS.AutoScaling.Types.AutoScalingInstanceDetails
import Network.AWS.AutoScaling.Types.BlockDeviceMapping
import Network.AWS.AutoScaling.Types.ClassicLinkVPCId
import Network.AWS.AutoScaling.Types.CustomizedMetricSpecification
import Network.AWS.AutoScaling.Types.DefaultResult
import Network.AWS.AutoScaling.Types.DeviceName
import Network.AWS.AutoScaling.Types.Ebs
import Network.AWS.AutoScaling.Types.EnabledMetric
import Network.AWS.AutoScaling.Types.FailedScheduledUpdateGroupActionRequest
import Network.AWS.AutoScaling.Types.Filter
import Network.AWS.AutoScaling.Types.HealthCheckType
import Network.AWS.AutoScaling.Types.HealthStatus
import Network.AWS.AutoScaling.Types.IamInstanceProfile
import Network.AWS.AutoScaling.Types.ImageId
import Network.AWS.AutoScaling.Types.Instance
import Network.AWS.AutoScaling.Types.InstanceId
import Network.AWS.AutoScaling.Types.InstanceMetadataEndpointState
import Network.AWS.AutoScaling.Types.InstanceMetadataHttpTokensState
import Network.AWS.AutoScaling.Types.InstanceMetadataOptions
import Network.AWS.AutoScaling.Types.InstanceMonitoring
import Network.AWS.AutoScaling.Types.InstanceRefresh
import Network.AWS.AutoScaling.Types.InstanceRefreshId
import Network.AWS.AutoScaling.Types.InstanceRefreshStatus
import Network.AWS.AutoScaling.Types.InstanceType
import Network.AWS.AutoScaling.Types.InstancesDistribution
import Network.AWS.AutoScaling.Types.KernelId
import Network.AWS.AutoScaling.Types.Key
import Network.AWS.AutoScaling.Types.KeyName
import Network.AWS.AutoScaling.Types.LaunchConfiguration
import Network.AWS.AutoScaling.Types.LaunchConfigurationARN
import Network.AWS.AutoScaling.Types.LaunchConfigurationName
import Network.AWS.AutoScaling.Types.LaunchTemplate
import Network.AWS.AutoScaling.Types.LaunchTemplateName
import Network.AWS.AutoScaling.Types.LaunchTemplateOverrides
import Network.AWS.AutoScaling.Types.LaunchTemplateSpecification
import Network.AWS.AutoScaling.Types.LifecycleActionResult
import Network.AWS.AutoScaling.Types.LifecycleActionToken
import Network.AWS.AutoScaling.Types.LifecycleHook
import Network.AWS.AutoScaling.Types.LifecycleHookName
import Network.AWS.AutoScaling.Types.LifecycleHookSpecification
import Network.AWS.AutoScaling.Types.LifecycleState
import Network.AWS.AutoScaling.Types.LifecycleTransition
import Network.AWS.AutoScaling.Types.LoadBalancerState
import Network.AWS.AutoScaling.Types.LoadBalancerTargetGroupState
import Network.AWS.AutoScaling.Types.MetricAggregationType
import Network.AWS.AutoScaling.Types.MetricCollectionType
import Network.AWS.AutoScaling.Types.MetricDimension
import Network.AWS.AutoScaling.Types.MetricDimensionName
import Network.AWS.AutoScaling.Types.MetricGranularityType
import Network.AWS.AutoScaling.Types.MetricName
import Network.AWS.AutoScaling.Types.MetricStatistic
import Network.AWS.AutoScaling.Types.MetricType
import Network.AWS.AutoScaling.Types.MixedInstancesPolicy
import Network.AWS.AutoScaling.Types.Namespace
import Network.AWS.AutoScaling.Types.NotificationConfiguration
import Network.AWS.AutoScaling.Types.NotificationMetadata
import Network.AWS.AutoScaling.Types.NotificationTargetARN
import Network.AWS.AutoScaling.Types.NotificationType
import Network.AWS.AutoScaling.Types.PlacementGroup
import Network.AWS.AutoScaling.Types.PlacementTenancy
import Network.AWS.AutoScaling.Types.PolicyName
import Network.AWS.AutoScaling.Types.PredefinedMetricSpecification
import Network.AWS.AutoScaling.Types.ProcessType
import Network.AWS.AutoScaling.Types.RamdiskId
import Network.AWS.AutoScaling.Types.RefreshPreferences
import Network.AWS.AutoScaling.Types.RefreshStrategy
import Network.AWS.AutoScaling.Types.ResourceName
import Network.AWS.AutoScaling.Types.RoleARN
import Network.AWS.AutoScaling.Types.ScalingActivityStatusCode
import Network.AWS.AutoScaling.Types.ScalingPolicy
import Network.AWS.AutoScaling.Types.ScalingProcessQuery
import Network.AWS.AutoScaling.Types.ScheduledUpdateGroupAction
import Network.AWS.AutoScaling.Types.ScheduledUpdateGroupActionRequest
import Network.AWS.AutoScaling.Types.ServiceLinkedRoleARN
import Network.AWS.AutoScaling.Types.SpotMaxPrice
import Network.AWS.AutoScaling.Types.SpotPrice
import Network.AWS.AutoScaling.Types.Status
import Network.AWS.AutoScaling.Types.StatusReason
import Network.AWS.AutoScaling.Types.StepAdjustment
import Network.AWS.AutoScaling.Types.SuspendedProcess
import Network.AWS.AutoScaling.Types.Tag
import Network.AWS.AutoScaling.Types.TagDescription
import Network.AWS.AutoScaling.Types.TargetTrackingConfiguration
import Network.AWS.AutoScaling.Types.TopicARN
import Network.AWS.AutoScaling.Types.Unit
import Network.AWS.AutoScaling.Types.UserData
import Network.AWS.AutoScaling.Types.VPCZoneIdentifier
import Network.AWS.AutoScaling.Types.Value
import Network.AWS.AutoScaling.Types.VirtualName
import Network.AWS.AutoScaling.Types.VolumeType
import Network.AWS.AutoScaling.Types.WeightedCapacity
import Network.AWS.AutoScaling.Types.XmlString
import Network.AWS.AutoScaling.Types.XmlStringMaxLen1023
import Network.AWS.AutoScaling.Types.XmlStringMaxLen1600
import Network.AWS.AutoScaling.Types.XmlStringMaxLen19
import Network.AWS.AutoScaling.Types.XmlStringMaxLen2047
import Network.AWS.AutoScaling.Types.XmlStringMaxLen255
import Network.AWS.AutoScaling.Types.XmlStringMaxLen32
import Network.AWS.AutoScaling.Types.XmlStringMaxLen511
import Network.AWS.AutoScaling.Types.XmlStringMaxLen64
import Network.AWS.AutoScaling.Types.XmlStringUserData
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2011-01-01@ of the Amazon Auto Scaling SDK configuration.
mkServiceConfig :: Core.Service
mkServiceConfig =
  Core.Service
    { Core._svcAbbrev = "AutoScaling",
      Core._svcSigner = Sign.v4,
      Core._svcPrefix = "autoscaling",
      Core._svcVersion = "2011-01-01",
      Core._svcTimeout = Core.Just 70,
      Core._svcCheck = Core.statusSuccess,
      Core._svcRetry = retry,
      Core._svcError = Core.parseXMLError "AutoScaling",
      Core._svcEndpoint = Core.defaultEndpoint mkServiceConfig
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
          (Core.hasCode "ThrottledException" Core.. Core.hasStatus 400)
          e =
        Core.Just "throttled_exception"
      | Lens.has (Core.hasStatus 429) e = Core.Just "too_many_requests"
      | Lens.has
          (Core.hasCode "ThrottlingException" Core.. Core.hasStatus 400)
          e =
        Core.Just "throttling_exception"
      | Lens.has (Core.hasCode "Throttling" Core.. Core.hasStatus 400) e =
        Core.Just "throttling"
      | Lens.has
          ( Core.hasCode "ProvisionedThroughputExceededException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 504) e = Core.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 502) e = Core.Just "bad_gateway"
      | Lens.has (Core.hasStatus 503) e = Core.Just "service_unavailable"
      | Lens.has (Core.hasStatus 500) e =
        Core.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e = Core.Just "limit_exceeded"
      | Core.otherwise = Core.Nothing

-- | The request failed because an active instance refresh operation already exists for the specified Auto Scaling group.
_InstanceRefreshInProgressFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InstanceRefreshInProgressFault =
  Core._MatchServiceError
    mkServiceConfig
    "InstanceRefreshInProgress"
    Core.. Core.hasStatues 400
{-# DEPRECATED _InstanceRefreshInProgressFault "Use generic-lens or generic-optics instead." #-}

-- | You already have an Auto Scaling group or launch configuration with this name.
_AlreadyExistsFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_AlreadyExistsFault =
  Core._MatchServiceError mkServiceConfig "AlreadyExists"
    Core.. Core.hasStatues 400
{-# DEPRECATED _AlreadyExistsFault "Use generic-lens or generic-optics instead." #-}

-- | You have already reached a limit for your Amazon EC2 Auto Scaling resources (for example, Auto Scaling groups, launch configurations, or lifecycle hooks). For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/APIReference/API_DescribeAccountLimits.html DescribeAccountLimits> in the /Amazon EC2 Auto Scaling API Reference/ .
_LimitExceededFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_LimitExceededFault =
  Core._MatchServiceError mkServiceConfig "LimitExceeded"
    Core.. Core.hasStatues 400
{-# DEPRECATED _LimitExceededFault "Use generic-lens or generic-optics instead." #-}

-- | The operation can't be performed because the resource is in use.
_ResourceInUseFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ResourceInUseFault =
  Core._MatchServiceError mkServiceConfig "ResourceInUse"
    Core.. Core.hasStatues 400
{-# DEPRECATED _ResourceInUseFault "Use generic-lens or generic-optics instead." #-}

-- | The @NextToken@ value is not valid.
_InvalidNextToken :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidNextToken =
  Core._MatchServiceError mkServiceConfig "InvalidNextToken"
    Core.. Core.hasStatues 400
{-# DEPRECATED _InvalidNextToken "Use generic-lens or generic-optics instead." #-}

-- | The operation can't be performed because there are scaling activities in progress.
_ScalingActivityInProgressFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ScalingActivityInProgressFault =
  Core._MatchServiceError
    mkServiceConfig
    "ScalingActivityInProgress"
    Core.. Core.hasStatues 400
{-# DEPRECATED _ScalingActivityInProgressFault "Use generic-lens or generic-optics instead." #-}

-- | You already have a pending update to an Amazon EC2 Auto Scaling resource (for example, an Auto Scaling group, instance, or load balancer).
_ResourceContentionFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ResourceContentionFault =
  Core._MatchServiceError mkServiceConfig "ResourceContention"
    Core.. Core.hasStatues 500
{-# DEPRECATED _ResourceContentionFault "Use generic-lens or generic-optics instead." #-}

-- | The service-linked role is not yet ready for use.
_ServiceLinkedRoleFailure :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ServiceLinkedRoleFailure =
  Core._MatchServiceError
    mkServiceConfig
    "ServiceLinkedRoleFailure"
    Core.. Core.hasStatues 500
{-# DEPRECATED _ServiceLinkedRoleFailure "Use generic-lens or generic-optics instead." #-}

-- | The request failed because an active instance refresh for the specified Auto Scaling group was not found.
_ActiveInstanceRefreshNotFoundFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ActiveInstanceRefreshNotFoundFault =
  Core._MatchServiceError
    mkServiceConfig
    "ActiveInstanceRefreshNotFound"
    Core.. Core.hasStatues 400
{-# DEPRECATED _ActiveInstanceRefreshNotFoundFault "Use generic-lens or generic-optics instead." #-}
