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
    autoScalingService,

    -- * Errors

    -- * InstanceMetadataEndpointState
    InstanceMetadataEndpointState (..),

    -- * InstanceMetadataHTTPTokensState
    InstanceMetadataHTTPTokensState (..),

    -- * InstanceRefreshStatus
    InstanceRefreshStatus (..),

    -- * LifecycleState
    LifecycleState (..),

    -- * MetricStatistic
    MetricStatistic (..),

    -- * MetricType
    MetricType (..),

    -- * RefreshStrategy
    RefreshStrategy (..),

    -- * ScalingActivityStatusCode
    ScalingActivityStatusCode (..),

    -- * Activity
    Activity (..),
    mkActivity,
    aProgress,
    aStartTime,
    aActivityId,
    aCause,
    aStatusMessage,
    aAutoScalingGroupName,
    aEndTime,
    aDetails,
    aDescription,
    aStatusCode,

    -- * AdjustmentType
    AdjustmentType (..),
    mkAdjustmentType,
    atAdjustmentType,

    -- * Alarm
    Alarm (..),
    mkAlarm,
    aAlarmName,
    aAlarmARN,

    -- * AutoScalingGroup
    AutoScalingGroup (..),
    mkAutoScalingGroup,
    asgStatus,
    asgTerminationPolicies,
    asgCreatedTime,
    asgHealthCheckGracePeriod,
    asgServiceLinkedRoleARN,
    asgNewInstancesProtectedFromScaleIn,
    asgVPCZoneIdentifier,
    asgTargetGroupARNs,
    asgMaxInstanceLifetime,
    asgDefaultCooldown,
    asgMaxSize,
    asgAvailabilityZones,
    asgDesiredCapacity,
    asgMixedInstancesPolicy,
    asgMinSize,
    asgEnabledMetrics,
    asgAutoScalingGroupName,
    asgLaunchConfigurationName,
    asgInstances,
    asgHealthCheckType,
    asgLaunchTemplate,
    asgCapacityRebalance,
    asgAutoScalingGroupARN,
    asgPlacementGroup,
    asgSuspendedProcesses,
    asgLoadBalancerNames,
    asgTags,

    -- * AutoScalingInstanceDetails
    AutoScalingInstanceDetails (..),
    mkAutoScalingInstanceDetails,
    asidInstanceId,
    asidWeightedCapacity,
    asidProtectedFromScaleIn,
    asidInstanceType,
    asidAvailabilityZone,
    asidAutoScalingGroupName,
    asidLaunchConfigurationName,
    asidLaunchTemplate,
    asidHealthStatus,
    asidLifecycleState,

    -- * BlockDeviceMapping
    BlockDeviceMapping (..),
    mkBlockDeviceMapping,
    bdmVirtualName,
    bdmNoDevice,
    bdmEBS,
    bdmDeviceName,

    -- * CustomizedMetricSpecification
    CustomizedMetricSpecification (..),
    mkCustomizedMetricSpecification,
    cmsMetricName,
    cmsNamespace,
    cmsDimensions,
    cmsUnit,
    cmsStatistic,

    -- * EBS
    EBS (..),
    mkEBS,
    ebsDeleteOnTermination,
    ebsVolumeSize,
    ebsIOPS,
    ebsEncrypted,
    ebsVolumeType,
    ebsSnapshotId,

    -- * EnabledMetric
    EnabledMetric (..),
    mkEnabledMetric,
    emGranularity,
    emMetric,

    -- * FailedScheduledUpdateGroupActionRequest
    FailedScheduledUpdateGroupActionRequest (..),
    mkFailedScheduledUpdateGroupActionRequest,
    fsugarScheduledActionName,
    fsugarErrorCode,
    fsugarErrorMessage,

    -- * Filter
    Filter (..),
    mkFilter,
    fValues,
    fName,

    -- * Instance
    Instance (..),
    mkInstance,
    iInstanceId,
    iWeightedCapacity,
    iProtectedFromScaleIn,
    iInstanceType,
    iAvailabilityZone,
    iLaunchConfigurationName,
    iLaunchTemplate,
    iHealthStatus,
    iLifecycleState,

    -- * InstanceMetadataOptions
    InstanceMetadataOptions (..),
    mkInstanceMetadataOptions,
    imoHTTPEndpoint,
    imoHTTPPutResponseHopLimit,
    imoHTTPTokens,

    -- * InstanceMonitoring
    InstanceMonitoring (..),
    mkInstanceMonitoring,
    imEnabled,

    -- * InstanceRefresh
    InstanceRefresh (..),
    mkInstanceRefresh,
    irStatus,
    irStartTime,
    irInstancesToUpdate,
    irPercentageComplete,
    irAutoScalingGroupName,
    irEndTime,
    irStatusReason,
    irInstanceRefreshId,

    -- * InstancesDistribution
    InstancesDistribution (..),
    mkInstancesDistribution,
    idSpotAllocationStrategy,
    idSpotInstancePools,
    idSpotMaxPrice,
    idOnDemandBaseCapacity,
    idOnDemandAllocationStrategy,
    idOnDemandPercentageAboveBaseCapacity,

    -- * LaunchConfiguration
    LaunchConfiguration (..),
    mkLaunchConfiguration,
    lcAssociatePublicIPAddress,
    lcSecurityGroups,
    lcSpotPrice,
    lcCreatedTime,
    lcInstanceMonitoring,
    lcKeyName,
    lcClassicLinkVPCSecurityGroups,
    lcRAMDiskId,
    lcKernelId,
    lcInstanceType,
    lcEBSOptimized,
    lcUserData,
    lcClassicLinkVPCId,
    lcIAMInstanceProfile,
    lcImageId,
    lcLaunchConfigurationName,
    lcMetadataOptions,
    lcLaunchConfigurationARN,
    lcPlacementTenancy,
    lcBlockDeviceMappings,

    -- * LaunchTemplate
    LaunchTemplate (..),
    mkLaunchTemplate,
    ltOverrides,
    ltLaunchTemplateSpecification,

    -- * LaunchTemplateOverrides
    LaunchTemplateOverrides (..),
    mkLaunchTemplateOverrides,
    ltoWeightedCapacity,
    ltoInstanceType,
    ltoLaunchTemplateSpecification,

    -- * LaunchTemplateSpecification
    LaunchTemplateSpecification (..),
    mkLaunchTemplateSpecification,
    ltsLaunchTemplateName,
    ltsLaunchTemplateId,
    ltsVersion,

    -- * LifecycleHook
    LifecycleHook (..),
    mkLifecycleHook,
    lhDefaultResult,
    lhLifecycleHookName,
    lhHeartbeatTimeout,
    lhAutoScalingGroupName,
    lhNotificationMetadata,
    lhGlobalTimeout,
    lhNotificationTargetARN,
    lhLifecycleTransition,
    lhRoleARN,

    -- * LifecycleHookSpecification
    LifecycleHookSpecification (..),
    mkLifecycleHookSpecification,
    lhsDefaultResult,
    lhsLifecycleHookName,
    lhsHeartbeatTimeout,
    lhsNotificationMetadata,
    lhsNotificationTargetARN,
    lhsLifecycleTransition,
    lhsRoleARN,

    -- * LoadBalancerState
    LoadBalancerState (..),
    mkLoadBalancerState,
    lbsState,
    lbsLoadBalancerName,

    -- * LoadBalancerTargetGroupState
    LoadBalancerTargetGroupState (..),
    mkLoadBalancerTargetGroupState,
    lbtgsState,
    lbtgsLoadBalancerTargetGroupARN,

    -- * MetricCollectionType
    MetricCollectionType (..),
    mkMetricCollectionType,
    mctMetric,

    -- * MetricDimension
    MetricDimension (..),
    mkMetricDimension,
    mdValue,
    mdName,

    -- * MetricGranularityType
    MetricGranularityType (..),
    mkMetricGranularityType,
    mgtGranularity,

    -- * MixedInstancesPolicy
    MixedInstancesPolicy (..),
    mkMixedInstancesPolicy,
    mipLaunchTemplate,
    mipInstancesDistribution,

    -- * NotificationConfiguration
    NotificationConfiguration (..),
    mkNotificationConfiguration,
    ncTopicARN,
    ncAutoScalingGroupName,
    ncNotificationType,

    -- * PredefinedMetricSpecification
    PredefinedMetricSpecification (..),
    mkPredefinedMetricSpecification,
    pmsPredefinedMetricType,
    pmsResourceLabel,

    -- * ProcessType
    ProcessType (..),
    mkProcessType,
    ptProcessName,

    -- * RefreshPreferences
    RefreshPreferences (..),
    mkRefreshPreferences,
    rpMinHealthyPercentage,
    rpInstanceWarmup,

    -- * ScalingPolicy
    ScalingPolicy (..),
    mkScalingPolicy,
    spMinAdjustmentStep,
    spEstimatedInstanceWarmup,
    spPolicyName,
    spEnabled,
    spPolicyType,
    spStepAdjustments,
    spTargetTrackingConfiguration,
    spAdjustmentType,
    spAutoScalingGroupName,
    spScalingAdjustment,
    spCooldown,
    spPolicyARN,
    spAlarms,
    spMetricAggregationType,
    spMinAdjustmentMagnitude,

    -- * ScalingProcessQuery
    ScalingProcessQuery (..),
    mkScalingProcessQuery,
    spqAutoScalingGroupName,
    spqScalingProcesses,

    -- * ScheduledUpdateGroupAction
    ScheduledUpdateGroupAction (..),
    mkScheduledUpdateGroupAction,
    sugaScheduledActionARN,
    sugaStartTime,
    sugaTime,
    sugaScheduledActionName,
    sugaMaxSize,
    sugaRecurrence,
    sugaDesiredCapacity,
    sugaMinSize,
    sugaAutoScalingGroupName,
    sugaEndTime,

    -- * ScheduledUpdateGroupActionRequest
    ScheduledUpdateGroupActionRequest (..),
    mkScheduledUpdateGroupActionRequest,
    sugarStartTime,
    sugarScheduledActionName,
    sugarMaxSize,
    sugarRecurrence,
    sugarDesiredCapacity,
    sugarMinSize,
    sugarEndTime,

    -- * StepAdjustment
    StepAdjustment (..),
    mkStepAdjustment,
    saMetricIntervalLowerBound,
    saMetricIntervalUpperBound,
    saScalingAdjustment,

    -- * SuspendedProcess
    SuspendedProcess (..),
    mkSuspendedProcess,
    spProcessName,
    spSuspensionReason,

    -- * Tag
    Tag (..),
    mkTag,
    tResourceId,
    tResourceType,
    tValue,
    tKey,
    tPropagateAtLaunch,

    -- * TagDescription
    TagDescription (..),
    mkTagDescription,
    tdResourceId,
    tdResourceType,
    tdValue,
    tdKey,
    tdPropagateAtLaunch,

    -- * TargetTrackingConfiguration
    TargetTrackingConfiguration (..),
    mkTargetTrackingConfiguration,
    ttcPredefinedMetricSpecification,
    ttcTargetValue,
    ttcCustomizedMetricSpecification,
    ttcDisableScaleIn,
  )
where

import Network.AWS.AutoScaling.Types.Activity
import Network.AWS.AutoScaling.Types.AdjustmentType
import Network.AWS.AutoScaling.Types.Alarm
import Network.AWS.AutoScaling.Types.AutoScalingGroup
import Network.AWS.AutoScaling.Types.AutoScalingInstanceDetails
import Network.AWS.AutoScaling.Types.BlockDeviceMapping
import Network.AWS.AutoScaling.Types.CustomizedMetricSpecification
import Network.AWS.AutoScaling.Types.EBS
import Network.AWS.AutoScaling.Types.EnabledMetric
import Network.AWS.AutoScaling.Types.FailedScheduledUpdateGroupActionRequest
import Network.AWS.AutoScaling.Types.Filter
import Network.AWS.AutoScaling.Types.Instance
import Network.AWS.AutoScaling.Types.InstanceMetadataEndpointState
import Network.AWS.AutoScaling.Types.InstanceMetadataHTTPTokensState
import Network.AWS.AutoScaling.Types.InstanceMetadataOptions
import Network.AWS.AutoScaling.Types.InstanceMonitoring
import Network.AWS.AutoScaling.Types.InstanceRefresh
import Network.AWS.AutoScaling.Types.InstanceRefreshStatus
import Network.AWS.AutoScaling.Types.InstancesDistribution
import Network.AWS.AutoScaling.Types.LaunchConfiguration
import Network.AWS.AutoScaling.Types.LaunchTemplate
import Network.AWS.AutoScaling.Types.LaunchTemplateOverrides
import Network.AWS.AutoScaling.Types.LaunchTemplateSpecification
import Network.AWS.AutoScaling.Types.LifecycleHook
import Network.AWS.AutoScaling.Types.LifecycleHookSpecification
import Network.AWS.AutoScaling.Types.LifecycleState
import Network.AWS.AutoScaling.Types.LoadBalancerState
import Network.AWS.AutoScaling.Types.LoadBalancerTargetGroupState
import Network.AWS.AutoScaling.Types.MetricCollectionType
import Network.AWS.AutoScaling.Types.MetricDimension
import Network.AWS.AutoScaling.Types.MetricGranularityType
import Network.AWS.AutoScaling.Types.MetricStatistic
import Network.AWS.AutoScaling.Types.MetricType
import Network.AWS.AutoScaling.Types.MixedInstancesPolicy
import Network.AWS.AutoScaling.Types.NotificationConfiguration
import Network.AWS.AutoScaling.Types.PredefinedMetricSpecification
import Network.AWS.AutoScaling.Types.ProcessType
import Network.AWS.AutoScaling.Types.RefreshPreferences
import Network.AWS.AutoScaling.Types.RefreshStrategy
import Network.AWS.AutoScaling.Types.ScalingActivityStatusCode
import Network.AWS.AutoScaling.Types.ScalingPolicy
import Network.AWS.AutoScaling.Types.ScalingProcessQuery
import Network.AWS.AutoScaling.Types.ScheduledUpdateGroupAction
import Network.AWS.AutoScaling.Types.ScheduledUpdateGroupActionRequest
import Network.AWS.AutoScaling.Types.StepAdjustment
import Network.AWS.AutoScaling.Types.SuspendedProcess
import Network.AWS.AutoScaling.Types.Tag
import Network.AWS.AutoScaling.Types.TagDescription
import Network.AWS.AutoScaling.Types.TargetTrackingConfiguration
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2011-01-01@ of the Amazon Auto Scaling SDK configuration.
autoScalingService :: Lude.Service
autoScalingService =
  Lude.Service
    { Lude._svcAbbrev = "AutoScaling",
      Lude._svcSigner = Sign.v4,
      Lude._svcPrefix = "autoscaling",
      Lude._svcVersion = "2011-01-01",
      Lude._svcEndpoint = Lude.defaultEndpoint autoScalingService,
      Lude._svcTimeout = Lude.Just 70,
      Lude._svcCheck = Lude.statusSuccess,
      Lude._svcError = Lude.parseXMLError "AutoScaling",
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
