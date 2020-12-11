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
    aStatusMessage,
    aEndTime,
    aDetails,
    aDescription,
    aActivityId,
    aAutoScalingGroupName,
    aCause,
    aStartTime,
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
    asgHealthCheckGracePeriod,
    asgServiceLinkedRoleARN,
    asgNewInstancesProtectedFromScaleIn,
    asgVPCZoneIdentifier,
    asgTargetGroupARNs,
    asgMaxInstanceLifetime,
    asgMixedInstancesPolicy,
    asgEnabledMetrics,
    asgLaunchConfigurationName,
    asgInstances,
    asgLaunchTemplate,
    asgCapacityRebalance,
    asgAutoScalingGroupARN,
    asgPlacementGroup,
    asgSuspendedProcesses,
    asgLoadBalancerNames,
    asgTags,
    asgAutoScalingGroupName,
    asgMinSize,
    asgMaxSize,
    asgDesiredCapacity,
    asgDefaultCooldown,
    asgAvailabilityZones,
    asgHealthCheckType,
    asgCreatedTime,

    -- * AutoScalingInstanceDetails
    AutoScalingInstanceDetails (..),
    mkAutoScalingInstanceDetails,
    asidWeightedCapacity,
    asidInstanceType,
    asidLaunchConfigurationName,
    asidLaunchTemplate,
    asidInstanceId,
    asidAutoScalingGroupName,
    asidAvailabilityZone,
    asidLifecycleState,
    asidHealthStatus,
    asidProtectedFromScaleIn,

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
    cmsDimensions,
    cmsUnit,
    cmsMetricName,
    cmsNamespace,
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
    fsugarErrorCode,
    fsugarErrorMessage,
    fsugarScheduledActionName,

    -- * Filter
    Filter (..),
    mkFilter,
    fValues,
    fName,

    -- * Instance
    Instance (..),
    mkInstance,
    iWeightedCapacity,
    iInstanceType,
    iLaunchConfigurationName,
    iLaunchTemplate,
    iInstanceId,
    iAvailabilityZone,
    iLifecycleState,
    iHealthStatus,
    iProtectedFromScaleIn,

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
    lcInstanceMonitoring,
    lcKeyName,
    lcClassicLinkVPCSecurityGroups,
    lcRAMDiskId,
    lcKernelId,
    lcEBSOptimized,
    lcUserData,
    lcClassicLinkVPCId,
    lcIAMInstanceProfile,
    lcMetadataOptions,
    lcLaunchConfigurationARN,
    lcPlacementTenancy,
    lcBlockDeviceMappings,
    lcLaunchConfigurationName,
    lcImageId,
    lcInstanceType,
    lcCreatedTime,

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
    lhsHeartbeatTimeout,
    lhsNotificationMetadata,
    lhsNotificationTargetARN,
    lhsRoleARN,
    lhsLifecycleHookName,
    lhsLifecycleTransition,

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
    mdName,
    mdValue,

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
    pmsResourceLabel,
    pmsPredefinedMetricType,

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
    sMinAdjustmentStep,
    sEstimatedInstanceWarmup,
    sPolicyName,
    sEnabled,
    sPolicyType,
    sStepAdjustments,
    sTargetTrackingConfiguration,
    sAdjustmentType,
    sAutoScalingGroupName,
    sScalingAdjustment,
    sCooldown,
    sPolicyARN,
    sAlarms,
    sMetricAggregationType,
    sMinAdjustmentMagnitude,

    -- * ScalingProcessQuery
    ScalingProcessQuery (..),
    mkScalingProcessQuery,
    spqScalingProcesses,
    spqAutoScalingGroupName,

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
    sugarMaxSize,
    sugarRecurrence,
    sugarDesiredCapacity,
    sugarMinSize,
    sugarEndTime,
    sugarScheduledActionName,

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
    tKey,
    tResourceId,
    tResourceType,
    tPropagateAtLaunch,
    tValue,

    -- * TagDescription
    TagDescription (..),
    mkTagDescription,
    tdResourceId,
    tdResourceType,
    tdKey,
    tdPropagateAtLaunch,
    tdValue,

    -- * TargetTrackingConfiguration
    TargetTrackingConfiguration (..),
    mkTargetTrackingConfiguration,
    ttcPredefinedMetricSpecification,
    ttcCustomizedMetricSpecification,
    ttcDisableScaleIn,
    ttcTargetValue,
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
