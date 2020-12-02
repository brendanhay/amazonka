{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AutoScaling.Types
  ( -- * Service Configuration
    autoScaling,

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
    Activity,
    activity,
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
    AdjustmentType,
    adjustmentType,
    atAdjustmentType,

    -- * Alarm
    Alarm,
    alarm,
    aAlarmName,
    aAlarmARN,

    -- * AutoScalingGroup
    AutoScalingGroup,
    autoScalingGroup,
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
    AutoScalingInstanceDetails,
    autoScalingInstanceDetails,
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
    BlockDeviceMapping,
    blockDeviceMapping,
    bdmVirtualName,
    bdmNoDevice,
    bdmEBS,
    bdmDeviceName,

    -- * CustomizedMetricSpecification
    CustomizedMetricSpecification,
    customizedMetricSpecification,
    cmsDimensions,
    cmsUnit,
    cmsMetricName,
    cmsNamespace,
    cmsStatistic,

    -- * EBS
    EBS,
    ebs,
    ebsDeleteOnTermination,
    ebsVolumeSize,
    ebsIOPS,
    ebsEncrypted,
    ebsVolumeType,
    ebsSnapshotId,

    -- * EnabledMetric
    EnabledMetric,
    enabledMetric,
    emGranularity,
    emMetric,

    -- * FailedScheduledUpdateGroupActionRequest
    FailedScheduledUpdateGroupActionRequest,
    failedScheduledUpdateGroupActionRequest,
    fsugarErrorCode,
    fsugarErrorMessage,
    fsugarScheduledActionName,

    -- * Filter
    Filter,
    filter',
    fValues,
    fName,

    -- * Instance
    Instance,
    instance',
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
    InstanceMetadataOptions,
    instanceMetadataOptions,
    imoHTTPEndpoint,
    imoHTTPPutResponseHopLimit,
    imoHTTPTokens,

    -- * InstanceMonitoring
    InstanceMonitoring,
    instanceMonitoring,
    imEnabled,

    -- * InstanceRefresh
    InstanceRefresh,
    instanceRefresh,
    irStatus,
    irStartTime,
    irInstancesToUpdate,
    irPercentageComplete,
    irAutoScalingGroupName,
    irEndTime,
    irStatusReason,
    irInstanceRefreshId,

    -- * InstancesDistribution
    InstancesDistribution,
    instancesDistribution,
    idSpotAllocationStrategy,
    idSpotInstancePools,
    idSpotMaxPrice,
    idOnDemandBaseCapacity,
    idOnDemandAllocationStrategy,
    idOnDemandPercentageAboveBaseCapacity,

    -- * LaunchConfiguration
    LaunchConfiguration,
    launchConfiguration,
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
    LaunchTemplate,
    launchTemplate,
    ltOverrides,
    ltLaunchTemplateSpecification,

    -- * LaunchTemplateOverrides
    LaunchTemplateOverrides,
    launchTemplateOverrides,
    ltoWeightedCapacity,
    ltoInstanceType,
    ltoLaunchTemplateSpecification,

    -- * LaunchTemplateSpecification
    LaunchTemplateSpecification,
    launchTemplateSpecification,
    ltsLaunchTemplateName,
    ltsLaunchTemplateId,
    ltsVersion,

    -- * LifecycleHook
    LifecycleHook,
    lifecycleHook,
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
    LifecycleHookSpecification,
    lifecycleHookSpecification,
    lhsDefaultResult,
    lhsHeartbeatTimeout,
    lhsNotificationMetadata,
    lhsNotificationTargetARN,
    lhsRoleARN,
    lhsLifecycleHookName,
    lhsLifecycleTransition,

    -- * LoadBalancerState
    LoadBalancerState,
    loadBalancerState,
    lbsState,
    lbsLoadBalancerName,

    -- * LoadBalancerTargetGroupState
    LoadBalancerTargetGroupState,
    loadBalancerTargetGroupState,
    lbtgsState,
    lbtgsLoadBalancerTargetGroupARN,

    -- * MetricCollectionType
    MetricCollectionType,
    metricCollectionType,
    mctMetric,

    -- * MetricDimension
    MetricDimension,
    metricDimension,
    mdName,
    mdValue,

    -- * MetricGranularityType
    MetricGranularityType,
    metricGranularityType,
    mgtGranularity,

    -- * MixedInstancesPolicy
    MixedInstancesPolicy,
    mixedInstancesPolicy,
    mipLaunchTemplate,
    mipInstancesDistribution,

    -- * NotificationConfiguration
    NotificationConfiguration,
    notificationConfiguration,
    ncTopicARN,
    ncAutoScalingGroupName,
    ncNotificationType,

    -- * PredefinedMetricSpecification
    PredefinedMetricSpecification,
    predefinedMetricSpecification,
    pmsResourceLabel,
    pmsPredefinedMetricType,

    -- * ProcessType
    ProcessType,
    processType,
    ptProcessName,

    -- * RefreshPreferences
    RefreshPreferences,
    refreshPreferences,
    rpMinHealthyPercentage,
    rpInstanceWarmup,

    -- * ScalingPolicy
    ScalingPolicy,
    scalingPolicy,
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
    ScalingProcessQuery,
    scalingProcessQuery,
    spqScalingProcesses,
    spqAutoScalingGroupName,

    -- * ScheduledUpdateGroupAction
    ScheduledUpdateGroupAction,
    scheduledUpdateGroupAction,
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
    ScheduledUpdateGroupActionRequest,
    scheduledUpdateGroupActionRequest,
    sugarStartTime,
    sugarMaxSize,
    sugarRecurrence,
    sugarDesiredCapacity,
    sugarMinSize,
    sugarEndTime,
    sugarScheduledActionName,

    -- * StepAdjustment
    StepAdjustment,
    stepAdjustment,
    saMetricIntervalLowerBound,
    saMetricIntervalUpperBound,
    saScalingAdjustment,

    -- * SuspendedProcess
    SuspendedProcess,
    suspendedProcess,
    spProcessName,
    spSuspensionReason,

    -- * Tag
    Tag,
    tag,
    tagKey,
    tagResourceId,
    tagResourceType,
    tagPropagateAtLaunch,
    tagValue,

    -- * TagDescription
    TagDescription,
    tagDescription,
    tdResourceId,
    tdResourceType,
    tdKey,
    tdPropagateAtLaunch,
    tdValue,

    -- * TargetTrackingConfiguration
    TargetTrackingConfiguration,
    targetTrackingConfiguration,
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
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Sign.V4

-- | API version @2011-01-01@ of the Amazon Auto Scaling SDK configuration.
autoScaling :: Service
autoScaling =
  Service
    { _svcAbbrev = "AutoScaling",
      _svcSigner = v4,
      _svcPrefix = "autoscaling",
      _svcVersion = "2011-01-01",
      _svcEndpoint = defaultEndpoint autoScaling,
      _svcTimeout = Just 70,
      _svcCheck = statusSuccess,
      _svcError = parseXMLError "AutoScaling",
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
