{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.Types
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AutoScaling.Types
    (
    -- * Service Configuration
      autoScaling

    -- * Errors
    , _AlreadyExistsFault
    , _LimitExceededFault
    , _ResourceInUseFault
    , _InvalidNextToken
    , _ScalingActivityInProgressFault
    , _ResourceContentionFault
    , _ServiceLinkedRoleFailure

    -- * LifecycleState
    , LifecycleState (..)

    -- * MetricStatistic
    , MetricStatistic (..)

    -- * MetricType
    , MetricType (..)

    -- * ScalingActivityStatusCode
    , ScalingActivityStatusCode (..)

    -- * Activity
    , Activity
    , activity
    , aProgress
    , aStatusMessage
    , aEndTime
    , aDetails
    , aDescription
    , aActivityId
    , aAutoScalingGroupName
    , aCause
    , aStartTime
    , aStatusCode

    -- * AdjustmentType
    , AdjustmentType
    , adjustmentType
    , atAdjustmentType

    -- * Alarm
    , Alarm
    , alarm
    , aAlarmName
    , aAlarmARN

    -- * AutoScalingGroup
    , AutoScalingGroup
    , autoScalingGroup
    , asgStatus
    , asgTerminationPolicies
    , asgHealthCheckGracePeriod
    , asgServiceLinkedRoleARN
    , asgNewInstancesProtectedFromScaleIn
    , asgVPCZoneIdentifier
    , asgTargetGroupARNs
    , asgEnabledMetrics
    , asgLaunchConfigurationName
    , asgInstances
    , asgLaunchTemplate
    , asgAutoScalingGroupARN
    , asgPlacementGroup
    , asgSuspendedProcesses
    , asgLoadBalancerNames
    , asgTags
    , asgAutoScalingGroupName
    , asgMinSize
    , asgMaxSize
    , asgDesiredCapacity
    , asgDefaultCooldown
    , asgAvailabilityZones
    , asgHealthCheckType
    , asgCreatedTime

    -- * AutoScalingInstanceDetails
    , AutoScalingInstanceDetails
    , autoScalingInstanceDetails
    , asidLaunchConfigurationName
    , asidLaunchTemplate
    , asidInstanceId
    , asidAutoScalingGroupName
    , asidAvailabilityZone
    , asidLifecycleState
    , asidHealthStatus
    , asidProtectedFromScaleIn

    -- * BlockDeviceMapping
    , BlockDeviceMapping
    , blockDeviceMapping
    , bdmVirtualName
    , bdmNoDevice
    , bdmEBS
    , bdmDeviceName

    -- * CustomizedMetricSpecification
    , CustomizedMetricSpecification
    , customizedMetricSpecification
    , cmsDimensions
    , cmsUnit
    , cmsMetricName
    , cmsNamespace
    , cmsStatistic

    -- * EBS
    , EBS
    , ebs
    , ebsDeleteOnTermination
    , ebsVolumeSize
    , ebsIOPS
    , ebsEncrypted
    , ebsVolumeType
    , ebsSnapshotId

    -- * EnabledMetric
    , EnabledMetric
    , enabledMetric
    , emGranularity
    , emMetric

    -- * Filter
    , Filter
    , filter'
    , fValues
    , fName

    -- * Instance
    , Instance
    , instance'
    , iLaunchConfigurationName
    , iLaunchTemplate
    , iInstanceId
    , iAvailabilityZone
    , iLifecycleState
    , iHealthStatus
    , iProtectedFromScaleIn

    -- * InstanceMonitoring
    , InstanceMonitoring
    , instanceMonitoring
    , imEnabled

    -- * LaunchConfiguration
    , LaunchConfiguration
    , launchConfiguration
    , lcAssociatePublicIPAddress
    , lcSecurityGroups
    , lcSpotPrice
    , lcInstanceMonitoring
    , lcKeyName
    , lcClassicLinkVPCSecurityGroups
    , lcRAMDiskId
    , lcKernelId
    , lcEBSOptimized
    , lcUserData
    , lcClassicLinkVPCId
    , lcIAMInstanceProfile
    , lcLaunchConfigurationARN
    , lcPlacementTenancy
    , lcBlockDeviceMappings
    , lcLaunchConfigurationName
    , lcImageId
    , lcInstanceType
    , lcCreatedTime

    -- * LaunchTemplateSpecification
    , LaunchTemplateSpecification
    , launchTemplateSpecification
    , ltsLaunchTemplateName
    , ltsLaunchTemplateId
    , ltsVersion

    -- * LifecycleHook
    , LifecycleHook
    , lifecycleHook
    , lhDefaultResult
    , lhLifecycleHookName
    , lhHeartbeatTimeout
    , lhAutoScalingGroupName
    , lhNotificationMetadata
    , lhGlobalTimeout
    , lhNotificationTargetARN
    , lhLifecycleTransition
    , lhRoleARN

    -- * LifecycleHookSpecification
    , LifecycleHookSpecification
    , lifecycleHookSpecification
    , lhsDefaultResult
    , lhsHeartbeatTimeout
    , lhsNotificationMetadata
    , lhsNotificationTargetARN
    , lhsRoleARN
    , lhsLifecycleHookName
    , lhsLifecycleTransition

    -- * LoadBalancerState
    , LoadBalancerState
    , loadBalancerState
    , lbsState
    , lbsLoadBalancerName

    -- * LoadBalancerTargetGroupState
    , LoadBalancerTargetGroupState
    , loadBalancerTargetGroupState
    , lbtgsState
    , lbtgsLoadBalancerTargetGroupARN

    -- * MetricCollectionType
    , MetricCollectionType
    , metricCollectionType
    , mctMetric

    -- * MetricDimension
    , MetricDimension
    , metricDimension
    , mdName
    , mdValue

    -- * MetricGranularityType
    , MetricGranularityType
    , metricGranularityType
    , mgtGranularity

    -- * NotificationConfiguration
    , NotificationConfiguration
    , notificationConfiguration
    , ncTopicARN
    , ncAutoScalingGroupName
    , ncNotificationType

    -- * PredefinedMetricSpecification
    , PredefinedMetricSpecification
    , predefinedMetricSpecification
    , pmsResourceLabel
    , pmsPredefinedMetricType

    -- * ProcessType
    , ProcessType
    , processType
    , ptProcessName

    -- * ScalingPolicy
    , ScalingPolicy
    , scalingPolicy
    , sMinAdjustmentStep
    , sEstimatedInstanceWarmup
    , sPolicyName
    , sPolicyType
    , sStepAdjustments
    , sTargetTrackingConfiguration
    , sAdjustmentType
    , sAutoScalingGroupName
    , sScalingAdjustment
    , sCooldown
    , sPolicyARN
    , sAlarms
    , sMetricAggregationType
    , sMinAdjustmentMagnitude

    -- * ScalingProcessQuery
    , ScalingProcessQuery
    , scalingProcessQuery
    , spqScalingProcesses
    , spqAutoScalingGroupName

    -- * ScheduledUpdateGroupAction
    , ScheduledUpdateGroupAction
    , scheduledUpdateGroupAction
    , sugaScheduledActionARN
    , sugaStartTime
    , sugaTime
    , sugaScheduledActionName
    , sugaMaxSize
    , sugaRecurrence
    , sugaDesiredCapacity
    , sugaMinSize
    , sugaAutoScalingGroupName
    , sugaEndTime

    -- * StepAdjustment
    , StepAdjustment
    , stepAdjustment
    , saMetricIntervalLowerBound
    , saMetricIntervalUpperBound
    , saScalingAdjustment

    -- * SuspendedProcess
    , SuspendedProcess
    , suspendedProcess
    , spProcessName
    , spSuspensionReason

    -- * Tag
    , Tag
    , tag
    , tagKey
    , tagResourceId
    , tagResourceType
    , tagPropagateAtLaunch
    , tagValue

    -- * TagDescription
    , TagDescription
    , tagDescription
    , tdResourceId
    , tdResourceType
    , tdKey
    , tdPropagateAtLaunch
    , tdValue

    -- * TargetTrackingConfiguration
    , TargetTrackingConfiguration
    , targetTrackingConfiguration
    , ttcPredefinedMetricSpecification
    , ttcCustomizedMetricSpecification
    , ttcDisableScaleIn
    , ttcTargetValue
    ) where

import Network.AWS.AutoScaling.Types.Product
import Network.AWS.AutoScaling.Types.Sum
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Sign.V4

-- | API version @2011-01-01@ of the Amazon Auto Scaling SDK configuration.
autoScaling :: Service
autoScaling =
  Service
    { _svcAbbrev = "AutoScaling"
    , _svcSigner = v4
    , _svcPrefix = "autoscaling"
    , _svcVersion = "2011-01-01"
    , _svcEndpoint = defaultEndpoint autoScaling
    , _svcTimeout = Just 70
    , _svcCheck = statusSuccess
    , _svcError = parseXMLError "AutoScaling"
    , _svcRetry = retry
    }
  where
    retry =
      Exponential
        { _retryBase = 5.0e-2
        , _retryGrowth = 2
        , _retryAttempts = 5
        , _retryCheck = check
        }
    check e
      | has (hasCode "ThrottledException" . hasStatus 400) e =
        Just "throttled_exception"
      | has (hasStatus 429) e = Just "too_many_requests"
      | has (hasCode "ThrottlingException" . hasStatus 400) e =
        Just "throttling_exception"
      | has (hasCode "Throttling" . hasStatus 400) e = Just "throttling"
      | has (hasStatus 504) e = Just "gateway_timeout"
      | has (hasCode "RequestThrottledException" . hasStatus 400) e =
        Just "request_throttled_exception"
      | has (hasStatus 502) e = Just "bad_gateway"
      | has (hasStatus 503) e = Just "service_unavailable"
      | has (hasStatus 500) e = Just "general_server_error"
      | has (hasStatus 509) e = Just "limit_exceeded"
      | otherwise = Nothing


-- | You already have an Auto Scaling group or launch configuration with this name.
--
--
_AlreadyExistsFault :: AsError a => Getting (First ServiceError) a ServiceError
_AlreadyExistsFault =
  _MatchServiceError autoScaling "AlreadyExists" . hasStatus 400


-- | You have already reached a limit for your Auto Scaling resources (for example, groups, launch configurations, or lifecycle hooks). For more information, see 'DescribeAccountLimits' .
--
--
_LimitExceededFault :: AsError a => Getting (First ServiceError) a ServiceError
_LimitExceededFault =
  _MatchServiceError autoScaling "LimitExceeded" . hasStatus 400


-- | The operation can't be performed because the resource is in use.
--
--
_ResourceInUseFault :: AsError a => Getting (First ServiceError) a ServiceError
_ResourceInUseFault =
  _MatchServiceError autoScaling "ResourceInUse" . hasStatus 400


-- | The @NextToken@ value is not valid.
--
--
_InvalidNextToken :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidNextToken =
  _MatchServiceError autoScaling "InvalidNextToken" . hasStatus 400


-- | The operation can't be performed because there are scaling activities in progress.
--
--
_ScalingActivityInProgressFault :: AsError a => Getting (First ServiceError) a ServiceError
_ScalingActivityInProgressFault =
  _MatchServiceError autoScaling "ScalingActivityInProgress" . hasStatus 400


-- | You already have a pending update to an Auto Scaling resource (for example, a group, instance, or load balancer).
--
--
_ResourceContentionFault :: AsError a => Getting (First ServiceError) a ServiceError
_ResourceContentionFault =
  _MatchServiceError autoScaling "ResourceContention" . hasStatus 500


-- | The service-linked role is not yet ready for use.
--
--
_ServiceLinkedRoleFailure :: AsError a => Getting (First ServiceError) a ServiceError
_ServiceLinkedRoleFailure =
  _MatchServiceError autoScaling "ServiceLinkedRoleFailure" . hasStatus 500

