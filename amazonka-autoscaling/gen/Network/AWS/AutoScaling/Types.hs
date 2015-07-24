{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.Types
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AutoScaling.Types
    (
    -- * Service
      AutoScaling

    -- * Errors
    , _LimitExceededFault
    , _AlreadyExistsFault
    , _ResourceInUseFault
    , _InvalidNextToken
    , _ScalingActivityInProgressFault
    , _ResourceContentionFault

    -- * LifecycleState
    , LifecycleState (..)

    -- * ScalingActivityStatusCode
    , ScalingActivityStatusCode (..)

    -- * Activity
    , Activity
    , activity
    , aProgress
    , aStatusMessage
    , aDetails
    , aEndTime
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
    , asgVPCZoneIdentifier
    , asgEnabledMetrics
    , asgInstances
    , asgAutoScalingGroupARN
    , asgSuspendedProcesses
    , asgPlacementGroup
    , asgLoadBalancerNames
    , asgTags
    , asgAutoScalingGroupName
    , asgLaunchConfigurationName
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
    , asidInstanceId
    , asidAutoScalingGroupName
    , asidAvailabilityZone
    , asidLifecycleState
    , asidHealthStatus
    , asidLaunchConfigurationName

    -- * BlockDeviceMapping
    , BlockDeviceMapping
    , blockDeviceMapping
    , bdmVirtualName
    , bdmNoDevice
    , bdmEBS
    , bdmDeviceName

    -- * EBS
    , EBS
    , ebs
    , ebsDeleteOnTermination
    , ebsVolumeSize
    , ebsIOPS
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
    , iInstanceId
    , iAvailabilityZone
    , iLifecycleState
    , iHealthStatus
    , iLaunchConfigurationName

    -- * InstanceMonitoring
    , InstanceMonitoring
    , instanceMonitoring
    , imEnabled

    -- * LaunchConfiguration
    , LaunchConfiguration
    , launchConfiguration
    , lcSecurityGroups
    , lcAssociatePublicIPAddress
    , lcInstanceMonitoring
    , lcSpotPrice
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

    -- * LifecycleHook
    , LifecycleHook
    , lifecycleHook
    , lhDefaultResult
    , lhLifecycleHookName
    , lhHeartbeatTimeout
    , lhAutoScalingGroupName
    , lhNotificationMetadata
    , lhGlobalTimeout
    , lhRoleARN
    , lhLifecycleTransition
    , lhNotificationTargetARN

    -- * LoadBalancerState
    , LoadBalancerState
    , loadBalancerState
    , lbsState
    , lbsLoadBalancerName

    -- * MetricCollectionType
    , MetricCollectionType
    , metricCollectionType
    , mctMetric

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

    -- * ProcessType
    , ProcessType
    , processType
    , ptProcessName

    -- * ScalingPolicy
    , ScalingPolicy
    , scalingPolicy
    , sEstimatedInstanceWarmup
    , sMinAdjustmentStep
    , sPolicyName
    , sPolicyType
    , sStepAdjustments
    , sAdjustmentType
    , sScalingAdjustment
    , sAutoScalingGroupName
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
    , sugaTime
    , sugaStartTime
    , sugaScheduledActionName
    , sugaMaxSize
    , sugaDesiredCapacity
    , sugaRecurrence
    , sugaMinSize
    , sugaEndTime
    , sugaAutoScalingGroupName

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
    ) where

import           Network.AWS.AutoScaling.Types.Product
import           Network.AWS.AutoScaling.Types.Sum
import           Network.AWS.Prelude
import           Network.AWS.Sign.V4

-- | Version @2011-01-01@ of the Amazon Auto Scaling SDK.
data AutoScaling

instance AWSService AutoScaling where
    type Sg AutoScaling = V4
    service = const svc
      where
        svc =
            Service
            { _svcAbbrev = "AutoScaling"
            , _svcPrefix = "autoscaling"
            , _svcVersion = "2011-01-01"
            , _svcEndpoint = defaultEndpoint svc
            , _svcPreflight = id
            , _svcTimeout = Just 70000000
            , _svcStatus = statusSuccess
            , _svcError = parseXMLError
            , _svcRetry = retry
            }
        retry =
            Exponential
            { _retryBase = 5.0e-2
            , _retryGrowth = 2
            , _retryAttempts = 5
            , _retryCheck = check
            }
        check e
          | has (hasCode "ThrottlingException" . hasStatus 400) e =
              Just "throttling_exception"
          | has (hasCode "Throttling" . hasStatus 400) e = Just "throttling"
          | has (hasStatus 503) e = Just "service_unavailable"
          | has (hasStatus 500) e = Just "general_server_error"
          | has (hasStatus 509) e = Just "limit_exceeded"
          | otherwise = Nothing

-- | You have already reached a limit for your Auto Scaling resources (for
-- example, groups, launch configurations, or lifecycle hooks). For more
-- information, see DescribeAccountLimits.
_LimitExceededFault :: AWSError a => Getting (First ServiceError) a ServiceError
_LimitExceededFault = _ServiceError . hasStatus 400 . hasCode "LimitExceeded"

-- | You already have an Auto Scaling group or launch configuration with this
-- name.
_AlreadyExistsFault :: AWSError a => Getting (First ServiceError) a ServiceError
_AlreadyExistsFault = _ServiceError . hasStatus 400 . hasCode "AlreadyExists"

-- | The Auto Scaling group or launch configuration can\'t be deleted because
-- it is in use.
_ResourceInUseFault :: AWSError a => Getting (First ServiceError) a ServiceError
_ResourceInUseFault = _ServiceError . hasStatus 400 . hasCode "ResourceInUse"

-- | The @NextToken@ value is not valid.
_InvalidNextToken :: AWSError a => Getting (First ServiceError) a ServiceError
_InvalidNextToken = _ServiceError . hasStatus 400 . hasCode "InvalidNextToken"

-- | The Auto Scaling group can\'t be deleted because there are scaling
-- activities in progress.
_ScalingActivityInProgressFault :: AWSError a => Getting (First ServiceError) a ServiceError
_ScalingActivityInProgressFault =
    _ServiceError . hasStatus 400 . hasCode "ScalingActivityInProgress"

-- | You already have a pending update to an Auto Scaling resource (for
-- example, a group, instance, or load balancer).
_ResourceContentionFault :: AWSError a => Getting (First ServiceError) a ServiceError
_ResourceContentionFault =
    _ServiceError . hasStatus 500 . hasCode "ResourceContention"
