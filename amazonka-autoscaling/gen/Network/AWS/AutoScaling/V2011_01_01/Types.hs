{-# LANGUAGE DeriveDataTypeable          #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE StandaloneDeriving          #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.AutoScaling.V2011_01_01.Types
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Auto Scaling allows you to scale your Amazon EC2 capacity up or down
-- automatically according to conditions you define. With Auto Scaling, you
-- can ensure that the number of Amazon EC2 instances you’re using increases
-- seamlessly during demand spikes to maintain performance, and decreases
-- automatically during demand lulls to minimize costs. Auto Scaling is
-- particularly well suited for applications that experience hourly, daily, or
-- weekly variability in usage. Auto Scaling is enabled by Amazon CloudWatch
-- and available at no additional charge beyond Amazon CloudWatch fees.
module Network.AWS.AutoScaling.V2011_01_01.Types
    (
    -- * Service
      AutoScaling
    -- ** Errors
    , Er (..)
    -- ** XML
    , xmlOptions

    -- * LifecycleState
    , LifecycleState (..)

    -- * ScalingActivityStatusCode
    , ScalingActivityStatusCode (..)

    -- * AdjustmentType
    , AdjustmentType (..)
    , auAdjustmentType

    -- * InstanceMonitoring
    , InstanceMonitoring (..)
    , imEnabled

    -- * MetricCollectionType
    , MetricCollectionType (..)
    , mcuMetric

    -- * MetricGranularityType
    , MetricGranularityType (..)
    , mguGranularity

    -- * ProcessType
    , ProcessType (..)
    , pwProcessName

    -- * Activity
    , Activity (..)
    , ayActivityId
    , ayAutoScalingGroupName
    , ayDescription
    , ayCause
    , ayStartTime
    , ayEndTime
    , ayStatusCode
    , ayStatusMessage
    , ayProgress
    , ayDetails

    -- * Alarm
    , Alarm (..)
    , amAlarmName
    , amAlarmARN

    -- * AutoScalingGroup
    , AutoScalingGroup (..)
    , ashAutoScalingGroupName
    , ashAutoScalingGroupARN
    , ashLaunchConfigurationName
    , ashMinSize
    , ashMaxSize
    , ashDesiredCapacity
    , ashDefaultCooldown
    , ashAvailabilityZones
    , ashLoadBalancerNames
    , ashHealthCheckType
    , ashHealthCheckGracePeriod
    , ashInstances
    , ashCreatedTime
    , ashSuspendedProcesses
    , ashPlacementGroup
    , ashVPCZoneIdentifier
    , ashEnabledMetrics
    , ashStatus
    , ashTags
    , ashTerminationPolicies

    -- * AutoScalingInstanceDetails
    , AutoScalingInstanceDetails (..)
    , asidInstanceId
    , asidAutoScalingGroupName
    , asidAvailabilityZone
    , asidLifecycleState
    , asidHealthStatus
    , asidLaunchConfigurationName

    -- * BlockDeviceMapping
    , BlockDeviceMapping (..)
    , bdnVirtualName
    , bdnDeviceName
    , bdnEbs
    , bdnNoDevice

    -- * Ebs
    , Ebs (..)
    , eSnapshotId
    , eVolumeSize
    , eVolumeType
    , eDeleteOnTermination
    , eIops

    -- * EnabledMetric
    , EnabledMetric (..)
    , enMetric
    , enGranularity

    -- * Filter
    , Filter (..)
    , frName
    , frValues

    -- * Instance
    , Instance (..)
    , pInstanceId
    , pAvailabilityZone
    , pLifecycleState
    , pHealthStatus
    , pLaunchConfigurationName

    -- * LaunchConfiguration
    , LaunchConfiguration (..)
    , ldLaunchConfigurationName
    , ldLaunchConfigurationARN
    , ldImageId
    , ldKeyName
    , ldSecurityGroups
    , ldUserData
    , ldInstanceType
    , ldKernelId
    , ldRamdiskId
    , ldBlockDeviceMappings
    , ldInstanceMonitoring
    , ldSpotPrice
    , ldIamInstanceProfile
    , ldCreatedTime
    , ldEbsOptimized
    , ldAssociatePublicIpAddress
    , ldPlacementTenancy

    -- * LifecycleHook
    , LifecycleHook (..)
    , liLifecycleHookName
    , liAutoScalingGroupName
    , liLifecycleTransition
    , liNotificationTargetARN
    , liRoleARN
    , liNotificationMetadata
    , liHeartbeatTimeout
    , liGlobalTimeout
    , liDefaultResult

    -- * NotificationConfiguration
    , NotificationConfiguration (..)
    , nfAutoScalingGroupName
    , nfTopicARN
    , nfNotificationType

    -- * ScalingPolicy
    , ScalingPolicy (..)
    , suAutoScalingGroupName
    , suPolicyName
    , suScalingAdjustment
    , suAdjustmentType
    , suCooldown
    , suPolicyARN
    , suAlarms
    , suMinAdjustmentStep

    -- * ScheduledUpdateGroupAction
    , ScheduledUpdateGroupAction (..)
    , sugbAutoScalingGroupName
    , sugbScheduledActionName
    , sugbScheduledActionARN
    , sugbTime
    , sugbStartTime
    , sugbEndTime
    , sugbRecurrence
    , sugbMinSize
    , sugbMaxSize
    , sugbDesiredCapacity

    -- * SuspendedProcess
    , SuspendedProcess (..)
    , srProcessName
    , srSuspensionReason

    -- * Tag
    , Tag (..)
    , uResourceId
    , uResourceType
    , uKey
    , uValue
    , uPropagateAtLaunch

    -- * TagDescription
    , TagDescription (..)
    , tdResourceId
    , tdResourceType
    , tdKey
    , tdValue
    , tdPropagateAtLaunch

    ) where

import Network.AWS.Prelude
import Network.AWS.Signing.V4

-- | Supported version (@2011-01-01@) of the
-- @Auto Scaling@ service.
data AutoScaling deriving (Typeable)

instance AWSService AutoScaling where
    type Sg AutoScaling = V4
    data Er AutoScaling
        = AlreadyExistsFault
            { _aefMessage :: Maybe Text
            }
        | AutoScalingClient HttpException
        | AutoScalingSerializer String
        | AutoScalingService String
        | InvalidNextToken
            { _intMessage :: Maybe Text
            }
        | LimitExceededFault
            { _lefMessage :: Maybe Text
            }
        | ResourceInUseFault
            { _riufMessage :: Maybe Text
            }
        | ScalingActivityInProgressFault
            { _saipfMessage :: Maybe Text
            }

    service = Service'
        { _svcEndpoint = Regional
        , _svcPrefix   = "autoscaling"
        , _svcVersion  = "2011-01-01"
        , _svcTarget   = Nothing
        }

deriving instance Show    (Er AutoScaling)
deriving instance Generic (Er AutoScaling)

instance AWSError (Er AutoScaling) where
    awsError = const "AutoScalingError"

instance AWSServiceError (Er AutoScaling) where
    serviceError    = AutoScalingService
    clientError     = AutoScalingClient
    serializerError = AutoScalingSerializer

instance Exception (Er AutoScaling)

xmlOptions :: Tagged a XMLOptions
xmlOptions = Tagged def
    { xmlNamespace = Just "http://autoscaling.amazonaws.com/doc/2011-01-01/"
    }

-- | Contains a description of the current lifecycle state. The Quarantined
-- lifecycle state is currently not used.
data LifecycleState
    = LifecycleStateDetached -- ^ Detached
    | LifecycleStateDetaching -- ^ Detaching
    | LifecycleStateEnteringStandby -- ^ EnteringStandby
    | LifecycleStateInService -- ^ InService
    | LifecycleStatePending -- ^ Pending
    | LifecycleStatePendingProceed -- ^ Pending:Proceed
    | LifecycleStatePendingWait -- ^ Pending:Wait
    | LifecycleStateQuarantined -- ^ Quarantined
    | LifecycleStateStandby -- ^ Standby
    | LifecycleStateTerminated -- ^ Terminated
    | LifecycleStateTerminating -- ^ Terminating
    | LifecycleStateTerminatingProceed -- ^ Terminating:Proceed
    | LifecycleStateTerminatingWait -- ^ Terminating:Wait
      deriving (Eq, Show, Generic)

instance Hashable LifecycleState

instance FromText LifecycleState where
    parser = match "Detached" LifecycleStateDetached
         <|> match "Detaching" LifecycleStateDetaching
         <|> match "EnteringStandby" LifecycleStateEnteringStandby
         <|> match "InService" LifecycleStateInService
         <|> match "Pending" LifecycleStatePending
         <|> match "Pending:Proceed" LifecycleStatePendingProceed
         <|> match "Pending:Wait" LifecycleStatePendingWait
         <|> match "Quarantined" LifecycleStateQuarantined
         <|> match "Standby" LifecycleStateStandby
         <|> match "Terminated" LifecycleStateTerminated
         <|> match "Terminating" LifecycleStateTerminating
         <|> match "Terminating:Proceed" LifecycleStateTerminatingProceed
         <|> match "Terminating:Wait" LifecycleStateTerminatingWait

instance ToText LifecycleState where
    toText LifecycleStateDetached = "Detached"
    toText LifecycleStateDetaching = "Detaching"
    toText LifecycleStateEnteringStandby = "EnteringStandby"
    toText LifecycleStateInService = "InService"
    toText LifecycleStatePending = "Pending"
    toText LifecycleStatePendingProceed = "Pending:Proceed"
    toText LifecycleStatePendingWait = "Pending:Wait"
    toText LifecycleStateQuarantined = "Quarantined"
    toText LifecycleStateStandby = "Standby"
    toText LifecycleStateTerminated = "Terminated"
    toText LifecycleStateTerminating = "Terminating"
    toText LifecycleStateTerminatingProceed = "Terminating:Proceed"
    toText LifecycleStateTerminatingWait = "Terminating:Wait"

instance ToByteString LifecycleState

instance FromXML LifecycleState where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "LifecycleState"

instance ToQuery LifecycleState where
    toQuery = genericQuery def

-- | Contains the current status of the activity.
data ScalingActivityStatusCode
    = ScalingActivityStatusCodeCancelled -- ^ Cancelled
    | ScalingActivityStatusCodeFailed -- ^ Failed
    | ScalingActivityStatusCodeInProgress -- ^ InProgress
    | ScalingActivityStatusCodeMidLifecycleAction -- ^ MidLifecycleAction
    | ScalingActivityStatusCodePreInService -- ^ PreInService
    | ScalingActivityStatusCodeSuccessful -- ^ Successful
    | ScalingActivityStatusCodeWaitingForELBConnectionDraining -- ^ WaitingForELBConnectionDraining
    | ScalingActivityStatusCodeWaitingForInstanceId -- ^ WaitingForInstanceId
    | ScalingActivityStatusCodeWaitingForSpotInstanceId -- ^ WaitingForSpotInstanceId
    | ScalingActivityStatusCodeWaitingForSpotInstanceRequestId -- ^ WaitingForSpotInstanceRequestId
      deriving (Eq, Show, Generic)

instance Hashable ScalingActivityStatusCode

instance FromText ScalingActivityStatusCode where
    parser = match "Cancelled" ScalingActivityStatusCodeCancelled
         <|> match "Failed" ScalingActivityStatusCodeFailed
         <|> match "InProgress" ScalingActivityStatusCodeInProgress
         <|> match "MidLifecycleAction" ScalingActivityStatusCodeMidLifecycleAction
         <|> match "PreInService" ScalingActivityStatusCodePreInService
         <|> match "Successful" ScalingActivityStatusCodeSuccessful
         <|> match "WaitingForELBConnectionDraining" ScalingActivityStatusCodeWaitingForELBConnectionDraining
         <|> match "WaitingForInstanceId" ScalingActivityStatusCodeWaitingForInstanceId
         <|> match "WaitingForSpotInstanceId" ScalingActivityStatusCodeWaitingForSpotInstanceId
         <|> match "WaitingForSpotInstanceRequestId" ScalingActivityStatusCodeWaitingForSpotInstanceRequestId

instance ToText ScalingActivityStatusCode where
    toText ScalingActivityStatusCodeCancelled = "Cancelled"
    toText ScalingActivityStatusCodeFailed = "Failed"
    toText ScalingActivityStatusCodeInProgress = "InProgress"
    toText ScalingActivityStatusCodeMidLifecycleAction = "MidLifecycleAction"
    toText ScalingActivityStatusCodePreInService = "PreInService"
    toText ScalingActivityStatusCodeSuccessful = "Successful"
    toText ScalingActivityStatusCodeWaitingForELBConnectionDraining = "WaitingForELBConnectionDraining"
    toText ScalingActivityStatusCodeWaitingForInstanceId = "WaitingForInstanceId"
    toText ScalingActivityStatusCodeWaitingForSpotInstanceId = "WaitingForSpotInstanceId"
    toText ScalingActivityStatusCodeWaitingForSpotInstanceRequestId = "WaitingForSpotInstanceRequestId"

instance ToByteString ScalingActivityStatusCode

instance FromXML ScalingActivityStatusCode where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ScalingActivityStatusCode"

instance ToQuery ScalingActivityStatusCode where
    toQuery = genericQuery def

-- | Specifies whether the PutScalingPolicy ScalingAdjustment parameter is an
-- absolute number or a percentage of the current capacity.
newtype AdjustmentType = AdjustmentType
    { _auAdjustmentType :: Maybe Text
      -- ^ A policy adjustment type. Valid values are ChangeInCapacity,
      -- ExactCapacity, and PercentChangeInCapacity.
    } deriving (Show, Generic)

-- | A policy adjustment type. Valid values are ChangeInCapacity, ExactCapacity,
-- and PercentChangeInCapacity.
auAdjustmentType :: Lens' AdjustmentType (Maybe Text)
auAdjustmentType f x =
    f (_auAdjustmentType x)
        <&> \y -> x { _auAdjustmentType = y }
{-# INLINE auAdjustmentType #-}

instance FromXML AdjustmentType where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "AdjustmentType"

-- | Enables detailed monitoring if it is disabled. Detailed monitoring is
-- enabled by default. When detailed monitoring is enabled, Amazon Cloudwatch
-- will generate metrics every minute and your account will be charged a fee.
-- When you disable detailed monitoring, by specifying False, Cloudwatch will
-- generate metrics every 5 minutes. For more information, see Monitor Your
-- Auto Scaling Instances. For information about Amazon CloudWatch, see the
-- Amazon CloudWatch Developer Guide.
newtype InstanceMonitoring = InstanceMonitoring
    { _imEnabled :: Maybe Bool
      -- ^ If True, instance monitoring is enabled.
    } deriving (Show, Generic)

-- | If True, instance monitoring is enabled.
imEnabled :: Lens' InstanceMonitoring (Maybe Bool)
imEnabled f x =
    f (_imEnabled x)
        <&> \y -> x { _imEnabled = y }
{-# INLINE imEnabled #-}

instance FromXML InstanceMonitoring where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "InstanceMonitoring"

instance ToQuery InstanceMonitoring where
    toQuery = genericQuery def

-- | The MetricCollectionType data type.
newtype MetricCollectionType = MetricCollectionType
    { _mcuMetric :: Maybe Text
      -- ^ 
    } deriving (Show, Generic)

-- | 
mcuMetric :: Lens' MetricCollectionType (Maybe Text)
mcuMetric f x =
    f (_mcuMetric x)
        <&> \y -> x { _mcuMetric = y }
{-# INLINE mcuMetric #-}

instance FromXML MetricCollectionType where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "MetricCollectionType"

-- | The MetricGranularityType data type.
newtype MetricGranularityType = MetricGranularityType
    { _mguGranularity :: Maybe Text
      -- ^ The granularity of a Metric.
    } deriving (Show, Generic)

-- | The granularity of a Metric.
mguGranularity :: Lens' MetricGranularityType (Maybe Text)
mguGranularity f x =
    f (_mguGranularity x)
        <&> \y -> x { _mguGranularity = y }
{-# INLINE mguGranularity #-}

instance FromXML MetricGranularityType where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "MetricGranularityType"

-- | There are two primary Auto Scaling process types--Launch and Terminate. The
-- Launch process creates a new Amazon EC2 instance for an Auto Scaling group,
-- and the Terminate process removes an existing Amazon EC2 instance. The
-- remaining Auto Scaling process types relate to specific Auto Scaling
-- features: AddToLoadBalancer AlarmNotification AZRebalance HealthCheck
-- ReplaceUnhealthy ScheduledActions If you suspend Launch or Terminate, all
-- other process types are affected to varying degrees. The following
-- descriptions discuss how each process type is affected by a suspension of
-- Launch or Terminate. The AddToLoadBalancer process type adds instances to
-- the load balancer when the instances are launched. If you suspend this
-- process, Auto Scaling will launch the instances but will not add them to
-- the load balancer. If you resume the AddToLoadBalancer process, Auto
-- Scaling will also resume adding new instances to the load balancer when
-- they are launched. However, Auto Scaling will not add running instances
-- that were launched while the process was suspended; those instances must be
-- added manually using the the RegisterInstancesWithLoadBalancer call in the
-- Elastic Load Balancing API Reference. The AlarmNotification process type
-- accepts notifications from Amazon CloudWatch alarms that are associated
-- with the Auto Scaling group. If you suspend the AlarmNotification process
-- type, Auto Scaling will not automatically execute scaling policies that
-- would be triggered by alarms. Although the AlarmNotification process type
-- is not directly affected by a suspension of Launch or Terminate, alarm
-- notifications are often used to signal that a change in the size of the
-- Auto Scaling group is warranted. If you suspend Launch or Terminate, Auto
-- Scaling might not be able to implement the alarm's associated policy. The
-- AZRebalance process type seeks to maintain a balanced number of instances
-- across Availability Zones within a Region. If you remove an Availability
-- Zone from your Auto Scaling group or an Availability Zone otherwise becomes
-- unhealthy or unavailable, Auto Scaling launches new instances in an
-- unaffected Availability Zone before terminating the unhealthy or
-- unavailable instances. When the unhealthy Availability Zone returns to a
-- healthy state, Auto Scaling automatically redistributes the application
-- instances evenly across all of the designated Availability Zones. If you
-- call SuspendProcesses on the launch process type, the AZRebalance process
-- will neither launch new instances nor terminate existing instances. This is
-- because the AZRebalance process terminates existing instances only after
-- launching the replacement instances. If you call SuspendProcesses on the
-- terminate process type, the AZRebalance process can cause your Auto Scaling
-- group to grow up to ten percent larger than the maximum size. This is
-- because Auto Scaling allows groups to temporarily grow larger than the
-- maximum size during rebalancing activities. If Auto Scaling cannot
-- terminate instances, your Auto Scaling group could remain up to ten percent
-- larger than the maximum size until you resume the terminate process type.
-- The HealthCheck process type checks the health of the instances. Auto
-- Scaling marks an instance as unhealthy if Amazon EC2 or Elastic Load
-- Balancing informs Auto Scaling that the instance is unhealthy. The
-- HealthCheck process can override the health status of an instance that you
-- set with SetInstanceHealth. The ReplaceUnhealthy process type terminates
-- instances that are marked as unhealthy and subsequently creates new
-- instances to replace them. This process calls both of the primary process
-- types--first Terminate and then Launch. The HealthCheck process type works
-- in conjunction with the ReplaceUnhealthly process type to provide health
-- check functionality. If you suspend either Launch or Terminate, the
-- ReplaceUnhealthy process type will not function properly. The
-- ScheduledActions process type performs scheduled actions that you create
-- with PutScheduledUpdateGroupAction. Scheduled actions often involve
-- launching new instances or terminating existing instances. If you suspend
-- either Launch or Terminate, your scheduled actions might not function as
-- expected.
newtype ProcessType = ProcessType
    { _pwProcessName :: Text
      -- ^ The name of a process.
    } deriving (Show, Generic)

-- | The name of a process.
pwProcessName :: Lens' ProcessType (Text)
pwProcessName f x =
    f (_pwProcessName x)
        <&> \y -> x { _pwProcessName = y }
{-# INLINE pwProcessName #-}

instance FromXML ProcessType where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ProcessType"

-- | A scaling Activity is a long-running process that represents a change to
-- your AutoScalingGroup, such as changing the size of the group. It can also
-- be a process to replace an instance, or a process to perform any other
-- long-running operations supported by the API.
data Activity = Activity
    { _ayActivityId :: Text
      -- ^ Specifies the ID of the activity.
    , _ayAutoScalingGroupName :: Text
      -- ^ The name of the Auto Scaling group.
    , _ayDescription :: Maybe Text
      -- ^ Contains a friendly, more verbose description of the scaling
      -- activity.
    , _ayCause :: Text
      -- ^ Contains the reason the activity was begun.
    , _ayStartTime :: ISO8601
      -- ^ Provides the start time of this activity.
    , _ayEndTime :: Maybe ISO8601
      -- ^ Provides the end time of this activity.
    , _ayStatusCode :: ScalingActivityStatusCode
      -- ^ Contains the current status of the activity.
    , _ayStatusMessage :: Maybe Text
      -- ^ Contains a friendly, more verbose description of the activity
      -- status.
    , _ayProgress :: Maybe Integer
      -- ^ Specifies a value between 0 and 100 that indicates the progress
      -- of the activity.
    , _ayDetails :: Maybe Text
      -- ^ Contains details of the scaling activity.
    } deriving (Show, Generic)

-- | Specifies the ID of the activity.
ayActivityId :: Lens' Activity (Text)
ayActivityId f x =
    f (_ayActivityId x)
        <&> \y -> x { _ayActivityId = y }
{-# INLINE ayActivityId #-}

-- | The name of the Auto Scaling group.
ayAutoScalingGroupName :: Lens' Activity (Text)
ayAutoScalingGroupName f x =
    f (_ayAutoScalingGroupName x)
        <&> \y -> x { _ayAutoScalingGroupName = y }
{-# INLINE ayAutoScalingGroupName #-}

-- | Contains a friendly, more verbose description of the scaling activity.
ayDescription :: Lens' Activity (Maybe Text)
ayDescription f x =
    f (_ayDescription x)
        <&> \y -> x { _ayDescription = y }
{-# INLINE ayDescription #-}

-- | Contains the reason the activity was begun.
ayCause :: Lens' Activity (Text)
ayCause f x =
    f (_ayCause x)
        <&> \y -> x { _ayCause = y }
{-# INLINE ayCause #-}

-- | Provides the start time of this activity.
ayStartTime :: Lens' Activity (ISO8601)
ayStartTime f x =
    f (_ayStartTime x)
        <&> \y -> x { _ayStartTime = y }
{-# INLINE ayStartTime #-}

-- | Provides the end time of this activity.
ayEndTime :: Lens' Activity (Maybe ISO8601)
ayEndTime f x =
    f (_ayEndTime x)
        <&> \y -> x { _ayEndTime = y }
{-# INLINE ayEndTime #-}

-- | Contains the current status of the activity.
ayStatusCode :: Lens' Activity (ScalingActivityStatusCode)
ayStatusCode f x =
    f (_ayStatusCode x)
        <&> \y -> x { _ayStatusCode = y }
{-# INLINE ayStatusCode #-}

-- | Contains a friendly, more verbose description of the activity status.
ayStatusMessage :: Lens' Activity (Maybe Text)
ayStatusMessage f x =
    f (_ayStatusMessage x)
        <&> \y -> x { _ayStatusMessage = y }
{-# INLINE ayStatusMessage #-}

-- | Specifies a value between 0 and 100 that indicates the progress of the
-- activity.
ayProgress :: Lens' Activity (Maybe Integer)
ayProgress f x =
    f (_ayProgress x)
        <&> \y -> x { _ayProgress = y }
{-# INLINE ayProgress #-}

-- | Contains details of the scaling activity.
ayDetails :: Lens' Activity (Maybe Text)
ayDetails f x =
    f (_ayDetails x)
        <&> \y -> x { _ayDetails = y }
{-# INLINE ayDetails #-}

instance FromXML Activity where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Activity"

-- | The Alarm data type.
data Alarm = Alarm
    { _amAlarmName :: Maybe Text
      -- ^ The name of the alarm.
    , _amAlarmARN :: Maybe Text
      -- ^ The Amazon Resource Name (ARN) of the alarm.
    } deriving (Show, Generic)

-- | The name of the alarm.
amAlarmName :: Lens' Alarm (Maybe Text)
amAlarmName f x =
    f (_amAlarmName x)
        <&> \y -> x { _amAlarmName = y }
{-# INLINE amAlarmName #-}

-- | The Amazon Resource Name (ARN) of the alarm.
amAlarmARN :: Lens' Alarm (Maybe Text)
amAlarmARN f x =
    f (_amAlarmARN x)
        <&> \y -> x { _amAlarmARN = y }
{-# INLINE amAlarmARN #-}

instance FromXML Alarm where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Alarm"

instance ToQuery Alarm where
    toQuery = genericQuery def

-- | The AutoScalingGroup data type.
data AutoScalingGroup = AutoScalingGroup
    { _ashAutoScalingGroupName :: Text
      -- ^ Specifies the name of the group.
    , _ashAutoScalingGroupARN :: Maybe Text
      -- ^ The Amazon Resource Name (ARN) of the Auto Scaling group.
    , _ashLaunchConfigurationName :: Text
      -- ^ Specifies the name of the associated LaunchConfiguration.
    , _ashMinSize :: Integer
      -- ^ Contains the minimum size of the Auto Scaling group.
    , _ashMaxSize :: Integer
      -- ^ Contains the maximum size of the Auto Scaling group.
    , _ashDesiredCapacity :: Integer
      -- ^ Specifies the desired capacity for the Auto Scaling group.
    , _ashDefaultCooldown :: Integer
      -- ^ The number of seconds after a scaling activity completes before
      -- any further scaling activities can start.
    , _ashAvailabilityZones :: [Text]
      -- ^ Contains a list of Availability Zones for the group.
    , _ashLoadBalancerNames :: [Text]
      -- ^ A list of load balancers associated with this Auto Scaling group.
    , _ashHealthCheckType :: Text
      -- ^ The service of interest for the health status check, either "EC2"
      -- for Amazon EC2 or "ELB" for Elastic Load Balancing.
    , _ashHealthCheckGracePeriod :: Maybe Integer
      -- ^ The length of time that Auto Scaling waits before checking an
      -- instance's health status. The grace period begins when an
      -- instance comes into service.
    , _ashInstances :: [Instance]
      -- ^ Provides a summary list of Amazon EC2 instances.
    , _ashCreatedTime :: ISO8601
      -- ^ Specifies the date and time the Auto Scaling group was created.
    , _ashSuspendedProcesses :: [SuspendedProcess]
      -- ^ Suspended processes associated with this Auto Scaling group.
    , _ashPlacementGroup :: Maybe Text
      -- ^ The name of the cluster placement group, if applicable. For more
      -- information, go to Using Cluster Instances in the Amazon EC2 User
      -- Guide.
    , _ashVPCZoneIdentifier :: Maybe Text
      -- ^ The subnet identifier for the Amazon VPC connection, if
      -- applicable. You can specify several subnets in a comma-separated
      -- list. When you specify VPCZoneIdentifier with AvailabilityZones,
      -- ensure that the subnets' Availability Zones match the values you
      -- specify for AvailabilityZones.
    , _ashEnabledMetrics :: [EnabledMetric]
      -- ^ A list of metrics enabled for this Auto Scaling group.
    , _ashStatus :: Maybe Text
      -- ^ The current state of the Auto Scaling group when a
      -- DeleteAutoScalingGroup action is in progress.
    , _ashTags :: [TagDescription]
      -- ^ A list of tags for the Auto Scaling group.
    , _ashTerminationPolicies :: [Text]
      -- ^ A standalone termination policy or a list of termination policies
      -- for this Auto Scaling group.
    } deriving (Show, Generic)

-- | Specifies the name of the group.
ashAutoScalingGroupName :: Lens' AutoScalingGroup (Text)
ashAutoScalingGroupName f x =
    f (_ashAutoScalingGroupName x)
        <&> \y -> x { _ashAutoScalingGroupName = y }
{-# INLINE ashAutoScalingGroupName #-}

-- | The Amazon Resource Name (ARN) of the Auto Scaling group.
ashAutoScalingGroupARN :: Lens' AutoScalingGroup (Maybe Text)
ashAutoScalingGroupARN f x =
    f (_ashAutoScalingGroupARN x)
        <&> \y -> x { _ashAutoScalingGroupARN = y }
{-# INLINE ashAutoScalingGroupARN #-}

-- | Specifies the name of the associated LaunchConfiguration.
ashLaunchConfigurationName :: Lens' AutoScalingGroup (Text)
ashLaunchConfigurationName f x =
    f (_ashLaunchConfigurationName x)
        <&> \y -> x { _ashLaunchConfigurationName = y }
{-# INLINE ashLaunchConfigurationName #-}

-- | Contains the minimum size of the Auto Scaling group.
ashMinSize :: Lens' AutoScalingGroup (Integer)
ashMinSize f x =
    f (_ashMinSize x)
        <&> \y -> x { _ashMinSize = y }
{-# INLINE ashMinSize #-}

-- | Contains the maximum size of the Auto Scaling group.
ashMaxSize :: Lens' AutoScalingGroup (Integer)
ashMaxSize f x =
    f (_ashMaxSize x)
        <&> \y -> x { _ashMaxSize = y }
{-# INLINE ashMaxSize #-}

-- | Specifies the desired capacity for the Auto Scaling group.
ashDesiredCapacity :: Lens' AutoScalingGroup (Integer)
ashDesiredCapacity f x =
    f (_ashDesiredCapacity x)
        <&> \y -> x { _ashDesiredCapacity = y }
{-# INLINE ashDesiredCapacity #-}

-- | The number of seconds after a scaling activity completes before any further
-- scaling activities can start.
ashDefaultCooldown :: Lens' AutoScalingGroup (Integer)
ashDefaultCooldown f x =
    f (_ashDefaultCooldown x)
        <&> \y -> x { _ashDefaultCooldown = y }
{-# INLINE ashDefaultCooldown #-}

-- | Contains a list of Availability Zones for the group.
ashAvailabilityZones :: Lens' AutoScalingGroup ([Text])
ashAvailabilityZones f x =
    f (_ashAvailabilityZones x)
        <&> \y -> x { _ashAvailabilityZones = y }
{-# INLINE ashAvailabilityZones #-}

-- | A list of load balancers associated with this Auto Scaling group.
ashLoadBalancerNames :: Lens' AutoScalingGroup ([Text])
ashLoadBalancerNames f x =
    f (_ashLoadBalancerNames x)
        <&> \y -> x { _ashLoadBalancerNames = y }
{-# INLINE ashLoadBalancerNames #-}

-- | The service of interest for the health status check, either "EC2" for
-- Amazon EC2 or "ELB" for Elastic Load Balancing.
ashHealthCheckType :: Lens' AutoScalingGroup (Text)
ashHealthCheckType f x =
    f (_ashHealthCheckType x)
        <&> \y -> x { _ashHealthCheckType = y }
{-# INLINE ashHealthCheckType #-}

-- | The length of time that Auto Scaling waits before checking an instance's
-- health status. The grace period begins when an instance comes into service.
ashHealthCheckGracePeriod :: Lens' AutoScalingGroup (Maybe Integer)
ashHealthCheckGracePeriod f x =
    f (_ashHealthCheckGracePeriod x)
        <&> \y -> x { _ashHealthCheckGracePeriod = y }
{-# INLINE ashHealthCheckGracePeriod #-}

-- | Provides a summary list of Amazon EC2 instances.
ashInstances :: Lens' AutoScalingGroup ([Instance])
ashInstances f x =
    f (_ashInstances x)
        <&> \y -> x { _ashInstances = y }
{-# INLINE ashInstances #-}

-- | Specifies the date and time the Auto Scaling group was created.
ashCreatedTime :: Lens' AutoScalingGroup (ISO8601)
ashCreatedTime f x =
    f (_ashCreatedTime x)
        <&> \y -> x { _ashCreatedTime = y }
{-# INLINE ashCreatedTime #-}

-- | Suspended processes associated with this Auto Scaling group.
ashSuspendedProcesses :: Lens' AutoScalingGroup ([SuspendedProcess])
ashSuspendedProcesses f x =
    f (_ashSuspendedProcesses x)
        <&> \y -> x { _ashSuspendedProcesses = y }
{-# INLINE ashSuspendedProcesses #-}

-- | The name of the cluster placement group, if applicable. For more
-- information, go to Using Cluster Instances in the Amazon EC2 User Guide.
ashPlacementGroup :: Lens' AutoScalingGroup (Maybe Text)
ashPlacementGroup f x =
    f (_ashPlacementGroup x)
        <&> \y -> x { _ashPlacementGroup = y }
{-# INLINE ashPlacementGroup #-}

-- | The subnet identifier for the Amazon VPC connection, if applicable. You can
-- specify several subnets in a comma-separated list. When you specify
-- VPCZoneIdentifier with AvailabilityZones, ensure that the subnets'
-- Availability Zones match the values you specify for AvailabilityZones.
ashVPCZoneIdentifier :: Lens' AutoScalingGroup (Maybe Text)
ashVPCZoneIdentifier f x =
    f (_ashVPCZoneIdentifier x)
        <&> \y -> x { _ashVPCZoneIdentifier = y }
{-# INLINE ashVPCZoneIdentifier #-}

-- | A list of metrics enabled for this Auto Scaling group.
ashEnabledMetrics :: Lens' AutoScalingGroup ([EnabledMetric])
ashEnabledMetrics f x =
    f (_ashEnabledMetrics x)
        <&> \y -> x { _ashEnabledMetrics = y }
{-# INLINE ashEnabledMetrics #-}

-- | The current state of the Auto Scaling group when a DeleteAutoScalingGroup
-- action is in progress.
ashStatus :: Lens' AutoScalingGroup (Maybe Text)
ashStatus f x =
    f (_ashStatus x)
        <&> \y -> x { _ashStatus = y }
{-# INLINE ashStatus #-}

-- | A list of tags for the Auto Scaling group.
ashTags :: Lens' AutoScalingGroup ([TagDescription])
ashTags f x =
    f (_ashTags x)
        <&> \y -> x { _ashTags = y }
{-# INLINE ashTags #-}

-- | A standalone termination policy or a list of termination policies for this
-- Auto Scaling group.
ashTerminationPolicies :: Lens' AutoScalingGroup ([Text])
ashTerminationPolicies f x =
    f (_ashTerminationPolicies x)
        <&> \y -> x { _ashTerminationPolicies = y }
{-# INLINE ashTerminationPolicies #-}

instance FromXML AutoScalingGroup where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "AutoScalingGroup"

-- | The AutoScalingInstanceDetails data type.
data AutoScalingInstanceDetails = AutoScalingInstanceDetails
    { _asidInstanceId :: Text
      -- ^ The instance ID of the Amazon EC2 instance.
    , _asidAutoScalingGroupName :: Text
      -- ^ The name of the Auto Scaling group associated with this instance.
    , _asidAvailabilityZone :: Text
      -- ^ The Availability Zone in which this instance resides.
    , _asidLifecycleState :: Text
      -- ^ The life cycle state of this instance. for more information, see
      -- Instance Lifecycle State in the Auto Scaling Developer Guide.
    , _asidHealthStatus :: Text
      -- ^ The health status of this instance. "Healthy" means that the
      -- instance is healthy and should remain in service. "Unhealthy"
      -- means that the instance is unhealthy. Auto Scaling should
      -- terminate and replace it.
    , _asidLaunchConfigurationName :: Text
      -- ^ The launch configuration associated with this instance.
    } deriving (Show, Generic)

-- | The instance ID of the Amazon EC2 instance.
asidInstanceId :: Lens' AutoScalingInstanceDetails (Text)
asidInstanceId f x =
    f (_asidInstanceId x)
        <&> \y -> x { _asidInstanceId = y }
{-# INLINE asidInstanceId #-}

-- | The name of the Auto Scaling group associated with this instance.
asidAutoScalingGroupName :: Lens' AutoScalingInstanceDetails (Text)
asidAutoScalingGroupName f x =
    f (_asidAutoScalingGroupName x)
        <&> \y -> x { _asidAutoScalingGroupName = y }
{-# INLINE asidAutoScalingGroupName #-}

-- | The Availability Zone in which this instance resides.
asidAvailabilityZone :: Lens' AutoScalingInstanceDetails (Text)
asidAvailabilityZone f x =
    f (_asidAvailabilityZone x)
        <&> \y -> x { _asidAvailabilityZone = y }
{-# INLINE asidAvailabilityZone #-}

-- | The life cycle state of this instance. for more information, see Instance
-- Lifecycle State in the Auto Scaling Developer Guide.
asidLifecycleState :: Lens' AutoScalingInstanceDetails (Text)
asidLifecycleState f x =
    f (_asidLifecycleState x)
        <&> \y -> x { _asidLifecycleState = y }
{-# INLINE asidLifecycleState #-}

-- | The health status of this instance. "Healthy" means that the instance is
-- healthy and should remain in service. "Unhealthy" means that the instance
-- is unhealthy. Auto Scaling should terminate and replace it.
asidHealthStatus :: Lens' AutoScalingInstanceDetails (Text)
asidHealthStatus f x =
    f (_asidHealthStatus x)
        <&> \y -> x { _asidHealthStatus = y }
{-# INLINE asidHealthStatus #-}

-- | The launch configuration associated with this instance.
asidLaunchConfigurationName :: Lens' AutoScalingInstanceDetails (Text)
asidLaunchConfigurationName f x =
    f (_asidLaunchConfigurationName x)
        <&> \y -> x { _asidLaunchConfigurationName = y }
{-# INLINE asidLaunchConfigurationName #-}

instance FromXML AutoScalingInstanceDetails where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "AutoScalingInstanceDetails"

-- | The BlockDeviceMapping data type.
data BlockDeviceMapping = BlockDeviceMapping
    { _bdnVirtualName :: Maybe Text
      -- ^ The virtual name associated with the device.
    , _bdnDeviceName :: Text
      -- ^ The name of the device within Amazon EC2 (for example, /dev/sdh
      -- or xvdh).
    , _bdnEbs :: Maybe Ebs
      -- ^ The Elastic Block Storage volume information.
    , _bdnNoDevice :: Maybe Bool
      -- ^ Suppresses the device mapping. If NoDevice is set to true for the
      -- root device, the instance might fail the EC2 health check. Auto
      -- Scaling launches a replacement instance if the instance fails the
      -- health check.
    } deriving (Show, Generic)

-- | The virtual name associated with the device.
bdnVirtualName :: Lens' BlockDeviceMapping (Maybe Text)
bdnVirtualName f x =
    f (_bdnVirtualName x)
        <&> \y -> x { _bdnVirtualName = y }
{-# INLINE bdnVirtualName #-}

-- | The name of the device within Amazon EC2 (for example, /dev/sdh or xvdh).
bdnDeviceName :: Lens' BlockDeviceMapping (Text)
bdnDeviceName f x =
    f (_bdnDeviceName x)
        <&> \y -> x { _bdnDeviceName = y }
{-# INLINE bdnDeviceName #-}

-- | The Elastic Block Storage volume information.
bdnEbs :: Lens' BlockDeviceMapping (Maybe Ebs)
bdnEbs f x =
    f (_bdnEbs x)
        <&> \y -> x { _bdnEbs = y }
{-# INLINE bdnEbs #-}

-- | Suppresses the device mapping. If NoDevice is set to true for the root
-- device, the instance might fail the EC2 health check. Auto Scaling launches
-- a replacement instance if the instance fails the health check.
bdnNoDevice :: Lens' BlockDeviceMapping (Maybe Bool)
bdnNoDevice f x =
    f (_bdnNoDevice x)
        <&> \y -> x { _bdnNoDevice = y }
{-# INLINE bdnNoDevice #-}

instance FromXML BlockDeviceMapping where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "BlockDeviceMapping"

instance ToQuery BlockDeviceMapping where
    toQuery = genericQuery def

-- | The Elastic Block Storage volume information.
data Ebs = Ebs
    { _eSnapshotId :: Maybe Text
      -- ^ The snapshot ID.
    , _eVolumeSize :: Maybe Integer
      -- ^ The volume size, in gigabytes. Valid values: If the volume type
      -- is io1, the minimum size of the volume is 10. Default: If you're
      -- creating the volume from a snapshot, and you don't specify a
      -- volume size, the default is the snapshot size. Required: Required
      -- when the volume type is io1.
    , _eVolumeType :: Maybe Text
      -- ^ The volume type. Valid values: standard | io1 Default: standard.
    , _eDeleteOnTermination :: Maybe Bool
      -- ^ Indicates whether to delete the volume on instance termination.
      -- Default: true.
    , _eIops :: Maybe Integer
      -- ^ The number of I/O operations per second (IOPS) that the volume
      -- supports. The maximum ratio of IOPS to volume size is 30.0 Valid
      -- Values: Range is 100 to 4000. Default: None.
    } deriving (Show, Generic)

-- | The snapshot ID.
eSnapshotId :: Lens' Ebs (Maybe Text)
eSnapshotId f x =
    f (_eSnapshotId x)
        <&> \y -> x { _eSnapshotId = y }
{-# INLINE eSnapshotId #-}

-- | The volume size, in gigabytes. Valid values: If the volume type is io1, the
-- minimum size of the volume is 10. Default: If you're creating the volume
-- from a snapshot, and you don't specify a volume size, the default is the
-- snapshot size. Required: Required when the volume type is io1.
eVolumeSize :: Lens' Ebs (Maybe Integer)
eVolumeSize f x =
    f (_eVolumeSize x)
        <&> \y -> x { _eVolumeSize = y }
{-# INLINE eVolumeSize #-}

-- | The volume type. Valid values: standard | io1 Default: standard.
eVolumeType :: Lens' Ebs (Maybe Text)
eVolumeType f x =
    f (_eVolumeType x)
        <&> \y -> x { _eVolumeType = y }
{-# INLINE eVolumeType #-}

-- | Indicates whether to delete the volume on instance termination. Default:
-- true.
eDeleteOnTermination :: Lens' Ebs (Maybe Bool)
eDeleteOnTermination f x =
    f (_eDeleteOnTermination x)
        <&> \y -> x { _eDeleteOnTermination = y }
{-# INLINE eDeleteOnTermination #-}

-- | The number of I/O operations per second (IOPS) that the volume supports.
-- The maximum ratio of IOPS to volume size is 30.0 Valid Values: Range is 100
-- to 4000. Default: None.
eIops :: Lens' Ebs (Maybe Integer)
eIops f x =
    f (_eIops x)
        <&> \y -> x { _eIops = y }
{-# INLINE eIops #-}

instance FromXML Ebs where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Ebs"

instance ToQuery Ebs where
    toQuery = genericQuery def

-- | The EnabledMetric data type.
data EnabledMetric = EnabledMetric
    { _enMetric :: Maybe Text
      -- ^ The name of the enabled metric.
    , _enGranularity :: Maybe Text
      -- ^ The granularity of the enabled metric.
    } deriving (Show, Generic)

-- | The name of the enabled metric.
enMetric :: Lens' EnabledMetric (Maybe Text)
enMetric f x =
    f (_enMetric x)
        <&> \y -> x { _enMetric = y }
{-# INLINE enMetric #-}

-- | The granularity of the enabled metric.
enGranularity :: Lens' EnabledMetric (Maybe Text)
enGranularity f x =
    f (_enGranularity x)
        <&> \y -> x { _enGranularity = y }
{-# INLINE enGranularity #-}

instance FromXML EnabledMetric where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "EnabledMetric"

instance ToQuery EnabledMetric where
    toQuery = genericQuery def

-- | The Filter data type.
data Filter = Filter
    { _frName :: Text
      -- ^ The name of the filter. Valid Name values are:
      -- "auto-scaling-group", "key", "value", and "propagate-at-launch".
    , _frValues :: [Text]
      -- ^ The value of the filter.
    } deriving (Show, Generic)

-- | The name of the filter. Valid Name values are: "auto-scaling-group", "key",
-- "value", and "propagate-at-launch".
frName :: Lens' Filter (Text)
frName f x =
    f (_frName x)
        <&> \y -> x { _frName = y }
{-# INLINE frName #-}

-- | The value of the filter.
frValues :: Lens' Filter ([Text])
frValues f x =
    f (_frValues x)
        <&> \y -> x { _frValues = y }
{-# INLINE frValues #-}

instance ToQuery Filter where
    toQuery = genericQuery def

-- | The Instance data type.
data Instance = Instance
    { _pInstanceId :: Text
      -- ^ Specifies the ID of the Amazon EC2 instance.
    , _pAvailabilityZone :: Text
      -- ^ Availability Zones associated with this instance.
    , _pLifecycleState :: LifecycleState
      -- ^ Contains a description of the current lifecycle state. The
      -- Quarantined lifecycle state is currently not used.
    , _pHealthStatus :: Text
      -- ^ The instance's health status.
    , _pLaunchConfigurationName :: Text
      -- ^ The launch configuration associated with this instance.
    } deriving (Show, Generic)

-- | Specifies the ID of the Amazon EC2 instance.
pInstanceId :: Lens' Instance (Text)
pInstanceId f x =
    f (_pInstanceId x)
        <&> \y -> x { _pInstanceId = y }
{-# INLINE pInstanceId #-}

-- | Availability Zones associated with this instance.
pAvailabilityZone :: Lens' Instance (Text)
pAvailabilityZone f x =
    f (_pAvailabilityZone x)
        <&> \y -> x { _pAvailabilityZone = y }
{-# INLINE pAvailabilityZone #-}

-- | Contains a description of the current lifecycle state. The Quarantined
-- lifecycle state is currently not used.
pLifecycleState :: Lens' Instance (LifecycleState)
pLifecycleState f x =
    f (_pLifecycleState x)
        <&> \y -> x { _pLifecycleState = y }
{-# INLINE pLifecycleState #-}

-- | The instance's health status.
pHealthStatus :: Lens' Instance (Text)
pHealthStatus f x =
    f (_pHealthStatus x)
        <&> \y -> x { _pHealthStatus = y }
{-# INLINE pHealthStatus #-}

-- | The launch configuration associated with this instance.
pLaunchConfigurationName :: Lens' Instance (Text)
pLaunchConfigurationName f x =
    f (_pLaunchConfigurationName x)
        <&> \y -> x { _pLaunchConfigurationName = y }
{-# INLINE pLaunchConfigurationName #-}

instance FromXML Instance where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Instance"

instance ToQuery Instance where
    toQuery = genericQuery def

-- | The LaunchConfiguration data type.
data LaunchConfiguration = LaunchConfiguration
    { _ldLaunchConfigurationName :: Text
      -- ^ Specifies the name of the launch configuration.
    , _ldLaunchConfigurationARN :: Maybe Text
      -- ^ The launch configuration's Amazon Resource Name (ARN).
    , _ldImageId :: Text
      -- ^ Provides the unique ID of the Amazon Machine Image (AMI) that was
      -- assigned during registration.
    , _ldKeyName :: Maybe Text
      -- ^ Provides the name of the Amazon EC2 key pair.
    , _ldSecurityGroups :: [Text]
      -- ^ A description of the security groups to associate with the Amazon
      -- EC2 instances.
    , _ldUserData :: Maybe Text
      -- ^ The user data available to the launched Amazon EC2 instances.
    , _ldInstanceType :: Text
      -- ^ Specifies the instance type of the Amazon EC2 instance.
    , _ldKernelId :: Maybe Text
      -- ^ Provides the ID of the kernel associated with the Amazon EC2 AMI.
    , _ldRamdiskId :: Maybe Text
      -- ^ Provides ID of the RAM disk associated with the Amazon EC2 AMI.
    , _ldBlockDeviceMappings :: [BlockDeviceMapping]
      -- ^ Specifies how block devices are exposed to the instance. Each
      -- mapping is made up of a virtualName and a deviceName.
    , _ldInstanceMonitoring :: Maybe InstanceMonitoring
      -- ^ Controls whether instances in this group are launched with
      -- detailed monitoring or not.
    , _ldSpotPrice :: Maybe Text
      -- ^ Specifies the price to bid when launching Spot Instances.
    , _ldIamInstanceProfile :: Maybe Text
      -- ^ Provides the name or the Amazon Resource Name (ARN) of the
      -- instance profile associated with the IAM role for the instance.
      -- The instance profile contains the IAM role.
    , _ldCreatedTime :: ISO8601
      -- ^ Provides the creation date and time for this launch
      -- configuration.
    , _ldEbsOptimized :: Maybe Bool
      -- ^ Specifies whether the instance is optimized for EBS I/O (true) or
      -- not (false).
    , _ldAssociatePublicIpAddress :: Maybe Bool
      -- ^ Specifies whether the instance is associated with a public IP
      -- address (true) or not (false).
    , _ldPlacementTenancy :: Maybe Text
      -- ^ Specifies the tenancy of the instance. It can be either default
      -- or dedicated. An instance with dedicated tenancy runs in an
      -- isolated, single-tenant hardware and it can only be launched in a
      -- VPC.
    } deriving (Show, Generic)

-- | Specifies the name of the launch configuration.
ldLaunchConfigurationName :: Lens' LaunchConfiguration (Text)
ldLaunchConfigurationName f x =
    f (_ldLaunchConfigurationName x)
        <&> \y -> x { _ldLaunchConfigurationName = y }
{-# INLINE ldLaunchConfigurationName #-}

-- | The launch configuration's Amazon Resource Name (ARN).
ldLaunchConfigurationARN :: Lens' LaunchConfiguration (Maybe Text)
ldLaunchConfigurationARN f x =
    f (_ldLaunchConfigurationARN x)
        <&> \y -> x { _ldLaunchConfigurationARN = y }
{-# INLINE ldLaunchConfigurationARN #-}

-- | Provides the unique ID of the Amazon Machine Image (AMI) that was assigned
-- during registration.
ldImageId :: Lens' LaunchConfiguration (Text)
ldImageId f x =
    f (_ldImageId x)
        <&> \y -> x { _ldImageId = y }
{-# INLINE ldImageId #-}

-- | Provides the name of the Amazon EC2 key pair.
ldKeyName :: Lens' LaunchConfiguration (Maybe Text)
ldKeyName f x =
    f (_ldKeyName x)
        <&> \y -> x { _ldKeyName = y }
{-# INLINE ldKeyName #-}

-- | A description of the security groups to associate with the Amazon EC2
-- instances.
ldSecurityGroups :: Lens' LaunchConfiguration ([Text])
ldSecurityGroups f x =
    f (_ldSecurityGroups x)
        <&> \y -> x { _ldSecurityGroups = y }
{-# INLINE ldSecurityGroups #-}

-- | The user data available to the launched Amazon EC2 instances.
ldUserData :: Lens' LaunchConfiguration (Maybe Text)
ldUserData f x =
    f (_ldUserData x)
        <&> \y -> x { _ldUserData = y }
{-# INLINE ldUserData #-}

-- | Specifies the instance type of the Amazon EC2 instance.
ldInstanceType :: Lens' LaunchConfiguration (Text)
ldInstanceType f x =
    f (_ldInstanceType x)
        <&> \y -> x { _ldInstanceType = y }
{-# INLINE ldInstanceType #-}

-- | Provides the ID of the kernel associated with the Amazon EC2 AMI.
ldKernelId :: Lens' LaunchConfiguration (Maybe Text)
ldKernelId f x =
    f (_ldKernelId x)
        <&> \y -> x { _ldKernelId = y }
{-# INLINE ldKernelId #-}

-- | Provides ID of the RAM disk associated with the Amazon EC2 AMI.
ldRamdiskId :: Lens' LaunchConfiguration (Maybe Text)
ldRamdiskId f x =
    f (_ldRamdiskId x)
        <&> \y -> x { _ldRamdiskId = y }
{-# INLINE ldRamdiskId #-}

-- | Specifies how block devices are exposed to the instance. Each mapping is
-- made up of a virtualName and a deviceName.
ldBlockDeviceMappings :: Lens' LaunchConfiguration ([BlockDeviceMapping])
ldBlockDeviceMappings f x =
    f (_ldBlockDeviceMappings x)
        <&> \y -> x { _ldBlockDeviceMappings = y }
{-# INLINE ldBlockDeviceMappings #-}

-- | Controls whether instances in this group are launched with detailed
-- monitoring or not.
ldInstanceMonitoring :: Lens' LaunchConfiguration (Maybe InstanceMonitoring)
ldInstanceMonitoring f x =
    f (_ldInstanceMonitoring x)
        <&> \y -> x { _ldInstanceMonitoring = y }
{-# INLINE ldInstanceMonitoring #-}

-- | Specifies the price to bid when launching Spot Instances.
ldSpotPrice :: Lens' LaunchConfiguration (Maybe Text)
ldSpotPrice f x =
    f (_ldSpotPrice x)
        <&> \y -> x { _ldSpotPrice = y }
{-# INLINE ldSpotPrice #-}

-- | Provides the name or the Amazon Resource Name (ARN) of the instance profile
-- associated with the IAM role for the instance. The instance profile
-- contains the IAM role.
ldIamInstanceProfile :: Lens' LaunchConfiguration (Maybe Text)
ldIamInstanceProfile f x =
    f (_ldIamInstanceProfile x)
        <&> \y -> x { _ldIamInstanceProfile = y }
{-# INLINE ldIamInstanceProfile #-}

-- | Provides the creation date and time for this launch configuration.
ldCreatedTime :: Lens' LaunchConfiguration (ISO8601)
ldCreatedTime f x =
    f (_ldCreatedTime x)
        <&> \y -> x { _ldCreatedTime = y }
{-# INLINE ldCreatedTime #-}

-- | Specifies whether the instance is optimized for EBS I/O (true) or not
-- (false).
ldEbsOptimized :: Lens' LaunchConfiguration (Maybe Bool)
ldEbsOptimized f x =
    f (_ldEbsOptimized x)
        <&> \y -> x { _ldEbsOptimized = y }
{-# INLINE ldEbsOptimized #-}

-- | Specifies whether the instance is associated with a public IP address
-- (true) or not (false).
ldAssociatePublicIpAddress :: Lens' LaunchConfiguration (Maybe Bool)
ldAssociatePublicIpAddress f x =
    f (_ldAssociatePublicIpAddress x)
        <&> \y -> x { _ldAssociatePublicIpAddress = y }
{-# INLINE ldAssociatePublicIpAddress #-}

-- | Specifies the tenancy of the instance. It can be either default or
-- dedicated. An instance with dedicated tenancy runs in an isolated,
-- single-tenant hardware and it can only be launched in a VPC.
ldPlacementTenancy :: Lens' LaunchConfiguration (Maybe Text)
ldPlacementTenancy f x =
    f (_ldPlacementTenancy x)
        <&> \y -> x { _ldPlacementTenancy = y }
{-# INLINE ldPlacementTenancy #-}

instance FromXML LaunchConfiguration where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "LaunchConfiguration"

-- | A lifecycle hook tells Auto Scaling that you want to perform an action when
-- an instance launches or terminates. When you have a lifecycle hook in
-- place, the Auto Scaling group will either: Pause the instance after it
-- launches, but before it is put into service Pause the instance as it
-- terminates, but before it is fully terminated To learn more, see Auto
-- Scaling Pending State and Auto Scaling Terminating State.
data LifecycleHook = LifecycleHook
    { _liLifecycleHookName :: Maybe Text
      -- ^ The name of the lifecycle action hook.
    , _liAutoScalingGroupName :: Maybe Text
      -- ^ The name of the Auto Scaling group to which the lifecycle action
      -- belongs.
    , _liLifecycleTransition :: Maybe Text
      -- ^ The Amazon EC2 instance state to which you want to attach the
      -- lifecycle hook. See DescribeLifecycleHooks for a list of
      -- available lifecycle hook types.
    , _liNotificationTargetARN :: Maybe Text
      -- ^ The ARN of the notification target that Auto Scaling will use to
      -- notify you when an instance is in the transition state for the
      -- lifecycle hook. This ARN target can be either an SQS queue or an
      -- SNS topic. The notification message sent to the target will
      -- include: Lifecycle action token User account ID Name of the Auto
      -- Scaling group Lifecycle hook name EC2 instance ID Lifecycle
      -- transition Notification metadata.
    , _liRoleARN :: Maybe Text
      -- ^ The ARN of the Amazon IAM role that allows the Auto Scaling group
      -- to publish to the specified notification target.
    , _liNotificationMetadata :: Maybe Text
      -- ^ Contains additional information that you want to include any time
      -- Auto Scaling sends a message to the notification target.
    , _liHeartbeatTimeout :: Maybe Integer
      -- ^ Defines the amount of time that can elapse before the lifecycle
      -- hook times out. When the lifecycle hook times out, Auto Scaling
      -- performs the action defined in the DefaultResult parameter. You
      -- can prevent the lifecycle hook from timing out by calling
      -- RecordLifecycleActionHeartbeat.
    , _liGlobalTimeout :: Maybe Integer
      -- ^ The maximum length of time an instance can remain in a
      -- Pending:Wait or Terminating:Wait state. Currently, this value is
      -- set at 48 hours.
    , _liDefaultResult :: Maybe Text
      -- ^ Defines the action the Auto Scaling group should take when the
      -- lifecycle hook timeout elapses or if an unexpected failure
      -- occurs. The value for this parameter can be either CONTINUE or
      -- ABANDON. The default value for this parameter is CONTINUE.
    } deriving (Show, Generic)

-- | The name of the lifecycle action hook.
liLifecycleHookName :: Lens' LifecycleHook (Maybe Text)
liLifecycleHookName f x =
    f (_liLifecycleHookName x)
        <&> \y -> x { _liLifecycleHookName = y }
{-# INLINE liLifecycleHookName #-}

-- | The name of the Auto Scaling group to which the lifecycle action belongs.
liAutoScalingGroupName :: Lens' LifecycleHook (Maybe Text)
liAutoScalingGroupName f x =
    f (_liAutoScalingGroupName x)
        <&> \y -> x { _liAutoScalingGroupName = y }
{-# INLINE liAutoScalingGroupName #-}

-- | The Amazon EC2 instance state to which you want to attach the lifecycle
-- hook. See DescribeLifecycleHooks for a list of available lifecycle hook
-- types.
liLifecycleTransition :: Lens' LifecycleHook (Maybe Text)
liLifecycleTransition f x =
    f (_liLifecycleTransition x)
        <&> \y -> x { _liLifecycleTransition = y }
{-# INLINE liLifecycleTransition #-}

-- | The ARN of the notification target that Auto Scaling will use to notify you
-- when an instance is in the transition state for the lifecycle hook. This
-- ARN target can be either an SQS queue or an SNS topic. The notification
-- message sent to the target will include: Lifecycle action token User
-- account ID Name of the Auto Scaling group Lifecycle hook name EC2 instance
-- ID Lifecycle transition Notification metadata.
liNotificationTargetARN :: Lens' LifecycleHook (Maybe Text)
liNotificationTargetARN f x =
    f (_liNotificationTargetARN x)
        <&> \y -> x { _liNotificationTargetARN = y }
{-# INLINE liNotificationTargetARN #-}

-- | The ARN of the Amazon IAM role that allows the Auto Scaling group to
-- publish to the specified notification target.
liRoleARN :: Lens' LifecycleHook (Maybe Text)
liRoleARN f x =
    f (_liRoleARN x)
        <&> \y -> x { _liRoleARN = y }
{-# INLINE liRoleARN #-}

-- | Contains additional information that you want to include any time Auto
-- Scaling sends a message to the notification target.
liNotificationMetadata :: Lens' LifecycleHook (Maybe Text)
liNotificationMetadata f x =
    f (_liNotificationMetadata x)
        <&> \y -> x { _liNotificationMetadata = y }
{-# INLINE liNotificationMetadata #-}

-- | Defines the amount of time that can elapse before the lifecycle hook times
-- out. When the lifecycle hook times out, Auto Scaling performs the action
-- defined in the DefaultResult parameter. You can prevent the lifecycle hook
-- from timing out by calling RecordLifecycleActionHeartbeat.
liHeartbeatTimeout :: Lens' LifecycleHook (Maybe Integer)
liHeartbeatTimeout f x =
    f (_liHeartbeatTimeout x)
        <&> \y -> x { _liHeartbeatTimeout = y }
{-# INLINE liHeartbeatTimeout #-}

-- | The maximum length of time an instance can remain in a Pending:Wait or
-- Terminating:Wait state. Currently, this value is set at 48 hours.
liGlobalTimeout :: Lens' LifecycleHook (Maybe Integer)
liGlobalTimeout f x =
    f (_liGlobalTimeout x)
        <&> \y -> x { _liGlobalTimeout = y }
{-# INLINE liGlobalTimeout #-}

-- | Defines the action the Auto Scaling group should take when the lifecycle
-- hook timeout elapses or if an unexpected failure occurs. The value for this
-- parameter can be either CONTINUE or ABANDON. The default value for this
-- parameter is CONTINUE.
liDefaultResult :: Lens' LifecycleHook (Maybe Text)
liDefaultResult f x =
    f (_liDefaultResult x)
        <&> \y -> x { _liDefaultResult = y }
{-# INLINE liDefaultResult #-}

instance FromXML LifecycleHook where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "LifecycleHook"

-- | The NotificationConfiguration data type.
data NotificationConfiguration = NotificationConfiguration
    { _nfAutoScalingGroupName :: Maybe Text
      -- ^ Specifies the Auto Scaling group name.
    , _nfTopicARN :: Maybe Text
      -- ^ The Amazon Resource Name (ARN) of the Amazon Simple Notification
      -- Service (SNS) topic.
    , _nfNotificationType :: Maybe Text
      -- ^ The types of events for an action to start.
    } deriving (Show, Generic)

-- | Specifies the Auto Scaling group name.
nfAutoScalingGroupName :: Lens' NotificationConfiguration (Maybe Text)
nfAutoScalingGroupName f x =
    f (_nfAutoScalingGroupName x)
        <&> \y -> x { _nfAutoScalingGroupName = y }
{-# INLINE nfAutoScalingGroupName #-}

-- | The Amazon Resource Name (ARN) of the Amazon Simple Notification Service
-- (SNS) topic.
nfTopicARN :: Lens' NotificationConfiguration (Maybe Text)
nfTopicARN f x =
    f (_nfTopicARN x)
        <&> \y -> x { _nfTopicARN = y }
{-# INLINE nfTopicARN #-}

-- | The types of events for an action to start.
nfNotificationType :: Lens' NotificationConfiguration (Maybe Text)
nfNotificationType f x =
    f (_nfNotificationType x)
        <&> \y -> x { _nfNotificationType = y }
{-# INLINE nfNotificationType #-}

instance FromXML NotificationConfiguration where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "NotificationConfiguration"

-- | The ScalingPolicy data type.
data ScalingPolicy = ScalingPolicy
    { _suAutoScalingGroupName :: Maybe Text
      -- ^ The name of the Auto Scaling group associated with this scaling
      -- policy.
    , _suPolicyName :: Maybe Text
      -- ^ The name of the scaling policy.
    , _suScalingAdjustment :: Maybe Integer
      -- ^ The number associated with the specified adjustment type. A
      -- positive value adds to the current capacity and a negative value
      -- removes from the current capacity.
    , _suAdjustmentType :: Maybe Text
      -- ^ Specifies whether the ScalingAdjustment is an absolute number or
      -- a percentage of the current capacity. Valid values are
      -- ChangeInCapacity, ExactCapacity, and PercentChangeInCapacity.
    , _suCooldown :: Maybe Integer
      -- ^ The amount of time, in seconds, after a scaling activity
      -- completes before any further trigger-related scaling activities
      -- can start.
    , _suPolicyARN :: Maybe Text
      -- ^ The Amazon Resource Name (ARN) of the policy.
    , _suAlarms :: [Alarm]
      -- ^ A list of CloudWatch Alarms related to the policy.
    , _suMinAdjustmentStep :: Maybe Integer
      -- ^ Changes the DesiredCapacity of the Auto Scaling group by at least
      -- the specified number of instances.
    } deriving (Show, Generic)

-- | The name of the Auto Scaling group associated with this scaling policy.
suAutoScalingGroupName :: Lens' ScalingPolicy (Maybe Text)
suAutoScalingGroupName f x =
    f (_suAutoScalingGroupName x)
        <&> \y -> x { _suAutoScalingGroupName = y }
{-# INLINE suAutoScalingGroupName #-}

-- | The name of the scaling policy.
suPolicyName :: Lens' ScalingPolicy (Maybe Text)
suPolicyName f x =
    f (_suPolicyName x)
        <&> \y -> x { _suPolicyName = y }
{-# INLINE suPolicyName #-}

-- | The number associated with the specified adjustment type. A positive value
-- adds to the current capacity and a negative value removes from the current
-- capacity.
suScalingAdjustment :: Lens' ScalingPolicy (Maybe Integer)
suScalingAdjustment f x =
    f (_suScalingAdjustment x)
        <&> \y -> x { _suScalingAdjustment = y }
{-# INLINE suScalingAdjustment #-}

-- | Specifies whether the ScalingAdjustment is an absolute number or a
-- percentage of the current capacity. Valid values are ChangeInCapacity,
-- ExactCapacity, and PercentChangeInCapacity.
suAdjustmentType :: Lens' ScalingPolicy (Maybe Text)
suAdjustmentType f x =
    f (_suAdjustmentType x)
        <&> \y -> x { _suAdjustmentType = y }
{-# INLINE suAdjustmentType #-}

-- | The amount of time, in seconds, after a scaling activity completes before
-- any further trigger-related scaling activities can start.
suCooldown :: Lens' ScalingPolicy (Maybe Integer)
suCooldown f x =
    f (_suCooldown x)
        <&> \y -> x { _suCooldown = y }
{-# INLINE suCooldown #-}

-- | The Amazon Resource Name (ARN) of the policy.
suPolicyARN :: Lens' ScalingPolicy (Maybe Text)
suPolicyARN f x =
    f (_suPolicyARN x)
        <&> \y -> x { _suPolicyARN = y }
{-# INLINE suPolicyARN #-}

-- | A list of CloudWatch Alarms related to the policy.
suAlarms :: Lens' ScalingPolicy ([Alarm])
suAlarms f x =
    f (_suAlarms x)
        <&> \y -> x { _suAlarms = y }
{-# INLINE suAlarms #-}

-- | Changes the DesiredCapacity of the Auto Scaling group by at least the
-- specified number of instances.
suMinAdjustmentStep :: Lens' ScalingPolicy (Maybe Integer)
suMinAdjustmentStep f x =
    f (_suMinAdjustmentStep x)
        <&> \y -> x { _suMinAdjustmentStep = y }
{-# INLINE suMinAdjustmentStep #-}

instance FromXML ScalingPolicy where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ScalingPolicy"

-- | This data type stores information about a scheduled update to an Auto
-- Scaling group.
data ScheduledUpdateGroupAction = ScheduledUpdateGroupAction
    { _sugbAutoScalingGroupName :: Maybe Text
      -- ^ The name of the Auto Scaling group to be updated.
    , _sugbScheduledActionName :: Maybe Text
      -- ^ The name of this scheduled action.
    , _sugbScheduledActionARN :: Maybe Text
      -- ^ The Amazon Resource Name (ARN) of this scheduled action.
    , _sugbTime :: Maybe ISO8601
      -- ^ Time is deprecated. The time that the action is scheduled to
      -- begin. Time is an alias for StartTime.
    , _sugbStartTime :: Maybe ISO8601
      -- ^ The time that the action is scheduled to begin. This value can be
      -- up to one month in the future. When StartTime and EndTime are
      -- specified with Recurrence, they form the boundaries of when the
      -- recurring action will start and stop.
    , _sugbEndTime :: Maybe ISO8601
      -- ^ The time that the action is scheduled to end. This value can be
      -- up to one month in the future.
    , _sugbRecurrence :: Maybe Text
      -- ^ The regular schedule that an action occurs.
    , _sugbMinSize :: Maybe Integer
      -- ^ The minimum size of the Auto Scaling group.
    , _sugbMaxSize :: Maybe Integer
      -- ^ The maximum size of the Auto Scaling group.
    , _sugbDesiredCapacity :: Maybe Integer
      -- ^ The number of instances you prefer to maintain in your Auto
      -- Scaling group.
    } deriving (Show, Generic)

-- | The name of the Auto Scaling group to be updated.
sugbAutoScalingGroupName :: Lens' ScheduledUpdateGroupAction (Maybe Text)
sugbAutoScalingGroupName f x =
    f (_sugbAutoScalingGroupName x)
        <&> \y -> x { _sugbAutoScalingGroupName = y }
{-# INLINE sugbAutoScalingGroupName #-}

-- | The name of this scheduled action.
sugbScheduledActionName :: Lens' ScheduledUpdateGroupAction (Maybe Text)
sugbScheduledActionName f x =
    f (_sugbScheduledActionName x)
        <&> \y -> x { _sugbScheduledActionName = y }
{-# INLINE sugbScheduledActionName #-}

-- | The Amazon Resource Name (ARN) of this scheduled action.
sugbScheduledActionARN :: Lens' ScheduledUpdateGroupAction (Maybe Text)
sugbScheduledActionARN f x =
    f (_sugbScheduledActionARN x)
        <&> \y -> x { _sugbScheduledActionARN = y }
{-# INLINE sugbScheduledActionARN #-}

-- | Time is deprecated. The time that the action is scheduled to begin. Time is
-- an alias for StartTime.
sugbTime :: Lens' ScheduledUpdateGroupAction (Maybe ISO8601)
sugbTime f x =
    f (_sugbTime x)
        <&> \y -> x { _sugbTime = y }
{-# INLINE sugbTime #-}

-- | The time that the action is scheduled to begin. This value can be up to one
-- month in the future. When StartTime and EndTime are specified with
-- Recurrence, they form the boundaries of when the recurring action will
-- start and stop.
sugbStartTime :: Lens' ScheduledUpdateGroupAction (Maybe ISO8601)
sugbStartTime f x =
    f (_sugbStartTime x)
        <&> \y -> x { _sugbStartTime = y }
{-# INLINE sugbStartTime #-}

-- | The time that the action is scheduled to end. This value can be up to one
-- month in the future.
sugbEndTime :: Lens' ScheduledUpdateGroupAction (Maybe ISO8601)
sugbEndTime f x =
    f (_sugbEndTime x)
        <&> \y -> x { _sugbEndTime = y }
{-# INLINE sugbEndTime #-}

-- | The regular schedule that an action occurs.
sugbRecurrence :: Lens' ScheduledUpdateGroupAction (Maybe Text)
sugbRecurrence f x =
    f (_sugbRecurrence x)
        <&> \y -> x { _sugbRecurrence = y }
{-# INLINE sugbRecurrence #-}

-- | The minimum size of the Auto Scaling group.
sugbMinSize :: Lens' ScheduledUpdateGroupAction (Maybe Integer)
sugbMinSize f x =
    f (_sugbMinSize x)
        <&> \y -> x { _sugbMinSize = y }
{-# INLINE sugbMinSize #-}

-- | The maximum size of the Auto Scaling group.
sugbMaxSize :: Lens' ScheduledUpdateGroupAction (Maybe Integer)
sugbMaxSize f x =
    f (_sugbMaxSize x)
        <&> \y -> x { _sugbMaxSize = y }
{-# INLINE sugbMaxSize #-}

-- | The number of instances you prefer to maintain in your Auto Scaling group.
sugbDesiredCapacity :: Lens' ScheduledUpdateGroupAction (Maybe Integer)
sugbDesiredCapacity f x =
    f (_sugbDesiredCapacity x)
        <&> \y -> x { _sugbDesiredCapacity = y }
{-# INLINE sugbDesiredCapacity #-}

instance FromXML ScheduledUpdateGroupAction where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ScheduledUpdateGroupAction"

-- | An Auto Scaling process that has been suspended. For more information, see
-- ProcessType.
data SuspendedProcess = SuspendedProcess
    { _srProcessName :: Maybe Text
      -- ^ The name of the suspended process.
    , _srSuspensionReason :: Maybe Text
      -- ^ The reason that the process was suspended.
    } deriving (Show, Generic)

-- | The name of the suspended process.
srProcessName :: Lens' SuspendedProcess (Maybe Text)
srProcessName f x =
    f (_srProcessName x)
        <&> \y -> x { _srProcessName = y }
{-# INLINE srProcessName #-}

-- | The reason that the process was suspended.
srSuspensionReason :: Lens' SuspendedProcess (Maybe Text)
srSuspensionReason f x =
    f (_srSuspensionReason x)
        <&> \y -> x { _srSuspensionReason = y }
{-# INLINE srSuspensionReason #-}

instance FromXML SuspendedProcess where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "SuspendedProcess"

instance ToQuery SuspendedProcess where
    toQuery = genericQuery def

-- | The tag applied to an Auto Scaling group.
data Tag = Tag
    { _uResourceId :: Text
      -- ^ The name of the Auto Scaling group.
    , _uResourceType :: Text
      -- ^ The kind of resource to which the tag is applied. Currently, Auto
      -- Scaling supports the auto-scaling-group resource type.
    , _uKey :: Text
      -- ^ The key of the tag.
    , _uValue :: Text
      -- ^ The value of the tag.
    , _uPropagateAtLaunch :: Bool
      -- ^ Specifies whether the new tag will be applied to instances
      -- launched after the tag is created. The same behavior applies to
      -- updates: If you change a tag, the changed tag will be applied to
      -- all instances launched after you made the change.
    } deriving (Show, Generic)

-- | The name of the Auto Scaling group.
uResourceId :: Lens' Tag (Text)
uResourceId f x =
    f (_uResourceId x)
        <&> \y -> x { _uResourceId = y }
{-# INLINE uResourceId #-}

-- | The kind of resource to which the tag is applied. Currently, Auto Scaling
-- supports the auto-scaling-group resource type.
uResourceType :: Lens' Tag (Text)
uResourceType f x =
    f (_uResourceType x)
        <&> \y -> x { _uResourceType = y }
{-# INLINE uResourceType #-}

-- | The key of the tag.
uKey :: Lens' Tag (Text)
uKey f x =
    f (_uKey x)
        <&> \y -> x { _uKey = y }
{-# INLINE uKey #-}

-- | The value of the tag.
uValue :: Lens' Tag (Text)
uValue f x =
    f (_uValue x)
        <&> \y -> x { _uValue = y }
{-# INLINE uValue #-}

-- | Specifies whether the new tag will be applied to instances launched after
-- the tag is created. The same behavior applies to updates: If you change a
-- tag, the changed tag will be applied to all instances launched after you
-- made the change.
uPropagateAtLaunch :: Lens' Tag (Bool)
uPropagateAtLaunch f x =
    f (_uPropagateAtLaunch x)
        <&> \y -> x { _uPropagateAtLaunch = y }
{-# INLINE uPropagateAtLaunch #-}

instance ToQuery Tag where
    toQuery = genericQuery def

-- | The tag applied to an Auto Scaling group.
data TagDescription = TagDescription
    { _tdResourceId :: Text
      -- ^ The name of the Auto Scaling group.
    , _tdResourceType :: Text
      -- ^ The kind of resource to which the tag is applied. Currently, Auto
      -- Scaling supports the auto-scaling-group resource type.
    , _tdKey :: Text
      -- ^ The key of the tag.
    , _tdValue :: Text
      -- ^ The value of the tag.
    , _tdPropagateAtLaunch :: Bool
      -- ^ Specifies whether the new tag will be applied to instances
      -- launched after the tag is created. The same behavior applies to
      -- updates: If you change a tag, the changed tag will be applied to
      -- all instances launched after you made the change.
    } deriving (Show, Generic)

-- | The name of the Auto Scaling group.
tdResourceId :: Lens' TagDescription (Text)
tdResourceId f x =
    f (_tdResourceId x)
        <&> \y -> x { _tdResourceId = y }
{-# INLINE tdResourceId #-}

-- | The kind of resource to which the tag is applied. Currently, Auto Scaling
-- supports the auto-scaling-group resource type.
tdResourceType :: Lens' TagDescription (Text)
tdResourceType f x =
    f (_tdResourceType x)
        <&> \y -> x { _tdResourceType = y }
{-# INLINE tdResourceType #-}

-- | The key of the tag.
tdKey :: Lens' TagDescription (Text)
tdKey f x =
    f (_tdKey x)
        <&> \y -> x { _tdKey = y }
{-# INLINE tdKey #-}

-- | The value of the tag.
tdValue :: Lens' TagDescription (Text)
tdValue f x =
    f (_tdValue x)
        <&> \y -> x { _tdValue = y }
{-# INLINE tdValue #-}

-- | Specifies whether the new tag will be applied to instances launched after
-- the tag is created. The same behavior applies to updates: If you change a
-- tag, the changed tag will be applied to all instances launched after you
-- made the change.
tdPropagateAtLaunch :: Lens' TagDescription (Bool)
tdPropagateAtLaunch f x =
    f (_tdPropagateAtLaunch x)
        <&> \y -> x { _tdPropagateAtLaunch = y }
{-# INLINE tdPropagateAtLaunch #-}

instance FromXML TagDescription where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "TagDescription"

instance ToQuery TagDescription where
    toQuery = genericQuery def
