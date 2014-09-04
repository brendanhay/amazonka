{-# LANGUAGE DeriveDataTypeable          #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
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
    , AdjustmentType
    , auAdjustmentType

    -- * InstanceMonitoring
    , InstanceMonitoring
    , mkInstanceMonitoring
    , imEnabled

    -- * MetricCollectionType
    , MetricCollectionType
    , mcuMetric

    -- * MetricGranularityType
    , MetricGranularityType
    , mguGranularity

    -- * ProcessType
    , ProcessType
    , pwProcessName

    -- * Activity
    , Activity
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
    , Alarm
    , mkAlarm
    , amAlarmName
    , amAlarmARN

    -- * AutoScalingGroup
    , AutoScalingGroup
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
    , AutoScalingInstanceDetails
    , asidInstanceId
    , asidAutoScalingGroupName
    , asidAvailabilityZone
    , asidLifecycleState
    , asidHealthStatus
    , asidLaunchConfigurationName

    -- * BlockDeviceMapping
    , BlockDeviceMapping
    , mkBlockDeviceMapping
    , bdnVirtualName
    , bdnDeviceName
    , bdnEbs
    , bdnNoDevice

    -- * Ebs
    , Ebs
    , mkEbs
    , eSnapshotId
    , eVolumeSize
    , eVolumeType
    , eDeleteOnTermination
    , eIops

    -- * EnabledMetric
    , EnabledMetric
    , mkEnabledMetric
    , enMetric
    , enGranularity

    -- * Filter
    , Filter
    , mkFilter
    , frName
    , frValues

    -- * Instance
    , Instance
    , mkInstance
    , pInstanceId
    , pAvailabilityZone
    , pLifecycleState
    , pHealthStatus
    , pLaunchConfigurationName

    -- * LaunchConfiguration
    , LaunchConfiguration
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
    , LifecycleHook
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
    , NotificationConfiguration
    , nfAutoScalingGroupName
    , nfTopicARN
    , nfNotificationType

    -- * ScalingPolicy
    , ScalingPolicy
    , suAutoScalingGroupName
    , suPolicyName
    , suScalingAdjustment
    , suAdjustmentType
    , suCooldown
    , suPolicyARN
    , suAlarms
    , suMinAdjustmentStep

    -- * ScheduledUpdateGroupAction
    , ScheduledUpdateGroupAction
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
    , SuspendedProcess
    , mkSuspendedProcess
    , srProcessName
    , srSuspensionReason

    -- * Tag
    , Tag
    , mkTag
    , uResourceId
    , uResourceType
    , uKey
    , uValue
    , uPropagateAtLaunch

    -- * TagDescription
    , TagDescription
    , mkTagDescription
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
auAdjustmentType = lens _auAdjustmentType (\s a -> s { _auAdjustmentType = a })
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
imEnabled = lens _imEnabled (\s a -> s { _imEnabled = a })
{-# INLINE imEnabled #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'InstanceMonitoring' data type to populate a request.
mkInstanceMonitoring :: InstanceMonitoring
mkInstanceMonitoring = InstanceMonitoring
    { _imEnabled = Nothing
    }
{-# INLINE mkInstanceMonitoring #-}

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
mcuMetric = lens _mcuMetric (\s a -> s { _mcuMetric = a })
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
mguGranularity = lens _mguGranularity (\s a -> s { _mguGranularity = a })
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
pwProcessName = lens _pwProcessName (\s a -> s { _pwProcessName = a })
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
ayActivityId = lens _ayActivityId (\s a -> s { _ayActivityId = a })
{-# INLINE ayActivityId #-}

-- | The name of the Auto Scaling group.
ayAutoScalingGroupName :: Lens' Activity (Text)
ayAutoScalingGroupName = lens _ayAutoScalingGroupName (\s a -> s { _ayAutoScalingGroupName = a })
{-# INLINE ayAutoScalingGroupName #-}

-- | Contains a friendly, more verbose description of the scaling activity.
ayDescription :: Lens' Activity (Maybe Text)
ayDescription = lens _ayDescription (\s a -> s { _ayDescription = a })
{-# INLINE ayDescription #-}

-- | Contains the reason the activity was begun.
ayCause :: Lens' Activity (Text)
ayCause = lens _ayCause (\s a -> s { _ayCause = a })
{-# INLINE ayCause #-}

-- | Provides the start time of this activity.
ayStartTime :: Lens' Activity (ISO8601)
ayStartTime = lens _ayStartTime (\s a -> s { _ayStartTime = a })
{-# INLINE ayStartTime #-}

-- | Provides the end time of this activity.
ayEndTime :: Lens' Activity (Maybe ISO8601)
ayEndTime = lens _ayEndTime (\s a -> s { _ayEndTime = a })
{-# INLINE ayEndTime #-}

-- | Contains the current status of the activity.
ayStatusCode :: Lens' Activity (ScalingActivityStatusCode)
ayStatusCode = lens _ayStatusCode (\s a -> s { _ayStatusCode = a })
{-# INLINE ayStatusCode #-}

-- | Contains a friendly, more verbose description of the activity status.
ayStatusMessage :: Lens' Activity (Maybe Text)
ayStatusMessage = lens _ayStatusMessage (\s a -> s { _ayStatusMessage = a })
{-# INLINE ayStatusMessage #-}

-- | Specifies a value between 0 and 100 that indicates the progress of the
-- activity.
ayProgress :: Lens' Activity (Maybe Integer)
ayProgress = lens _ayProgress (\s a -> s { _ayProgress = a })
{-# INLINE ayProgress #-}

-- | Contains details of the scaling activity.
ayDetails :: Lens' Activity (Maybe Text)
ayDetails = lens _ayDetails (\s a -> s { _ayDetails = a })
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
amAlarmName = lens _amAlarmName (\s a -> s { _amAlarmName = a })
{-# INLINE amAlarmName #-}

-- | The Amazon Resource Name (ARN) of the alarm.
amAlarmARN :: Lens' Alarm (Maybe Text)
amAlarmARN = lens _amAlarmARN (\s a -> s { _amAlarmARN = a })
{-# INLINE amAlarmARN #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'Alarm' data type to populate a request.
mkAlarm :: Alarm
mkAlarm = Alarm
    { _amAlarmName = Nothing
    , _amAlarmARN = Nothing
    }
{-# INLINE mkAlarm #-}

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
ashAutoScalingGroupName = lens _ashAutoScalingGroupName (\s a -> s { _ashAutoScalingGroupName = a })
{-# INLINE ashAutoScalingGroupName #-}

-- | The Amazon Resource Name (ARN) of the Auto Scaling group.
ashAutoScalingGroupARN :: Lens' AutoScalingGroup (Maybe Text)
ashAutoScalingGroupARN = lens _ashAutoScalingGroupARN (\s a -> s { _ashAutoScalingGroupARN = a })
{-# INLINE ashAutoScalingGroupARN #-}

-- | Specifies the name of the associated LaunchConfiguration.
ashLaunchConfigurationName :: Lens' AutoScalingGroup (Text)
ashLaunchConfigurationName = lens _ashLaunchConfigurationName (\s a -> s { _ashLaunchConfigurationName = a })
{-# INLINE ashLaunchConfigurationName #-}

-- | Contains the minimum size of the Auto Scaling group.
ashMinSize :: Lens' AutoScalingGroup (Integer)
ashMinSize = lens _ashMinSize (\s a -> s { _ashMinSize = a })
{-# INLINE ashMinSize #-}

-- | Contains the maximum size of the Auto Scaling group.
ashMaxSize :: Lens' AutoScalingGroup (Integer)
ashMaxSize = lens _ashMaxSize (\s a -> s { _ashMaxSize = a })
{-# INLINE ashMaxSize #-}

-- | Specifies the desired capacity for the Auto Scaling group.
ashDesiredCapacity :: Lens' AutoScalingGroup (Integer)
ashDesiredCapacity = lens _ashDesiredCapacity (\s a -> s { _ashDesiredCapacity = a })
{-# INLINE ashDesiredCapacity #-}

-- | The number of seconds after a scaling activity completes before any further
-- scaling activities can start.
ashDefaultCooldown :: Lens' AutoScalingGroup (Integer)
ashDefaultCooldown = lens _ashDefaultCooldown (\s a -> s { _ashDefaultCooldown = a })
{-# INLINE ashDefaultCooldown #-}

-- | Contains a list of Availability Zones for the group.
ashAvailabilityZones :: Lens' AutoScalingGroup ([Text])
ashAvailabilityZones = lens _ashAvailabilityZones (\s a -> s { _ashAvailabilityZones = a })
{-# INLINE ashAvailabilityZones #-}

-- | A list of load balancers associated with this Auto Scaling group.
ashLoadBalancerNames :: Lens' AutoScalingGroup ([Text])
ashLoadBalancerNames = lens _ashLoadBalancerNames (\s a -> s { _ashLoadBalancerNames = a })
{-# INLINE ashLoadBalancerNames #-}

-- | The service of interest for the health status check, either "EC2" for
-- Amazon EC2 or "ELB" for Elastic Load Balancing.
ashHealthCheckType :: Lens' AutoScalingGroup (Text)
ashHealthCheckType = lens _ashHealthCheckType (\s a -> s { _ashHealthCheckType = a })
{-# INLINE ashHealthCheckType #-}

-- | The length of time that Auto Scaling waits before checking an instance's
-- health status. The grace period begins when an instance comes into service.
ashHealthCheckGracePeriod :: Lens' AutoScalingGroup (Maybe Integer)
ashHealthCheckGracePeriod = lens _ashHealthCheckGracePeriod (\s a -> s { _ashHealthCheckGracePeriod = a })
{-# INLINE ashHealthCheckGracePeriod #-}

-- | Provides a summary list of Amazon EC2 instances.
ashInstances :: Lens' AutoScalingGroup ([Instance])
ashInstances = lens _ashInstances (\s a -> s { _ashInstances = a })
{-# INLINE ashInstances #-}

-- | Specifies the date and time the Auto Scaling group was created.
ashCreatedTime :: Lens' AutoScalingGroup (ISO8601)
ashCreatedTime = lens _ashCreatedTime (\s a -> s { _ashCreatedTime = a })
{-# INLINE ashCreatedTime #-}

-- | Suspended processes associated with this Auto Scaling group.
ashSuspendedProcesses :: Lens' AutoScalingGroup ([SuspendedProcess])
ashSuspendedProcesses = lens _ashSuspendedProcesses (\s a -> s { _ashSuspendedProcesses = a })
{-# INLINE ashSuspendedProcesses #-}

-- | The name of the cluster placement group, if applicable. For more
-- information, go to Using Cluster Instances in the Amazon EC2 User Guide.
ashPlacementGroup :: Lens' AutoScalingGroup (Maybe Text)
ashPlacementGroup = lens _ashPlacementGroup (\s a -> s { _ashPlacementGroup = a })
{-# INLINE ashPlacementGroup #-}

-- | The subnet identifier for the Amazon VPC connection, if applicable. You can
-- specify several subnets in a comma-separated list. When you specify
-- VPCZoneIdentifier with AvailabilityZones, ensure that the subnets'
-- Availability Zones match the values you specify for AvailabilityZones.
ashVPCZoneIdentifier :: Lens' AutoScalingGroup (Maybe Text)
ashVPCZoneIdentifier = lens _ashVPCZoneIdentifier (\s a -> s { _ashVPCZoneIdentifier = a })
{-# INLINE ashVPCZoneIdentifier #-}

-- | A list of metrics enabled for this Auto Scaling group.
ashEnabledMetrics :: Lens' AutoScalingGroup ([EnabledMetric])
ashEnabledMetrics = lens _ashEnabledMetrics (\s a -> s { _ashEnabledMetrics = a })
{-# INLINE ashEnabledMetrics #-}

-- | The current state of the Auto Scaling group when a DeleteAutoScalingGroup
-- action is in progress.
ashStatus :: Lens' AutoScalingGroup (Maybe Text)
ashStatus = lens _ashStatus (\s a -> s { _ashStatus = a })
{-# INLINE ashStatus #-}

-- | A list of tags for the Auto Scaling group.
ashTags :: Lens' AutoScalingGroup ([TagDescription])
ashTags = lens _ashTags (\s a -> s { _ashTags = a })
{-# INLINE ashTags #-}

-- | A standalone termination policy or a list of termination policies for this
-- Auto Scaling group.
ashTerminationPolicies :: Lens' AutoScalingGroup ([Text])
ashTerminationPolicies = lens _ashTerminationPolicies (\s a -> s { _ashTerminationPolicies = a })
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
asidInstanceId = lens _asidInstanceId (\s a -> s { _asidInstanceId = a })
{-# INLINE asidInstanceId #-}

-- | The name of the Auto Scaling group associated with this instance.
asidAutoScalingGroupName :: Lens' AutoScalingInstanceDetails (Text)
asidAutoScalingGroupName = lens _asidAutoScalingGroupName (\s a -> s { _asidAutoScalingGroupName = a })
{-# INLINE asidAutoScalingGroupName #-}

-- | The Availability Zone in which this instance resides.
asidAvailabilityZone :: Lens' AutoScalingInstanceDetails (Text)
asidAvailabilityZone = lens _asidAvailabilityZone (\s a -> s { _asidAvailabilityZone = a })
{-# INLINE asidAvailabilityZone #-}

-- | The life cycle state of this instance. for more information, see Instance
-- Lifecycle State in the Auto Scaling Developer Guide.
asidLifecycleState :: Lens' AutoScalingInstanceDetails (Text)
asidLifecycleState = lens _asidLifecycleState (\s a -> s { _asidLifecycleState = a })
{-# INLINE asidLifecycleState #-}

-- | The health status of this instance. "Healthy" means that the instance is
-- healthy and should remain in service. "Unhealthy" means that the instance
-- is unhealthy. Auto Scaling should terminate and replace it.
asidHealthStatus :: Lens' AutoScalingInstanceDetails (Text)
asidHealthStatus = lens _asidHealthStatus (\s a -> s { _asidHealthStatus = a })
{-# INLINE asidHealthStatus #-}

-- | The launch configuration associated with this instance.
asidLaunchConfigurationName :: Lens' AutoScalingInstanceDetails (Text)
asidLaunchConfigurationName = lens _asidLaunchConfigurationName (\s a -> s { _asidLaunchConfigurationName = a })
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
bdnVirtualName = lens _bdnVirtualName (\s a -> s { _bdnVirtualName = a })
{-# INLINE bdnVirtualName #-}

-- | The name of the device within Amazon EC2 (for example, /dev/sdh or xvdh).
bdnDeviceName :: Lens' BlockDeviceMapping (Text)
bdnDeviceName = lens _bdnDeviceName (\s a -> s { _bdnDeviceName = a })
{-# INLINE bdnDeviceName #-}

-- | The Elastic Block Storage volume information.
bdnEbs :: Lens' BlockDeviceMapping (Maybe Ebs)
bdnEbs = lens _bdnEbs (\s a -> s { _bdnEbs = a })
{-# INLINE bdnEbs #-}

-- | Suppresses the device mapping. If NoDevice is set to true for the root
-- device, the instance might fail the EC2 health check. Auto Scaling launches
-- a replacement instance if the instance fails the health check.
bdnNoDevice :: Lens' BlockDeviceMapping (Maybe Bool)
bdnNoDevice = lens _bdnNoDevice (\s a -> s { _bdnNoDevice = a })
{-# INLINE bdnNoDevice #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'BlockDeviceMapping' data type to populate a request.
mkBlockDeviceMapping :: Text -- ^ 'bdnDeviceName'
                     -> BlockDeviceMapping
mkBlockDeviceMapping p1 = BlockDeviceMapping
    { _bdnVirtualName = Nothing
    , _bdnDeviceName = p2
    , _bdnEbs = Nothing
    , _bdnNoDevice = Nothing
    }
{-# INLINE mkBlockDeviceMapping #-}

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
eSnapshotId = lens _eSnapshotId (\s a -> s { _eSnapshotId = a })
{-# INLINE eSnapshotId #-}

-- | The volume size, in gigabytes. Valid values: If the volume type is io1, the
-- minimum size of the volume is 10. Default: If you're creating the volume
-- from a snapshot, and you don't specify a volume size, the default is the
-- snapshot size. Required: Required when the volume type is io1.
eVolumeSize :: Lens' Ebs (Maybe Integer)
eVolumeSize = lens _eVolumeSize (\s a -> s { _eVolumeSize = a })
{-# INLINE eVolumeSize #-}

-- | The volume type. Valid values: standard | io1 Default: standard.
eVolumeType :: Lens' Ebs (Maybe Text)
eVolumeType = lens _eVolumeType (\s a -> s { _eVolumeType = a })
{-# INLINE eVolumeType #-}

-- | Indicates whether to delete the volume on instance termination. Default:
-- true.
eDeleteOnTermination :: Lens' Ebs (Maybe Bool)
eDeleteOnTermination = lens _eDeleteOnTermination (\s a -> s { _eDeleteOnTermination = a })
{-# INLINE eDeleteOnTermination #-}

-- | The number of I/O operations per second (IOPS) that the volume supports.
-- The maximum ratio of IOPS to volume size is 30.0 Valid Values: Range is 100
-- to 4000. Default: None.
eIops :: Lens' Ebs (Maybe Integer)
eIops = lens _eIops (\s a -> s { _eIops = a })
{-# INLINE eIops #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'Ebs' data type to populate a request.
mkEbs :: Ebs
mkEbs = Ebs
    { _eSnapshotId = Nothing
    , _eVolumeSize = Nothing
    , _eVolumeType = Nothing
    , _eDeleteOnTermination = Nothing
    , _eIops = Nothing
    }
{-# INLINE mkEbs #-}

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
enMetric = lens _enMetric (\s a -> s { _enMetric = a })
{-# INLINE enMetric #-}

-- | The granularity of the enabled metric.
enGranularity :: Lens' EnabledMetric (Maybe Text)
enGranularity = lens _enGranularity (\s a -> s { _enGranularity = a })
{-# INLINE enGranularity #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'EnabledMetric' data type to populate a request.
mkEnabledMetric :: EnabledMetric
mkEnabledMetric = EnabledMetric
    { _enMetric = Nothing
    , _enGranularity = Nothing
    }
{-# INLINE mkEnabledMetric #-}

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
frName = lens _frName (\s a -> s { _frName = a })
{-# INLINE frName #-}

-- | The value of the filter.
frValues :: Lens' Filter ([Text])
frValues = lens _frValues (\s a -> s { _frValues = a })
{-# INLINE frValues #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'Filter' data type to populate a request.
mkFilter :: Text -- ^ 'frName'
         -> Filter
mkFilter p1 = Filter
    { _frName = p1
    , _frValues = mempty
    }
{-# INLINE mkFilter #-}

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
pInstanceId = lens _pInstanceId (\s a -> s { _pInstanceId = a })
{-# INLINE pInstanceId #-}

-- | Availability Zones associated with this instance.
pAvailabilityZone :: Lens' Instance (Text)
pAvailabilityZone = lens _pAvailabilityZone (\s a -> s { _pAvailabilityZone = a })
{-# INLINE pAvailabilityZone #-}

-- | Contains a description of the current lifecycle state. The Quarantined
-- lifecycle state is currently not used.
pLifecycleState :: Lens' Instance (LifecycleState)
pLifecycleState = lens _pLifecycleState (\s a -> s { _pLifecycleState = a })
{-# INLINE pLifecycleState #-}

-- | The instance's health status.
pHealthStatus :: Lens' Instance (Text)
pHealthStatus = lens _pHealthStatus (\s a -> s { _pHealthStatus = a })
{-# INLINE pHealthStatus #-}

-- | The launch configuration associated with this instance.
pLaunchConfigurationName :: Lens' Instance (Text)
pLaunchConfigurationName = lens _pLaunchConfigurationName (\s a -> s { _pLaunchConfigurationName = a })
{-# INLINE pLaunchConfigurationName #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'Instance' data type to populate a request.
mkInstance :: Text -- ^ 'pInstanceId'
           -> Text -- ^ 'pAvailabilityZone'
           -> LifecycleState -- ^ 'pLifecycleState'
           -> Text -- ^ 'pHealthStatus'
           -> Text -- ^ 'pLaunchConfigurationName'
           -> Instance
mkInstance p1 p2 p3 p4 p5 = Instance
    { _pInstanceId = p1
    , _pAvailabilityZone = p2
    , _pLifecycleState = p3
    , _pHealthStatus = p4
    , _pLaunchConfigurationName = p5
    }
{-# INLINE mkInstance #-}

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
ldLaunchConfigurationName = lens _ldLaunchConfigurationName (\s a -> s { _ldLaunchConfigurationName = a })
{-# INLINE ldLaunchConfigurationName #-}

-- | The launch configuration's Amazon Resource Name (ARN).
ldLaunchConfigurationARN :: Lens' LaunchConfiguration (Maybe Text)
ldLaunchConfigurationARN = lens _ldLaunchConfigurationARN (\s a -> s { _ldLaunchConfigurationARN = a })
{-# INLINE ldLaunchConfigurationARN #-}

-- | Provides the unique ID of the Amazon Machine Image (AMI) that was assigned
-- during registration.
ldImageId :: Lens' LaunchConfiguration (Text)
ldImageId = lens _ldImageId (\s a -> s { _ldImageId = a })
{-# INLINE ldImageId #-}

-- | Provides the name of the Amazon EC2 key pair.
ldKeyName :: Lens' LaunchConfiguration (Maybe Text)
ldKeyName = lens _ldKeyName (\s a -> s { _ldKeyName = a })
{-# INLINE ldKeyName #-}

-- | A description of the security groups to associate with the Amazon EC2
-- instances.
ldSecurityGroups :: Lens' LaunchConfiguration ([Text])
ldSecurityGroups = lens _ldSecurityGroups (\s a -> s { _ldSecurityGroups = a })
{-# INLINE ldSecurityGroups #-}

-- | The user data available to the launched Amazon EC2 instances.
ldUserData :: Lens' LaunchConfiguration (Maybe Text)
ldUserData = lens _ldUserData (\s a -> s { _ldUserData = a })
{-# INLINE ldUserData #-}

-- | Specifies the instance type of the Amazon EC2 instance.
ldInstanceType :: Lens' LaunchConfiguration (Text)
ldInstanceType = lens _ldInstanceType (\s a -> s { _ldInstanceType = a })
{-# INLINE ldInstanceType #-}

-- | Provides the ID of the kernel associated with the Amazon EC2 AMI.
ldKernelId :: Lens' LaunchConfiguration (Maybe Text)
ldKernelId = lens _ldKernelId (\s a -> s { _ldKernelId = a })
{-# INLINE ldKernelId #-}

-- | Provides ID of the RAM disk associated with the Amazon EC2 AMI.
ldRamdiskId :: Lens' LaunchConfiguration (Maybe Text)
ldRamdiskId = lens _ldRamdiskId (\s a -> s { _ldRamdiskId = a })
{-# INLINE ldRamdiskId #-}

-- | Specifies how block devices are exposed to the instance. Each mapping is
-- made up of a virtualName and a deviceName.
ldBlockDeviceMappings :: Lens' LaunchConfiguration ([BlockDeviceMapping])
ldBlockDeviceMappings = lens _ldBlockDeviceMappings (\s a -> s { _ldBlockDeviceMappings = a })
{-# INLINE ldBlockDeviceMappings #-}

-- | Controls whether instances in this group are launched with detailed
-- monitoring or not.
ldInstanceMonitoring :: Lens' LaunchConfiguration (Maybe InstanceMonitoring)
ldInstanceMonitoring = lens _ldInstanceMonitoring (\s a -> s { _ldInstanceMonitoring = a })
{-# INLINE ldInstanceMonitoring #-}

-- | Specifies the price to bid when launching Spot Instances.
ldSpotPrice :: Lens' LaunchConfiguration (Maybe Text)
ldSpotPrice = lens _ldSpotPrice (\s a -> s { _ldSpotPrice = a })
{-# INLINE ldSpotPrice #-}

-- | Provides the name or the Amazon Resource Name (ARN) of the instance profile
-- associated with the IAM role for the instance. The instance profile
-- contains the IAM role.
ldIamInstanceProfile :: Lens' LaunchConfiguration (Maybe Text)
ldIamInstanceProfile = lens _ldIamInstanceProfile (\s a -> s { _ldIamInstanceProfile = a })
{-# INLINE ldIamInstanceProfile #-}

-- | Provides the creation date and time for this launch configuration.
ldCreatedTime :: Lens' LaunchConfiguration (ISO8601)
ldCreatedTime = lens _ldCreatedTime (\s a -> s { _ldCreatedTime = a })
{-# INLINE ldCreatedTime #-}

-- | Specifies whether the instance is optimized for EBS I/O (true) or not
-- (false).
ldEbsOptimized :: Lens' LaunchConfiguration (Maybe Bool)
ldEbsOptimized = lens _ldEbsOptimized (\s a -> s { _ldEbsOptimized = a })
{-# INLINE ldEbsOptimized #-}

-- | Specifies whether the instance is associated with a public IP address
-- (true) or not (false).
ldAssociatePublicIpAddress :: Lens' LaunchConfiguration (Maybe Bool)
ldAssociatePublicIpAddress = lens _ldAssociatePublicIpAddress (\s a -> s { _ldAssociatePublicIpAddress = a })
{-# INLINE ldAssociatePublicIpAddress #-}

-- | Specifies the tenancy of the instance. It can be either default or
-- dedicated. An instance with dedicated tenancy runs in an isolated,
-- single-tenant hardware and it can only be launched in a VPC.
ldPlacementTenancy :: Lens' LaunchConfiguration (Maybe Text)
ldPlacementTenancy = lens _ldPlacementTenancy (\s a -> s { _ldPlacementTenancy = a })
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
liLifecycleHookName = lens _liLifecycleHookName (\s a -> s { _liLifecycleHookName = a })
{-# INLINE liLifecycleHookName #-}

-- | The name of the Auto Scaling group to which the lifecycle action belongs.
liAutoScalingGroupName :: Lens' LifecycleHook (Maybe Text)
liAutoScalingGroupName = lens _liAutoScalingGroupName (\s a -> s { _liAutoScalingGroupName = a })
{-# INLINE liAutoScalingGroupName #-}

-- | The Amazon EC2 instance state to which you want to attach the lifecycle
-- hook. See DescribeLifecycleHooks for a list of available lifecycle hook
-- types.
liLifecycleTransition :: Lens' LifecycleHook (Maybe Text)
liLifecycleTransition = lens _liLifecycleTransition (\s a -> s { _liLifecycleTransition = a })
{-# INLINE liLifecycleTransition #-}

-- | The ARN of the notification target that Auto Scaling will use to notify you
-- when an instance is in the transition state for the lifecycle hook. This
-- ARN target can be either an SQS queue or an SNS topic. The notification
-- message sent to the target will include: Lifecycle action token User
-- account ID Name of the Auto Scaling group Lifecycle hook name EC2 instance
-- ID Lifecycle transition Notification metadata.
liNotificationTargetARN :: Lens' LifecycleHook (Maybe Text)
liNotificationTargetARN = lens _liNotificationTargetARN (\s a -> s { _liNotificationTargetARN = a })
{-# INLINE liNotificationTargetARN #-}

-- | The ARN of the Amazon IAM role that allows the Auto Scaling group to
-- publish to the specified notification target.
liRoleARN :: Lens' LifecycleHook (Maybe Text)
liRoleARN = lens _liRoleARN (\s a -> s { _liRoleARN = a })
{-# INLINE liRoleARN #-}

-- | Contains additional information that you want to include any time Auto
-- Scaling sends a message to the notification target.
liNotificationMetadata :: Lens' LifecycleHook (Maybe Text)
liNotificationMetadata = lens _liNotificationMetadata (\s a -> s { _liNotificationMetadata = a })
{-# INLINE liNotificationMetadata #-}

-- | Defines the amount of time that can elapse before the lifecycle hook times
-- out. When the lifecycle hook times out, Auto Scaling performs the action
-- defined in the DefaultResult parameter. You can prevent the lifecycle hook
-- from timing out by calling RecordLifecycleActionHeartbeat.
liHeartbeatTimeout :: Lens' LifecycleHook (Maybe Integer)
liHeartbeatTimeout = lens _liHeartbeatTimeout (\s a -> s { _liHeartbeatTimeout = a })
{-# INLINE liHeartbeatTimeout #-}

-- | The maximum length of time an instance can remain in a Pending:Wait or
-- Terminating:Wait state. Currently, this value is set at 48 hours.
liGlobalTimeout :: Lens' LifecycleHook (Maybe Integer)
liGlobalTimeout = lens _liGlobalTimeout (\s a -> s { _liGlobalTimeout = a })
{-# INLINE liGlobalTimeout #-}

-- | Defines the action the Auto Scaling group should take when the lifecycle
-- hook timeout elapses or if an unexpected failure occurs. The value for this
-- parameter can be either CONTINUE or ABANDON. The default value for this
-- parameter is CONTINUE.
liDefaultResult :: Lens' LifecycleHook (Maybe Text)
liDefaultResult = lens _liDefaultResult (\s a -> s { _liDefaultResult = a })
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
nfAutoScalingGroupName = lens _nfAutoScalingGroupName (\s a -> s { _nfAutoScalingGroupName = a })
{-# INLINE nfAutoScalingGroupName #-}

-- | The Amazon Resource Name (ARN) of the Amazon Simple Notification Service
-- (SNS) topic.
nfTopicARN :: Lens' NotificationConfiguration (Maybe Text)
nfTopicARN = lens _nfTopicARN (\s a -> s { _nfTopicARN = a })
{-# INLINE nfTopicARN #-}

-- | The types of events for an action to start.
nfNotificationType :: Lens' NotificationConfiguration (Maybe Text)
nfNotificationType = lens _nfNotificationType (\s a -> s { _nfNotificationType = a })
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
suAutoScalingGroupName = lens _suAutoScalingGroupName (\s a -> s { _suAutoScalingGroupName = a })
{-# INLINE suAutoScalingGroupName #-}

-- | The name of the scaling policy.
suPolicyName :: Lens' ScalingPolicy (Maybe Text)
suPolicyName = lens _suPolicyName (\s a -> s { _suPolicyName = a })
{-# INLINE suPolicyName #-}

-- | The number associated with the specified adjustment type. A positive value
-- adds to the current capacity and a negative value removes from the current
-- capacity.
suScalingAdjustment :: Lens' ScalingPolicy (Maybe Integer)
suScalingAdjustment = lens _suScalingAdjustment (\s a -> s { _suScalingAdjustment = a })
{-# INLINE suScalingAdjustment #-}

-- | Specifies whether the ScalingAdjustment is an absolute number or a
-- percentage of the current capacity. Valid values are ChangeInCapacity,
-- ExactCapacity, and PercentChangeInCapacity.
suAdjustmentType :: Lens' ScalingPolicy (Maybe Text)
suAdjustmentType = lens _suAdjustmentType (\s a -> s { _suAdjustmentType = a })
{-# INLINE suAdjustmentType #-}

-- | The amount of time, in seconds, after a scaling activity completes before
-- any further trigger-related scaling activities can start.
suCooldown :: Lens' ScalingPolicy (Maybe Integer)
suCooldown = lens _suCooldown (\s a -> s { _suCooldown = a })
{-# INLINE suCooldown #-}

-- | The Amazon Resource Name (ARN) of the policy.
suPolicyARN :: Lens' ScalingPolicy (Maybe Text)
suPolicyARN = lens _suPolicyARN (\s a -> s { _suPolicyARN = a })
{-# INLINE suPolicyARN #-}

-- | A list of CloudWatch Alarms related to the policy.
suAlarms :: Lens' ScalingPolicy ([Alarm])
suAlarms = lens _suAlarms (\s a -> s { _suAlarms = a })
{-# INLINE suAlarms #-}

-- | Changes the DesiredCapacity of the Auto Scaling group by at least the
-- specified number of instances.
suMinAdjustmentStep :: Lens' ScalingPolicy (Maybe Integer)
suMinAdjustmentStep = lens _suMinAdjustmentStep (\s a -> s { _suMinAdjustmentStep = a })
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
sugbAutoScalingGroupName = lens _sugbAutoScalingGroupName (\s a -> s { _sugbAutoScalingGroupName = a })
{-# INLINE sugbAutoScalingGroupName #-}

-- | The name of this scheduled action.
sugbScheduledActionName :: Lens' ScheduledUpdateGroupAction (Maybe Text)
sugbScheduledActionName = lens _sugbScheduledActionName (\s a -> s { _sugbScheduledActionName = a })
{-# INLINE sugbScheduledActionName #-}

-- | The Amazon Resource Name (ARN) of this scheduled action.
sugbScheduledActionARN :: Lens' ScheduledUpdateGroupAction (Maybe Text)
sugbScheduledActionARN = lens _sugbScheduledActionARN (\s a -> s { _sugbScheduledActionARN = a })
{-# INLINE sugbScheduledActionARN #-}

-- | Time is deprecated. The time that the action is scheduled to begin. Time is
-- an alias for StartTime.
sugbTime :: Lens' ScheduledUpdateGroupAction (Maybe ISO8601)
sugbTime = lens _sugbTime (\s a -> s { _sugbTime = a })
{-# INLINE sugbTime #-}

-- | The time that the action is scheduled to begin. This value can be up to one
-- month in the future. When StartTime and EndTime are specified with
-- Recurrence, they form the boundaries of when the recurring action will
-- start and stop.
sugbStartTime :: Lens' ScheduledUpdateGroupAction (Maybe ISO8601)
sugbStartTime = lens _sugbStartTime (\s a -> s { _sugbStartTime = a })
{-# INLINE sugbStartTime #-}

-- | The time that the action is scheduled to end. This value can be up to one
-- month in the future.
sugbEndTime :: Lens' ScheduledUpdateGroupAction (Maybe ISO8601)
sugbEndTime = lens _sugbEndTime (\s a -> s { _sugbEndTime = a })
{-# INLINE sugbEndTime #-}

-- | The regular schedule that an action occurs.
sugbRecurrence :: Lens' ScheduledUpdateGroupAction (Maybe Text)
sugbRecurrence = lens _sugbRecurrence (\s a -> s { _sugbRecurrence = a })
{-# INLINE sugbRecurrence #-}

-- | The minimum size of the Auto Scaling group.
sugbMinSize :: Lens' ScheduledUpdateGroupAction (Maybe Integer)
sugbMinSize = lens _sugbMinSize (\s a -> s { _sugbMinSize = a })
{-# INLINE sugbMinSize #-}

-- | The maximum size of the Auto Scaling group.
sugbMaxSize :: Lens' ScheduledUpdateGroupAction (Maybe Integer)
sugbMaxSize = lens _sugbMaxSize (\s a -> s { _sugbMaxSize = a })
{-# INLINE sugbMaxSize #-}

-- | The number of instances you prefer to maintain in your Auto Scaling group.
sugbDesiredCapacity :: Lens' ScheduledUpdateGroupAction (Maybe Integer)
sugbDesiredCapacity = lens _sugbDesiredCapacity (\s a -> s { _sugbDesiredCapacity = a })
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
srProcessName = lens _srProcessName (\s a -> s { _srProcessName = a })
{-# INLINE srProcessName #-}

-- | The reason that the process was suspended.
srSuspensionReason :: Lens' SuspendedProcess (Maybe Text)
srSuspensionReason = lens _srSuspensionReason (\s a -> s { _srSuspensionReason = a })
{-# INLINE srSuspensionReason #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'SuspendedProcess' data type to populate a request.
mkSuspendedProcess :: SuspendedProcess
mkSuspendedProcess = SuspendedProcess
    { _srProcessName = Nothing
    , _srSuspensionReason = Nothing
    }
{-# INLINE mkSuspendedProcess #-}

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
uResourceId = lens _uResourceId (\s a -> s { _uResourceId = a })
{-# INLINE uResourceId #-}

-- | The kind of resource to which the tag is applied. Currently, Auto Scaling
-- supports the auto-scaling-group resource type.
uResourceType :: Lens' Tag (Text)
uResourceType = lens _uResourceType (\s a -> s { _uResourceType = a })
{-# INLINE uResourceType #-}

-- | The key of the tag.
uKey :: Lens' Tag (Text)
uKey = lens _uKey (\s a -> s { _uKey = a })
{-# INLINE uKey #-}

-- | The value of the tag.
uValue :: Lens' Tag (Text)
uValue = lens _uValue (\s a -> s { _uValue = a })
{-# INLINE uValue #-}

-- | Specifies whether the new tag will be applied to instances launched after
-- the tag is created. The same behavior applies to updates: If you change a
-- tag, the changed tag will be applied to all instances launched after you
-- made the change.
uPropagateAtLaunch :: Lens' Tag (Bool)
uPropagateAtLaunch = lens _uPropagateAtLaunch (\s a -> s { _uPropagateAtLaunch = a })
{-# INLINE uPropagateAtLaunch #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'Tag' data type to populate a request.
mkTag :: Text -- ^ 'uResourceId'
      -> Text -- ^ 'uResourceType'
      -> Text -- ^ 'uKey'
      -> Text -- ^ 'uValue'
      -> Bool -- ^ 'uPropagateAtLaunch'
      -> Tag
mkTag p1 p2 p3 p4 p5 = Tag
    { _uResourceId = p1
    , _uResourceType = p2
    , _uKey = p3
    , _uValue = p4
    , _uPropagateAtLaunch = p5
    }
{-# INLINE mkTag #-}

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
tdResourceId = lens _tdResourceId (\s a -> s { _tdResourceId = a })
{-# INLINE tdResourceId #-}

-- | The kind of resource to which the tag is applied. Currently, Auto Scaling
-- supports the auto-scaling-group resource type.
tdResourceType :: Lens' TagDescription (Text)
tdResourceType = lens _tdResourceType (\s a -> s { _tdResourceType = a })
{-# INLINE tdResourceType #-}

-- | The key of the tag.
tdKey :: Lens' TagDescription (Text)
tdKey = lens _tdKey (\s a -> s { _tdKey = a })
{-# INLINE tdKey #-}

-- | The value of the tag.
tdValue :: Lens' TagDescription (Text)
tdValue = lens _tdValue (\s a -> s { _tdValue = a })
{-# INLINE tdValue #-}

-- | Specifies whether the new tag will be applied to instances launched after
-- the tag is created. The same behavior applies to updates: If you change a
-- tag, the changed tag will be applied to all instances launched after you
-- made the change.
tdPropagateAtLaunch :: Lens' TagDescription (Bool)
tdPropagateAtLaunch = lens _tdPropagateAtLaunch (\s a -> s { _tdPropagateAtLaunch = a })
{-# INLINE tdPropagateAtLaunch #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'TagDescription' data type to populate a request.
mkTagDescription :: Text -- ^ 'tdResourceId'
                 -> Text -- ^ 'tdResourceType'
                 -> Text -- ^ 'tdKey'
                 -> Text -- ^ 'tdValue'
                 -> Bool -- ^ 'tdPropagateAtLaunch'
                 -> TagDescription
mkTagDescription p1 p2 p3 p4 p5 = TagDescription
    { _tdResourceId = p1
    , _tdResourceType = p2
    , _tdKey = p3
    , _tdValue = p4
    , _tdPropagateAtLaunch = p5
    }
{-# INLINE mkTagDescription #-}

instance FromXML TagDescription where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "TagDescription"

instance ToQuery TagDescription where
    toQuery = genericQuery def
