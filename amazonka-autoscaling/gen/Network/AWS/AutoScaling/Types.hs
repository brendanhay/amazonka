{-# LANGUAGE DeriveDataTypeable          #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE StandaloneDeriving          #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.AutoScaling.Types
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
module Network.AWS.AutoScaling.Types
    (
    -- * Service
      AutoScaling
    -- ** XML
    , xmlOptions

    -- * LifecycleState
    , LifecycleState (..)

    -- * ScalingActivityStatusCode
    , ScalingActivityStatusCode (..)

    -- * AdjustmentType
    , AdjustmentType
    , mkAdjustmentType
    , atAdjustmentType

    -- * InstanceMonitoring
    , InstanceMonitoring
    , mkInstanceMonitoring
    , imEnabled

    -- * MetricCollectionType
    , MetricCollectionType
    , mkMetricCollectionType
    , mctMetric

    -- * MetricGranularityType
    , MetricGranularityType
    , mkMetricGranularityType
    , mgtGranularity

    -- * ProcessType
    , ProcessType
    , mkProcessType
    , ptProcessName

    -- * Activity
    , Activity
    , mkActivity
    , arActivityId
    , arAutoScalingGroupName
    , arDescription
    , arCause
    , arStartTime
    , arEndTime
    , arStatusCode
    , arStatusMessage
    , arProgress
    , arDetails

    -- * Alarm
    , Alarm
    , mkAlarm
    , aAlarmName
    , aAlarmARN

    -- * AutoScalingGroup
    , AutoScalingGroup
    , mkAutoScalingGroup
    , asgAutoScalingGroupName
    , asgAutoScalingGroupARN
    , asgLaunchConfigurationName
    , asgMinSize
    , asgMaxSize
    , asgDesiredCapacity
    , asgDefaultCooldown
    , asgAvailabilityZones
    , asgLoadBalancerNames
    , asgHealthCheckType
    , asgHealthCheckGracePeriod
    , asgInstances
    , asgCreatedTime
    , asgSuspendedProcesses
    , asgPlacementGroup
    , asgVPCZoneIdentifier
    , asgEnabledMetrics
    , asgStatus
    , asgTags
    , asgTerminationPolicies

    -- * AutoScalingInstanceDetails
    , AutoScalingInstanceDetails
    , mkAutoScalingInstanceDetails
    , asidInstanceId
    , asidAutoScalingGroupName
    , asidAvailabilityZone
    , asidLifecycleState
    , asidHealthStatus
    , asidLaunchConfigurationName

    -- * BlockDeviceMapping
    , BlockDeviceMapping
    , mkBlockDeviceMapping
    , bdmVirtualName
    , bdmDeviceName
    , bdmEbs
    , bdmNoDevice

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
    , emMetric
    , emGranularity

    -- * Filter
    , Filter
    , mkFilter
    , fName
    , fValues

    -- * Instance
    , Instance
    , mkInstance
    , iInstanceId
    , iAvailabilityZone
    , iLifecycleState
    , iHealthStatus
    , iLaunchConfigurationName

    -- * LaunchConfiguration
    , LaunchConfiguration
    , mkLaunchConfiguration
    , lcLaunchConfigurationName
    , lcLaunchConfigurationARN
    , lcImageId
    , lcKeyName
    , lcSecurityGroups
    , lcUserData
    , lcInstanceType
    , lcKernelId
    , lcRamdiskId
    , lcBlockDeviceMappings
    , lcInstanceMonitoring
    , lcSpotPrice
    , lcIamInstanceProfile
    , lcCreatedTime
    , lcEbsOptimized
    , lcAssociatePublicIpAddress
    , lcPlacementTenancy

    -- * LifecycleHook
    , LifecycleHook
    , mkLifecycleHook
    , lhLifecycleHookName
    , lhAutoScalingGroupName
    , lhLifecycleTransition
    , lhNotificationTargetARN
    , lhRoleARN
    , lhNotificationMetadata
    , lhHeartbeatTimeout
    , lhGlobalTimeout
    , lhDefaultResult

    -- * NotificationConfiguration
    , NotificationConfiguration
    , mkNotificationConfiguration
    , ncAutoScalingGroupName
    , ncTopicARN
    , ncNotificationType

    -- * ScalingPolicy
    , ScalingPolicy
    , mkScalingPolicy
    , sprAutoScalingGroupName
    , sprPolicyName
    , sprScalingAdjustment
    , sprAdjustmentType
    , sprCooldown
    , sprPolicyARN
    , sprAlarms
    , sprMinAdjustmentStep

    -- * ScheduledUpdateGroupAction
    , ScheduledUpdateGroupAction
    , mkScheduledUpdateGroupAction
    , sugaAutoScalingGroupName
    , sugaScheduledActionName
    , sugaScheduledActionARN
    , sugaTime
    , sugaStartTime
    , sugaEndTime
    , sugaRecurrence
    , sugaMinSize
    , sugaMaxSize
    , sugaDesiredCapacity

    -- * SuspendedProcess
    , SuspendedProcess
    , mkSuspendedProcess
    , spProcessName
    , spSuspensionReason

    -- * Tag
    , Tag
    , mkTag
    , tResourceId
    , tResourceType
    , tKey
    , tValue
    , tPropagateAtLaunch

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
    { _atAdjustmentType :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'AdjustmentType' data type.
--
-- 'AdjustmentType' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @AdjustmentType ::@ @Maybe Text@
--
mkAdjustmentType :: AdjustmentType
mkAdjustmentType = AdjustmentType
    { _atAdjustmentType = Nothing
    }

-- | A policy adjustment type. Valid values are ChangeInCapacity, ExactCapacity,
-- and PercentChangeInCapacity.
atAdjustmentType :: Lens' AdjustmentType (Maybe Text)
atAdjustmentType =
    lens _atAdjustmentType (\s a -> s { _atAdjustmentType = a })

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
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'InstanceMonitoring' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Enabled ::@ @Maybe Bool@
--
mkInstanceMonitoring :: InstanceMonitoring
mkInstanceMonitoring = InstanceMonitoring
    { _imEnabled = Nothing
    }

-- | If True, instance monitoring is enabled.
imEnabled :: Lens' InstanceMonitoring (Maybe Bool)
imEnabled = lens _imEnabled (\s a -> s { _imEnabled = a })

instance FromXML InstanceMonitoring where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "InstanceMonitoring"

instance ToQuery InstanceMonitoring where
    toQuery = genericQuery def

-- | The MetricCollectionType data type.
newtype MetricCollectionType = MetricCollectionType
    { _mctMetric :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'MetricCollectionType' data type.
--
-- 'MetricCollectionType' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Metric ::@ @Maybe Text@
--
mkMetricCollectionType :: MetricCollectionType
mkMetricCollectionType = MetricCollectionType
    { _mctMetric = Nothing
    }

-- | 
mctMetric :: Lens' MetricCollectionType (Maybe Text)
mctMetric = lens _mctMetric (\s a -> s { _mctMetric = a })

instance FromXML MetricCollectionType where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "MetricCollectionType"

-- | The MetricGranularityType data type.
newtype MetricGranularityType = MetricGranularityType
    { _mgtGranularity :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'MetricGranularityType' data type.
--
-- 'MetricGranularityType' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Granularity ::@ @Maybe Text@
--
mkMetricGranularityType :: MetricGranularityType
mkMetricGranularityType = MetricGranularityType
    { _mgtGranularity = Nothing
    }

-- | The granularity of a Metric.
mgtGranularity :: Lens' MetricGranularityType (Maybe Text)
mgtGranularity = lens _mgtGranularity (\s a -> s { _mgtGranularity = a })

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
    { _ptProcessName :: Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'ProcessType' data type.
--
-- 'ProcessType' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @ProcessName ::@ @Text@
--
mkProcessType :: Text -- ^ 'ptProcessName'
              -> ProcessType
mkProcessType p1 = ProcessType
    { _ptProcessName = p1
    }

-- | The name of a process.
ptProcessName :: Lens' ProcessType Text
ptProcessName = lens _ptProcessName (\s a -> s { _ptProcessName = a })

instance FromXML ProcessType where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ProcessType"

-- | A scaling Activity is a long-running process that represents a change to
-- your AutoScalingGroup, such as changing the size of the group. It can also
-- be a process to replace an instance, or a process to perform any other
-- long-running operations supported by the API.
data Activity = Activity
    { _arActivityId :: Text
    , _arAutoScalingGroupName :: Text
    , _arDescription :: Maybe Text
    , _arCause :: Text
    , _arStartTime :: ISO8601
    , _arEndTime :: Maybe ISO8601
    , _arStatusCode :: ScalingActivityStatusCode
    , _arStatusMessage :: Maybe Text
    , _arProgress :: Maybe Integer
    , _arDetails :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'Activity' data type.
--
-- 'Activity' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @ActivityId ::@ @Text@
--
-- * @AutoScalingGroupName ::@ @Text@
--
-- * @Description ::@ @Maybe Text@
--
-- * @Cause ::@ @Text@
--
-- * @StartTime ::@ @ISO8601@
--
-- * @EndTime ::@ @Maybe ISO8601@
--
-- * @StatusCode ::@ @ScalingActivityStatusCode@
--
-- * @StatusMessage ::@ @Maybe Text@
--
-- * @Progress ::@ @Maybe Integer@
--
-- * @Details ::@ @Maybe Text@
--
mkActivity :: Text -- ^ 'arActivityId'
           -> Text -- ^ 'arAutoScalingGroupName'
           -> Text -- ^ 'arCause'
           -> ISO8601 -- ^ 'arStartTime'
           -> ScalingActivityStatusCode -- ^ 'arStatusCode'
           -> Activity
mkActivity p1 p2 p4 p5 p7 = Activity
    { _arActivityId = p1
    , _arAutoScalingGroupName = p2
    , _arDescription = Nothing
    , _arCause = p4
    , _arStartTime = p5
    , _arEndTime = Nothing
    , _arStatusCode = p7
    , _arStatusMessage = Nothing
    , _arProgress = Nothing
    , _arDetails = Nothing
    }

-- | Specifies the ID of the activity.
arActivityId :: Lens' Activity Text
arActivityId = lens _arActivityId (\s a -> s { _arActivityId = a })

-- | The name of the Auto Scaling group.
arAutoScalingGroupName :: Lens' Activity Text
arAutoScalingGroupName =
    lens _arAutoScalingGroupName (\s a -> s { _arAutoScalingGroupName = a })

-- | Contains a friendly, more verbose description of the scaling activity.
arDescription :: Lens' Activity (Maybe Text)
arDescription = lens _arDescription (\s a -> s { _arDescription = a })

-- | Contains the reason the activity was begun.
arCause :: Lens' Activity Text
arCause = lens _arCause (\s a -> s { _arCause = a })

-- | Provides the start time of this activity.
arStartTime :: Lens' Activity ISO8601
arStartTime = lens _arStartTime (\s a -> s { _arStartTime = a })

-- | Provides the end time of this activity.
arEndTime :: Lens' Activity (Maybe ISO8601)
arEndTime = lens _arEndTime (\s a -> s { _arEndTime = a })

-- | Contains the current status of the activity.
arStatusCode :: Lens' Activity ScalingActivityStatusCode
arStatusCode = lens _arStatusCode (\s a -> s { _arStatusCode = a })

-- | Contains a friendly, more verbose description of the activity status.
arStatusMessage :: Lens' Activity (Maybe Text)
arStatusMessage = lens _arStatusMessage (\s a -> s { _arStatusMessage = a })

-- | Specifies a value between 0 and 100 that indicates the progress of the
-- activity.
arProgress :: Lens' Activity (Maybe Integer)
arProgress = lens _arProgress (\s a -> s { _arProgress = a })

-- | Contains details of the scaling activity.
arDetails :: Lens' Activity (Maybe Text)
arDetails = lens _arDetails (\s a -> s { _arDetails = a })

instance FromXML Activity where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Activity"

-- | The Alarm data type.
data Alarm = Alarm
    { _aAlarmName :: Maybe Text
    , _aAlarmARN :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'Alarm' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @AlarmName ::@ @Maybe Text@
--
-- * @AlarmARN ::@ @Maybe Text@
--
mkAlarm :: Alarm
mkAlarm = Alarm
    { _aAlarmName = Nothing
    , _aAlarmARN = Nothing
    }

-- | The name of the alarm.
aAlarmName :: Lens' Alarm (Maybe Text)
aAlarmName = lens _aAlarmName (\s a -> s { _aAlarmName = a })

-- | The Amazon Resource Name (ARN) of the alarm.
aAlarmARN :: Lens' Alarm (Maybe Text)
aAlarmARN = lens _aAlarmARN (\s a -> s { _aAlarmARN = a })

instance FromXML Alarm where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Alarm"

instance ToQuery Alarm where
    toQuery = genericQuery def

-- | The AutoScalingGroup data type.
data AutoScalingGroup = AutoScalingGroup
    { _asgAutoScalingGroupName :: Text
    , _asgAutoScalingGroupARN :: Maybe Text
    , _asgLaunchConfigurationName :: Text
    , _asgMinSize :: !Integer
    , _asgMaxSize :: !Integer
    , _asgDesiredCapacity :: !Integer
    , _asgDefaultCooldown :: !Integer
    , _asgAvailabilityZones :: List1 Text
    , _asgLoadBalancerNames :: [Text]
    , _asgHealthCheckType :: Text
    , _asgHealthCheckGracePeriod :: Maybe Integer
    , _asgInstances :: [Instance]
    , _asgCreatedTime :: ISO8601
    , _asgSuspendedProcesses :: [SuspendedProcess]
    , _asgPlacementGroup :: Maybe Text
    , _asgVPCZoneIdentifier :: Maybe Text
    , _asgEnabledMetrics :: [EnabledMetric]
    , _asgStatus :: Maybe Text
    , _asgTags :: [TagDescription]
    , _asgTerminationPolicies :: [Text]
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'AutoScalingGroup' data type.
--
-- 'AutoScalingGroup' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @AutoScalingGroupName ::@ @Text@
--
-- * @AutoScalingGroupARN ::@ @Maybe Text@
--
-- * @LaunchConfigurationName ::@ @Text@
--
-- * @MinSize ::@ @Integer@
--
-- * @MaxSize ::@ @Integer@
--
-- * @DesiredCapacity ::@ @Integer@
--
-- * @DefaultCooldown ::@ @Integer@
--
-- * @AvailabilityZones ::@ @List1 Text@
--
-- * @LoadBalancerNames ::@ @[Text]@
--
-- * @HealthCheckType ::@ @Text@
--
-- * @HealthCheckGracePeriod ::@ @Maybe Integer@
--
-- * @Instances ::@ @[Instance]@
--
-- * @CreatedTime ::@ @ISO8601@
--
-- * @SuspendedProcesses ::@ @[SuspendedProcess]@
--
-- * @PlacementGroup ::@ @Maybe Text@
--
-- * @VPCZoneIdentifier ::@ @Maybe Text@
--
-- * @EnabledMetrics ::@ @[EnabledMetric]@
--
-- * @Status ::@ @Maybe Text@
--
-- * @Tags ::@ @[TagDescription]@
--
-- * @TerminationPolicies ::@ @[Text]@
--
mkAutoScalingGroup :: Text -- ^ 'asgAutoScalingGroupName'
                   -> Text -- ^ 'asgHealthCheckType'
                   -> ISO8601 -- ^ 'asgCreatedTime'
                   -> Text -- ^ 'asgLaunchConfigurationName'
                   -> Integer -- ^ 'asgMinSize'
                   -> Integer -- ^ 'asgMaxSize'
                   -> Integer -- ^ 'asgDesiredCapacity'
                   -> Integer -- ^ 'asgDefaultCooldown'
                   -> List1 Text -- ^ 'asgAvailabilityZones'
                   -> AutoScalingGroup
mkAutoScalingGroup p1 p10 p13 p3 p4 p5 p6 p7 p8 = AutoScalingGroup
    { _asgAutoScalingGroupName = p1
    , _asgAutoScalingGroupARN = Nothing
    , _asgLaunchConfigurationName = p3
    , _asgMinSize = p4
    , _asgMaxSize = p5
    , _asgDesiredCapacity = p6
    , _asgDefaultCooldown = p7
    , _asgAvailabilityZones = p8
    , _asgLoadBalancerNames = mempty
    , _asgHealthCheckType = p10
    , _asgHealthCheckGracePeriod = Nothing
    , _asgInstances = mempty
    , _asgCreatedTime = p13
    , _asgSuspendedProcesses = mempty
    , _asgPlacementGroup = Nothing
    , _asgVPCZoneIdentifier = Nothing
    , _asgEnabledMetrics = mempty
    , _asgStatus = Nothing
    , _asgTags = mempty
    , _asgTerminationPolicies = mempty
    }

-- | Specifies the name of the group.
asgAutoScalingGroupName :: Lens' AutoScalingGroup Text
asgAutoScalingGroupName =
    lens _asgAutoScalingGroupName
         (\s a -> s { _asgAutoScalingGroupName = a })

-- | The Amazon Resource Name (ARN) of the Auto Scaling group.
asgAutoScalingGroupARN :: Lens' AutoScalingGroup (Maybe Text)
asgAutoScalingGroupARN =
    lens _asgAutoScalingGroupARN (\s a -> s { _asgAutoScalingGroupARN = a })

-- | Specifies the name of the associated LaunchConfiguration.
asgLaunchConfigurationName :: Lens' AutoScalingGroup Text
asgLaunchConfigurationName =
    lens _asgLaunchConfigurationName
         (\s a -> s { _asgLaunchConfigurationName = a })

-- | Contains the minimum size of the Auto Scaling group.
asgMinSize :: Lens' AutoScalingGroup Integer
asgMinSize = lens _asgMinSize (\s a -> s { _asgMinSize = a })

-- | Contains the maximum size of the Auto Scaling group.
asgMaxSize :: Lens' AutoScalingGroup Integer
asgMaxSize = lens _asgMaxSize (\s a -> s { _asgMaxSize = a })

-- | Specifies the desired capacity for the Auto Scaling group.
asgDesiredCapacity :: Lens' AutoScalingGroup Integer
asgDesiredCapacity =
    lens _asgDesiredCapacity (\s a -> s { _asgDesiredCapacity = a })

-- | The number of seconds after a scaling activity completes before any further
-- scaling activities can start.
asgDefaultCooldown :: Lens' AutoScalingGroup Integer
asgDefaultCooldown =
    lens _asgDefaultCooldown (\s a -> s { _asgDefaultCooldown = a })

-- | Contains a list of Availability Zones for the group.
asgAvailabilityZones :: Lens' AutoScalingGroup (List1 Text)
asgAvailabilityZones =
    lens _asgAvailabilityZones (\s a -> s { _asgAvailabilityZones = a })

-- | A list of load balancers associated with this Auto Scaling group.
asgLoadBalancerNames :: Lens' AutoScalingGroup [Text]
asgLoadBalancerNames =
    lens _asgLoadBalancerNames (\s a -> s { _asgLoadBalancerNames = a })

-- | The service of interest for the health status check, either "EC2" for
-- Amazon EC2 or "ELB" for Elastic Load Balancing.
asgHealthCheckType :: Lens' AutoScalingGroup Text
asgHealthCheckType =
    lens _asgHealthCheckType (\s a -> s { _asgHealthCheckType = a })

-- | The length of time that Auto Scaling waits before checking an instance's
-- health status. The grace period begins when an instance comes into service.
asgHealthCheckGracePeriod :: Lens' AutoScalingGroup (Maybe Integer)
asgHealthCheckGracePeriod =
    lens _asgHealthCheckGracePeriod
         (\s a -> s { _asgHealthCheckGracePeriod = a })

-- | Provides a summary list of Amazon EC2 instances.
asgInstances :: Lens' AutoScalingGroup [Instance]
asgInstances = lens _asgInstances (\s a -> s { _asgInstances = a })

-- | Specifies the date and time the Auto Scaling group was created.
asgCreatedTime :: Lens' AutoScalingGroup ISO8601
asgCreatedTime = lens _asgCreatedTime (\s a -> s { _asgCreatedTime = a })

-- | Suspended processes associated with this Auto Scaling group.
asgSuspendedProcesses :: Lens' AutoScalingGroup [SuspendedProcess]
asgSuspendedProcesses =
    lens _asgSuspendedProcesses (\s a -> s { _asgSuspendedProcesses = a })

-- | The name of the cluster placement group, if applicable. For more
-- information, go to Using Cluster Instances in the Amazon EC2 User Guide.
asgPlacementGroup :: Lens' AutoScalingGroup (Maybe Text)
asgPlacementGroup =
    lens _asgPlacementGroup (\s a -> s { _asgPlacementGroup = a })

-- | The subnet identifier for the Amazon VPC connection, if applicable. You can
-- specify several subnets in a comma-separated list. When you specify
-- VPCZoneIdentifier with AvailabilityZones, ensure that the subnets'
-- Availability Zones match the values you specify for AvailabilityZones.
asgVPCZoneIdentifier :: Lens' AutoScalingGroup (Maybe Text)
asgVPCZoneIdentifier =
    lens _asgVPCZoneIdentifier (\s a -> s { _asgVPCZoneIdentifier = a })

-- | A list of metrics enabled for this Auto Scaling group.
asgEnabledMetrics :: Lens' AutoScalingGroup [EnabledMetric]
asgEnabledMetrics =
    lens _asgEnabledMetrics (\s a -> s { _asgEnabledMetrics = a })

-- | The current state of the Auto Scaling group when a DeleteAutoScalingGroup
-- action is in progress.
asgStatus :: Lens' AutoScalingGroup (Maybe Text)
asgStatus = lens _asgStatus (\s a -> s { _asgStatus = a })

-- | A list of tags for the Auto Scaling group.
asgTags :: Lens' AutoScalingGroup [TagDescription]
asgTags = lens _asgTags (\s a -> s { _asgTags = a })

-- | A standalone termination policy or a list of termination policies for this
-- Auto Scaling group.
asgTerminationPolicies :: Lens' AutoScalingGroup [Text]
asgTerminationPolicies =
    lens _asgTerminationPolicies (\s a -> s { _asgTerminationPolicies = a })

instance FromXML AutoScalingGroup where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "AutoScalingGroup"

-- | The AutoScalingInstanceDetails data type.
data AutoScalingInstanceDetails = AutoScalingInstanceDetails
    { _asidInstanceId :: Text
    , _asidAutoScalingGroupName :: Text
    , _asidAvailabilityZone :: Text
    , _asidLifecycleState :: Text
    , _asidHealthStatus :: Text
    , _asidLaunchConfigurationName :: Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'AutoScalingInstanceDetails' data type.
--
-- 'AutoScalingInstanceDetails' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @InstanceId ::@ @Text@
--
-- * @AutoScalingGroupName ::@ @Text@
--
-- * @AvailabilityZone ::@ @Text@
--
-- * @LifecycleState ::@ @Text@
--
-- * @HealthStatus ::@ @Text@
--
-- * @LaunchConfigurationName ::@ @Text@
--
mkAutoScalingInstanceDetails :: Text -- ^ 'asidInstanceId'
                             -> Text -- ^ 'asidAutoScalingGroupName'
                             -> Text -- ^ 'asidAvailabilityZone'
                             -> Text -- ^ 'asidLifecycleState'
                             -> Text -- ^ 'asidHealthStatus'
                             -> Text -- ^ 'asidLaunchConfigurationName'
                             -> AutoScalingInstanceDetails
mkAutoScalingInstanceDetails p1 p2 p3 p4 p5 p6 = AutoScalingInstanceDetails
    { _asidInstanceId = p1
    , _asidAutoScalingGroupName = p2
    , _asidAvailabilityZone = p3
    , _asidLifecycleState = p4
    , _asidHealthStatus = p5
    , _asidLaunchConfigurationName = p6
    }

-- | The instance ID of the Amazon EC2 instance.
asidInstanceId :: Lens' AutoScalingInstanceDetails Text
asidInstanceId = lens _asidInstanceId (\s a -> s { _asidInstanceId = a })

-- | The name of the Auto Scaling group associated with this instance.
asidAutoScalingGroupName :: Lens' AutoScalingInstanceDetails Text
asidAutoScalingGroupName =
    lens _asidAutoScalingGroupName
         (\s a -> s { _asidAutoScalingGroupName = a })

-- | The Availability Zone in which this instance resides.
asidAvailabilityZone :: Lens' AutoScalingInstanceDetails Text
asidAvailabilityZone =
    lens _asidAvailabilityZone (\s a -> s { _asidAvailabilityZone = a })

-- | The life cycle state of this instance. for more information, see Instance
-- Lifecycle State in the Auto Scaling Developer Guide.
asidLifecycleState :: Lens' AutoScalingInstanceDetails Text
asidLifecycleState =
    lens _asidLifecycleState (\s a -> s { _asidLifecycleState = a })

-- | The health status of this instance. "Healthy" means that the instance is
-- healthy and should remain in service. "Unhealthy" means that the instance
-- is unhealthy. Auto Scaling should terminate and replace it.
asidHealthStatus :: Lens' AutoScalingInstanceDetails Text
asidHealthStatus =
    lens _asidHealthStatus (\s a -> s { _asidHealthStatus = a })

-- | The launch configuration associated with this instance.
asidLaunchConfigurationName :: Lens' AutoScalingInstanceDetails Text
asidLaunchConfigurationName =
    lens _asidLaunchConfigurationName
         (\s a -> s { _asidLaunchConfigurationName = a })

instance FromXML AutoScalingInstanceDetails where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "AutoScalingInstanceDetails"

-- | The BlockDeviceMapping data type.
data BlockDeviceMapping = BlockDeviceMapping
    { _bdmVirtualName :: Maybe Text
    , _bdmDeviceName :: Text
    , _bdmEbs :: Maybe Ebs
    , _bdmNoDevice :: Maybe Bool
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'BlockDeviceMapping' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @VirtualName ::@ @Maybe Text@
--
-- * @DeviceName ::@ @Text@
--
-- * @Ebs ::@ @Maybe Ebs@
--
-- * @NoDevice ::@ @Maybe Bool@
--
mkBlockDeviceMapping :: Text -- ^ 'bdmDeviceName'
                     -> BlockDeviceMapping
mkBlockDeviceMapping p2 = BlockDeviceMapping
    { _bdmVirtualName = Nothing
    , _bdmDeviceName = p2
    , _bdmEbs = Nothing
    , _bdmNoDevice = Nothing
    }

-- | The virtual name associated with the device.
bdmVirtualName :: Lens' BlockDeviceMapping (Maybe Text)
bdmVirtualName = lens _bdmVirtualName (\s a -> s { _bdmVirtualName = a })

-- | The name of the device within Amazon EC2 (for example, /dev/sdh or xvdh).
bdmDeviceName :: Lens' BlockDeviceMapping Text
bdmDeviceName = lens _bdmDeviceName (\s a -> s { _bdmDeviceName = a })

-- | The Elastic Block Storage volume information.
bdmEbs :: Lens' BlockDeviceMapping (Maybe Ebs)
bdmEbs = lens _bdmEbs (\s a -> s { _bdmEbs = a })

-- | Suppresses the device mapping. If NoDevice is set to true for the root
-- device, the instance might fail the EC2 health check. Auto Scaling launches
-- a replacement instance if the instance fails the health check.
bdmNoDevice :: Lens' BlockDeviceMapping (Maybe Bool)
bdmNoDevice = lens _bdmNoDevice (\s a -> s { _bdmNoDevice = a })

instance FromXML BlockDeviceMapping where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "BlockDeviceMapping"

instance ToQuery BlockDeviceMapping where
    toQuery = genericQuery def

-- | The Elastic Block Storage volume information.
data Ebs = Ebs
    { _eSnapshotId :: Maybe Text
    , _eVolumeSize :: Maybe Integer
    , _eVolumeType :: Maybe Text
    , _eDeleteOnTermination :: Maybe Bool
    , _eIops :: Maybe Integer
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'Ebs' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @SnapshotId ::@ @Maybe Text@
--
-- * @VolumeSize ::@ @Maybe Integer@
--
-- * @VolumeType ::@ @Maybe Text@
--
-- * @DeleteOnTermination ::@ @Maybe Bool@
--
-- * @Iops ::@ @Maybe Integer@
--
mkEbs :: Ebs
mkEbs = Ebs
    { _eSnapshotId = Nothing
    , _eVolumeSize = Nothing
    , _eVolumeType = Nothing
    , _eDeleteOnTermination = Nothing
    , _eIops = Nothing
    }

-- | The snapshot ID.
eSnapshotId :: Lens' Ebs (Maybe Text)
eSnapshotId = lens _eSnapshotId (\s a -> s { _eSnapshotId = a })

-- | The volume size, in gigabytes. Valid values: If the volume type is io1, the
-- minimum size of the volume is 10. Default: If you're creating the volume
-- from a snapshot, and you don't specify a volume size, the default is the
-- snapshot size. Required: Required when the volume type is io1.
eVolumeSize :: Lens' Ebs (Maybe Integer)
eVolumeSize = lens _eVolumeSize (\s a -> s { _eVolumeSize = a })

-- | The volume type. Valid values: standard | io1 Default: standard.
eVolumeType :: Lens' Ebs (Maybe Text)
eVolumeType = lens _eVolumeType (\s a -> s { _eVolumeType = a })

-- | Indicates whether to delete the volume on instance termination. Default:
-- true.
eDeleteOnTermination :: Lens' Ebs (Maybe Bool)
eDeleteOnTermination =
    lens _eDeleteOnTermination (\s a -> s { _eDeleteOnTermination = a })

-- | The number of I/O operations per second (IOPS) that the volume supports.
-- The maximum ratio of IOPS to volume size is 30.0 Valid Values: Range is 100
-- to 4000. Default: None.
eIops :: Lens' Ebs (Maybe Integer)
eIops = lens _eIops (\s a -> s { _eIops = a })

instance FromXML Ebs where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Ebs"

instance ToQuery Ebs where
    toQuery = genericQuery def

-- | The EnabledMetric data type.
data EnabledMetric = EnabledMetric
    { _emMetric :: Maybe Text
    , _emGranularity :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'EnabledMetric' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Metric ::@ @Maybe Text@
--
-- * @Granularity ::@ @Maybe Text@
--
mkEnabledMetric :: EnabledMetric
mkEnabledMetric = EnabledMetric
    { _emMetric = Nothing
    , _emGranularity = Nothing
    }

-- | The name of the enabled metric.
emMetric :: Lens' EnabledMetric (Maybe Text)
emMetric = lens _emMetric (\s a -> s { _emMetric = a })

-- | The granularity of the enabled metric.
emGranularity :: Lens' EnabledMetric (Maybe Text)
emGranularity = lens _emGranularity (\s a -> s { _emGranularity = a })

instance FromXML EnabledMetric where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "EnabledMetric"

instance ToQuery EnabledMetric where
    toQuery = genericQuery def

-- | The Filter data type.
data Filter = Filter
    { _fName :: Text
    , _fValues :: [Text]
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'Filter' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Name ::@ @Text@
--
-- * @Values ::@ @[Text]@
--
mkFilter :: Text -- ^ 'fName'
         -> Filter
mkFilter p1 = Filter
    { _fName = p1
    , _fValues = mempty
    }

-- | The name of the filter. Valid Name values are: "auto-scaling-group", "key",
-- "value", and "propagate-at-launch".
fName :: Lens' Filter Text
fName = lens _fName (\s a -> s { _fName = a })

-- | The value of the filter.
fValues :: Lens' Filter [Text]
fValues = lens _fValues (\s a -> s { _fValues = a })

instance ToQuery Filter where
    toQuery = genericQuery def

-- | The Instance data type.
data Instance = Instance
    { _iInstanceId :: Text
    , _iAvailabilityZone :: Text
    , _iLifecycleState :: LifecycleState
    , _iHealthStatus :: Text
    , _iLaunchConfigurationName :: Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'Instance' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @InstanceId ::@ @Text@
--
-- * @AvailabilityZone ::@ @Text@
--
-- * @LifecycleState ::@ @LifecycleState@
--
-- * @HealthStatus ::@ @Text@
--
-- * @LaunchConfigurationName ::@ @Text@
--
mkInstance :: Text -- ^ 'iInstanceId'
           -> Text -- ^ 'iAvailabilityZone'
           -> LifecycleState -- ^ 'iLifecycleState'
           -> Text -- ^ 'iHealthStatus'
           -> Text -- ^ 'iLaunchConfigurationName'
           -> Instance
mkInstance p1 p2 p3 p4 p5 = Instance
    { _iInstanceId = p1
    , _iAvailabilityZone = p2
    , _iLifecycleState = p3
    , _iHealthStatus = p4
    , _iLaunchConfigurationName = p5
    }

-- | Specifies the ID of the Amazon EC2 instance.
iInstanceId :: Lens' Instance Text
iInstanceId = lens _iInstanceId (\s a -> s { _iInstanceId = a })

-- | Availability Zones associated with this instance.
iAvailabilityZone :: Lens' Instance Text
iAvailabilityZone =
    lens _iAvailabilityZone (\s a -> s { _iAvailabilityZone = a })

-- | Contains a description of the current lifecycle state. The Quarantined
-- lifecycle state is currently not used.
iLifecycleState :: Lens' Instance LifecycleState
iLifecycleState = lens _iLifecycleState (\s a -> s { _iLifecycleState = a })

-- | The instance's health status.
iHealthStatus :: Lens' Instance Text
iHealthStatus = lens _iHealthStatus (\s a -> s { _iHealthStatus = a })

-- | The launch configuration associated with this instance.
iLaunchConfigurationName :: Lens' Instance Text
iLaunchConfigurationName =
    lens _iLaunchConfigurationName
         (\s a -> s { _iLaunchConfigurationName = a })

instance FromXML Instance where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Instance"

instance ToQuery Instance where
    toQuery = genericQuery def

-- | The LaunchConfiguration data type.
data LaunchConfiguration = LaunchConfiguration
    { _lcLaunchConfigurationName :: Text
    , _lcLaunchConfigurationARN :: Maybe Text
    , _lcImageId :: Text
    , _lcKeyName :: Maybe Text
    , _lcSecurityGroups :: [Text]
    , _lcUserData :: Maybe Text
    , _lcInstanceType :: Text
    , _lcKernelId :: Maybe Text
    , _lcRamdiskId :: Maybe Text
    , _lcBlockDeviceMappings :: [BlockDeviceMapping]
    , _lcInstanceMonitoring :: Maybe InstanceMonitoring
    , _lcSpotPrice :: Maybe Text
    , _lcIamInstanceProfile :: Maybe Text
    , _lcCreatedTime :: ISO8601
    , _lcEbsOptimized :: Maybe Bool
    , _lcAssociatePublicIpAddress :: Maybe Bool
    , _lcPlacementTenancy :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'LaunchConfiguration' data type.
--
-- 'LaunchConfiguration' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @LaunchConfigurationName ::@ @Text@
--
-- * @LaunchConfigurationARN ::@ @Maybe Text@
--
-- * @ImageId ::@ @Text@
--
-- * @KeyName ::@ @Maybe Text@
--
-- * @SecurityGroups ::@ @[Text]@
--
-- * @UserData ::@ @Maybe Text@
--
-- * @InstanceType ::@ @Text@
--
-- * @KernelId ::@ @Maybe Text@
--
-- * @RamdiskId ::@ @Maybe Text@
--
-- * @BlockDeviceMappings ::@ @[BlockDeviceMapping]@
--
-- * @InstanceMonitoring ::@ @Maybe InstanceMonitoring@
--
-- * @SpotPrice ::@ @Maybe Text@
--
-- * @IamInstanceProfile ::@ @Maybe Text@
--
-- * @CreatedTime ::@ @ISO8601@
--
-- * @EbsOptimized ::@ @Maybe Bool@
--
-- * @AssociatePublicIpAddress ::@ @Maybe Bool@
--
-- * @PlacementTenancy ::@ @Maybe Text@
--
mkLaunchConfiguration :: Text -- ^ 'lcLaunchConfigurationName'
                      -> ISO8601 -- ^ 'lcCreatedTime'
                      -> Text -- ^ 'lcImageId'
                      -> Text -- ^ 'lcInstanceType'
                      -> LaunchConfiguration
mkLaunchConfiguration p1 p14 p3 p7 = LaunchConfiguration
    { _lcLaunchConfigurationName = p1
    , _lcLaunchConfigurationARN = Nothing
    , _lcImageId = p3
    , _lcKeyName = Nothing
    , _lcSecurityGroups = mempty
    , _lcUserData = Nothing
    , _lcInstanceType = p7
    , _lcKernelId = Nothing
    , _lcRamdiskId = Nothing
    , _lcBlockDeviceMappings = mempty
    , _lcInstanceMonitoring = Nothing
    , _lcSpotPrice = Nothing
    , _lcIamInstanceProfile = Nothing
    , _lcCreatedTime = p14
    , _lcEbsOptimized = Nothing
    , _lcAssociatePublicIpAddress = Nothing
    , _lcPlacementTenancy = Nothing
    }

-- | Specifies the name of the launch configuration.
lcLaunchConfigurationName :: Lens' LaunchConfiguration Text
lcLaunchConfigurationName =
    lens _lcLaunchConfigurationName
         (\s a -> s { _lcLaunchConfigurationName = a })

-- | The launch configuration's Amazon Resource Name (ARN).
lcLaunchConfigurationARN :: Lens' LaunchConfiguration (Maybe Text)
lcLaunchConfigurationARN =
    lens _lcLaunchConfigurationARN
         (\s a -> s { _lcLaunchConfigurationARN = a })

-- | Provides the unique ID of the Amazon Machine Image (AMI) that was assigned
-- during registration.
lcImageId :: Lens' LaunchConfiguration Text
lcImageId = lens _lcImageId (\s a -> s { _lcImageId = a })

-- | Provides the name of the Amazon EC2 key pair.
lcKeyName :: Lens' LaunchConfiguration (Maybe Text)
lcKeyName = lens _lcKeyName (\s a -> s { _lcKeyName = a })

-- | A description of the security groups to associate with the Amazon EC2
-- instances.
lcSecurityGroups :: Lens' LaunchConfiguration [Text]
lcSecurityGroups =
    lens _lcSecurityGroups (\s a -> s { _lcSecurityGroups = a })

-- | The user data available to the launched Amazon EC2 instances.
lcUserData :: Lens' LaunchConfiguration (Maybe Text)
lcUserData = lens _lcUserData (\s a -> s { _lcUserData = a })

-- | Specifies the instance type of the Amazon EC2 instance.
lcInstanceType :: Lens' LaunchConfiguration Text
lcInstanceType = lens _lcInstanceType (\s a -> s { _lcInstanceType = a })

-- | Provides the ID of the kernel associated with the Amazon EC2 AMI.
lcKernelId :: Lens' LaunchConfiguration (Maybe Text)
lcKernelId = lens _lcKernelId (\s a -> s { _lcKernelId = a })

-- | Provides ID of the RAM disk associated with the Amazon EC2 AMI.
lcRamdiskId :: Lens' LaunchConfiguration (Maybe Text)
lcRamdiskId = lens _lcRamdiskId (\s a -> s { _lcRamdiskId = a })

-- | Specifies how block devices are exposed to the instance. Each mapping is
-- made up of a virtualName and a deviceName.
lcBlockDeviceMappings :: Lens' LaunchConfiguration [BlockDeviceMapping]
lcBlockDeviceMappings =
    lens _lcBlockDeviceMappings (\s a -> s { _lcBlockDeviceMappings = a })

-- | Controls whether instances in this group are launched with detailed
-- monitoring or not.
lcInstanceMonitoring :: Lens' LaunchConfiguration (Maybe InstanceMonitoring)
lcInstanceMonitoring =
    lens _lcInstanceMonitoring (\s a -> s { _lcInstanceMonitoring = a })

-- | Specifies the price to bid when launching Spot Instances.
lcSpotPrice :: Lens' LaunchConfiguration (Maybe Text)
lcSpotPrice = lens _lcSpotPrice (\s a -> s { _lcSpotPrice = a })

-- | Provides the name or the Amazon Resource Name (ARN) of the instance profile
-- associated with the IAM role for the instance. The instance profile
-- contains the IAM role.
lcIamInstanceProfile :: Lens' LaunchConfiguration (Maybe Text)
lcIamInstanceProfile =
    lens _lcIamInstanceProfile (\s a -> s { _lcIamInstanceProfile = a })

-- | Provides the creation date and time for this launch configuration.
lcCreatedTime :: Lens' LaunchConfiguration ISO8601
lcCreatedTime = lens _lcCreatedTime (\s a -> s { _lcCreatedTime = a })

-- | Specifies whether the instance is optimized for EBS I/O (true) or not
-- (false).
lcEbsOptimized :: Lens' LaunchConfiguration (Maybe Bool)
lcEbsOptimized = lens _lcEbsOptimized (\s a -> s { _lcEbsOptimized = a })

-- | Specifies whether the instance is associated with a public IP address
-- (true) or not (false).
lcAssociatePublicIpAddress :: Lens' LaunchConfiguration (Maybe Bool)
lcAssociatePublicIpAddress =
    lens _lcAssociatePublicIpAddress
         (\s a -> s { _lcAssociatePublicIpAddress = a })

-- | Specifies the tenancy of the instance. It can be either default or
-- dedicated. An instance with dedicated tenancy runs in an isolated,
-- single-tenant hardware and it can only be launched in a VPC.
lcPlacementTenancy :: Lens' LaunchConfiguration (Maybe Text)
lcPlacementTenancy =
    lens _lcPlacementTenancy (\s a -> s { _lcPlacementTenancy = a })

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
    { _lhLifecycleHookName :: Maybe Text
    , _lhAutoScalingGroupName :: Maybe Text
    , _lhLifecycleTransition :: Maybe Text
    , _lhNotificationTargetARN :: Maybe Text
    , _lhRoleARN :: Maybe Text
    , _lhNotificationMetadata :: Maybe Text
    , _lhHeartbeatTimeout :: Maybe Integer
    , _lhGlobalTimeout :: Maybe Integer
    , _lhDefaultResult :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'LifecycleHook' data type.
--
-- 'LifecycleHook' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @LifecycleHookName ::@ @Maybe Text@
--
-- * @AutoScalingGroupName ::@ @Maybe Text@
--
-- * @LifecycleTransition ::@ @Maybe Text@
--
-- * @NotificationTargetARN ::@ @Maybe Text@
--
-- * @RoleARN ::@ @Maybe Text@
--
-- * @NotificationMetadata ::@ @Maybe Text@
--
-- * @HeartbeatTimeout ::@ @Maybe Integer@
--
-- * @GlobalTimeout ::@ @Maybe Integer@
--
-- * @DefaultResult ::@ @Maybe Text@
--
mkLifecycleHook :: LifecycleHook
mkLifecycleHook = LifecycleHook
    { _lhLifecycleHookName = Nothing
    , _lhAutoScalingGroupName = Nothing
    , _lhLifecycleTransition = Nothing
    , _lhNotificationTargetARN = Nothing
    , _lhRoleARN = Nothing
    , _lhNotificationMetadata = Nothing
    , _lhHeartbeatTimeout = Nothing
    , _lhGlobalTimeout = Nothing
    , _lhDefaultResult = Nothing
    }

-- | The name of the lifecycle action hook.
lhLifecycleHookName :: Lens' LifecycleHook (Maybe Text)
lhLifecycleHookName =
    lens _lhLifecycleHookName (\s a -> s { _lhLifecycleHookName = a })

-- | The name of the Auto Scaling group to which the lifecycle action belongs.
lhAutoScalingGroupName :: Lens' LifecycleHook (Maybe Text)
lhAutoScalingGroupName =
    lens _lhAutoScalingGroupName (\s a -> s { _lhAutoScalingGroupName = a })

-- | The Amazon EC2 instance state to which you want to attach the lifecycle
-- hook. See DescribeLifecycleHooks for a list of available lifecycle hook
-- types.
lhLifecycleTransition :: Lens' LifecycleHook (Maybe Text)
lhLifecycleTransition =
    lens _lhLifecycleTransition (\s a -> s { _lhLifecycleTransition = a })

-- | The ARN of the notification target that Auto Scaling will use to notify you
-- when an instance is in the transition state for the lifecycle hook. This
-- ARN target can be either an SQS queue or an SNS topic. The notification
-- message sent to the target will include: Lifecycle action token User
-- account ID Name of the Auto Scaling group Lifecycle hook name EC2 instance
-- ID Lifecycle transition Notification metadata.
lhNotificationTargetARN :: Lens' LifecycleHook (Maybe Text)
lhNotificationTargetARN =
    lens _lhNotificationTargetARN
         (\s a -> s { _lhNotificationTargetARN = a })

-- | The ARN of the Amazon IAM role that allows the Auto Scaling group to
-- publish to the specified notification target.
lhRoleARN :: Lens' LifecycleHook (Maybe Text)
lhRoleARN = lens _lhRoleARN (\s a -> s { _lhRoleARN = a })

-- | Contains additional information that you want to include any time Auto
-- Scaling sends a message to the notification target.
lhNotificationMetadata :: Lens' LifecycleHook (Maybe Text)
lhNotificationMetadata =
    lens _lhNotificationMetadata (\s a -> s { _lhNotificationMetadata = a })

-- | Defines the amount of time that can elapse before the lifecycle hook times
-- out. When the lifecycle hook times out, Auto Scaling performs the action
-- defined in the DefaultResult parameter. You can prevent the lifecycle hook
-- from timing out by calling RecordLifecycleActionHeartbeat.
lhHeartbeatTimeout :: Lens' LifecycleHook (Maybe Integer)
lhHeartbeatTimeout =
    lens _lhHeartbeatTimeout (\s a -> s { _lhHeartbeatTimeout = a })

-- | The maximum length of time an instance can remain in a Pending:Wait or
-- Terminating:Wait state. Currently, this value is set at 48 hours.
lhGlobalTimeout :: Lens' LifecycleHook (Maybe Integer)
lhGlobalTimeout = lens _lhGlobalTimeout (\s a -> s { _lhGlobalTimeout = a })

-- | Defines the action the Auto Scaling group should take when the lifecycle
-- hook timeout elapses or if an unexpected failure occurs. The value for this
-- parameter can be either CONTINUE or ABANDON. The default value for this
-- parameter is CONTINUE.
lhDefaultResult :: Lens' LifecycleHook (Maybe Text)
lhDefaultResult = lens _lhDefaultResult (\s a -> s { _lhDefaultResult = a })

instance FromXML LifecycleHook where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "LifecycleHook"

-- | The NotificationConfiguration data type.
data NotificationConfiguration = NotificationConfiguration
    { _ncAutoScalingGroupName :: Maybe Text
    , _ncTopicARN :: Maybe Text
    , _ncNotificationType :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'NotificationConfiguration' data type.
--
-- 'NotificationConfiguration' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @AutoScalingGroupName ::@ @Maybe Text@
--
-- * @TopicARN ::@ @Maybe Text@
--
-- * @NotificationType ::@ @Maybe Text@
--
mkNotificationConfiguration :: NotificationConfiguration
mkNotificationConfiguration = NotificationConfiguration
    { _ncAutoScalingGroupName = Nothing
    , _ncTopicARN = Nothing
    , _ncNotificationType = Nothing
    }

-- | Specifies the Auto Scaling group name.
ncAutoScalingGroupName :: Lens' NotificationConfiguration (Maybe Text)
ncAutoScalingGroupName =
    lens _ncAutoScalingGroupName (\s a -> s { _ncAutoScalingGroupName = a })

-- | The Amazon Resource Name (ARN) of the Amazon Simple Notification Service
-- (SNS) topic.
ncTopicARN :: Lens' NotificationConfiguration (Maybe Text)
ncTopicARN = lens _ncTopicARN (\s a -> s { _ncTopicARN = a })

-- | The types of events for an action to start.
ncNotificationType :: Lens' NotificationConfiguration (Maybe Text)
ncNotificationType =
    lens _ncNotificationType (\s a -> s { _ncNotificationType = a })

instance FromXML NotificationConfiguration where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "NotificationConfiguration"

-- | The ScalingPolicy data type.
data ScalingPolicy = ScalingPolicy
    { _sprAutoScalingGroupName :: Maybe Text
    , _sprPolicyName :: Maybe Text
    , _sprScalingAdjustment :: Maybe Integer
    , _sprAdjustmentType :: Maybe Text
    , _sprCooldown :: Maybe Integer
    , _sprPolicyARN :: Maybe Text
    , _sprAlarms :: [Alarm]
    , _sprMinAdjustmentStep :: Maybe Integer
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'ScalingPolicy' data type.
--
-- 'ScalingPolicy' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @AutoScalingGroupName ::@ @Maybe Text@
--
-- * @PolicyName ::@ @Maybe Text@
--
-- * @ScalingAdjustment ::@ @Maybe Integer@
--
-- * @AdjustmentType ::@ @Maybe Text@
--
-- * @Cooldown ::@ @Maybe Integer@
--
-- * @PolicyARN ::@ @Maybe Text@
--
-- * @Alarms ::@ @[Alarm]@
--
-- * @MinAdjustmentStep ::@ @Maybe Integer@
--
mkScalingPolicy :: ScalingPolicy
mkScalingPolicy = ScalingPolicy
    { _sprAutoScalingGroupName = Nothing
    , _sprPolicyName = Nothing
    , _sprScalingAdjustment = Nothing
    , _sprAdjustmentType = Nothing
    , _sprCooldown = Nothing
    , _sprPolicyARN = Nothing
    , _sprAlarms = mempty
    , _sprMinAdjustmentStep = Nothing
    }

-- | The name of the Auto Scaling group associated with this scaling policy.
sprAutoScalingGroupName :: Lens' ScalingPolicy (Maybe Text)
sprAutoScalingGroupName =
    lens _sprAutoScalingGroupName
         (\s a -> s { _sprAutoScalingGroupName = a })

-- | The name of the scaling policy.
sprPolicyName :: Lens' ScalingPolicy (Maybe Text)
sprPolicyName = lens _sprPolicyName (\s a -> s { _sprPolicyName = a })

-- | The number associated with the specified adjustment type. A positive value
-- adds to the current capacity and a negative value removes from the current
-- capacity.
sprScalingAdjustment :: Lens' ScalingPolicy (Maybe Integer)
sprScalingAdjustment =
    lens _sprScalingAdjustment (\s a -> s { _sprScalingAdjustment = a })

-- | Specifies whether the ScalingAdjustment is an absolute number or a
-- percentage of the current capacity. Valid values are ChangeInCapacity,
-- ExactCapacity, and PercentChangeInCapacity.
sprAdjustmentType :: Lens' ScalingPolicy (Maybe Text)
sprAdjustmentType =
    lens _sprAdjustmentType (\s a -> s { _sprAdjustmentType = a })

-- | The amount of time, in seconds, after a scaling activity completes before
-- any further trigger-related scaling activities can start.
sprCooldown :: Lens' ScalingPolicy (Maybe Integer)
sprCooldown = lens _sprCooldown (\s a -> s { _sprCooldown = a })

-- | The Amazon Resource Name (ARN) of the policy.
sprPolicyARN :: Lens' ScalingPolicy (Maybe Text)
sprPolicyARN = lens _sprPolicyARN (\s a -> s { _sprPolicyARN = a })

-- | A list of CloudWatch Alarms related to the policy.
sprAlarms :: Lens' ScalingPolicy [Alarm]
sprAlarms = lens _sprAlarms (\s a -> s { _sprAlarms = a })

-- | Changes the DesiredCapacity of the Auto Scaling group by at least the
-- specified number of instances.
sprMinAdjustmentStep :: Lens' ScalingPolicy (Maybe Integer)
sprMinAdjustmentStep =
    lens _sprMinAdjustmentStep (\s a -> s { _sprMinAdjustmentStep = a })

instance FromXML ScalingPolicy where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ScalingPolicy"

-- | This data type stores information about a scheduled update to an Auto
-- Scaling group.
data ScheduledUpdateGroupAction = ScheduledUpdateGroupAction
    { _sugaAutoScalingGroupName :: Maybe Text
    , _sugaScheduledActionName :: Maybe Text
    , _sugaScheduledActionARN :: Maybe Text
    , _sugaTime :: Maybe ISO8601
    , _sugaStartTime :: Maybe ISO8601
    , _sugaEndTime :: Maybe ISO8601
    , _sugaRecurrence :: Maybe Text
    , _sugaMinSize :: Maybe Integer
    , _sugaMaxSize :: Maybe Integer
    , _sugaDesiredCapacity :: Maybe Integer
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'ScheduledUpdateGroupAction' data type.
--
-- 'ScheduledUpdateGroupAction' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @AutoScalingGroupName ::@ @Maybe Text@
--
-- * @ScheduledActionName ::@ @Maybe Text@
--
-- * @ScheduledActionARN ::@ @Maybe Text@
--
-- * @Time ::@ @Maybe ISO8601@
--
-- * @StartTime ::@ @Maybe ISO8601@
--
-- * @EndTime ::@ @Maybe ISO8601@
--
-- * @Recurrence ::@ @Maybe Text@
--
-- * @MinSize ::@ @Maybe Integer@
--
-- * @MaxSize ::@ @Maybe Integer@
--
-- * @DesiredCapacity ::@ @Maybe Integer@
--
mkScheduledUpdateGroupAction :: ScheduledUpdateGroupAction
mkScheduledUpdateGroupAction = ScheduledUpdateGroupAction
    { _sugaAutoScalingGroupName = Nothing
    , _sugaScheduledActionName = Nothing
    , _sugaScheduledActionARN = Nothing
    , _sugaTime = Nothing
    , _sugaStartTime = Nothing
    , _sugaEndTime = Nothing
    , _sugaRecurrence = Nothing
    , _sugaMinSize = Nothing
    , _sugaMaxSize = Nothing
    , _sugaDesiredCapacity = Nothing
    }

-- | The name of the Auto Scaling group to be updated.
sugaAutoScalingGroupName :: Lens' ScheduledUpdateGroupAction (Maybe Text)
sugaAutoScalingGroupName =
    lens _sugaAutoScalingGroupName
         (\s a -> s { _sugaAutoScalingGroupName = a })

-- | The name of this scheduled action.
sugaScheduledActionName :: Lens' ScheduledUpdateGroupAction (Maybe Text)
sugaScheduledActionName =
    lens _sugaScheduledActionName
         (\s a -> s { _sugaScheduledActionName = a })

-- | The Amazon Resource Name (ARN) of this scheduled action.
sugaScheduledActionARN :: Lens' ScheduledUpdateGroupAction (Maybe Text)
sugaScheduledActionARN =
    lens _sugaScheduledActionARN (\s a -> s { _sugaScheduledActionARN = a })

-- | Time is deprecated. The time that the action is scheduled to begin. Time is
-- an alias for StartTime.
sugaTime :: Lens' ScheduledUpdateGroupAction (Maybe ISO8601)
sugaTime = lens _sugaTime (\s a -> s { _sugaTime = a })

-- | The time that the action is scheduled to begin. This value can be up to one
-- month in the future. When StartTime and EndTime are specified with
-- Recurrence, they form the boundaries of when the recurring action will
-- start and stop.
sugaStartTime :: Lens' ScheduledUpdateGroupAction (Maybe ISO8601)
sugaStartTime = lens _sugaStartTime (\s a -> s { _sugaStartTime = a })

-- | The time that the action is scheduled to end. This value can be up to one
-- month in the future.
sugaEndTime :: Lens' ScheduledUpdateGroupAction (Maybe ISO8601)
sugaEndTime = lens _sugaEndTime (\s a -> s { _sugaEndTime = a })

-- | The regular schedule that an action occurs.
sugaRecurrence :: Lens' ScheduledUpdateGroupAction (Maybe Text)
sugaRecurrence = lens _sugaRecurrence (\s a -> s { _sugaRecurrence = a })

-- | The minimum size of the Auto Scaling group.
sugaMinSize :: Lens' ScheduledUpdateGroupAction (Maybe Integer)
sugaMinSize = lens _sugaMinSize (\s a -> s { _sugaMinSize = a })

-- | The maximum size of the Auto Scaling group.
sugaMaxSize :: Lens' ScheduledUpdateGroupAction (Maybe Integer)
sugaMaxSize = lens _sugaMaxSize (\s a -> s { _sugaMaxSize = a })

-- | The number of instances you prefer to maintain in your Auto Scaling group.
sugaDesiredCapacity :: Lens' ScheduledUpdateGroupAction (Maybe Integer)
sugaDesiredCapacity =
    lens _sugaDesiredCapacity (\s a -> s { _sugaDesiredCapacity = a })

instance FromXML ScheduledUpdateGroupAction where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ScheduledUpdateGroupAction"

-- | An Auto Scaling process that has been suspended. For more information, see
-- ProcessType.
data SuspendedProcess = SuspendedProcess
    { _spProcessName :: Maybe Text
    , _spSuspensionReason :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'SuspendedProcess' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @ProcessName ::@ @Maybe Text@
--
-- * @SuspensionReason ::@ @Maybe Text@
--
mkSuspendedProcess :: SuspendedProcess
mkSuspendedProcess = SuspendedProcess
    { _spProcessName = Nothing
    , _spSuspensionReason = Nothing
    }

-- | The name of the suspended process.
spProcessName :: Lens' SuspendedProcess (Maybe Text)
spProcessName = lens _spProcessName (\s a -> s { _spProcessName = a })

-- | The reason that the process was suspended.
spSuspensionReason :: Lens' SuspendedProcess (Maybe Text)
spSuspensionReason =
    lens _spSuspensionReason (\s a -> s { _spSuspensionReason = a })

instance FromXML SuspendedProcess where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "SuspendedProcess"

instance ToQuery SuspendedProcess where
    toQuery = genericQuery def

-- | The tag applied to an Auto Scaling group.
data Tag = Tag
    { _tResourceId :: Text
    , _tResourceType :: Text
    , _tKey :: Text
    , _tValue :: Text
    , _tPropagateAtLaunch :: Bool
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'Tag' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @ResourceId ::@ @Text@
--
-- * @ResourceType ::@ @Text@
--
-- * @Key ::@ @Text@
--
-- * @Value ::@ @Text@
--
-- * @PropagateAtLaunch ::@ @Bool@
--
mkTag :: Text -- ^ 'tResourceId'
      -> Text -- ^ 'tResourceType'
      -> Text -- ^ 'tKey'
      -> Text -- ^ 'tValue'
      -> Bool -- ^ 'tPropagateAtLaunch'
      -> Tag
mkTag p1 p2 p3 p4 p5 = Tag
    { _tResourceId = p1
    , _tResourceType = p2
    , _tKey = p3
    , _tValue = p4
    , _tPropagateAtLaunch = p5
    }

-- | The name of the Auto Scaling group.
tResourceId :: Lens' Tag Text
tResourceId = lens _tResourceId (\s a -> s { _tResourceId = a })

-- | The kind of resource to which the tag is applied. Currently, Auto Scaling
-- supports the auto-scaling-group resource type.
tResourceType :: Lens' Tag Text
tResourceType = lens _tResourceType (\s a -> s { _tResourceType = a })

-- | The key of the tag.
tKey :: Lens' Tag Text
tKey = lens _tKey (\s a -> s { _tKey = a })

-- | The value of the tag.
tValue :: Lens' Tag Text
tValue = lens _tValue (\s a -> s { _tValue = a })

-- | Specifies whether the new tag will be applied to instances launched after
-- the tag is created. The same behavior applies to updates: If you change a
-- tag, the changed tag will be applied to all instances launched after you
-- made the change.
tPropagateAtLaunch :: Lens' Tag Bool
tPropagateAtLaunch =
    lens _tPropagateAtLaunch (\s a -> s { _tPropagateAtLaunch = a })

instance ToQuery Tag where
    toQuery = genericQuery def

-- | The tag applied to an Auto Scaling group.
data TagDescription = TagDescription
    { _tdResourceId :: Text
    , _tdResourceType :: Text
    , _tdKey :: Text
    , _tdValue :: Text
    , _tdPropagateAtLaunch :: Bool
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'TagDescription' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @ResourceId ::@ @Text@
--
-- * @ResourceType ::@ @Text@
--
-- * @Key ::@ @Text@
--
-- * @Value ::@ @Text@
--
-- * @PropagateAtLaunch ::@ @Bool@
--
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

-- | The name of the Auto Scaling group.
tdResourceId :: Lens' TagDescription Text
tdResourceId = lens _tdResourceId (\s a -> s { _tdResourceId = a })

-- | The kind of resource to which the tag is applied. Currently, Auto Scaling
-- supports the auto-scaling-group resource type.
tdResourceType :: Lens' TagDescription Text
tdResourceType = lens _tdResourceType (\s a -> s { _tdResourceType = a })

-- | The key of the tag.
tdKey :: Lens' TagDescription Text
tdKey = lens _tdKey (\s a -> s { _tdKey = a })

-- | The value of the tag.
tdValue :: Lens' TagDescription Text
tdValue = lens _tdValue (\s a -> s { _tdValue = a })

-- | Specifies whether the new tag will be applied to instances launched after
-- the tag is created. The same behavior applies to updates: If you change a
-- tag, the changed tag will be applied to all instances launched after you
-- made the change.
tdPropagateAtLaunch :: Lens' TagDescription Bool
tdPropagateAtLaunch =
    lens _tdPropagateAtLaunch (\s a -> s { _tdPropagateAtLaunch = a })

instance FromXML TagDescription where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "TagDescription"

instance ToQuery TagDescription where
    toQuery = genericQuery def
