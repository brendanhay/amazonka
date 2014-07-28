{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies       #-}

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

-- | Auto Scaling Auto Scaling is a web service designed to automatically launch
-- or terminate Amazon Elastic Compute Cloud (Amazon EC2) instances based on
-- user-defined policies, schedules, and health checks. This service is used
-- in conjunction with Amazon CloudWatch and Elastic Load Balancing services.
-- Auto Scaling provides APIs that you can call by submitting a Query Request.
-- Query requests are HTTP or HTTPS requests that use the HTTP verbs GET or
-- POST and a Query parameter named Action or Operation that specifies the API
-- you are calling. Action is used throughout this documentation, although
-- Operation is also supported for backward compatibility with other Amazon
-- Web Services (AWS) Query APIs. Calling the API using a Query request is the
-- most direct way to access the web service, but requires that your
-- application handle low-level details such as generating the hash to sign
-- the request and error handling. The benefit of calling the service using a
-- Query request is that you are assured of having access to the complete
-- functionality of the API. For information about signing a a query request,
-- see Use Query Requests to Call Auto Scaling APIs This guide provides
-- detailed information about Auto Scaling actions, data types, parameters,
-- and errors. For detailed information about Auto Scaling features and their
-- associated API actions, go to the Auto Scaling Developer Guide. This
-- reference is based on the current WSDL, which is available at:
-- http://autoscaling.amazonaws.com/doc/2011-01-01/AutoScaling.wsdl Endpoints
-- The examples in this guide assume that your instances are launched in the
-- US East (Northern Virginia) region and use us-east-1 as the endpoint. You
-- can set up your Auto Scaling infrastructure in other AWS regions. For
-- information about this product's regions and endpoints, see Regions and
-- Endpoints in the Amazon Web Services General Reference.
module Network.AWS.AutoScaling.V2011_01_01.Types where

import Control.Applicative
import Control.Exception      (Exception)
import Data.Default
import Data.Tagged
import Data.Text              (Text)
import Data.Typeable
import GHC.Generics
import Network.AWS.Data
import Network.AWS.Signing.V4
import Network.AWS.Types      hiding (Error, Region)
import Network.HTTP.Client    (HttpException)

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

    service = Service
        { _svcEndpoint = Regional
        , _svcPrefix   = "autoscaling"
        , _svcVersion  = "2011-01-01"
        , _svcTarget   = Nothing
        }

deriving instance Show    (Er AutoScaling)
deriving instance Generic (Er AutoScaling)

instance AWSError (Er AutoScaling) where
    awsError = const "AutoScalingError"

instance ServiceError (Er AutoScaling) where
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
    = LifecycleStateInService -- ^ InService
    | LifecycleStatePending -- ^ Pending
    | LifecycleStateQuarantined -- ^ Quarantined
    | LifecycleStateTerminated -- ^ Terminated
    | LifecycleStateTerminating -- ^ Terminating
      deriving (Eq, Show, Generic)

instance FromText LifecycleState where
    parser = match "InService" LifecycleStateInService
         <|> match "Pending" LifecycleStatePending
         <|> match "Quarantined" LifecycleStateQuarantined
         <|> match "Terminated" LifecycleStateTerminated
         <|> match "Terminating" LifecycleStateTerminating

instance ToText LifecycleState where
    toText LifecycleStateInService = "InService"
    toText LifecycleStatePending = "Pending"
    toText LifecycleStateQuarantined = "Quarantined"
    toText LifecycleStateTerminated = "Terminated"
    toText LifecycleStateTerminating = "Terminating"

instance ToByteString LifecycleState where
    toBS LifecycleStateInService = "InService"
    toBS LifecycleStatePending = "Pending"
    toBS LifecycleStateQuarantined = "Quarantined"
    toBS LifecycleStateTerminated = "Terminated"
    toBS LifecycleStateTerminating = "Terminating"

instance FromXML LifecycleState where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "LifecycleState"

instance ToQuery LifecycleState where
    toQuery = genericToQuery def

-- | Contains the current status of the activity.
data ScalingActivityStatusCode
    = ScalingActivityStatusCodeCancelled -- ^ Cancelled
    | ScalingActivityStatusCodeFailed -- ^ Failed
    | ScalingActivityStatusCodeInProgress -- ^ InProgress
    | ScalingActivityStatusCodePreInService -- ^ PreInService
    | ScalingActivityStatusCodeSuccessful -- ^ Successful
    | ScalingActivityStatusCodeWaitingForInstanceId -- ^ WaitingForInstanceId
    | ScalingActivityStatusCodeWaitingForSpotInstanceId -- ^ WaitingForSpotInstanceId
    | ScalingActivityStatusCodeWaitingForSpotInstanceRequestId -- ^ WaitingForSpotInstanceRequestId
      deriving (Eq, Show, Generic)

instance FromText ScalingActivityStatusCode where
    parser = match "Cancelled" ScalingActivityStatusCodeCancelled
         <|> match "Failed" ScalingActivityStatusCodeFailed
         <|> match "InProgress" ScalingActivityStatusCodeInProgress
         <|> match "PreInService" ScalingActivityStatusCodePreInService
         <|> match "Successful" ScalingActivityStatusCodeSuccessful
         <|> match "WaitingForInstanceId" ScalingActivityStatusCodeWaitingForInstanceId
         <|> match "WaitingForSpotInstanceId" ScalingActivityStatusCodeWaitingForSpotInstanceId
         <|> match "WaitingForSpotInstanceRequestId" ScalingActivityStatusCodeWaitingForSpotInstanceRequestId

instance ToText ScalingActivityStatusCode where
    toText ScalingActivityStatusCodeCancelled = "Cancelled"
    toText ScalingActivityStatusCodeFailed = "Failed"
    toText ScalingActivityStatusCodeInProgress = "InProgress"
    toText ScalingActivityStatusCodePreInService = "PreInService"
    toText ScalingActivityStatusCodeSuccessful = "Successful"
    toText ScalingActivityStatusCodeWaitingForInstanceId = "WaitingForInstanceId"
    toText ScalingActivityStatusCodeWaitingForSpotInstanceId = "WaitingForSpotInstanceId"
    toText ScalingActivityStatusCodeWaitingForSpotInstanceRequestId = "WaitingForSpotInstanceRequestId"

instance ToByteString ScalingActivityStatusCode where
    toBS ScalingActivityStatusCodeCancelled = "Cancelled"
    toBS ScalingActivityStatusCodeFailed = "Failed"
    toBS ScalingActivityStatusCodeInProgress = "InProgress"
    toBS ScalingActivityStatusCodePreInService = "PreInService"
    toBS ScalingActivityStatusCodeSuccessful = "Successful"
    toBS ScalingActivityStatusCodeWaitingForInstanceId = "WaitingForInstanceId"
    toBS ScalingActivityStatusCodeWaitingForSpotInstanceId = "WaitingForSpotInstanceId"
    toBS ScalingActivityStatusCodeWaitingForSpotInstanceRequestId = "WaitingForSpotInstanceRequestId"

instance FromXML ScalingActivityStatusCode where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ScalingActivityStatusCode"

instance ToQuery ScalingActivityStatusCode where
    toQuery = genericToQuery def

-- | Specifies whether the PutScalingPolicy ScalingAdjustment parameter is an
-- absolute number or a percentage of the current capacity.
newtype AdjustmentType = AdjustmentType
    { _axAdjustmentType :: Maybe Text
      -- ^ A policy adjustment type. Valid values are ChangeInCapacity,
      -- ExactCapacity, and PercentChangeInCapacity.
    } deriving (Generic)

instance FromXML AdjustmentType where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "AdjustmentType"

-- | Controls whether instances in this group are launched with detailed
-- monitoring or not.
newtype InstanceMonitoring = InstanceMonitoring
    { _imEnabled :: Maybe Bool
      -- ^ If True, instance monitoring is enabled.
    } deriving (Generic)

instance FromXML InstanceMonitoring where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "InstanceMonitoring"

instance ToQuery InstanceMonitoring where
    toQuery = genericToQuery def

-- | The MetricCollectionType data type.
newtype MetricCollectionType = MetricCollectionType
    { _mcuMetric :: Maybe Text
      -- ^ 
    } deriving (Generic)

instance FromXML MetricCollectionType where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "MetricCollectionType"

-- | The MetricGranularityType data type.
newtype MetricGranularityType = MetricGranularityType
    { _mguGranularity :: Maybe Text
      -- ^ The granularity of a Metric.
    } deriving (Generic)

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
    { _puProcessName :: Text
      -- ^ The name of a process.
    } deriving (Generic)

instance FromXML ProcessType where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ProcessType"

-- | A scaling Activity.
data Activity = Activity
    { _ayProgress :: Maybe Integer
      -- ^ Specifies a value between 0 and 100 that indicates the progress
      -- of the activity.
    , _ayStartTime :: ISO8601
      -- ^ Provides the start time of this activity.
    , _ayActivityId :: Text
      -- ^ Specifies the ID of the activity.
    , _ayCause :: Text
      -- ^ Contains the reason the activity was begun.
    , _ayStatusMessage :: Maybe Text
      -- ^ Contains a friendly, more verbose description of the activity
      -- status.
    , _ayAutoScalingGroupName :: Text
      -- ^ The name of the Auto Scaling group.
    , _ayEndTime :: Maybe ISO8601
      -- ^ Provides the end time of this activity.
    , _ayDetails :: Maybe Text
      -- ^ Contains details of the scaling activity.
    , _ayDescription :: Maybe Text
      -- ^ Contains a friendly, more verbose description of the scaling
      -- activity.
    , _ayStatusCode :: ScalingActivityStatusCode
      -- ^ Contains the current status of the activity.
    } deriving (Generic)

instance FromXML Activity where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Activity"

-- | The Alarm data type.
data Alarm = Alarm
    { _amAlarmName :: Maybe Text
      -- ^ The name of the alarm.
    , _amAlarmARN :: Maybe Text
      -- ^ The Amazon Resource Name (ARN) of the alarm.
    } deriving (Generic)

instance FromXML Alarm where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Alarm"

instance ToQuery Alarm where
    toQuery = genericToQuery def

-- | The AutoScalingGroup data type.
data AutoScalingGroup = AutoScalingGroup
    { _ashStatus :: Maybe Text
      -- ^ The current state of the Auto Scaling group when a
      -- DeleteAutoScalingGroup action is in progress.
    , _ashTerminationPolicies :: [Text]
      -- ^ A standalone termination policy or a list of termination policies
      -- for this Auto Scaling group.
    , _ashCreatedTime :: ISO8601
      -- ^ Specifies the date and time the Auto Scaling group was created.
    , _ashHealthCheckGracePeriod :: Maybe Integer
      -- ^ The length of time that Auto Scaling waits before checking an
      -- instance's health status. The grace period begins when an
      -- instance comes into service.
    , _ashVPCZoneIdentifier :: Maybe Text
      -- ^ The subnet identifier for the Amazon VPC connection, if
      -- applicable. You can specify several subnets in a comma-separated
      -- list. When you specify VPCZoneIdentifier with AvailabilityZones,
      -- ensure that the subnets' Availability Zones match the values you
      -- specify for AvailabilityZones.
    , _ashDefaultCooldown :: Integer
      -- ^ The number of seconds after a scaling activity completes before
      -- any further scaling activities can start.
    , _ashMaxSize :: Integer
      -- ^ Contains the maximum size of the Auto Scaling group.
    , _ashAvailabilityZones :: [Text]
      -- ^ Contains a list of Availability Zones for the group.
    , _ashDesiredCapacity :: Integer
      -- ^ Specifies the desired capacity for the Auto Scaling group.
    , _ashMinSize :: Integer
      -- ^ Contains the minimum size of the Auto Scaling group.
    , _ashEnabledMetrics :: [EnabledMetric]
      -- ^ A list of metrics enabled for this Auto Scaling group.
    , _ashAutoScalingGroupName :: Text
      -- ^ Specifies the name of the group.
    , _ashLaunchConfigurationName :: Text
      -- ^ Specifies the name of the associated LaunchConfiguration.
    , _ashInstances :: [Instance]
      -- ^ Provides a summary list of Amazon EC2 instances.
    , _ashHealthCheckType :: Text
      -- ^ The service of interest for the health status check, either "EC2"
      -- for Amazon EC2 or "ELB" for Elastic Load Balancing.
    , _ashAutoScalingGroupARN :: Maybe Text
      -- ^ The Amazon Resource Name (ARN) of the Auto Scaling group.
    , _ashPlacementGroup :: Maybe Text
      -- ^ The name of the cluster placement group, if applicable. For more
      -- information, go to Using Cluster Instances in the Amazon EC2 User
      -- Guide.
    , _ashSuspendedProcesses :: [SuspendedProcess]
      -- ^ Suspended processes associated with this Auto Scaling group.
    , _ashLoadBalancerNames :: [Text]
      -- ^ A list of load balancers associated with this Auto Scaling group.
    , _ashTags :: [TagDescription]
      -- ^ A list of tags for the Auto Scaling group.
    } deriving (Generic)

instance FromXML AutoScalingGroup where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "AutoScalingGroup"

-- | The AutoScalingInstanceDetails data type.
data AutoScalingInstanceDetails = AutoScalingInstanceDetails
    { _asidInstanceId :: Text
      -- ^ The instance ID of the Amazon EC2 instance.
    , _asidAvailabilityZone :: Text
      -- ^ The Availability Zone in which this instance resides.
    , _asidAutoScalingGroupName :: Text
      -- ^ The name of the Auto Scaling group associated with this instance.
    , _asidLaunchConfigurationName :: Text
      -- ^ The launch configuration associated with this instance.
    , _asidHealthStatus :: Text
      -- ^ The health status of this instance. "Healthy" means that the
      -- instance is healthy and should remain in service. "Unhealthy"
      -- means that the instance is unhealthy. Auto Scaling should
      -- terminate and replace it.
    , _asidLifecycleState :: Text
      -- ^ The life cycle state of this instance. for more information, see
      -- Instance Lifecycle State in the Auto Scaling Developer Guide.
    } deriving (Generic)

instance FromXML AutoScalingInstanceDetails where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "AutoScalingInstanceDetails"

-- | The BlockDeviceMapping data type.
data BlockDeviceMapping = BlockDeviceMapping
    { _bdnVirtualName :: Maybe Text
      -- ^ The virtual name associated with the device.
    , _bdnNoDevice :: Maybe Bool
      -- ^ Suppresses the device mapping. If NoDevice is set to true for the
      -- root device, the instance might fail the EC2 health check. Auto
      -- Scaling launches a replacement instance if the instance fails the
      -- health check.
    , _bdnEbs :: Maybe Ebs
      -- ^ The Elastic Block Storage volume information.
    , _bdnDeviceName :: Text
      -- ^ The name of the device within Amazon EC2 (for example, /dev/sdh
      -- or xvdh).
    } deriving (Generic)

instance FromXML BlockDeviceMapping where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "BlockDeviceMapping"

instance ToQuery BlockDeviceMapping where
    toQuery = genericToQuery def

-- | The Elastic Block Storage volume information.
data Ebs = Ebs
    { _esDeleteOnTermination :: Maybe Bool
      -- ^ Indicates whether to delete the volume on instance termination.
      -- Default: true.
    , _esVolumeSize :: Maybe Integer
      -- ^ The volume size, in gigabytes. Valid values: If the volume type
      -- is io1, the minimum size of the volume is 10. Default: If you're
      -- creating the volume from a snapshot, and you don't specify a
      -- volume size, the default is the snapshot size. Required: Required
      -- when the volume type is io1.
    , _esIops :: Maybe Integer
      -- ^ The number of I/O operations per second (IOPS) that the volume
      -- supports. The maximum ratio of IOPS to volume size is 30.0 Valid
      -- Values: Range is 100 to 4000. Default: None.
    , _esVolumeType :: Maybe Text
      -- ^ The volume type. Valid values: standard | io1 Default: standard.
    , _esSnapshotId :: Maybe Text
      -- ^ The snapshot ID.
    } deriving (Generic)

instance FromXML Ebs where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Ebs"

instance ToQuery Ebs where
    toQuery = genericToQuery def

-- | The EnabledMetric data type.
data EnabledMetric = EnabledMetric
    { _enGranularity :: Maybe Text
      -- ^ The granularity of the enabled metric.
    , _enMetric :: Maybe Text
      -- ^ The name of the enabled metric.
    } deriving (Generic)

instance FromXML EnabledMetric where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "EnabledMetric"

instance ToQuery EnabledMetric where
    toQuery = genericToQuery def

-- | The Filter data type.
data Filter = Filter
    { _gValues :: [Text]
      -- ^ The value of the filter.
    , _gName :: Maybe Text
      -- ^ The name of the filter. Valid Name values are:
      -- "auto-scaling-group", "key", "value", and "propagate-at-launch".
    } deriving (Generic)

instance ToQuery Filter where
    toQuery = genericToQuery def

-- | The Instance data type.
data Instance = Instance
    { _sInstanceId :: Text
      -- ^ Specifies the ID of the Amazon EC2 instance.
    , _sAvailabilityZone :: Text
      -- ^ Availability Zones associated with this instance.
    , _sLaunchConfigurationName :: Text
      -- ^ The launch configuration associated with this instance.
    , _sHealthStatus :: Text
      -- ^ The instance's health status.
    , _sLifecycleState :: LifecycleState
      -- ^ Contains a description of the current lifecycle state. The
      -- Quarantined lifecycle state is currently not used.
    } deriving (Generic)

instance FromXML Instance where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Instance"

instance ToQuery Instance where
    toQuery = genericToQuery def

-- | The LaunchConfiguration data type.
data LaunchConfiguration = LaunchConfiguration
    { _ldAssociatePublicIpAddress :: Maybe Bool
      -- ^ Specifies whether the instance is associated with a public IP
      -- address (true) or not (false).
    , _ldSecurityGroups :: [Text]
      -- ^ A description of the security groups to associate with the Amazon
      -- EC2 instances.
    , _ldSpotPrice :: Maybe Text
      -- ^ Specifies the price to bid when launching Spot Instances.
    , _ldCreatedTime :: ISO8601
      -- ^ Provides the creation date and time for this launch
      -- configuration.
    , _ldInstanceMonitoring :: Maybe InstanceMonitoring
      -- ^ Controls whether instances in this group are launched with
      -- detailed monitoring or not.
    , _ldKeyName :: Maybe Text
      -- ^ Provides the name of the Amazon EC2 key pair.
    , _ldRamdiskId :: Maybe Text
      -- ^ Provides ID of the RAM disk associated with the Amazon EC2 AMI.
    , _ldKernelId :: Maybe Text
      -- ^ Provides the ID of the kernel associated with the Amazon EC2 AMI.
    , _ldInstanceType :: Text
      -- ^ Specifies the instance type of the Amazon EC2 instance.
    , _ldEbsOptimized :: Maybe Bool
      -- ^ Specifies whether the instance is optimized for EBS I/O (true) or
      -- not (false).
    , _ldUserData :: Maybe Text
      -- ^ The user data available to the launched Amazon EC2 instances.
    , _ldIamInstanceProfile :: Maybe Text
      -- ^ Provides the name or the Amazon Resource Name (ARN) of the
      -- instance profile associated with the IAM role for the instance.
      -- The instance profile contains the IAM role.
    , _ldImageId :: Text
      -- ^ Provides the unique ID of the Amazon Machine Image (AMI) that was
      -- assigned during registration.
    , _ldLaunchConfigurationName :: Text
      -- ^ Specifies the name of the launch configuration.
    , _ldLaunchConfigurationARN :: Maybe Text
      -- ^ The launch configuration's Amazon Resource Name (ARN).
    , _ldPlacementTenancy :: Maybe Text
      -- ^ Specifies the tenancy of the instance. It can be either default
      -- or dedicated. An instance with dedicated tenancy runs in an
      -- isolated, single-tenant hardware and it can only be launched in a
      -- VPC.
    , _ldBlockDeviceMappings :: [BlockDeviceMapping]
      -- ^ Specifies how block devices are exposed to the instance. Each
      -- mapping is made up of a virtualName and a deviceName.
    } deriving (Generic)

instance FromXML LaunchConfiguration where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "LaunchConfiguration"

-- | The NotificationConfiguration data type.
data NotificationConfiguration = NotificationConfiguration
    { _neTopicARN :: Maybe Text
      -- ^ The Amazon Resource Name (ARN) of the Amazon Simple Notification
      -- Service (SNS) topic.
    , _neAutoScalingGroupName :: Maybe Text
      -- ^ Specifies the Auto Scaling group name.
    , _neNotificationType :: Maybe Text
      -- ^ The types of events for an action to start.
    } deriving (Generic)

instance FromXML NotificationConfiguration where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "NotificationConfiguration"

-- | The ScalingPolicy data type.
data ScalingPolicy = ScalingPolicy
    { _ssMinAdjustmentStep :: Maybe Integer
      -- ^ Changes the DesiredCapacity of the Auto Scaling group by at least
      -- the specified number of instances.
    , _ssPolicyName :: Maybe Text
      -- ^ The name of the scaling policy.
    , _ssAdjustmentType :: Maybe Text
      -- ^ Specifies whether the ScalingAdjustment is an absolute number or
      -- a percentage of the current capacity. Valid values are
      -- ChangeInCapacity, ExactCapacity, and PercentChangeInCapacity.
    , _ssAutoScalingGroupName :: Maybe Text
      -- ^ The name of the Auto Scaling group associated with this scaling
      -- policy.
    , _ssScalingAdjustment :: Maybe Integer
      -- ^ The number associated with the specified adjustment type. A
      -- positive value adds to the current capacity and a negative value
      -- removes from the current capacity.
    , _ssCooldown :: Maybe Integer
      -- ^ The amount of time, in seconds, after a scaling activity
      -- completes before any further trigger-related scaling activities
      -- can start.
    , _ssPolicyARN :: Maybe Text
      -- ^ The Amazon Resource Name (ARN) of the policy.
    , _ssAlarms :: [Alarm]
      -- ^ A list of CloudWatch Alarms related to the policy.
    } deriving (Generic)

instance FromXML ScalingPolicy where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ScalingPolicy"

-- | This data type stores information about a scheduled update to an Auto
-- Scaling group.
data ScheduledUpdateGroupAction = ScheduledUpdateGroupAction
    { _sugbScheduledActionARN :: Maybe Text
      -- ^ The Amazon Resource Name (ARN) of this scheduled action.
    , _sugbStartTime :: Maybe ISO8601
      -- ^ The time that the action is scheduled to begin. This value can be
      -- up to one month in the future. When StartTime and EndTime are
      -- specified with Recurrence, they form the boundaries of when the
      -- recurring action will start and stop.
    , _sugbTime :: Maybe ISO8601
      -- ^ Time is deprecated. The time that the action is scheduled to
      -- begin. Time is an alias for StartTime.
    , _sugbScheduledActionName :: Maybe Text
      -- ^ The name of this scheduled action.
    , _sugbMaxSize :: Maybe Integer
      -- ^ The maximum size of the Auto Scaling group.
    , _sugbRecurrence :: Maybe Text
      -- ^ The regular schedule that an action occurs.
    , _sugbDesiredCapacity :: Maybe Integer
      -- ^ The number of instances you prefer to maintain in your Auto
      -- Scaling group.
    , _sugbMinSize :: Maybe Integer
      -- ^ The minimum size of the Auto Scaling group.
    , _sugbAutoScalingGroupName :: Maybe Text
      -- ^ The name of the Auto Scaling group to be updated.
    , _sugbEndTime :: Maybe ISO8601
      -- ^ The time that the action is scheduled to end. This value can be
      -- up to one month in the future.
    } deriving (Generic)

instance FromXML ScheduledUpdateGroupAction where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ScheduledUpdateGroupAction"

-- | An Auto Scaling process that has been suspended. For more information, see
-- ProcessType.
data SuspendedProcess = SuspendedProcess
    { _sqProcessName :: Maybe Text
      -- ^ The name of the suspended process.
    , _sqSuspensionReason :: Maybe Text
      -- ^ The reason that the process was suspended.
    } deriving (Generic)

instance FromXML SuspendedProcess where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "SuspendedProcess"

instance ToQuery SuspendedProcess where
    toQuery = genericToQuery def

-- | The tag applied to an Auto Scaling group.
data Tag = Tag
    { _uResourceId :: Maybe Text
      -- ^ The name of the Auto Scaling group.
    , _uResourceType :: Maybe Text
      -- ^ The kind of resource to which the tag is applied. Currently, Auto
      -- Scaling supports the auto-scaling-group resource type.
    , _uValue :: Maybe Text
      -- ^ The value of the tag.
    , _uKey :: Text
      -- ^ The key of the tag.
    , _uPropagateAtLaunch :: Maybe Bool
      -- ^ Specifies whether the new tag will be applied to instances
      -- launched after the tag is created. The same behavior applies to
      -- updates: If you change a tag, the changed tag will be applied to
      -- all instances launched after you made the change.
    } deriving (Generic)

instance ToQuery Tag where
    toQuery = genericToQuery def

-- | The tag applied to an Auto Scaling group.
data TagDescription = TagDescription
    { _tdResourceId :: Maybe Text
      -- ^ The name of the Auto Scaling group.
    , _tdResourceType :: Maybe Text
      -- ^ The kind of resource to which the tag is applied. Currently, Auto
      -- Scaling supports the auto-scaling-group resource type.
    , _tdValue :: Maybe Text
      -- ^ The value of the tag.
    , _tdKey :: Maybe Text
      -- ^ The key of the tag.
    , _tdPropagateAtLaunch :: Maybe Bool
      -- ^ Specifies whether the new tag will be applied to instances
      -- launched after the tag is created. The same behavior applies to
      -- updates: If you change a tag, the changed tag will be applied to
      -- all instances launched after you made the change.
    } deriving (Generic)

instance FromXML TagDescription where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "TagDescription"

instance ToQuery TagDescription where
    toQuery = genericToQuery def
