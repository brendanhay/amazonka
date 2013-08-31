{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE OverloadedStrings          #-}

-- Module      : Network.AWS.AutoScaling.Types
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.AutoScaling.Types where

import Data.ByteString      (ByteString)
import Data.Monoid
import Data.Time
import Network.AWS.Internal

-- | Currently supported version of the AutoScaling service.
autoScalingVersion :: ByteString
autoScalingVersion = "2011-01-01"

-- | XML namespace to annotate AutoScaling elements with.
autoScalingNS :: ByteString
autoScalingNS = "http://autoscaling.amazonaws.com/doc/" <> autoScalingVersion <> "/"

-- | Helper to define AutoScaling namespaced XML elements.
autoScalingElem :: ByteString -> NName ByteString
autoScalingElem = mkNName autoScalingNS

data ResponseMetadata = ResponseMetadata
    { rmRequestId :: !ByteString
    } deriving (Eq, Show, Generic)

instance IsXML ResponseMetadata where
    xmlPickler = withNS autoScalingNS

data ErrorType = Receiver | Sender
    deriving (Eq, Show, Read, Generic)

instance IsXML ErrorType where
    xmlPickler = xpContent xpPrim

data Error = Error
    { eType    :: !ErrorType
    , eCode    :: !ByteString
    , eMessage :: !ByteString
    , eDetail  :: !ByteString
    } deriving (Eq, Show, Generic)

instance IsXML Error where
    xmlPickler = withNS autoScalingNS

newtype ResourceName = ResourceName ByteString
    deriving (Eq, Show, Generic, IsByteString)

instance IsQuery ResourceName

-- | A scaling Activity is a long-running process that represents a change to
-- your AutoScalingGroup, such as changing the size of the group. It can also
-- be a process to replace an instance, or a process to perform any other
-- long-running operations supported by the API.
--
-- <http://docs.aws.amazon.com/AutoScaling/latest/APIReference/API_Activity.html>
data Activity = Activity
    { aActivityId           :: !ByteString
      -- ^ Specifies the ID of the activity.
    , aAutoScalingGroupName :: !ByteString
      -- ^ The name of the Auto Scaling group.
    , aCause                :: !ByteString
      -- ^ Contains the reason the activity was begun.
    , aDescription          :: Maybe ByteString
      -- ^ Contains a friendly, more verbose description of the scaling activity.
    , aDetails              :: Maybe ByteString
      -- ^ Contains details of the scaling activity.
    , aEndTime              :: Maybe UTCTime
      -- ^ Provides the end time of this activity.
    , aProgress             :: Maybe Integer
      -- ^ Specifies a value between 0 and 100 that indicates the progress
      -- of the activity.
    , aStartTime            :: !UTCTime
      -- ^ Provides the start time of this activity.
    , aStatusCode           :: !ByteString
      -- ^ Contains the current status of the activity.
    , aStatusMessage        :: Maybe ByteString
      -- ^ Contains a friendly, more verbose description of the activity status.
    } deriving (Eq, Show, Generic)

instance IsQuery Activity

instance IsXML Activity where
    xmlPickler = withNS autoScalingNS

-- | Specifies whether the PutScalingPolicy ScalingAdjustment parameter is an
-- absolute number or a percentage of the current capacity.
--
-- <http://docs.aws.amazon.com/AutoScaling/latest/APIReference/API_AdjustmentType.html>
data AdjustmentType
    = ChangeInCapacity
    | ExactCapacity
    | PercentChangeInCapacity
      deriving (Eq, Read, Show, Generic)

instance IsQuery AdjustmentType where
    queryPickler = qpPrim

instance IsXML AdjustmentType where
    xmlPickler = xpElem (autoScalingElem "AdjustmentType") $ xpContent xpPrim

-- | The Alarm data type.
--
-- <http://docs.aws.amazon.com/AutoScaling/latest/APIReference/API_Alarm.html>
data Alarm = Alarm
    { aAlarmARN  :: Maybe ByteString
      -- ^ The Amazon Resource Name (ARN) of the alarm.
    , aAlarmName :: Maybe ByteString
      -- ^ The name of the alarm.
    } deriving (Eq, Show, Generic)

instance IsQuery Alarm

instance IsXML Alarm where
    xmlPickler = withNS autoScalingNS

-- | The AutoScalingGroup data type.
--
-- <http://docs.aws.amazon.com/AutoScaling/latest/APIReference/API_AutoScalingGroup.html>
data AutoScalingGroup = AutoScalingGroup
    { asgAutoScalingGroupARN     :: Maybe ByteString
      -- ^ The Amazon Resource Name (ARN) of the Auto Scaling group.
    , asgAutoScalingGroupName    :: !ByteString
      -- ^ Specifies the name of the group.
    , asgAvailabilityZones       :: Members ByteString
      -- ^ Contains a list of Availability Zones for the group.
    , asgCreatedTime             :: !UTCTime
      -- ^ Specifies the date and time the Auto Scaling group was created.
    , asgDefaultCooldown         :: !Integer
      -- ^ The number of seconds after a scaling activity completes before
      -- any further scaling activities can start.
    , asgDesiredCapacity         :: !Integer
      -- ^ Specifies the desired capacity for the Auto Scaling group.
    , asgEnabledMetrics          :: Members EnabledMetric
      -- ^ A list of metrics enabled for this Auto Scaling group.
    , asgHealthCheckGracePeriod  :: Maybe Integer
      -- ^ The length of time that Auto Scaling waits before checking an
      -- instance's health status. The grace period begins when an
      -- instance comes into service.
    , asgHealthCheckType         :: !ByteString
      -- ^ The service of interest for the health status check, either "EC2"
      -- for Amazon EC2 or "ELB" for Elastic Load Balancing.
    , asgInstances               :: Members Instance
      -- ^ Provides a summary list of Amazon EC2 instances.
    , asgLaunchConfigurationName :: !ByteString
      -- ^ Specifies the name of the associated LaunchConfiguration.
    , asgLoadBalancerNames       :: Members ByteString
      -- ^ A list of load balancers associated with this Auto Scaling group.
    , asgMaxSize                 :: !Integer
      -- ^ Contains the maximum size of the Auto Scaling group.
    , asgMinSize                 :: !Integer
      -- ^ Contains the minimum size of the Auto Scaling group.
    , asgPlacementGroup          :: Maybe ByteString
      -- ^ The name of the cluster placement group, if applicable.
    , asgStatus                  :: Maybe ByteString
      -- ^ The current state of the Auto Scaling group when a
      -- DeleteAutoScalingGroup action is in progress.
    , asgSuspendedProcesses      :: Members SuspendedProcess
      -- ^ Suspended processes associated with this Auto Scaling group.
    , asgTags                    :: Members Tag
      -- ^ A list of tags for the Auto Scaling group.
    , asgTerminationPolicies     :: Members ByteString
      -- ^ A standalone termination policy or a list of termination policies
      -- for this Auto Scaling group.
    , asgVPCZoneIdentifier       :: Maybe ByteString
      -- ^ The subnet identifier for the Amazon VPC connection, if applicable.
      -- You can specify several subnets in a comma-separated list.
    } deriving (Eq, Show, Generic)

instance IsQuery AutoScalingGroup

instance IsXML AutoScalingGroup where
    xmlPickler = withNS autoScalingNS

-- | The AutoScalingInstanceDetails data type.
--
-- <http://docs.aws.amazon.com/AutoScaling/latest/APIReference/API_AutoScalingInstanceDetails.html>
data AutoScalingInstanceDetails = AutoScalingInstanceDetails
    { asidAutoScalingGroupName    :: !ByteString
      -- ^ The name of the Auto Scaling group associated with this instance.
    , asidAvailabilityZone        :: !ByteString
      -- ^ The Availability Zone in which this instance resides.
    , asidHealthStatus            :: !ByteString
      -- ^ The health status of this instance. "Healthy" means that the
      -- instance is healthy and should remain in service. "Unhealthy"
      -- means that the instance is unhealthy. Auto Scaling should
      -- terminate and replace it.
    , asidInstanceId              :: !ByteString
      -- ^ The instance ID of the Amazon EC2 instance.
    , asidLaunchConfigurationName :: !ByteString
      -- ^ The launch configuration associated with this instance.
    , asidLifecycleState          :: !ByteString
      -- ^ The life cycle state of this instance. for more information, see
      -- Instance Lifecycle State in the Auto Scaling Developer Guide.
    } deriving (Eq, Show, Generic)

instance IsQuery AutoScalingInstanceDetails

instance IsXML AutoScalingInstanceDetails where
    xmlPickler = withNS autoScalingNS

-- | The BlockDeviceMapping data type.
--
-- <http://docs.aws.amazon.com/AutoScaling/latest/APIReference/API_BlockDeviceMapping.html>
data BlockDeviceMapping = BlockDeviceMapping
    { bdmDeviceName  :: !ByteString
      -- ^ The name of the device within Amazon EC2.
    , bdmEbs         :: Maybe Ebs
      -- ^ The Elastic Block Storage volume information.
    , bdmVirtualName :: Maybe ByteString
      -- ^ The virtual name associated with the device.
    } deriving (Eq, Show, Generic)

instance IsQuery BlockDeviceMapping

instance IsXML BlockDeviceMapping where
    xmlPickler = withNS autoScalingNS

-- | The output of the DescribeAdjustmentTypes action.
--
-- <http://docs.aws.amazon.com/AutoScaling/latest/APIReference/API_DescribeAdjustmentTypesResult.html>
data DescribeAdjustmentTypesResult = DescribeAdjustmentTypesResult
    { datrAdjustmentTypes :: Members AdjustmentType
      -- ^ A list of specific policy adjustment types.
    } deriving (Eq, Show, Generic)

instance IsXML DescribeAdjustmentTypesResult where
    xmlPickler = withNS autoScalingNS

-- | The AutoScalingGroupsType data type.
--
-- <http://docs.aws.amazon.com/AutoScaling/latest/APIReference/API_DescribeAutoScalingGroupsResult.html>
data DescribeAutoScalingGroupsResult = DescribeAutoScalingGroupsResult
    { dasgrAutoScalingGroups :: Members AutoScalingGroup
      -- ^ A list of Auto Scaling groups.
    , dasgrNextToken         :: Maybe ByteString
      -- ^ A string that marks the start of the next batch of returned results.
    } deriving (Eq, Show, Generic)

instance IsXML DescribeAutoScalingGroupsResult where
    xmlPickler = withNS autoScalingNS

-- | The AutoScalingInstancesType data type.
--
-- <http://docs.aws.amazon.com/AutoScaling/latest/APIReference/API_DescribeAutoScalingInstancesResult.html>
data DescribeAutoScalingInstancesResult = DescribeAutoScalingInstancesResult
    { dasirAutoScalingInstances :: Members AutoScalingInstanceDetails
      -- ^ A list of Auto Scaling instances.
    , dasirNextToken            :: Maybe ByteString
      -- ^ A string that marks the start of the next batch of returned results.
    } deriving (Eq, Show, Generic)

instance IsXML DescribeAutoScalingInstancesResult where
    xmlPickler = withNS autoScalingNS

-- | The AutoScalingNotificationTypes data type.
--
-- <http://docs.aws.amazon.com/AutoScaling/latest/APIReference/API_DescribeAutoScalingNotificationTypesResult.html>
data DescribeAutoScalingNotificationTypesResult = DescribeAutoScalingNotificationTypesResult
    { dasntrAutoScalingNotificationTypes :: [ByteString]
      -- ^ Returns a list of all notification types supported by Auto Scaling.
    } deriving (Eq, Show, Generic)

instance IsXML DescribeAutoScalingNotificationTypesResult where
    xmlPickler = withNS autoScalingNS

-- | The LaunchConfigurationsType data type.
--
-- <http://docs.aws.amazon.com/AutoScaling/latest/APIReference/API_DescribeLaunchConfigurationsResult.html>
data DescribeLaunchConfigurationsResult = DescribeLaunchConfigurationsResult
    { dlcrLaunchConfigurations :: Members LaunchConfiguration
      -- ^ A list of launch configurations.
    , dlcrNextToken            :: Maybe ByteString
      -- ^ A string that marks the start of the next batch of returned results.
    } deriving (Eq, Show, Generic)

instance IsXML DescribeLaunchConfigurationsResult where
    xmlPickler = withNS autoScalingNS

-- | The output of the DescribeMetricCollectionTypes action.
--
-- <http://docs.aws.amazon.com/AutoScaling/latest/APIReference/API_DescribeMetricCollectionTypesResult.html>
data DescribeMetricCollectionTypesResult = DescribeMetricCollectionTypesResult
    { dmctrGranularities :: Members MetricGranularityType
      -- ^ A list of granularities for the listed Metrics.
    , dmctrMetrics       :: Members MetricCollectionType
      -- ^ The list of Metrics collected.
    } deriving (Eq, Show, Generic)

instance IsXML DescribeMetricCollectionTypesResult where
    xmlPickler = withNS autoScalingNS

-- | The output of the DescribeNotificationConfigurations action.
--
-- <http://docs.aws.amazon.com/AutoScaling/latest/APIReference/API_DescribeNotificationConfigurationsResult.html>
data DescribeNotificationConfigurationsResult = DescribeNotificationConfigurationsResult
    { dncrNextToken                  :: Maybe ByteString
      -- ^ A string that is used to mark the start of the next batch of
      -- returned results for pagination.
    , dncrNotificationConfigurations :: Members NotificationConfiguration
      -- ^ The list of notification configurations.
    } deriving (Eq, Show, Generic)

instance IsXML DescribeNotificationConfigurationsResult where
    xmlPickler = withNS autoScalingNS

-- | The PoliciesType data type.
--
-- <http://docs.aws.amazon.com/AutoScaling/latest/APIReference/API_DescribePoliciesResult.html>
data DescribePoliciesResult = DescribePoliciesResult
    { dprNextToken       :: Maybe ByteString
      -- ^ A string that marks the start of the next batch of returned results.
    , dprScalingPolicies :: Members ScalingPolicy
      -- ^ A list of scaling policies.
    } deriving (Eq, Show, Generic)

instance IsXML DescribePoliciesResult where
    xmlPickler = withNS autoScalingNS

-- | The output for the DescribeScalingActivities action.
--
-- <http://docs.aws.amazon.com/AutoScaling/latest/APIReference/API_DescribeScalingActivitiesResult.html>
data DescribeScalingActivitiesResult = DescribeScalingActivitiesResult
    { dsarActivities :: Members Activity
      -- ^ A list of the requested scaling activities.
    , dsarNextToken  :: Maybe ByteString
      -- ^ Acts as a paging mechanism for large result sets. Set to a
      -- non-empty string if there are additional results waiting to be
      -- returned. Pass this in to subsequent calls to return additional
      -- results.
    } deriving (Eq, Show, Generic)

instance IsXML DescribeScalingActivitiesResult where
    xmlPickler = withNS autoScalingNS

-- | The output of the DescribeScalingProcessTypes action.
--
-- <http://docs.aws.amazon.com/AutoScaling/latest/APIReference/API_DescribeScalingProcessTypesResult.html>
data DescribeScalingProcessTypesResult = DescribeScalingProcessTypesResult
    { dsptrProcesses :: Members ProcessType
      -- ^ A list of ProcessType names.
    } deriving (Eq, Show, Generic)

instance IsXML DescribeScalingProcessTypesResult where
    xmlPickler = withNS autoScalingNS

-- | A scaling action that is scheduled for a future time and date. An action
-- can be scheduled up to thirty days in advance. Starting with API version
-- 2011-01-01, you can use recurrence to specify that a scaling action occurs
-- regularly on a schedule.
--
-- <http://docs.aws.amazon.com/AutoScaling/latest/APIReference/API_DescribeScheduledActionsResult.html>
data DescribeScheduledActionsResult = DescribeScheduledActionsResult
    { dsasNextToken                   :: Maybe ByteString
      -- ^ A string that marks the start of the next batch of returned
      -- results.
    , dsasScheduledUpdateGroupActions :: Members ScheduledUpdateGroupAction
      -- ^ A list of scheduled actions designed to update an Auto Scaling
      -- group.
    } deriving (Eq, Show, Generic)

instance IsXML DescribeScheduledActionsResult where
    xmlPickler = withNS autoScalingNS

-- | DescribeTagsResult
--
-- <http://docs.aws.amazon.com/AutoScaling/latest/APIReference/API_DescribeTagsResult.html>
data DescribeTagsResult = DescribeTagsResult
    { dtrNextToken :: Maybe ByteString
      -- ^ A string used to mark the start of the next batch of returned results.
    , dtrTags      :: Members Tag
      -- ^ The list of tags.
    } deriving (Eq, Show, Generic)

instance IsXML DescribeTagsResult where
    xmlPickler = withNS autoScalingNS

-- | The TerminationPolicyTypes data type.
--
-- <http://docs.aws.amazon.com/AutoScaling/latest/APIReference/API_DescribeTerminationPolicyTypesResult.html>
data DescribeTerminationPolicyTypesResult = DescribeTerminationPolicyTypesResult
    { dtptrTerminationPolicyTypes :: [ByteString]
      -- ^ Termination policies supported by Auto Scaling. They are:
      -- OldestInstance, OldestLaunchConfiguration, NewestInstance,
      -- ClosestToNextInstanceHour, Default
    } deriving (Eq, Show, Generic)

instance IsXML DescribeTerminationPolicyTypesResult where
    xmlPickler = withNS autoScalingNS

-- | The Ebs data type.
--
-- <http://docs.aws.amazon.com/AutoScaling/latest/APIReference/API_Ebs.html>
data Ebs = Ebs
    { eSnapshotId :: Maybe ByteString
      -- ^ The snapshot ID.
    , eVolumeSize :: Maybe Integer
      -- ^ The volume size, in gigabytes.
    } deriving (Eq, Show, Generic)

instance IsQuery Ebs

instance IsXML Ebs where
    xmlPickler = withNS autoScalingNS

-- | The EnabledMetric data type.
--
-- <http://docs.aws.amazon.com/AutoScaling/latest/APIReference/API_EnabledMetric.html>
data EnabledMetric = EnabledMetric
    { emGranularity :: Maybe ByteString
      -- ^ The granularity of the enabled metric.
    , emMetric      :: Maybe ByteString
      -- ^ The name of the enabled metric.
    } deriving (Eq, Show, Generic)

instance IsQuery EnabledMetric

instance IsXML EnabledMetric where
    xmlPickler = withNS autoScalingNS

-- | The Filter data type.
--
-- <http://docs.aws.amazon.com/AutoScaling/latest/APIReference/API_Filter.html>
data Filter = Filter
    { fName   :: Maybe ByteString
      -- ^ The name of the filter. Valid Name values are:
      -- "auto-scaling-group", "key", "value", and "propagate-at-launch".
    , fValues :: Maybe ByteString
      -- ^ The value of the filter.
    } deriving (Eq, Show, Generic)

instance IsQuery Filter

instance IsXML Filter where
    xmlPickler = withNS autoScalingNS

-- | The Instance data type.
--
-- <http://docs.aws.amazon.com/AutoScaling/latest/APIReference/API_Instance.html>
data Instance = Instance
    { iAvailabilityZone        :: !ByteString
      -- ^ Availability Zones associated with this instance.
    , iHealthStatus            :: !ByteString
      -- ^ The instance's health status.
    , iInstanceId              :: !ByteString
      -- ^ Specifies the ID of the Amazon EC2 instance.
    , iLaunchConfigurationName :: !ByteString
      -- ^ The launch configuration associated with this instance.
    , iLifecycleState          :: !ByteString
      -- ^ Contains a description of the current lifecycle state.
    } deriving (Eq, Show, Generic)

instance IsQuery Instance

instance IsXML Instance where
    xmlPickler = withNS autoScalingNS

-- | The InstanceMonitoring data type.
--
-- <http://docs.aws.amazon.com/AutoScaling/latest/APIReference/API_InstanceMonitoring.html>
data InstanceMonitoring = InstanceMonitoring
    { imEnabled :: Bool
      -- ^ If True, instance monitoring is enabled.
    } deriving (Eq, Show, Generic)

instance IsQuery InstanceMonitoring

instance IsXML InstanceMonitoring where
    xmlPickler = withNS autoScalingNS

-- | The LaunchConfiguration data type.
--
-- <http://docs.aws.amazon.com/AutoScaling/latest/APIReference/API_LaunchConfiguration.html>
data LaunchConfiguration = LaunchConfiguration
    { lcBlockDeviceMappings     :: Members BlockDeviceMapping
      -- ^ Specifies how block devices are exposed to the instance. Each
      -- mapping is made up of a virtualName and a deviceName.
    , lcCreatedTime             :: !UTCTime
      -- ^ Provides the creation date and time for this launch
      -- configuration.
    , lcEbsOptimized            :: Maybe Bool
      -- ^ Specifies whether the instance is optimized for EBS I/O (true) or
      -- not (false).
    , lcIamInstanceProfile      :: Maybe ByteString
      -- ^ Provides the name or the Amazon Resource Name (ARN) of the
      -- instance profile associated with the IAM role for the instance.
      -- The instance profile contains the IAM role.
    , lcImageId                 :: !ByteString
      -- ^ Provides the unique ID of the Amazon Machine Image (AMI) that was
      -- assigned during registration.
    , lcInstanceMonitoring      :: Maybe InstanceMonitoring
      -- ^ Controls whether instances in this group are launched with
      -- detailed monitoring or not.
    , lcInstanceType            :: !ByteString
      -- ^ Specifies the instance type of the Amazon EC2 instance.
    , lcKernelId                :: Maybe ByteString
      -- ^ Provides the ID of the kernel associated with the Amazon EC2 AMI.
    , lcKeyName                 :: Maybe ByteString
      -- ^ Provides the name of the Amazon EC2 key pair.
    , lcLaunchConfigurationARN  :: Maybe ByteString
      -- ^ The launch configuration's Amazon Resource Name (ARN).
    , lcLaunchConfigurationName :: !ByteString
      -- ^ Specifies the name of the launch configuration.
    , lcRamdiskId               :: Maybe ByteString
      -- ^ Provides ID of the RAM disk associated with the Amazon EC2 AMI.
    , lcSecurityGroups          :: Maybe ByteString
      -- ^ A description of the security groups to associate with the Amazon
      -- EC2 instances.
    , lcSpotPrice               :: Maybe ByteString
      -- ^ Specifies the price to bid when launching Spot Instances.
    , lcUserData                :: Maybe ByteString
      -- ^ The user data available to the launched Amazon EC2 instances.
    } deriving (Eq, Show, Generic)

instance IsQuery LaunchConfiguration

instance IsXML LaunchConfiguration where
    xmlPickler = withNS autoScalingNS

-- | The MetricCollectionType data type.
--
-- <http://docs.aws.amazon.com/AutoScaling/latest/APIReference/API_MetricCollectionType.html>
data MetricCollectionType = MetricCollectionType
    { mctMetric :: ByteString
      -- ^ Type: String
    } deriving (Eq, Show, Generic)

instance IsQuery MetricCollectionType

instance IsXML MetricCollectionType where
    xmlPickler = withNS autoScalingNS

-- | The MetricGranularityType data type.
--
-- <http://docs.aws.amazon.com/AutoScaling/latest/APIReference/API_MetricGranularityType.html>
data MetricGranularityType = MetricGranularityType
    { mgtGranularity :: ByteString
      -- ^ The granularity of a Metric.
    } deriving (Eq, Show, Generic)

instance IsQuery MetricGranularityType

instance IsXML MetricGranularityType where
    xmlPickler = withNS autoScalingNS

-- | The NotificationConfiguration data type.
--
-- <http://docs.aws.amazon.com/AutoScaling/latest/APIReference/API_NotificationConfiguration.html>
data NotificationConfiguration = NotificationConfiguration
    { ncAutoScalingGroupName :: Maybe ByteString
      -- ^ Specifies the Auto Scaling group name.
    , ncNotificationType     :: Maybe ByteString
      -- ^ The types of events for an action to start.
    , ncTopicARN             :: Maybe ByteString
      -- ^ The Amazon Resource Name (ARN) of the Amazon Simple Notification
      -- Service (SNS) topic.
    } deriving (Eq, Show, Generic)

instance IsQuery NotificationConfiguration

instance IsXML NotificationConfiguration where
    xmlPickler = withNS autoScalingNS

-- | There are two primary Auto Scaling process types: Launch and Terminate. The
-- Launch process creates a new Amazon EC2 instance for an Auto Scaling group,
-- and the Terminate process removes an existing Amazon EC2 instance. The
-- remaining Auto Scaling process types relate to specific Auto Scaling
-- features: Important If you suspend Launch or Terminate, all other process
-- types are affected to varying degrees.
--
-- <http://docs.aws.amazon.com/AutoScaling/latest/APIReference/API_ProcessType.html>
data ProcessType = ProcessType
    { ptProcessName :: !ByteString
      -- ^ The name of a process.
    } deriving (Eq, Show, Generic)

instance IsQuery ProcessType

instance IsXML ProcessType where
    xmlPickler = withNS autoScalingNS

-- | The PolicyARNType data type.
--
-- <http://docs.aws.amazon.com/AutoScaling/latest/APIReference/API_PutScalingPolicyResult.html>
data PutScalingPolicyResult = PutScalingPolicyResult
    { psprPolicyARN :: ByteString
      -- ^ A policy's Amazon Resource Name (ARN).
    } deriving (Eq, Show, Generic)

instance IsXML PutScalingPolicyResult where
    xmlPickler = withNS autoScalingNS

-- | The ScalingPolicy data type.
--
-- <http://docs.aws.amazon.com/AutoScaling/latest/APIReference/API_ScalingPolicy.html>
data ScalingPolicy = ScalingPolicy
    { spAdjustmentType       :: Maybe ByteString
      -- ^ Specifies whether the ScalingAdjustment is an absolute number or
      -- a percentage of the current capacity. Valid values are
      -- ChangeInCapacity, ExactCapacity, and PercentChangeInCapacity.
    , spAlarms               :: Members Alarm
      -- ^ A list of CloudWatch Alarms related to the policy.
    , sqAutoScalingGroupName :: Maybe ByteString
      -- ^ The name of the Auto Scaling group associated with this scaling
      -- policy.
    , sqCooldown             :: Maybe Integer
      -- ^ The amount of time, in seconds, after a scaling activity
      -- completes before any further trigger-related scaling activities
      -- can start.
    , sqMinAdjustmentStep    :: Maybe Integer
      -- ^ Changes the DesiredCapacity of the Auto Scaling group by at least
      -- the specified number of instances.
    , sqPolicyARN            :: Maybe ByteString
      -- ^ The Amazon Resource Name (ARN) of the policy.
    , sqPolicyName           :: Maybe ByteString
      -- ^ The name of the scaling policy.
    , sqScalingAdjustment    :: Maybe Integer
      -- ^ The number associated with the specified adjustment type. A
      -- positive value adds to the current capacity and a negative value
      -- removes from the current capacity.
    } deriving (Eq, Show, Generic)

instance IsQuery ScalingPolicy

instance IsXML ScalingPolicy where
    xmlPickler = withNS autoScalingNS

-- | This data type stores information about a scheduled update to an Auto
-- Scaling group.
--
-- <http://docs.aws.amazon.com/AutoScaling/latest/APIReference/API_ScheduledUpdateGroupAction.html>
data ScheduledUpdateGroupAction = ScheduledUpdateGroupAction
    { sugaAutoScalingGroupName :: Maybe ByteString
      -- ^ The name of the Auto Scaling group to be updated.
    , sugaDesiredCapacity      :: Maybe Integer
      -- ^ The number of instances you prefer to maintain in your Auto
      -- Scaling group.
    , sugaEndTime              :: Maybe UTCTime
      -- ^ The time that the action is scheduled to end. This value can be
      -- up to one month in the future.
    , sugaMaxSize              :: Maybe Integer
      -- ^ The maximum size of the Auto Scaling group.
    , sugaMinSize              :: Maybe Integer
      -- ^ The minimum size of the Auto Scaling group.
    , sugaRecurrence           :: Maybe ByteString
      -- ^ The regular schedule that an action occurs.
    , sugaScheduledActionARN   :: Maybe ByteString
      -- ^ The Amazon Resource Name (ARN) of this scheduled action.
    , sugaScheduledActionName  :: Maybe ByteString
      -- ^ The name of this scheduled action.
    , sugaStartTime            :: Maybe UTCTime
      -- ^ The time that the action is scheduled to begin. This value can be
      -- up to one month in the future.
    , sugaTime                 :: Maybe UTCTime
      -- ^ Time is deprecated.
    } deriving (Eq, Show, Generic)

instance IsQuery ScheduledUpdateGroupAction

instance IsXML ScheduledUpdateGroupAction where
    xmlPickler = withNS autoScalingNS

-- | An Auto Scaling process that has been suspended. For more information, see
-- ProcessType.
--
-- <http://docs.aws.amazon.com/AutoScaling/latest/APIReference/API_SuspendedProcess.html>
data SuspendedProcess = SuspendedProcess
    { spProcessName      :: Maybe ByteString
      -- ^ The name of the suspended process.
    , spSuspensionReason :: Maybe ByteString
      -- ^ The reason that the process was suspended.
    } deriving (Eq, Show, Generic)

instance IsQuery SuspendedProcess

instance IsXML SuspendedProcess where
    xmlPickler = withNS autoScalingNS

-- | The tag applied to an Auto Scaling group.
--
-- <http://docs.aws.amazon.com/AutoScaling/latest/APIReference/API_Tag.html>
data Tag = Tag
    { tKey               :: !ByteString
      -- ^ The key of the tag.
    , tPropagateAtLaunch :: Maybe Bool
      -- ^ Specifies whether the new tag will be applied to instances
      -- launched after the tag is created. The same behavior applies to
      -- updates: If you change a tag, the changed tag will be applied to
      -- all instances launched after you made the change.
    , tResourceId        :: Maybe ByteString
      -- ^ The name of the Auto Scaling group.
    , tResourceType      :: Maybe ByteString
      -- ^ The kind of resource to which the tag is applied. Currently, Auto
      -- Scaling supports the auto-scaling-group resource type.
    , tValue             :: Maybe ByteString
      -- ^ The value of the tag.
    } deriving (Eq, Show, Generic)

instance IsQuery Tag

instance IsXML Tag where
    xmlPickler = withNS autoScalingNS

-- | The output for the TerminateInstanceInAutoScalingGroup action.
--
-- <http://docs.aws.amazon.com/AutoScaling/latest/APIReference/API_TerminateInstanceInAutoScalingGroupResult.html>
data TerminateInstanceInAutoScalingGroupResult = TerminateInstanceInAutoScalingGroupResult
    { tiiasgrActivity :: Maybe Activity
      -- ^ A scaling Activity.
    } deriving (Eq, Show, Generic)

instance IsXML TerminateInstanceInAutoScalingGroupResult where
    xmlPickler = withNS autoScalingNS
