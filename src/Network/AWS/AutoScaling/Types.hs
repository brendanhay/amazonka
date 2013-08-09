-- |
-- Module : Network.AWS.AutoScaling.Types
-- Copyright : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.AutoScaling.Types where

import Data.Text
import Network.AWS.Internal

data ResponseMetadata = ResponseMetadata
    { rmRequestId :: !Text
    } deriving (Show)

data ErrorType = Receiver | Sender

data Error = Error
    { eType :: !ErrorType
    , eCode :: !Text
    , eMessage :: !Text
    , eDetail :: !Text
    } deriving (Show)

data ErrorResponse = ErrorResponse
    { erError :: ![Error]
    , erRequestId :: !Text
    } deriving (Show)

data DescribeAutoScalingGroupsResponse = DescribeAutoScalingGroupsResponse
    { dasgrDescribeAutoScalingGroupsResult :: !DescribeAutoScalingGroupsResult
    , dasgrResponseMetadata :: !ResponseMetadata
    } deriving (Show)

data DescribeAutoScalingGroupsResult = DescribeAutoScalingGroupsResult
    { dasgrAutoScalingGroups :: !AutoScalingGroups
    , dasgrNextToken :: !(Maybe Text)
    } deriving (Show)

data EnableMetricsCollectionResponse = EnableMetricsCollectionResponse
    { emcrResponseMetadata :: !ResponseMetadata
    } deriving (Show)

data ResumeProcessesResponse = ResumeProcessesResponse
    { rprResponseMetadata :: !ResponseMetadata
    } deriving (Show)

data DeleteLaunchConfigurationResponse = DeleteLaunchConfigurationResponse
    { dlcrResponseMetadata :: !ResponseMetadata
    } deriving (Show)

data DescribePoliciesResponse = DescribePoliciesResponse
    { dprDescribePoliciesResult :: !DescribePoliciesResult
    , dprResponseMetadata :: !ResponseMetadata
    } deriving (Show)

data DescribePoliciesResult = DescribePoliciesResult
    { dprScalingPolicies :: !(Maybe ScalingPolicies)
    , dprNextToken :: !(Maybe Text)
    } deriving (Show)

data DescribeScalingProcessTypesResponse = DescribeScalingProcessTypesResponse
    { dsptrDescribeScalingProcessTypesResult :: !DescribeScalingProcessTypesResult
    , dsptrResponseMetadata :: !ResponseMetadata
    } deriving (Show)

data DescribeScalingProcessTypesResult = DescribeScalingProcessTypesResult
    { dsptrProcesses :: !(Maybe Processes)
    } deriving (Show)

data CreateAutoScalingGroupResponse = CreateAutoScalingGroupResponse
    { casgrResponseMetadata :: !ResponseMetadata
    } deriving (Show)

data DescribeScalingActivitiesResponse = DescribeScalingActivitiesResponse
    { dsarDescribeScalingActivitiesResult :: !DescribeScalingActivitiesResult
    , dsarResponseMetadata :: !ResponseMetadata
    } deriving (Show)

data DescribeScalingActivitiesResult = DescribeScalingActivitiesResult
    { dsarActivities :: !Activities
    , dsarNextToken :: !(Maybe Text)
    } deriving (Show)

data DescribeNotificationConfigurationsResponse = DescribeNotificationConfigurationsResponse
    { dncrDescribeNotificationConfigurationsResult :: !DescribeNotificationConfigurationsResult
    , dncrResponseMetadata :: !ResponseMetadata
    } deriving (Show)

data DescribeNotificationConfigurationsResult = DescribeNotificationConfigurationsResult
    { dncrNotificationConfigurations :: !NotificationConfigurations
    , dncrNextToken :: !(Maybe Text)
    } deriving (Show)

data DescribeTerminationPolicyTypesResponse = DescribeTerminationPolicyTypesResponse
    { dtptrDescribeTerminationPolicyTypesResult :: !DescribeTerminationPolicyTypesResult
    , dtptrResponseMetadata :: !ResponseMetadata
    } deriving (Show)

data DescribeTerminationPolicyTypesResult = DescribeTerminationPolicyTypesResult
    { dtptrTerminationPolicyTypes :: !(Maybe TerminationPolicies)
    } deriving (Show)

data DescribeTagsResponse = DescribeTagsResponse
    { dtrDescribeTagsResult :: !DescribeTagsResult
    , dtrResponseMetadata :: !ResponseMetadata
    } deriving (Show)

data DescribeTagsResult = DescribeTagsResult
    { dtrTags :: !(Maybe TagDescriptionList)
    , dtrNextToken :: !(Maybe Text)
    } deriving (Show)

data ExecutePolicyResponse = ExecutePolicyResponse
    { eprResponseMetadata :: !ResponseMetadata
    } deriving (Show)

data DeleteTagsResponse = DeleteTagsResponse
    { dtrResponseMetadata :: !ResponseMetadata
    } deriving (Show)

data PutScalingPolicyResponse = PutScalingPolicyResponse
    { psprPutScalingPolicyResult :: !PutScalingPolicyResult
    , psprResponseMetadata :: !ResponseMetadata
    } deriving (Show)

data PutScalingPolicyResult = PutScalingPolicyResult
    { psprPolicyARN :: !(Maybe ResourceName)
    } deriving (Show)

data PutNotificationConfigurationResponse = PutNotificationConfigurationResponse
    { pncrResponseMetadata :: !ResponseMetadata
    } deriving (Show)

data DeletePolicyResponse = DeletePolicyResponse
    { dprResponseMetadata :: !ResponseMetadata
    } deriving (Show)

data DeleteNotificationConfigurationResponse = DeleteNotificationConfigurationResponse
    { dncrResponseMetadata :: !ResponseMetadata
    } deriving (Show)

data DeleteScheduledActionResponse = DeleteScheduledActionResponse
    { dsarResponseMetadata :: !ResponseMetadata
    } deriving (Show)

data SetInstanceHealthResponse = SetInstanceHealthResponse
    { sihrResponseMetadata :: !ResponseMetadata
    } deriving (Show)

data DescribeAutoScalingNotificationTypes = DescribeAutoScalingNotificationTypes
    deriving (Show)

data DescribeAutoScalingNotificationTypesResponse = DescribeAutoScalingNotificationTypesResponse
    { dasntrDescribeAutoScalingNotificationTypesResult :: !DescribeAutoScalingNotificationTypesResult
    , dasntrResponseMetadata :: !ResponseMetadata
    } deriving (Show)

data DescribeAutoScalingNotificationTypesResult = DescribeAutoScalingNotificationTypesResult
    { dasntrAutoScalingNotificationTypes :: !(Maybe AutoScalingNotificationTypes)
    } deriving (Show)

data CreateOrUpdateTagsResponse = CreateOrUpdateTagsResponse
    { coutrResponseMetadata :: !ResponseMetadata
    } deriving (Show)

data SuspendProcessesResponse = SuspendProcessesResponse
    { sprResponseMetadata :: !ResponseMetadata
    } deriving (Show)

data DescribeAutoScalingInstancesResponse = DescribeAutoScalingInstancesResponse
    { dasirDescribeAutoScalingInstancesResult :: !DescribeAutoScalingInstancesResult
    , dasirResponseMetadata :: !ResponseMetadata
    } deriving (Show)

data DescribeAutoScalingInstancesResult = DescribeAutoScalingInstancesResult
    { dasirAutoScalingInstances :: !(Maybe AutoScalingInstances)
    , dasirNextToken :: !(Maybe Text)
    } deriving (Show)

data CreateLaunchConfigurationResponse = CreateLaunchConfigurationResponse
    { clcrResponseMetadata :: !ResponseMetadata
    } deriving (Show)

data DeleteAutoScalingGroupResponse = DeleteAutoScalingGroupResponse
    { dasgrResponseMetadata :: !ResponseMetadata
    } deriving (Show)

data DisableMetricsCollectionResponse = DisableMetricsCollectionResponse
    { dmcrResponseMetadata :: !ResponseMetadata
    } deriving (Show)

data UpdateAutoScalingGroupResponse = UpdateAutoScalingGroupResponse
    { uasgrResponseMetadata :: !ResponseMetadata
    } deriving (Show)

data DescribeLaunchConfigurationsResponse = DescribeLaunchConfigurationsResponse
    { dlcrDescribeLaunchConfigurationsResult :: !DescribeLaunchConfigurationsResult
    , dlcrResponseMetadata :: !ResponseMetadata
    } deriving (Show)

data DescribeLaunchConfigurationsResult = DescribeLaunchConfigurationsResult
    { dlcrLaunchConfigurations :: !LaunchConfigurations
    , dlcrNextToken :: !(Maybe Text)
    } deriving (Show)

data DescribeAdjustmentTypesResponse = DescribeAdjustmentTypesResponse
    { datrDescribeAdjustmentTypesResult :: !DescribeAdjustmentTypesResult
    , datrResponseMetadata :: !ResponseMetadata
    } deriving (Show)

data DescribeAdjustmentTypesResult = DescribeAdjustmentTypesResult
    { datrAdjustmentTypes :: !(Maybe AdjustmentTypes)
    } deriving (Show)

data DescribeScheduledActionsResponse = DescribeScheduledActionsResponse
    { dsarDescribeScheduledActionsResult :: !DescribeScheduledActionsResult
    , dsarResponseMetadata :: !ResponseMetadata
    } deriving (Show)

data DescribeScheduledActionsResult = DescribeScheduledActionsResult
    { dsarScheduledUpdateGroupActions :: !(Maybe ScheduledUpdateGroupActions)
    , dsarNextToken :: !(Maybe Text)
    } deriving (Show)

data PutScheduledUpdateGroupActionResponse = PutScheduledUpdateGroupActionResponse
    { psugarResponseMetadata :: !ResponseMetadata
    } deriving (Show)

data DescribeMetricCollectionTypesResponse = DescribeMetricCollectionTypesResponse
    { dmctrDescribeMetricCollectionTypesResult :: !DescribeMetricCollectionTypesResult
    , dmctrResponseMetadata :: !ResponseMetadata
    } deriving (Show)

data DescribeMetricCollectionTypesResult = DescribeMetricCollectionTypesResult
    { dmctrMetrics :: !(Maybe MetricCollectionTypes)
    , dmctrGranularities :: !(Maybe MetricGranularityTypes)
    } deriving (Show)

data SetDesiredCapacityResponse = SetDesiredCapacityResponse
    { sdcrResponseMetadata :: !ResponseMetadata
    } deriving (Show)

data TerminateInstanceInAutoScalingGroupResponse = TerminateInstanceInAutoScalingGroupResponse
    { tiiasgrTerminateInstanceInAutoScalingGroupResult :: !TerminateInstanceInAutoScalingGroupResult
    , tiiasgrResponseMetadata :: !ResponseMetadata
    } deriving (Show)

data TerminateInstanceInAutoScalingGroupResult = TerminateInstanceInAutoScalingGroupResult
    { tiiasgrActivity :: !(Maybe Activity)
    } deriving (Show)

--
-- Types
--

data MetricGranularityType = MetricGranularityType
    { mgtGranularity :: !(Maybe Text)
    } deriving (Show)

data SuspendedProcesses = SuspendedProcesses
    { spMember :: ![SuspendedProcess]
    } deriving (Show)

data LoadBalancerNames = LoadBalancerNames
    { lbnMember :: ![Text]
    } deriving (Show)

data Tag = Tag
    { tResourceId :: !(Maybe Text)
    , tResourceType :: !(Maybe Text)
    , tKey :: !Text
    , tValue :: !(Maybe Text)
    , tPropagateAtLaunch :: !(Maybe Boolean)
    } deriving (Show)

data PolicyNames = PolicyNames
    { pnMember :: ![ResourceName]
    } deriving (Show)

data TagDescription = TagDescription
    { tdResourceId :: !(Maybe Text)
    , tdResourceType :: !(Maybe Text)
    , tdKey :: !(Maybe Text)
    , tdValue :: !(Maybe Text)
    , tdPropagateAtLaunch :: !(Maybe Boolean)
    } deriving (Show)

data NotificationConfiguration = NotificationConfiguration
    { ncAutoScalingGroupName :: !(Maybe ResourceName)
    , ncTopicARN :: !(Maybe ResourceName)
    , ncNotificationType :: !(Maybe Text)
    } deriving (Show)

data AdjustmentTypes = AdjustmentTypes
    { atMember :: ![AdjustmentType]
    } deriving (Show)

data ScheduledActionNames = ScheduledActionNames
    { sanMember :: ![ResourceName]
    } deriving (Show)

data NotificationConfigurations = NotificationConfigurations
    { ncMember :: ![NotificationConfiguration]
    } deriving (Show)

data LaunchConfigurations = LaunchConfigurations
    { lcMember :: ![LaunchConfiguration]
    } deriving (Show)

data AutoScalingGroup = AutoScalingGroup
    { asgAutoScalingGroupName :: !Text
    , asgAutoScalingGroupARN :: !(Maybe ResourceName)
    , asgLaunchConfigurationName :: !Text
    , asgMinSize :: !Integer
    , asgMaxSize :: !Integer
    , asgDesiredCapacity :: !Integer
    , asgDefaultCooldown :: !Integer
    , asgAvailabilityZones :: !AvailabilityZones
    , asgLoadBalancerNames :: !(Maybe LoadBalancerNames)
    , asgHealthCheckType :: !Text
    , asgHealthCheckGracePeriod :: !(Maybe Integer)
    , asgInstances :: !(Maybe Instances)
    , asgCreatedTime :: !DateTime
    , asgSuspendedProcesses :: !(Maybe SuspendedProcesses)
    , asgPlacementGroup :: !(Maybe Text)
    , asgVPCZoneIdentifier :: !(Maybe Text)
    , asgEnabledMetrics :: !(Maybe EnabledMetrics)
    , asgStatus :: !(Maybe Text)
    , asgTags :: !(Maybe TagDescriptionList)
    , asgTerminationPolicies :: !(Maybe TerminationPolicies)
    } deriving (Show)

data Ebs = Ebs
    { eSnapshotId :: !(Maybe Text)
    , eVolumeSize :: !(Maybe Integer)
    } deriving (Show)

data LifecycleState
    = Pending
    | Quarantined
    | InService
    | Terminating
    | Terminated

data BlockDeviceMapping = BlockDeviceMapping
    { bdmVirtualName :: !(Maybe Text)
    , bdmDeviceName :: !Text
    , bdmEbs :: !(Maybe Ebs)
    } deriving (Show)

data Instance = Instance
    { iInstanceId :: !Text
    , iAvailabilityZone :: !Text
    , iLifecycleState :: !LifecycleState
    , iHealthStatus :: !Text
    , iLaunchConfigurationName :: !Text
    } deriving (Show)

data Instances = Instances
    { iMember :: ![Instance]
    } deriving (Show)

data ScheduledUpdateGroupActions = ScheduledUpdateGroupActions
    { sugaMember :: ![ScheduledUpdateGroupAction]
    } deriving (Show)

data Tags = Tags
    { tMember :: ![Tag]
    } deriving (Show)

data MetricCollectionTypes = MetricCollectionTypes
    { mctMember :: ![MetricCollectionType]
    } deriving (Show)

data AutoScalingInstanceDetails = AutoScalingInstanceDetails
    { asidInstanceId :: !Text
    , asidAutoScalingGroupName :: !Text
    , asidAvailabilityZone :: !Text
    , asidLifecycleState :: !Text
    , asidHealthStatus :: !Text
    , asidLaunchConfigurationName :: !Text
    } deriving (Show)

data EnabledMetrics = EnabledMetrics
    { emMember :: ![EnabledMetric]
    } deriving (Show)

data Metrics = Metrics
    { mMember :: ![Text]
    } deriving (Show)

data Activity = Activity
    { aActivityId :: !Text
    , aAutoScalingGroupName :: !Text
    , aDescription :: !(Maybe Text)
    , aCause :: !Text
    , aStartTime :: !DateTime
    , aEndTime :: !(Maybe DateTime)
    , aStatusCode :: !ScalingActivityStatusCode
    , aStatusMessage :: !(Maybe Text)
    , aProgress :: !(Maybe Integer)
    , aDetails :: !(Maybe Text)
    } deriving (Show)

data ScalingPolicy = ScalingPolicy
    { spAutoScalingGroupName :: !(Maybe Text)
    , spPolicyName :: !(Maybe Text)
    , spScalingAdjustment :: !(Maybe Integer)
    , spAdjustmentType :: !(Maybe Text)
    , spCooldown :: !(Maybe Integer)
    , spPolicyARN :: !(Maybe ResourceName)
    , spAlarms :: !(Maybe Alarms)
    , spMinAdjustmentStep :: !(Maybe Integer)
    } deriving (Show)

data Values = Values
    { vMember :: ![Text]
    } deriving (Show)

data AutoScalingInstances = AutoScalingInstances
    { asiMember :: ![AutoScalingInstanceDetails]
    } deriving (Show)

data SecurityGroups = SecurityGroups
    { sgMember :: ![Text]
    } deriving (Show)

data ProcessType = ProcessType
    { ptProcessName :: !Text
    } deriving (Show)

data Processes = Processes
    { pMember :: ![ProcessType]
    } deriving (Show)

data AdjustmentType = AdjustmentType
    { atAdjustmentType :: !(Maybe Text)
    } deriving (Show)

data AvailabilityZones = AvailabilityZones
    { azMember :: ![Text]
    } deriving (Show)

data Alarm = Alarm
    { aAlarmName :: !(Maybe Text)
    , aAlarmARN :: !(Maybe ResourceName)
    } deriving (Show)

data InstanceIds = InstanceIds
    { iiMember :: ![Text]
    } deriving (Show)

data LaunchConfigurationNames = LaunchConfigurationNames
    { lcnMember :: ![ResourceName]
    } deriving (Show)

data TagDescriptionList = TagDescriptionList
    { tdlMember :: ![TagDescription]
    } deriving (Show)

data MetricCollectionType = MetricCollectionType
    { mctMetric :: !(Maybe Text)
    } deriving (Show)

data ScheduledUpdateGroupAction = ScheduledUpdateGroupAction
    { sugaAutoScalingGroupName :: !(Maybe Text)
    , sugaScheduledActionName :: !(Maybe Text)
    , sugaScheduledActionARN :: !(Maybe ResourceName)
    , sugaTime :: !(Maybe DateTime)
    , sugaStartTime :: !(Maybe DateTime)
    , sugaEndTime :: !(Maybe DateTime)
    , sugaRecurrence :: !(Maybe Text)
    , sugaMinSize :: !(Maybe Integer)
    , sugaMaxSize :: !(Maybe Integer)
    , sugaDesiredCapacity :: !(Maybe Integer)
    } deriving (Show)

data ScalingActivityStatusCode
    = WaitingForSpotInstanceRequestId
    | WaitingForSpotInstanceId
    | WaitingForInstanceId
    | PreInService
    | InProgress
    | Successful
    | Failed
    | Cancelled

data MetricGranularityTypes = MetricGranularityTypes
    { mgtMember :: ![MetricGranularityType]
    } deriving (Show)

data SuspendedProcess = SuspendedProcess
    { spProcessName :: !(Maybe Text)
    , spSuspensionReason :: !(Maybe Text)
    } deriving (Show)

data BlockDeviceMappings = BlockDeviceMappings
    { bdmMember :: ![BlockDeviceMapping]
    } deriving (Show)

data AutoScalingGroupNames = AutoScalingGroupNames
    { asgnMember :: ![ResourceName]
    } deriving (Show)

newtype ResourceName = ResourceName Text
    deriving (Show, IsText)

data Filters = Filters
    { fMember :: ![Filter]
    } deriving (Show)

data Filter = Filter
    { fName :: !(Maybe Text)
    , fValues :: !(Maybe Values)
    } deriving (Show)

data InstanceMonitoring = InstanceMonitoring
    { imEnabled :: !(Maybe Boolean)
    } deriving (Show)

data AutoScalingGroups = AutoScalingGroups
    { asgMember :: ![AutoScalingGroup]
    } deriving (Show)

data Alarms = Alarms
    { aMember :: ![Alarm]
    } deriving (Show)

data ScalingPolicies = ScalingPolicies
    { spMember :: ![ScalingPolicy]
    } deriving (Show)

data Activities = Activities
    { aMember :: ![Activity]
    } deriving (Show)

data ActivityIds = ActivityIds
    { aiMember :: ![Text]
    } deriving (Show)

data TerminationPolicies = TerminationPolicies
    { tpMember :: ![Text]
    } deriving (Show)

data ProcessNames = ProcessNames
    { pnMember :: ![Text]
    } deriving (Show)

data LaunchConfiguration = LaunchConfiguration
    { lcLaunchConfigurationName :: !Text
    , lcLaunchConfigurationARN :: !(Maybe ResourceName)
    , lcImageId :: !Text
    , lcKeyName :: !(Maybe Text)
    , lcSecurityGroups :: !(Maybe SecurityGroups)
    , lcUserData :: !(Maybe Text)
    , lcInstanceType :: !Text
    , lcKernelId :: !(Maybe Text)
    , lcRamdiskId :: !(Maybe Text)
    , lcBlockDeviceMappings :: !(Maybe BlockDeviceMappings)
    , lcInstanceMonitoring :: !(Maybe InstanceMonitoring)
    , lcSpotPrice :: !(Maybe Text)
    , lcIamInstanceProfile :: !(Maybe Text)
    , lcCreatedTime :: !DateTime
    , lcEbsOptimized :: !(Maybe Boolean)
    } deriving (Show)

data EnabledMetric = EnabledMetric
    { emMetric :: !(Maybe Text)
    , emGranularity :: !(Maybe Text)
    } deriving (Show)
