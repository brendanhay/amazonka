data ResponseMetadata = ResponseMetadata
    { RequestId :: !Text
    } deriving (Show)

data ErrorType = Receiver | Sender

data Error = Error
    { Type :: !ErrorType
    , Code :: !Text
    , Message :: !Text
    , Detail :: !Text
    } deriving (Show)

data ErrorResponse = ErrorResponse
    { Error :: ![Error]
    , RequestId :: !Text
    } deriving (Show)

data DescribeAutoScalingGroups = DescribeAutoScalingGroups
    { AutoScalingGroupNames :: !(Maybe AutoScalingGroupNames)
    , NextToken :: !(Maybe Text)
    , MaxRecords :: !(Maybe Integer)
    } deriving (Show)

data DescribeAutoScalingGroupsResponse = DescribeAutoScalingGroupsResponse
    { DescribeAutoScalingGroupsResult :: !DescribeAutoScalingGroupsResult
    , ResponseMetadata :: !ResponseMetadata
    } deriving (Show)

data DescribeAutoScalingGroupsResult = DescribeAutoScalingGroupsResult
    { AutoScalingGroups :: !AutoScalingGroups
    , NextToken :: !(Maybe Text)
    } deriving (Show)

data EnableMetricsCollection = EnableMetricsCollection
    { AutoScalingGroupName :: !ResourceName
    , Metrics :: !(Maybe Metrics)
    , Granularity :: !Text
    } deriving (Show)

data EnableMetricsCollectionResponse = EnableMetricsCollectionResponse
    { ResponseMetadata :: !ResponseMetadata
    } deriving (Show)

data ResumeProcesses = ResumeProcesses
    { AutoScalingGroupName :: !ResourceName
    , ScalingProcesses :: !(Maybe ProcessNames)
    } deriving (Show)

data ResumeProcessesResponse = ResumeProcessesResponse
    { ResponseMetadata :: !ResponseMetadata
    } deriving (Show)

data DeleteLaunchConfiguration = DeleteLaunchConfiguration
    { LaunchConfigurationName :: !ResourceName
    } deriving (Show)

data DeleteLaunchConfigurationResponse = DeleteLaunchConfigurationResponse
    { ResponseMetadata :: !ResponseMetadata
    } deriving (Show)

data DescribePolicies = DescribePolicies
    { AutoScalingGroupName :: !(Maybe ResourceName)
    , PolicyNames :: !(Maybe PolicyNames)
    , NextToken :: !(Maybe Text)
    , MaxRecords :: !(Maybe Integer)
    } deriving (Show)

data DescribePoliciesResponse = DescribePoliciesResponse
    { DescribePoliciesResult :: !DescribePoliciesResult
    , ResponseMetadata :: !ResponseMetadata
    } deriving (Show)

data DescribePoliciesResult = DescribePoliciesResult
    { ScalingPolicies :: !(Maybe ScalingPolicies)
    , NextToken :: !(Maybe Text)
    } deriving (Show)

data DescribeScalingProcessTypes = DescribeScalingProcessTypes
    deriving (Show)

data DescribeScalingProcessTypesResponse = DescribeScalingProcessTypesResponse
    { DescribeScalingProcessTypesResult :: !DescribeScalingProcessTypesResult
    , ResponseMetadata :: !ResponseMetadata
    } deriving (Show)

data DescribeScalingProcessTypesResult = DescribeScalingProcessTypesResult
    { Processes :: !(Maybe Processes)
    } deriving (Show)

data CreateAutoScalingGroup = CreateAutoScalingGroup
    { AutoScalingGroupName :: !Text
    , LaunchConfigurationName :: !ResourceName
    , MinSize :: !Integer
    , MaxSize :: !Integer
    , DesiredCapacity :: !(Maybe Integer)
    , DefaultCooldown :: !(Maybe Integer)
    , AvailabilityZones :: !(Maybe AvailabilityZones)
    , LoadBalancerNames :: !(Maybe LoadBalancerNames)
    , HealthCheckType :: !(Maybe Text)
    , HealthCheckGracePeriod :: !(Maybe Integer)
    , PlacementGroup :: !(Maybe Text)
    , VPCZoneIdentifier :: !(Maybe Text)
    , TerminationPolicies :: !(Maybe TerminationPolicies)
    , Tags :: !(Maybe Tags)
    } deriving (Show)

data CreateAutoScalingGroupResponse = CreateAutoScalingGroupResponse
    { ResponseMetadata :: !ResponseMetadata
    } deriving (Show)

data DescribeScalingActivities = DescribeScalingActivities
    { ActivityIds :: !(Maybe ActivityIds)
    , AutoScalingGroupName :: !(Maybe ResourceName)
    , MaxRecords :: !(Maybe Integer)
    , NextToken :: !(Maybe Text)
    } deriving (Show)

data DescribeScalingActivitiesResponse = DescribeScalingActivitiesResponse
    { DescribeScalingActivitiesResult :: !DescribeScalingActivitiesResult
    , ResponseMetadata :: !ResponseMetadata
    } deriving (Show)

data DescribeScalingActivitiesResult = DescribeScalingActivitiesResult
    { Activities :: !Activities
    , NextToken :: !(Maybe Text)
    } deriving (Show)

data DescribeNotificationConfigurations = DescribeNotificationConfigurations
    { AutoScalingGroupNames :: !(Maybe AutoScalingGroupNames)
    , NextToken :: !(Maybe Text)
    , MaxRecords :: !(Maybe Integer)
    } deriving (Show)

data DescribeNotificationConfigurationsResponse = DescribeNotificationConfigurationsResponse
    { DescribeNotificationConfigurationsResult :: !DescribeNotificationConfigurationsResult
    , ResponseMetadata :: !ResponseMetadata
    } deriving (Show)

data DescribeNotificationConfigurationsResult = DescribeNotificationConfigurationsResult
    { NotificationConfigurations :: !NotificationConfigurations
    , NextToken :: !(Maybe Text)
    } deriving (Show)

data DescribeTerminationPolicyTypes = DescribeTerminationPolicyTypes
    deriving (Show)

data DescribeTerminationPolicyTypesResponse = DescribeTerminationPolicyTypesResponse
    { DescribeTerminationPolicyTypesResult :: !DescribeTerminationPolicyTypesResult
    , ResponseMetadata :: !ResponseMetadata
    } deriving (Show)

data DescribeTerminationPolicyTypesResult = DescribeTerminationPolicyTypesResult
    { TerminationPolicyTypes :: !(Maybe TerminationPolicies)
    } deriving (Show)

data DescribeTags = DescribeTags
    { Filters :: !(Maybe Filters)
    , NextToken :: !(Maybe Text)
    , MaxRecords :: !(Maybe Integer)
    } deriving (Show)

data DescribeTagsResponse = DescribeTagsResponse
    { DescribeTagsResult :: !DescribeTagsResult
    , ResponseMetadata :: !ResponseMetadata
    } deriving (Show)

data DescribeTagsResult = DescribeTagsResult
    { Tags :: !(Maybe TagDescriptionList)
    , NextToken :: !(Maybe Text)
    } deriving (Show)

data ExecutePolicy = ExecutePolicy
    { AutoScalingGroupName :: !(Maybe ResourceName)
    , PolicyName :: !ResourceName
    , HonorCooldown :: !(Maybe Boolean)
    } deriving (Show)

data ExecutePolicyResponse = ExecutePolicyResponse
    { ResponseMetadata :: !ResponseMetadata
    } deriving (Show)

data DeleteTags = DeleteTags
    { Tags :: !Tags
    } deriving (Show)

data DeleteTagsResponse = DeleteTagsResponse
    { ResponseMetadata :: !ResponseMetadata
    } deriving (Show)

data PutScalingPolicy = PutScalingPolicy
    { AutoScalingGroupName :: !ResourceName
    , PolicyName :: !Text
    , ScalingAdjustment :: !Integer
    , AdjustmentType :: !Text
    , Cooldown :: !(Maybe Integer)
    , MinAdjustmentStep :: !(Maybe Integer)
    } deriving (Show)

data PutScalingPolicyResponse = PutScalingPolicyResponse
    { PutScalingPolicyResult :: !PutScalingPolicyResult
    , ResponseMetadata :: !ResponseMetadata
    } deriving (Show)

data PutScalingPolicyResult = PutScalingPolicyResult
    { PolicyARN :: !(Maybe ResourceName)
    } deriving (Show)

data PutNotificationConfiguration = PutNotificationConfiguration
    { AutoScalingGroupName :: !ResourceName
    , TopicARN :: !ResourceName
    , NotificationTypes :: !AutoScalingNotificationTypes
    } deriving (Show)

data PutNotificationConfigurationResponse = PutNotificationConfigurationResponse
    { ResponseMetadata :: !ResponseMetadata
    } deriving (Show)

data DeletePolicy = DeletePolicy
    { AutoScalingGroupName :: !(Maybe ResourceName)
    , PolicyName :: !ResourceName
    } deriving (Show)

data DeletePolicyResponse = DeletePolicyResponse
    { ResponseMetadata :: !ResponseMetadata
    } deriving (Show)

data DeleteNotificationConfiguration = DeleteNotificationConfiguration
    { AutoScalingGroupName :: !ResourceName
    , TopicARN :: !ResourceName
    } deriving (Show)

data DeleteNotificationConfigurationResponse = DeleteNotificationConfigurationResponse
    { ResponseMetadata :: !ResponseMetadata
    } deriving (Show)

data DeleteScheduledAction = DeleteScheduledAction
    { AutoScalingGroupName :: !(Maybe ResourceName)
    , ScheduledActionName :: !ResourceName
    } deriving (Show)

data DeleteScheduledActionResponse = DeleteScheduledActionResponse
    { ResponseMetadata :: !ResponseMetadata
    } deriving (Show)

data SetInstanceHealth = SetInstanceHealth
    { InstanceId :: !Text
    , HealthStatus :: !Text
    , ShouldRespectGracePeriod :: !(Maybe Boolean)
    } deriving (Show)

data SetInstanceHealthResponse = SetInstanceHealthResponse
    { ResponseMetadata :: !ResponseMetadata
    } deriving (Show)

data DescribeAutoScalingNotificationTypes = DescribeAutoScalingNotificationTypes
    deriving (Show)

data DescribeAutoScalingNotificationTypesResponse = DescribeAutoScalingNotificationTypesResponse
    { DescribeAutoScalingNotificationTypesResult :: !DescribeAutoScalingNotificationTypesResult
    , ResponseMetadata :: !ResponseMetadata
    } deriving (Show)

data DescribeAutoScalingNotificationTypesResult = DescribeAutoScalingNotificationTypesResult
    { AutoScalingNotificationTypes :: !(Maybe AutoScalingNotificationTypes)
    } deriving (Show)

data CreateOrUpdateTags = CreateOrUpdateTags
    { Tags :: !Tags
    } deriving (Show)

data CreateOrUpdateTagsResponse = CreateOrUpdateTagsResponse
    { ResponseMetadata :: !ResponseMetadata
    } deriving (Show)

data SuspendProcesses = SuspendProcesses
    { AutoScalingGroupName :: !ResourceName
    , ScalingProcesses :: !(Maybe ProcessNames)
    } deriving (Show)

data SuspendProcessesResponse = SuspendProcessesResponse
    { ResponseMetadata :: !ResponseMetadata
    } deriving (Show)

data DescribeAutoScalingInstances = DescribeAutoScalingInstances
    { InstanceIds :: !(Maybe InstanceIds)
    , MaxRecords :: !(Maybe Integer)
    , NextToken :: !(Maybe Text)
    } deriving (Show)

data DescribeAutoScalingInstancesResponse = DescribeAutoScalingInstancesResponse
    { DescribeAutoScalingInstancesResult :: !DescribeAutoScalingInstancesResult
    , ResponseMetadata :: !ResponseMetadata
    } deriving (Show)

data DescribeAutoScalingInstancesResult = DescribeAutoScalingInstancesResult
    { AutoScalingInstances :: !(Maybe AutoScalingInstances)
    , NextToken :: !(Maybe Text)
    } deriving (Show)

data CreateLaunchConfiguration = CreateLaunchConfiguration
    { LaunchConfigurationName :: !Text
    , ImageId :: !Text
    , KeyName :: !(Maybe Text)
    , SecurityGroups :: !(Maybe SecurityGroups)
    , UserData :: !(Maybe Text)
    , InstanceType :: !Text
    , KernelId :: !(Maybe Text)
    , RamdiskId :: !(Maybe Text)
    , BlockDeviceMappings :: !(Maybe BlockDeviceMappings)
    , InstanceMonitoring :: !(Maybe InstanceMonitoring)
    , SpotPrice :: !(Maybe Text)
    , IamInstanceProfile :: !(Maybe Text)
    , EbsOptimized :: !(Maybe Boolean)
    } deriving (Show)

data CreateLaunchConfigurationResponse = CreateLaunchConfigurationResponse
    { ResponseMetadata :: !ResponseMetadata
    } deriving (Show)

data DeleteAutoScalingGroup = DeleteAutoScalingGroup
    { AutoScalingGroupName :: !ResourceName
    , ForceDelete :: !(Maybe Boolean)
    } deriving (Show)

data DeleteAutoScalingGroupResponse = DeleteAutoScalingGroupResponse
    { ResponseMetadata :: !ResponseMetadata
    } deriving (Show)

data DisableMetricsCollection = DisableMetricsCollection
    { AutoScalingGroupName :: !ResourceName
    , Metrics :: !(Maybe Metrics)
    } deriving (Show)

data DisableMetricsCollectionResponse = DisableMetricsCollectionResponse
    { ResponseMetadata :: !ResponseMetadata
    } deriving (Show)

data UpdateAutoScalingGroup = UpdateAutoScalingGroup
    { AutoScalingGroupName :: !ResourceName
    , LaunchConfigurationName :: !(Maybe ResourceName)
    , MinSize :: !(Maybe Integer)
    , MaxSize :: !(Maybe Integer)
    , DesiredCapacity :: !(Maybe Integer)
    , DefaultCooldown :: !(Maybe Integer)
    , AvailabilityZones :: !(Maybe AvailabilityZones)
    , HealthCheckType :: !(Maybe Text)
    , HealthCheckGracePeriod :: !(Maybe Integer)
    , PlacementGroup :: !(Maybe Text)
    , VPCZoneIdentifier :: !(Maybe Text)
    , TerminationPolicies :: !(Maybe TerminationPolicies)
    } deriving (Show)

data UpdateAutoScalingGroupResponse = UpdateAutoScalingGroupResponse
    { ResponseMetadata :: !ResponseMetadata
    } deriving (Show)

data DescribeLaunchConfigurations = DescribeLaunchConfigurations
    { LaunchConfigurationNames :: !(Maybe LaunchConfigurationNames)
    , NextToken :: !(Maybe Text)
    , MaxRecords :: !(Maybe Integer)
    } deriving (Show)

data DescribeLaunchConfigurationsResponse = DescribeLaunchConfigurationsResponse
    { DescribeLaunchConfigurationsResult :: !DescribeLaunchConfigurationsResult
    , ResponseMetadata :: !ResponseMetadata
    } deriving (Show)

data DescribeLaunchConfigurationsResult = DescribeLaunchConfigurationsResult
    { LaunchConfigurations :: !LaunchConfigurations
    , NextToken :: !(Maybe Text)
    } deriving (Show)

data DescribeAdjustmentTypes = DescribeAdjustmentTypes
    deriving (Show)

data DescribeAdjustmentTypesResponse = DescribeAdjustmentTypesResponse
    { DescribeAdjustmentTypesResult :: !DescribeAdjustmentTypesResult
    , ResponseMetadata :: !ResponseMetadata
    } deriving (Show)

data DescribeAdjustmentTypesResult = DescribeAdjustmentTypesResult
    { AdjustmentTypes :: !(Maybe AdjustmentTypes)
    } deriving (Show)

data DescribeScheduledActions = DescribeScheduledActions
    { AutoScalingGroupName :: !(Maybe ResourceName)
    , ScheduledActionNames :: !(Maybe ScheduledActionNames)
    , StartTime :: !(Maybe DateTime)
    , EndTime :: !(Maybe DateTime)
    , NextToken :: !(Maybe Text)
    , MaxRecords :: !(Maybe Integer)
    } deriving (Show)

data DescribeScheduledActionsResponse = DescribeScheduledActionsResponse
    { DescribeScheduledActionsResult :: !DescribeScheduledActionsResult
    , ResponseMetadata :: !ResponseMetadata
    } deriving (Show)

data DescribeScheduledActionsResult = DescribeScheduledActionsResult
    { ScheduledUpdateGroupActions :: !(Maybe ScheduledUpdateGroupActions)
    , NextToken :: !(Maybe Text)
    } deriving (Show)

data PutScheduledUpdateGroupAction = PutScheduledUpdateGroupAction
    { AutoScalingGroupName :: !ResourceName
    , ScheduledActionName :: !Text
    , Time :: !(Maybe DateTime)
    , StartTime :: !(Maybe DateTime)
    , EndTime :: !(Maybe DateTime)
    , Recurrence :: !(Maybe Text)
    , MinSize :: !(Maybe Integer)
    , MaxSize :: !(Maybe Integer)
    , DesiredCapacity :: !(Maybe Integer)
    } deriving (Show)

data PutScheduledUpdateGroupActionResponse = PutScheduledUpdateGroupActionResponse
    { ResponseMetadata :: !ResponseMetadata
    } deriving (Show)

data DescribeMetricCollectionTypes = DescribeMetricCollectionTypes
    deriving (Show)

data DescribeMetricCollectionTypesResponse = DescribeMetricCollectionTypesResponse
    { DescribeMetricCollectionTypesResult :: !DescribeMetricCollectionTypesResult
    , ResponseMetadata :: !ResponseMetadata
    } deriving (Show)

data DescribeMetricCollectionTypesResult = DescribeMetricCollectionTypesResult
    { Metrics :: !(Maybe MetricCollectionTypes)
    , Granularities :: !(Maybe MetricGranularityTypes)
    } deriving (Show)

data SetDesiredCapacity = SetDesiredCapacity
    { AutoScalingGroupName :: !ResourceName
    , DesiredCapacity :: !Integer
    , HonorCooldown :: !(Maybe Boolean)
    } deriving (Show)

data SetDesiredCapacityResponse = SetDesiredCapacityResponse
    { ResponseMetadata :: !ResponseMetadata
    } deriving (Show)

data TerminateInstanceInAutoScalingGroup = TerminateInstanceInAutoScalingGroup
    { InstanceId :: !Text
    , ShouldDecrementDesiredCapacity :: !Boolean
    } deriving (Show)

data TerminateInstanceInAutoScalingGroupResponse = TerminateInstanceInAutoScalingGroupResponse
    { TerminateInstanceInAutoScalingGroupResult :: !TerminateInstanceInAutoScalingGroupResult
    , ResponseMetadata :: !ResponseMetadata
    } deriving (Show)

data TerminateInstanceInAutoScalingGroupResult = TerminateInstanceInAutoScalingGroupResult
    { Activity :: !(Maybe Activity)
    } deriving (Show)

--
-- Types
--

data MetricGranularityType = MetricGranularityType
    { Granularity :: !(Maybe Text)
    } deriving (Show)

data SuspendedProcesses = SuspendedProcesses
    { Member :: ![SuspendedProcess]
    } deriving (Show)

data LoadBalancerNames = LoadBalancerNames
    { Member :: ![Text]
    } deriving (Show)

data Tag = Tag
    { ResourceId :: !(Maybe Text)
    , ResourceType :: !(Maybe Text)
    , Key :: !Text
    , Value :: !(Maybe Text)
    , PropagateAtLaunch :: !(Maybe Boolean)
    } deriving (Show)

data PolicyNames = PolicyNames
    { Member :: ![ResourceName]
    } deriving (Show)

data TagDescription = TagDescription
    { ResourceId :: !(Maybe Text)
    , ResourceType :: !(Maybe Text)
    , Key :: !(Maybe Text)
    , Value :: !(Maybe Text)
    , PropagateAtLaunch :: !(Maybe Boolean)
    } deriving (Show)

data NotificationConfiguration = NotificationConfiguration
    { AutoScalingGroupName :: !(Maybe ResourceName)
    , TopicARN :: !(Maybe ResourceName)
    , NotificationType :: !(Maybe Text)
    } deriving (Show)

data AdjustmentTypes = AdjustmentTypes
    { Member :: ![AdjustmentType]
    } deriving (Show)

data ScheduledActionNames = ScheduledActionNames
    { Member :: ![ResourceName]
    } deriving (Show)

data NotificationConfigurations = NotificationConfigurations
    { Member :: ![NotificationConfiguration]
    } deriving (Show)

data LaunchConfigurations = LaunchConfigurations
    { Member :: ![LaunchConfiguration]
    } deriving (Show)

data AutoScalingGroup = AutoScalingGroup
    { AutoScalingGroupName :: !Text
    , AutoScalingGroupARN :: !(Maybe ResourceName)
    , LaunchConfigurationName :: !Text
    , MinSize :: !Integer
    , MaxSize :: !Integer
    , DesiredCapacity :: !Integer
    , DefaultCooldown :: !Integer
    , AvailabilityZones :: !AvailabilityZones
    , LoadBalancerNames :: !(Maybe LoadBalancerNames)
    , HealthCheckType :: !Text
    , HealthCheckGracePeriod :: !(Maybe Integer)
    , Instances :: !(Maybe Instances)
    , CreatedTime :: !DateTime
    , SuspendedProcesses :: !(Maybe SuspendedProcesses)
    , PlacementGroup :: !(Maybe Text)
    , VPCZoneIdentifier :: !(Maybe Text)
    , EnabledMetrics :: !(Maybe EnabledMetrics)
    , Status :: !(Maybe Text)
    , Tags :: !(Maybe TagDescriptionList)
    , TerminationPolicies :: !(Maybe TerminationPolicies)
    } deriving (Show)

data Ebs = Ebs
    { SnapshotId :: !(Maybe Text)
    , VolumeSize :: !(Maybe Integer)
    } deriving (Show)

data LifecycleState
    = Pending
    | Quarantined
    | InService
    | Terminating
    | Terminated

data BlockDeviceMapping = BlockDeviceMapping
    { VirtualName :: !(Maybe Text)
    , DeviceName :: !Text
    , Ebs :: !(Maybe Ebs)
    } deriving (Show)

data Instance = Instance
    { InstanceId :: !Text
    , AvailabilityZone :: !Text
    , LifecycleState :: !LifecycleState
    , HealthStatus :: !Text
    , LaunchConfigurationName :: !Text
    } deriving (Show)

data Instances = Instances
    { Member :: ![Instance]
    } deriving (Show)

data ScheduledUpdateGroupActions = ScheduledUpdateGroupActions
    { Member :: ![ScheduledUpdateGroupAction]
    } deriving (Show)

data Tags = Tags
    { Member :: ![Tag]
    } deriving (Show)

data MetricCollectionTypes = MetricCollectionTypes
    { Member :: ![MetricCollectionType]
    } deriving (Show)

data AutoScalingInstanceDetails = AutoScalingInstanceDetails
    { InstanceId :: !Text
    , AutoScalingGroupName :: !Text
    , AvailabilityZone :: !Text
    , LifecycleState :: !Text
    , HealthStatus :: !Text
    , LaunchConfigurationName :: !Text
    } deriving (Show)

data EnabledMetrics = EnabledMetrics
    { Member :: ![EnabledMetric]
    } deriving (Show)

data Metrics = Metrics
    { Member :: ![Text]
    } deriving (Show)

data Activity = Activity
    { ActivityId :: !Text
    , AutoScalingGroupName :: !Text
    , Description :: !(Maybe Text)
    , Cause :: !Text
    , StartTime :: !DateTime
    , EndTime :: !(Maybe DateTime)
    , StatusCode :: !ScalingActivityStatusCode
    , StatusMessage :: !(Maybe Text)
    , Progress :: !(Maybe Integer)
    , Details :: !(Maybe Text)
    } deriving (Show)

data ScalingPolicy = ScalingPolicy
    { AutoScalingGroupName :: !(Maybe Text)
    , PolicyName :: !(Maybe Text)
    , ScalingAdjustment :: !(Maybe Integer)
    , AdjustmentType :: !(Maybe Text)
    , Cooldown :: !(Maybe Integer)
    , PolicyARN :: !(Maybe ResourceName)
    , Alarms :: !(Maybe Alarms)
    , MinAdjustmentStep :: !(Maybe Integer)
    } deriving (Show)

data Values = Values
    { Member :: ![Text]
    } deriving (Show)

data AutoScalingInstances = AutoScalingInstances
    { Member :: ![AutoScalingInstanceDetails]
    } deriving (Show)

data SecurityGroups = SecurityGroups
    { Member :: ![Text]
    } deriving (Show)

data ProcessType = ProcessType
    { ProcessName :: !Text
    } deriving (Show)

data Processes = Processes
    { Member :: ![ProcessType]
    } deriving (Show)

data AdjustmentType = AdjustmentType
    { AdjustmentType :: !(Maybe Text)
    } deriving (Show)

data AvailabilityZones = AvailabilityZones
    { Member :: ![Text]
    } deriving (Show)

data Alarm = Alarm
    { AlarmName :: !(Maybe Text)
    , AlarmARN :: !(Maybe ResourceName)
    } deriving (Show)

data InstanceIds = InstanceIds
    { Member :: ![Text]
    } deriving (Show)

data LaunchConfigurationNames = LaunchConfigurationNames
    { Member :: ![ResourceName]
    } deriving (Show)

data TagDescriptionList = TagDescriptionList
    { Member :: ![TagDescription]
    } deriving (Show)

data MetricCollectionType = MetricCollectionType
    { Metric :: !(Maybe Text)
    } deriving (Show)

data ScheduledUpdateGroupAction = ScheduledUpdateGroupAction
    { AutoScalingGroupName :: !(Maybe Text)
    , ScheduledActionName :: !(Maybe Text)
    , ScheduledActionARN :: !(Maybe ResourceName)
    , Time :: !(Maybe DateTime)
    , StartTime :: !(Maybe DateTime)
    , EndTime :: !(Maybe DateTime)
    , Recurrence :: !(Maybe Text)
    , MinSize :: !(Maybe Integer)
    , MaxSize :: !(Maybe Integer)
    , DesiredCapacity :: !(Maybe Integer)
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
    { Member :: ![MetricGranularityType]
    } deriving (Show)

data SuspendedProcess = SuspendedProcess
    { ProcessName :: !(Maybe Text)
    , SuspensionReason :: !(Maybe Text)
    } deriving (Show)

data BlockDeviceMappings = BlockDeviceMappings
    { Member :: ![BlockDeviceMapping]
    } deriving (Show)

data AutoScalingGroupNames = AutoScalingGroupNames
    { Member :: ![ResourceName]
    } deriving (Show)

newtype ResourceName = ResourceName Text
    deriving (Show, IsText)

data Filters = Filters
    { Member :: ![Filter]
    } deriving (Show)

data Filter = Filter
    { Name :: !(Maybe Text)
    , Values :: !(Maybe Values)
    } deriving (Show)

data InstanceMonitoring = InstanceMonitoring
    { Enabled :: !(Maybe Boolean)
    } deriving (Show)

data AutoScalingGroups = AutoScalingGroups
    { Member :: ![AutoScalingGroup]
    } deriving (Show)

data Alarms = Alarms
    { Member :: ![Alarm]
    } deriving (Show)

data AutoScalingNotificationTypes = AutoScalingNotificationTypes
    { Member :: ![Text]
    } deriving (Show)

data ScalingPolicies = ScalingPolicies
    { Member :: ![ScalingPolicy]
    } deriving (Show)

data Activities = Activities
    { Member :: ![Activity]
    } deriving (Show)

data ActivityIds = ActivityIds
    { Member :: ![Text]
    } deriving (Show)

data TerminationPolicies = TerminationPolicies
    { Member :: ![Text]
    } deriving (Show)

data ProcessNames = ProcessNames
    { Member :: ![Text]
    } deriving (Show)

data LaunchConfiguration = LaunchConfiguration
    { LaunchConfigurationName :: !Text
    , LaunchConfigurationARN :: !(Maybe ResourceName)
    , ImageId :: !Text
    , KeyName :: !(Maybe Text)
    , SecurityGroups :: !(Maybe SecurityGroups)
    , UserData :: !(Maybe Text)
    , InstanceType :: !Text
    , KernelId :: !(Maybe Text)
    , RamdiskId :: !(Maybe Text)
    , BlockDeviceMappings :: !(Maybe BlockDeviceMappings)
    , InstanceMonitoring :: !(Maybe InstanceMonitoring)
    , SpotPrice :: !(Maybe Text)
    , IamInstanceProfile :: !(Maybe Text)
    , CreatedTime :: !DateTime
    , EbsOptimized :: !(Maybe Boolean)
    } deriving (Show)

data EnabledMetric = EnabledMetric
    { Metric :: !(Maybe Text)
    , Granularity :: !(Maybe Text)
    } deriving (Show)
