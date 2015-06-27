{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.AutoScaling.Types
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

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
    , actProgress
    , actStatusMessage
    , actDetails
    , actEndTime
    , actDescription
    , actActivityId
    , actAutoScalingGroupName
    , actCause
    , actStartTime
    , actStatusCode

    -- * AdjustmentType
    , AdjustmentType
    , adjustmentType
    , atAdjustmentType

    -- * Alarm
    , Alarm
    , alarm
    , alaAlarmName
    , alaAlarmARN

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
    , filValues
    , filName

    -- * Instance
    , Instance
    , instance'
    , insInstanceId
    , insAvailabilityZone
    , insLifecycleState
    , insHealthStatus
    , insLaunchConfigurationName

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
    , scaMinAdjustmentStep
    , scaPolicyName
    , scaAdjustmentType
    , scaScalingAdjustment
    , scaAutoScalingGroupName
    , scaCooldown
    , scaPolicyARN
    , scaAlarms

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
            , _svcTimeout = 80000000
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

data LifecycleState
    = PendingWait
    | Terminating
    | TerminatingWait
    | Pending
    | Standby
    | EnteringStandby
    | InService
    | Detached
    | Detaching
    | Quarantined
    | PendingProceed
    | Terminated
    | TerminatingProceed
    deriving (Eq,Ord,Read,Show,Enum,Generic)

instance FromText LifecycleState where
    parser = takeLowerText >>= \case
        "Detached" -> pure Detached
        "Detaching" -> pure Detaching
        "EnteringStandby" -> pure EnteringStandby
        "InService" -> pure InService
        "Pending" -> pure Pending
        "Pending:Proceed" -> pure PendingProceed
        "Pending:Wait" -> pure PendingWait
        "Quarantined" -> pure Quarantined
        "Standby" -> pure Standby
        "Terminated" -> pure Terminated
        "Terminating" -> pure Terminating
        "Terminating:Proceed" -> pure TerminatingProceed
        "Terminating:Wait" -> pure TerminatingWait
        e -> fail ("Failure parsing LifecycleState from " ++ show e)

instance ToText LifecycleState where
    toText = \case
        Detached -> "Detached"
        Detaching -> "Detaching"
        EnteringStandby -> "EnteringStandby"
        InService -> "InService"
        Pending -> "Pending"
        PendingProceed -> "Pending:Proceed"
        PendingWait -> "Pending:Wait"
        Quarantined -> "Quarantined"
        Standby -> "Standby"
        Terminated -> "Terminated"
        Terminating -> "Terminating"
        TerminatingProceed -> "Terminating:Proceed"
        TerminatingWait -> "Terminating:Wait"

instance Hashable LifecycleState
instance ToQuery LifecycleState
instance ToHeader LifecycleState

instance FromXML LifecycleState where
    parseXML = parseXMLText "LifecycleState"

data ScalingActivityStatusCode
    = WaitingForSpotInstanceId
    | WaitingForSpotInstanceRequestId
    | WaitingForInstanceId
    | Successful
    | InProgress
    | PreInService
    | WaitingForELBConnectionDraining
    | MidLifecycleAction
    | Cancelled
    | Failed
    deriving (Eq,Ord,Read,Show,Enum,Generic)

instance FromText ScalingActivityStatusCode where
    parser = takeLowerText >>= \case
        "Cancelled" -> pure Cancelled
        "Failed" -> pure Failed
        "InProgress" -> pure InProgress
        "MidLifecycleAction" -> pure MidLifecycleAction
        "PreInService" -> pure PreInService
        "Successful" -> pure Successful
        "WaitingForELBConnectionDraining" -> pure WaitingForELBConnectionDraining
        "WaitingForInstanceId" -> pure WaitingForInstanceId
        "WaitingForSpotInstanceId" -> pure WaitingForSpotInstanceId
        "WaitingForSpotInstanceRequestId" -> pure WaitingForSpotInstanceRequestId
        e -> fail ("Failure parsing ScalingActivityStatusCode from " ++ show e)

instance ToText ScalingActivityStatusCode where
    toText = \case
        Cancelled -> "Cancelled"
        Failed -> "Failed"
        InProgress -> "InProgress"
        MidLifecycleAction -> "MidLifecycleAction"
        PreInService -> "PreInService"
        Successful -> "Successful"
        WaitingForELBConnectionDraining -> "WaitingForELBConnectionDraining"
        WaitingForInstanceId -> "WaitingForInstanceId"
        WaitingForSpotInstanceId -> "WaitingForSpotInstanceId"
        WaitingForSpotInstanceRequestId -> "WaitingForSpotInstanceRequestId"

instance Hashable ScalingActivityStatusCode
instance ToQuery ScalingActivityStatusCode
instance ToHeader ScalingActivityStatusCode

instance FromXML ScalingActivityStatusCode where
    parseXML = parseXMLText "ScalingActivityStatusCode"

-- | Describes scaling activity, which is a long-running process that
-- represents a change to your Auto Scaling group, such as changing its
-- size or replacing an instance.
--
-- /See:/ 'activity' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'actProgress'
--
-- * 'actStatusMessage'
--
-- * 'actDetails'
--
-- * 'actEndTime'
--
-- * 'actDescription'
--
-- * 'actActivityId'
--
-- * 'actAutoScalingGroupName'
--
-- * 'actCause'
--
-- * 'actStartTime'
--
-- * 'actStatusCode'
data Activity = Activity'
    { _actProgress             :: !(Maybe Int)
    , _actStatusMessage        :: !(Maybe Text)
    , _actDetails              :: !(Maybe Text)
    , _actEndTime              :: !(Maybe ISO8601)
    , _actDescription          :: !(Maybe Text)
    , _actActivityId           :: !Text
    , _actAutoScalingGroupName :: !Text
    , _actCause                :: !Text
    , _actStartTime            :: !ISO8601
    , _actStatusCode           :: !ScalingActivityStatusCode
    } deriving (Eq,Read,Show)

-- | 'Activity' smart constructor.
activity :: Text -> Text -> Text -> UTCTime -> ScalingActivityStatusCode -> Activity
activity pActivityId pAutoScalingGroupName pCause pStartTime pStatusCode =
    Activity'
    { _actProgress = Nothing
    , _actStatusMessage = Nothing
    , _actDetails = Nothing
    , _actEndTime = Nothing
    , _actDescription = Nothing
    , _actActivityId = pActivityId
    , _actAutoScalingGroupName = pAutoScalingGroupName
    , _actCause = pCause
    , _actStartTime = _Time # pStartTime
    , _actStatusCode = pStatusCode
    }

-- | A value between 0 and 100 that indicates the progress of the activity.
actProgress :: Lens' Activity (Maybe Int)
actProgress = lens _actProgress (\ s a -> s{_actProgress = a});

-- | A friendly, more verbose description of the activity status.
actStatusMessage :: Lens' Activity (Maybe Text)
actStatusMessage = lens _actStatusMessage (\ s a -> s{_actStatusMessage = a});

-- | The details about the activity.
actDetails :: Lens' Activity (Maybe Text)
actDetails = lens _actDetails (\ s a -> s{_actDetails = a});

-- | The end time of the activity.
actEndTime :: Lens' Activity (Maybe UTCTime)
actEndTime = lens _actEndTime (\ s a -> s{_actEndTime = a}) . mapping _Time;

-- | A friendly, more verbose description of the activity.
actDescription :: Lens' Activity (Maybe Text)
actDescription = lens _actDescription (\ s a -> s{_actDescription = a});

-- | The ID of the activity.
actActivityId :: Lens' Activity Text
actActivityId = lens _actActivityId (\ s a -> s{_actActivityId = a});

-- | The name of the Auto Scaling group.
actAutoScalingGroupName :: Lens' Activity Text
actAutoScalingGroupName = lens _actAutoScalingGroupName (\ s a -> s{_actAutoScalingGroupName = a});

-- | The reason the activity began.
actCause :: Lens' Activity Text
actCause = lens _actCause (\ s a -> s{_actCause = a});

-- | The start time of the activity.
actStartTime :: Lens' Activity UTCTime
actStartTime = lens _actStartTime (\ s a -> s{_actStartTime = a}) . _Time;

-- | The current status of the activity.
actStatusCode :: Lens' Activity ScalingActivityStatusCode
actStatusCode = lens _actStatusCode (\ s a -> s{_actStatusCode = a});

instance FromXML Activity where
        parseXML x
          = Activity' <$>
              (x .@? "Progress") <*> (x .@? "StatusMessage") <*>
                (x .@? "Details")
                <*> (x .@? "EndTime")
                <*> (x .@? "Description")
                <*> (x .@ "ActivityId")
                <*> (x .@ "AutoScalingGroupName")
                <*> (x .@ "Cause")
                <*> (x .@ "StartTime")
                <*> (x .@ "StatusCode")

-- | Describes a policy adjustment type.
--
-- For more information, see
-- <http://docs.aws.amazon.com/AutoScaling/latest/DeveloperGuide/as-scale-based-on-demand.html Dynamic Scaling>
-- in the /Auto Scaling Developer Guide/.
--
-- /See:/ 'adjustmentType' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'atAdjustmentType'
newtype AdjustmentType = AdjustmentType'
    { _atAdjustmentType :: Maybe Text
    } deriving (Eq,Read,Show)

-- | 'AdjustmentType' smart constructor.
adjustmentType :: AdjustmentType
adjustmentType =
    AdjustmentType'
    { _atAdjustmentType = Nothing
    }

-- | The policy adjustment type. The valid values are @ChangeInCapacity@,
-- @ExactCapacity@, and @PercentChangeInCapacity@.
atAdjustmentType :: Lens' AdjustmentType (Maybe Text)
atAdjustmentType = lens _atAdjustmentType (\ s a -> s{_atAdjustmentType = a});

instance FromXML AdjustmentType where
        parseXML x
          = AdjustmentType' <$> (x .@? "AdjustmentType")

-- | Describes an alarm.
--
-- /See:/ 'alarm' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'alaAlarmName'
--
-- * 'alaAlarmARN'
data Alarm = Alarm'
    { _alaAlarmName :: !(Maybe Text)
    , _alaAlarmARN  :: !(Maybe Text)
    } deriving (Eq,Read,Show)

-- | 'Alarm' smart constructor.
alarm :: Alarm
alarm =
    Alarm'
    { _alaAlarmName = Nothing
    , _alaAlarmARN = Nothing
    }

-- | The name of the alarm.
alaAlarmName :: Lens' Alarm (Maybe Text)
alaAlarmName = lens _alaAlarmName (\ s a -> s{_alaAlarmName = a});

-- | The Amazon Resource Name (ARN) of the alarm.
alaAlarmARN :: Lens' Alarm (Maybe Text)
alaAlarmARN = lens _alaAlarmARN (\ s a -> s{_alaAlarmARN = a});

instance FromXML Alarm where
        parseXML x
          = Alarm' <$>
              (x .@? "AlarmName") <*> (x .@? "AlarmARN")

-- | Describes an Auto Scaling group.
--
-- /See:/ 'autoScalingGroup' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'asgStatus'
--
-- * 'asgTerminationPolicies'
--
-- * 'asgHealthCheckGracePeriod'
--
-- * 'asgVPCZoneIdentifier'
--
-- * 'asgEnabledMetrics'
--
-- * 'asgInstances'
--
-- * 'asgAutoScalingGroupARN'
--
-- * 'asgSuspendedProcesses'
--
-- * 'asgPlacementGroup'
--
-- * 'asgLoadBalancerNames'
--
-- * 'asgTags'
--
-- * 'asgAutoScalingGroupName'
--
-- * 'asgLaunchConfigurationName'
--
-- * 'asgMinSize'
--
-- * 'asgMaxSize'
--
-- * 'asgDesiredCapacity'
--
-- * 'asgDefaultCooldown'
--
-- * 'asgAvailabilityZones'
--
-- * 'asgHealthCheckType'
--
-- * 'asgCreatedTime'
data AutoScalingGroup = AutoScalingGroup'
    { _asgStatus                  :: !(Maybe Text)
    , _asgTerminationPolicies     :: !(Maybe [Text])
    , _asgHealthCheckGracePeriod  :: !(Maybe Int)
    , _asgVPCZoneIdentifier       :: !(Maybe Text)
    , _asgEnabledMetrics          :: !(Maybe [EnabledMetric])
    , _asgInstances               :: !(Maybe [Instance])
    , _asgAutoScalingGroupARN     :: !(Maybe Text)
    , _asgSuspendedProcesses      :: !(Maybe [SuspendedProcess])
    , _asgPlacementGroup          :: !(Maybe Text)
    , _asgLoadBalancerNames       :: !(Maybe [Text])
    , _asgTags                    :: !(Maybe [TagDescription])
    , _asgAutoScalingGroupName    :: !Text
    , _asgLaunchConfigurationName :: !Text
    , _asgMinSize                 :: !Int
    , _asgMaxSize                 :: !Int
    , _asgDesiredCapacity         :: !Int
    , _asgDefaultCooldown         :: !Int
    , _asgAvailabilityZones       :: !(List1 Text)
    , _asgHealthCheckType         :: !Text
    , _asgCreatedTime             :: !ISO8601
    } deriving (Eq,Read,Show)

-- | 'AutoScalingGroup' smart constructor.
autoScalingGroup :: Text -> Text -> Int -> Int -> Int -> Int -> NonEmpty Text -> Text -> UTCTime -> AutoScalingGroup
autoScalingGroup pAutoScalingGroupName pLaunchConfigurationName pMinSize pMaxSize pDesiredCapacity pDefaultCooldown pAvailabilityZones pHealthCheckType pCreatedTime =
    AutoScalingGroup'
    { _asgStatus = Nothing
    , _asgTerminationPolicies = Nothing
    , _asgHealthCheckGracePeriod = Nothing
    , _asgVPCZoneIdentifier = Nothing
    , _asgEnabledMetrics = Nothing
    , _asgInstances = Nothing
    , _asgAutoScalingGroupARN = Nothing
    , _asgSuspendedProcesses = Nothing
    , _asgPlacementGroup = Nothing
    , _asgLoadBalancerNames = Nothing
    , _asgTags = Nothing
    , _asgAutoScalingGroupName = pAutoScalingGroupName
    , _asgLaunchConfigurationName = pLaunchConfigurationName
    , _asgMinSize = pMinSize
    , _asgMaxSize = pMaxSize
    , _asgDesiredCapacity = pDesiredCapacity
    , _asgDefaultCooldown = pDefaultCooldown
    , _asgAvailabilityZones = _List1 # pAvailabilityZones
    , _asgHealthCheckType = pHealthCheckType
    , _asgCreatedTime = _Time # pCreatedTime
    }

-- | The current state of the group when DeleteAutoScalingGroup is in
-- progress.
asgStatus :: Lens' AutoScalingGroup (Maybe Text)
asgStatus = lens _asgStatus (\ s a -> s{_asgStatus = a});

-- | The termination policies for the group.
asgTerminationPolicies :: Lens' AutoScalingGroup [Text]
asgTerminationPolicies = lens _asgTerminationPolicies (\ s a -> s{_asgTerminationPolicies = a}) . _Default;

-- | The amount of time that Auto Scaling waits before checking an
-- instance\'s health status. The grace period begins when an instance
-- comes into service.
asgHealthCheckGracePeriod :: Lens' AutoScalingGroup (Maybe Int)
asgHealthCheckGracePeriod = lens _asgHealthCheckGracePeriod (\ s a -> s{_asgHealthCheckGracePeriod = a});

-- | One or more subnet IDs, if applicable, separated by commas.
--
-- If you specify @VPCZoneIdentifier@ and @AvailabilityZones@, ensure that
-- the Availability Zones of the subnets match the values for
-- @AvailabilityZones@.
asgVPCZoneIdentifier :: Lens' AutoScalingGroup (Maybe Text)
asgVPCZoneIdentifier = lens _asgVPCZoneIdentifier (\ s a -> s{_asgVPCZoneIdentifier = a});

-- | The metrics enabled for the group.
asgEnabledMetrics :: Lens' AutoScalingGroup [EnabledMetric]
asgEnabledMetrics = lens _asgEnabledMetrics (\ s a -> s{_asgEnabledMetrics = a}) . _Default;

-- | The EC2 instances associated with the group.
asgInstances :: Lens' AutoScalingGroup [Instance]
asgInstances = lens _asgInstances (\ s a -> s{_asgInstances = a}) . _Default;

-- | The Amazon Resource Name (ARN) of the group.
asgAutoScalingGroupARN :: Lens' AutoScalingGroup (Maybe Text)
asgAutoScalingGroupARN = lens _asgAutoScalingGroupARN (\ s a -> s{_asgAutoScalingGroupARN = a});

-- | The suspended processes associated with the group.
asgSuspendedProcesses :: Lens' AutoScalingGroup [SuspendedProcess]
asgSuspendedProcesses = lens _asgSuspendedProcesses (\ s a -> s{_asgSuspendedProcesses = a}) . _Default;

-- | The name of the placement group into which you\'ll launch your
-- instances, if any. For more information, see
-- <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/placement-groups.html Placement Groups>.
asgPlacementGroup :: Lens' AutoScalingGroup (Maybe Text)
asgPlacementGroup = lens _asgPlacementGroup (\ s a -> s{_asgPlacementGroup = a});

-- | One or more load balancers associated with the group.
asgLoadBalancerNames :: Lens' AutoScalingGroup [Text]
asgLoadBalancerNames = lens _asgLoadBalancerNames (\ s a -> s{_asgLoadBalancerNames = a}) . _Default;

-- | The tags for the group.
asgTags :: Lens' AutoScalingGroup [TagDescription]
asgTags = lens _asgTags (\ s a -> s{_asgTags = a}) . _Default;

-- | The name of the group.
asgAutoScalingGroupName :: Lens' AutoScalingGroup Text
asgAutoScalingGroupName = lens _asgAutoScalingGroupName (\ s a -> s{_asgAutoScalingGroupName = a});

-- | The name of the associated launch configuration.
asgLaunchConfigurationName :: Lens' AutoScalingGroup Text
asgLaunchConfigurationName = lens _asgLaunchConfigurationName (\ s a -> s{_asgLaunchConfigurationName = a});

-- | The minimum size of the group.
asgMinSize :: Lens' AutoScalingGroup Int
asgMinSize = lens _asgMinSize (\ s a -> s{_asgMinSize = a});

-- | The maximum size of the group.
asgMaxSize :: Lens' AutoScalingGroup Int
asgMaxSize = lens _asgMaxSize (\ s a -> s{_asgMaxSize = a});

-- | The desired size of the group.
asgDesiredCapacity :: Lens' AutoScalingGroup Int
asgDesiredCapacity = lens _asgDesiredCapacity (\ s a -> s{_asgDesiredCapacity = a});

-- | The number of seconds after a scaling activity completes before any
-- further scaling activities can start.
asgDefaultCooldown :: Lens' AutoScalingGroup Int
asgDefaultCooldown = lens _asgDefaultCooldown (\ s a -> s{_asgDefaultCooldown = a});

-- | One or more Availability Zones for the group.
asgAvailabilityZones :: Lens' AutoScalingGroup (NonEmpty Text)
asgAvailabilityZones = lens _asgAvailabilityZones (\ s a -> s{_asgAvailabilityZones = a}) . _List1;

-- | The service of interest for the health status check, which can be either
-- @EC2@ for Amazon EC2 or @ELB@ for Elastic Load Balancing.
asgHealthCheckType :: Lens' AutoScalingGroup Text
asgHealthCheckType = lens _asgHealthCheckType (\ s a -> s{_asgHealthCheckType = a});

-- | The date and time the group was created.
asgCreatedTime :: Lens' AutoScalingGroup UTCTime
asgCreatedTime = lens _asgCreatedTime (\ s a -> s{_asgCreatedTime = a}) . _Time;

instance FromXML AutoScalingGroup where
        parseXML x
          = AutoScalingGroup' <$>
              (x .@? "Status") <*>
                (x .@? "TerminationPolicies" .!@ mempty >>=
                   may (parseXMLList "member"))
                <*> (x .@? "HealthCheckGracePeriod")
                <*> (x .@? "VPCZoneIdentifier")
                <*>
                (x .@? "EnabledMetrics" .!@ mempty >>=
                   may (parseXMLList "member"))
                <*>
                (x .@? "Instances" .!@ mempty >>=
                   may (parseXMLList "member"))
                <*> (x .@? "AutoScalingGroupARN")
                <*>
                (x .@? "SuspendedProcesses" .!@ mempty >>=
                   may (parseXMLList "member"))
                <*> (x .@? "PlacementGroup")
                <*>
                (x .@? "LoadBalancerNames" .!@ mempty >>=
                   may (parseXMLList "member"))
                <*>
                (x .@? "Tags" .!@ mempty >>=
                   may (parseXMLList "member"))
                <*> (x .@ "AutoScalingGroupName")
                <*> (x .@ "LaunchConfigurationName")
                <*> (x .@ "MinSize")
                <*> (x .@ "MaxSize")
                <*> (x .@ "DesiredCapacity")
                <*> (x .@ "DefaultCooldown")
                <*>
                (x .@? "AvailabilityZones" .!@ mempty >>=
                   parseXMLList1 "member")
                <*> (x .@ "HealthCheckType")
                <*> (x .@ "CreatedTime")

-- | Describes an EC2 instance associated with an Auto Scaling group.
--
-- /See:/ 'autoScalingInstanceDetails' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'asidInstanceId'
--
-- * 'asidAutoScalingGroupName'
--
-- * 'asidAvailabilityZone'
--
-- * 'asidLifecycleState'
--
-- * 'asidHealthStatus'
--
-- * 'asidLaunchConfigurationName'
data AutoScalingInstanceDetails = AutoScalingInstanceDetails'
    { _asidInstanceId              :: !Text
    , _asidAutoScalingGroupName    :: !Text
    , _asidAvailabilityZone        :: !Text
    , _asidLifecycleState          :: !Text
    , _asidHealthStatus            :: !Text
    , _asidLaunchConfigurationName :: !Text
    } deriving (Eq,Read,Show)

-- | 'AutoScalingInstanceDetails' smart constructor.
autoScalingInstanceDetails :: Text -> Text -> Text -> Text -> Text -> Text -> AutoScalingInstanceDetails
autoScalingInstanceDetails pInstanceId pAutoScalingGroupName pAvailabilityZone pLifecycleState pHealthStatus pLaunchConfigurationName =
    AutoScalingInstanceDetails'
    { _asidInstanceId = pInstanceId
    , _asidAutoScalingGroupName = pAutoScalingGroupName
    , _asidAvailabilityZone = pAvailabilityZone
    , _asidLifecycleState = pLifecycleState
    , _asidHealthStatus = pHealthStatus
    , _asidLaunchConfigurationName = pLaunchConfigurationName
    }

-- | The ID of the instance.
asidInstanceId :: Lens' AutoScalingInstanceDetails Text
asidInstanceId = lens _asidInstanceId (\ s a -> s{_asidInstanceId = a});

-- | The name of the Auto Scaling group associated with the instance.
asidAutoScalingGroupName :: Lens' AutoScalingInstanceDetails Text
asidAutoScalingGroupName = lens _asidAutoScalingGroupName (\ s a -> s{_asidAutoScalingGroupName = a});

-- | The Availability Zone for the instance.
asidAvailabilityZone :: Lens' AutoScalingInstanceDetails Text
asidAvailabilityZone = lens _asidAvailabilityZone (\ s a -> s{_asidAvailabilityZone = a});

-- | The lifecycle state for the instance. For more information, see
-- <http://docs.aws.amazon.com/AutoScaling/latest/DeveloperGuide/AutoScalingGroupLifecycle.html#AutoScalingStates Auto Scaling Instance States>
-- in the /Auto Scaling Developer Guide/.
asidLifecycleState :: Lens' AutoScalingInstanceDetails Text
asidLifecycleState = lens _asidLifecycleState (\ s a -> s{_asidLifecycleState = a});

-- | The health status of this instance. \"Healthy\" means that the instance
-- is healthy and should remain in service. \"Unhealthy\" means that the
-- instance is unhealthy and Auto Scaling should terminate and replace it.
asidHealthStatus :: Lens' AutoScalingInstanceDetails Text
asidHealthStatus = lens _asidHealthStatus (\ s a -> s{_asidHealthStatus = a});

-- | The launch configuration associated with the instance.
asidLaunchConfigurationName :: Lens' AutoScalingInstanceDetails Text
asidLaunchConfigurationName = lens _asidLaunchConfigurationName (\ s a -> s{_asidLaunchConfigurationName = a});

instance FromXML AutoScalingInstanceDetails where
        parseXML x
          = AutoScalingInstanceDetails' <$>
              (x .@ "InstanceId") <*> (x .@ "AutoScalingGroupName")
                <*> (x .@ "AvailabilityZone")
                <*> (x .@ "LifecycleState")
                <*> (x .@ "HealthStatus")
                <*> (x .@ "LaunchConfigurationName")

-- | Describes a block device mapping.
--
-- /See:/ 'blockDeviceMapping' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'bdmVirtualName'
--
-- * 'bdmNoDevice'
--
-- * 'bdmEBS'
--
-- * 'bdmDeviceName'
data BlockDeviceMapping = BlockDeviceMapping'
    { _bdmVirtualName :: !(Maybe Text)
    , _bdmNoDevice    :: !(Maybe Bool)
    , _bdmEBS         :: !(Maybe EBS)
    , _bdmDeviceName  :: !Text
    } deriving (Eq,Read,Show)

-- | 'BlockDeviceMapping' smart constructor.
blockDeviceMapping :: Text -> BlockDeviceMapping
blockDeviceMapping pDeviceName =
    BlockDeviceMapping'
    { _bdmVirtualName = Nothing
    , _bdmNoDevice = Nothing
    , _bdmEBS = Nothing
    , _bdmDeviceName = pDeviceName
    }

-- | The name of the virtual device, @ephemeral0@ to @ephemeral3@.
bdmVirtualName :: Lens' BlockDeviceMapping (Maybe Text)
bdmVirtualName = lens _bdmVirtualName (\ s a -> s{_bdmVirtualName = a});

-- | Suppresses a device mapping.
--
-- If @NoDevice@ is set to @true@ for the root device, the instance might
-- fail the EC2 health check. Auto Scaling launches a replacement instance
-- if the instance fails the health check.
bdmNoDevice :: Lens' BlockDeviceMapping (Maybe Bool)
bdmNoDevice = lens _bdmNoDevice (\ s a -> s{_bdmNoDevice = a});

-- | The information about the Amazon EBS volume.
bdmEBS :: Lens' BlockDeviceMapping (Maybe EBS)
bdmEBS = lens _bdmEBS (\ s a -> s{_bdmEBS = a});

-- | The device name exposed to the EC2 instance (for example, @\/dev\/sdh@
-- or @xvdh@).
bdmDeviceName :: Lens' BlockDeviceMapping Text
bdmDeviceName = lens _bdmDeviceName (\ s a -> s{_bdmDeviceName = a});

instance FromXML BlockDeviceMapping where
        parseXML x
          = BlockDeviceMapping' <$>
              (x .@? "VirtualName") <*> (x .@? "NoDevice") <*>
                (x .@? "Ebs")
                <*> (x .@ "DeviceName")

instance ToQuery BlockDeviceMapping where
        toQuery BlockDeviceMapping'{..}
          = mconcat
              ["VirtualName" =: _bdmVirtualName,
               "NoDevice" =: _bdmNoDevice, "Ebs" =: _bdmEBS,
               "DeviceName" =: _bdmDeviceName]

-- | Describes an Amazon EBS volume.
--
-- /See:/ 'ebs' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ebsDeleteOnTermination'
--
-- * 'ebsVolumeSize'
--
-- * 'ebsIOPS'
--
-- * 'ebsVolumeType'
--
-- * 'ebsSnapshotId'
data EBS = EBS'
    { _ebsDeleteOnTermination :: !(Maybe Bool)
    , _ebsVolumeSize          :: !(Maybe Nat)
    , _ebsIOPS                :: !(Maybe Nat)
    , _ebsVolumeType          :: !(Maybe Text)
    , _ebsSnapshotId          :: !(Maybe Text)
    } deriving (Eq,Read,Show)

-- | 'EBS' smart constructor.
ebs :: EBS
ebs =
    EBS'
    { _ebsDeleteOnTermination = Nothing
    , _ebsVolumeSize = Nothing
    , _ebsIOPS = Nothing
    , _ebsVolumeType = Nothing
    , _ebsSnapshotId = Nothing
    }

-- | Indicates whether to delete the volume on instance termination.
--
-- Default: @true@
ebsDeleteOnTermination :: Lens' EBS (Maybe Bool)
ebsDeleteOnTermination = lens _ebsDeleteOnTermination (\ s a -> s{_ebsDeleteOnTermination = a});

-- | The volume size, in gigabytes.
--
-- Valid values: If the volume type is @io1@, the minimum size of the
-- volume is 10 GiB. If you specify @SnapshotId@ and @VolumeSize@,
-- @VolumeSize@ must be equal to or larger than the size of the snapshot.
--
-- Default: If you create a volume from a snapshot and you don\'t specify a
-- volume size, the default is the size of the snapshot.
--
-- Required: Required when the volume type is @io1@.
ebsVolumeSize :: Lens' EBS (Maybe Natural)
ebsVolumeSize = lens _ebsVolumeSize (\ s a -> s{_ebsVolumeSize = a}) . mapping _Nat;

-- | For Provisioned IOPS (SSD) volumes only. The number of I\/O operations
-- per second (IOPS) to provision for the volume.
--
-- Valid values: Range is 100 to 4000.
--
-- Default: None
ebsIOPS :: Lens' EBS (Maybe Natural)
ebsIOPS = lens _ebsIOPS (\ s a -> s{_ebsIOPS = a}) . mapping _Nat;

-- | The volume type.
--
-- Valid values: @standard | io1 | gp2@
--
-- Default: @standard@
ebsVolumeType :: Lens' EBS (Maybe Text)
ebsVolumeType = lens _ebsVolumeType (\ s a -> s{_ebsVolumeType = a});

-- | The ID of the snapshot.
ebsSnapshotId :: Lens' EBS (Maybe Text)
ebsSnapshotId = lens _ebsSnapshotId (\ s a -> s{_ebsSnapshotId = a});

instance FromXML EBS where
        parseXML x
          = EBS' <$>
              (x .@? "DeleteOnTermination") <*>
                (x .@? "VolumeSize")
                <*> (x .@? "Iops")
                <*> (x .@? "VolumeType")
                <*> (x .@? "SnapshotId")

instance ToQuery EBS where
        toQuery EBS'{..}
          = mconcat
              ["DeleteOnTermination" =: _ebsDeleteOnTermination,
               "VolumeSize" =: _ebsVolumeSize, "Iops" =: _ebsIOPS,
               "VolumeType" =: _ebsVolumeType,
               "SnapshotId" =: _ebsSnapshotId]

-- | Describes an enabled metric.
--
-- /See:/ 'enabledMetric' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'emGranularity'
--
-- * 'emMetric'
data EnabledMetric = EnabledMetric'
    { _emGranularity :: !(Maybe Text)
    , _emMetric      :: !(Maybe Text)
    } deriving (Eq,Read,Show)

-- | 'EnabledMetric' smart constructor.
enabledMetric :: EnabledMetric
enabledMetric =
    EnabledMetric'
    { _emGranularity = Nothing
    , _emMetric = Nothing
    }

-- | The granularity of the metric. The only valid value is @1Minute@.
emGranularity :: Lens' EnabledMetric (Maybe Text)
emGranularity = lens _emGranularity (\ s a -> s{_emGranularity = a});

-- | The name of the metric.
--
-- -   @GroupMinSize@
--
-- -   @GroupMaxSize@
--
-- -   @GroupDesiredCapacity@
--
-- -   @GroupInServiceInstances@
--
-- -   @GroupPendingInstances@
--
-- -   @GroupStandbyInstances@
--
-- -   @GroupTerminatingInstances@
--
-- -   @GroupTotalInstances@
--
emMetric :: Lens' EnabledMetric (Maybe Text)
emMetric = lens _emMetric (\ s a -> s{_emMetric = a});

instance FromXML EnabledMetric where
        parseXML x
          = EnabledMetric' <$>
              (x .@? "Granularity") <*> (x .@? "Metric")

-- | Describes a filter.
--
-- /See:/ 'filter'' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'filValues'
--
-- * 'filName'
data Filter = Filter'
    { _filValues :: !(Maybe [Text])
    , _filName   :: !Text
    } deriving (Eq,Read,Show)

-- | 'Filter' smart constructor.
filter' :: Text -> Filter
filter' pName =
    Filter'
    { _filValues = Nothing
    , _filName = pName
    }

-- | The value of the filter.
filValues :: Lens' Filter [Text]
filValues = lens _filValues (\ s a -> s{_filValues = a}) . _Default;

-- | The name of the filter. The valid values are: @\"auto-scaling-group\"@,
-- @\"key\"@, @\"value\"@, and @\"propagate-at-launch\"@.
filName :: Lens' Filter Text
filName = lens _filName (\ s a -> s{_filName = a});

instance ToQuery Filter where
        toQuery Filter'{..}
          = mconcat
              ["Values" =:
                 toQuery (toQueryList "member" <$> _filValues),
               "Name" =: _filName]

-- | Describes an EC2 instance.
--
-- /See:/ 'instance'' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'insInstanceId'
--
-- * 'insAvailabilityZone'
--
-- * 'insLifecycleState'
--
-- * 'insHealthStatus'
--
-- * 'insLaunchConfigurationName'
data Instance = Instance'
    { _insInstanceId              :: !Text
    , _insAvailabilityZone        :: !Text
    , _insLifecycleState          :: !LifecycleState
    , _insHealthStatus            :: !Text
    , _insLaunchConfigurationName :: !Text
    } deriving (Eq,Read,Show)

-- | 'Instance' smart constructor.
instance' :: Text -> Text -> LifecycleState -> Text -> Text -> Instance
instance' pInstanceId pAvailabilityZone pLifecycleState pHealthStatus pLaunchConfigurationName =
    Instance'
    { _insInstanceId = pInstanceId
    , _insAvailabilityZone = pAvailabilityZone
    , _insLifecycleState = pLifecycleState
    , _insHealthStatus = pHealthStatus
    , _insLaunchConfigurationName = pLaunchConfigurationName
    }

-- | The ID of the instance.
insInstanceId :: Lens' Instance Text
insInstanceId = lens _insInstanceId (\ s a -> s{_insInstanceId = a});

-- | The Availability Zone in which the instance is running.
insAvailabilityZone :: Lens' Instance Text
insAvailabilityZone = lens _insAvailabilityZone (\ s a -> s{_insAvailabilityZone = a});

-- | A description of the current lifecycle state. Note that the
-- @Quarantined@ state is not used.
insLifecycleState :: Lens' Instance LifecycleState
insLifecycleState = lens _insLifecycleState (\ s a -> s{_insLifecycleState = a});

-- | The health status of the instance.
insHealthStatus :: Lens' Instance Text
insHealthStatus = lens _insHealthStatus (\ s a -> s{_insHealthStatus = a});

-- | The launch configuration associated with the instance.
insLaunchConfigurationName :: Lens' Instance Text
insLaunchConfigurationName = lens _insLaunchConfigurationName (\ s a -> s{_insLaunchConfigurationName = a});

instance FromXML Instance where
        parseXML x
          = Instance' <$>
              (x .@ "InstanceId") <*> (x .@ "AvailabilityZone") <*>
                (x .@ "LifecycleState")
                <*> (x .@ "HealthStatus")
                <*> (x .@ "LaunchConfigurationName")

-- | Describes whether instance monitoring is enabled.
--
-- /See:/ 'instanceMonitoring' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'imEnabled'
newtype InstanceMonitoring = InstanceMonitoring'
    { _imEnabled :: Maybe Bool
    } deriving (Eq,Read,Show)

-- | 'InstanceMonitoring' smart constructor.
instanceMonitoring :: InstanceMonitoring
instanceMonitoring =
    InstanceMonitoring'
    { _imEnabled = Nothing
    }

-- | If @True@, instance monitoring is enabled.
imEnabled :: Lens' InstanceMonitoring (Maybe Bool)
imEnabled = lens _imEnabled (\ s a -> s{_imEnabled = a});

instance FromXML InstanceMonitoring where
        parseXML x
          = InstanceMonitoring' <$> (x .@? "Enabled")

instance ToQuery InstanceMonitoring where
        toQuery InstanceMonitoring'{..}
          = mconcat ["Enabled" =: _imEnabled]

-- | Describes a launch configuration.
--
-- /See:/ 'launchConfiguration' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lcSecurityGroups'
--
-- * 'lcAssociatePublicIPAddress'
--
-- * 'lcInstanceMonitoring'
--
-- * 'lcSpotPrice'
--
-- * 'lcKeyName'
--
-- * 'lcClassicLinkVPCSecurityGroups'
--
-- * 'lcRAMDiskId'
--
-- * 'lcKernelId'
--
-- * 'lcEBSOptimized'
--
-- * 'lcUserData'
--
-- * 'lcClassicLinkVPCId'
--
-- * 'lcIAMInstanceProfile'
--
-- * 'lcLaunchConfigurationARN'
--
-- * 'lcPlacementTenancy'
--
-- * 'lcBlockDeviceMappings'
--
-- * 'lcLaunchConfigurationName'
--
-- * 'lcImageId'
--
-- * 'lcInstanceType'
--
-- * 'lcCreatedTime'
data LaunchConfiguration = LaunchConfiguration'
    { _lcSecurityGroups               :: !(Maybe [Text])
    , _lcAssociatePublicIPAddress     :: !(Maybe Bool)
    , _lcInstanceMonitoring           :: !(Maybe InstanceMonitoring)
    , _lcSpotPrice                    :: !(Maybe Text)
    , _lcKeyName                      :: !(Maybe Text)
    , _lcClassicLinkVPCSecurityGroups :: !(Maybe [Text])
    , _lcRAMDiskId                    :: !(Maybe Text)
    , _lcKernelId                     :: !(Maybe Text)
    , _lcEBSOptimized                 :: !(Maybe Bool)
    , _lcUserData                     :: !(Maybe Text)
    , _lcClassicLinkVPCId             :: !(Maybe Text)
    , _lcIAMInstanceProfile           :: !(Maybe Text)
    , _lcLaunchConfigurationARN       :: !(Maybe Text)
    , _lcPlacementTenancy             :: !(Maybe Text)
    , _lcBlockDeviceMappings          :: !(Maybe [BlockDeviceMapping])
    , _lcLaunchConfigurationName      :: !Text
    , _lcImageId                      :: !Text
    , _lcInstanceType                 :: !Text
    , _lcCreatedTime                  :: !ISO8601
    } deriving (Eq,Read,Show)

-- | 'LaunchConfiguration' smart constructor.
launchConfiguration :: Text -> Text -> Text -> UTCTime -> LaunchConfiguration
launchConfiguration pLaunchConfigurationName pImageId pInstanceType pCreatedTime =
    LaunchConfiguration'
    { _lcSecurityGroups = Nothing
    , _lcAssociatePublicIPAddress = Nothing
    , _lcInstanceMonitoring = Nothing
    , _lcSpotPrice = Nothing
    , _lcKeyName = Nothing
    , _lcClassicLinkVPCSecurityGroups = Nothing
    , _lcRAMDiskId = Nothing
    , _lcKernelId = Nothing
    , _lcEBSOptimized = Nothing
    , _lcUserData = Nothing
    , _lcClassicLinkVPCId = Nothing
    , _lcIAMInstanceProfile = Nothing
    , _lcLaunchConfigurationARN = Nothing
    , _lcPlacementTenancy = Nothing
    , _lcBlockDeviceMappings = Nothing
    , _lcLaunchConfigurationName = pLaunchConfigurationName
    , _lcImageId = pImageId
    , _lcInstanceType = pInstanceType
    , _lcCreatedTime = _Time # pCreatedTime
    }

-- | The security groups to associate with the instances.
lcSecurityGroups :: Lens' LaunchConfiguration [Text]
lcSecurityGroups = lens _lcSecurityGroups (\ s a -> s{_lcSecurityGroups = a}) . _Default;

-- | Specifies whether the instances are associated with a public IP address
-- (@true@) or not (@false@).
lcAssociatePublicIPAddress :: Lens' LaunchConfiguration (Maybe Bool)
lcAssociatePublicIPAddress = lens _lcAssociatePublicIPAddress (\ s a -> s{_lcAssociatePublicIPAddress = a});

-- | Controls whether instances in this group are launched with detailed
-- monitoring.
lcInstanceMonitoring :: Lens' LaunchConfiguration (Maybe InstanceMonitoring)
lcInstanceMonitoring = lens _lcInstanceMonitoring (\ s a -> s{_lcInstanceMonitoring = a});

-- | The price to bid when launching Spot Instances.
lcSpotPrice :: Lens' LaunchConfiguration (Maybe Text)
lcSpotPrice = lens _lcSpotPrice (\ s a -> s{_lcSpotPrice = a});

-- | The name of the key pair.
lcKeyName :: Lens' LaunchConfiguration (Maybe Text)
lcKeyName = lens _lcKeyName (\ s a -> s{_lcKeyName = a});

-- | The IDs of one or more security groups for the VPC specified in
-- @ClassicLinkVPCId@. This parameter is required if @ClassicLinkVPCId@ is
-- specified, and cannot be used otherwise. For more information, see
-- <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/vpc-classiclink.html ClassicLink>
-- in the /Amazon Elastic Compute Cloud User Guide/.
lcClassicLinkVPCSecurityGroups :: Lens' LaunchConfiguration [Text]
lcClassicLinkVPCSecurityGroups = lens _lcClassicLinkVPCSecurityGroups (\ s a -> s{_lcClassicLinkVPCSecurityGroups = a}) . _Default;

-- | The ID of the RAM disk associated with the AMI.
lcRAMDiskId :: Lens' LaunchConfiguration (Maybe Text)
lcRAMDiskId = lens _lcRAMDiskId (\ s a -> s{_lcRAMDiskId = a});

-- | The ID of the kernel associated with the AMI.
lcKernelId :: Lens' LaunchConfiguration (Maybe Text)
lcKernelId = lens _lcKernelId (\ s a -> s{_lcKernelId = a});

-- | Controls whether the instance is optimized for EBS I\/O (@true@) or not
-- (@false@).
lcEBSOptimized :: Lens' LaunchConfiguration (Maybe Bool)
lcEBSOptimized = lens _lcEBSOptimized (\ s a -> s{_lcEBSOptimized = a});

-- | The user data available to the instances.
lcUserData :: Lens' LaunchConfiguration (Maybe Text)
lcUserData = lens _lcUserData (\ s a -> s{_lcUserData = a});

-- | The ID of a ClassicLink-enabled VPC to link your EC2-Classic instances
-- to. This parameter can only be used if you are launching EC2-Classic
-- instances. For more information, see
-- <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/vpc-classiclink.html ClassicLink>
-- in the /Amazon Elastic Compute Cloud User Guide/.
lcClassicLinkVPCId :: Lens' LaunchConfiguration (Maybe Text)
lcClassicLinkVPCId = lens _lcClassicLinkVPCId (\ s a -> s{_lcClassicLinkVPCId = a});

-- | The name or Amazon Resource Name (ARN) of the instance profile
-- associated with the IAM role for the instance.
lcIAMInstanceProfile :: Lens' LaunchConfiguration (Maybe Text)
lcIAMInstanceProfile = lens _lcIAMInstanceProfile (\ s a -> s{_lcIAMInstanceProfile = a});

-- | The Amazon Resource Name (ARN) of the launch configuration.
lcLaunchConfigurationARN :: Lens' LaunchConfiguration (Maybe Text)
lcLaunchConfigurationARN = lens _lcLaunchConfigurationARN (\ s a -> s{_lcLaunchConfigurationARN = a});

-- | The tenancy of the instance, either @default@ or @dedicated@. An
-- instance with @dedicated@ tenancy runs in an isolated, single-tenant
-- hardware and can only be launched into a VPC.
lcPlacementTenancy :: Lens' LaunchConfiguration (Maybe Text)
lcPlacementTenancy = lens _lcPlacementTenancy (\ s a -> s{_lcPlacementTenancy = a});

-- | A block device mapping, which specifies the block devices for the
-- instance.
lcBlockDeviceMappings :: Lens' LaunchConfiguration [BlockDeviceMapping]
lcBlockDeviceMappings = lens _lcBlockDeviceMappings (\ s a -> s{_lcBlockDeviceMappings = a}) . _Default;

-- | The name of the launch configuration.
lcLaunchConfigurationName :: Lens' LaunchConfiguration Text
lcLaunchConfigurationName = lens _lcLaunchConfigurationName (\ s a -> s{_lcLaunchConfigurationName = a});

-- | The ID of the Amazon Machine Image (AMI).
lcImageId :: Lens' LaunchConfiguration Text
lcImageId = lens _lcImageId (\ s a -> s{_lcImageId = a});

-- | The instance type for the instances.
lcInstanceType :: Lens' LaunchConfiguration Text
lcInstanceType = lens _lcInstanceType (\ s a -> s{_lcInstanceType = a});

-- | The creation date and time for the launch configuration.
lcCreatedTime :: Lens' LaunchConfiguration UTCTime
lcCreatedTime = lens _lcCreatedTime (\ s a -> s{_lcCreatedTime = a}) . _Time;

instance FromXML LaunchConfiguration where
        parseXML x
          = LaunchConfiguration' <$>
              (x .@? "SecurityGroups" .!@ mempty >>=
                 may (parseXMLList "member"))
                <*> (x .@? "AssociatePublicIpAddress")
                <*> (x .@? "InstanceMonitoring")
                <*> (x .@? "SpotPrice")
                <*> (x .@? "KeyName")
                <*>
                (x .@? "ClassicLinkVPCSecurityGroups" .!@ mempty >>=
                   may (parseXMLList "member"))
                <*> (x .@? "RamdiskId")
                <*> (x .@? "KernelId")
                <*> (x .@? "EbsOptimized")
                <*> (x .@? "UserData")
                <*> (x .@? "ClassicLinkVPCId")
                <*> (x .@? "IamInstanceProfile")
                <*> (x .@? "LaunchConfigurationARN")
                <*> (x .@? "PlacementTenancy")
                <*>
                (x .@? "BlockDeviceMappings" .!@ mempty >>=
                   may (parseXMLList "member"))
                <*> (x .@ "LaunchConfigurationName")
                <*> (x .@ "ImageId")
                <*> (x .@ "InstanceType")
                <*> (x .@ "CreatedTime")

-- | Describes a lifecycle hook, which tells Auto Scaling that you want to
-- perform an action when an instance launches or terminates. When you have
-- a lifecycle hook in place, the Auto Scaling group will either:
--
-- -   Pause the instance after it launches, but before it is put into
--     service
-- -   Pause the instance as it terminates, but before it is fully
--     terminated
--
-- For more information, see
-- <http://docs.aws.amazon.com/AutoScaling/latest/DeveloperGuide/AutoScalingPendingState.html Auto Scaling Pending State>
-- and
-- <http://docs.aws.amazon.com/AutoScaling/latest/DeveloperGuide/AutoScalingTerminatingState.html Auto Scaling Terminating State>
-- in the /Auto Scaling Developer Guide/.
--
-- /See:/ 'lifecycleHook' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lhDefaultResult'
--
-- * 'lhLifecycleHookName'
--
-- * 'lhHeartbeatTimeout'
--
-- * 'lhAutoScalingGroupName'
--
-- * 'lhNotificationMetadata'
--
-- * 'lhGlobalTimeout'
--
-- * 'lhRoleARN'
--
-- * 'lhLifecycleTransition'
--
-- * 'lhNotificationTargetARN'
data LifecycleHook = LifecycleHook'
    { _lhDefaultResult         :: !(Maybe Text)
    , _lhLifecycleHookName     :: !(Maybe Text)
    , _lhHeartbeatTimeout      :: !(Maybe Int)
    , _lhAutoScalingGroupName  :: !(Maybe Text)
    , _lhNotificationMetadata  :: !(Maybe Text)
    , _lhGlobalTimeout         :: !(Maybe Int)
    , _lhRoleARN               :: !(Maybe Text)
    , _lhLifecycleTransition   :: !(Maybe Text)
    , _lhNotificationTargetARN :: !(Maybe Text)
    } deriving (Eq,Read,Show)

-- | 'LifecycleHook' smart constructor.
lifecycleHook :: LifecycleHook
lifecycleHook =
    LifecycleHook'
    { _lhDefaultResult = Nothing
    , _lhLifecycleHookName = Nothing
    , _lhHeartbeatTimeout = Nothing
    , _lhAutoScalingGroupName = Nothing
    , _lhNotificationMetadata = Nothing
    , _lhGlobalTimeout = Nothing
    , _lhRoleARN = Nothing
    , _lhLifecycleTransition = Nothing
    , _lhNotificationTargetARN = Nothing
    }

-- | Defines the action the Auto Scaling group should take when the lifecycle
-- hook timeout elapses or if an unexpected failure occurs. The valid
-- values are @CONTINUE@ and @ABANDON@. The default value is @CONTINUE@.
lhDefaultResult :: Lens' LifecycleHook (Maybe Text)
lhDefaultResult = lens _lhDefaultResult (\ s a -> s{_lhDefaultResult = a});

-- | The name of the lifecycle hook.
lhLifecycleHookName :: Lens' LifecycleHook (Maybe Text)
lhLifecycleHookName = lens _lhLifecycleHookName (\ s a -> s{_lhLifecycleHookName = a});

-- | The amount of time that can elapse before the lifecycle hook times out.
-- When the lifecycle hook times out, Auto Scaling performs the action
-- defined in the @DefaultResult@ parameter. You can prevent the lifecycle
-- hook from timing out by calling RecordLifecycleActionHeartbeat.
lhHeartbeatTimeout :: Lens' LifecycleHook (Maybe Int)
lhHeartbeatTimeout = lens _lhHeartbeatTimeout (\ s a -> s{_lhHeartbeatTimeout = a});

-- | The name of the Auto Scaling group for the lifecycle hook.
lhAutoScalingGroupName :: Lens' LifecycleHook (Maybe Text)
lhAutoScalingGroupName = lens _lhAutoScalingGroupName (\ s a -> s{_lhAutoScalingGroupName = a});

-- | Additional information that you want to include any time Auto Scaling
-- sends a message to the notification target.
lhNotificationMetadata :: Lens' LifecycleHook (Maybe Text)
lhNotificationMetadata = lens _lhNotificationMetadata (\ s a -> s{_lhNotificationMetadata = a});

-- | The maximum length of time an instance can remain in a @Pending:Wait@ or
-- @Terminating:Wait@ state. Currently, this value is set at 48 hours.
lhGlobalTimeout :: Lens' LifecycleHook (Maybe Int)
lhGlobalTimeout = lens _lhGlobalTimeout (\ s a -> s{_lhGlobalTimeout = a});

-- | The ARN of the IAM role that allows the Auto Scaling group to publish to
-- the specified notification target.
lhRoleARN :: Lens' LifecycleHook (Maybe Text)
lhRoleARN = lens _lhRoleARN (\ s a -> s{_lhRoleARN = a});

-- | The state of the EC2 instance to which you want to attach the lifecycle
-- hook. For a list of lifecycle hook types, see
-- DescribeLifecycleHookTypes.
lhLifecycleTransition :: Lens' LifecycleHook (Maybe Text)
lhLifecycleTransition = lens _lhLifecycleTransition (\ s a -> s{_lhLifecycleTransition = a});

-- | The ARN of the notification target that Auto Scaling uses to notify you
-- when an instance is in the transition state for the lifecycle hook. This
-- ARN target can be either an SQS queue or an SNS topic. The notification
-- message sent to the target includes the following:
--
-- -   Lifecycle action token
-- -   User account ID
-- -   Name of the Auto Scaling group
-- -   Lifecycle hook name
-- -   EC2 instance ID
-- -   Lifecycle transition
-- -   Notification metadata
lhNotificationTargetARN :: Lens' LifecycleHook (Maybe Text)
lhNotificationTargetARN = lens _lhNotificationTargetARN (\ s a -> s{_lhNotificationTargetARN = a});

instance FromXML LifecycleHook where
        parseXML x
          = LifecycleHook' <$>
              (x .@? "DefaultResult") <*>
                (x .@? "LifecycleHookName")
                <*> (x .@? "HeartbeatTimeout")
                <*> (x .@? "AutoScalingGroupName")
                <*> (x .@? "NotificationMetadata")
                <*> (x .@? "GlobalTimeout")
                <*> (x .@? "RoleARN")
                <*> (x .@? "LifecycleTransition")
                <*> (x .@? "NotificationTargetARN")

-- | Describes the state of a load balancer.
--
-- /See:/ 'loadBalancerState' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lbsState'
--
-- * 'lbsLoadBalancerName'
data LoadBalancerState = LoadBalancerState'
    { _lbsState            :: !(Maybe Text)
    , _lbsLoadBalancerName :: !(Maybe Text)
    } deriving (Eq,Read,Show)

-- | 'LoadBalancerState' smart constructor.
loadBalancerState :: LoadBalancerState
loadBalancerState =
    LoadBalancerState'
    { _lbsState = Nothing
    , _lbsLoadBalancerName = Nothing
    }

-- | The state of the load balancer.
--
-- -   @Adding@ - The instances in the group are being registered with the
--     load balancer.
--
-- -   @Added@ - All instances in the group are registered with the load
--     balancer.
--
-- -   @InService@ - At least one instance in the group passed an ELB
--     health check.
--
-- -   @Removing@ - The instances are being deregistered from the load
--     balancer. If connection draining is enabled, Elastic Load Balancing
--     waits for in-flight requests to complete before deregistering the
--     instances.
--
lbsState :: Lens' LoadBalancerState (Maybe Text)
lbsState = lens _lbsState (\ s a -> s{_lbsState = a});

-- | The name of the load balancer.
lbsLoadBalancerName :: Lens' LoadBalancerState (Maybe Text)
lbsLoadBalancerName = lens _lbsLoadBalancerName (\ s a -> s{_lbsLoadBalancerName = a});

instance FromXML LoadBalancerState where
        parseXML x
          = LoadBalancerState' <$>
              (x .@? "State") <*> (x .@? "LoadBalancerName")

-- | Describes a metric.
--
-- /See:/ 'metricCollectionType' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'mctMetric'
newtype MetricCollectionType = MetricCollectionType'
    { _mctMetric :: Maybe Text
    } deriving (Eq,Read,Show)

-- | 'MetricCollectionType' smart constructor.
metricCollectionType :: MetricCollectionType
metricCollectionType =
    MetricCollectionType'
    { _mctMetric = Nothing
    }

-- | The metric.
--
-- -   @GroupMinSize@
--
-- -   @GroupMaxSize@
--
-- -   @GroupDesiredCapacity@
--
-- -   @GroupInServiceInstances@
--
-- -   @GroupPendingInstances@
--
-- -   @GroupStandbyInstances@
--
-- -   @GroupTerminatingInstances@
--
-- -   @GroupTotalInstances@
--
mctMetric :: Lens' MetricCollectionType (Maybe Text)
mctMetric = lens _mctMetric (\ s a -> s{_mctMetric = a});

instance FromXML MetricCollectionType where
        parseXML x
          = MetricCollectionType' <$> (x .@? "Metric")

-- | Describes a granularity of a metric.
--
-- /See:/ 'metricGranularityType' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'mgtGranularity'
newtype MetricGranularityType = MetricGranularityType'
    { _mgtGranularity :: Maybe Text
    } deriving (Eq,Read,Show)

-- | 'MetricGranularityType' smart constructor.
metricGranularityType :: MetricGranularityType
metricGranularityType =
    MetricGranularityType'
    { _mgtGranularity = Nothing
    }

-- | The granularity. The only valid value is @1Minute@.
mgtGranularity :: Lens' MetricGranularityType (Maybe Text)
mgtGranularity = lens _mgtGranularity (\ s a -> s{_mgtGranularity = a});

instance FromXML MetricGranularityType where
        parseXML x
          = MetricGranularityType' <$> (x .@? "Granularity")

-- | Describes a notification.
--
-- /See:/ 'notificationConfiguration' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ncTopicARN'
--
-- * 'ncAutoScalingGroupName'
--
-- * 'ncNotificationType'
data NotificationConfiguration = NotificationConfiguration'
    { _ncTopicARN             :: !(Maybe Text)
    , _ncAutoScalingGroupName :: !(Maybe Text)
    , _ncNotificationType     :: !(Maybe Text)
    } deriving (Eq,Read,Show)

-- | 'NotificationConfiguration' smart constructor.
notificationConfiguration :: NotificationConfiguration
notificationConfiguration =
    NotificationConfiguration'
    { _ncTopicARN = Nothing
    , _ncAutoScalingGroupName = Nothing
    , _ncNotificationType = Nothing
    }

-- | The Amazon Resource Name (ARN) of the Amazon Simple Notification Service
-- (SNS) topic.
ncTopicARN :: Lens' NotificationConfiguration (Maybe Text)
ncTopicARN = lens _ncTopicARN (\ s a -> s{_ncTopicARN = a});

-- | The name of the group.
ncAutoScalingGroupName :: Lens' NotificationConfiguration (Maybe Text)
ncAutoScalingGroupName = lens _ncAutoScalingGroupName (\ s a -> s{_ncAutoScalingGroupName = a});

-- | The types of events for an action to start.
--
-- -   @autoscaling:EC2_INSTANCE_LAUNCH@
--
-- -   @autoscaling:EC2_INSTANCE_LAUNCH_ERROR@
--
-- -   @autoscaling:EC2_INSTANCE_TERMINATE@
--
-- -   @autoscaling:EC2_INSTANCE_TERMINATE_ERROR@
--
-- -   @autoscaling:TEST_NOTIFICATION@
--
ncNotificationType :: Lens' NotificationConfiguration (Maybe Text)
ncNotificationType = lens _ncNotificationType (\ s a -> s{_ncNotificationType = a});

instance FromXML NotificationConfiguration where
        parseXML x
          = NotificationConfiguration' <$>
              (x .@? "TopicARN") <*> (x .@? "AutoScalingGroupName")
                <*> (x .@? "NotificationType")

-- | Describes a process type.
--
-- For more information, see
-- <http://docs.aws.amazon.com/AutoScaling/latest/DeveloperGuide/US_SuspendResume.html#process-types Auto Scaling Processes>
-- in the /Auto Scaling Developer Guide/.
--
-- /See:/ 'processType' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ptProcessName'
newtype ProcessType = ProcessType'
    { _ptProcessName :: Text
    } deriving (Eq,Read,Show)

-- | 'ProcessType' smart constructor.
processType :: Text -> ProcessType
processType pProcessName =
    ProcessType'
    { _ptProcessName = pProcessName
    }

-- | The name of the process.
--
-- -   @Launch@
--
-- -   @Terminate@
--
-- -   @AddToLoadBalancer@
--
-- -   @AlarmNotification@
--
-- -   @AZRebalance@
--
-- -   @HealthCheck@
--
-- -   @ReplaceUnhealthy@
--
-- -   @ScheduledActions@
--
ptProcessName :: Lens' ProcessType Text
ptProcessName = lens _ptProcessName (\ s a -> s{_ptProcessName = a});

instance FromXML ProcessType where
        parseXML x = ProcessType' <$> (x .@ "ProcessName")

-- | Describes a scaling policy.
--
-- /See:/ 'scalingPolicy' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'scaMinAdjustmentStep'
--
-- * 'scaPolicyName'
--
-- * 'scaAdjustmentType'
--
-- * 'scaScalingAdjustment'
--
-- * 'scaAutoScalingGroupName'
--
-- * 'scaCooldown'
--
-- * 'scaPolicyARN'
--
-- * 'scaAlarms'
data ScalingPolicy = ScalingPolicy'
    { _scaMinAdjustmentStep    :: !(Maybe Int)
    , _scaPolicyName           :: !(Maybe Text)
    , _scaAdjustmentType       :: !(Maybe Text)
    , _scaScalingAdjustment    :: !(Maybe Int)
    , _scaAutoScalingGroupName :: !(Maybe Text)
    , _scaCooldown             :: !(Maybe Int)
    , _scaPolicyARN            :: !(Maybe Text)
    , _scaAlarms               :: !(Maybe [Alarm])
    } deriving (Eq,Read,Show)

-- | 'ScalingPolicy' smart constructor.
scalingPolicy :: ScalingPolicy
scalingPolicy =
    ScalingPolicy'
    { _scaMinAdjustmentStep = Nothing
    , _scaPolicyName = Nothing
    , _scaAdjustmentType = Nothing
    , _scaScalingAdjustment = Nothing
    , _scaAutoScalingGroupName = Nothing
    , _scaCooldown = Nothing
    , _scaPolicyARN = Nothing
    , _scaAlarms = Nothing
    }

-- | Changes the @DesiredCapacity@ of the Auto Scaling group by at least the
-- specified number of instances.
scaMinAdjustmentStep :: Lens' ScalingPolicy (Maybe Int)
scaMinAdjustmentStep = lens _scaMinAdjustmentStep (\ s a -> s{_scaMinAdjustmentStep = a});

-- | The name of the scaling policy.
scaPolicyName :: Lens' ScalingPolicy (Maybe Text)
scaPolicyName = lens _scaPolicyName (\ s a -> s{_scaPolicyName = a});

-- | Specifies whether the @ScalingAdjustment@ is an absolute number or a
-- percentage of the current capacity. Valid values are @ChangeInCapacity@,
-- @ExactCapacity@, and @PercentChangeInCapacity@.
scaAdjustmentType :: Lens' ScalingPolicy (Maybe Text)
scaAdjustmentType = lens _scaAdjustmentType (\ s a -> s{_scaAdjustmentType = a});

-- | The number associated with the specified adjustment type. A positive
-- value adds to the current capacity and a negative value removes from the
-- current capacity.
scaScalingAdjustment :: Lens' ScalingPolicy (Maybe Int)
scaScalingAdjustment = lens _scaScalingAdjustment (\ s a -> s{_scaScalingAdjustment = a});

-- | The name of the Auto Scaling group associated with this scaling policy.
scaAutoScalingGroupName :: Lens' ScalingPolicy (Maybe Text)
scaAutoScalingGroupName = lens _scaAutoScalingGroupName (\ s a -> s{_scaAutoScalingGroupName = a});

-- | The amount of time, in seconds, after a scaling activity completes
-- before any further trigger-related scaling activities can start.
scaCooldown :: Lens' ScalingPolicy (Maybe Int)
scaCooldown = lens _scaCooldown (\ s a -> s{_scaCooldown = a});

-- | The Amazon Resource Name (ARN) of the policy.
scaPolicyARN :: Lens' ScalingPolicy (Maybe Text)
scaPolicyARN = lens _scaPolicyARN (\ s a -> s{_scaPolicyARN = a});

-- | The CloudWatch alarms related to the policy.
scaAlarms :: Lens' ScalingPolicy [Alarm]
scaAlarms = lens _scaAlarms (\ s a -> s{_scaAlarms = a}) . _Default;

instance FromXML ScalingPolicy where
        parseXML x
          = ScalingPolicy' <$>
              (x .@? "MinAdjustmentStep") <*> (x .@? "PolicyName")
                <*> (x .@? "AdjustmentType")
                <*> (x .@? "ScalingAdjustment")
                <*> (x .@? "AutoScalingGroupName")
                <*> (x .@? "Cooldown")
                <*> (x .@? "PolicyARN")
                <*>
                (x .@? "Alarms" .!@ mempty >>=
                   may (parseXMLList "member"))

-- | /See:/ 'scalingProcessQuery' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'spqScalingProcesses'
--
-- * 'spqAutoScalingGroupName'
data ScalingProcessQuery = ScalingProcessQuery'
    { _spqScalingProcesses     :: !(Maybe [Text])
    , _spqAutoScalingGroupName :: !Text
    } deriving (Eq,Read,Show)

-- | 'ScalingProcessQuery' smart constructor.
scalingProcessQuery :: Text -> ScalingProcessQuery
scalingProcessQuery pAutoScalingGroupName =
    ScalingProcessQuery'
    { _spqScalingProcesses = Nothing
    , _spqAutoScalingGroupName = pAutoScalingGroupName
    }

-- | One or more of the following processes:
--
-- -   @Launch@
--
-- -   @Terminate@
--
-- -   @HealthCheck@
--
-- -   @ReplaceUnhealthy@
--
-- -   @AZRebalance@
--
-- -   @AlarmNotification@
--
-- -   @ScheduledActions@
--
-- -   @AddToLoadBalancer@
--
spqScalingProcesses :: Lens' ScalingProcessQuery [Text]
spqScalingProcesses = lens _spqScalingProcesses (\ s a -> s{_spqScalingProcesses = a}) . _Default;

-- | The name or Amazon Resource Name (ARN) of the Auto Scaling group.
spqAutoScalingGroupName :: Lens' ScalingProcessQuery Text
spqAutoScalingGroupName = lens _spqAutoScalingGroupName (\ s a -> s{_spqAutoScalingGroupName = a});

instance ToQuery ScalingProcessQuery where
        toQuery ScalingProcessQuery'{..}
          = mconcat
              ["ScalingProcesses" =:
                 toQuery
                   (toQueryList "member" <$> _spqScalingProcesses),
               "AutoScalingGroupName" =: _spqAutoScalingGroupName]

-- | Describes a scheduled update to an Auto Scaling group.
--
-- /See:/ 'scheduledUpdateGroupAction' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'sugaScheduledActionARN'
--
-- * 'sugaTime'
--
-- * 'sugaStartTime'
--
-- * 'sugaScheduledActionName'
--
-- * 'sugaMaxSize'
--
-- * 'sugaDesiredCapacity'
--
-- * 'sugaRecurrence'
--
-- * 'sugaMinSize'
--
-- * 'sugaEndTime'
--
-- * 'sugaAutoScalingGroupName'
data ScheduledUpdateGroupAction = ScheduledUpdateGroupAction'
    { _sugaScheduledActionARN   :: !(Maybe Text)
    , _sugaTime                 :: !(Maybe ISO8601)
    , _sugaStartTime            :: !(Maybe ISO8601)
    , _sugaScheduledActionName  :: !(Maybe Text)
    , _sugaMaxSize              :: !(Maybe Int)
    , _sugaDesiredCapacity      :: !(Maybe Int)
    , _sugaRecurrence           :: !(Maybe Text)
    , _sugaMinSize              :: !(Maybe Int)
    , _sugaEndTime              :: !(Maybe ISO8601)
    , _sugaAutoScalingGroupName :: !(Maybe Text)
    } deriving (Eq,Read,Show)

-- | 'ScheduledUpdateGroupAction' smart constructor.
scheduledUpdateGroupAction :: ScheduledUpdateGroupAction
scheduledUpdateGroupAction =
    ScheduledUpdateGroupAction'
    { _sugaScheduledActionARN = Nothing
    , _sugaTime = Nothing
    , _sugaStartTime = Nothing
    , _sugaScheduledActionName = Nothing
    , _sugaMaxSize = Nothing
    , _sugaDesiredCapacity = Nothing
    , _sugaRecurrence = Nothing
    , _sugaMinSize = Nothing
    , _sugaEndTime = Nothing
    , _sugaAutoScalingGroupName = Nothing
    }

-- | The Amazon Resource Name (ARN) of the scheduled action.
sugaScheduledActionARN :: Lens' ScheduledUpdateGroupAction (Maybe Text)
sugaScheduledActionARN = lens _sugaScheduledActionARN (\ s a -> s{_sugaScheduledActionARN = a});

-- | @Time@ is deprecated; use @StartTime@ instead.
sugaTime :: Lens' ScheduledUpdateGroupAction (Maybe UTCTime)
sugaTime = lens _sugaTime (\ s a -> s{_sugaTime = a}) . mapping _Time;

-- | The time that the action is scheduled to begin. This value can be up to
-- one month in the future.
--
-- When @StartTime@ and @EndTime@ are specified with @Recurrence@, they
-- form the boundaries of when the recurring action will start and stop.
sugaStartTime :: Lens' ScheduledUpdateGroupAction (Maybe UTCTime)
sugaStartTime = lens _sugaStartTime (\ s a -> s{_sugaStartTime = a}) . mapping _Time;

-- | The name of the scheduled action.
sugaScheduledActionName :: Lens' ScheduledUpdateGroupAction (Maybe Text)
sugaScheduledActionName = lens _sugaScheduledActionName (\ s a -> s{_sugaScheduledActionName = a});

-- | The maximum size of the group.
sugaMaxSize :: Lens' ScheduledUpdateGroupAction (Maybe Int)
sugaMaxSize = lens _sugaMaxSize (\ s a -> s{_sugaMaxSize = a});

-- | The number of instances you prefer to maintain in the group.
sugaDesiredCapacity :: Lens' ScheduledUpdateGroupAction (Maybe Int)
sugaDesiredCapacity = lens _sugaDesiredCapacity (\ s a -> s{_sugaDesiredCapacity = a});

-- | The recurring schedule for the action.
sugaRecurrence :: Lens' ScheduledUpdateGroupAction (Maybe Text)
sugaRecurrence = lens _sugaRecurrence (\ s a -> s{_sugaRecurrence = a});

-- | The minimum size of the group.
sugaMinSize :: Lens' ScheduledUpdateGroupAction (Maybe Int)
sugaMinSize = lens _sugaMinSize (\ s a -> s{_sugaMinSize = a});

-- | The time that the action is scheduled to end. This value can be up to
-- one month in the future.
sugaEndTime :: Lens' ScheduledUpdateGroupAction (Maybe UTCTime)
sugaEndTime = lens _sugaEndTime (\ s a -> s{_sugaEndTime = a}) . mapping _Time;

-- | The name of the group.
sugaAutoScalingGroupName :: Lens' ScheduledUpdateGroupAction (Maybe Text)
sugaAutoScalingGroupName = lens _sugaAutoScalingGroupName (\ s a -> s{_sugaAutoScalingGroupName = a});

instance FromXML ScheduledUpdateGroupAction where
        parseXML x
          = ScheduledUpdateGroupAction' <$>
              (x .@? "ScheduledActionARN") <*> (x .@? "Time") <*>
                (x .@? "StartTime")
                <*> (x .@? "ScheduledActionName")
                <*> (x .@? "MaxSize")
                <*> (x .@? "DesiredCapacity")
                <*> (x .@? "Recurrence")
                <*> (x .@? "MinSize")
                <*> (x .@? "EndTime")
                <*> (x .@? "AutoScalingGroupName")

-- | Describes an Auto Scaling process that has been suspended. For more
-- information, see ProcessType.
--
-- /See:/ 'suspendedProcess' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'spProcessName'
--
-- * 'spSuspensionReason'
data SuspendedProcess = SuspendedProcess'
    { _spProcessName      :: !(Maybe Text)
    , _spSuspensionReason :: !(Maybe Text)
    } deriving (Eq,Read,Show)

-- | 'SuspendedProcess' smart constructor.
suspendedProcess :: SuspendedProcess
suspendedProcess =
    SuspendedProcess'
    { _spProcessName = Nothing
    , _spSuspensionReason = Nothing
    }

-- | The name of the suspended process.
spProcessName :: Lens' SuspendedProcess (Maybe Text)
spProcessName = lens _spProcessName (\ s a -> s{_spProcessName = a});

-- | The reason that the process was suspended.
spSuspensionReason :: Lens' SuspendedProcess (Maybe Text)
spSuspensionReason = lens _spSuspensionReason (\ s a -> s{_spSuspensionReason = a});

instance FromXML SuspendedProcess where
        parseXML x
          = SuspendedProcess' <$>
              (x .@? "ProcessName") <*> (x .@? "SuspensionReason")

-- | Describes a tag for an Auto Scaling group.
--
-- /See:/ 'tag' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'tagKey'
--
-- * 'tagResourceId'
--
-- * 'tagResourceType'
--
-- * 'tagPropagateAtLaunch'
--
-- * 'tagValue'
data Tag = Tag'
    { _tagKey               :: !Text
    , _tagResourceId        :: !Text
    , _tagResourceType      :: !Text
    , _tagPropagateAtLaunch :: !Bool
    , _tagValue             :: !Text
    } deriving (Eq,Read,Show)

-- | 'Tag' smart constructor.
tag :: Text -> Text -> Text -> Bool -> Text -> Tag
tag pKey pResourceId pResourceType pPropagateAtLaunch pValue =
    Tag'
    { _tagKey = pKey
    , _tagResourceId = pResourceId
    , _tagResourceType = pResourceType
    , _tagPropagateAtLaunch = pPropagateAtLaunch
    , _tagValue = pValue
    }

-- | The tag key.
tagKey :: Lens' Tag Text
tagKey = lens _tagKey (\ s a -> s{_tagKey = a});

-- | The name of the group.
tagResourceId :: Lens' Tag Text
tagResourceId = lens _tagResourceId (\ s a -> s{_tagResourceId = a});

-- | The type of resource. The only supported value is @auto-scaling-group@.
tagResourceType :: Lens' Tag Text
tagResourceType = lens _tagResourceType (\ s a -> s{_tagResourceType = a});

-- | Determines whether the tag is added to new instances as they are
-- launched in the group.
tagPropagateAtLaunch :: Lens' Tag Bool
tagPropagateAtLaunch = lens _tagPropagateAtLaunch (\ s a -> s{_tagPropagateAtLaunch = a});

-- | The tag value.
tagValue :: Lens' Tag Text
tagValue = lens _tagValue (\ s a -> s{_tagValue = a});

instance ToQuery Tag where
        toQuery Tag'{..}
          = mconcat
              ["Key" =: _tagKey, "ResourceId" =: _tagResourceId,
               "ResourceType" =: _tagResourceType,
               "PropagateAtLaunch" =: _tagPropagateAtLaunch,
               "Value" =: _tagValue]

-- | Describes a tag for an Auto Scaling group.
--
-- /See:/ 'tagDescription' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'tdResourceId'
--
-- * 'tdResourceType'
--
-- * 'tdKey'
--
-- * 'tdPropagateAtLaunch'
--
-- * 'tdValue'
data TagDescription = TagDescription'
    { _tdResourceId        :: !Text
    , _tdResourceType      :: !Text
    , _tdKey               :: !Text
    , _tdPropagateAtLaunch :: !Bool
    , _tdValue             :: !Text
    } deriving (Eq,Read,Show)

-- | 'TagDescription' smart constructor.
tagDescription :: Text -> Text -> Text -> Bool -> Text -> TagDescription
tagDescription pResourceId pResourceType pKey pPropagateAtLaunch pValue =
    TagDescription'
    { _tdResourceId = pResourceId
    , _tdResourceType = pResourceType
    , _tdKey = pKey
    , _tdPropagateAtLaunch = pPropagateAtLaunch
    , _tdValue = pValue
    }

-- | The name of the group.
tdResourceId :: Lens' TagDescription Text
tdResourceId = lens _tdResourceId (\ s a -> s{_tdResourceId = a});

-- | The type of resource. The only supported value is @auto-scaling-group@.
tdResourceType :: Lens' TagDescription Text
tdResourceType = lens _tdResourceType (\ s a -> s{_tdResourceType = a});

-- | The tag key.
tdKey :: Lens' TagDescription Text
tdKey = lens _tdKey (\ s a -> s{_tdKey = a});

-- | Determines whether the tag is added to new instances as they are
-- launched in the group.
tdPropagateAtLaunch :: Lens' TagDescription Bool
tdPropagateAtLaunch = lens _tdPropagateAtLaunch (\ s a -> s{_tdPropagateAtLaunch = a});

-- | The tag value.
tdValue :: Lens' TagDescription Text
tdValue = lens _tdValue (\ s a -> s{_tdValue = a});

instance FromXML TagDescription where
        parseXML x
          = TagDescription' <$>
              (x .@ "ResourceId") <*> (x .@ "ResourceType") <*>
                (x .@ "Key")
                <*> (x .@ "PropagateAtLaunch")
                <*> (x .@ "Value")
