{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
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

module Network.AWS.AutoScaling.Types
    (
    -- * Service
      AutoScaling
    -- ** Error
    , RESTError

    -- * TagDescription
    , TagDescription
    , tagDescription
    , tdKey
    , tdPropagateAtLaunch
    , tdResourceId
    , tdResourceType
    , tdValue

    -- * Tag
    , Tag
    , tag
    , tagKey
    , tagPropagateAtLaunch
    , tagResourceId
    , tagResourceType
    , tagValue

    -- * NotificationConfiguration
    , NotificationConfiguration
    , notificationConfiguration
    , ncAutoScalingGroupName
    , ncNotificationType
    , ncTopicARN

    -- * BlockDeviceMapping
    , BlockDeviceMapping
    , blockDeviceMapping
    , bdmDeviceName
    , bdmEbs
    , bdmNoDevice
    , bdmVirtualName

    -- * LaunchConfiguration
    , LaunchConfiguration
    , launchConfiguration
    , lcAssociatePublicIpAddress
    , lcBlockDeviceMappings
    , lcCreatedTime
    , lcEbsOptimized
    , lcIamInstanceProfile
    , lcImageId
    , lcInstanceMonitoring
    , lcInstanceType
    , lcKernelId
    , lcKeyName
    , lcLaunchConfigurationARN
    , lcLaunchConfigurationName
    , lcPlacementTenancy
    , lcRamdiskId
    , lcSecurityGroups
    , lcSpotPrice
    , lcUserData

    -- * AutoScalingGroup
    , AutoScalingGroup
    , autoScalingGroup
    , asgAutoScalingGroupARN
    , asgAutoScalingGroupName
    , asgAvailabilityZones
    , asgCreatedTime
    , asgDefaultCooldown
    , asgDesiredCapacity
    , asgEnabledMetrics
    , asgHealthCheckGracePeriod
    , asgHealthCheckType
    , asgInstances
    , asgLaunchConfigurationName
    , asgLoadBalancerNames
    , asgMaxSize
    , asgMinSize
    , asgPlacementGroup
    , asgStatus
    , asgSuspendedProcesses
    , asgTags
    , asgTerminationPolicies
    , asgVPCZoneIdentifier

    -- * ScalingPolicy
    , ScalingPolicy
    , scalingPolicy
    , sp1AdjustmentType
    , sp1Alarms
    , sp1AutoScalingGroupName
    , sp1Cooldown
    , sp1MinAdjustmentStep
    , sp1PolicyARN
    , sp1PolicyName
    , sp1ScalingAdjustment

    -- * InstanceMonitoring
    , InstanceMonitoring
    , instanceMonitoring
    , imEnabled

    -- * ScheduledUpdateGroupAction
    , ScheduledUpdateGroupAction
    , scheduledUpdateGroupAction
    , sugaAutoScalingGroupName
    , sugaDesiredCapacity
    , sugaEndTime
    , sugaMaxSize
    , sugaMinSize
    , sugaRecurrence
    , sugaScheduledActionARN
    , sugaScheduledActionName
    , sugaStartTime
    , sugaTime

    -- * ScalingProcessQuery
    , ScalingProcessQuery
    , scalingProcessQuery
    , spqAutoScalingGroupName
    , spqScalingProcesses

    -- * Ebs
    , Ebs
    , ebs
    , ebsDeleteOnTermination
    , ebsIops
    , ebsSnapshotId
    , ebsVolumeSize
    , ebsVolumeType

    -- * AdjustmentType
    , AdjustmentType
    , adjustmentType
    , atAdjustmentType

    -- * MetricCollectionType
    , MetricCollectionType
    , metricCollectionType
    , mctMetric

    -- * LifecycleHook
    , LifecycleHook
    , lifecycleHook
    , lhAutoScalingGroupName
    , lhDefaultResult
    , lhGlobalTimeout
    , lhHeartbeatTimeout
    , lhLifecycleHookName
    , lhLifecycleTransition
    , lhNotificationMetadata
    , lhNotificationTargetARN
    , lhRoleARN

    -- * Activity
    , Activity
    , activity
    , aActivityId
    , aAutoScalingGroupName
    , aCause
    , aDescription
    , aDetails
    , aEndTime
    , aProgress
    , aStartTime
    , aStatusCode
    , aStatusMessage

    -- * SuspendedProcess
    , SuspendedProcess
    , suspendedProcess
    , spProcessName
    , spSuspensionReason

    -- * MetricGranularityType
    , MetricGranularityType
    , metricGranularityType
    , mgtGranularity

    -- * Filter
    , Filter
    , filter'
    , fName
    , fValues

    -- * ProcessType
    , ProcessType
    , processType
    , ptProcessName

    -- * Alarm
    , Alarm
    , alarm
    , aAlarmARN
    , aAlarmName

    -- * EnabledMetric
    , EnabledMetric
    , enabledMetric
    , emGranularity
    , emMetric

    -- * Instance
    , Instance
    , instance'
    , iAvailabilityZone
    , iHealthStatus
    , iInstanceId
    , iLaunchConfigurationName
    , iLifecycleState

    -- * LifecycleState
    , LifecycleState (..)

    -- * AutoScalingInstanceDetails
    , AutoScalingInstanceDetails
    , autoScalingInstanceDetails
    , asidAutoScalingGroupName
    , asidAvailabilityZone
    , asidHealthStatus
    , asidInstanceId
    , asidLaunchConfigurationName
    , asidLifecycleState

    -- * ScalingActivityStatusCode
    , ScalingActivityStatusCode (..)
    ) where

import Network.AWS.Error
import Network.AWS.Prelude
import Network.AWS.Signing.V4
import qualified GHC.Exts

-- | Version @2011-01-01@ of the Amazon Auto Scaling service.
data AutoScaling

instance AWSService AutoScaling where
    type Sg AutoScaling = V4
    type Er AutoScaling = RESTError

    service = Service
        { _svcEndpoint     = regional
        , _svcAbbrev       = "AutoScaling"
        , _svcPrefix       = "autoscaling"
        , _svcVersion      = "2011-01-01"
        , _svcTargetPrefix = Nothing
        , _svcJSONVersion  = Nothing
        }

    handle = restError alwaysFail

data TagDescription = TagDescription
    { _tdKey               :: Text
    , _tdPropagateAtLaunch :: Bool
    , _tdResourceId        :: Text
    , _tdResourceType      :: Text
    , _tdValue             :: Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'TagDescription' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'tdKey' @::@ 'Text'
--
-- * 'tdPropagateAtLaunch' @::@ 'Bool'
--
-- * 'tdResourceId' @::@ 'Text'
--
-- * 'tdResourceType' @::@ 'Text'
--
-- * 'tdValue' @::@ 'Text'
--
tagDescription :: Text -- ^ 'tdResourceId'
               -> Text -- ^ 'tdResourceType'
               -> Text -- ^ 'tdKey'
               -> Text -- ^ 'tdValue'
               -> Bool -- ^ 'tdPropagateAtLaunch'
               -> TagDescription
tagDescription p1 p2 p3 p4 p5 = TagDescription
    { _tdResourceId        = p1
    , _tdResourceType      = p2
    , _tdKey               = p3
    , _tdValue             = p4
    , _tdPropagateAtLaunch = p5
    }

-- | The key of the tag.
tdKey :: Lens' TagDescription Text
tdKey = lens _tdKey (\s a -> s { _tdKey = a })

-- | Specifies whether the new tag will be applied to instances launched after
-- the tag is created. The same behavior applies to updates: If you change a
-- tag, the changed tag will be applied to all instances launched after you
-- made the change.
tdPropagateAtLaunch :: Lens' TagDescription Bool
tdPropagateAtLaunch =
    lens _tdPropagateAtLaunch (\s a -> s { _tdPropagateAtLaunch = a })

-- | The name of the Auto Scaling group.
tdResourceId :: Lens' TagDescription Text
tdResourceId = lens _tdResourceId (\s a -> s { _tdResourceId = a })

-- | The kind of resource to which the tag is applied. Currently, Auto Scaling
-- supports the auto-scaling-group resource type.
tdResourceType :: Lens' TagDescription Text
tdResourceType = lens _tdResourceType (\s a -> s { _tdResourceType = a })

-- | The value of the tag.
tdValue :: Lens' TagDescription Text
tdValue = lens _tdValue (\s a -> s { _tdValue = a })

instance FromXML TagDescription where
    parseXML x = TagDescription
        <$> x .@ "Key"
        <*> x .@ "PropagateAtLaunch"
        <*> x .@ "ResourceId"
        <*> x .@ "ResourceType"
        <*> x .@ "Value"

instance ToQuery TagDescription

data Tag = Tag
    { _tagKey               :: Text
    , _tagPropagateAtLaunch :: Bool
    , _tagResourceId        :: Text
    , _tagResourceType      :: Text
    , _tagValue             :: Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'Tag' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'tagKey' @::@ 'Text'
--
-- * 'tagPropagateAtLaunch' @::@ 'Bool'
--
-- * 'tagResourceId' @::@ 'Text'
--
-- * 'tagResourceType' @::@ 'Text'
--
-- * 'tagValue' @::@ 'Text'
--
tag :: Text -- ^ 'tagResourceId'
    -> Text -- ^ 'tagResourceType'
    -> Text -- ^ 'tagKey'
    -> Text -- ^ 'tagValue'
    -> Bool -- ^ 'tagPropagateAtLaunch'
    -> Tag
tag p1 p2 p3 p4 p5 = Tag
    { _tagResourceId        = p1
    , _tagResourceType      = p2
    , _tagKey               = p3
    , _tagValue             = p4
    , _tagPropagateAtLaunch = p5
    }

-- | The key of the tag.
tagKey :: Lens' Tag Text
tagKey = lens _tagKey (\s a -> s { _tagKey = a })

-- | Specifies whether the new tag will be applied to instances launched after
-- the tag is created. The same behavior applies to updates: If you change a
-- tag, the changed tag will be applied to all instances launched after you
-- made the change.
tagPropagateAtLaunch :: Lens' Tag Bool
tagPropagateAtLaunch =
    lens _tagPropagateAtLaunch (\s a -> s { _tagPropagateAtLaunch = a })

-- | The name of the Auto Scaling group.
tagResourceId :: Lens' Tag Text
tagResourceId = lens _tagResourceId (\s a -> s { _tagResourceId = a })

-- | The kind of resource to which the tag is applied. Currently, Auto Scaling
-- supports the auto-scaling-group resource type.
tagResourceType :: Lens' Tag Text
tagResourceType = lens _tagResourceType (\s a -> s { _tagResourceType = a })

-- | The value of the tag.
tagValue :: Lens' Tag Text
tagValue = lens _tagValue (\s a -> s { _tagValue = a })

instance FromXML Tag where
    parseXML x = Tag
        <$> x .@ "Key"
        <*> x .@ "PropagateAtLaunch"
        <*> x .@ "ResourceId"
        <*> x .@ "ResourceType"
        <*> x .@ "Value"

instance ToQuery Tag

data NotificationConfiguration = NotificationConfiguration
    { _ncAutoScalingGroupName :: Maybe Text
    , _ncNotificationType     :: Maybe Text
    , _ncTopicARN             :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'NotificationConfiguration' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ncAutoScalingGroupName' @::@ 'Maybe' 'Text'
--
-- * 'ncNotificationType' @::@ 'Maybe' 'Text'
--
-- * 'ncTopicARN' @::@ 'Maybe' 'Text'
--
notificationConfiguration :: NotificationConfiguration
notificationConfiguration = NotificationConfiguration
    { _ncAutoScalingGroupName = Nothing
    , _ncTopicARN             = Nothing
    , _ncNotificationType     = Nothing
    }

-- | Specifies the Auto Scaling group name.
ncAutoScalingGroupName :: Lens' NotificationConfiguration (Maybe Text)
ncAutoScalingGroupName =
    lens _ncAutoScalingGroupName (\s a -> s { _ncAutoScalingGroupName = a })

-- | The types of events for an action to start.
ncNotificationType :: Lens' NotificationConfiguration (Maybe Text)
ncNotificationType =
    lens _ncNotificationType (\s a -> s { _ncNotificationType = a })

-- | The Amazon Resource Name (ARN) of the Amazon Simple Notification Service
-- (SNS) topic.
ncTopicARN :: Lens' NotificationConfiguration (Maybe Text)
ncTopicARN = lens _ncTopicARN (\s a -> s { _ncTopicARN = a })

instance FromXML NotificationConfiguration where
    parseXML x = NotificationConfiguration
        <$> x .@? "AutoScalingGroupName"
        <*> x .@? "NotificationType"
        <*> x .@? "TopicARN"

instance ToQuery NotificationConfiguration

data BlockDeviceMapping = BlockDeviceMapping
    { _bdmDeviceName  :: Text
    , _bdmEbs         :: Maybe Ebs
    , _bdmNoDevice    :: Maybe Bool
    , _bdmVirtualName :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | 'BlockDeviceMapping' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'bdmDeviceName' @::@ 'Text'
--
-- * 'bdmEbs' @::@ 'Maybe' 'Ebs'
--
-- * 'bdmNoDevice' @::@ 'Maybe' 'Bool'
--
-- * 'bdmVirtualName' @::@ 'Maybe' 'Text'
--
blockDeviceMapping :: Text -- ^ 'bdmDeviceName'
                   -> BlockDeviceMapping
blockDeviceMapping p1 = BlockDeviceMapping
    { _bdmDeviceName  = p1
    , _bdmVirtualName = Nothing
    , _bdmEbs         = Nothing
    , _bdmNoDevice    = Nothing
    }

-- | The name of the device within Amazon EC2 (for example, /dev/sdh or xvdh).
bdmDeviceName :: Lens' BlockDeviceMapping Text
bdmDeviceName = lens _bdmDeviceName (\s a -> s { _bdmDeviceName = a })

-- | The Elastic Block Storage volume information.
bdmEbs :: Lens' BlockDeviceMapping (Maybe Ebs)
bdmEbs = lens _bdmEbs (\s a -> s { _bdmEbs = a })

-- | Suppresses the device mapping.
bdmNoDevice :: Lens' BlockDeviceMapping (Maybe Bool)
bdmNoDevice = lens _bdmNoDevice (\s a -> s { _bdmNoDevice = a })

-- | The virtual name associated with the device.
bdmVirtualName :: Lens' BlockDeviceMapping (Maybe Text)
bdmVirtualName = lens _bdmVirtualName (\s a -> s { _bdmVirtualName = a })

instance FromXML BlockDeviceMapping where
    parseXML x = BlockDeviceMapping
        <$> x .@ "DeviceName"
        <*> x .@? "Ebs"
        <*> x .@? "NoDevice"
        <*> x .@? "VirtualName"

instance ToQuery BlockDeviceMapping

data LaunchConfiguration = LaunchConfiguration
    { _lcAssociatePublicIpAddress :: Maybe Bool
    , _lcBlockDeviceMappings      :: [BlockDeviceMapping]
    , _lcCreatedTime              :: RFC822
    , _lcEbsOptimized             :: Maybe Bool
    , _lcIamInstanceProfile       :: Maybe Text
    , _lcImageId                  :: Text
    , _lcInstanceMonitoring       :: Maybe InstanceMonitoring
    , _lcInstanceType             :: Text
    , _lcKernelId                 :: Maybe Text
    , _lcKeyName                  :: Maybe Text
    , _lcLaunchConfigurationARN   :: Maybe Text
    , _lcLaunchConfigurationName  :: Text
    , _lcPlacementTenancy         :: Maybe Text
    , _lcRamdiskId                :: Maybe Text
    , _lcSecurityGroups           :: [Text]
    , _lcSpotPrice                :: Maybe Text
    , _lcUserData                 :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | 'LaunchConfiguration' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lcAssociatePublicIpAddress' @::@ 'Maybe' 'Bool'
--
-- * 'lcBlockDeviceMappings' @::@ ['BlockDeviceMapping']
--
-- * 'lcCreatedTime' @::@ 'UTCTime'
--
-- * 'lcEbsOptimized' @::@ 'Maybe' 'Bool'
--
-- * 'lcIamInstanceProfile' @::@ 'Maybe' 'Text'
--
-- * 'lcImageId' @::@ 'Text'
--
-- * 'lcInstanceMonitoring' @::@ 'Maybe' 'InstanceMonitoring'
--
-- * 'lcInstanceType' @::@ 'Text'
--
-- * 'lcKernelId' @::@ 'Maybe' 'Text'
--
-- * 'lcKeyName' @::@ 'Maybe' 'Text'
--
-- * 'lcLaunchConfigurationARN' @::@ 'Maybe' 'Text'
--
-- * 'lcLaunchConfigurationName' @::@ 'Text'
--
-- * 'lcPlacementTenancy' @::@ 'Maybe' 'Text'
--
-- * 'lcRamdiskId' @::@ 'Maybe' 'Text'
--
-- * 'lcSecurityGroups' @::@ ['Text']
--
-- * 'lcSpotPrice' @::@ 'Maybe' 'Text'
--
-- * 'lcUserData' @::@ 'Maybe' 'Text'
--
launchConfiguration :: Text -- ^ 'lcLaunchConfigurationName'
                    -> Text -- ^ 'lcImageId'
                    -> Text -- ^ 'lcInstanceType'
                    -> UTCTime -- ^ 'lcCreatedTime'
                    -> LaunchConfiguration
launchConfiguration p1 p2 p3 p4 = LaunchConfiguration
    { _lcLaunchConfigurationName  = p1
    , _lcImageId                  = p2
    , _lcInstanceType             = p3
    , _lcCreatedTime              = withIso _Time (const id) p4
    , _lcLaunchConfigurationARN   = Nothing
    , _lcKeyName                  = Nothing
    , _lcSecurityGroups           = mempty
    , _lcUserData                 = Nothing
    , _lcKernelId                 = Nothing
    , _lcRamdiskId                = Nothing
    , _lcBlockDeviceMappings      = mempty
    , _lcInstanceMonitoring       = Nothing
    , _lcSpotPrice                = Nothing
    , _lcIamInstanceProfile       = Nothing
    , _lcEbsOptimized             = Nothing
    , _lcAssociatePublicIpAddress = Nothing
    , _lcPlacementTenancy         = Nothing
    }

-- | Specifies whether the instance is associated with a public IP address
-- (true) or not (false).
lcAssociatePublicIpAddress :: Lens' LaunchConfiguration (Maybe Bool)
lcAssociatePublicIpAddress =
    lens _lcAssociatePublicIpAddress
        (\s a -> s { _lcAssociatePublicIpAddress = a })

-- | Specifies how block devices are exposed to the instance. Each mapping is
-- made up of a virtualName and a deviceName.
lcBlockDeviceMappings :: Lens' LaunchConfiguration [BlockDeviceMapping]
lcBlockDeviceMappings =
    lens _lcBlockDeviceMappings (\s a -> s { _lcBlockDeviceMappings = a })

-- | Provides the creation date and time for this launch configuration.
lcCreatedTime :: Lens' LaunchConfiguration UTCTime
lcCreatedTime = lens _lcCreatedTime (\s a -> s { _lcCreatedTime = a })
    . _Time

-- | Specifies whether the instance is optimized for EBS I/O (true) or not
-- (false).
lcEbsOptimized :: Lens' LaunchConfiguration (Maybe Bool)
lcEbsOptimized = lens _lcEbsOptimized (\s a -> s { _lcEbsOptimized = a })

-- | Provides the name or the Amazon Resource Name (ARN) of the instance
-- profile associated with the IAM role for the instance. The instance
-- profile contains the IAM role.
lcIamInstanceProfile :: Lens' LaunchConfiguration (Maybe Text)
lcIamInstanceProfile =
    lens _lcIamInstanceProfile (\s a -> s { _lcIamInstanceProfile = a })

-- | Provides the unique ID of the Amazon Machine Image (AMI) that was
-- assigned during registration.
lcImageId :: Lens' LaunchConfiguration Text
lcImageId = lens _lcImageId (\s a -> s { _lcImageId = a })

-- | Controls whether instances in this group are launched with detailed
-- monitoring or not.
lcInstanceMonitoring :: Lens' LaunchConfiguration (Maybe InstanceMonitoring)
lcInstanceMonitoring =
    lens _lcInstanceMonitoring (\s a -> s { _lcInstanceMonitoring = a })

-- | Specifies the instance type of the Amazon EC2 instance.
lcInstanceType :: Lens' LaunchConfiguration Text
lcInstanceType = lens _lcInstanceType (\s a -> s { _lcInstanceType = a })

-- | Provides the ID of the kernel associated with the Amazon EC2 AMI.
lcKernelId :: Lens' LaunchConfiguration (Maybe Text)
lcKernelId = lens _lcKernelId (\s a -> s { _lcKernelId = a })

-- | Provides the name of the Amazon EC2 key pair.
lcKeyName :: Lens' LaunchConfiguration (Maybe Text)
lcKeyName = lens _lcKeyName (\s a -> s { _lcKeyName = a })

-- | The launch configuration's Amazon Resource Name (ARN).
lcLaunchConfigurationARN :: Lens' LaunchConfiguration (Maybe Text)
lcLaunchConfigurationARN =
    lens _lcLaunchConfigurationARN
        (\s a -> s { _lcLaunchConfigurationARN = a })

-- | Specifies the name of the launch configuration.
lcLaunchConfigurationName :: Lens' LaunchConfiguration Text
lcLaunchConfigurationName =
    lens _lcLaunchConfigurationName
        (\s a -> s { _lcLaunchConfigurationName = a })

-- | Specifies the tenancy of the instance. It can be either default or
-- dedicated. An instance with dedicated tenancy runs in an isolated,
-- single-tenant hardware and it can only be launched in a VPC.
lcPlacementTenancy :: Lens' LaunchConfiguration (Maybe Text)
lcPlacementTenancy =
    lens _lcPlacementTenancy (\s a -> s { _lcPlacementTenancy = a })

-- | Provides ID of the RAM disk associated with the Amazon EC2 AMI.
lcRamdiskId :: Lens' LaunchConfiguration (Maybe Text)
lcRamdiskId = lens _lcRamdiskId (\s a -> s { _lcRamdiskId = a })

-- | A description of the security groups to associate with the Amazon EC2
-- instances.
lcSecurityGroups :: Lens' LaunchConfiguration [Text]
lcSecurityGroups = lens _lcSecurityGroups (\s a -> s { _lcSecurityGroups = a })

-- | Specifies the price to bid when launching Spot Instances.
lcSpotPrice :: Lens' LaunchConfiguration (Maybe Text)
lcSpotPrice = lens _lcSpotPrice (\s a -> s { _lcSpotPrice = a })

-- | The user data available to the launched Amazon EC2 instances.
lcUserData :: Lens' LaunchConfiguration (Maybe Text)
lcUserData = lens _lcUserData (\s a -> s { _lcUserData = a })

instance FromXML LaunchConfiguration where
    parseXML x = LaunchConfiguration
        <$> x .@? "AssociatePublicIpAddress"
        <*> x .@ "BlockDeviceMappings"
        <*> x .@ "CreatedTime"
        <*> x .@? "EbsOptimized"
        <*> x .@? "IamInstanceProfile"
        <*> x .@ "ImageId"
        <*> x .@? "InstanceMonitoring"
        <*> x .@ "InstanceType"
        <*> x .@? "KernelId"
        <*> x .@? "KeyName"
        <*> x .@? "LaunchConfigurationARN"
        <*> x .@ "LaunchConfigurationName"
        <*> x .@? "PlacementTenancy"
        <*> x .@? "RamdiskId"
        <*> x .@ "SecurityGroups"
        <*> x .@? "SpotPrice"
        <*> x .@? "UserData"

instance ToQuery LaunchConfiguration

data AutoScalingGroup = AutoScalingGroup
    { _asgAutoScalingGroupARN     :: Maybe Text
    , _asgAutoScalingGroupName    :: Text
    , _asgAvailabilityZones       :: List1 Text
    , _asgCreatedTime             :: RFC822
    , _asgDefaultCooldown         :: Int
    , _asgDesiredCapacity         :: Int
    , _asgEnabledMetrics          :: [EnabledMetric]
    , _asgHealthCheckGracePeriod  :: Maybe Int
    , _asgHealthCheckType         :: Text
    , _asgInstances               :: [Instance]
    , _asgLaunchConfigurationName :: Text
    , _asgLoadBalancerNames       :: [Text]
    , _asgMaxSize                 :: Int
    , _asgMinSize                 :: Int
    , _asgPlacementGroup          :: Maybe Text
    , _asgStatus                  :: Maybe Text
    , _asgSuspendedProcesses      :: [SuspendedProcess]
    , _asgTags                    :: [TagDescription]
    , _asgTerminationPolicies     :: [Text]
    , _asgVPCZoneIdentifier       :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | 'AutoScalingGroup' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'asgAutoScalingGroupARN' @::@ 'Maybe' 'Text'
--
-- * 'asgAutoScalingGroupName' @::@ 'Text'
--
-- * 'asgAvailabilityZones' @::@ 'NonEmpty' 'Text'
--
-- * 'asgCreatedTime' @::@ 'UTCTime'
--
-- * 'asgDefaultCooldown' @::@ 'Int'
--
-- * 'asgDesiredCapacity' @::@ 'Int'
--
-- * 'asgEnabledMetrics' @::@ ['EnabledMetric']
--
-- * 'asgHealthCheckGracePeriod' @::@ 'Maybe' 'Int'
--
-- * 'asgHealthCheckType' @::@ 'Text'
--
-- * 'asgInstances' @::@ ['Instance']
--
-- * 'asgLaunchConfigurationName' @::@ 'Text'
--
-- * 'asgLoadBalancerNames' @::@ ['Text']
--
-- * 'asgMaxSize' @::@ 'Int'
--
-- * 'asgMinSize' @::@ 'Int'
--
-- * 'asgPlacementGroup' @::@ 'Maybe' 'Text'
--
-- * 'asgStatus' @::@ 'Maybe' 'Text'
--
-- * 'asgSuspendedProcesses' @::@ ['SuspendedProcess']
--
-- * 'asgTags' @::@ ['TagDescription']
--
-- * 'asgTerminationPolicies' @::@ ['Text']
--
-- * 'asgVPCZoneIdentifier' @::@ 'Maybe' 'Text'
--
autoScalingGroup :: Text -- ^ 'asgAutoScalingGroupName'
                 -> Text -- ^ 'asgLaunchConfigurationName'
                 -> Int -- ^ 'asgMinSize'
                 -> Int -- ^ 'asgMaxSize'
                 -> Int -- ^ 'asgDesiredCapacity'
                 -> Int -- ^ 'asgDefaultCooldown'
                 -> NonEmpty Text -- ^ 'asgAvailabilityZones'
                 -> Text -- ^ 'asgHealthCheckType'
                 -> UTCTime -- ^ 'asgCreatedTime'
                 -> AutoScalingGroup
autoScalingGroup p1 p2 p3 p4 p5 p6 p7 p8 p9 = AutoScalingGroup
    { _asgAutoScalingGroupName    = p1
    , _asgLaunchConfigurationName = p2
    , _asgMinSize                 = p3
    , _asgMaxSize                 = p4
    , _asgDesiredCapacity         = p5
    , _asgDefaultCooldown         = p6
    , _asgAvailabilityZones       = withIso _List1 (const id) p7
    , _asgHealthCheckType         = p8
    , _asgCreatedTime             = withIso _Time (const id) p9
    , _asgAutoScalingGroupARN     = Nothing
    , _asgLoadBalancerNames       = mempty
    , _asgHealthCheckGracePeriod  = Nothing
    , _asgInstances               = mempty
    , _asgSuspendedProcesses      = mempty
    , _asgPlacementGroup          = Nothing
    , _asgVPCZoneIdentifier       = Nothing
    , _asgEnabledMetrics          = mempty
    , _asgStatus                  = Nothing
    , _asgTags                    = mempty
    , _asgTerminationPolicies     = mempty
    }

-- | The Amazon Resource Name (ARN) of the Auto Scaling group.
asgAutoScalingGroupARN :: Lens' AutoScalingGroup (Maybe Text)
asgAutoScalingGroupARN =
    lens _asgAutoScalingGroupARN (\s a -> s { _asgAutoScalingGroupARN = a })

-- | Specifies the name of the group.
asgAutoScalingGroupName :: Lens' AutoScalingGroup Text
asgAutoScalingGroupName =
    lens _asgAutoScalingGroupName (\s a -> s { _asgAutoScalingGroupName = a })

-- | Contains a list of Availability Zones for the group.
asgAvailabilityZones :: Lens' AutoScalingGroup (NonEmpty Text)
asgAvailabilityZones =
    lens _asgAvailabilityZones (\s a -> s { _asgAvailabilityZones = a })
        . _List1

-- | Specifies the date and time the Auto Scaling group was created.
asgCreatedTime :: Lens' AutoScalingGroup UTCTime
asgCreatedTime = lens _asgCreatedTime (\s a -> s { _asgCreatedTime = a })
    . _Time

-- | The number of seconds after a scaling activity completes before any
-- further scaling activities can start.
asgDefaultCooldown :: Lens' AutoScalingGroup Int
asgDefaultCooldown =
    lens _asgDefaultCooldown (\s a -> s { _asgDefaultCooldown = a })

-- | Specifies the desired capacity for the Auto Scaling group.
asgDesiredCapacity :: Lens' AutoScalingGroup Int
asgDesiredCapacity =
    lens _asgDesiredCapacity (\s a -> s { _asgDesiredCapacity = a })

-- | A list of metrics enabled for this Auto Scaling group.
asgEnabledMetrics :: Lens' AutoScalingGroup [EnabledMetric]
asgEnabledMetrics =
    lens _asgEnabledMetrics (\s a -> s { _asgEnabledMetrics = a })

-- | The length of time that Auto Scaling waits before checking an instance's
-- health status. The grace period begins when an instance comes into
-- service.
asgHealthCheckGracePeriod :: Lens' AutoScalingGroup (Maybe Int)
asgHealthCheckGracePeriod =
    lens _asgHealthCheckGracePeriod
        (\s a -> s { _asgHealthCheckGracePeriod = a })

-- | The service of interest for the health status check, either "EC2" for
-- Amazon EC2 or "ELB" for Elastic Load Balancing.
asgHealthCheckType :: Lens' AutoScalingGroup Text
asgHealthCheckType =
    lens _asgHealthCheckType (\s a -> s { _asgHealthCheckType = a })

-- | Provides a summary list of Amazon EC2 instances.
asgInstances :: Lens' AutoScalingGroup [Instance]
asgInstances = lens _asgInstances (\s a -> s { _asgInstances = a })

-- | Specifies the name of the associated LaunchConfiguration.
asgLaunchConfigurationName :: Lens' AutoScalingGroup Text
asgLaunchConfigurationName =
    lens _asgLaunchConfigurationName
        (\s a -> s { _asgLaunchConfigurationName = a })

-- | A list of load balancers associated with this Auto Scaling group.
asgLoadBalancerNames :: Lens' AutoScalingGroup [Text]
asgLoadBalancerNames =
    lens _asgLoadBalancerNames (\s a -> s { _asgLoadBalancerNames = a })

-- | Contains the maximum size of the Auto Scaling group.
asgMaxSize :: Lens' AutoScalingGroup Int
asgMaxSize = lens _asgMaxSize (\s a -> s { _asgMaxSize = a })

-- | Contains the minimum size of the Auto Scaling group.
asgMinSize :: Lens' AutoScalingGroup Int
asgMinSize = lens _asgMinSize (\s a -> s { _asgMinSize = a })

-- | The name of the cluster placement group, if applicable. For more
-- information, go to Using Cluster Instances in the Amazon EC2 User Guide.
asgPlacementGroup :: Lens' AutoScalingGroup (Maybe Text)
asgPlacementGroup =
    lens _asgPlacementGroup (\s a -> s { _asgPlacementGroup = a })

-- | The current state of the Auto Scaling group when a DeleteAutoScalingGroup
-- action is in progress.
asgStatus :: Lens' AutoScalingGroup (Maybe Text)
asgStatus = lens _asgStatus (\s a -> s { _asgStatus = a })

-- | Suspended processes associated with this Auto Scaling group.
asgSuspendedProcesses :: Lens' AutoScalingGroup [SuspendedProcess]
asgSuspendedProcesses =
    lens _asgSuspendedProcesses (\s a -> s { _asgSuspendedProcesses = a })

-- | A list of tags for the Auto Scaling group.
asgTags :: Lens' AutoScalingGroup [TagDescription]
asgTags = lens _asgTags (\s a -> s { _asgTags = a })

-- | A standalone termination policy or a list of termination policies for
-- this Auto Scaling group.
asgTerminationPolicies :: Lens' AutoScalingGroup [Text]
asgTerminationPolicies =
    lens _asgTerminationPolicies (\s a -> s { _asgTerminationPolicies = a })

-- | The subnet identifier for the Amazon VPC connection, if applicable. You
-- can specify several subnets in a comma-separated list. When you specify
-- VPCZoneIdentifier with AvailabilityZones, ensure that the subnets'
-- Availability Zones match the values you specify for AvailabilityZones.
asgVPCZoneIdentifier :: Lens' AutoScalingGroup (Maybe Text)
asgVPCZoneIdentifier =
    lens _asgVPCZoneIdentifier (\s a -> s { _asgVPCZoneIdentifier = a })

instance FromXML AutoScalingGroup where
    parseXML x = AutoScalingGroup
        <$> x .@? "AutoScalingGroupARN"
        <*> x .@ "AutoScalingGroupName"
        <*> x .@ "AvailabilityZones"
        <*> x .@ "CreatedTime"
        <*> x .@ "DefaultCooldown"
        <*> x .@ "DesiredCapacity"
        <*> x .@ "EnabledMetrics"
        <*> x .@? "HealthCheckGracePeriod"
        <*> x .@ "HealthCheckType"
        <*> x .@ "Instances"
        <*> x .@ "LaunchConfigurationName"
        <*> x .@ "LoadBalancerNames"
        <*> x .@ "MaxSize"
        <*> x .@ "MinSize"
        <*> x .@? "PlacementGroup"
        <*> x .@? "Status"
        <*> x .@ "SuspendedProcesses"
        <*> x .@ "Tags"
        <*> x .@ "TerminationPolicies"
        <*> x .@? "VPCZoneIdentifier"

instance ToQuery AutoScalingGroup

data ScalingPolicy = ScalingPolicy
    { _sp1AdjustmentType       :: Maybe Text
    , _sp1Alarms               :: [Alarm]
    , _sp1AutoScalingGroupName :: Maybe Text
    , _sp1Cooldown             :: Maybe Int
    , _sp1MinAdjustmentStep    :: Maybe Int
    , _sp1PolicyARN            :: Maybe Text
    , _sp1PolicyName           :: Maybe Text
    , _sp1ScalingAdjustment    :: Maybe Int
    } deriving (Eq, Show, Generic)

-- | 'ScalingPolicy' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'sp1AdjustmentType' @::@ 'Maybe' 'Text'
--
-- * 'sp1Alarms' @::@ ['Alarm']
--
-- * 'sp1AutoScalingGroupName' @::@ 'Maybe' 'Text'
--
-- * 'sp1Cooldown' @::@ 'Maybe' 'Int'
--
-- * 'sp1MinAdjustmentStep' @::@ 'Maybe' 'Int'
--
-- * 'sp1PolicyARN' @::@ 'Maybe' 'Text'
--
-- * 'sp1PolicyName' @::@ 'Maybe' 'Text'
--
-- * 'sp1ScalingAdjustment' @::@ 'Maybe' 'Int'
--
scalingPolicy :: ScalingPolicy
scalingPolicy = ScalingPolicy
    { _sp1AutoScalingGroupName = Nothing
    , _sp1PolicyName           = Nothing
    , _sp1ScalingAdjustment    = Nothing
    , _sp1AdjustmentType       = Nothing
    , _sp1Cooldown             = Nothing
    , _sp1PolicyARN            = Nothing
    , _sp1Alarms               = mempty
    , _sp1MinAdjustmentStep    = Nothing
    }

-- | Specifies whether the ScalingAdjustment is an absolute number or a
-- percentage of the current capacity. Valid values are ChangeInCapacity,
-- ExactCapacity, and PercentChangeInCapacity.
sp1AdjustmentType :: Lens' ScalingPolicy (Maybe Text)
sp1AdjustmentType =
    lens _sp1AdjustmentType (\s a -> s { _sp1AdjustmentType = a })

-- | A list of CloudWatch Alarms related to the policy.
sp1Alarms :: Lens' ScalingPolicy [Alarm]
sp1Alarms = lens _sp1Alarms (\s a -> s { _sp1Alarms = a })

-- | The name of the Auto Scaling group associated with this scaling policy.
sp1AutoScalingGroupName :: Lens' ScalingPolicy (Maybe Text)
sp1AutoScalingGroupName =
    lens _sp1AutoScalingGroupName (\s a -> s { _sp1AutoScalingGroupName = a })

-- | The amount of time, in seconds, after a scaling activity completes before
-- any further trigger-related scaling activities can start.
sp1Cooldown :: Lens' ScalingPolicy (Maybe Int)
sp1Cooldown = lens _sp1Cooldown (\s a -> s { _sp1Cooldown = a })

-- | Changes the DesiredCapacity of the Auto Scaling group by at least the
-- specified number of instances.
sp1MinAdjustmentStep :: Lens' ScalingPolicy (Maybe Int)
sp1MinAdjustmentStep =
    lens _sp1MinAdjustmentStep (\s a -> s { _sp1MinAdjustmentStep = a })

-- | The Amazon Resource Name (ARN) of the policy.
sp1PolicyARN :: Lens' ScalingPolicy (Maybe Text)
sp1PolicyARN = lens _sp1PolicyARN (\s a -> s { _sp1PolicyARN = a })

-- | The name of the scaling policy.
sp1PolicyName :: Lens' ScalingPolicy (Maybe Text)
sp1PolicyName = lens _sp1PolicyName (\s a -> s { _sp1PolicyName = a })

-- | The number associated with the specified adjustment type. A positive
-- value adds to the current capacity and a negative value removes from the
-- current capacity.
sp1ScalingAdjustment :: Lens' ScalingPolicy (Maybe Int)
sp1ScalingAdjustment =
    lens _sp1ScalingAdjustment (\s a -> s { _sp1ScalingAdjustment = a })

instance FromXML ScalingPolicy where
    parseXML x = ScalingPolicy
        <$> x .@? "AdjustmentType"
        <*> x .@ "Alarms"
        <*> x .@? "AutoScalingGroupName"
        <*> x .@? "Cooldown"
        <*> x .@? "MinAdjustmentStep"
        <*> x .@? "PolicyARN"
        <*> x .@? "PolicyName"
        <*> x .@? "ScalingAdjustment"

instance ToQuery ScalingPolicy

newtype InstanceMonitoring = InstanceMonitoring
    { _imEnabled :: Maybe Bool
    } deriving (Eq, Ord, Show, Generic)

-- | 'InstanceMonitoring' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'imEnabled' @::@ 'Maybe' 'Bool'
--
instanceMonitoring :: InstanceMonitoring
instanceMonitoring = InstanceMonitoring
    { _imEnabled = Nothing
    }

-- | If True, instance monitoring is enabled.
imEnabled :: Lens' InstanceMonitoring (Maybe Bool)
imEnabled = lens _imEnabled (\s a -> s { _imEnabled = a })

instance FromXML InstanceMonitoring where
    parseXML x = InstanceMonitoring
        <$> x .@? "Enabled"

instance ToQuery InstanceMonitoring

data ScheduledUpdateGroupAction = ScheduledUpdateGroupAction
    { _sugaAutoScalingGroupName :: Maybe Text
    , _sugaDesiredCapacity      :: Maybe Int
    , _sugaEndTime              :: Maybe RFC822
    , _sugaMaxSize              :: Maybe Int
    , _sugaMinSize              :: Maybe Int
    , _sugaRecurrence           :: Maybe Text
    , _sugaScheduledActionARN   :: Maybe Text
    , _sugaScheduledActionName  :: Maybe Text
    , _sugaStartTime            :: Maybe RFC822
    , _sugaTime                 :: Maybe RFC822
    } deriving (Eq, Ord, Show, Generic)

-- | 'ScheduledUpdateGroupAction' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'sugaAutoScalingGroupName' @::@ 'Maybe' 'Text'
--
-- * 'sugaDesiredCapacity' @::@ 'Maybe' 'Int'
--
-- * 'sugaEndTime' @::@ 'Maybe' 'UTCTime'
--
-- * 'sugaMaxSize' @::@ 'Maybe' 'Int'
--
-- * 'sugaMinSize' @::@ 'Maybe' 'Int'
--
-- * 'sugaRecurrence' @::@ 'Maybe' 'Text'
--
-- * 'sugaScheduledActionARN' @::@ 'Maybe' 'Text'
--
-- * 'sugaScheduledActionName' @::@ 'Maybe' 'Text'
--
-- * 'sugaStartTime' @::@ 'Maybe' 'UTCTime'
--
-- * 'sugaTime' @::@ 'Maybe' 'UTCTime'
--
scheduledUpdateGroupAction :: ScheduledUpdateGroupAction
scheduledUpdateGroupAction = ScheduledUpdateGroupAction
    { _sugaAutoScalingGroupName = Nothing
    , _sugaScheduledActionName  = Nothing
    , _sugaScheduledActionARN   = Nothing
    , _sugaTime                 = Nothing
    , _sugaStartTime            = Nothing
    , _sugaEndTime              = Nothing
    , _sugaRecurrence           = Nothing
    , _sugaMinSize              = Nothing
    , _sugaMaxSize              = Nothing
    , _sugaDesiredCapacity      = Nothing
    }

-- | The name of the Auto Scaling group to be updated.
sugaAutoScalingGroupName :: Lens' ScheduledUpdateGroupAction (Maybe Text)
sugaAutoScalingGroupName =
    lens _sugaAutoScalingGroupName
        (\s a -> s { _sugaAutoScalingGroupName = a })

-- | The number of instances you prefer to maintain in your Auto Scaling
-- group.
sugaDesiredCapacity :: Lens' ScheduledUpdateGroupAction (Maybe Int)
sugaDesiredCapacity =
    lens _sugaDesiredCapacity (\s a -> s { _sugaDesiredCapacity = a })

-- | The time that the action is scheduled to end. This value can be up to one
-- month in the future.
sugaEndTime :: Lens' ScheduledUpdateGroupAction (Maybe UTCTime)
sugaEndTime = lens _sugaEndTime (\s a -> s { _sugaEndTime = a })
    . mapping _Time

-- | The maximum size of the Auto Scaling group.
sugaMaxSize :: Lens' ScheduledUpdateGroupAction (Maybe Int)
sugaMaxSize = lens _sugaMaxSize (\s a -> s { _sugaMaxSize = a })

-- | The minimum size of the Auto Scaling group.
sugaMinSize :: Lens' ScheduledUpdateGroupAction (Maybe Int)
sugaMinSize = lens _sugaMinSize (\s a -> s { _sugaMinSize = a })

-- | The regular schedule that an action occurs.
sugaRecurrence :: Lens' ScheduledUpdateGroupAction (Maybe Text)
sugaRecurrence = lens _sugaRecurrence (\s a -> s { _sugaRecurrence = a })

-- | The Amazon Resource Name (ARN) of this scheduled action.
sugaScheduledActionARN :: Lens' ScheduledUpdateGroupAction (Maybe Text)
sugaScheduledActionARN =
    lens _sugaScheduledActionARN (\s a -> s { _sugaScheduledActionARN = a })

-- | The name of this scheduled action.
sugaScheduledActionName :: Lens' ScheduledUpdateGroupAction (Maybe Text)
sugaScheduledActionName =
    lens _sugaScheduledActionName (\s a -> s { _sugaScheduledActionName = a })

-- | The time that the action is scheduled to begin. This value can be up to
-- one month in the future. When StartTime and EndTime are specified with
-- Recurrence, they form the boundaries of when the recurring action will
-- start and stop.
sugaStartTime :: Lens' ScheduledUpdateGroupAction (Maybe UTCTime)
sugaStartTime = lens _sugaStartTime (\s a -> s { _sugaStartTime = a })
    . mapping _Time

-- | Time is deprecated. The time that the action is scheduled to begin. Time
-- is an alias for StartTime.
sugaTime :: Lens' ScheduledUpdateGroupAction (Maybe UTCTime)
sugaTime = lens _sugaTime (\s a -> s { _sugaTime = a })
    . mapping _Time

instance FromXML ScheduledUpdateGroupAction where
    parseXML x = ScheduledUpdateGroupAction
        <$> x .@? "AutoScalingGroupName"
        <*> x .@? "DesiredCapacity"
        <*> x .@? "EndTime"
        <*> x .@? "MaxSize"
        <*> x .@? "MinSize"
        <*> x .@? "Recurrence"
        <*> x .@? "ScheduledActionARN"
        <*> x .@? "ScheduledActionName"
        <*> x .@? "StartTime"
        <*> x .@? "Time"

instance ToQuery ScheduledUpdateGroupAction

data ScalingProcessQuery = ScalingProcessQuery
    { _spqAutoScalingGroupName :: Text
    , _spqScalingProcesses     :: [Text]
    } deriving (Eq, Ord, Show, Generic)

-- | 'ScalingProcessQuery' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'spqAutoScalingGroupName' @::@ 'Text'
--
-- * 'spqScalingProcesses' @::@ ['Text']
--
scalingProcessQuery :: Text -- ^ 'spqAutoScalingGroupName'
                    -> ScalingProcessQuery
scalingProcessQuery p1 = ScalingProcessQuery
    { _spqAutoScalingGroupName = p1
    , _spqScalingProcesses     = mempty
    }

-- | The name or Amazon Resource Name (ARN) of the Auto Scaling group.
spqAutoScalingGroupName :: Lens' ScalingProcessQuery Text
spqAutoScalingGroupName =
    lens _spqAutoScalingGroupName (\s a -> s { _spqAutoScalingGroupName = a })

-- | The processes that you want to suspend or resume, which can include one
-- or more of the following: Launch Terminate HealthCheck ReplaceUnhealthy
-- AZRebalance AlarmNotification ScheduledActions AddToLoadBalancer To
-- suspend all process types, omit this parameter.
spqScalingProcesses :: Lens' ScalingProcessQuery [Text]
spqScalingProcesses =
    lens _spqScalingProcesses (\s a -> s { _spqScalingProcesses = a })

instance FromXML ScalingProcessQuery where
    parseXML x = ScalingProcessQuery
        <$> x .@ "AutoScalingGroupName"
        <*> x .@ "ScalingProcesses"

instance ToQuery ScalingProcessQuery

data Ebs = Ebs
    { _ebsDeleteOnTermination :: Maybe Bool
    , _ebsIops                :: Maybe Nat
    , _ebsSnapshotId          :: Maybe Text
    , _ebsVolumeSize          :: Maybe Nat
    , _ebsVolumeType          :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'Ebs' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ebsDeleteOnTermination' @::@ 'Maybe' 'Bool'
--
-- * 'ebsIops' @::@ 'Maybe' 'Natural'
--
-- * 'ebsSnapshotId' @::@ 'Maybe' 'Text'
--
-- * 'ebsVolumeSize' @::@ 'Maybe' 'Natural'
--
-- * 'ebsVolumeType' @::@ 'Maybe' 'Text'
--
ebs :: Ebs
ebs = Ebs
    { _ebsSnapshotId          = Nothing
    , _ebsVolumeSize          = Nothing
    , _ebsVolumeType          = Nothing
    , _ebsDeleteOnTermination = Nothing
    , _ebsIops                = Nothing
    }

-- | Indicates whether to delete the volume on instance termination. Default:
-- true.
ebsDeleteOnTermination :: Lens' Ebs (Maybe Bool)
ebsDeleteOnTermination =
    lens _ebsDeleteOnTermination (\s a -> s { _ebsDeleteOnTermination = a })

-- | The number of I/O operations per second (IOPS) that the volume supports.
-- The maximum ratio of IOPS to volume size is 30.0 Valid Values: Range is
-- 100 to 4000. Default: None.
ebsIops :: Lens' Ebs (Maybe Natural)
ebsIops = lens _ebsIops (\s a -> s { _ebsIops = a })
    . mapping _Nat

-- | The snapshot ID.
ebsSnapshotId :: Lens' Ebs (Maybe Text)
ebsSnapshotId = lens _ebsSnapshotId (\s a -> s { _ebsSnapshotId = a })

-- | The volume size, in gigabytes. Valid values: If the volume type is io1,
-- the minimum size of the volume is 10. Default: If you're creating the
-- volume from a snapshot, and you don't specify a volume size, the default
-- is the snapshot size. Required: Required when the volume type is io1.
ebsVolumeSize :: Lens' Ebs (Maybe Natural)
ebsVolumeSize = lens _ebsVolumeSize (\s a -> s { _ebsVolumeSize = a })
    . mapping _Nat

-- | The volume type. Valid values: standard | io1 Default: standard.
ebsVolumeType :: Lens' Ebs (Maybe Text)
ebsVolumeType = lens _ebsVolumeType (\s a -> s { _ebsVolumeType = a })

instance FromXML Ebs where
    parseXML x = Ebs
        <$> x .@? "DeleteOnTermination"
        <*> x .@? "Iops"
        <*> x .@? "SnapshotId"
        <*> x .@? "VolumeSize"
        <*> x .@? "VolumeType"

instance ToQuery Ebs

newtype AdjustmentType = AdjustmentType
    { _atAdjustmentType :: Maybe Text
    } deriving (Eq, Ord, Show, Generic, Monoid)

-- | 'AdjustmentType' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'atAdjustmentType' @::@ 'Maybe' 'Text'
--
adjustmentType :: AdjustmentType
adjustmentType = AdjustmentType
    { _atAdjustmentType = Nothing
    }

-- | A policy adjustment type. Valid values are ChangeInCapacity,
-- ExactCapacity, and PercentChangeInCapacity.
atAdjustmentType :: Lens' AdjustmentType (Maybe Text)
atAdjustmentType = lens _atAdjustmentType (\s a -> s { _atAdjustmentType = a })

instance FromXML AdjustmentType where
    parseXML x = AdjustmentType
        <$> x .@? "AdjustmentType"

instance ToQuery AdjustmentType

newtype MetricCollectionType = MetricCollectionType
    { _mctMetric :: Maybe Text
    } deriving (Eq, Ord, Show, Generic, Monoid)

-- | 'MetricCollectionType' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'mctMetric' @::@ 'Maybe' 'Text'
--
metricCollectionType :: MetricCollectionType
metricCollectionType = MetricCollectionType
    { _mctMetric = Nothing
    }

mctMetric :: Lens' MetricCollectionType (Maybe Text)
mctMetric = lens _mctMetric (\s a -> s { _mctMetric = a })

instance FromXML MetricCollectionType where
    parseXML x = MetricCollectionType
        <$> x .@? "Metric"

instance ToQuery MetricCollectionType

data LifecycleHook = LifecycleHook
    { _lhAutoScalingGroupName  :: Maybe Text
    , _lhDefaultResult         :: Maybe Text
    , _lhGlobalTimeout         :: Maybe Int
    , _lhHeartbeatTimeout      :: Maybe Int
    , _lhLifecycleHookName     :: Maybe Text
    , _lhLifecycleTransition   :: Maybe Text
    , _lhNotificationMetadata  :: Maybe Text
    , _lhNotificationTargetARN :: Maybe Text
    , _lhRoleARN               :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'LifecycleHook' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lhAutoScalingGroupName' @::@ 'Maybe' 'Text'
--
-- * 'lhDefaultResult' @::@ 'Maybe' 'Text'
--
-- * 'lhGlobalTimeout' @::@ 'Maybe' 'Int'
--
-- * 'lhHeartbeatTimeout' @::@ 'Maybe' 'Int'
--
-- * 'lhLifecycleHookName' @::@ 'Maybe' 'Text'
--
-- * 'lhLifecycleTransition' @::@ 'Maybe' 'Text'
--
-- * 'lhNotificationMetadata' @::@ 'Maybe' 'Text'
--
-- * 'lhNotificationTargetARN' @::@ 'Maybe' 'Text'
--
-- * 'lhRoleARN' @::@ 'Maybe' 'Text'
--
lifecycleHook :: LifecycleHook
lifecycleHook = LifecycleHook
    { _lhLifecycleHookName     = Nothing
    , _lhAutoScalingGroupName  = Nothing
    , _lhLifecycleTransition   = Nothing
    , _lhNotificationTargetARN = Nothing
    , _lhRoleARN               = Nothing
    , _lhNotificationMetadata  = Nothing
    , _lhHeartbeatTimeout      = Nothing
    , _lhGlobalTimeout         = Nothing
    , _lhDefaultResult         = Nothing
    }

-- | The name of the Auto Scaling group to which the lifecycle action belongs.
lhAutoScalingGroupName :: Lens' LifecycleHook (Maybe Text)
lhAutoScalingGroupName =
    lens _lhAutoScalingGroupName (\s a -> s { _lhAutoScalingGroupName = a })

-- | Defines the action the Auto Scaling group should take when the lifecycle
-- hook timeout elapses or if an unexpected failure occurs. The value for
-- this parameter can be either CONTINUE or ABANDON. The default value for
-- this parameter is CONTINUE.
lhDefaultResult :: Lens' LifecycleHook (Maybe Text)
lhDefaultResult = lens _lhDefaultResult (\s a -> s { _lhDefaultResult = a })

-- | The maximum length of time an instance can remain in a Pending:Wait or
-- Terminating:Wait state. Currently, this value is set at 48 hours.
lhGlobalTimeout :: Lens' LifecycleHook (Maybe Int)
lhGlobalTimeout = lens _lhGlobalTimeout (\s a -> s { _lhGlobalTimeout = a })

-- | Defines the amount of time that can elapse before the lifecycle hook
-- times out. When the lifecycle hook times out, Auto Scaling performs the
-- action defined in the DefaultResult parameter. You can prevent the
-- lifecycle hook from timing out by calling RecordLifecycleActionHeartbeat.
lhHeartbeatTimeout :: Lens' LifecycleHook (Maybe Int)
lhHeartbeatTimeout =
    lens _lhHeartbeatTimeout (\s a -> s { _lhHeartbeatTimeout = a })

-- | The name of the lifecycle action hook.
lhLifecycleHookName :: Lens' LifecycleHook (Maybe Text)
lhLifecycleHookName =
    lens _lhLifecycleHookName (\s a -> s { _lhLifecycleHookName = a })

-- | The Amazon EC2 instance state to which you want to attach the lifecycle
-- hook. See DescribeLifecycleHooks for a list of available lifecycle hook
-- types.
lhLifecycleTransition :: Lens' LifecycleHook (Maybe Text)
lhLifecycleTransition =
    lens _lhLifecycleTransition (\s a -> s { _lhLifecycleTransition = a })

-- | Contains additional information that you want to include any time Auto
-- Scaling sends a message to the notification target.
lhNotificationMetadata :: Lens' LifecycleHook (Maybe Text)
lhNotificationMetadata =
    lens _lhNotificationMetadata (\s a -> s { _lhNotificationMetadata = a })

-- | The ARN of the notification target that Auto Scaling will use to notify
-- you when an instance is in the transition state for the lifecycle hook.
-- This ARN target can be either an SQS queue or an SNS topic. The
-- notification message sent to the target will include: Lifecycle action
-- token User account ID Name of the Auto Scaling group Lifecycle hook name
-- EC2 instance ID Lifecycle transition Notification metadata.
lhNotificationTargetARN :: Lens' LifecycleHook (Maybe Text)
lhNotificationTargetARN =
    lens _lhNotificationTargetARN (\s a -> s { _lhNotificationTargetARN = a })

-- | The ARN of the Amazon IAM role that allows the Auto Scaling group to
-- publish to the specified notification target.
lhRoleARN :: Lens' LifecycleHook (Maybe Text)
lhRoleARN = lens _lhRoleARN (\s a -> s { _lhRoleARN = a })

instance FromXML LifecycleHook where
    parseXML x = LifecycleHook
        <$> x .@? "AutoScalingGroupName"
        <*> x .@? "DefaultResult"
        <*> x .@? "GlobalTimeout"
        <*> x .@? "HeartbeatTimeout"
        <*> x .@? "LifecycleHookName"
        <*> x .@? "LifecycleTransition"
        <*> x .@? "NotificationMetadata"
        <*> x .@? "NotificationTargetARN"
        <*> x .@? "RoleARN"

instance ToQuery LifecycleHook

data Activity = Activity
    { _aActivityId           :: Text
    , _aAutoScalingGroupName :: Text
    , _aCause                :: Text
    , _aDescription          :: Maybe Text
    , _aDetails              :: Maybe Text
    , _aEndTime              :: Maybe RFC822
    , _aProgress             :: Maybe Int
    , _aStartTime            :: RFC822
    , _aStatusCode           :: Text
    , _aStatusMessage        :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'Activity' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'aActivityId' @::@ 'Text'
--
-- * 'aAutoScalingGroupName' @::@ 'Text'
--
-- * 'aCause' @::@ 'Text'
--
-- * 'aDescription' @::@ 'Maybe' 'Text'
--
-- * 'aDetails' @::@ 'Maybe' 'Text'
--
-- * 'aEndTime' @::@ 'Maybe' 'UTCTime'
--
-- * 'aProgress' @::@ 'Maybe' 'Int'
--
-- * 'aStartTime' @::@ 'UTCTime'
--
-- * 'aStatusCode' @::@ 'Text'
--
-- * 'aStatusMessage' @::@ 'Maybe' 'Text'
--
activity :: Text -- ^ 'aActivityId'
         -> Text -- ^ 'aAutoScalingGroupName'
         -> Text -- ^ 'aCause'
         -> UTCTime -- ^ 'aStartTime'
         -> Text -- ^ 'aStatusCode'
         -> Activity
activity p1 p2 p3 p4 p5 = Activity
    { _aActivityId           = p1
    , _aAutoScalingGroupName = p2
    , _aCause                = p3
    , _aStartTime            = withIso _Time (const id) p4
    , _aStatusCode           = p5
    , _aDescription          = Nothing
    , _aEndTime              = Nothing
    , _aStatusMessage        = Nothing
    , _aProgress             = Nothing
    , _aDetails              = Nothing
    }

-- | Specifies the ID of the activity.
aActivityId :: Lens' Activity Text
aActivityId = lens _aActivityId (\s a -> s { _aActivityId = a })

-- | The name of the Auto Scaling group.
aAutoScalingGroupName :: Lens' Activity Text
aAutoScalingGroupName =
    lens _aAutoScalingGroupName (\s a -> s { _aAutoScalingGroupName = a })

-- | Contains the reason the activity was begun.
aCause :: Lens' Activity Text
aCause = lens _aCause (\s a -> s { _aCause = a })

-- | Contains a friendly, more verbose description of the scaling activity.
aDescription :: Lens' Activity (Maybe Text)
aDescription = lens _aDescription (\s a -> s { _aDescription = a })

-- | Contains details of the scaling activity.
aDetails :: Lens' Activity (Maybe Text)
aDetails = lens _aDetails (\s a -> s { _aDetails = a })

-- | Provides the end time of this activity.
aEndTime :: Lens' Activity (Maybe UTCTime)
aEndTime = lens _aEndTime (\s a -> s { _aEndTime = a })
    . mapping _Time

-- | Specifies a value between 0 and 100 that indicates the progress of the
-- activity.
aProgress :: Lens' Activity (Maybe Int)
aProgress = lens _aProgress (\s a -> s { _aProgress = a })

-- | Provides the start time of this activity.
aStartTime :: Lens' Activity UTCTime
aStartTime = lens _aStartTime (\s a -> s { _aStartTime = a })
    . _Time

-- | Contains the current status of the activity.
aStatusCode :: Lens' Activity Text
aStatusCode = lens _aStatusCode (\s a -> s { _aStatusCode = a })

-- | Contains a friendly, more verbose description of the activity status.
aStatusMessage :: Lens' Activity (Maybe Text)
aStatusMessage = lens _aStatusMessage (\s a -> s { _aStatusMessage = a })

instance FromXML Activity where
    parseXML x = Activity
        <$> x .@ "ActivityId"
        <*> x .@ "AutoScalingGroupName"
        <*> x .@ "Cause"
        <*> x .@? "Description"
        <*> x .@? "Details"
        <*> x .@? "EndTime"
        <*> x .@? "Progress"
        <*> x .@ "StartTime"
        <*> x .@ "StatusCode"
        <*> x .@? "StatusMessage"

instance ToQuery Activity

data SuspendedProcess = SuspendedProcess
    { _spProcessName      :: Maybe Text
    , _spSuspensionReason :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'SuspendedProcess' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'spProcessName' @::@ 'Maybe' 'Text'
--
-- * 'spSuspensionReason' @::@ 'Maybe' 'Text'
--
suspendedProcess :: SuspendedProcess
suspendedProcess = SuspendedProcess
    { _spProcessName      = Nothing
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
    parseXML x = SuspendedProcess
        <$> x .@? "ProcessName"
        <*> x .@? "SuspensionReason"

instance ToQuery SuspendedProcess

newtype MetricGranularityType = MetricGranularityType
    { _mgtGranularity :: Maybe Text
    } deriving (Eq, Ord, Show, Generic, Monoid)

-- | 'MetricGranularityType' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'mgtGranularity' @::@ 'Maybe' 'Text'
--
metricGranularityType :: MetricGranularityType
metricGranularityType = MetricGranularityType
    { _mgtGranularity = Nothing
    }

-- | The granularity of a Metric.
mgtGranularity :: Lens' MetricGranularityType (Maybe Text)
mgtGranularity = lens _mgtGranularity (\s a -> s { _mgtGranularity = a })

instance FromXML MetricGranularityType where
    parseXML x = MetricGranularityType
        <$> x .@? "Granularity"

instance ToQuery MetricGranularityType

data Filter = Filter
    { _fName   :: Text
    , _fValues :: [Text]
    } deriving (Eq, Ord, Show, Generic)

-- | 'Filter' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'fName' @::@ 'Text'
--
-- * 'fValues' @::@ ['Text']
--
filter' :: Text -- ^ 'fName'
        -> Filter
filter' p1 = Filter
    { _fName   = p1
    , _fValues = mempty
    }

-- | The name of the filter. Valid Name values are: "auto-scaling-group",
-- "key", "value", and "propagate-at-launch".
fName :: Lens' Filter Text
fName = lens _fName (\s a -> s { _fName = a })

-- | The value of the filter.
fValues :: Lens' Filter [Text]
fValues = lens _fValues (\s a -> s { _fValues = a })

instance FromXML Filter where
    parseXML x = Filter
        <$> x .@ "Name"
        <*> x .@ "Values"

instance ToQuery Filter

newtype ProcessType = ProcessType
    { _ptProcessName :: Text
    } deriving (Eq, Ord, Show, Generic, Monoid, IsString)

-- | 'ProcessType' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ptProcessName' @::@ 'Text'
--
processType :: Text -- ^ 'ptProcessName'
            -> ProcessType
processType p1 = ProcessType
    { _ptProcessName = p1
    }

-- | The name of a process.
ptProcessName :: Lens' ProcessType Text
ptProcessName = lens _ptProcessName (\s a -> s { _ptProcessName = a })

instance FromXML ProcessType where
    parseXML x = ProcessType
        <$> x .@ "ProcessName"

instance ToQuery ProcessType

data Alarm = Alarm
    { _aAlarmARN  :: Maybe Text
    , _aAlarmName :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'Alarm' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'aAlarmARN' @::@ 'Maybe' 'Text'
--
-- * 'aAlarmName' @::@ 'Maybe' 'Text'
--
alarm :: Alarm
alarm = Alarm
    { _aAlarmName = Nothing
    , _aAlarmARN  = Nothing
    }

-- | The Amazon Resource Name (ARN) of the alarm.
aAlarmARN :: Lens' Alarm (Maybe Text)
aAlarmARN = lens _aAlarmARN (\s a -> s { _aAlarmARN = a })

-- | The name of the alarm.
aAlarmName :: Lens' Alarm (Maybe Text)
aAlarmName = lens _aAlarmName (\s a -> s { _aAlarmName = a })

instance FromXML Alarm where
    parseXML x = Alarm
        <$> x .@? "AlarmARN"
        <*> x .@? "AlarmName"

instance ToQuery Alarm

data EnabledMetric = EnabledMetric
    { _emGranularity :: Maybe Text
    , _emMetric      :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'EnabledMetric' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'emGranularity' @::@ 'Maybe' 'Text'
--
-- * 'emMetric' @::@ 'Maybe' 'Text'
--
enabledMetric :: EnabledMetric
enabledMetric = EnabledMetric
    { _emMetric      = Nothing
    , _emGranularity = Nothing
    }

-- | The granularity of the enabled metric.
emGranularity :: Lens' EnabledMetric (Maybe Text)
emGranularity = lens _emGranularity (\s a -> s { _emGranularity = a })

-- | The name of the enabled metric.
emMetric :: Lens' EnabledMetric (Maybe Text)
emMetric = lens _emMetric (\s a -> s { _emMetric = a })

instance FromXML EnabledMetric where
    parseXML x = EnabledMetric
        <$> x .@? "Granularity"
        <*> x .@? "Metric"

instance ToQuery EnabledMetric

data Instance = Instance
    { _iAvailabilityZone        :: Text
    , _iHealthStatus            :: Text
    , _iInstanceId              :: Text
    , _iLaunchConfigurationName :: Text
    , _iLifecycleState          :: Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'Instance' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'iAvailabilityZone' @::@ 'Text'
--
-- * 'iHealthStatus' @::@ 'Text'
--
-- * 'iInstanceId' @::@ 'Text'
--
-- * 'iLaunchConfigurationName' @::@ 'Text'
--
-- * 'iLifecycleState' @::@ 'Text'
--
instance' :: Text -- ^ 'iInstanceId'
          -> Text -- ^ 'iAvailabilityZone'
          -> Text -- ^ 'iLifecycleState'
          -> Text -- ^ 'iHealthStatus'
          -> Text -- ^ 'iLaunchConfigurationName'
          -> Instance
instance' p1 p2 p3 p4 p5 = Instance
    { _iInstanceId              = p1
    , _iAvailabilityZone        = p2
    , _iLifecycleState          = p3
    , _iHealthStatus            = p4
    , _iLaunchConfigurationName = p5
    }

-- | Availability Zones associated with this instance.
iAvailabilityZone :: Lens' Instance Text
iAvailabilityZone =
    lens _iAvailabilityZone (\s a -> s { _iAvailabilityZone = a })

-- | The instance's health status.
iHealthStatus :: Lens' Instance Text
iHealthStatus = lens _iHealthStatus (\s a -> s { _iHealthStatus = a })

-- | Specifies the ID of the Amazon EC2 instance.
iInstanceId :: Lens' Instance Text
iInstanceId = lens _iInstanceId (\s a -> s { _iInstanceId = a })

-- | The launch configuration associated with this instance.
iLaunchConfigurationName :: Lens' Instance Text
iLaunchConfigurationName =
    lens _iLaunchConfigurationName
        (\s a -> s { _iLaunchConfigurationName = a })

-- | Contains a description of the current lifecycle state.
iLifecycleState :: Lens' Instance Text
iLifecycleState = lens _iLifecycleState (\s a -> s { _iLifecycleState = a })

instance FromXML Instance where
    parseXML x = Instance
        <$> x .@ "AvailabilityZone"
        <*> x .@ "HealthStatus"
        <*> x .@ "InstanceId"
        <*> x .@ "LaunchConfigurationName"
        <*> x .@ "LifecycleState"

instance ToQuery Instance

data LifecycleState
    = Detached           -- ^ Detached
    | Detaching          -- ^ Detaching
    | EnteringStandby    -- ^ EnteringStandby
    | InService          -- ^ InService
    | Pending            -- ^ Pending
    | PendingProceed     -- ^ Pending:Proceed
    | PendingWait        -- ^ Pending:Wait
    | Quarantined        -- ^ Quarantined
    | Standby            -- ^ Standby
    | Terminated         -- ^ Terminated
    | Terminating        -- ^ Terminating
    | TerminatingProceed -- ^ Terminating:Proceed
    | TerminatingWait    -- ^ Terminating:Wait
      deriving (Eq, Ord, Show, Generic, Enum)

instance Hashable LifecycleState

instance FromText LifecycleState where
    parser = match "Detached"            Detached
         <|> match "Detaching"           Detaching
         <|> match "EnteringStandby"     EnteringStandby
         <|> match "InService"           InService
         <|> match "Pending"             Pending
         <|> match "Pending:Proceed"     PendingProceed
         <|> match "Pending:Wait"        PendingWait
         <|> match "Quarantined"         Quarantined
         <|> match "Standby"             Standby
         <|> match "Terminated"          Terminated
         <|> match "Terminating"         Terminating
         <|> match "Terminating:Proceed" TerminatingProceed
         <|> match "Terminating:Wait"    TerminatingWait

instance ToText LifecycleState where
    toText = \case
        Detached           -> "Detached"
        Detaching          -> "Detaching"
        EnteringStandby    -> "EnteringStandby"
        InService          -> "InService"
        Pending            -> "Pending"
        PendingProceed     -> "Pending:Proceed"
        PendingWait        -> "Pending:Wait"
        Quarantined        -> "Quarantined"
        Standby            -> "Standby"
        Terminated         -> "Terminated"
        Terminating        -> "Terminating"
        TerminatingProceed -> "Terminating:Proceed"
        TerminatingWait    -> "Terminating:Wait"

instance FromXML LifecycleState where
    parseXML = parseXMLText "LifecycleState"

instance ToQuery LifecycleState

data AutoScalingInstanceDetails = AutoScalingInstanceDetails
    { _asidAutoScalingGroupName    :: Text
    , _asidAvailabilityZone        :: Text
    , _asidHealthStatus            :: Text
    , _asidInstanceId              :: Text
    , _asidLaunchConfigurationName :: Text
    , _asidLifecycleState          :: Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'AutoScalingInstanceDetails' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'asidAutoScalingGroupName' @::@ 'Text'
--
-- * 'asidAvailabilityZone' @::@ 'Text'
--
-- * 'asidHealthStatus' @::@ 'Text'
--
-- * 'asidInstanceId' @::@ 'Text'
--
-- * 'asidLaunchConfigurationName' @::@ 'Text'
--
-- * 'asidLifecycleState' @::@ 'Text'
--
autoScalingInstanceDetails :: Text -- ^ 'asidInstanceId'
                           -> Text -- ^ 'asidAutoScalingGroupName'
                           -> Text -- ^ 'asidAvailabilityZone'
                           -> Text -- ^ 'asidLifecycleState'
                           -> Text -- ^ 'asidHealthStatus'
                           -> Text -- ^ 'asidLaunchConfigurationName'
                           -> AutoScalingInstanceDetails
autoScalingInstanceDetails p1 p2 p3 p4 p5 p6 = AutoScalingInstanceDetails
    { _asidInstanceId              = p1
    , _asidAutoScalingGroupName    = p2
    , _asidAvailabilityZone        = p3
    , _asidLifecycleState          = p4
    , _asidHealthStatus            = p5
    , _asidLaunchConfigurationName = p6
    }

-- | The name of the Auto Scaling group associated with this instance.
asidAutoScalingGroupName :: Lens' AutoScalingInstanceDetails Text
asidAutoScalingGroupName =
    lens _asidAutoScalingGroupName
        (\s a -> s { _asidAutoScalingGroupName = a })

-- | The Availability Zone in which this instance resides.
asidAvailabilityZone :: Lens' AutoScalingInstanceDetails Text
asidAvailabilityZone =
    lens _asidAvailabilityZone (\s a -> s { _asidAvailabilityZone = a })

-- | The health status of this instance. "Healthy" means that the instance is
-- healthy and should remain in service. "Unhealthy" means that the instance
-- is unhealthy. Auto Scaling should terminate and replace it.
asidHealthStatus :: Lens' AutoScalingInstanceDetails Text
asidHealthStatus = lens _asidHealthStatus (\s a -> s { _asidHealthStatus = a })

-- | The instance ID of the Amazon EC2 instance.
asidInstanceId :: Lens' AutoScalingInstanceDetails Text
asidInstanceId = lens _asidInstanceId (\s a -> s { _asidInstanceId = a })

-- | The launch configuration associated with this instance.
asidLaunchConfigurationName :: Lens' AutoScalingInstanceDetails Text
asidLaunchConfigurationName =
    lens _asidLaunchConfigurationName
        (\s a -> s { _asidLaunchConfigurationName = a })

-- | The life cycle state of this instance. for more information, see Instance
-- Lifecycle State in the Auto Scaling Developer Guide.
asidLifecycleState :: Lens' AutoScalingInstanceDetails Text
asidLifecycleState =
    lens _asidLifecycleState (\s a -> s { _asidLifecycleState = a })

instance FromXML AutoScalingInstanceDetails where
    parseXML x = AutoScalingInstanceDetails
        <$> x .@ "AutoScalingGroupName"
        <*> x .@ "AvailabilityZone"
        <*> x .@ "HealthStatus"
        <*> x .@ "InstanceId"
        <*> x .@ "LaunchConfigurationName"
        <*> x .@ "LifecycleState"

instance ToQuery AutoScalingInstanceDetails

data ScalingActivityStatusCode
    = Cancelled                       -- ^ Cancelled
    | Failed                          -- ^ Failed
    | InProgress                      -- ^ InProgress
    | MidLifecycleAction              -- ^ MidLifecycleAction
    | PreInService                    -- ^ PreInService
    | Successful                      -- ^ Successful
    | WaitingForELBConnectionDraining -- ^ WaitingForELBConnectionDraining
    | WaitingForInstanceId            -- ^ WaitingForInstanceId
    | WaitingForSpotInstanceId        -- ^ WaitingForSpotInstanceId
    | WaitingForSpotInstanceRequestId -- ^ WaitingForSpotInstanceRequestId
      deriving (Eq, Ord, Show, Generic, Enum)

instance Hashable ScalingActivityStatusCode

instance FromText ScalingActivityStatusCode where
    parser = match "Cancelled"                       Cancelled
         <|> match "Failed"                          Failed
         <|> match "InProgress"                      InProgress
         <|> match "MidLifecycleAction"              MidLifecycleAction
         <|> match "PreInService"                    PreInService
         <|> match "Successful"                      Successful
         <|> match "WaitingForELBConnectionDraining" WaitingForELBConnectionDraining
         <|> match "WaitingForInstanceId"            WaitingForInstanceId
         <|> match "WaitingForSpotInstanceId"        WaitingForSpotInstanceId
         <|> match "WaitingForSpotInstanceRequestId" WaitingForSpotInstanceRequestId

instance ToText ScalingActivityStatusCode where
    toText = \case
        Cancelled                       -> "Cancelled"
        Failed                          -> "Failed"
        InProgress                      -> "InProgress"
        MidLifecycleAction              -> "MidLifecycleAction"
        PreInService                    -> "PreInService"
        Successful                      -> "Successful"
        WaitingForELBConnectionDraining -> "WaitingForELBConnectionDraining"
        WaitingForInstanceId            -> "WaitingForInstanceId"
        WaitingForSpotInstanceId        -> "WaitingForSpotInstanceId"
        WaitingForSpotInstanceRequestId -> "WaitingForSpotInstanceRequestId"

instance FromXML ScalingActivityStatusCode where
    parseXML = parseXMLText "ScalingActivityStatusCode"

instance ToQuery ScalingActivityStatusCode
