{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}
{-# LANGUAGE ViewPatterns                #-}

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
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

module Network.AWS.AutoScaling.Types
    (
    -- * Service
      AutoScaling
    -- ** Error
    , RESTError (..)
    -- ** XML
    , ns

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
import Network.AWS.Signing
import qualified GHC.Exts

-- | Version @2011-01-01@ of the Amazon Auto Scaling service.
data AutoScaling

instance AWSService AutoScaling where
    type Sg AutoScaling = V4
    type Er AutoScaling = RESTError

    service = Service
        { _svcAbbrev       = "AutoScaling"
        , _svcPrefix       = "autoscaling"
        , _svcVersion      = "2011-01-01"
        , _svcTargetPrefix = Nothing
        , _svcJSONVersion  = Nothing
        , _svcHandle       = restError statusSuccess
        , _svcDelay        = delay
        , _svcRetry        = retry
        }

delay :: Delay
delay = Exp 0.05 2 5
{-# INLINE delay #-}

retry :: AWSErrorCode -> Status -> a -> Retry
retry (statusCode -> s) (awsErrorCode -> e)
    | s == 500  = True -- General Server Error
    | s == 509  = True -- Limit Exceeded
    | s == 503  = True -- Service Unavailable
    | s == 400  = "Throttling" == e -- Throttling
    | otherwise = False
{-# INLINE retry #-}

ns :: Text
ns = "http://autoscaling.amazonaws.com/doc/2011-01-01/"
{-# INLINE ns #-}

data TagDescription = TagDescription
    { _tdKey               :: Text
    , _tdPropagateAtLaunch :: Bool
    , _tdResourceId        :: Text
    , _tdResourceType      :: Text
    , _tdValue             :: Text
    } deriving (Eq, Ord, Show)

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

-- | The tag key.
tdKey :: Lens' TagDescription Text
tdKey = lens _tdKey (\s a -> s { _tdKey = a })

-- | Specifies whether the tag is applied to instances launched after the tag is
-- created. The same behavior applies to updates: If you change a tag, it is
-- applied to all instances launched after you made the change.
tdPropagateAtLaunch :: Lens' TagDescription Bool
tdPropagateAtLaunch =
    lens _tdPropagateAtLaunch (\s a -> s { _tdPropagateAtLaunch = a })

-- | The name of the group.
tdResourceId :: Lens' TagDescription Text
tdResourceId = lens _tdResourceId (\s a -> s { _tdResourceId = a })

-- | The kind of resource to which the tag is applied. Currently, Auto Scaling
-- supports the 'auto-scaling-group' resource type.
tdResourceType :: Lens' TagDescription Text
tdResourceType = lens _tdResourceType (\s a -> s { _tdResourceType = a })

-- | The tag value.
tdValue :: Lens' TagDescription Text
tdValue = lens _tdValue (\s a -> s { _tdValue = a })

instance FromXML TagDescription where
    parseXML x = TagDescription
        <$> x .@  "Key"
        <*> x .@  "PropagateAtLaunch"
        <*> x .@  "ResourceId"
        <*> x .@  "ResourceType"
        <*> x .@  "Value"

instance ToQuery TagDescription where
    toQuery TagDescription{..} = mconcat
        [ "Key"               =? _tdKey
        , "PropagateAtLaunch" =? _tdPropagateAtLaunch
        , "ResourceId"        =? _tdResourceId
        , "ResourceType"      =? _tdResourceType
        , "Value"             =? _tdValue
        ]

data Tag = Tag
    { _tagKey               :: Text
    , _tagPropagateAtLaunch :: Bool
    , _tagResourceId        :: Text
    , _tagResourceType      :: Text
    , _tagValue             :: Text
    } deriving (Eq, Ord, Show)

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

-- | The tag key.
tagKey :: Lens' Tag Text
tagKey = lens _tagKey (\s a -> s { _tagKey = a })

-- | Specifies whether the tag is applied to instances launched after the tag is
-- created. The same behavior applies to updates: If you change a tag, it is
-- applied to all instances launched after you made the change.
tagPropagateAtLaunch :: Lens' Tag Bool
tagPropagateAtLaunch =
    lens _tagPropagateAtLaunch (\s a -> s { _tagPropagateAtLaunch = a })

-- | The name of the group.
tagResourceId :: Lens' Tag Text
tagResourceId = lens _tagResourceId (\s a -> s { _tagResourceId = a })

-- | The kind of resource to which the tag is applied. Currently, Auto Scaling
-- supports the 'auto-scaling-group' resource type.
tagResourceType :: Lens' Tag Text
tagResourceType = lens _tagResourceType (\s a -> s { _tagResourceType = a })

-- | The tag value.
tagValue :: Lens' Tag Text
tagValue = lens _tagValue (\s a -> s { _tagValue = a })

instance FromXML Tag where
    parseXML x = Tag
        <$> x .@  "Key"
        <*> x .@  "PropagateAtLaunch"
        <*> x .@  "ResourceId"
        <*> x .@  "ResourceType"
        <*> x .@  "Value"

instance ToQuery Tag where
    toQuery Tag{..} = mconcat
        [ "Key"               =? _tagKey
        , "PropagateAtLaunch" =? _tagPropagateAtLaunch
        , "ResourceId"        =? _tagResourceId
        , "ResourceType"      =? _tagResourceType
        , "Value"             =? _tagValue
        ]

data NotificationConfiguration = NotificationConfiguration
    { _ncAutoScalingGroupName :: Maybe Text
    , _ncNotificationType     :: Maybe Text
    , _ncTopicARN             :: Maybe Text
    } deriving (Eq, Ord, Show)

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

-- | The name of the group.
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

instance ToQuery NotificationConfiguration where
    toQuery NotificationConfiguration{..} = mconcat
        [ "AutoScalingGroupName" =? _ncAutoScalingGroupName
        , "NotificationType"     =? _ncNotificationType
        , "TopicARN"             =? _ncTopicARN
        ]

data BlockDeviceMapping = BlockDeviceMapping
    { _bdmDeviceName  :: Text
    , _bdmEbs         :: Maybe Ebs
    , _bdmNoDevice    :: Maybe Bool
    , _bdmVirtualName :: Maybe Text
    } deriving (Eq, Show)

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

-- | The device name exposed to the EC2 instance (for example, '/dev/sdh' or 'xvdh').
bdmDeviceName :: Lens' BlockDeviceMapping Text
bdmDeviceName = lens _bdmDeviceName (\s a -> s { _bdmDeviceName = a })

-- | The information about the Amazon EBS volume.
bdmEbs :: Lens' BlockDeviceMapping (Maybe Ebs)
bdmEbs = lens _bdmEbs (\s a -> s { _bdmEbs = a })

-- | Suppresses a device mapping.
--
-- If 'NoDevice' is set to 'true' for the root device, the instance might fail the
-- EC2 health check. Auto Scaling launches a replacement instance if the
-- instance fails the health check.
bdmNoDevice :: Lens' BlockDeviceMapping (Maybe Bool)
bdmNoDevice = lens _bdmNoDevice (\s a -> s { _bdmNoDevice = a })

-- | The name of the virtual device, 'ephemeral0' to 'ephemeral3'.
bdmVirtualName :: Lens' BlockDeviceMapping (Maybe Text)
bdmVirtualName = lens _bdmVirtualName (\s a -> s { _bdmVirtualName = a })

instance FromXML BlockDeviceMapping where
    parseXML x = BlockDeviceMapping
        <$> x .@  "DeviceName"
        <*> x .@? "Ebs"
        <*> x .@? "NoDevice"
        <*> x .@? "VirtualName"

instance ToQuery BlockDeviceMapping where
    toQuery BlockDeviceMapping{..} = mconcat
        [ "DeviceName"  =? _bdmDeviceName
        , "Ebs"         =? _bdmEbs
        , "NoDevice"    =? _bdmNoDevice
        , "VirtualName" =? _bdmVirtualName
        ]

data LaunchConfiguration = LaunchConfiguration
    { _lcAssociatePublicIpAddress :: Maybe Bool
    , _lcBlockDeviceMappings      :: List "member" BlockDeviceMapping
    , _lcCreatedTime              :: ISO8601
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
    , _lcSecurityGroups           :: List "member" Text
    , _lcSpotPrice                :: Maybe Text
    , _lcUserData                 :: Maybe Text
    } deriving (Eq, Show)

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

-- | Specifies whether the EC2 instances are associated with a public IP address ('true') or not ('false').
lcAssociatePublicIpAddress :: Lens' LaunchConfiguration (Maybe Bool)
lcAssociatePublicIpAddress =
    lens _lcAssociatePublicIpAddress
        (\s a -> s { _lcAssociatePublicIpAddress = a })

-- | A block device mapping that specifies how block devices are exposed to the
-- instance. Each mapping is made up of a 'virtualName' and a 'deviceName'.
lcBlockDeviceMappings :: Lens' LaunchConfiguration [BlockDeviceMapping]
lcBlockDeviceMappings =
    lens _lcBlockDeviceMappings (\s a -> s { _lcBlockDeviceMappings = a })
        . _List

-- | The creation date and time for the launch configuration.
lcCreatedTime :: Lens' LaunchConfiguration UTCTime
lcCreatedTime = lens _lcCreatedTime (\s a -> s { _lcCreatedTime = a }) . _Time

-- | Controls whether the instance is optimized for EBS I/O ('true') or not ('false').
lcEbsOptimized :: Lens' LaunchConfiguration (Maybe Bool)
lcEbsOptimized = lens _lcEbsOptimized (\s a -> s { _lcEbsOptimized = a })

-- | The name or Amazon Resource Name (ARN) of the instance profile associated
-- with the IAM role for the instance.
lcIamInstanceProfile :: Lens' LaunchConfiguration (Maybe Text)
lcIamInstanceProfile =
    lens _lcIamInstanceProfile (\s a -> s { _lcIamInstanceProfile = a })

-- | The ID of the Amazon Machine Image (AMI).
lcImageId :: Lens' LaunchConfiguration Text
lcImageId = lens _lcImageId (\s a -> s { _lcImageId = a })

-- | Controls whether instances in this group are launched with detailed
-- monitoring.
lcInstanceMonitoring :: Lens' LaunchConfiguration (Maybe InstanceMonitoring)
lcInstanceMonitoring =
    lens _lcInstanceMonitoring (\s a -> s { _lcInstanceMonitoring = a })

-- | The instance type for the EC2 instances.
lcInstanceType :: Lens' LaunchConfiguration Text
lcInstanceType = lens _lcInstanceType (\s a -> s { _lcInstanceType = a })

-- | The ID of the kernel associated with the AMI.
lcKernelId :: Lens' LaunchConfiguration (Maybe Text)
lcKernelId = lens _lcKernelId (\s a -> s { _lcKernelId = a })

-- | The name of the key pair.
lcKeyName :: Lens' LaunchConfiguration (Maybe Text)
lcKeyName = lens _lcKeyName (\s a -> s { _lcKeyName = a })

-- | The Amazon Resource Name (ARN) of the launch configuration.
lcLaunchConfigurationARN :: Lens' LaunchConfiguration (Maybe Text)
lcLaunchConfigurationARN =
    lens _lcLaunchConfigurationARN
        (\s a -> s { _lcLaunchConfigurationARN = a })

-- | The name of the launch configuration.
lcLaunchConfigurationName :: Lens' LaunchConfiguration Text
lcLaunchConfigurationName =
    lens _lcLaunchConfigurationName
        (\s a -> s { _lcLaunchConfigurationName = a })

-- | The tenancy of the instance, either 'default' or 'dedicated'. An instance with 'dedicated' tenancy runs in an isolated, single-tenant hardware and can only be launched
-- in a VPC.
lcPlacementTenancy :: Lens' LaunchConfiguration (Maybe Text)
lcPlacementTenancy =
    lens _lcPlacementTenancy (\s a -> s { _lcPlacementTenancy = a })

-- | The ID of the RAM disk associated with the AMI.
lcRamdiskId :: Lens' LaunchConfiguration (Maybe Text)
lcRamdiskId = lens _lcRamdiskId (\s a -> s { _lcRamdiskId = a })

-- | The security groups to associate with the EC2 instances.
lcSecurityGroups :: Lens' LaunchConfiguration [Text]
lcSecurityGroups = lens _lcSecurityGroups (\s a -> s { _lcSecurityGroups = a }) . _List

-- | The price to bid when launching Spot Instances.
lcSpotPrice :: Lens' LaunchConfiguration (Maybe Text)
lcSpotPrice = lens _lcSpotPrice (\s a -> s { _lcSpotPrice = a })

-- | The user data available to the EC2 instances.
lcUserData :: Lens' LaunchConfiguration (Maybe Text)
lcUserData = lens _lcUserData (\s a -> s { _lcUserData = a })

instance FromXML LaunchConfiguration where
    parseXML x = LaunchConfiguration
        <$> x .@? "AssociatePublicIpAddress"
        <*> x .@? "BlockDeviceMappings" .!@ mempty
        <*> x .@  "CreatedTime"
        <*> x .@? "EbsOptimized"
        <*> x .@? "IamInstanceProfile"
        <*> x .@  "ImageId"
        <*> x .@? "InstanceMonitoring"
        <*> x .@  "InstanceType"
        <*> x .@? "KernelId"
        <*> x .@? "KeyName"
        <*> x .@? "LaunchConfigurationARN"
        <*> x .@  "LaunchConfigurationName"
        <*> x .@? "PlacementTenancy"
        <*> x .@? "RamdiskId"
        <*> x .@? "SecurityGroups" .!@ mempty
        <*> x .@? "SpotPrice"
        <*> x .@? "UserData"

instance ToQuery LaunchConfiguration where
    toQuery LaunchConfiguration{..} = mconcat
        [ "AssociatePublicIpAddress" =? _lcAssociatePublicIpAddress
        , "BlockDeviceMappings"      =? _lcBlockDeviceMappings
        , "CreatedTime"              =? _lcCreatedTime
        , "EbsOptimized"             =? _lcEbsOptimized
        , "IamInstanceProfile"       =? _lcIamInstanceProfile
        , "ImageId"                  =? _lcImageId
        , "InstanceMonitoring"       =? _lcInstanceMonitoring
        , "InstanceType"             =? _lcInstanceType
        , "KernelId"                 =? _lcKernelId
        , "KeyName"                  =? _lcKeyName
        , "LaunchConfigurationARN"   =? _lcLaunchConfigurationARN
        , "LaunchConfigurationName"  =? _lcLaunchConfigurationName
        , "PlacementTenancy"         =? _lcPlacementTenancy
        , "RamdiskId"                =? _lcRamdiskId
        , "SecurityGroups"           =? _lcSecurityGroups
        , "SpotPrice"                =? _lcSpotPrice
        , "UserData"                 =? _lcUserData
        ]

data AutoScalingGroup = AutoScalingGroup
    { _asgAutoScalingGroupARN     :: Maybe Text
    , _asgAutoScalingGroupName    :: Text
    , _asgAvailabilityZones       :: List1 "member" Text
    , _asgCreatedTime             :: ISO8601
    , _asgDefaultCooldown         :: Int
    , _asgDesiredCapacity         :: Int
    , _asgEnabledMetrics          :: List "member" EnabledMetric
    , _asgHealthCheckGracePeriod  :: Maybe Int
    , _asgHealthCheckType         :: Text
    , _asgInstances               :: List "member" Instance
    , _asgLaunchConfigurationName :: Text
    , _asgLoadBalancerNames       :: List "member" Text
    , _asgMaxSize                 :: Int
    , _asgMinSize                 :: Int
    , _asgPlacementGroup          :: Maybe Text
    , _asgStatus                  :: Maybe Text
    , _asgSuspendedProcesses      :: List "member" SuspendedProcess
    , _asgTags                    :: List "member" TagDescription
    , _asgTerminationPolicies     :: List "member" Text
    , _asgVPCZoneIdentifier       :: Maybe Text
    } deriving (Eq, Show)

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

-- | The Amazon Resource Name (ARN) of the group.
asgAutoScalingGroupARN :: Lens' AutoScalingGroup (Maybe Text)
asgAutoScalingGroupARN =
    lens _asgAutoScalingGroupARN (\s a -> s { _asgAutoScalingGroupARN = a })

-- | The name of the group.
asgAutoScalingGroupName :: Lens' AutoScalingGroup Text
asgAutoScalingGroupName =
    lens _asgAutoScalingGroupName (\s a -> s { _asgAutoScalingGroupName = a })

-- | One or more Availability Zones for the group.
asgAvailabilityZones :: Lens' AutoScalingGroup (NonEmpty Text)
asgAvailabilityZones =
    lens _asgAvailabilityZones (\s a -> s { _asgAvailabilityZones = a })
        . _List1

-- | The date and time the group was created.
asgCreatedTime :: Lens' AutoScalingGroup UTCTime
asgCreatedTime = lens _asgCreatedTime (\s a -> s { _asgCreatedTime = a }) . _Time

-- | The number of seconds after a scaling activity completes before any further
-- scaling activities can start.
asgDefaultCooldown :: Lens' AutoScalingGroup Int
asgDefaultCooldown =
    lens _asgDefaultCooldown (\s a -> s { _asgDefaultCooldown = a })

-- | The size of the group.
asgDesiredCapacity :: Lens' AutoScalingGroup Int
asgDesiredCapacity =
    lens _asgDesiredCapacity (\s a -> s { _asgDesiredCapacity = a })

-- | The metrics enabled for this Auto Scaling group.
asgEnabledMetrics :: Lens' AutoScalingGroup [EnabledMetric]
asgEnabledMetrics =
    lens _asgEnabledMetrics (\s a -> s { _asgEnabledMetrics = a })
        . _List

-- | The amount of time that Auto Scaling waits before checking an instance's
-- health status. The grace period begins when an instance comes into service.
asgHealthCheckGracePeriod :: Lens' AutoScalingGroup (Maybe Int)
asgHealthCheckGracePeriod =
    lens _asgHealthCheckGracePeriod
        (\s a -> s { _asgHealthCheckGracePeriod = a })

-- | The service of interest for the health status check, which can be either 'EC2'
-- for Amazon EC2 or 'ELB' for Elastic Load Balancing.
asgHealthCheckType :: Lens' AutoScalingGroup Text
asgHealthCheckType =
    lens _asgHealthCheckType (\s a -> s { _asgHealthCheckType = a })

-- | The EC2 instances associated with the group.
asgInstances :: Lens' AutoScalingGroup [Instance]
asgInstances = lens _asgInstances (\s a -> s { _asgInstances = a }) . _List

-- | The name of the associated launch configuration.
asgLaunchConfigurationName :: Lens' AutoScalingGroup Text
asgLaunchConfigurationName =
    lens _asgLaunchConfigurationName
        (\s a -> s { _asgLaunchConfigurationName = a })

-- | One or more load balancers associated with the group.
asgLoadBalancerNames :: Lens' AutoScalingGroup [Text]
asgLoadBalancerNames =
    lens _asgLoadBalancerNames (\s a -> s { _asgLoadBalancerNames = a })
        . _List

-- | The maximum size of the group.
asgMaxSize :: Lens' AutoScalingGroup Int
asgMaxSize = lens _asgMaxSize (\s a -> s { _asgMaxSize = a })

-- | The minimum size of the group.
asgMinSize :: Lens' AutoScalingGroup Int
asgMinSize = lens _asgMinSize (\s a -> s { _asgMinSize = a })

-- | The name of the placement group into which you'll launch your instances, if
-- any. For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/placement-groups.html Placement Groups>.
asgPlacementGroup :: Lens' AutoScalingGroup (Maybe Text)
asgPlacementGroup =
    lens _asgPlacementGroup (\s a -> s { _asgPlacementGroup = a })

-- | The current state of the Auto Scaling group when a 'DeleteAutoScalingGroup'
-- action is in progress.
asgStatus :: Lens' AutoScalingGroup (Maybe Text)
asgStatus = lens _asgStatus (\s a -> s { _asgStatus = a })

-- | The suspended processes associated with the group.
asgSuspendedProcesses :: Lens' AutoScalingGroup [SuspendedProcess]
asgSuspendedProcesses =
    lens _asgSuspendedProcesses (\s a -> s { _asgSuspendedProcesses = a })
        . _List

-- | The tags for the Auto Scaling group.
asgTags :: Lens' AutoScalingGroup [TagDescription]
asgTags = lens _asgTags (\s a -> s { _asgTags = a }) . _List

-- | The termination policies for this Auto Scaling group.
asgTerminationPolicies :: Lens' AutoScalingGroup [Text]
asgTerminationPolicies =
    lens _asgTerminationPolicies (\s a -> s { _asgTerminationPolicies = a })
        . _List

-- | One or more subnet IDs, if applicable, separated by commas.
--
-- If you specify 'VPCZoneIdentifier' and 'AvailabilityZones', ensure that the
-- Availability Zones of the subnets match the values for 'AvailabilityZones'.
asgVPCZoneIdentifier :: Lens' AutoScalingGroup (Maybe Text)
asgVPCZoneIdentifier =
    lens _asgVPCZoneIdentifier (\s a -> s { _asgVPCZoneIdentifier = a })

instance FromXML AutoScalingGroup where
    parseXML x = AutoScalingGroup
        <$> x .@? "AutoScalingGroupARN"
        <*> x .@  "AutoScalingGroupName"
        <*> x .@  "AvailabilityZones"
        <*> x .@  "CreatedTime"
        <*> x .@  "DefaultCooldown"
        <*> x .@  "DesiredCapacity"
        <*> x .@? "EnabledMetrics" .!@ mempty
        <*> x .@? "HealthCheckGracePeriod"
        <*> x .@  "HealthCheckType"
        <*> x .@? "Instances" .!@ mempty
        <*> x .@  "LaunchConfigurationName"
        <*> x .@? "LoadBalancerNames" .!@ mempty
        <*> x .@  "MaxSize"
        <*> x .@  "MinSize"
        <*> x .@? "PlacementGroup"
        <*> x .@? "Status"
        <*> x .@? "SuspendedProcesses" .!@ mempty
        <*> x .@? "Tags" .!@ mempty
        <*> x .@? "TerminationPolicies" .!@ mempty
        <*> x .@? "VPCZoneIdentifier"

instance ToQuery AutoScalingGroup where
    toQuery AutoScalingGroup{..} = mconcat
        [ "AutoScalingGroupARN"     =? _asgAutoScalingGroupARN
        , "AutoScalingGroupName"    =? _asgAutoScalingGroupName
        , "AvailabilityZones"       =? _asgAvailabilityZones
        , "CreatedTime"             =? _asgCreatedTime
        , "DefaultCooldown"         =? _asgDefaultCooldown
        , "DesiredCapacity"         =? _asgDesiredCapacity
        , "EnabledMetrics"          =? _asgEnabledMetrics
        , "HealthCheckGracePeriod"  =? _asgHealthCheckGracePeriod
        , "HealthCheckType"         =? _asgHealthCheckType
        , "Instances"               =? _asgInstances
        , "LaunchConfigurationName" =? _asgLaunchConfigurationName
        , "LoadBalancerNames"       =? _asgLoadBalancerNames
        , "MaxSize"                 =? _asgMaxSize
        , "MinSize"                 =? _asgMinSize
        , "PlacementGroup"          =? _asgPlacementGroup
        , "Status"                  =? _asgStatus
        , "SuspendedProcesses"      =? _asgSuspendedProcesses
        , "Tags"                    =? _asgTags
        , "TerminationPolicies"     =? _asgTerminationPolicies
        , "VPCZoneIdentifier"       =? _asgVPCZoneIdentifier
        ]

data ScalingPolicy = ScalingPolicy
    { _sp1AdjustmentType       :: Maybe Text
    , _sp1Alarms               :: List "member" Alarm
    , _sp1AutoScalingGroupName :: Maybe Text
    , _sp1Cooldown             :: Maybe Int
    , _sp1MinAdjustmentStep    :: Maybe Int
    , _sp1PolicyARN            :: Maybe Text
    , _sp1PolicyName           :: Maybe Text
    , _sp1ScalingAdjustment    :: Maybe Int
    } deriving (Eq, Show)

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

-- | Specifies whether the 'ScalingAdjustment' is an absolute number or a percentage
-- of the current capacity. Valid values are 'ChangeInCapacity', 'ExactCapacity',
-- and 'PercentChangeInCapacity'.
sp1AdjustmentType :: Lens' ScalingPolicy (Maybe Text)
sp1AdjustmentType =
    lens _sp1AdjustmentType (\s a -> s { _sp1AdjustmentType = a })

-- | The CloudWatch Alarms related to the policy.
sp1Alarms :: Lens' ScalingPolicy [Alarm]
sp1Alarms = lens _sp1Alarms (\s a -> s { _sp1Alarms = a }) . _List

-- | The name of the Auto Scaling group associated with this scaling policy.
sp1AutoScalingGroupName :: Lens' ScalingPolicy (Maybe Text)
sp1AutoScalingGroupName =
    lens _sp1AutoScalingGroupName (\s a -> s { _sp1AutoScalingGroupName = a })

-- | The amount of time, in seconds, after a scaling activity completes before any
-- further trigger-related scaling activities can start.
sp1Cooldown :: Lens' ScalingPolicy (Maybe Int)
sp1Cooldown = lens _sp1Cooldown (\s a -> s { _sp1Cooldown = a })

-- | Changes the 'DesiredCapacity' of the Auto Scaling group by at least the
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

-- | The number associated with the specified adjustment type. A positive value
-- adds to the current capacity and a negative value removes from the current
-- capacity.
sp1ScalingAdjustment :: Lens' ScalingPolicy (Maybe Int)
sp1ScalingAdjustment =
    lens _sp1ScalingAdjustment (\s a -> s { _sp1ScalingAdjustment = a })

instance FromXML ScalingPolicy where
    parseXML x = ScalingPolicy
        <$> x .@? "AdjustmentType"
        <*> x .@? "Alarms" .!@ mempty
        <*> x .@? "AutoScalingGroupName"
        <*> x .@? "Cooldown"
        <*> x .@? "MinAdjustmentStep"
        <*> x .@? "PolicyARN"
        <*> x .@? "PolicyName"
        <*> x .@? "ScalingAdjustment"

instance ToQuery ScalingPolicy where
    toQuery ScalingPolicy{..} = mconcat
        [ "AdjustmentType"       =? _sp1AdjustmentType
        , "Alarms"               =? _sp1Alarms
        , "AutoScalingGroupName" =? _sp1AutoScalingGroupName
        , "Cooldown"             =? _sp1Cooldown
        , "MinAdjustmentStep"    =? _sp1MinAdjustmentStep
        , "PolicyARN"            =? _sp1PolicyARN
        , "PolicyName"           =? _sp1PolicyName
        , "ScalingAdjustment"    =? _sp1ScalingAdjustment
        ]

newtype InstanceMonitoring = InstanceMonitoring
    { _imEnabled :: Maybe Bool
    } deriving (Eq, Ord, Show)

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

-- | If 'True', instance monitoring is enabled.
imEnabled :: Lens' InstanceMonitoring (Maybe Bool)
imEnabled = lens _imEnabled (\s a -> s { _imEnabled = a })

instance FromXML InstanceMonitoring where
    parseXML x = InstanceMonitoring
        <$> x .@? "Enabled"

instance ToQuery InstanceMonitoring where
    toQuery InstanceMonitoring{..} = mconcat
        [ "Enabled" =? _imEnabled
        ]

data ScheduledUpdateGroupAction = ScheduledUpdateGroupAction
    { _sugaAutoScalingGroupName :: Maybe Text
    , _sugaDesiredCapacity      :: Maybe Int
    , _sugaEndTime              :: Maybe ISO8601
    , _sugaMaxSize              :: Maybe Int
    , _sugaMinSize              :: Maybe Int
    , _sugaRecurrence           :: Maybe Text
    , _sugaScheduledActionARN   :: Maybe Text
    , _sugaScheduledActionName  :: Maybe Text
    , _sugaStartTime            :: Maybe ISO8601
    , _sugaTime                 :: Maybe ISO8601
    } deriving (Eq, Ord, Show)

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

-- | The name of the group.
sugaAutoScalingGroupName :: Lens' ScheduledUpdateGroupAction (Maybe Text)
sugaAutoScalingGroupName =
    lens _sugaAutoScalingGroupName
        (\s a -> s { _sugaAutoScalingGroupName = a })

-- | The number of instances you prefer to maintain in the group.
sugaDesiredCapacity :: Lens' ScheduledUpdateGroupAction (Maybe Int)
sugaDesiredCapacity =
    lens _sugaDesiredCapacity (\s a -> s { _sugaDesiredCapacity = a })

-- | The time that the action is scheduled to end. This value can be up to one
-- month in the future.
sugaEndTime :: Lens' ScheduledUpdateGroupAction (Maybe UTCTime)
sugaEndTime = lens _sugaEndTime (\s a -> s { _sugaEndTime = a }) . mapping _Time

-- | The maximum size of the group.
sugaMaxSize :: Lens' ScheduledUpdateGroupAction (Maybe Int)
sugaMaxSize = lens _sugaMaxSize (\s a -> s { _sugaMaxSize = a })

-- | The minimum size of the group.
sugaMinSize :: Lens' ScheduledUpdateGroupAction (Maybe Int)
sugaMinSize = lens _sugaMinSize (\s a -> s { _sugaMinSize = a })

-- | The regular schedule that an action occurs.
sugaRecurrence :: Lens' ScheduledUpdateGroupAction (Maybe Text)
sugaRecurrence = lens _sugaRecurrence (\s a -> s { _sugaRecurrence = a })

-- | The Amazon Resource Name (ARN) of the scheduled action.
sugaScheduledActionARN :: Lens' ScheduledUpdateGroupAction (Maybe Text)
sugaScheduledActionARN =
    lens _sugaScheduledActionARN (\s a -> s { _sugaScheduledActionARN = a })

-- | The name of the scheduled action.
sugaScheduledActionName :: Lens' ScheduledUpdateGroupAction (Maybe Text)
sugaScheduledActionName =
    lens _sugaScheduledActionName (\s a -> s { _sugaScheduledActionName = a })

-- | The time that the action is scheduled to begin. This value can be up to one
-- month in the future.
--
-- When 'StartTime' and 'EndTime' are specified with 'Recurrence', they form the
-- boundaries of when the recurring action will start and stop.
sugaStartTime :: Lens' ScheduledUpdateGroupAction (Maybe UTCTime)
sugaStartTime = lens _sugaStartTime (\s a -> s { _sugaStartTime = a }) . mapping _Time

-- | 'Time' is deprecated.
--
-- The time that the action is scheduled to begin. 'Time' is an alias for 'StartTime'.
sugaTime :: Lens' ScheduledUpdateGroupAction (Maybe UTCTime)
sugaTime = lens _sugaTime (\s a -> s { _sugaTime = a }) . mapping _Time

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

instance ToQuery ScheduledUpdateGroupAction where
    toQuery ScheduledUpdateGroupAction{..} = mconcat
        [ "AutoScalingGroupName" =? _sugaAutoScalingGroupName
        , "DesiredCapacity"      =? _sugaDesiredCapacity
        , "EndTime"              =? _sugaEndTime
        , "MaxSize"              =? _sugaMaxSize
        , "MinSize"              =? _sugaMinSize
        , "Recurrence"           =? _sugaRecurrence
        , "ScheduledActionARN"   =? _sugaScheduledActionARN
        , "ScheduledActionName"  =? _sugaScheduledActionName
        , "StartTime"            =? _sugaStartTime
        , "Time"                 =? _sugaTime
        ]

data ScalingProcessQuery = ScalingProcessQuery
    { _spqAutoScalingGroupName :: Text
    , _spqScalingProcesses     :: List "member" Text
    } deriving (Eq, Ord, Show)

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

-- | One or more of the following processes:
--
-- Launch Terminate HealthCheck ReplaceUnhealthy AZRebalance AlarmNotification
-- ScheduledActions AddToLoadBalancer
spqScalingProcesses :: Lens' ScalingProcessQuery [Text]
spqScalingProcesses =
    lens _spqScalingProcesses (\s a -> s { _spqScalingProcesses = a })
        . _List

instance FromXML ScalingProcessQuery where
    parseXML x = ScalingProcessQuery
        <$> x .@  "AutoScalingGroupName"
        <*> x .@? "ScalingProcesses" .!@ mempty

instance ToQuery ScalingProcessQuery where
    toQuery ScalingProcessQuery{..} = mconcat
        [ "AutoScalingGroupName" =? _spqAutoScalingGroupName
        , "ScalingProcesses"     =? _spqScalingProcesses
        ]

data Ebs = Ebs
    { _ebsDeleteOnTermination :: Maybe Bool
    , _ebsIops                :: Maybe Nat
    , _ebsSnapshotId          :: Maybe Text
    , _ebsVolumeSize          :: Maybe Nat
    , _ebsVolumeType          :: Maybe Text
    } deriving (Eq, Ord, Show)

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

-- | Indicates whether to delete the volume on instance termination.
--
-- Default: 'true'
ebsDeleteOnTermination :: Lens' Ebs (Maybe Bool)
ebsDeleteOnTermination =
    lens _ebsDeleteOnTermination (\s a -> s { _ebsDeleteOnTermination = a })

-- | For Provisioned IOPS (SSD) volumes only. The number of I/O operations per
-- second (IOPS) to provision for the volume.
--
-- Valid values: Range is 100 to 4000.
--
-- Default: None
ebsIops :: Lens' Ebs (Maybe Natural)
ebsIops = lens _ebsIops (\s a -> s { _ebsIops = a }) . mapping _Nat

-- | The ID of the snapshot.
ebsSnapshotId :: Lens' Ebs (Maybe Text)
ebsSnapshotId = lens _ebsSnapshotId (\s a -> s { _ebsSnapshotId = a })

-- | The volume size, in gigabytes.
--
-- Valid values: If the volume type is 'io1', the minimum size of the volume is
-- 10 GiB. If you specify 'SnapshotId' and 'VolumeSize', 'VolumeSize' must be equal to
-- or larger than the size of the snapshot.
--
-- Default: If you create a volume from a snapshot and you don't specify a
-- volume size, the default is the size of the snapshot.
--
-- Required: Required when the volume type is 'io1'.
ebsVolumeSize :: Lens' Ebs (Maybe Natural)
ebsVolumeSize = lens _ebsVolumeSize (\s a -> s { _ebsVolumeSize = a }) . mapping _Nat

-- | The volume type.
--
-- Valid values: 'standard | io1 | gp2'
--
-- Default: 'standard'
ebsVolumeType :: Lens' Ebs (Maybe Text)
ebsVolumeType = lens _ebsVolumeType (\s a -> s { _ebsVolumeType = a })

instance FromXML Ebs where
    parseXML x = Ebs
        <$> x .@? "DeleteOnTermination"
        <*> x .@? "Iops"
        <*> x .@? "SnapshotId"
        <*> x .@? "VolumeSize"
        <*> x .@? "VolumeType"

instance ToQuery Ebs where
    toQuery Ebs{..} = mconcat
        [ "DeleteOnTermination" =? _ebsDeleteOnTermination
        , "Iops"                =? _ebsIops
        , "SnapshotId"          =? _ebsSnapshotId
        , "VolumeSize"          =? _ebsVolumeSize
        , "VolumeType"          =? _ebsVolumeType
        ]

newtype AdjustmentType = AdjustmentType
    { _atAdjustmentType :: Maybe Text
    } deriving (Eq, Ord, Show, Monoid)

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

-- | The policy adjustment type. The valid values are 'ChangeInCapacity', 'ExactCapacity', and 'PercentChangeInCapacity'.
--
-- For more information, see <http://docs.aws.amazon.com/AutoScaling/latest/DeveloperGuide/as-scale-based-on-demand.html Dynamic Scaling> in the /Auto Scaling Developer Guide/
-- .
atAdjustmentType :: Lens' AdjustmentType (Maybe Text)
atAdjustmentType = lens _atAdjustmentType (\s a -> s { _atAdjustmentType = a })

instance FromXML AdjustmentType where
    parseXML x = AdjustmentType
        <$> x .@? "AdjustmentType"

instance ToQuery AdjustmentType where
    toQuery AdjustmentType{..} = mconcat
        [ "AdjustmentType" =? _atAdjustmentType
        ]

newtype MetricCollectionType = MetricCollectionType
    { _mctMetric :: Maybe Text
    } deriving (Eq, Ord, Show, Monoid)

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

-- | The metric.
mctMetric :: Lens' MetricCollectionType (Maybe Text)
mctMetric = lens _mctMetric (\s a -> s { _mctMetric = a })

instance FromXML MetricCollectionType where
    parseXML x = MetricCollectionType
        <$> x .@? "Metric"

instance ToQuery MetricCollectionType where
    toQuery MetricCollectionType{..} = mconcat
        [ "Metric" =? _mctMetric
        ]

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
    } deriving (Eq, Ord, Show)

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

-- | The name of the Auto Scaling group for the lifecycle hook.
lhAutoScalingGroupName :: Lens' LifecycleHook (Maybe Text)
lhAutoScalingGroupName =
    lens _lhAutoScalingGroupName (\s a -> s { _lhAutoScalingGroupName = a })

-- | Defines the action the Auto Scaling group should take when the lifecycle hook
-- timeout elapses or if an unexpected failure occurs. The valid values are 'CONTINUE' and 'ABANDON'. The default value is 'CONTINUE'.
lhDefaultResult :: Lens' LifecycleHook (Maybe Text)
lhDefaultResult = lens _lhDefaultResult (\s a -> s { _lhDefaultResult = a })

-- | The maximum length of time an instance can remain in a 'Pending:Wait' or 'Terminating:Wait' state. Currently, this value is set at 48 hours.
lhGlobalTimeout :: Lens' LifecycleHook (Maybe Int)
lhGlobalTimeout = lens _lhGlobalTimeout (\s a -> s { _lhGlobalTimeout = a })

-- | The amount of time that can elapse before the lifecycle hook times out. When
-- the lifecycle hook times out, Auto Scaling performs the action defined in the 'DefaultResult' parameter. You can prevent the lifecycle hook from timing out
-- by calling 'RecordLifecycleActionHeartbeat'.
lhHeartbeatTimeout :: Lens' LifecycleHook (Maybe Int)
lhHeartbeatTimeout =
    lens _lhHeartbeatTimeout (\s a -> s { _lhHeartbeatTimeout = a })

-- | The name of the lifecycle hook.
lhLifecycleHookName :: Lens' LifecycleHook (Maybe Text)
lhLifecycleHookName =
    lens _lhLifecycleHookName (\s a -> s { _lhLifecycleHookName = a })

-- | The state of the EC2 instance to which you want to attach the lifecycle hook.
-- For a list of lifecycle hook types, see 'DescribeLifecycleHooks'.
lhLifecycleTransition :: Lens' LifecycleHook (Maybe Text)
lhLifecycleTransition =
    lens _lhLifecycleTransition (\s a -> s { _lhLifecycleTransition = a })

-- | Additional information that you want to include any time Auto Scaling sends a
-- message to the notification target.
lhNotificationMetadata :: Lens' LifecycleHook (Maybe Text)
lhNotificationMetadata =
    lens _lhNotificationMetadata (\s a -> s { _lhNotificationMetadata = a })

-- | The ARN of the notification target that Auto Scaling uses to notify you when
-- an instance is in the transition state for the lifecycle hook. This ARN
-- target can be either an SQS queue or an SNS topic. The notification message
-- sent to the target includes the following:
--
-- Lifecycle action token User account ID Name of the Auto Scaling group Lifecycle hook name
-- EC2 instance ID Lifecycle transition Notification metadata
lhNotificationTargetARN :: Lens' LifecycleHook (Maybe Text)
lhNotificationTargetARN =
    lens _lhNotificationTargetARN (\s a -> s { _lhNotificationTargetARN = a })

-- | The ARN of the IAM role that allows the Auto Scaling group to publish to the
-- specified notification target.
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

instance ToQuery LifecycleHook where
    toQuery LifecycleHook{..} = mconcat
        [ "AutoScalingGroupName"  =? _lhAutoScalingGroupName
        , "DefaultResult"         =? _lhDefaultResult
        , "GlobalTimeout"         =? _lhGlobalTimeout
        , "HeartbeatTimeout"      =? _lhHeartbeatTimeout
        , "LifecycleHookName"     =? _lhLifecycleHookName
        , "LifecycleTransition"   =? _lhLifecycleTransition
        , "NotificationMetadata"  =? _lhNotificationMetadata
        , "NotificationTargetARN" =? _lhNotificationTargetARN
        , "RoleARN"               =? _lhRoleARN
        ]

data Activity = Activity
    { _aActivityId           :: Text
    , _aAutoScalingGroupName :: Text
    , _aCause                :: Text
    , _aDescription          :: Maybe Text
    , _aDetails              :: Maybe Text
    , _aEndTime              :: Maybe ISO8601
    , _aProgress             :: Maybe Int
    , _aStartTime            :: ISO8601
    , _aStatusCode           :: ScalingActivityStatusCode
    , _aStatusMessage        :: Maybe Text
    } deriving (Eq, Show)

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
-- * 'aStatusCode' @::@ 'ScalingActivityStatusCode'
--
-- * 'aStatusMessage' @::@ 'Maybe' 'Text'
--
activity :: Text -- ^ 'aActivityId'
         -> Text -- ^ 'aAutoScalingGroupName'
         -> Text -- ^ 'aCause'
         -> UTCTime -- ^ 'aStartTime'
         -> ScalingActivityStatusCode -- ^ 'aStatusCode'
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

-- | The ID of the activity.
aActivityId :: Lens' Activity Text
aActivityId = lens _aActivityId (\s a -> s { _aActivityId = a })

-- | The name of the Auto Scaling group.
aAutoScalingGroupName :: Lens' Activity Text
aAutoScalingGroupName =
    lens _aAutoScalingGroupName (\s a -> s { _aAutoScalingGroupName = a })

-- | The reason the activity was begun.
aCause :: Lens' Activity Text
aCause = lens _aCause (\s a -> s { _aCause = a })

-- | A friendly, more verbose description of the scaling activity.
aDescription :: Lens' Activity (Maybe Text)
aDescription = lens _aDescription (\s a -> s { _aDescription = a })

-- | The details about the scaling activity.
aDetails :: Lens' Activity (Maybe Text)
aDetails = lens _aDetails (\s a -> s { _aDetails = a })

-- | The end time of this activity.
aEndTime :: Lens' Activity (Maybe UTCTime)
aEndTime = lens _aEndTime (\s a -> s { _aEndTime = a }) . mapping _Time

-- | A value between 0 and 100 that indicates the progress of the activity.
aProgress :: Lens' Activity (Maybe Int)
aProgress = lens _aProgress (\s a -> s { _aProgress = a })

-- | The start time of this activity.
aStartTime :: Lens' Activity UTCTime
aStartTime = lens _aStartTime (\s a -> s { _aStartTime = a }) . _Time

-- | The current status of the activity.
aStatusCode :: Lens' Activity ScalingActivityStatusCode
aStatusCode = lens _aStatusCode (\s a -> s { _aStatusCode = a })

-- | A friendly, more verbose description of the activity status.
aStatusMessage :: Lens' Activity (Maybe Text)
aStatusMessage = lens _aStatusMessage (\s a -> s { _aStatusMessage = a })

instance FromXML Activity where
    parseXML x = Activity
        <$> x .@  "ActivityId"
        <*> x .@  "AutoScalingGroupName"
        <*> x .@  "Cause"
        <*> x .@? "Description"
        <*> x .@? "Details"
        <*> x .@? "EndTime"
        <*> x .@? "Progress"
        <*> x .@  "StartTime"
        <*> x .@  "StatusCode"
        <*> x .@? "StatusMessage"

instance ToQuery Activity where
    toQuery Activity{..} = mconcat
        [ "ActivityId"           =? _aActivityId
        , "AutoScalingGroupName" =? _aAutoScalingGroupName
        , "Cause"                =? _aCause
        , "Description"          =? _aDescription
        , "Details"              =? _aDetails
        , "EndTime"              =? _aEndTime
        , "Progress"             =? _aProgress
        , "StartTime"            =? _aStartTime
        , "StatusCode"           =? _aStatusCode
        , "StatusMessage"        =? _aStatusMessage
        ]

data SuspendedProcess = SuspendedProcess
    { _spProcessName      :: Maybe Text
    , _spSuspensionReason :: Maybe Text
    } deriving (Eq, Ord, Show)

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

instance ToQuery SuspendedProcess where
    toQuery SuspendedProcess{..} = mconcat
        [ "ProcessName"      =? _spProcessName
        , "SuspensionReason" =? _spSuspensionReason
        ]

newtype MetricGranularityType = MetricGranularityType
    { _mgtGranularity :: Maybe Text
    } deriving (Eq, Ord, Show, Monoid)

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

-- | The granularity.
mgtGranularity :: Lens' MetricGranularityType (Maybe Text)
mgtGranularity = lens _mgtGranularity (\s a -> s { _mgtGranularity = a })

instance FromXML MetricGranularityType where
    parseXML x = MetricGranularityType
        <$> x .@? "Granularity"

instance ToQuery MetricGranularityType where
    toQuery MetricGranularityType{..} = mconcat
        [ "Granularity" =? _mgtGranularity
        ]

data Filter = Filter
    { _fName   :: Text
    , _fValues :: List "member" Text
    } deriving (Eq, Ord, Show)

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

-- | The name of the filter. The valid values are: '"auto-scaling-group"', '"key"', '"value"', and '"propagate-at-launch"'.
fName :: Lens' Filter Text
fName = lens _fName (\s a -> s { _fName = a })

-- | The value of the filter.
fValues :: Lens' Filter [Text]
fValues = lens _fValues (\s a -> s { _fValues = a }) . _List

instance FromXML Filter where
    parseXML x = Filter
        <$> x .@  "Name"
        <*> x .@? "Values" .!@ mempty

instance ToQuery Filter where
    toQuery Filter{..} = mconcat
        [ "Name"   =? _fName
        , "Values" =? _fValues
        ]

newtype ProcessType = ProcessType
    { _ptProcessName :: Text
    } deriving (Eq, Ord, Show, Monoid, IsString)

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

-- | The name of the process.
ptProcessName :: Lens' ProcessType Text
ptProcessName = lens _ptProcessName (\s a -> s { _ptProcessName = a })

instance FromXML ProcessType where
    parseXML x = ProcessType
        <$> x .@  "ProcessName"

instance ToQuery ProcessType where
    toQuery ProcessType{..} = mconcat
        [ "ProcessName" =? _ptProcessName
        ]

data Alarm = Alarm
    { _aAlarmARN  :: Maybe Text
    , _aAlarmName :: Maybe Text
    } deriving (Eq, Ord, Show)

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

instance ToQuery Alarm where
    toQuery Alarm{..} = mconcat
        [ "AlarmARN"  =? _aAlarmARN
        , "AlarmName" =? _aAlarmName
        ]

data EnabledMetric = EnabledMetric
    { _emGranularity :: Maybe Text
    , _emMetric      :: Maybe Text
    } deriving (Eq, Ord, Show)

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

-- | The granularity of the metric.
emGranularity :: Lens' EnabledMetric (Maybe Text)
emGranularity = lens _emGranularity (\s a -> s { _emGranularity = a })

-- | The name of the metric.
emMetric :: Lens' EnabledMetric (Maybe Text)
emMetric = lens _emMetric (\s a -> s { _emMetric = a })

instance FromXML EnabledMetric where
    parseXML x = EnabledMetric
        <$> x .@? "Granularity"
        <*> x .@? "Metric"

instance ToQuery EnabledMetric where
    toQuery EnabledMetric{..} = mconcat
        [ "Granularity" =? _emGranularity
        , "Metric"      =? _emMetric
        ]

data Instance = Instance
    { _iAvailabilityZone        :: Text
    , _iHealthStatus            :: Text
    , _iInstanceId              :: Text
    , _iLaunchConfigurationName :: Text
    , _iLifecycleState          :: LifecycleState
    } deriving (Eq, Show)

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
-- * 'iLifecycleState' @::@ 'LifecycleState'
--
instance' :: Text -- ^ 'iInstanceId'
          -> Text -- ^ 'iAvailabilityZone'
          -> LifecycleState -- ^ 'iLifecycleState'
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

-- | The Availability Zone associated with this instance.
iAvailabilityZone :: Lens' Instance Text
iAvailabilityZone =
    lens _iAvailabilityZone (\s a -> s { _iAvailabilityZone = a })

-- | The health status of the instance.
iHealthStatus :: Lens' Instance Text
iHealthStatus = lens _iHealthStatus (\s a -> s { _iHealthStatus = a })

-- | The ID of the instance.
iInstanceId :: Lens' Instance Text
iInstanceId = lens _iInstanceId (\s a -> s { _iInstanceId = a })

-- | The launch configuration associated with the instance.
iLaunchConfigurationName :: Lens' Instance Text
iLaunchConfigurationName =
    lens _iLaunchConfigurationName
        (\s a -> s { _iLaunchConfigurationName = a })

-- | A description of the current lifecycle state.
--
-- The 'Quarantined' lifecycle state is not used.
--
--
iLifecycleState :: Lens' Instance LifecycleState
iLifecycleState = lens _iLifecycleState (\s a -> s { _iLifecycleState = a })

instance FromXML Instance where
    parseXML x = Instance
        <$> x .@  "AvailabilityZone"
        <*> x .@  "HealthStatus"
        <*> x .@  "InstanceId"
        <*> x .@  "LaunchConfigurationName"
        <*> x .@  "LifecycleState"

instance ToQuery Instance where
    toQuery Instance{..} = mconcat
        [ "AvailabilityZone"        =? _iAvailabilityZone
        , "HealthStatus"            =? _iHealthStatus
        , "InstanceId"              =? _iInstanceId
        , "LaunchConfigurationName" =? _iLaunchConfigurationName
        , "LifecycleState"          =? _iLifecycleState
        ]

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
    parser = takeText >>= \case
        "Detached"            -> pure Detached
        "Detaching"           -> pure Detaching
        "EnteringStandby"     -> pure EnteringStandby
        "InService"           -> pure InService
        "Pending"             -> pure Pending
        "Pending:Proceed"     -> pure PendingProceed
        "Pending:Wait"        -> pure PendingWait
        "Quarantined"         -> pure Quarantined
        "Standby"             -> pure Standby
        "Terminated"          -> pure Terminated
        "Terminating"         -> pure Terminating
        "Terminating:Proceed" -> pure TerminatingProceed
        "Terminating:Wait"    -> pure TerminatingWait
        e                     -> fail $
            "Failure parsing LifecycleState from " ++ show e

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

instance ToByteString LifecycleState
instance ToHeader     LifecycleState
instance ToQuery      LifecycleState

instance FromXML LifecycleState where
    parseXML = parseXMLText "LifecycleState"

data AutoScalingInstanceDetails = AutoScalingInstanceDetails
    { _asidAutoScalingGroupName    :: Text
    , _asidAvailabilityZone        :: Text
    , _asidHealthStatus            :: Text
    , _asidInstanceId              :: Text
    , _asidLaunchConfigurationName :: Text
    , _asidLifecycleState          :: Text
    } deriving (Eq, Ord, Show)

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

-- | The name of the Auto Scaling group associated with the instance.
asidAutoScalingGroupName :: Lens' AutoScalingInstanceDetails Text
asidAutoScalingGroupName =
    lens _asidAutoScalingGroupName
        (\s a -> s { _asidAutoScalingGroupName = a })

-- | The Availability Zone for the instance.
asidAvailabilityZone :: Lens' AutoScalingInstanceDetails Text
asidAvailabilityZone =
    lens _asidAvailabilityZone (\s a -> s { _asidAvailabilityZone = a })

-- | The health status of this instance. "Healthy" means that the instance is
-- healthy and should remain in service. "Unhealthy" means that the instance is
-- unhealthy and Auto Scaling should terminate and replace it.
asidHealthStatus :: Lens' AutoScalingInstanceDetails Text
asidHealthStatus = lens _asidHealthStatus (\s a -> s { _asidHealthStatus = a })

-- | The ID of the instance.
asidInstanceId :: Lens' AutoScalingInstanceDetails Text
asidInstanceId = lens _asidInstanceId (\s a -> s { _asidInstanceId = a })

-- | The launch configuration associated with the instance.
asidLaunchConfigurationName :: Lens' AutoScalingInstanceDetails Text
asidLaunchConfigurationName =
    lens _asidLaunchConfigurationName
        (\s a -> s { _asidLaunchConfigurationName = a })

-- | The lifecycle state for the instance. For more information, see <http://docs.aws.amazon.com/AutoScaling/latest/DeveloperGuide/AutoScalingGroupLifecycle.html#AutoScalingStates Auto ScalingInstance States> in the /Auto Scaling Developer Guide/.
asidLifecycleState :: Lens' AutoScalingInstanceDetails Text
asidLifecycleState =
    lens _asidLifecycleState (\s a -> s { _asidLifecycleState = a })

instance FromXML AutoScalingInstanceDetails where
    parseXML x = AutoScalingInstanceDetails
        <$> x .@  "AutoScalingGroupName"
        <*> x .@  "AvailabilityZone"
        <*> x .@  "HealthStatus"
        <*> x .@  "InstanceId"
        <*> x .@  "LaunchConfigurationName"
        <*> x .@  "LifecycleState"

instance ToQuery AutoScalingInstanceDetails where
    toQuery AutoScalingInstanceDetails{..} = mconcat
        [ "AutoScalingGroupName"    =? _asidAutoScalingGroupName
        , "AvailabilityZone"        =? _asidAvailabilityZone
        , "HealthStatus"            =? _asidHealthStatus
        , "InstanceId"              =? _asidInstanceId
        , "LaunchConfigurationName" =? _asidLaunchConfigurationName
        , "LifecycleState"          =? _asidLifecycleState
        ]

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
    parser = takeText >>= \case
        "Cancelled"                       -> pure Cancelled
        "Failed"                          -> pure Failed
        "InProgress"                      -> pure InProgress
        "MidLifecycleAction"              -> pure MidLifecycleAction
        "PreInService"                    -> pure PreInService
        "Successful"                      -> pure Successful
        "WaitingForELBConnectionDraining" -> pure WaitingForELBConnectionDraining
        "WaitingForInstanceId"            -> pure WaitingForInstanceId
        "WaitingForSpotInstanceId"        -> pure WaitingForSpotInstanceId
        "WaitingForSpotInstanceRequestId" -> pure WaitingForSpotInstanceRequestId
        e                                 -> fail $
            "Failure parsing ScalingActivityStatusCode from " ++ show e

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

instance ToByteString ScalingActivityStatusCode
instance ToHeader     ScalingActivityStatusCode
instance ToQuery      ScalingActivityStatusCode

instance FromXML ScalingActivityStatusCode where
    parseXML = parseXMLText "ScalingActivityStatusCode"
