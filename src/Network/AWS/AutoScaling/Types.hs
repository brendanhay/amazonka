{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}

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
import Network.AWS.Internal

autoScalingVersion :: ByteString
autoScalingVersion = "2011-01-01"

autoScalingNS :: ByteString
autoScalingNS = "http://autoscaling.amazonaws.com/doc/" <> autoScalingVersion <> "/"

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

-- --
-- -- Types
-- --

-- -- Shouldnt need a FromXML since i've just aliased the MetadataResponse?
-- -- get as object then lookup ResponseMetadata as a key

-- data MetricGranularityType = MetricGranularityType
--     { mgtGranularity :: Maybe ByteString
--     } deriving (Eq, Show, Generic)

-- instance IsXML MetricGranularityType

-- newtype MetricGranularityTypes = MetricGranularityTypes [MetricGranularityType]
--    deriving (Eq, Show, Generic)

-- instance IsXML MetricGranularityTypes

-- newtype LoadBalancerNames = LoadBalancerNames [ByteString]
--     deriving (Eq, Show, Generic)

-- -- -- $(deriveQS' (++ ".member") ''LoadBalancerNames)
-- instance IsXML LoadBalancerNames

-- data SuspendedProcess = SuspendedProcess
--     { spProcessName :: Maybe ByteString
--     , spSuspensionReason :: Maybe ByteString
--     } deriving (Eq, Show, Generic)

-- instance IsQuery SuspendedProcess
-- instance IsXML SuspendedProcess

-- newtype SuspendedProcesses = SuspendedProcesses [SuspendedProcess]
--     deriving (Eq, Show, Generic)

-- instance IsXML SuspendedProcesses

data Tag = Tag
    { tResourceId        :: Maybe ByteString
    , tResourceType      :: Maybe ByteString
    , tKey               :: !ByteString
    , tValue             :: Maybe ByteString
    , tPropagateAtLaunch :: Maybe Bool
    } deriving (Eq, Show, Generic)

instance IsQuery Tag

-- instance IsQuery [Tag] where

-- newtype Tags = Tags [Tag]
--     deriving (Eq, Show, Generic)

-- -- $(deriveQS' (++ ".member") ''Tags)

-- -- data Tags = Tags
-- --     { tMember :: [Tag]
-- --     } deriving (Eq, Show, Generic)

-- newtype PolicyNames = PolicyNames [ResourceName]
--    deriving (Eq, Show, Generic)

-- -- $(deriveQS' (++ ".member") ''PolicyNames)

-- data NotificationConfiguration = NotificationConfiguration
--     { ncAutoScalingGroupName :: Maybe ResourceName
--     , ncTopicARN :: Maybe ResourceName
--     , ncNotificationType :: Maybe ByteString
--     } deriving (Eq, Show, Generic)

-- instance IsXML NotificationConfiguration

-- newtype NotificationConfigurations = NotificationConfigurations [NotificationConfiguration]
--    deriving (Eq, Show, Generic)

-- instance IsXML NotificationConfigurations

-- newtype ScheduledActionNames = ScheduledActionNames [ResourceName]
--    deriving (Eq, Show, Generic)

-- -- $(deriveQS' (++ ".member") ''ScheduledActionNames)

-- data ScheduledUpdateGroupAction = ScheduledUpdateGroupAction
--     { sugaAutoScalingGroupName :: Maybe ByteString
--     , sugaScheduledActionName :: Maybe ByteString
--     , sugaScheduledActionARN :: Maybe ResourceName
--     , sugaTime :: Maybe UTCTime
--     , sugaStartTime :: Maybe UTCTime
--     , sugaEndTime :: Maybe UTCTime
--     , sugaRecurrence :: Maybe ByteString
--     , sugaMinSize :: Maybe Integer
--     , sugaMaxSize :: Maybe Integer
--     , sugaDesiredCapacity :: Maybe Integer
--     } deriving (Eq, Show, Generic)

-- instance IsXML ScheduledUpdateGroupAction

-- newtype ScheduledUpdateGroupActions = ScheduledUpdateGroupActions [ScheduledUpdateGroupAction]
--    deriving (Eq, Show, Generic)

-- instance IsXML ScheduledUpdateGroupActions

-- newtype SecurityGroups = SecurityGroups [ByteString]
--    deriving (Eq, Show, Generic)

-- newtype SecurityGroup = SecurityGroup ByteString
--     deriving (Eq, Show, Generic)

-- -- $(deriveQS' (++ ".member") ''SecurityGroups)
-- instance IsXML SecurityGroups

data Ebs = Ebs
    { eSnapshotId :: Maybe ByteString
    , eVolumeSize :: Maybe Integer
    } deriving (Eq, Show, Generic)

instance IsQuery Ebs
-- instance IsXML Ebs

data BlockDeviceMapping = BlockDeviceMapping
    { bdmVirtualName :: Maybe ByteString
    , bdmDeviceName  :: !ByteString
    , bdmEbs         :: Maybe Ebs
    } deriving (Eq, Show, Generic)

instance IsQuery BlockDeviceMapping
-- instance IsXML BlockDeviceMapping

-- newtype BlockDeviceMappings = BlockDeviceMappings [BlockDeviceMapping]
--    deriving (Eq, Show, Generic)

-- -- $(deriveQS' (++ ".member") ''BlockDeviceMappings)
-- instance IsXML BlockDeviceMappings

data InstanceMonitoring = InstanceMonitoring
    { imEnabled :: Bool
    } deriving (Eq, Show, Generic)

instance IsQuery InstanceMonitoring



-- newtype LaunchConfigurationNames = LaunchConfigurationNames [ResourceName]
--    deriving (Eq, Show, Generic)

-- -- $(deriveQS' (++ ".member") ''LaunchConfigurationNames)
-- instance IsXML LaunchConfigurationNames

-- data LaunchConfiguration = LaunchConfiguration
--     { lcLaunchConfigurationName :: !ByteString
--     , lcLaunchConfigurationARN :: Maybe ResourceName
--     , lcImageId :: !ByteString
--     , lcKeyName :: Maybe ByteString
--     , lcSecurityGroups :: !(Params Member SecurityGroup)
--     , lcUserData :: Maybe ByteString
--     , lcInstanceType :: !ByteString
--     , lcKernelId :: Maybe ByteString
--     , lcRamdiskId :: Maybe ByteString
--     , lcBlockDeviceMappings :: Maybe BlockDeviceMappings
--     , lcInstanceMonitoring :: Maybe InstanceMonitoring
--     , lcSpotPrice :: Maybe ByteString
--     , lcIamInstanceProfile :: Maybe ByteString
--     , lcCreatedTime :: !UTCTime
--     , lcEbsOptimized :: Maybe Bool
--     } deriving (Eq, Show, Generic)

-- instance IsQuery LaunchConfiguration
-- instance IsXML LaunchConfiguration

-- newtype LaunchConfigurations = LaunchConfigurations [LaunchConfiguration]
--    deriving (Eq, Show, Generic)

-- -- $(deriveQS' (++ ".member") ''LaunchConfigurations)
-- instance IsXML LaunchConfigurations

-- newtype AvailabilityZones = AvailabilityZones [ByteString]
--    deriving (Eq, Show, Generic)

-- -- $(deriveQS' (++ ".member") ''AvailabilityZones)
-- instance IsXML AvailabilityZones

-- data EnabledMetric = EnabledMetric
--     { emMetric :: Maybe ByteString
--     , emGranularity :: Maybe ByteString
--     } deriving (Eq, Show, Generic)

-- instance IsXML EnabledMetric

-- newtype EnabledMetrics = EnabledMetrics [EnabledMetric]
--    deriving (Eq, Show, Generic)

-- instance IsXML EnabledMetrics

-- data TagDescription = TagDescription
--     { tdResourceId :: Maybe ByteString
--     , tdResourceType :: Maybe ByteString
--     , tdKey :: Maybe ByteString
--     , tdValue :: Maybe ByteString
--     , tdPropagateAtLaunch :: Maybe Bool
--     } deriving (Eq, Show, Generic)

-- instance IsQuery TagDescription
-- instance IsXML TagDescription

-- newtype TagDescriptionList = TagDescriptionList [TagDescription]
--     deriving (Eq, Show, Generic)

-- -- $(deriveQS' (++ ".member") ''TagDescriptionList)
-- instance IsXML TagDescriptionList

-- newtype TerminationPolicies = TerminationPolicies [ByteString]
--     deriving (Eq, Show, Generic)

-- -- $(deriveQS' (++ ".member") ''TerminationPolicies)
-- instance IsXML TerminationPolicies

-- data LifecycleState
--     = Pending
--     | Quarantined
--     | InService
--     | Terminating
--     | Terminated
--       deriving (Eq, Show, Generic)

-- instance IsXML LifecycleState

-- data Instance = Instance
--     { iInstanceId :: !ByteString
--     , iAvailabilityZone :: !ByteString
--     , iLifecycleState :: !LifecycleState
--     , iHealthStatus :: !ByteString
--     , iLaunchConfigurationName :: !ByteString
--     } deriving (Eq, Show, Generic)

-- instance IsXML Instance

-- newtype Instances = Instances [Instance]
--    deriving (Eq, Show, Generic)

-- instance IsXML Instances

-- data AutoScalingGroup = AutoScalingGroup
--     { asgAutoScalingGroupName :: !ByteString
--     , asgAutoScalingGroupARN :: Maybe ResourceName
--     , asgLaunchConfigurationName :: !ByteString
--     , asgMinSize :: !Integer
--     , asgMaxSize :: !Integer
--     , asgDesiredCapacity :: !Integer
--     , asgDefaultCooldown :: !Integer
--     , asgAvailabilityZones :: !AvailabilityZones
--     , asgLoadBalancerNames :: Maybe LoadBalancerNames
--     , asgHealthCheckType :: !ByteString
--     , asgHealthCheckGracePeriod :: Maybe Integer
--     , asgInstances :: Maybe Instances
--     , asgCreatedTime :: !UTCTime
--     , asgSuspendedProcesses :: !SuspendedProcesses
--     , asgPlacementGroup :: Maybe ByteString
--     , asgVPCZoneIdentifier :: Maybe ByteString
--     , asgEnabledMetrics :: Maybe EnabledMetrics
--     , asgStatus :: Maybe ByteString
--     , asgTags :: Maybe TagDescriptionList
--     , asgTerminationPolicies :: !TerminationPolicies
--     } deriving (Eq, Show, Generic)

-- instance IsXML AutoScalingGroup

-- data MetricCollectionType = MetricCollectionType
--     { mctMetric :: Maybe ByteString
--     } deriving (Eq, Show, Generic)

-- instance IsXML MetricCollectionType

-- newtype MetricCollectionTypes = MetricCollectionTypes [MetricCollectionType]
--    deriving (Eq, Show, Generic)

-- instance IsXML MetricCollectionTypes

-- newtype AutoScalingNotificationTypes = AutoScalingNotificationTypes [ByteString]
--     deriving (Eq, Show, Generic)

-- -- $(deriveQS' (++ ".member") ''AutoScalingNotificationTypes)
-- instance IsXML AutoScalingNotificationTypes

-- data AutoScalingInstanceDetails = AutoScalingInstanceDetails
--     { asidInstanceId :: !ByteString
--     , asidAutoScalingGroupName :: !ByteString
--     , asidAvailabilityZone :: !ByteString
--     , asidLifecycleState :: !ByteString
--     , asidHealthStatus :: !ByteString
--     , asidLaunchConfigurationName :: !ByteString
--     } deriving (Eq, Show, Generic)

-- instance IsXML AutoScalingInstanceDetails

-- newtype AutoScalingInstances = AutoScalingInstances [AutoScalingInstanceDetails]
--    deriving (Eq, Show, Generic)

-- instance IsXML AutoScalingInstances

-- newtype Metrics = Metrics [ByteString]
--    deriving (Eq, Show, Generic)

-- -- $(deriveQS' (++ ".member") ''Metrics)

-- data ScalingActivityStatusCode
--     = WaitingForSpotInstanceRequestId
--     | WaitingForSpotInstanceId
--     | WaitingForInstanceId
--     | PreInService
--     | InProgress
--     | Successful
--     | Failed
--     | Cancelled
--       deriving (Eq, Show, Generic)

-- instance IsXML ScalingActivityStatusCode

-- data Activity = Activity
--     { aActivityId :: !ByteString
--     , aAutoScalingGroupName :: !ByteString
--     , aDescription :: Maybe ByteString
--     , aCause :: !ByteString
--     , aStartTime :: !UTCTime
--     , aEndTime :: Maybe UTCTime
--     , aStatusCode :: !ScalingActivityStatusCode
--     , aStatusMessage :: Maybe ByteString
--     , aProgress :: Maybe Integer
--     , aDetails :: Maybe ByteString
--     } deriving (Eq, Show, Generic)

-- instance IsXML Activity

-- newtype Activities = Activities [Activity]
--    deriving (Eq, Show, Generic)

-- instance IsXML Activities

-- newtype ActivityIds = ActivityIds [ByteString]
--    deriving (Eq, Show, Generic)

-- -- $(deriveQS' (++ ".member") ''ActivityIds)

-- data Alarm = Alarm
--     { aAlarmName :: Maybe ByteString
--     , aAlarmARN :: Maybe ResourceName
--     } deriving (Eq, Show, Generic)

-- instance IsXML Alarm

-- newtype Alarms = Alarms [Alarm]
--    deriving (Eq, Show, Generic)

-- instance IsXML Alarms

-- data ScalingPolicy = ScalingPolicy
--     { spAutoScalingGroupName :: Maybe ByteString
--     , spPolicyName :: Maybe ByteString
--     , spScalingAdjustment :: Maybe Integer
--     , spAdjustmentType :: Maybe ByteString
--     , spCooldown :: Maybe Integer
--     , spPolicyARN :: Maybe ResourceName
--     , spAlarms :: [Alarms]
--     , spMinAdjustmentStep :: Maybe Integer
--     } deriving (Eq, Show, Generic)

-- instance IsXML ScalingPolicy

-- newtype ScalingPolicies = ScalingPolicies [ScalingPolicy]
--    deriving (Eq, Show, Generic)

-- instance IsXML ScalingPolicies

-- newtype Values = Values [ByteString]
--    deriving (Eq, Show, Generic)

-- -- $(deriveQS' (++ ".member") ''Values)

-- data Filter = Filter
--     { fName :: Maybe ByteString
--     , fValues :: Maybe Values
--     } deriving (Eq, Show, Generic)

-- instance IsQuery Filter

-- newtype Filters = Filters [Filter]
--    deriving (Eq, Show, Generic)

-- -- $(deriveQS' (++ ".member") ''Filters)

-- data ProcessType = ProcessType
--     { ptProcessName :: !ByteString
--     } deriving (Eq, Show, Generic)

-- instance IsXML ProcessType

-- newtype Processes = Processes [ProcessType]
--    deriving (Eq, Show, Generic)

-- instance IsXML Processes

-- newtype InstanceIds = InstanceIds [ByteString]
--    deriving (Eq, Show, Generic)

-- -- $(deriveQS' (++ ".member") ''InstanceIds)

-- newtype AutoScalingGroupNames = AutoScalingGroupNames [ResourceName]
--    deriving (Eq, Show, Generic)

-- -- $(deriveQS' (++ ".member") ''AutoScalingGroupNames)

-- newtype AutoScalingGroups = AutoScalingGroups [AutoScalingGroup]
--    deriving (Eq, Show, Generic)

-- instance IsXML AutoScalingGroups

-- newtype ProcessNames = ProcessNames [ByteString]
--    deriving (Eq, Show, Generic)

-- -- $(deriveQS' (++ ".member") ''ProcessNames)

-- newtype AdjustmentTypes = AdjustmentTypes [ByteString]
--    deriving (Eq, Show, Generic)

-- -- $(deriveQS' (++ ".member") ''AdjustmentTypes)
-- instance IsXML AdjustmentTypes
