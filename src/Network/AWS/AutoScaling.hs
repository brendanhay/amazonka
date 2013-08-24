{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}

-- Module      : Network.AWS.AutoScaling
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Auto Scaling is a web service designed to automatically launch or terminate
-- Amazon Elastic Compute Cloud (Amazon EC2) instances based on user-defined
-- policies, schedules, and health checks.
--
-- This service is used in conjunction with Amazon CloudWatch and
-- Elastic Load Balancing services.
module Network.AWS.AutoScaling where

import Data.ByteString                   (ByteString, empty)
import Data.Time
import Network.AWS.AutoScaling.Responses
import Network.AWS.AutoScaling.Types
import Network.AWS.Internal
import Network.Http.Client

data AutoScaling

instance AWSService AutoScaling where
    service _ = awsService "autoscaling" (toBS autoScalingVersion) SigningVersion4

autoScalingVersion :: ByteString
autoScalingVersion = "2011-01-01"

req :: (IsQuery a, IsXML b) => Method -> ByteString -> a -> AWS (RawRequest AutoScaling b)
req meth action qry = return $ (emptyRequest meth FormEncoded empty Nothing)
    { rqAction = Just action
    , rqQuery  = toQuery qry
    }

--
-- Actions
--

-- |
--
-- <http://docs.aws.amazon.com/AutoScaling/latest/APIReference/API_CreateAutoScalingGroup.html>
data CreateAutoScalingGroup = CreateAutoScalingGroup
    { casgAutoScalingGroupName :: !ByteString
    , casgLaunchConfigurationName :: !ResourceName
    , casgMinSize :: !Integer
    , casgMaxSize :: !Integer
    , casgDesiredCapacity :: !(Maybe Integer)
    , casgDefaultCooldown :: !(Maybe Integer)
    , casgAvailabilityZones :: !(Member ByteString)
    , casgLoadBalancerNames :: !(Member ByteString)
    , casgHealthCheckType :: !(Maybe ByteString)
    , casgHealthCheckGracePeriod :: !(Maybe Integer)
    , casgPlacementGroup :: !(Maybe ByteString)
    , casgVPCZoneIdentifier :: !(Maybe ByteString)
    , casgTerminationPolicies :: !(Member ByteString)
    , casgTags :: !(Member Tag)
    } deriving (Eq, Show, Generic)

instance IsQuery CreateAutoScalingGroup

instance AWSRequest AutoScaling CreateAutoScalingGroup MetadataResponse where
    request = req GET "CreateAutoScalingGroup"

-- -- |
-- --
-- -- <http://docs.aws.amazon.com/AutoScaling/latest/APIReference/API_CreateLaunchConfiguration.html>
-- data CreateLaunchConfiguration = CreateLaunchConfiguration
--     { clcLaunchConfigurationName :: !ByteString
--     , clcImageId :: !ByteString
--     , clcKeyName :: !(Maybe ByteString)
--     , clcSecurityGroups :: !(Maybe SecurityGroups)
--     , clcUserData :: !(Maybe ByteString)
--     , clcInstanceType :: !ByteString
--     , clcKernelId :: !(Maybe ByteString)
--     , clcRamdiskId :: !(Maybe ByteString)
--     , clcBlockDeviceMappings :: !(Maybe BlockDeviceMappings)
--     , clcInstanceMonitoring :: !(Maybe InstanceMonitoring)
--     , clcSpotPrice :: !(Maybe ByteString)
--     , clcIamInstanceProfile :: !(Maybe ByteString)
--     , clcEbsOptimized :: !(Maybe Bool)
--     } deriving (Eq, Show, Generic)

-- instance IsQuery CreateLaunchConfiguration

-- instance AWSRequest AutoScaling CreateLaunchConfiguration MetadataResponse where
--     request = req GET "CreateLaunchConfiguration"

-- -- |
-- --
-- -- <http://docs.aws.amazon.com/AutoScaling/latest/APIReference/API_CreateOrUpdateTags.html>
-- newtype CreateOrUpdateTags = CreateOrUpdateTags [Tag]
--     deriving (Eq, Show, Generic)

-- instance IsQuery CreateOrUpdateTags

-- instance AWSRequest AutoScaling CreateOrUpdateTags MetadataResponse where
--     request = req GET "CreateOrUpdateTags"

-- -- |
-- --
-- -- <http://docs.aws.amazon.com/AutoScaling/latest/APIReference/API_DeleteAutoScalingGroup.html>
-- data DeleteAutoScalingGroup = DeleteAutoScalingGroup
--     { dasgAutoScalingGroupName :: !ResourceName
--     , dasgForceDelete :: !(Maybe Bool)
--     } deriving (Eq, Show, Generic)

-- instance IsQuery DeleteAutoScalingGroup

-- instance AWSRequest AutoScaling DeleteAutoScalingGroup MetadataResponse where
--     request = req GET "DeleteAutoScalingGroup"

-- -- |
-- --
-- -- <http://docs.aws.amazon.com/AutoScaling/latest/APIReference/API_DeleteLaunchConfiguration.html>
-- data DeleteLaunchConfiguration = DeleteLaunchConfiguration
--     { dlcLaunchConfigurationName :: !ResourceName
--     } deriving (Eq, Show, Generic)

-- instance IsQuery DeleteLaunchConfiguration

-- instance AWSRequest AutoScaling DeleteLaunchConfiguration MetadataResponse where
--     request = req GET "DeleteLaunchConfiguration"

-- -- |
-- --
-- -- <http://docs.aws.amazon.com/AutoScaling/latest/APIReference/API_DeleteNotificationConfiguration.html>
-- data DeleteNotificationConfiguration = DeleteNotificationConfiguration
--     { dncAutoScalingGroupName :: !ResourceName
--     , dncTopicARN :: !ResourceName
--     } deriving (Eq, Show, Generic)

-- instance IsQuery DeleteNotificationConfiguration

-- instance AWSRequest AutoScaling DeleteNotificationConfiguration MetadataResponse where
--     request = req GET "DeleteNotificationConfiguration"

-- -- |
-- --
-- -- <http://docs.aws.amazon.com/AutoScaling/latest/APIReference/API_DeletePolicy.html>
-- data DeletePolicy = DeletePolicy
--     { dpAutoScalingGroupName :: !(Maybe ResourceName)
--     , dpPolicyName :: !ResourceName
--     } deriving (Eq, Show, Generic)

-- instance IsQuery DeletePolicy

-- instance AWSRequest AutoScaling DeletePolicy MetadataResponse where
--     request = req GET "DeletePolicy"

-- -- |
-- --
-- -- <http://docs.aws.amazon.com/AutoScaling/latest/APIReference/API_DeleteScheduledAction.html>
-- data DeleteScheduledAction = DeleteScheduledAction
--     { dsaAutoScalingGroupName :: !(Maybe ResourceName)
--     , dsaScheduledActionName :: !ResourceName
--     } deriving (Eq, Show, Generic)

-- instance IsQuery DeleteScheduledAction

-- instance AWSRequest AutoScaling DeleteScheduledAction MetadataResponse where
--     request = req GET "DeleteScheduledAction"

-- -- |
-- --
-- -- <http://docs.aws.amazon.com/AutoScaling/latest/APIReference/API_DeleteTags.html>
-- data DeleteTags = DeleteTags [Tag]
--     deriving (Eq, Show, Generic)

-- instance IsQuery DeleteTags

-- instance AWSRequest AutoScaling DeleteTags MetadataResponse where
--     request = req GET "DeleteTags"

-- -- |
-- --
-- -- <http://docs.aws.amazon.com/AutoScaling/latest/APIReference/API_DescribeAdjustmentTypes.html>
-- data DescribeAdjustmentTypes = DescribeAdjustmentTypes
--     deriving (Eq, Show, Generic)

-- instance IsQuery DescribeAdjustmentTypes

-- instance AWSRequest AutoScaling DescribeAdjustmentTypes DescribeAdjustmentTypesResponse where
--     request = req GET "DescribeAdjustmentTypes"

-- -- |
-- --
-- -- <http://docs.aws.amazon.com/AutoScaling/latest/APIReference/API_DescribeAutoScalingGroups.html>
-- data DescribeAutoScalingGroups = DescribeAutoScalingGroups
--     { dasgAutoScalingGroupNames :: !(Maybe AutoScalingGroupNames)
--     , dasgNextToken :: !(Maybe ByteString)
--     , dasgMaxRecords :: !(Maybe Integer)
--     } deriving (Eq, Show, Generic)

-- instance IsQuery DescribeAutoScalingGroups

-- instance AWSRequest AutoScaling DescribeAutoScalingGroups DescribeAutoScalingGroupsResponse where
--     request = req GET "DescribeAutoScalingGroups"

-- -- |
-- --
-- -- <http://docs.aws.amazon.com/AutoScaling/latest/APIReference/API_DescribeAutoScalingInstances.html>
-- data DescribeAutoScalingInstances = DescribeAutoScalingInstances
--     { dasiInstanceIds :: !(Maybe InstanceIds)
--     , dasiMaxRecords :: !(Maybe Integer)
--     , dasiNextToken :: !(Maybe ByteString)
--     } deriving (Eq, Show, Generic)

-- instance IsQuery DescribeAutoScalingInstances

-- instance AWSRequest AutoScaling DescribeAutoScalingInstances DescribeAutoScalingInstancesResponse where
--     request = req GET "DescribeAutoScalingInstances"

-- -- |
-- --
-- -- <http://docs.aws.amazon.com/AutoScaling/latest/APIReference/API_DescribeLaunchConfigurations.html>
-- data DescribeLaunchConfigurations = DescribeLaunchConfigurations
--     { dlcLaunchConfigurationNames :: !(Maybe LaunchConfigurationNames)
--     , dlcNextToken :: !(Maybe ByteString)
--     , dlcMaxRecords :: !(Maybe Integer)
--     } deriving (Eq, Show, Generic)

-- instance IsQuery DescribeLaunchConfigurations

-- instance AWSRequest AutoScaling DescribeLaunchConfigurations DescribeLaunchConfigurationsResponse where
--     request = req GET "DescribeLaunchConfigurations"

-- -- |
-- --
-- -- <http://docs.aws.amazon.com/AutoScaling/latest/APIReference/API_DescribeMetricCollectionTypes.html>
-- data DescribeMetricCollectionTypes = DescribeMetricCollectionTypes
--     deriving (Eq, Show, Generic)

-- instance IsQuery DescribeMetricCollectionTypes

-- instance AWSRequest AutoScaling DescribeMetricCollectionTypes DescribeMetricCollectionTypesResponse where
--     request = req GET "DescribeMetricCollectionTypes"

-- -- |
-- --
-- -- <http://docs.aws.amazon.com/AutoScaling/latest/APIReference/API_DescribeNotificationConfigurations.html>
-- data DescribeNotificationConfigurations = DescribeNotificationConfigurations
--     { dncAutoScalingGroupNames :: !(Maybe AutoScalingGroupNames)
--     , dncNextToken :: !(Maybe ByteString)
--     , dncMaxRecords :: !(Maybe Integer)
--     } deriving (Eq, Show, Generic)

-- instance IsQuery DescribeNotificationConfigurations

-- instance AWSRequest AutoScaling DescribeNotificationConfigurations DescribeNotificationConfigurationsResponse where
--     request = req GET "DescribeNotificationConfigurations"

-- -- |
-- --
-- -- <http://docs.aws.amazon.com/AutoScaling/latest/APIReference/API_DescribePolicies.html>
-- data DescribePolicies = DescribePolicies
--     { dpsAutoScalingGroupName :: !(Maybe ResourceName)
--     , dpsPolicyNames :: !(Maybe PolicyNames)
--     , dpsNextToken :: !(Maybe ByteString)
--     , dpsMaxRecords :: !(Maybe Integer)
--     } deriving (Eq, Show, Generic)

-- instance IsQuery DescribePolicies

-- instance AWSRequest AutoScaling DescribePolicies DescribePoliciesResponse where
--     request = req GET "DescribePolicies"

-- -- |
-- --
-- -- <http://docs.aws.amazon.com/AutoScaling/latest/APIReference/API_DescribeScalingProcessTypes.html>
-- data DescribeScalingProcessTypes = DescribeScalingProcessTypes
--     deriving (Eq, Show, Generic)

-- instance IsQuery DescribeScalingProcessTypes

-- instance AWSRequest AutoScaling DescribeScalingProcessTypes DescribeScalingProcessTypesResponse where
--     request = req GET "DescribeScalingProcessTypes"

-- -- |
-- --
-- -- <http://docs.aws.amazon.com/AutoScaling/latest/APIReference/API_DescribeScalingActivities.html>
-- data DescribeScalingActivities = DescribeScalingActivities
--     { dsasActivityIds :: !(Maybe ActivityIds)
--     , dsasAutoScalingGroupName :: !(Maybe ResourceName)
--     , dsasMaxRecords :: !(Maybe Integer)
--     , dsasNextToken :: !(Maybe ByteString)
--     } deriving (Eq, Show, Generic)

-- instance IsQuery DescribeScalingActivities

-- instance AWSRequest AutoScaling DescribeScalingActivities DescribeScalingActivitiesResponse where
--     request = req GET "DescribeScalingActivities"

-- -- |
-- --
-- -- <http://docs.aws.amazon.com/AutoScaling/latest/APIReference/API_DescribeScheduledActions.html>
-- data DescribeScheduledActions = DescribeScheduledActions
--     { dsacAutoScalingGroupName :: !(Maybe ResourceName)
--     , dsacScheduledActionNames :: !(Maybe ScheduledActionNames)
--     , dsacStartTime :: !(Maybe UTCTime)
--     , dsacEndTime :: !(Maybe UTCTime)
--     , dsacNextToken :: !(Maybe ByteString)
--     , dsacMaxRecords :: !(Maybe Integer)
--     } deriving (Eq, Show, Generic)

-- instance IsQuery DescribeScheduledActions

-- instance AWSRequest AutoScaling DescribeScheduledActions DescribeScheduledActionsResponse where
--     request = req GET "DescribeScheduledActions"

-- -- |
-- --
-- -- <http://docs.aws.amazon.com/AutoScaling/latest/APIReference/API_DescribeTags.html>
-- data DescribeTags = DescribeTags
--     { dtFilters :: !(Maybe Filters)
--     , dtNextToken :: !(Maybe ByteString)
--     , dtMaxRecords :: !(Maybe Integer)
--     } deriving (Eq, Show, Generic)

-- instance IsQuery DescribeTags

-- instance AWSRequest AutoScaling DescribeTags DescribeTagsResponse where
--     request = req GET "DescribeTags"

-- -- |
-- --
-- -- <http://docs.aws.amazon.com/AutoScaling/latest/APIReference/API_DescribeTerminationPolicyTypes.html>
-- data DescribeTerminationPolicyTypes = DescribeTerminationPolicyTypes
--     deriving (Eq, Show, Generic)

-- instance IsQuery DescribeTerminationPolicyTypes

-- instance AWSRequest AutoScaling DescribeTerminationPolicyTypes DescribeTerminationPolicyTypesResponse where
--     request = req GET "DescribeTerminationPolicyTypes"

-- -- |
-- --
-- -- <http://docs.aws.amazon.com/AutoScaling/latest/APIReference/API_DisableMetricsCollection.html>
-- data DisableMetricsCollection = DisableMetricsCollection
--     { dmcAutoScalingGroupName :: !ResourceName
--     , dmcMetrics :: !(Maybe Metrics)
--     } deriving (Eq, Show, Generic)

-- instance IsQuery DisableMetricsCollection

-- instance AWSRequest AutoScaling DisableMetricsCollection MetadataResponse where
--     request = req GET "DisableMetricsCollection"

-- -- |
-- --
-- -- <http://docs.aws.amazon.com/AutoScaling/latest/APIReference/API_EnableMetricsCollection.html>
-- data EnableMetricsCollection = EnableMetricsCollection
--     { emcAutoScalingGroupName :: !ResourceName
--     , emcMetrics :: !(Maybe Metrics)
--     , emcGranularity :: !ByteString
--     } deriving (Eq, Show, Generic)

-- instance IsQuery EnableMetricsCollection

-- instance AWSRequest AutoScaling EnableMetricsCollection MetadataResponse where
--     request = req GET "EnableMetricsCollection"

-- -- |
-- --
-- -- <http://docs.aws.amazon.com/AutoScaling/latest/APIReference/API_ExecutePolicy.html>
-- data ExecutePolicy = ExecutePolicy
--     { epAutoScalingGroupName :: !(Maybe ResourceName)
--     , epPolicyName :: !ResourceName
--     , epHonorCooldown :: !(Maybe Bool)
--     } deriving (Eq, Show, Generic)

-- instance IsQuery ExecutePolicy

-- instance AWSRequest AutoScaling ExecutePolicy MetadataResponse where
--     request = req GET "ExecutePolicy"

-- -- |
-- --
-- -- <http://docs.aws.amazon.com/AutoScaling/latest/APIReference/API_PutNotificationConfiguration.html>
-- data PutNotificationConfiguration = PutNotificationConfiguration
--     { pncAutoScalingGroupName :: !ResourceName
--     , pncTopicARN :: !ResourceName
--     , pncNotificationTypes :: !AutoScalingNotificationTypes
--     } deriving (Eq, Show, Generic)

-- instance IsQuery PutNotificationConfiguration

-- instance AWSRequest AutoScaling PutNotificationConfiguration MetadataResponse where
--     request = req GET "PutNotificationConfiguration"

-- -- |
-- --
-- -- <http://docs.aws.amazon.com/AutoScaling/latest/APIReference/API_PutScalingPolicy.html>
-- data PutScalingPolicy = PutScalingPolicy
--     { pspAutoScalingGroupName :: !ResourceName
--     , pspPolicyName :: !ByteString
--     , pspScalingAdjustment :: !Integer
--     , pspAdjustmentType :: !ByteString
--     , pspCooldown :: !(Maybe Integer)
--     , pspMinAdjustmentStep :: !(Maybe Integer)
--     } deriving (Eq, Show, Generic)

-- instance IsQuery PutScalingPolicy

-- instance AWSRequest AutoScaling PutScalingPolicy PutScalingPolicyResponse where
--     request = req GET "PutScalingPolicy"

-- -- |
-- --
-- -- <http://docs.aws.amazon.com/AutoScaling/latest/APIReference/API_PutScheduledUpdateGroupAction.html>
-- data PutScheduledUpdateGroupAction = PutScheduledUpdateGroupAction
--     { psugaAutoScalingGroupName :: !ResourceName
--     , psugaScheduledActionName :: !ByteString
--     , psugaTime :: !(Maybe UTCTime)
--     , psugaStartTime :: !(Maybe UTCTime)
--     , psugaEndTime :: !(Maybe UTCTime)
--     , psugaRecurrence :: !(Maybe ByteString)
--     , psugaMinSize :: !(Maybe Integer)
--     , psugaMaxSize :: !(Maybe Integer)
--     , psugaDesiredCapacity :: !(Maybe Integer)
--     } deriving (Eq, Show, Generic)

-- instance IsQuery PutScheduledUpdateGroupAction

-- instance AWSRequest AutoScaling PutScheduledUpdateGroupAction MetadataResponse where
--     request = req GET "PutScheduledUpdateGroupAction"

-- -- |
-- --
-- -- <http://docs.aws.amazon.com/AutoScaling/latest/APIReference/API_ResumeProcesses.html>
-- data ResumeProcesses = ResumeProcesses
--     { rpAutoScalingGroupName :: !ResourceName
--     , rpScalingProcesses :: !(Maybe ProcessNames)
--     } deriving (Eq, Show, Generic)

-- instance IsQuery ResumeProcesses

-- instance AWSRequest AutoScaling ResumeProcesses MetadataResponse where
--     request = req GET "ResumeProcesses"

-- -- |
-- --
-- -- <http://docs.aws.amazon.com/AutoScaling/latest/APIReference/API_SetDesiredCapacity.html>
-- data SetDesiredCapacity = SetDesiredCapacity
--     { sdcAutoScalingGroupName :: !ResourceName
--     , sdcDesiredCapacity :: !Integer
--     , sdcHonorCooldown :: !(Maybe Bool)
--     } deriving (Eq, Show, Generic)

-- instance IsQuery SetDesiredCapacity

-- instance AWSRequest AutoScaling SetDesiredCapacity MetadataResponse where
--     request = req GET "SetDesiredCapacity"

-- -- |
-- --
-- -- <http://docs.aws.amazon.com/AutoScaling/latest/APIReference/API_SetInstanceHealth.html>
-- data SetInstanceHealth = SetInstanceHealth
--     { sihInstanceId :: !ByteString
--     , sihHealthStatus :: !ByteString
--     , sihShouldRespectGracePeriod :: !(Maybe Bool)
--     } deriving (Eq, Show, Generic)

-- instance IsQuery SetInstanceHealth

-- instance AWSRequest AutoScaling SetInstanceHealth MetadataResponse where
--     request = req GET "SetInstanceHealth"

-- -- |
-- --
-- -- <http://docs.aws.amazon.com/AutoScaling/latest/APIReference/API_SuspendProcesses.html>
-- data SuspendProcesses = SuspendProcesses
--     { spAutoScalingGroupName :: !ResourceName
--     , spScalingProcesses :: !(Maybe ProcessNames)
--     } deriving (Eq, Show, Generic)

-- instance IsQuery SuspendProcesses

-- instance AWSRequest AutoScaling SuspendProcesses MetadataResponse where
--     request = req GET "SuspendProcesses"

-- -- |
-- --
-- -- <http://docs.aws.amazon.com/AutoScaling/latest/APIReference/API_TerminateInstanceInAutoScalingGroup.html>
-- data TerminateInstanceInAutoScalingGroup = TerminateInstanceInAutoScalingGroup
--     { tiiasgInstanceId :: !ByteString
--     , tiiasgShouldDecrementDesiredCapacity :: !Bool
--     } deriving (Eq, Show, Generic)

-- instance IsQuery TerminateInstanceInAutoScalingGroup

-- instance AWSRequest AutoScaling TerminateInstanceInAutoScalingGroup TerminateInstanceInAutoScalingGroupResponse where
--     request = req GET "TerminateInstanceInAutoScalingGroup"

-- -- |
-- --
-- -- <http://docs.aws.amazon.com/AutoScaling/latest/APIReference/API_UpdateAutoScalingGroup.html>
-- data UpdateAutoScalingGroup = UpdateAutoScalingGroup
--     { uasgAutoScalingGroupName :: !ResourceName
--     , uasgLaunchConfigurationName :: !(Maybe ResourceName)
--     , uasgMinSize :: !(Maybe Integer)
--     , uasgMaxSize :: !(Maybe Integer)
--     , uasgDesiredCapacity :: !(Maybe Integer)
--     , uasgDefaultCooldown :: !(Maybe Integer)
--     , uasgAvailabilityZones :: !(Maybe AvailabilityZones)
--     , uasgHealthCheckType :: !(Maybe ByteString)
--     , uasgHealthCheckGracePeriod :: !(Maybe Integer)
--     , uasgPlacementGroup :: !(Maybe ByteString)
--     , uasgVPCZoneIdentifier :: !(Maybe ByteString)
--     , uasgTerminationPolicies :: !(Param Member ByteString)
--     } deriving (Eq, Show, Generic)

-- instance IsQuery UpdateAutoScalingGroup

-- instance AWSRequest AutoScaling UpdateAutoScalingGroup MetadataResponse where
--     request = req GET "UpdateAutoScalingGroup"
