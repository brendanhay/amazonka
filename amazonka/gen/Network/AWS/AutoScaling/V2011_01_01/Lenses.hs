{-# LANGUAGE TemplateHaskell             #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.AutoScaling.V2011_01_01.Lenses
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.AutoScaling.V2011_01_01.Lenses where

import Control.Lens.TH
import Network.AWS.AutoScaling.V2011_01_01.Types
import Network.AWS.AutoScaling.V2011_01_01.DescribeMetricCollectionTypes
import Network.AWS.AutoScaling.V2011_01_01.PutNotificationConfiguration
import Network.AWS.AutoScaling.V2011_01_01.DescribeTags
import Network.AWS.AutoScaling.V2011_01_01.DeleteNotificationConfiguration
import Network.AWS.AutoScaling.V2011_01_01.PutScalingPolicy
import Network.AWS.AutoScaling.V2011_01_01.DeleteLaunchConfiguration
import Network.AWS.AutoScaling.V2011_01_01.SuspendProcesses
import Network.AWS.AutoScaling.V2011_01_01.SetInstanceHealth
import Network.AWS.AutoScaling.V2011_01_01.DescribeTerminationPolicyTypes
import Network.AWS.AutoScaling.V2011_01_01.DescribeAutoScalingInstances
import Network.AWS.AutoScaling.V2011_01_01.DisableMetricsCollection
import Network.AWS.AutoScaling.V2011_01_01.EnableMetricsCollection
import Network.AWS.AutoScaling.V2011_01_01.DescribeScalingProcessTypes
import Network.AWS.AutoScaling.V2011_01_01.DeleteTags
import Network.AWS.AutoScaling.V2011_01_01.DescribeAutoScalingGroups
import Network.AWS.AutoScaling.V2011_01_01.DeleteScheduledAction
import Network.AWS.AutoScaling.V2011_01_01.SetDesiredCapacity
import Network.AWS.AutoScaling.V2011_01_01.DescribeAutoScalingNotificationTypes
import Network.AWS.AutoScaling.V2011_01_01.DescribeScheduledActions
import Network.AWS.AutoScaling.V2011_01_01.CreateOrUpdateTags
import Network.AWS.AutoScaling.V2011_01_01.DeletePolicy
import Network.AWS.AutoScaling.V2011_01_01.AttachInstances
import Network.AWS.AutoScaling.V2011_01_01.UpdateAutoScalingGroup
import Network.AWS.AutoScaling.V2011_01_01.DeleteAutoScalingGroup
import Network.AWS.AutoScaling.V2011_01_01.ResumeProcesses
import Network.AWS.AutoScaling.V2011_01_01.ExecutePolicy
import Network.AWS.AutoScaling.V2011_01_01.DescribeAccountLimits
import Network.AWS.AutoScaling.V2011_01_01.TerminateInstanceInAutoScalingGroup
import Network.AWS.AutoScaling.V2011_01_01.PutScheduledUpdateGroupAction
import Network.AWS.AutoScaling.V2011_01_01.DescribePolicies
import Network.AWS.AutoScaling.V2011_01_01.DescribeLaunchConfigurations
import Network.AWS.AutoScaling.V2011_01_01.DescribeScalingActivities
import Network.AWS.AutoScaling.V2011_01_01.DescribeNotificationConfigurations
import Network.AWS.AutoScaling.V2011_01_01.DescribeAdjustmentTypes
import Network.AWS.AutoScaling.V2011_01_01.CreateAutoScalingGroup
import Network.AWS.AutoScaling.V2011_01_01.CreateLaunchConfiguration

-- Newtypes
makeIso ''AdjustmentType
makeIso ''InstanceMonitoring
makeIso ''MetricCollectionType
makeIso ''MetricGranularityType
makeIso ''ProcessType

-- Products
makeLenses ''Activity
makeLenses ''Alarm
makeLenses ''AutoScalingGroup
makeLenses ''AutoScalingInstanceDetails
makeLenses ''BlockDeviceMapping
makeLenses ''Ebs
makeLenses ''EnabledMetric
makeLenses ''Filter
makeLenses ''Instance
makeLenses ''LaunchConfiguration
makeLenses ''NotificationConfiguration
makeLenses ''ScalingPolicy
makeLenses ''ScheduledUpdateGroupAction
makeLenses ''SuspendedProcess
makeLenses ''Tag
makeLenses ''TagDescription

-- Requests
makeLenses ''DescribeMetricCollectionTypes
makeLenses ''PutNotificationConfiguration
makeLenses ''DescribeTags
makeLenses ''DeleteNotificationConfiguration
makeLenses ''PutScalingPolicy
makeLenses ''DeleteLaunchConfiguration
makeLenses ''SuspendProcesses
makeLenses ''SetInstanceHealth
makeLenses ''DescribeTerminationPolicyTypes
makeLenses ''DescribeAutoScalingInstances
makeLenses ''DisableMetricsCollection
makeLenses ''EnableMetricsCollection
makeLenses ''DescribeScalingProcessTypes
makeLenses ''DeleteTags
makeLenses ''DescribeAutoScalingGroups
makeLenses ''DeleteScheduledAction
makeLenses ''SetDesiredCapacity
makeLenses ''DescribeAutoScalingNotificationTypes
makeLenses ''DescribeScheduledActions
makeLenses ''CreateOrUpdateTags
makeLenses ''DeletePolicy
makeLenses ''AttachInstances
makeLenses ''UpdateAutoScalingGroup
makeLenses ''DeleteAutoScalingGroup
makeLenses ''ResumeProcesses
makeLenses ''ExecutePolicy
makeLenses ''DescribeAccountLimits
makeLenses ''TerminateInstanceInAutoScalingGroup
makeLenses ''PutScheduledUpdateGroupAction
makeLenses ''DescribePolicies
makeLenses ''DescribeLaunchConfigurations
makeLenses ''DescribeScalingActivities
makeLenses ''DescribeNotificationConfigurations
makeLenses ''DescribeAdjustmentTypes
makeLenses ''CreateAutoScalingGroup
makeLenses ''CreateLaunchConfiguration

-- Responses
makeLenses ''DescribeMetricCollectionTypesResponse
makeLenses ''PutNotificationConfigurationResponse
makeLenses ''DescribeTagsResponse
makeLenses ''DeleteNotificationConfigurationResponse
makeLenses ''PutScalingPolicyResponse
makeLenses ''DeleteLaunchConfigurationResponse
makeLenses ''SuspendProcessesResponse
makeLenses ''SetInstanceHealthResponse
makeLenses ''DescribeTerminationPolicyTypesResponse
makeLenses ''DescribeAutoScalingInstancesResponse
makeLenses ''DisableMetricsCollectionResponse
makeLenses ''EnableMetricsCollectionResponse
makeLenses ''DescribeScalingProcessTypesResponse
makeLenses ''DeleteTagsResponse
makeLenses ''DescribeAutoScalingGroupsResponse
makeLenses ''DeleteScheduledActionResponse
makeLenses ''SetDesiredCapacityResponse
makeLenses ''DescribeAutoScalingNotificationTypesResponse
makeLenses ''DescribeScheduledActionsResponse
makeLenses ''CreateOrUpdateTagsResponse
makeLenses ''DeletePolicyResponse
makeLenses ''AttachInstancesResponse
makeLenses ''UpdateAutoScalingGroupResponse
makeLenses ''DeleteAutoScalingGroupResponse
makeLenses ''ResumeProcessesResponse
makeLenses ''ExecutePolicyResponse
makeLenses ''DescribeAccountLimitsResponse
makeLenses ''TerminateInstanceInAutoScalingGroupResponse
makeLenses ''PutScheduledUpdateGroupActionResponse
makeLenses ''DescribePoliciesResponse
makeLenses ''DescribeLaunchConfigurationsResponse
makeLenses ''DescribeScalingActivitiesResponse
makeLenses ''DescribeNotificationConfigurationsResponse
makeLenses ''DescribeAdjustmentTypesResponse
makeLenses ''CreateAutoScalingGroupResponse
makeLenses ''CreateLaunchConfigurationResponse
