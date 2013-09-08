{-# LANGUAGE TemplateHaskell      #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- Module      : Test.AutoScaling
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Test.AutoScaling (tests) where

import qualified Data.Text           as Text
import           Network.AWS.AutoScaling
import           Test.Common

tests :: [Test]
tests = (:[]) $ testVersion autoScalingVersion
    [ testGroup "Requests"
        [ testProperty "CreateAutoScalingGroup"                       (prop :: TRq CreateAutoScalingGroup)
        , testProperty "CreateLaunchConfiguration"                    (prop :: TRq CreateLaunchConfiguration)
        , testProperty "CreateOrUpdateTags"                           (prop :: TRq CreateOrUpdateTags)
        , testProperty "DeleteAutoScalingGroup"                       (prop :: TRq DeleteAutoScalingGroup)
        , testProperty "DeleteLaunchConfiguration"                    (prop :: TRq DeleteLaunchConfiguration)
        , testProperty "DeleteNotificationConfiguration"              (prop :: TRq DeleteNotificationConfiguration)
        , testProperty "DeletePolicy"                                 (prop :: TRq DeletePolicy)
        , testProperty "DeleteScheduledAction"                        (prop :: TRq DeleteScheduledAction)
        , testProperty "DeleteTags"                                   (prop :: TRq DeleteTags)
        , testProperty "DescribeAdjustmentTypes"                      (prop :: TRq DescribeAdjustmentTypes)
        , testProperty "DescribeAutoScalingGroups"                    (prop :: TRq DescribeAutoScalingGroups)
        , testProperty "DescribeAutoScalingInstances"                 (prop :: TRq DescribeAutoScalingInstances)
        , testProperty "DescribeAutoScalingNotificationTypes"         (prop :: TRq DescribeAutoScalingNotificationTypes)
        , testProperty "DescribeLaunchConfigurations"                 (prop :: TRq DescribeLaunchConfigurations)
        , testProperty "DescribeMetricCollectionTypes"                (prop :: TRq DescribeMetricCollectionTypes)
        , testProperty "DescribeNotificationConfigurations"           (prop :: TRq DescribeNotificationConfigurations)
        , testProperty "DescribePolicies"                             (prop :: TRq DescribePolicies)
        , testProperty "DescribeScalingActivities"                    (prop :: TRq DescribeScalingActivities)
        , testProperty "DescribeScalingProcessTypes"                  (prop :: TRq DescribeScalingProcessTypes)
        , testProperty "DescribeScheduledActions"                     (prop :: TRq DescribeScheduledActions)
        , testProperty "DescribeTags"                                 (prop :: TRq DescribeTags)
        , testProperty "DescribeTerminationPolicyTypes"               (prop :: TRq DescribeTerminationPolicyTypes)
        , testProperty "DisableMetricsCollection"                     (prop :: TRq DisableMetricsCollection)
        , testProperty "EnableMetricsCollection"                      (prop :: TRq EnableMetricsCollection)
        , testProperty "ExecutePolicy"                                (prop :: TRq ExecutePolicy)
        , testProperty "PutNotificationConfiguration"                 (prop :: TRq PutNotificationConfiguration)
        , testProperty "PutScalingPolicy"                             (prop :: TRq PutScalingPolicy)
        , testProperty "PutScheduledUpdateGroupAction"                (prop :: TRq PutScheduledUpdateGroupAction)
        , testProperty "ResumeProcesses"                              (prop :: TRq ResumeProcesses)
        , testProperty "SetDesiredCapacity"                           (prop :: TRq SetDesiredCapacity)
        , testProperty "SetInstanceHealth"                            (prop :: TRq SetInstanceHealth)
        , testProperty "SuspendProcesses"                             (prop :: TRq SuspendProcesses)
        , testProperty "TerminateInstanceInAutoScalingGroup"          (prop :: TRq TerminateInstanceInAutoScalingGroup)
        , testProperty "UpdateAutoScalingGroup"                       (prop :: TRq UpdateAutoScalingGroup)
        ]

    , testGroup "Responses"
        [ testProperty "CreateAutoScalingGroupResponse"               (prop :: TRs CreateAutoScalingGroupResponse)
        , testProperty "CreateLaunchConfigurationResponse"            (prop :: TRs CreateLaunchConfigurationResponse)
        , testProperty "CreateOrUpdateTagsResponse"                   (prop :: TRs CreateOrUpdateTagsResponse)
        , testProperty "DeleteAutoScalingGroupResponse"               (prop :: TRs DeleteAutoScalingGroupResponse)
        , testProperty "DeleteLaunchConfigurationResponse"            (prop :: TRs DeleteLaunchConfigurationResponse)
        , testProperty "DeleteNotificationConfigurationResponse"      (prop :: TRs DeleteNotificationConfigurationResponse)
        , testProperty "DeletePolicyResponse"                         (prop :: TRs DeletePolicyResponse)
        , testProperty "DeleteScheduledActionResponse"                (prop :: TRs DeleteScheduledActionResponse)
        , testProperty "DeleteTagsResponse"                           (prop :: TRs DeleteTagsResponse)
        , testProperty "DescribeAdjustmentTypesResponse"              (prop :: TRs DescribeAdjustmentTypesResponse)
        -- , testProperty "DescribeAutoScalingGroupsResponse"            (prop :: TRs DescribeAutoScalingGroupsResponse)
        , testProperty "DescribeAutoScalingInstancesResponse"         (prop :: TRs DescribeAutoScalingInstancesResponse)
        , testProperty "DescribeAutoScalingNotificationTypesResponse" (prop :: TRs DescribeAutoScalingNotificationTypesResponse)
        , testProperty "DescribeLaunchConfigurationsResponse"         (prop :: TRs DescribeLaunchConfigurationsResponse)
        , testProperty "DescribeMetricCollectionTypesResponse"        (prop :: TRs DescribeMetricCollectionTypesResponse)
        , testProperty "DescribeNotificationConfigurationsResponse"   (prop :: TRs DescribeNotificationConfigurationsResponse)
        , testProperty "DescribePoliciesResponse"                     (prop :: TRs DescribePoliciesResponse)
        , testProperty "DescribeScalingActivitiesResponse"            (prop :: TRs DescribeScalingActivitiesResponse)
        , testProperty "DescribeScalingProcessTypesResponse"          (prop :: TRs DescribeScalingProcessTypesResponse)
        , testProperty "DescribeScheduledActionsResponse"             (prop :: TRs DescribeScheduledActionsResponse)
        , testProperty "DescribeTagsResponse"                         (prop :: TRs DescribeTagsResponse)
        , testProperty "DescribeTerminationPolicyTypesResponse"       (prop :: TRs DescribeTerminationPolicyTypesResponse)
        , testProperty "DisableMetricsCollectionResponse"             (prop :: TRs DisableMetricsCollectionResponse)
        , testProperty "EnableMetricsCollectionResponse"              (prop :: TRs EnableMetricsCollectionResponse)
        , testProperty "ExecutePolicyResponse"                        (prop :: TRs ExecutePolicyResponse)
        , testProperty "PutNotificationConfigurationResponse"         (prop :: TRs PutNotificationConfigurationResponse)
        , testProperty "PutScalingPolicyResponse"                     (prop :: TRs PutScalingPolicyResponse)
        , testProperty "PutScheduledUpdateGroupActionResponse"        (prop :: TRs PutScheduledUpdateGroupActionResponse)
        , testProperty "ResumeProcessesResponse"                      (prop :: TRs ResumeProcessesResponse)
        , testProperty "SetDesiredCapacityResponse"                   (prop :: TRs SetDesiredCapacityResponse)
        , testProperty "SetInstanceHealthResponse"                    (prop :: TRs SetInstanceHealthResponse)
        , testProperty "SuspendProcessesResponse"                     (prop :: TRs SuspendProcessesResponse)
        , testProperty "TerminateInstanceInAutoScalingGroupResponse"  (prop :: TRs TerminateInstanceInAutoScalingGroupResponse)
        , testProperty "UpdateAutoScalingGroupResponse"               (prop :: TRs UpdateAutoScalingGroupResponse)
        ]
    ]

instance ToJSON AdjustmentType where
    toJSON = stringify . show

$(deriveArbitrary
    [ ''AdjustmentType
    ])

$(deriveDependency
    [ ''Activity
    , ''Alarm
    , ''AutoScalingGroup
    , ''AutoScalingInstanceDetails
    , ''BlockDeviceMapping
    , ''DescribeAdjustmentTypesResult
    , ''DescribeAutoScalingGroupsResult
    , ''DescribeAutoScalingInstancesResult
    , ''DescribeAutoScalingNotificationTypesResult
    , ''DescribeLaunchConfigurationsResult
    , ''DescribeMetricCollectionTypesResult
    , ''DescribeNotificationConfigurationsResult
    , ''DescribePoliciesResult
    , ''DescribeScalingActivitiesResult
    , ''DescribeScalingProcessTypesResult
    , ''DescribeScheduledActionsResult
    , ''DescribeTagsResult
    , ''DescribeTerminationPolicyTypesResult
    , ''Ebs
    , ''EnabledMetric
    , ''Filter
    , ''Instance
    , ''InstanceMonitoring
    , ''LaunchConfiguration
    , ''MetricCollectionType
    , ''MetricGranularityType
    , ''NotificationConfiguration
    , ''ProcessType
    , ''PutScalingPolicyResult
    , ''ScalingPolicy
    , ''ScheduledUpdateGroupAction
    , ''SuspendedProcess
    , ''Tag
    , ''TerminateInstanceInAutoScalingGroupResult
    , ''ResponseMetadata
    ])

$(deriveProperty "test/resources/AutoScaling"
    [ ''CreateAutoScalingGroup
    , ''CreateAutoScalingGroupResponse
    , ''CreateLaunchConfiguration
    , ''CreateLaunchConfigurationResponse
    , ''CreateOrUpdateTags
    , ''CreateOrUpdateTagsResponse
    , ''DeleteAutoScalingGroup
    , ''DeleteAutoScalingGroupResponse
    , ''DeleteLaunchConfiguration
    , ''DeleteLaunchConfigurationResponse
    , ''DeleteNotificationConfiguration
    , ''DeleteNotificationConfigurationResponse
    , ''DeletePolicy
    , ''DeletePolicyResponse
    , ''DeleteScheduledAction
    , ''DeleteScheduledActionResponse
    , ''DeleteTags
    , ''DeleteTagsResponse
    , ''DescribeAdjustmentTypes
    , ''DescribeAdjustmentTypesResponse
    , ''DescribeAutoScalingGroups
    , ''DescribeAutoScalingGroupsResponse
    , ''DescribeAutoScalingInstances
    , ''DescribeAutoScalingInstancesResponse
    , ''DescribeAutoScalingNotificationTypes
    , ''DescribeAutoScalingNotificationTypesResponse
    , ''DescribeLaunchConfigurations
    , ''DescribeLaunchConfigurationsResponse
    , ''DescribeMetricCollectionTypes
    , ''DescribeMetricCollectionTypesResponse
    , ''DescribeNotificationConfigurations
    , ''DescribeNotificationConfigurationsResponse
    , ''DescribePolicies
    , ''DescribePoliciesResponse
    , ''DescribeScalingActivities
    , ''DescribeScalingActivitiesResponse
    , ''DescribeScalingProcessTypes
    , ''DescribeScalingProcessTypesResponse
    , ''DescribeScheduledActions
    , ''DescribeScheduledActionsResponse
    , ''DescribeTags
    , ''DescribeTagsResponse
    , ''DescribeTerminationPolicyTypes
    , ''DescribeTerminationPolicyTypesResponse
    , ''DisableMetricsCollection
    , ''DisableMetricsCollectionResponse
    , ''EnableMetricsCollection
    , ''EnableMetricsCollectionResponse
    , ''ExecutePolicy
    , ''ExecutePolicyResponse
    , ''PutNotificationConfiguration
    , ''PutNotificationConfigurationResponse
    , ''PutScalingPolicy
    , ''PutScalingPolicyResponse
    , ''PutScheduledUpdateGroupAction
    , ''PutScheduledUpdateGroupActionResponse
    , ''ResumeProcesses
    , ''ResumeProcessesResponse
    , ''SetDesiredCapacity
    , ''SetDesiredCapacityResponse
    , ''SetInstanceHealth
    , ''SetInstanceHealthResponse
    , ''SuspendProcesses
    , ''SuspendProcessesResponse
    , ''TerminateInstanceInAutoScalingGroup
    , ''TerminateInstanceInAutoScalingGroupResponse
    , ''UpdateAutoScalingGroup
    , ''UpdateAutoScalingGroupResponse
    ])
