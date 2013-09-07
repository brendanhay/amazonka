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
        [ testProperty "CreateAutoScalingGroup"                       (prop :: Rq CreateAutoScalingGroup)
        , testProperty "CreateLaunchConfiguration"                    (prop :: Rq CreateLaunchConfiguration)
        , testProperty "CreateOrUpdateTags"                           (prop :: Rq CreateOrUpdateTags)
        , testProperty "DeleteAutoScalingGroup"                       (prop :: Rq DeleteAutoScalingGroup)
        , testProperty "DeleteLaunchConfiguration"                    (prop :: Rq DeleteLaunchConfiguration)
        , testProperty "DeleteNotificationConfiguration"              (prop :: Rq DeleteNotificationConfiguration)
        , testProperty "DeletePolicy"                                 (prop :: Rq DeletePolicy)
        , testProperty "DeleteScheduledAction"                        (prop :: Rq DeleteScheduledAction)
        , testProperty "DeleteTags"                                   (prop :: Rq DeleteTags)
        , testProperty "DescribeAdjustmentTypes"                      (prop :: Rq DescribeAdjustmentTypes)
        , testProperty "DescribeAutoScalingGroups"                    (prop :: Rq DescribeAutoScalingGroups)
        , testProperty "DescribeAutoScalingInstances"                 (prop :: Rq DescribeAutoScalingInstances)
        , testProperty "DescribeAutoScalingNotificationTypes"         (prop :: Rq DescribeAutoScalingNotificationTypes)
        , testProperty "DescribeLaunchConfigurations"                 (prop :: Rq DescribeLaunchConfigurations)
        , testProperty "DescribeMetricCollectionTypes"                (prop :: Rq DescribeMetricCollectionTypes)
        , testProperty "DescribeNotificationConfigurations"           (prop :: Rq DescribeNotificationConfigurations)
        , testProperty "DescribePolicies"                             (prop :: Rq DescribePolicies)
        , testProperty "DescribeScalingActivities"                    (prop :: Rq DescribeScalingActivities)
        , testProperty "DescribeScalingProcessTypes"                  (prop :: Rq DescribeScalingProcessTypes)
        , testProperty "DescribeScheduledActions"                     (prop :: Rq DescribeScheduledActions)
        , testProperty "DescribeTags"                                 (prop :: Rq DescribeTags)
        , testProperty "DescribeTerminationPolicyTypes"               (prop :: Rq DescribeTerminationPolicyTypes)
        , testProperty "DisableMetricsCollection"                     (prop :: Rq DisableMetricsCollection)
        , testProperty "EnableMetricsCollection"                      (prop :: Rq EnableMetricsCollection)
        , testProperty "ExecutePolicy"                                (prop :: Rq ExecutePolicy)
        , testProperty "PutNotificationConfiguration"                 (prop :: Rq PutNotificationConfiguration)
        , testProperty "PutScalingPolicy"                             (prop :: Rq PutScalingPolicy)
        , testProperty "PutScheduledUpdateGroupAction"                (prop :: Rq PutScheduledUpdateGroupAction)
        , testProperty "ResumeProcesses"                              (prop :: Rq ResumeProcesses)
        , testProperty "SetDesiredCapacity"                           (prop :: Rq SetDesiredCapacity)
        , testProperty "SetInstanceHealth"                            (prop :: Rq SetInstanceHealth)
        , testProperty "SuspendProcesses"                             (prop :: Rq SuspendProcesses)
        , testProperty "TerminateInstanceInAutoScalingGroup"          (prop :: Rq TerminateInstanceInAutoScalingGroup)
        , testProperty "UpdateAutoScalingGroup"                       (prop :: Rq UpdateAutoScalingGroup)
        ]

    , testGroup "Responses"
        [ testProperty "CreateAutoScalingGroupResponse"               (prop :: Rs CreateAutoScalingGroupResponse)
        , testProperty "CreateLaunchConfigurationResponse"            (prop :: Rs CreateLaunchConfigurationResponse)
        , testProperty "CreateOrUpdateTagsResponse"                   (prop :: Rs CreateOrUpdateTagsResponse)
        , testProperty "DeleteAutoScalingGroupResponse"               (prop :: Rs DeleteAutoScalingGroupResponse)
        , testProperty "DeleteLaunchConfigurationResponse"            (prop :: Rs DeleteLaunchConfigurationResponse)
        , testProperty "DeleteNotificationConfigurationResponse"      (prop :: Rs DeleteNotificationConfigurationResponse)
        , testProperty "DeletePolicyResponse"                         (prop :: Rs DeletePolicyResponse)
        , testProperty "DeleteScheduledActionResponse"                (prop :: Rs DeleteScheduledActionResponse)
        , testProperty "DeleteTagsResponse"                           (prop :: Rs DeleteTagsResponse)
        , testProperty "DescribeAdjustmentTypesResponse"              (prop :: Rs DescribeAdjustmentTypesResponse)
        -- , testProperty "DescribeAutoScalingGroupsResponse"            (prop :: Rs DescribeAutoScalingGroupsResponse)
        , testProperty "DescribeAutoScalingInstancesResponse"         (prop :: Rs DescribeAutoScalingInstancesResponse)
        , testProperty "DescribeAutoScalingNotificationTypesResponse" (prop :: Rs DescribeAutoScalingNotificationTypesResponse)
        , testProperty "DescribeLaunchConfigurationsResponse"         (prop :: Rs DescribeLaunchConfigurationsResponse)
        , testProperty "DescribeMetricCollectionTypesResponse"        (prop :: Rs DescribeMetricCollectionTypesResponse)
        , testProperty "DescribeNotificationConfigurationsResponse"   (prop :: Rs DescribeNotificationConfigurationsResponse)
        , testProperty "DescribePoliciesResponse"                     (prop :: Rs DescribePoliciesResponse)
        , testProperty "DescribeScalingActivitiesResponse"            (prop :: Rs DescribeScalingActivitiesResponse)
        , testProperty "DescribeScalingProcessTypesResponse"          (prop :: Rs DescribeScalingProcessTypesResponse)
        , testProperty "DescribeScheduledActionsResponse"             (prop :: Rs DescribeScheduledActionsResponse)
        , testProperty "DescribeTagsResponse"                         (prop :: Rs DescribeTagsResponse)
        , testProperty "DescribeTerminationPolicyTypesResponse"       (prop :: Rs DescribeTerminationPolicyTypesResponse)
        , testProperty "DisableMetricsCollectionResponse"             (prop :: Rs DisableMetricsCollectionResponse)
        , testProperty "EnableMetricsCollectionResponse"              (prop :: Rs EnableMetricsCollectionResponse)
        , testProperty "ExecutePolicyResponse"                        (prop :: Rs ExecutePolicyResponse)
        , testProperty "PutNotificationConfigurationResponse"         (prop :: Rs PutNotificationConfigurationResponse)
        , testProperty "PutScalingPolicyResponse"                     (prop :: Rs PutScalingPolicyResponse)
        , testProperty "PutScheduledUpdateGroupActionResponse"        (prop :: Rs PutScheduledUpdateGroupActionResponse)
        , testProperty "ResumeProcessesResponse"                      (prop :: Rs ResumeProcessesResponse)
        , testProperty "SetDesiredCapacityResponse"                   (prop :: Rs SetDesiredCapacityResponse)
        , testProperty "SetInstanceHealthResponse"                    (prop :: Rs SetInstanceHealthResponse)
        , testProperty "SuspendProcessesResponse"                     (prop :: Rs SuspendProcessesResponse)
        , testProperty "TerminateInstanceInAutoScalingGroupResponse"  (prop :: Rs TerminateInstanceInAutoScalingGroupResponse)
        , testProperty "UpdateAutoScalingGroupResponse"               (prop :: Rs UpdateAutoScalingGroupResponse)
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
