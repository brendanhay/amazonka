{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.AutoScaling
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
module Test.AWS.Gen.AutoScaling where

import Data.Proxy
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.Tasty
import Network.AWS.AutoScaling
import Test.AWS.AutoScaling.Internal

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ testDescribeMetricCollectionTypes $
--             describeMetricCollectionTypes
--
--         , testPutScalingPolicy $
--             putScalingPolicy
--
--         , testDeleteNotificationConfiguration $
--             deleteNotificationConfiguration
--
--         , testDescribeTags $
--             describeTags
--
--         , testDeleteLaunchConfiguration $
--             deleteLaunchConfiguration
--
--         , testDescribeLoadBalancers $
--             describeLoadBalancers
--
--         , testPutNotificationConfiguration $
--             putNotificationConfiguration
--
--         , testSetInstanceHealth $
--             setInstanceHealth
--
--         , testEnterStandby $
--             enterStandby
--
--         , testSuspendProcesses $
--             suspendProcesses
--
--         , testExitStandby $
--             exitStandby
--
--         , testDescribeTerminationPolicyTypes $
--             describeTerminationPolicyTypes
--
--         , testDescribeAutoScalingInstances $
--             describeAutoScalingInstances
--
--         , testDetachInstances $
--             detachInstances
--
--         , testDisableMetricsCollection $
--             disableMetricsCollection
--
--         , testRecordLifecycleActionHeartbeat $
--             recordLifecycleActionHeartbeat
--
--         , testDeleteTags $
--             deleteTags
--
--         , testDescribeScalingProcessTypes $
--             describeScalingProcessTypes
--
--         , testEnableMetricsCollection $
--             enableMetricsCollection
--
--         , testDescribeLifecycleHooks $
--             describeLifecycleHooks
--
--         , testDescribeAutoScalingGroups $
--             describeAutoScalingGroups
--
--         , testSetDesiredCapacity $
--             setDesiredCapacity
--
--         , testDetachLoadBalancers $
--             detachLoadBalancers
--
--         , testDeleteScheduledAction $
--             deleteScheduledAction
--
--         , testCreateOrUpdateTags $
--             createOrUpdateTags
--
--         , testDeletePolicy $
--             deletePolicy
--
--         , testDescribeAutoScalingNotificationTypes $
--             describeAutoScalingNotificationTypes
--
--         , testCompleteLifecycleAction $
--             completeLifecycleAction
--
--         , testAttachInstances $
--             attachInstances
--
--         , testDescribeScheduledActions $
--             describeScheduledActions
--
--         , testDeleteAutoScalingGroup $
--             deleteAutoScalingGroup
--
--         , testPutLifecycleHook $
--             putLifecycleHook
--
--         , testUpdateAutoScalingGroup $
--             updateAutoScalingGroup
--
--         , testDeleteLifecycleHook $
--             deleteLifecycleHook
--
--         , testResumeProcesses $
--             resumeProcesses
--
--         , testExecutePolicy $
--             executePolicy
--
--         , testTerminateInstanceInAutoScalingGroup $
--             terminateInstanceInAutoScalingGroup
--
--         , testDescribeAccountLimits $
--             describeAccountLimits
--
--         , testAttachLoadBalancers $
--             attachLoadBalancers
--
--         , testPutScheduledUpdateGroupAction $
--             putScheduledUpdateGroupAction
--
--         , testDescribePolicies $
--             describePolicies
--
--         , testDescribeNotificationConfigurations $
--             describeNotificationConfigurations
--
--         , testDescribeLaunchConfigurations $
--             describeLaunchConfigurations
--
--         , testDescribeLifecycleHookTypes $
--             describeLifecycleHookTypes
--
--         , testDescribeScalingActivities $
--             describeScalingActivities
--
--         , testCreateAutoScalingGroup $
--             createAutoScalingGroup
--
--         , testCreateLaunchConfiguration $
--             createLaunchConfiguration
--
--         , testDescribeAdjustmentTypes $
--             describeAdjustmentTypes
--
--           ]

--     , testGroup "response"
--         [ testDescribeMetricCollectionTypesResponse $
--             describeMetricCollectionTypesResponse
--
--         , testPutScalingPolicyResponse $
--             putScalingPolicyResponse
--
--         , testDeleteNotificationConfigurationResponse $
--             deleteNotificationConfigurationResponse
--
--         , testDescribeTagsResponse $
--             describeTagsResponse
--
--         , testDeleteLaunchConfigurationResponse $
--             deleteLaunchConfigurationResponse
--
--         , testDescribeLoadBalancersResponse $
--             describeLoadBalancersResponse
--
--         , testPutNotificationConfigurationResponse $
--             putNotificationConfigurationResponse
--
--         , testSetInstanceHealthResponse $
--             setInstanceHealthResponse
--
--         , testEnterStandbyResponse $
--             enterStandbyResponse
--
--         , testSuspendProcessesResponse $
--             suspendProcessesResponse
--
--         , testExitStandbyResponse $
--             exitStandbyResponse
--
--         , testDescribeTerminationPolicyTypesResponse $
--             describeTerminationPolicyTypesResponse
--
--         , testDescribeAutoScalingInstancesResponse $
--             describeAutoScalingInstancesResponse
--
--         , testDetachInstancesResponse $
--             detachInstancesResponse
--
--         , testDisableMetricsCollectionResponse $
--             disableMetricsCollectionResponse
--
--         , testRecordLifecycleActionHeartbeatResponse $
--             recordLifecycleActionHeartbeatResponse
--
--         , testDeleteTagsResponse $
--             deleteTagsResponse
--
--         , testDescribeScalingProcessTypesResponse $
--             describeScalingProcessTypesResponse
--
--         , testEnableMetricsCollectionResponse $
--             enableMetricsCollectionResponse
--
--         , testDescribeLifecycleHooksResponse $
--             describeLifecycleHooksResponse
--
--         , testDescribeAutoScalingGroupsResponse $
--             describeAutoScalingGroupsResponse
--
--         , testSetDesiredCapacityResponse $
--             setDesiredCapacityResponse
--
--         , testDetachLoadBalancersResponse $
--             detachLoadBalancersResponse
--
--         , testDeleteScheduledActionResponse $
--             deleteScheduledActionResponse
--
--         , testCreateOrUpdateTagsResponse $
--             createOrUpdateTagsResponse
--
--         , testDeletePolicyResponse $
--             deletePolicyResponse
--
--         , testDescribeAutoScalingNotificationTypesResponse $
--             describeAutoScalingNotificationTypesResponse
--
--         , testCompleteLifecycleActionResponse $
--             completeLifecycleActionResponse
--
--         , testAttachInstancesResponse $
--             attachInstancesResponse
--
--         , testDescribeScheduledActionsResponse $
--             describeScheduledActionsResponse
--
--         , testDeleteAutoScalingGroupResponse $
--             deleteAutoScalingGroupResponse
--
--         , testPutLifecycleHookResponse $
--             putLifecycleHookResponse
--
--         , testUpdateAutoScalingGroupResponse $
--             updateAutoScalingGroupResponse
--
--         , testDeleteLifecycleHookResponse $
--             deleteLifecycleHookResponse
--
--         , testResumeProcessesResponse $
--             resumeProcessesResponse
--
--         , testExecutePolicyResponse $
--             executePolicyResponse
--
--         , testTerminateInstanceInAutoScalingGroupResponse $
--             terminateInstanceInAutoScalingGroupResponse
--
--         , testDescribeAccountLimitsResponse $
--             describeAccountLimitsResponse
--
--         , testAttachLoadBalancersResponse $
--             attachLoadBalancersResponse
--
--         , testPutScheduledUpdateGroupActionResponse $
--             putScheduledUpdateGroupActionResponse
--
--         , testDescribePoliciesResponse $
--             describePoliciesResponse
--
--         , testDescribeNotificationConfigurationsResponse $
--             describeNotificationConfigurationsResponse
--
--         , testDescribeLaunchConfigurationsResponse $
--             describeLaunchConfigurationsResponse
--
--         , testDescribeLifecycleHookTypesResponse $
--             describeLifecycleHookTypesResponse
--
--         , testDescribeScalingActivitiesResponse $
--             describeScalingActivitiesResponse
--
--         , testCreateAutoScalingGroupResponse $
--             createAutoScalingGroupResponse
--
--         , testCreateLaunchConfigurationResponse $
--             createLaunchConfigurationResponse
--
--         , testDescribeAdjustmentTypesResponse $
--             describeAdjustmentTypesResponse
--
--           ]
--     ]

-- Requests

testDescribeMetricCollectionTypes :: DescribeMetricCollectionTypes -> TestTree
testDescribeMetricCollectionTypes = req
    "DescribeMetricCollectionTypes"
    "fixture/DescribeMetricCollectionTypes"

testPutScalingPolicy :: PutScalingPolicy -> TestTree
testPutScalingPolicy = req
    "PutScalingPolicy"
    "fixture/PutScalingPolicy"

testDeleteNotificationConfiguration :: DeleteNotificationConfiguration -> TestTree
testDeleteNotificationConfiguration = req
    "DeleteNotificationConfiguration"
    "fixture/DeleteNotificationConfiguration"

testDescribeTags :: DescribeTags -> TestTree
testDescribeTags = req
    "DescribeTags"
    "fixture/DescribeTags"

testDeleteLaunchConfiguration :: DeleteLaunchConfiguration -> TestTree
testDeleteLaunchConfiguration = req
    "DeleteLaunchConfiguration"
    "fixture/DeleteLaunchConfiguration"

testDescribeLoadBalancers :: DescribeLoadBalancers -> TestTree
testDescribeLoadBalancers = req
    "DescribeLoadBalancers"
    "fixture/DescribeLoadBalancers"

testPutNotificationConfiguration :: PutNotificationConfiguration -> TestTree
testPutNotificationConfiguration = req
    "PutNotificationConfiguration"
    "fixture/PutNotificationConfiguration"

testSetInstanceHealth :: SetInstanceHealth -> TestTree
testSetInstanceHealth = req
    "SetInstanceHealth"
    "fixture/SetInstanceHealth"

testEnterStandby :: EnterStandby -> TestTree
testEnterStandby = req
    "EnterStandby"
    "fixture/EnterStandby"

testSuspendProcesses :: SuspendProcesses -> TestTree
testSuspendProcesses = req
    "SuspendProcesses"
    "fixture/SuspendProcesses"

testExitStandby :: ExitStandby -> TestTree
testExitStandby = req
    "ExitStandby"
    "fixture/ExitStandby"

testDescribeTerminationPolicyTypes :: DescribeTerminationPolicyTypes -> TestTree
testDescribeTerminationPolicyTypes = req
    "DescribeTerminationPolicyTypes"
    "fixture/DescribeTerminationPolicyTypes"

testDescribeAutoScalingInstances :: DescribeAutoScalingInstances -> TestTree
testDescribeAutoScalingInstances = req
    "DescribeAutoScalingInstances"
    "fixture/DescribeAutoScalingInstances"

testDetachInstances :: DetachInstances -> TestTree
testDetachInstances = req
    "DetachInstances"
    "fixture/DetachInstances"

testDisableMetricsCollection :: DisableMetricsCollection -> TestTree
testDisableMetricsCollection = req
    "DisableMetricsCollection"
    "fixture/DisableMetricsCollection"

testRecordLifecycleActionHeartbeat :: RecordLifecycleActionHeartbeat -> TestTree
testRecordLifecycleActionHeartbeat = req
    "RecordLifecycleActionHeartbeat"
    "fixture/RecordLifecycleActionHeartbeat"

testDeleteTags :: DeleteTags -> TestTree
testDeleteTags = req
    "DeleteTags"
    "fixture/DeleteTags"

testDescribeScalingProcessTypes :: DescribeScalingProcessTypes -> TestTree
testDescribeScalingProcessTypes = req
    "DescribeScalingProcessTypes"
    "fixture/DescribeScalingProcessTypes"

testEnableMetricsCollection :: EnableMetricsCollection -> TestTree
testEnableMetricsCollection = req
    "EnableMetricsCollection"
    "fixture/EnableMetricsCollection"

testDescribeLifecycleHooks :: DescribeLifecycleHooks -> TestTree
testDescribeLifecycleHooks = req
    "DescribeLifecycleHooks"
    "fixture/DescribeLifecycleHooks"

testDescribeAutoScalingGroups :: DescribeAutoScalingGroups -> TestTree
testDescribeAutoScalingGroups = req
    "DescribeAutoScalingGroups"
    "fixture/DescribeAutoScalingGroups"

testSetDesiredCapacity :: SetDesiredCapacity -> TestTree
testSetDesiredCapacity = req
    "SetDesiredCapacity"
    "fixture/SetDesiredCapacity"

testDetachLoadBalancers :: DetachLoadBalancers -> TestTree
testDetachLoadBalancers = req
    "DetachLoadBalancers"
    "fixture/DetachLoadBalancers"

testDeleteScheduledAction :: DeleteScheduledAction -> TestTree
testDeleteScheduledAction = req
    "DeleteScheduledAction"
    "fixture/DeleteScheduledAction"

testCreateOrUpdateTags :: CreateOrUpdateTags -> TestTree
testCreateOrUpdateTags = req
    "CreateOrUpdateTags"
    "fixture/CreateOrUpdateTags"

testDeletePolicy :: DeletePolicy -> TestTree
testDeletePolicy = req
    "DeletePolicy"
    "fixture/DeletePolicy"

testDescribeAutoScalingNotificationTypes :: DescribeAutoScalingNotificationTypes -> TestTree
testDescribeAutoScalingNotificationTypes = req
    "DescribeAutoScalingNotificationTypes"
    "fixture/DescribeAutoScalingNotificationTypes"

testCompleteLifecycleAction :: CompleteLifecycleAction -> TestTree
testCompleteLifecycleAction = req
    "CompleteLifecycleAction"
    "fixture/CompleteLifecycleAction"

testAttachInstances :: AttachInstances -> TestTree
testAttachInstances = req
    "AttachInstances"
    "fixture/AttachInstances"

testDescribeScheduledActions :: DescribeScheduledActions -> TestTree
testDescribeScheduledActions = req
    "DescribeScheduledActions"
    "fixture/DescribeScheduledActions"

testDeleteAutoScalingGroup :: DeleteAutoScalingGroup -> TestTree
testDeleteAutoScalingGroup = req
    "DeleteAutoScalingGroup"
    "fixture/DeleteAutoScalingGroup"

testPutLifecycleHook :: PutLifecycleHook -> TestTree
testPutLifecycleHook = req
    "PutLifecycleHook"
    "fixture/PutLifecycleHook"

testUpdateAutoScalingGroup :: UpdateAutoScalingGroup -> TestTree
testUpdateAutoScalingGroup = req
    "UpdateAutoScalingGroup"
    "fixture/UpdateAutoScalingGroup"

testDeleteLifecycleHook :: DeleteLifecycleHook -> TestTree
testDeleteLifecycleHook = req
    "DeleteLifecycleHook"
    "fixture/DeleteLifecycleHook"

testResumeProcesses :: ResumeProcesses -> TestTree
testResumeProcesses = req
    "ResumeProcesses"
    "fixture/ResumeProcesses"

testExecutePolicy :: ExecutePolicy -> TestTree
testExecutePolicy = req
    "ExecutePolicy"
    "fixture/ExecutePolicy"

testTerminateInstanceInAutoScalingGroup :: TerminateInstanceInAutoScalingGroup -> TestTree
testTerminateInstanceInAutoScalingGroup = req
    "TerminateInstanceInAutoScalingGroup"
    "fixture/TerminateInstanceInAutoScalingGroup"

testDescribeAccountLimits :: DescribeAccountLimits -> TestTree
testDescribeAccountLimits = req
    "DescribeAccountLimits"
    "fixture/DescribeAccountLimits"

testAttachLoadBalancers :: AttachLoadBalancers -> TestTree
testAttachLoadBalancers = req
    "AttachLoadBalancers"
    "fixture/AttachLoadBalancers"

testPutScheduledUpdateGroupAction :: PutScheduledUpdateGroupAction -> TestTree
testPutScheduledUpdateGroupAction = req
    "PutScheduledUpdateGroupAction"
    "fixture/PutScheduledUpdateGroupAction"

testDescribePolicies :: DescribePolicies -> TestTree
testDescribePolicies = req
    "DescribePolicies"
    "fixture/DescribePolicies"

testDescribeNotificationConfigurations :: DescribeNotificationConfigurations -> TestTree
testDescribeNotificationConfigurations = req
    "DescribeNotificationConfigurations"
    "fixture/DescribeNotificationConfigurations"

testDescribeLaunchConfigurations :: DescribeLaunchConfigurations -> TestTree
testDescribeLaunchConfigurations = req
    "DescribeLaunchConfigurations"
    "fixture/DescribeLaunchConfigurations"

testDescribeLifecycleHookTypes :: DescribeLifecycleHookTypes -> TestTree
testDescribeLifecycleHookTypes = req
    "DescribeLifecycleHookTypes"
    "fixture/DescribeLifecycleHookTypes"

testDescribeScalingActivities :: DescribeScalingActivities -> TestTree
testDescribeScalingActivities = req
    "DescribeScalingActivities"
    "fixture/DescribeScalingActivities"

testCreateAutoScalingGroup :: CreateAutoScalingGroup -> TestTree
testCreateAutoScalingGroup = req
    "CreateAutoScalingGroup"
    "fixture/CreateAutoScalingGroup"

testCreateLaunchConfiguration :: CreateLaunchConfiguration -> TestTree
testCreateLaunchConfiguration = req
    "CreateLaunchConfiguration"
    "fixture/CreateLaunchConfiguration"

testDescribeAdjustmentTypes :: DescribeAdjustmentTypes -> TestTree
testDescribeAdjustmentTypes = req
    "DescribeAdjustmentTypes"
    "fixture/DescribeAdjustmentTypes"

-- Responses

testDescribeMetricCollectionTypesResponse :: DescribeMetricCollectionTypesResponse -> TestTree
testDescribeMetricCollectionTypesResponse = res
    "DescribeMetricCollectionTypesResponse"
    "fixture/DescribeMetricCollectionTypesResponse"
    (Proxy :: Proxy DescribeMetricCollectionTypes)

testPutScalingPolicyResponse :: PutScalingPolicyResponse -> TestTree
testPutScalingPolicyResponse = res
    "PutScalingPolicyResponse"
    "fixture/PutScalingPolicyResponse"
    (Proxy :: Proxy PutScalingPolicy)

testDeleteNotificationConfigurationResponse :: DeleteNotificationConfigurationResponse -> TestTree
testDeleteNotificationConfigurationResponse = res
    "DeleteNotificationConfigurationResponse"
    "fixture/DeleteNotificationConfigurationResponse"
    (Proxy :: Proxy DeleteNotificationConfiguration)

testDescribeTagsResponse :: DescribeTagsResponse -> TestTree
testDescribeTagsResponse = res
    "DescribeTagsResponse"
    "fixture/DescribeTagsResponse"
    (Proxy :: Proxy DescribeTags)

testDeleteLaunchConfigurationResponse :: DeleteLaunchConfigurationResponse -> TestTree
testDeleteLaunchConfigurationResponse = res
    "DeleteLaunchConfigurationResponse"
    "fixture/DeleteLaunchConfigurationResponse"
    (Proxy :: Proxy DeleteLaunchConfiguration)

testDescribeLoadBalancersResponse :: DescribeLoadBalancersResponse -> TestTree
testDescribeLoadBalancersResponse = res
    "DescribeLoadBalancersResponse"
    "fixture/DescribeLoadBalancersResponse"
    (Proxy :: Proxy DescribeLoadBalancers)

testPutNotificationConfigurationResponse :: PutNotificationConfigurationResponse -> TestTree
testPutNotificationConfigurationResponse = res
    "PutNotificationConfigurationResponse"
    "fixture/PutNotificationConfigurationResponse"
    (Proxy :: Proxy PutNotificationConfiguration)

testSetInstanceHealthResponse :: SetInstanceHealthResponse -> TestTree
testSetInstanceHealthResponse = res
    "SetInstanceHealthResponse"
    "fixture/SetInstanceHealthResponse"
    (Proxy :: Proxy SetInstanceHealth)

testEnterStandbyResponse :: EnterStandbyResponse -> TestTree
testEnterStandbyResponse = res
    "EnterStandbyResponse"
    "fixture/EnterStandbyResponse"
    (Proxy :: Proxy EnterStandby)

testSuspendProcessesResponse :: SuspendProcessesResponse -> TestTree
testSuspendProcessesResponse = res
    "SuspendProcessesResponse"
    "fixture/SuspendProcessesResponse"
    (Proxy :: Proxy SuspendProcesses)

testExitStandbyResponse :: ExitStandbyResponse -> TestTree
testExitStandbyResponse = res
    "ExitStandbyResponse"
    "fixture/ExitStandbyResponse"
    (Proxy :: Proxy ExitStandby)

testDescribeTerminationPolicyTypesResponse :: DescribeTerminationPolicyTypesResponse -> TestTree
testDescribeTerminationPolicyTypesResponse = res
    "DescribeTerminationPolicyTypesResponse"
    "fixture/DescribeTerminationPolicyTypesResponse"
    (Proxy :: Proxy DescribeTerminationPolicyTypes)

testDescribeAutoScalingInstancesResponse :: DescribeAutoScalingInstancesResponse -> TestTree
testDescribeAutoScalingInstancesResponse = res
    "DescribeAutoScalingInstancesResponse"
    "fixture/DescribeAutoScalingInstancesResponse"
    (Proxy :: Proxy DescribeAutoScalingInstances)

testDetachInstancesResponse :: DetachInstancesResponse -> TestTree
testDetachInstancesResponse = res
    "DetachInstancesResponse"
    "fixture/DetachInstancesResponse"
    (Proxy :: Proxy DetachInstances)

testDisableMetricsCollectionResponse :: DisableMetricsCollectionResponse -> TestTree
testDisableMetricsCollectionResponse = res
    "DisableMetricsCollectionResponse"
    "fixture/DisableMetricsCollectionResponse"
    (Proxy :: Proxy DisableMetricsCollection)

testRecordLifecycleActionHeartbeatResponse :: RecordLifecycleActionHeartbeatResponse -> TestTree
testRecordLifecycleActionHeartbeatResponse = res
    "RecordLifecycleActionHeartbeatResponse"
    "fixture/RecordLifecycleActionHeartbeatResponse"
    (Proxy :: Proxy RecordLifecycleActionHeartbeat)

testDeleteTagsResponse :: DeleteTagsResponse -> TestTree
testDeleteTagsResponse = res
    "DeleteTagsResponse"
    "fixture/DeleteTagsResponse"
    (Proxy :: Proxy DeleteTags)

testDescribeScalingProcessTypesResponse :: DescribeScalingProcessTypesResponse -> TestTree
testDescribeScalingProcessTypesResponse = res
    "DescribeScalingProcessTypesResponse"
    "fixture/DescribeScalingProcessTypesResponse"
    (Proxy :: Proxy DescribeScalingProcessTypes)

testEnableMetricsCollectionResponse :: EnableMetricsCollectionResponse -> TestTree
testEnableMetricsCollectionResponse = res
    "EnableMetricsCollectionResponse"
    "fixture/EnableMetricsCollectionResponse"
    (Proxy :: Proxy EnableMetricsCollection)

testDescribeLifecycleHooksResponse :: DescribeLifecycleHooksResponse -> TestTree
testDescribeLifecycleHooksResponse = res
    "DescribeLifecycleHooksResponse"
    "fixture/DescribeLifecycleHooksResponse"
    (Proxy :: Proxy DescribeLifecycleHooks)

testDescribeAutoScalingGroupsResponse :: DescribeAutoScalingGroupsResponse -> TestTree
testDescribeAutoScalingGroupsResponse = res
    "DescribeAutoScalingGroupsResponse"
    "fixture/DescribeAutoScalingGroupsResponse"
    (Proxy :: Proxy DescribeAutoScalingGroups)

testSetDesiredCapacityResponse :: SetDesiredCapacityResponse -> TestTree
testSetDesiredCapacityResponse = res
    "SetDesiredCapacityResponse"
    "fixture/SetDesiredCapacityResponse"
    (Proxy :: Proxy SetDesiredCapacity)

testDetachLoadBalancersResponse :: DetachLoadBalancersResponse -> TestTree
testDetachLoadBalancersResponse = res
    "DetachLoadBalancersResponse"
    "fixture/DetachLoadBalancersResponse"
    (Proxy :: Proxy DetachLoadBalancers)

testDeleteScheduledActionResponse :: DeleteScheduledActionResponse -> TestTree
testDeleteScheduledActionResponse = res
    "DeleteScheduledActionResponse"
    "fixture/DeleteScheduledActionResponse"
    (Proxy :: Proxy DeleteScheduledAction)

testCreateOrUpdateTagsResponse :: CreateOrUpdateTagsResponse -> TestTree
testCreateOrUpdateTagsResponse = res
    "CreateOrUpdateTagsResponse"
    "fixture/CreateOrUpdateTagsResponse"
    (Proxy :: Proxy CreateOrUpdateTags)

testDeletePolicyResponse :: DeletePolicyResponse -> TestTree
testDeletePolicyResponse = res
    "DeletePolicyResponse"
    "fixture/DeletePolicyResponse"
    (Proxy :: Proxy DeletePolicy)

testDescribeAutoScalingNotificationTypesResponse :: DescribeAutoScalingNotificationTypesResponse -> TestTree
testDescribeAutoScalingNotificationTypesResponse = res
    "DescribeAutoScalingNotificationTypesResponse"
    "fixture/DescribeAutoScalingNotificationTypesResponse"
    (Proxy :: Proxy DescribeAutoScalingNotificationTypes)

testCompleteLifecycleActionResponse :: CompleteLifecycleActionResponse -> TestTree
testCompleteLifecycleActionResponse = res
    "CompleteLifecycleActionResponse"
    "fixture/CompleteLifecycleActionResponse"
    (Proxy :: Proxy CompleteLifecycleAction)

testAttachInstancesResponse :: AttachInstancesResponse -> TestTree
testAttachInstancesResponse = res
    "AttachInstancesResponse"
    "fixture/AttachInstancesResponse"
    (Proxy :: Proxy AttachInstances)

testDescribeScheduledActionsResponse :: DescribeScheduledActionsResponse -> TestTree
testDescribeScheduledActionsResponse = res
    "DescribeScheduledActionsResponse"
    "fixture/DescribeScheduledActionsResponse"
    (Proxy :: Proxy DescribeScheduledActions)

testDeleteAutoScalingGroupResponse :: DeleteAutoScalingGroupResponse -> TestTree
testDeleteAutoScalingGroupResponse = res
    "DeleteAutoScalingGroupResponse"
    "fixture/DeleteAutoScalingGroupResponse"
    (Proxy :: Proxy DeleteAutoScalingGroup)

testPutLifecycleHookResponse :: PutLifecycleHookResponse -> TestTree
testPutLifecycleHookResponse = res
    "PutLifecycleHookResponse"
    "fixture/PutLifecycleHookResponse"
    (Proxy :: Proxy PutLifecycleHook)

testUpdateAutoScalingGroupResponse :: UpdateAutoScalingGroupResponse -> TestTree
testUpdateAutoScalingGroupResponse = res
    "UpdateAutoScalingGroupResponse"
    "fixture/UpdateAutoScalingGroupResponse"
    (Proxy :: Proxy UpdateAutoScalingGroup)

testDeleteLifecycleHookResponse :: DeleteLifecycleHookResponse -> TestTree
testDeleteLifecycleHookResponse = res
    "DeleteLifecycleHookResponse"
    "fixture/DeleteLifecycleHookResponse"
    (Proxy :: Proxy DeleteLifecycleHook)

testResumeProcessesResponse :: ResumeProcessesResponse -> TestTree
testResumeProcessesResponse = res
    "ResumeProcessesResponse"
    "fixture/ResumeProcessesResponse"
    (Proxy :: Proxy ResumeProcesses)

testExecutePolicyResponse :: ExecutePolicyResponse -> TestTree
testExecutePolicyResponse = res
    "ExecutePolicyResponse"
    "fixture/ExecutePolicyResponse"
    (Proxy :: Proxy ExecutePolicy)

testTerminateInstanceInAutoScalingGroupResponse :: TerminateInstanceInAutoScalingGroupResponse -> TestTree
testTerminateInstanceInAutoScalingGroupResponse = res
    "TerminateInstanceInAutoScalingGroupResponse"
    "fixture/TerminateInstanceInAutoScalingGroupResponse"
    (Proxy :: Proxy TerminateInstanceInAutoScalingGroup)

testDescribeAccountLimitsResponse :: DescribeAccountLimitsResponse -> TestTree
testDescribeAccountLimitsResponse = res
    "DescribeAccountLimitsResponse"
    "fixture/DescribeAccountLimitsResponse"
    (Proxy :: Proxy DescribeAccountLimits)

testAttachLoadBalancersResponse :: AttachLoadBalancersResponse -> TestTree
testAttachLoadBalancersResponse = res
    "AttachLoadBalancersResponse"
    "fixture/AttachLoadBalancersResponse"
    (Proxy :: Proxy AttachLoadBalancers)

testPutScheduledUpdateGroupActionResponse :: PutScheduledUpdateGroupActionResponse -> TestTree
testPutScheduledUpdateGroupActionResponse = res
    "PutScheduledUpdateGroupActionResponse"
    "fixture/PutScheduledUpdateGroupActionResponse"
    (Proxy :: Proxy PutScheduledUpdateGroupAction)

testDescribePoliciesResponse :: DescribePoliciesResponse -> TestTree
testDescribePoliciesResponse = res
    "DescribePoliciesResponse"
    "fixture/DescribePoliciesResponse"
    (Proxy :: Proxy DescribePolicies)

testDescribeNotificationConfigurationsResponse :: DescribeNotificationConfigurationsResponse -> TestTree
testDescribeNotificationConfigurationsResponse = res
    "DescribeNotificationConfigurationsResponse"
    "fixture/DescribeNotificationConfigurationsResponse"
    (Proxy :: Proxy DescribeNotificationConfigurations)

testDescribeLaunchConfigurationsResponse :: DescribeLaunchConfigurationsResponse -> TestTree
testDescribeLaunchConfigurationsResponse = res
    "DescribeLaunchConfigurationsResponse"
    "fixture/DescribeLaunchConfigurationsResponse"
    (Proxy :: Proxy DescribeLaunchConfigurations)

testDescribeLifecycleHookTypesResponse :: DescribeLifecycleHookTypesResponse -> TestTree
testDescribeLifecycleHookTypesResponse = res
    "DescribeLifecycleHookTypesResponse"
    "fixture/DescribeLifecycleHookTypesResponse"
    (Proxy :: Proxy DescribeLifecycleHookTypes)

testDescribeScalingActivitiesResponse :: DescribeScalingActivitiesResponse -> TestTree
testDescribeScalingActivitiesResponse = res
    "DescribeScalingActivitiesResponse"
    "fixture/DescribeScalingActivitiesResponse"
    (Proxy :: Proxy DescribeScalingActivities)

testCreateAutoScalingGroupResponse :: CreateAutoScalingGroupResponse -> TestTree
testCreateAutoScalingGroupResponse = res
    "CreateAutoScalingGroupResponse"
    "fixture/CreateAutoScalingGroupResponse"
    (Proxy :: Proxy CreateAutoScalingGroup)

testCreateLaunchConfigurationResponse :: CreateLaunchConfigurationResponse -> TestTree
testCreateLaunchConfigurationResponse = res
    "CreateLaunchConfigurationResponse"
    "fixture/CreateLaunchConfigurationResponse"
    (Proxy :: Proxy CreateLaunchConfiguration)

testDescribeAdjustmentTypesResponse :: DescribeAdjustmentTypesResponse -> TestTree
testDescribeAdjustmentTypesResponse = res
    "DescribeAdjustmentTypesResponse"
    "fixture/DescribeAdjustmentTypesResponse"
    (Proxy :: Proxy DescribeAdjustmentTypes)
