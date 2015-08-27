{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.AutoScaling
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
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
    autoScaling
    (Proxy :: Proxy DescribeMetricCollectionTypes)

testPutScalingPolicyResponse :: PutScalingPolicyResponse -> TestTree
testPutScalingPolicyResponse = res
    "PutScalingPolicyResponse"
    "fixture/PutScalingPolicyResponse"
    autoScaling
    (Proxy :: Proxy PutScalingPolicy)

testDeleteNotificationConfigurationResponse :: DeleteNotificationConfigurationResponse -> TestTree
testDeleteNotificationConfigurationResponse = res
    "DeleteNotificationConfigurationResponse"
    "fixture/DeleteNotificationConfigurationResponse"
    autoScaling
    (Proxy :: Proxy DeleteNotificationConfiguration)

testDescribeTagsResponse :: DescribeTagsResponse -> TestTree
testDescribeTagsResponse = res
    "DescribeTagsResponse"
    "fixture/DescribeTagsResponse"
    autoScaling
    (Proxy :: Proxy DescribeTags)

testDeleteLaunchConfigurationResponse :: DeleteLaunchConfigurationResponse -> TestTree
testDeleteLaunchConfigurationResponse = res
    "DeleteLaunchConfigurationResponse"
    "fixture/DeleteLaunchConfigurationResponse"
    autoScaling
    (Proxy :: Proxy DeleteLaunchConfiguration)

testDescribeLoadBalancersResponse :: DescribeLoadBalancersResponse -> TestTree
testDescribeLoadBalancersResponse = res
    "DescribeLoadBalancersResponse"
    "fixture/DescribeLoadBalancersResponse"
    autoScaling
    (Proxy :: Proxy DescribeLoadBalancers)

testPutNotificationConfigurationResponse :: PutNotificationConfigurationResponse -> TestTree
testPutNotificationConfigurationResponse = res
    "PutNotificationConfigurationResponse"
    "fixture/PutNotificationConfigurationResponse"
    autoScaling
    (Proxy :: Proxy PutNotificationConfiguration)

testSetInstanceHealthResponse :: SetInstanceHealthResponse -> TestTree
testSetInstanceHealthResponse = res
    "SetInstanceHealthResponse"
    "fixture/SetInstanceHealthResponse"
    autoScaling
    (Proxy :: Proxy SetInstanceHealth)

testEnterStandbyResponse :: EnterStandbyResponse -> TestTree
testEnterStandbyResponse = res
    "EnterStandbyResponse"
    "fixture/EnterStandbyResponse"
    autoScaling
    (Proxy :: Proxy EnterStandby)

testSuspendProcessesResponse :: SuspendProcessesResponse -> TestTree
testSuspendProcessesResponse = res
    "SuspendProcessesResponse"
    "fixture/SuspendProcessesResponse"
    autoScaling
    (Proxy :: Proxy SuspendProcesses)

testExitStandbyResponse :: ExitStandbyResponse -> TestTree
testExitStandbyResponse = res
    "ExitStandbyResponse"
    "fixture/ExitStandbyResponse"
    autoScaling
    (Proxy :: Proxy ExitStandby)

testDescribeTerminationPolicyTypesResponse :: DescribeTerminationPolicyTypesResponse -> TestTree
testDescribeTerminationPolicyTypesResponse = res
    "DescribeTerminationPolicyTypesResponse"
    "fixture/DescribeTerminationPolicyTypesResponse"
    autoScaling
    (Proxy :: Proxy DescribeTerminationPolicyTypes)

testDescribeAutoScalingInstancesResponse :: DescribeAutoScalingInstancesResponse -> TestTree
testDescribeAutoScalingInstancesResponse = res
    "DescribeAutoScalingInstancesResponse"
    "fixture/DescribeAutoScalingInstancesResponse"
    autoScaling
    (Proxy :: Proxy DescribeAutoScalingInstances)

testDetachInstancesResponse :: DetachInstancesResponse -> TestTree
testDetachInstancesResponse = res
    "DetachInstancesResponse"
    "fixture/DetachInstancesResponse"
    autoScaling
    (Proxy :: Proxy DetachInstances)

testDisableMetricsCollectionResponse :: DisableMetricsCollectionResponse -> TestTree
testDisableMetricsCollectionResponse = res
    "DisableMetricsCollectionResponse"
    "fixture/DisableMetricsCollectionResponse"
    autoScaling
    (Proxy :: Proxy DisableMetricsCollection)

testRecordLifecycleActionHeartbeatResponse :: RecordLifecycleActionHeartbeatResponse -> TestTree
testRecordLifecycleActionHeartbeatResponse = res
    "RecordLifecycleActionHeartbeatResponse"
    "fixture/RecordLifecycleActionHeartbeatResponse"
    autoScaling
    (Proxy :: Proxy RecordLifecycleActionHeartbeat)

testDeleteTagsResponse :: DeleteTagsResponse -> TestTree
testDeleteTagsResponse = res
    "DeleteTagsResponse"
    "fixture/DeleteTagsResponse"
    autoScaling
    (Proxy :: Proxy DeleteTags)

testDescribeScalingProcessTypesResponse :: DescribeScalingProcessTypesResponse -> TestTree
testDescribeScalingProcessTypesResponse = res
    "DescribeScalingProcessTypesResponse"
    "fixture/DescribeScalingProcessTypesResponse"
    autoScaling
    (Proxy :: Proxy DescribeScalingProcessTypes)

testEnableMetricsCollectionResponse :: EnableMetricsCollectionResponse -> TestTree
testEnableMetricsCollectionResponse = res
    "EnableMetricsCollectionResponse"
    "fixture/EnableMetricsCollectionResponse"
    autoScaling
    (Proxy :: Proxy EnableMetricsCollection)

testDescribeLifecycleHooksResponse :: DescribeLifecycleHooksResponse -> TestTree
testDescribeLifecycleHooksResponse = res
    "DescribeLifecycleHooksResponse"
    "fixture/DescribeLifecycleHooksResponse"
    autoScaling
    (Proxy :: Proxy DescribeLifecycleHooks)

testDescribeAutoScalingGroupsResponse :: DescribeAutoScalingGroupsResponse -> TestTree
testDescribeAutoScalingGroupsResponse = res
    "DescribeAutoScalingGroupsResponse"
    "fixture/DescribeAutoScalingGroupsResponse"
    autoScaling
    (Proxy :: Proxy DescribeAutoScalingGroups)

testSetDesiredCapacityResponse :: SetDesiredCapacityResponse -> TestTree
testSetDesiredCapacityResponse = res
    "SetDesiredCapacityResponse"
    "fixture/SetDesiredCapacityResponse"
    autoScaling
    (Proxy :: Proxy SetDesiredCapacity)

testDetachLoadBalancersResponse :: DetachLoadBalancersResponse -> TestTree
testDetachLoadBalancersResponse = res
    "DetachLoadBalancersResponse"
    "fixture/DetachLoadBalancersResponse"
    autoScaling
    (Proxy :: Proxy DetachLoadBalancers)

testDeleteScheduledActionResponse :: DeleteScheduledActionResponse -> TestTree
testDeleteScheduledActionResponse = res
    "DeleteScheduledActionResponse"
    "fixture/DeleteScheduledActionResponse"
    autoScaling
    (Proxy :: Proxy DeleteScheduledAction)

testCreateOrUpdateTagsResponse :: CreateOrUpdateTagsResponse -> TestTree
testCreateOrUpdateTagsResponse = res
    "CreateOrUpdateTagsResponse"
    "fixture/CreateOrUpdateTagsResponse"
    autoScaling
    (Proxy :: Proxy CreateOrUpdateTags)

testDeletePolicyResponse :: DeletePolicyResponse -> TestTree
testDeletePolicyResponse = res
    "DeletePolicyResponse"
    "fixture/DeletePolicyResponse"
    autoScaling
    (Proxy :: Proxy DeletePolicy)

testDescribeAutoScalingNotificationTypesResponse :: DescribeAutoScalingNotificationTypesResponse -> TestTree
testDescribeAutoScalingNotificationTypesResponse = res
    "DescribeAutoScalingNotificationTypesResponse"
    "fixture/DescribeAutoScalingNotificationTypesResponse"
    autoScaling
    (Proxy :: Proxy DescribeAutoScalingNotificationTypes)

testCompleteLifecycleActionResponse :: CompleteLifecycleActionResponse -> TestTree
testCompleteLifecycleActionResponse = res
    "CompleteLifecycleActionResponse"
    "fixture/CompleteLifecycleActionResponse"
    autoScaling
    (Proxy :: Proxy CompleteLifecycleAction)

testAttachInstancesResponse :: AttachInstancesResponse -> TestTree
testAttachInstancesResponse = res
    "AttachInstancesResponse"
    "fixture/AttachInstancesResponse"
    autoScaling
    (Proxy :: Proxy AttachInstances)

testDescribeScheduledActionsResponse :: DescribeScheduledActionsResponse -> TestTree
testDescribeScheduledActionsResponse = res
    "DescribeScheduledActionsResponse"
    "fixture/DescribeScheduledActionsResponse"
    autoScaling
    (Proxy :: Proxy DescribeScheduledActions)

testDeleteAutoScalingGroupResponse :: DeleteAutoScalingGroupResponse -> TestTree
testDeleteAutoScalingGroupResponse = res
    "DeleteAutoScalingGroupResponse"
    "fixture/DeleteAutoScalingGroupResponse"
    autoScaling
    (Proxy :: Proxy DeleteAutoScalingGroup)

testPutLifecycleHookResponse :: PutLifecycleHookResponse -> TestTree
testPutLifecycleHookResponse = res
    "PutLifecycleHookResponse"
    "fixture/PutLifecycleHookResponse"
    autoScaling
    (Proxy :: Proxy PutLifecycleHook)

testUpdateAutoScalingGroupResponse :: UpdateAutoScalingGroupResponse -> TestTree
testUpdateAutoScalingGroupResponse = res
    "UpdateAutoScalingGroupResponse"
    "fixture/UpdateAutoScalingGroupResponse"
    autoScaling
    (Proxy :: Proxy UpdateAutoScalingGroup)

testDeleteLifecycleHookResponse :: DeleteLifecycleHookResponse -> TestTree
testDeleteLifecycleHookResponse = res
    "DeleteLifecycleHookResponse"
    "fixture/DeleteLifecycleHookResponse"
    autoScaling
    (Proxy :: Proxy DeleteLifecycleHook)

testResumeProcessesResponse :: ResumeProcessesResponse -> TestTree
testResumeProcessesResponse = res
    "ResumeProcessesResponse"
    "fixture/ResumeProcessesResponse"
    autoScaling
    (Proxy :: Proxy ResumeProcesses)

testExecutePolicyResponse :: ExecutePolicyResponse -> TestTree
testExecutePolicyResponse = res
    "ExecutePolicyResponse"
    "fixture/ExecutePolicyResponse"
    autoScaling
    (Proxy :: Proxy ExecutePolicy)

testTerminateInstanceInAutoScalingGroupResponse :: TerminateInstanceInAutoScalingGroupResponse -> TestTree
testTerminateInstanceInAutoScalingGroupResponse = res
    "TerminateInstanceInAutoScalingGroupResponse"
    "fixture/TerminateInstanceInAutoScalingGroupResponse"
    autoScaling
    (Proxy :: Proxy TerminateInstanceInAutoScalingGroup)

testDescribeAccountLimitsResponse :: DescribeAccountLimitsResponse -> TestTree
testDescribeAccountLimitsResponse = res
    "DescribeAccountLimitsResponse"
    "fixture/DescribeAccountLimitsResponse"
    autoScaling
    (Proxy :: Proxy DescribeAccountLimits)

testAttachLoadBalancersResponse :: AttachLoadBalancersResponse -> TestTree
testAttachLoadBalancersResponse = res
    "AttachLoadBalancersResponse"
    "fixture/AttachLoadBalancersResponse"
    autoScaling
    (Proxy :: Proxy AttachLoadBalancers)

testPutScheduledUpdateGroupActionResponse :: PutScheduledUpdateGroupActionResponse -> TestTree
testPutScheduledUpdateGroupActionResponse = res
    "PutScheduledUpdateGroupActionResponse"
    "fixture/PutScheduledUpdateGroupActionResponse"
    autoScaling
    (Proxy :: Proxy PutScheduledUpdateGroupAction)

testDescribePoliciesResponse :: DescribePoliciesResponse -> TestTree
testDescribePoliciesResponse = res
    "DescribePoliciesResponse"
    "fixture/DescribePoliciesResponse"
    autoScaling
    (Proxy :: Proxy DescribePolicies)

testDescribeNotificationConfigurationsResponse :: DescribeNotificationConfigurationsResponse -> TestTree
testDescribeNotificationConfigurationsResponse = res
    "DescribeNotificationConfigurationsResponse"
    "fixture/DescribeNotificationConfigurationsResponse"
    autoScaling
    (Proxy :: Proxy DescribeNotificationConfigurations)

testDescribeLaunchConfigurationsResponse :: DescribeLaunchConfigurationsResponse -> TestTree
testDescribeLaunchConfigurationsResponse = res
    "DescribeLaunchConfigurationsResponse"
    "fixture/DescribeLaunchConfigurationsResponse"
    autoScaling
    (Proxy :: Proxy DescribeLaunchConfigurations)

testDescribeLifecycleHookTypesResponse :: DescribeLifecycleHookTypesResponse -> TestTree
testDescribeLifecycleHookTypesResponse = res
    "DescribeLifecycleHookTypesResponse"
    "fixture/DescribeLifecycleHookTypesResponse"
    autoScaling
    (Proxy :: Proxy DescribeLifecycleHookTypes)

testDescribeScalingActivitiesResponse :: DescribeScalingActivitiesResponse -> TestTree
testDescribeScalingActivitiesResponse = res
    "DescribeScalingActivitiesResponse"
    "fixture/DescribeScalingActivitiesResponse"
    autoScaling
    (Proxy :: Proxy DescribeScalingActivities)

testCreateAutoScalingGroupResponse :: CreateAutoScalingGroupResponse -> TestTree
testCreateAutoScalingGroupResponse = res
    "CreateAutoScalingGroupResponse"
    "fixture/CreateAutoScalingGroupResponse"
    autoScaling
    (Proxy :: Proxy CreateAutoScalingGroup)

testCreateLaunchConfigurationResponse :: CreateLaunchConfigurationResponse -> TestTree
testCreateLaunchConfigurationResponse = res
    "CreateLaunchConfigurationResponse"
    "fixture/CreateLaunchConfigurationResponse"
    autoScaling
    (Proxy :: Proxy CreateLaunchConfiguration)

testDescribeAdjustmentTypesResponse :: DescribeAdjustmentTypesResponse -> TestTree
testDescribeAdjustmentTypesResponse = res
    "DescribeAdjustmentTypesResponse"
    "fixture/DescribeAdjustmentTypesResponse"
    autoScaling
    (Proxy :: Proxy DescribeAdjustmentTypes)
