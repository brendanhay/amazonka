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
    "fixture/DescribeMetricCollectionTypes.yaml"

testPutScalingPolicy :: PutScalingPolicy -> TestTree
testPutScalingPolicy = req
    "PutScalingPolicy"
    "fixture/PutScalingPolicy.yaml"

testDeleteNotificationConfiguration :: DeleteNotificationConfiguration -> TestTree
testDeleteNotificationConfiguration = req
    "DeleteNotificationConfiguration"
    "fixture/DeleteNotificationConfiguration.yaml"

testDescribeTags :: DescribeTags -> TestTree
testDescribeTags = req
    "DescribeTags"
    "fixture/DescribeTags.yaml"

testDeleteLaunchConfiguration :: DeleteLaunchConfiguration -> TestTree
testDeleteLaunchConfiguration = req
    "DeleteLaunchConfiguration"
    "fixture/DeleteLaunchConfiguration.yaml"

testDescribeLoadBalancers :: DescribeLoadBalancers -> TestTree
testDescribeLoadBalancers = req
    "DescribeLoadBalancers"
    "fixture/DescribeLoadBalancers.yaml"

testPutNotificationConfiguration :: PutNotificationConfiguration -> TestTree
testPutNotificationConfiguration = req
    "PutNotificationConfiguration"
    "fixture/PutNotificationConfiguration.yaml"

testSetInstanceHealth :: SetInstanceHealth -> TestTree
testSetInstanceHealth = req
    "SetInstanceHealth"
    "fixture/SetInstanceHealth.yaml"

testEnterStandby :: EnterStandby -> TestTree
testEnterStandby = req
    "EnterStandby"
    "fixture/EnterStandby.yaml"

testSuspendProcesses :: SuspendProcesses -> TestTree
testSuspendProcesses = req
    "SuspendProcesses"
    "fixture/SuspendProcesses.yaml"

testExitStandby :: ExitStandby -> TestTree
testExitStandby = req
    "ExitStandby"
    "fixture/ExitStandby.yaml"

testDescribeTerminationPolicyTypes :: DescribeTerminationPolicyTypes -> TestTree
testDescribeTerminationPolicyTypes = req
    "DescribeTerminationPolicyTypes"
    "fixture/DescribeTerminationPolicyTypes.yaml"

testDescribeAutoScalingInstances :: DescribeAutoScalingInstances -> TestTree
testDescribeAutoScalingInstances = req
    "DescribeAutoScalingInstances"
    "fixture/DescribeAutoScalingInstances.yaml"

testDetachInstances :: DetachInstances -> TestTree
testDetachInstances = req
    "DetachInstances"
    "fixture/DetachInstances.yaml"

testDisableMetricsCollection :: DisableMetricsCollection -> TestTree
testDisableMetricsCollection = req
    "DisableMetricsCollection"
    "fixture/DisableMetricsCollection.yaml"

testRecordLifecycleActionHeartbeat :: RecordLifecycleActionHeartbeat -> TestTree
testRecordLifecycleActionHeartbeat = req
    "RecordLifecycleActionHeartbeat"
    "fixture/RecordLifecycleActionHeartbeat.yaml"

testDeleteTags :: DeleteTags -> TestTree
testDeleteTags = req
    "DeleteTags"
    "fixture/DeleteTags.yaml"

testDescribeScalingProcessTypes :: DescribeScalingProcessTypes -> TestTree
testDescribeScalingProcessTypes = req
    "DescribeScalingProcessTypes"
    "fixture/DescribeScalingProcessTypes.yaml"

testEnableMetricsCollection :: EnableMetricsCollection -> TestTree
testEnableMetricsCollection = req
    "EnableMetricsCollection"
    "fixture/EnableMetricsCollection.yaml"

testDescribeLifecycleHooks :: DescribeLifecycleHooks -> TestTree
testDescribeLifecycleHooks = req
    "DescribeLifecycleHooks"
    "fixture/DescribeLifecycleHooks.yaml"

testDescribeAutoScalingGroups :: DescribeAutoScalingGroups -> TestTree
testDescribeAutoScalingGroups = req
    "DescribeAutoScalingGroups"
    "fixture/DescribeAutoScalingGroups.yaml"

testSetDesiredCapacity :: SetDesiredCapacity -> TestTree
testSetDesiredCapacity = req
    "SetDesiredCapacity"
    "fixture/SetDesiredCapacity.yaml"

testDetachLoadBalancers :: DetachLoadBalancers -> TestTree
testDetachLoadBalancers = req
    "DetachLoadBalancers"
    "fixture/DetachLoadBalancers.yaml"

testDeleteScheduledAction :: DeleteScheduledAction -> TestTree
testDeleteScheduledAction = req
    "DeleteScheduledAction"
    "fixture/DeleteScheduledAction.yaml"

testCreateOrUpdateTags :: CreateOrUpdateTags -> TestTree
testCreateOrUpdateTags = req
    "CreateOrUpdateTags"
    "fixture/CreateOrUpdateTags.yaml"

testDeletePolicy :: DeletePolicy -> TestTree
testDeletePolicy = req
    "DeletePolicy"
    "fixture/DeletePolicy.yaml"

testDescribeAutoScalingNotificationTypes :: DescribeAutoScalingNotificationTypes -> TestTree
testDescribeAutoScalingNotificationTypes = req
    "DescribeAutoScalingNotificationTypes"
    "fixture/DescribeAutoScalingNotificationTypes.yaml"

testCompleteLifecycleAction :: CompleteLifecycleAction -> TestTree
testCompleteLifecycleAction = req
    "CompleteLifecycleAction"
    "fixture/CompleteLifecycleAction.yaml"

testAttachInstances :: AttachInstances -> TestTree
testAttachInstances = req
    "AttachInstances"
    "fixture/AttachInstances.yaml"

testDescribeScheduledActions :: DescribeScheduledActions -> TestTree
testDescribeScheduledActions = req
    "DescribeScheduledActions"
    "fixture/DescribeScheduledActions.yaml"

testDeleteAutoScalingGroup :: DeleteAutoScalingGroup -> TestTree
testDeleteAutoScalingGroup = req
    "DeleteAutoScalingGroup"
    "fixture/DeleteAutoScalingGroup.yaml"

testPutLifecycleHook :: PutLifecycleHook -> TestTree
testPutLifecycleHook = req
    "PutLifecycleHook"
    "fixture/PutLifecycleHook.yaml"

testUpdateAutoScalingGroup :: UpdateAutoScalingGroup -> TestTree
testUpdateAutoScalingGroup = req
    "UpdateAutoScalingGroup"
    "fixture/UpdateAutoScalingGroup.yaml"

testDeleteLifecycleHook :: DeleteLifecycleHook -> TestTree
testDeleteLifecycleHook = req
    "DeleteLifecycleHook"
    "fixture/DeleteLifecycleHook.yaml"

testResumeProcesses :: ResumeProcesses -> TestTree
testResumeProcesses = req
    "ResumeProcesses"
    "fixture/ResumeProcesses.yaml"

testExecutePolicy :: ExecutePolicy -> TestTree
testExecutePolicy = req
    "ExecutePolicy"
    "fixture/ExecutePolicy.yaml"

testTerminateInstanceInAutoScalingGroup :: TerminateInstanceInAutoScalingGroup -> TestTree
testTerminateInstanceInAutoScalingGroup = req
    "TerminateInstanceInAutoScalingGroup"
    "fixture/TerminateInstanceInAutoScalingGroup.yaml"

testDescribeAccountLimits :: DescribeAccountLimits -> TestTree
testDescribeAccountLimits = req
    "DescribeAccountLimits"
    "fixture/DescribeAccountLimits.yaml"

testAttachLoadBalancers :: AttachLoadBalancers -> TestTree
testAttachLoadBalancers = req
    "AttachLoadBalancers"
    "fixture/AttachLoadBalancers.yaml"

testPutScheduledUpdateGroupAction :: PutScheduledUpdateGroupAction -> TestTree
testPutScheduledUpdateGroupAction = req
    "PutScheduledUpdateGroupAction"
    "fixture/PutScheduledUpdateGroupAction.yaml"

testDescribePolicies :: DescribePolicies -> TestTree
testDescribePolicies = req
    "DescribePolicies"
    "fixture/DescribePolicies.yaml"

testDescribeNotificationConfigurations :: DescribeNotificationConfigurations -> TestTree
testDescribeNotificationConfigurations = req
    "DescribeNotificationConfigurations"
    "fixture/DescribeNotificationConfigurations.yaml"

testDescribeLaunchConfigurations :: DescribeLaunchConfigurations -> TestTree
testDescribeLaunchConfigurations = req
    "DescribeLaunchConfigurations"
    "fixture/DescribeLaunchConfigurations.yaml"

testDescribeLifecycleHookTypes :: DescribeLifecycleHookTypes -> TestTree
testDescribeLifecycleHookTypes = req
    "DescribeLifecycleHookTypes"
    "fixture/DescribeLifecycleHookTypes.yaml"

testDescribeScalingActivities :: DescribeScalingActivities -> TestTree
testDescribeScalingActivities = req
    "DescribeScalingActivities"
    "fixture/DescribeScalingActivities.yaml"

testCreateAutoScalingGroup :: CreateAutoScalingGroup -> TestTree
testCreateAutoScalingGroup = req
    "CreateAutoScalingGroup"
    "fixture/CreateAutoScalingGroup.yaml"

testCreateLaunchConfiguration :: CreateLaunchConfiguration -> TestTree
testCreateLaunchConfiguration = req
    "CreateLaunchConfiguration"
    "fixture/CreateLaunchConfiguration.yaml"

testDescribeAdjustmentTypes :: DescribeAdjustmentTypes -> TestTree
testDescribeAdjustmentTypes = req
    "DescribeAdjustmentTypes"
    "fixture/DescribeAdjustmentTypes.yaml"

-- Responses

testDescribeMetricCollectionTypesResponse :: DescribeMetricCollectionTypesResponse -> TestTree
testDescribeMetricCollectionTypesResponse = res
    "DescribeMetricCollectionTypesResponse"
    "fixture/DescribeMetricCollectionTypesResponse.proto"
    autoScaling
    (Proxy :: Proxy DescribeMetricCollectionTypes)

testPutScalingPolicyResponse :: PutScalingPolicyResponse -> TestTree
testPutScalingPolicyResponse = res
    "PutScalingPolicyResponse"
    "fixture/PutScalingPolicyResponse.proto"
    autoScaling
    (Proxy :: Proxy PutScalingPolicy)

testDeleteNotificationConfigurationResponse :: DeleteNotificationConfigurationResponse -> TestTree
testDeleteNotificationConfigurationResponse = res
    "DeleteNotificationConfigurationResponse"
    "fixture/DeleteNotificationConfigurationResponse.proto"
    autoScaling
    (Proxy :: Proxy DeleteNotificationConfiguration)

testDescribeTagsResponse :: DescribeTagsResponse -> TestTree
testDescribeTagsResponse = res
    "DescribeTagsResponse"
    "fixture/DescribeTagsResponse.proto"
    autoScaling
    (Proxy :: Proxy DescribeTags)

testDeleteLaunchConfigurationResponse :: DeleteLaunchConfigurationResponse -> TestTree
testDeleteLaunchConfigurationResponse = res
    "DeleteLaunchConfigurationResponse"
    "fixture/DeleteLaunchConfigurationResponse.proto"
    autoScaling
    (Proxy :: Proxy DeleteLaunchConfiguration)

testDescribeLoadBalancersResponse :: DescribeLoadBalancersResponse -> TestTree
testDescribeLoadBalancersResponse = res
    "DescribeLoadBalancersResponse"
    "fixture/DescribeLoadBalancersResponse.proto"
    autoScaling
    (Proxy :: Proxy DescribeLoadBalancers)

testPutNotificationConfigurationResponse :: PutNotificationConfigurationResponse -> TestTree
testPutNotificationConfigurationResponse = res
    "PutNotificationConfigurationResponse"
    "fixture/PutNotificationConfigurationResponse.proto"
    autoScaling
    (Proxy :: Proxy PutNotificationConfiguration)

testSetInstanceHealthResponse :: SetInstanceHealthResponse -> TestTree
testSetInstanceHealthResponse = res
    "SetInstanceHealthResponse"
    "fixture/SetInstanceHealthResponse.proto"
    autoScaling
    (Proxy :: Proxy SetInstanceHealth)

testEnterStandbyResponse :: EnterStandbyResponse -> TestTree
testEnterStandbyResponse = res
    "EnterStandbyResponse"
    "fixture/EnterStandbyResponse.proto"
    autoScaling
    (Proxy :: Proxy EnterStandby)

testSuspendProcessesResponse :: SuspendProcessesResponse -> TestTree
testSuspendProcessesResponse = res
    "SuspendProcessesResponse"
    "fixture/SuspendProcessesResponse.proto"
    autoScaling
    (Proxy :: Proxy SuspendProcesses)

testExitStandbyResponse :: ExitStandbyResponse -> TestTree
testExitStandbyResponse = res
    "ExitStandbyResponse"
    "fixture/ExitStandbyResponse.proto"
    autoScaling
    (Proxy :: Proxy ExitStandby)

testDescribeTerminationPolicyTypesResponse :: DescribeTerminationPolicyTypesResponse -> TestTree
testDescribeTerminationPolicyTypesResponse = res
    "DescribeTerminationPolicyTypesResponse"
    "fixture/DescribeTerminationPolicyTypesResponse.proto"
    autoScaling
    (Proxy :: Proxy DescribeTerminationPolicyTypes)

testDescribeAutoScalingInstancesResponse :: DescribeAutoScalingInstancesResponse -> TestTree
testDescribeAutoScalingInstancesResponse = res
    "DescribeAutoScalingInstancesResponse"
    "fixture/DescribeAutoScalingInstancesResponse.proto"
    autoScaling
    (Proxy :: Proxy DescribeAutoScalingInstances)

testDetachInstancesResponse :: DetachInstancesResponse -> TestTree
testDetachInstancesResponse = res
    "DetachInstancesResponse"
    "fixture/DetachInstancesResponse.proto"
    autoScaling
    (Proxy :: Proxy DetachInstances)

testDisableMetricsCollectionResponse :: DisableMetricsCollectionResponse -> TestTree
testDisableMetricsCollectionResponse = res
    "DisableMetricsCollectionResponse"
    "fixture/DisableMetricsCollectionResponse.proto"
    autoScaling
    (Proxy :: Proxy DisableMetricsCollection)

testRecordLifecycleActionHeartbeatResponse :: RecordLifecycleActionHeartbeatResponse -> TestTree
testRecordLifecycleActionHeartbeatResponse = res
    "RecordLifecycleActionHeartbeatResponse"
    "fixture/RecordLifecycleActionHeartbeatResponse.proto"
    autoScaling
    (Proxy :: Proxy RecordLifecycleActionHeartbeat)

testDeleteTagsResponse :: DeleteTagsResponse -> TestTree
testDeleteTagsResponse = res
    "DeleteTagsResponse"
    "fixture/DeleteTagsResponse.proto"
    autoScaling
    (Proxy :: Proxy DeleteTags)

testDescribeScalingProcessTypesResponse :: DescribeScalingProcessTypesResponse -> TestTree
testDescribeScalingProcessTypesResponse = res
    "DescribeScalingProcessTypesResponse"
    "fixture/DescribeScalingProcessTypesResponse.proto"
    autoScaling
    (Proxy :: Proxy DescribeScalingProcessTypes)

testEnableMetricsCollectionResponse :: EnableMetricsCollectionResponse -> TestTree
testEnableMetricsCollectionResponse = res
    "EnableMetricsCollectionResponse"
    "fixture/EnableMetricsCollectionResponse.proto"
    autoScaling
    (Proxy :: Proxy EnableMetricsCollection)

testDescribeLifecycleHooksResponse :: DescribeLifecycleHooksResponse -> TestTree
testDescribeLifecycleHooksResponse = res
    "DescribeLifecycleHooksResponse"
    "fixture/DescribeLifecycleHooksResponse.proto"
    autoScaling
    (Proxy :: Proxy DescribeLifecycleHooks)

testDescribeAutoScalingGroupsResponse :: DescribeAutoScalingGroupsResponse -> TestTree
testDescribeAutoScalingGroupsResponse = res
    "DescribeAutoScalingGroupsResponse"
    "fixture/DescribeAutoScalingGroupsResponse.proto"
    autoScaling
    (Proxy :: Proxy DescribeAutoScalingGroups)

testSetDesiredCapacityResponse :: SetDesiredCapacityResponse -> TestTree
testSetDesiredCapacityResponse = res
    "SetDesiredCapacityResponse"
    "fixture/SetDesiredCapacityResponse.proto"
    autoScaling
    (Proxy :: Proxy SetDesiredCapacity)

testDetachLoadBalancersResponse :: DetachLoadBalancersResponse -> TestTree
testDetachLoadBalancersResponse = res
    "DetachLoadBalancersResponse"
    "fixture/DetachLoadBalancersResponse.proto"
    autoScaling
    (Proxy :: Proxy DetachLoadBalancers)

testDeleteScheduledActionResponse :: DeleteScheduledActionResponse -> TestTree
testDeleteScheduledActionResponse = res
    "DeleteScheduledActionResponse"
    "fixture/DeleteScheduledActionResponse.proto"
    autoScaling
    (Proxy :: Proxy DeleteScheduledAction)

testCreateOrUpdateTagsResponse :: CreateOrUpdateTagsResponse -> TestTree
testCreateOrUpdateTagsResponse = res
    "CreateOrUpdateTagsResponse"
    "fixture/CreateOrUpdateTagsResponse.proto"
    autoScaling
    (Proxy :: Proxy CreateOrUpdateTags)

testDeletePolicyResponse :: DeletePolicyResponse -> TestTree
testDeletePolicyResponse = res
    "DeletePolicyResponse"
    "fixture/DeletePolicyResponse.proto"
    autoScaling
    (Proxy :: Proxy DeletePolicy)

testDescribeAutoScalingNotificationTypesResponse :: DescribeAutoScalingNotificationTypesResponse -> TestTree
testDescribeAutoScalingNotificationTypesResponse = res
    "DescribeAutoScalingNotificationTypesResponse"
    "fixture/DescribeAutoScalingNotificationTypesResponse.proto"
    autoScaling
    (Proxy :: Proxy DescribeAutoScalingNotificationTypes)

testCompleteLifecycleActionResponse :: CompleteLifecycleActionResponse -> TestTree
testCompleteLifecycleActionResponse = res
    "CompleteLifecycleActionResponse"
    "fixture/CompleteLifecycleActionResponse.proto"
    autoScaling
    (Proxy :: Proxy CompleteLifecycleAction)

testAttachInstancesResponse :: AttachInstancesResponse -> TestTree
testAttachInstancesResponse = res
    "AttachInstancesResponse"
    "fixture/AttachInstancesResponse.proto"
    autoScaling
    (Proxy :: Proxy AttachInstances)

testDescribeScheduledActionsResponse :: DescribeScheduledActionsResponse -> TestTree
testDescribeScheduledActionsResponse = res
    "DescribeScheduledActionsResponse"
    "fixture/DescribeScheduledActionsResponse.proto"
    autoScaling
    (Proxy :: Proxy DescribeScheduledActions)

testDeleteAutoScalingGroupResponse :: DeleteAutoScalingGroupResponse -> TestTree
testDeleteAutoScalingGroupResponse = res
    "DeleteAutoScalingGroupResponse"
    "fixture/DeleteAutoScalingGroupResponse.proto"
    autoScaling
    (Proxy :: Proxy DeleteAutoScalingGroup)

testPutLifecycleHookResponse :: PutLifecycleHookResponse -> TestTree
testPutLifecycleHookResponse = res
    "PutLifecycleHookResponse"
    "fixture/PutLifecycleHookResponse.proto"
    autoScaling
    (Proxy :: Proxy PutLifecycleHook)

testUpdateAutoScalingGroupResponse :: UpdateAutoScalingGroupResponse -> TestTree
testUpdateAutoScalingGroupResponse = res
    "UpdateAutoScalingGroupResponse"
    "fixture/UpdateAutoScalingGroupResponse.proto"
    autoScaling
    (Proxy :: Proxy UpdateAutoScalingGroup)

testDeleteLifecycleHookResponse :: DeleteLifecycleHookResponse -> TestTree
testDeleteLifecycleHookResponse = res
    "DeleteLifecycleHookResponse"
    "fixture/DeleteLifecycleHookResponse.proto"
    autoScaling
    (Proxy :: Proxy DeleteLifecycleHook)

testResumeProcessesResponse :: ResumeProcessesResponse -> TestTree
testResumeProcessesResponse = res
    "ResumeProcessesResponse"
    "fixture/ResumeProcessesResponse.proto"
    autoScaling
    (Proxy :: Proxy ResumeProcesses)

testExecutePolicyResponse :: ExecutePolicyResponse -> TestTree
testExecutePolicyResponse = res
    "ExecutePolicyResponse"
    "fixture/ExecutePolicyResponse.proto"
    autoScaling
    (Proxy :: Proxy ExecutePolicy)

testTerminateInstanceInAutoScalingGroupResponse :: TerminateInstanceInAutoScalingGroupResponse -> TestTree
testTerminateInstanceInAutoScalingGroupResponse = res
    "TerminateInstanceInAutoScalingGroupResponse"
    "fixture/TerminateInstanceInAutoScalingGroupResponse.proto"
    autoScaling
    (Proxy :: Proxy TerminateInstanceInAutoScalingGroup)

testDescribeAccountLimitsResponse :: DescribeAccountLimitsResponse -> TestTree
testDescribeAccountLimitsResponse = res
    "DescribeAccountLimitsResponse"
    "fixture/DescribeAccountLimitsResponse.proto"
    autoScaling
    (Proxy :: Proxy DescribeAccountLimits)

testAttachLoadBalancersResponse :: AttachLoadBalancersResponse -> TestTree
testAttachLoadBalancersResponse = res
    "AttachLoadBalancersResponse"
    "fixture/AttachLoadBalancersResponse.proto"
    autoScaling
    (Proxy :: Proxy AttachLoadBalancers)

testPutScheduledUpdateGroupActionResponse :: PutScheduledUpdateGroupActionResponse -> TestTree
testPutScheduledUpdateGroupActionResponse = res
    "PutScheduledUpdateGroupActionResponse"
    "fixture/PutScheduledUpdateGroupActionResponse.proto"
    autoScaling
    (Proxy :: Proxy PutScheduledUpdateGroupAction)

testDescribePoliciesResponse :: DescribePoliciesResponse -> TestTree
testDescribePoliciesResponse = res
    "DescribePoliciesResponse"
    "fixture/DescribePoliciesResponse.proto"
    autoScaling
    (Proxy :: Proxy DescribePolicies)

testDescribeNotificationConfigurationsResponse :: DescribeNotificationConfigurationsResponse -> TestTree
testDescribeNotificationConfigurationsResponse = res
    "DescribeNotificationConfigurationsResponse"
    "fixture/DescribeNotificationConfigurationsResponse.proto"
    autoScaling
    (Proxy :: Proxy DescribeNotificationConfigurations)

testDescribeLaunchConfigurationsResponse :: DescribeLaunchConfigurationsResponse -> TestTree
testDescribeLaunchConfigurationsResponse = res
    "DescribeLaunchConfigurationsResponse"
    "fixture/DescribeLaunchConfigurationsResponse.proto"
    autoScaling
    (Proxy :: Proxy DescribeLaunchConfigurations)

testDescribeLifecycleHookTypesResponse :: DescribeLifecycleHookTypesResponse -> TestTree
testDescribeLifecycleHookTypesResponse = res
    "DescribeLifecycleHookTypesResponse"
    "fixture/DescribeLifecycleHookTypesResponse.proto"
    autoScaling
    (Proxy :: Proxy DescribeLifecycleHookTypes)

testDescribeScalingActivitiesResponse :: DescribeScalingActivitiesResponse -> TestTree
testDescribeScalingActivitiesResponse = res
    "DescribeScalingActivitiesResponse"
    "fixture/DescribeScalingActivitiesResponse.proto"
    autoScaling
    (Proxy :: Proxy DescribeScalingActivities)

testCreateAutoScalingGroupResponse :: CreateAutoScalingGroupResponse -> TestTree
testCreateAutoScalingGroupResponse = res
    "CreateAutoScalingGroupResponse"
    "fixture/CreateAutoScalingGroupResponse.proto"
    autoScaling
    (Proxy :: Proxy CreateAutoScalingGroup)

testCreateLaunchConfigurationResponse :: CreateLaunchConfigurationResponse -> TestTree
testCreateLaunchConfigurationResponse = res
    "CreateLaunchConfigurationResponse"
    "fixture/CreateLaunchConfigurationResponse.proto"
    autoScaling
    (Proxy :: Proxy CreateLaunchConfiguration)

testDescribeAdjustmentTypesResponse :: DescribeAdjustmentTypesResponse -> TestTree
testDescribeAdjustmentTypesResponse = res
    "DescribeAdjustmentTypesResponse"
    "fixture/DescribeAdjustmentTypesResponse.proto"
    autoScaling
    (Proxy :: Proxy DescribeAdjustmentTypes)
