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
--         , testDescribeLoadBalancers $
--             describeLoadBalancers
--
--         , testPutNotificationConfiguration $
--             putNotificationConfiguration
--
--         , testDescribeTags $
--             describeTags
--
--         , testDeleteNotificationConfiguration $
--             deleteNotificationConfiguration
--
--         , testPutScalingPolicy $
--             putScalingPolicy
--
--         , testDeleteLaunchConfiguration $
--             deleteLaunchConfiguration
--
--         , testEnterStandby $
--             enterStandby
--
--         , testSuspendProcesses $
--             suspendProcesses
--
--         , testSetInstanceHealth $
--             setInstanceHealth
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
--         , testRecordLifecycleActionHeartbeat $
--             recordLifecycleActionHeartbeat
--
--         , testDisableMetricsCollection $
--             disableMetricsCollection
--
--         , testDetachInstances $
--             detachInstances
--
--         , testEnableMetricsCollection $
--             enableMetricsCollection
--
--         , testDescribeScalingProcessTypes $
--             describeScalingProcessTypes
--
--         , testDeleteTags $
--             deleteTags
--
--         , testDescribeLifecycleHooks $
--             describeLifecycleHooks
--
--         , testDescribeAutoScalingGroups $
--             describeAutoScalingGroups
--
--         , testDeleteScheduledAction $
--             deleteScheduledAction
--
--         , testSetDesiredCapacity $
--             setDesiredCapacity
--
--         , testDetachLoadBalancers $
--             detachLoadBalancers
--
--         , testDescribeAutoScalingNotificationTypes $
--             describeAutoScalingNotificationTypes
--
--         , testDescribeScheduledActions $
--             describeScheduledActions
--
--         , testCreateOrUpdateTags $
--             createOrUpdateTags
--
--         , testCompleteLifecycleAction $
--             completeLifecycleAction
--
--         , testDeletePolicy $
--             deletePolicy
--
--         , testAttachInstances $
--             attachInstances
--
--         , testUpdateAutoScalingGroup $
--             updateAutoScalingGroup
--
--         , testDeleteAutoScalingGroup $
--             deleteAutoScalingGroup
--
--         , testPutLifecycleHook $
--             putLifecycleHook
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
--         , testDescribeAccountLimits $
--             describeAccountLimits
--
--         , testAttachLoadBalancers $
--             attachLoadBalancers
--
--         , testTerminateInstanceInAutoScalingGroup $
--             terminateInstanceInAutoScalingGroup
--
--         , testPutScheduledUpdateGroupAction $
--             putScheduledUpdateGroupAction
--
--         , testDescribePolicies $
--             describePolicies
--
--         , testDescribeLaunchConfigurations $
--             describeLaunchConfigurations
--
--         , testDescribeScalingActivities $
--             describeScalingActivities
--
--         , testDescribeNotificationConfigurations $
--             describeNotificationConfigurations
--
--         , testDescribeLifecycleHookTypes $
--             describeLifecycleHookTypes
--
--         , testDescribeAdjustmentTypes $
--             describeAdjustmentTypes
--
--         , testCreateAutoScalingGroup $
--             createAutoScalingGroup
--
--         , testCreateLaunchConfiguration $
--             createLaunchConfiguration
--
--           ]

--     , testGroup "response"
--         [ testDescribeMetricCollectionTypesResponse $
--             describeMetricCollectionTypesResponse
--
--         , testDescribeLoadBalancersResponse $
--             describeLoadBalancersResponse
--
--         , testPutNotificationConfigurationResponse $
--             putNotificationConfigurationResponse
--
--         , testDescribeTagsResponse $
--             describeTagsResponse
--
--         , testDeleteNotificationConfigurationResponse $
--             deleteNotificationConfigurationResponse
--
--         , testPutScalingPolicyResponse $
--             putScalingPolicyResponse
--
--         , testDeleteLaunchConfigurationResponse $
--             deleteLaunchConfigurationResponse
--
--         , testEnterStandbyResponse $
--             enterStandbyResponse
--
--         , testSuspendProcessesResponse $
--             suspendProcessesResponse
--
--         , testSetInstanceHealthResponse $
--             setInstanceHealthResponse
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
--         , testRecordLifecycleActionHeartbeatResponse $
--             recordLifecycleActionHeartbeatResponse
--
--         , testDisableMetricsCollectionResponse $
--             disableMetricsCollectionResponse
--
--         , testDetachInstancesResponse $
--             detachInstancesResponse
--
--         , testEnableMetricsCollectionResponse $
--             enableMetricsCollectionResponse
--
--         , testDescribeScalingProcessTypesResponse $
--             describeScalingProcessTypesResponse
--
--         , testDeleteTagsResponse $
--             deleteTagsResponse
--
--         , testDescribeLifecycleHooksResponse $
--             describeLifecycleHooksResponse
--
--         , testDescribeAutoScalingGroupsResponse $
--             describeAutoScalingGroupsResponse
--
--         , testDeleteScheduledActionResponse $
--             deleteScheduledActionResponse
--
--         , testSetDesiredCapacityResponse $
--             setDesiredCapacityResponse
--
--         , testDetachLoadBalancersResponse $
--             detachLoadBalancersResponse
--
--         , testDescribeAutoScalingNotificationTypesResponse $
--             describeAutoScalingNotificationTypesResponse
--
--         , testDescribeScheduledActionsResponse $
--             describeScheduledActionsResponse
--
--         , testCreateOrUpdateTagsResponse $
--             createOrUpdateTagsResponse
--
--         , testCompleteLifecycleActionResponse $
--             completeLifecycleActionResponse
--
--         , testDeletePolicyResponse $
--             deletePolicyResponse
--
--         , testAttachInstancesResponse $
--             attachInstancesResponse
--
--         , testUpdateAutoScalingGroupResponse $
--             updateAutoScalingGroupResponse
--
--         , testDeleteAutoScalingGroupResponse $
--             deleteAutoScalingGroupResponse
--
--         , testPutLifecycleHookResponse $
--             putLifecycleHookResponse
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
--         , testDescribeAccountLimitsResponse $
--             describeAccountLimitsResponse
--
--         , testAttachLoadBalancersResponse $
--             attachLoadBalancersResponse
--
--         , testTerminateInstanceInAutoScalingGroupResponse $
--             terminateInstanceInAutoScalingGroupResponse
--
--         , testPutScheduledUpdateGroupActionResponse $
--             putScheduledUpdateGroupActionResponse
--
--         , testDescribePoliciesResponse $
--             describePoliciesResponse
--
--         , testDescribeLaunchConfigurationsResponse $
--             describeLaunchConfigurationsResponse
--
--         , testDescribeScalingActivitiesResponse $
--             describeScalingActivitiesResponse
--
--         , testDescribeNotificationConfigurationsResponse $
--             describeNotificationConfigurationsResponse
--
--         , testDescribeLifecycleHookTypesResponse $
--             describeLifecycleHookTypesResponse
--
--         , testDescribeAdjustmentTypesResponse $
--             describeAdjustmentTypesResponse
--
--         , testCreateAutoScalingGroupResponse $
--             createAutoScalingGroupResponse
--
--         , testCreateLaunchConfigurationResponse $
--             createLaunchConfigurationResponse
--
--           ]
--     ]

-- Requests

testDescribeMetricCollectionTypes :: DescribeMetricCollectionTypes -> TestTree
testDescribeMetricCollectionTypes = req
    "DescribeMetricCollectionTypes"
    "fixture/DescribeMetricCollectionTypes.yaml"

testDescribeLoadBalancers :: DescribeLoadBalancers -> TestTree
testDescribeLoadBalancers = req
    "DescribeLoadBalancers"
    "fixture/DescribeLoadBalancers.yaml"

testPutNotificationConfiguration :: PutNotificationConfiguration -> TestTree
testPutNotificationConfiguration = req
    "PutNotificationConfiguration"
    "fixture/PutNotificationConfiguration.yaml"

testDescribeTags :: DescribeTags -> TestTree
testDescribeTags = req
    "DescribeTags"
    "fixture/DescribeTags.yaml"

testDeleteNotificationConfiguration :: DeleteNotificationConfiguration -> TestTree
testDeleteNotificationConfiguration = req
    "DeleteNotificationConfiguration"
    "fixture/DeleteNotificationConfiguration.yaml"

testPutScalingPolicy :: PutScalingPolicy -> TestTree
testPutScalingPolicy = req
    "PutScalingPolicy"
    "fixture/PutScalingPolicy.yaml"

testDeleteLaunchConfiguration :: DeleteLaunchConfiguration -> TestTree
testDeleteLaunchConfiguration = req
    "DeleteLaunchConfiguration"
    "fixture/DeleteLaunchConfiguration.yaml"

testEnterStandby :: EnterStandby -> TestTree
testEnterStandby = req
    "EnterStandby"
    "fixture/EnterStandby.yaml"

testSuspendProcesses :: SuspendProcesses -> TestTree
testSuspendProcesses = req
    "SuspendProcesses"
    "fixture/SuspendProcesses.yaml"

testSetInstanceHealth :: SetInstanceHealth -> TestTree
testSetInstanceHealth = req
    "SetInstanceHealth"
    "fixture/SetInstanceHealth.yaml"

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

testRecordLifecycleActionHeartbeat :: RecordLifecycleActionHeartbeat -> TestTree
testRecordLifecycleActionHeartbeat = req
    "RecordLifecycleActionHeartbeat"
    "fixture/RecordLifecycleActionHeartbeat.yaml"

testDisableMetricsCollection :: DisableMetricsCollection -> TestTree
testDisableMetricsCollection = req
    "DisableMetricsCollection"
    "fixture/DisableMetricsCollection.yaml"

testDetachInstances :: DetachInstances -> TestTree
testDetachInstances = req
    "DetachInstances"
    "fixture/DetachInstances.yaml"

testEnableMetricsCollection :: EnableMetricsCollection -> TestTree
testEnableMetricsCollection = req
    "EnableMetricsCollection"
    "fixture/EnableMetricsCollection.yaml"

testDescribeScalingProcessTypes :: DescribeScalingProcessTypes -> TestTree
testDescribeScalingProcessTypes = req
    "DescribeScalingProcessTypes"
    "fixture/DescribeScalingProcessTypes.yaml"

testDeleteTags :: DeleteTags -> TestTree
testDeleteTags = req
    "DeleteTags"
    "fixture/DeleteTags.yaml"

testDescribeLifecycleHooks :: DescribeLifecycleHooks -> TestTree
testDescribeLifecycleHooks = req
    "DescribeLifecycleHooks"
    "fixture/DescribeLifecycleHooks.yaml"

testDescribeAutoScalingGroups :: DescribeAutoScalingGroups -> TestTree
testDescribeAutoScalingGroups = req
    "DescribeAutoScalingGroups"
    "fixture/DescribeAutoScalingGroups.yaml"

testDeleteScheduledAction :: DeleteScheduledAction -> TestTree
testDeleteScheduledAction = req
    "DeleteScheduledAction"
    "fixture/DeleteScheduledAction.yaml"

testSetDesiredCapacity :: SetDesiredCapacity -> TestTree
testSetDesiredCapacity = req
    "SetDesiredCapacity"
    "fixture/SetDesiredCapacity.yaml"

testDetachLoadBalancers :: DetachLoadBalancers -> TestTree
testDetachLoadBalancers = req
    "DetachLoadBalancers"
    "fixture/DetachLoadBalancers.yaml"

testDescribeAutoScalingNotificationTypes :: DescribeAutoScalingNotificationTypes -> TestTree
testDescribeAutoScalingNotificationTypes = req
    "DescribeAutoScalingNotificationTypes"
    "fixture/DescribeAutoScalingNotificationTypes.yaml"

testDescribeScheduledActions :: DescribeScheduledActions -> TestTree
testDescribeScheduledActions = req
    "DescribeScheduledActions"
    "fixture/DescribeScheduledActions.yaml"

testCreateOrUpdateTags :: CreateOrUpdateTags -> TestTree
testCreateOrUpdateTags = req
    "CreateOrUpdateTags"
    "fixture/CreateOrUpdateTags.yaml"

testCompleteLifecycleAction :: CompleteLifecycleAction -> TestTree
testCompleteLifecycleAction = req
    "CompleteLifecycleAction"
    "fixture/CompleteLifecycleAction.yaml"

testDeletePolicy :: DeletePolicy -> TestTree
testDeletePolicy = req
    "DeletePolicy"
    "fixture/DeletePolicy.yaml"

testAttachInstances :: AttachInstances -> TestTree
testAttachInstances = req
    "AttachInstances"
    "fixture/AttachInstances.yaml"

testUpdateAutoScalingGroup :: UpdateAutoScalingGroup -> TestTree
testUpdateAutoScalingGroup = req
    "UpdateAutoScalingGroup"
    "fixture/UpdateAutoScalingGroup.yaml"

testDeleteAutoScalingGroup :: DeleteAutoScalingGroup -> TestTree
testDeleteAutoScalingGroup = req
    "DeleteAutoScalingGroup"
    "fixture/DeleteAutoScalingGroup.yaml"

testPutLifecycleHook :: PutLifecycleHook -> TestTree
testPutLifecycleHook = req
    "PutLifecycleHook"
    "fixture/PutLifecycleHook.yaml"

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

testDescribeAccountLimits :: DescribeAccountLimits -> TestTree
testDescribeAccountLimits = req
    "DescribeAccountLimits"
    "fixture/DescribeAccountLimits.yaml"

testAttachLoadBalancers :: AttachLoadBalancers -> TestTree
testAttachLoadBalancers = req
    "AttachLoadBalancers"
    "fixture/AttachLoadBalancers.yaml"

testTerminateInstanceInAutoScalingGroup :: TerminateInstanceInAutoScalingGroup -> TestTree
testTerminateInstanceInAutoScalingGroup = req
    "TerminateInstanceInAutoScalingGroup"
    "fixture/TerminateInstanceInAutoScalingGroup.yaml"

testPutScheduledUpdateGroupAction :: PutScheduledUpdateGroupAction -> TestTree
testPutScheduledUpdateGroupAction = req
    "PutScheduledUpdateGroupAction"
    "fixture/PutScheduledUpdateGroupAction.yaml"

testDescribePolicies :: DescribePolicies -> TestTree
testDescribePolicies = req
    "DescribePolicies"
    "fixture/DescribePolicies.yaml"

testDescribeLaunchConfigurations :: DescribeLaunchConfigurations -> TestTree
testDescribeLaunchConfigurations = req
    "DescribeLaunchConfigurations"
    "fixture/DescribeLaunchConfigurations.yaml"

testDescribeScalingActivities :: DescribeScalingActivities -> TestTree
testDescribeScalingActivities = req
    "DescribeScalingActivities"
    "fixture/DescribeScalingActivities.yaml"

testDescribeNotificationConfigurations :: DescribeNotificationConfigurations -> TestTree
testDescribeNotificationConfigurations = req
    "DescribeNotificationConfigurations"
    "fixture/DescribeNotificationConfigurations.yaml"

testDescribeLifecycleHookTypes :: DescribeLifecycleHookTypes -> TestTree
testDescribeLifecycleHookTypes = req
    "DescribeLifecycleHookTypes"
    "fixture/DescribeLifecycleHookTypes.yaml"

testDescribeAdjustmentTypes :: DescribeAdjustmentTypes -> TestTree
testDescribeAdjustmentTypes = req
    "DescribeAdjustmentTypes"
    "fixture/DescribeAdjustmentTypes.yaml"

testCreateAutoScalingGroup :: CreateAutoScalingGroup -> TestTree
testCreateAutoScalingGroup = req
    "CreateAutoScalingGroup"
    "fixture/CreateAutoScalingGroup.yaml"

testCreateLaunchConfiguration :: CreateLaunchConfiguration -> TestTree
testCreateLaunchConfiguration = req
    "CreateLaunchConfiguration"
    "fixture/CreateLaunchConfiguration.yaml"

-- Responses

testDescribeMetricCollectionTypesResponse :: DescribeMetricCollectionTypesResponse -> TestTree
testDescribeMetricCollectionTypesResponse = res
    "DescribeMetricCollectionTypesResponse"
    "fixture/DescribeMetricCollectionTypesResponse.proto"
    autoScaling
    (Proxy :: Proxy DescribeMetricCollectionTypes)

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

testDescribeTagsResponse :: DescribeTagsResponse -> TestTree
testDescribeTagsResponse = res
    "DescribeTagsResponse"
    "fixture/DescribeTagsResponse.proto"
    autoScaling
    (Proxy :: Proxy DescribeTags)

testDeleteNotificationConfigurationResponse :: DeleteNotificationConfigurationResponse -> TestTree
testDeleteNotificationConfigurationResponse = res
    "DeleteNotificationConfigurationResponse"
    "fixture/DeleteNotificationConfigurationResponse.proto"
    autoScaling
    (Proxy :: Proxy DeleteNotificationConfiguration)

testPutScalingPolicyResponse :: PutScalingPolicyResponse -> TestTree
testPutScalingPolicyResponse = res
    "PutScalingPolicyResponse"
    "fixture/PutScalingPolicyResponse.proto"
    autoScaling
    (Proxy :: Proxy PutScalingPolicy)

testDeleteLaunchConfigurationResponse :: DeleteLaunchConfigurationResponse -> TestTree
testDeleteLaunchConfigurationResponse = res
    "DeleteLaunchConfigurationResponse"
    "fixture/DeleteLaunchConfigurationResponse.proto"
    autoScaling
    (Proxy :: Proxy DeleteLaunchConfiguration)

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

testSetInstanceHealthResponse :: SetInstanceHealthResponse -> TestTree
testSetInstanceHealthResponse = res
    "SetInstanceHealthResponse"
    "fixture/SetInstanceHealthResponse.proto"
    autoScaling
    (Proxy :: Proxy SetInstanceHealth)

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

testRecordLifecycleActionHeartbeatResponse :: RecordLifecycleActionHeartbeatResponse -> TestTree
testRecordLifecycleActionHeartbeatResponse = res
    "RecordLifecycleActionHeartbeatResponse"
    "fixture/RecordLifecycleActionHeartbeatResponse.proto"
    autoScaling
    (Proxy :: Proxy RecordLifecycleActionHeartbeat)

testDisableMetricsCollectionResponse :: DisableMetricsCollectionResponse -> TestTree
testDisableMetricsCollectionResponse = res
    "DisableMetricsCollectionResponse"
    "fixture/DisableMetricsCollectionResponse.proto"
    autoScaling
    (Proxy :: Proxy DisableMetricsCollection)

testDetachInstancesResponse :: DetachInstancesResponse -> TestTree
testDetachInstancesResponse = res
    "DetachInstancesResponse"
    "fixture/DetachInstancesResponse.proto"
    autoScaling
    (Proxy :: Proxy DetachInstances)

testEnableMetricsCollectionResponse :: EnableMetricsCollectionResponse -> TestTree
testEnableMetricsCollectionResponse = res
    "EnableMetricsCollectionResponse"
    "fixture/EnableMetricsCollectionResponse.proto"
    autoScaling
    (Proxy :: Proxy EnableMetricsCollection)

testDescribeScalingProcessTypesResponse :: DescribeScalingProcessTypesResponse -> TestTree
testDescribeScalingProcessTypesResponse = res
    "DescribeScalingProcessTypesResponse"
    "fixture/DescribeScalingProcessTypesResponse.proto"
    autoScaling
    (Proxy :: Proxy DescribeScalingProcessTypes)

testDeleteTagsResponse :: DeleteTagsResponse -> TestTree
testDeleteTagsResponse = res
    "DeleteTagsResponse"
    "fixture/DeleteTagsResponse.proto"
    autoScaling
    (Proxy :: Proxy DeleteTags)

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

testDeleteScheduledActionResponse :: DeleteScheduledActionResponse -> TestTree
testDeleteScheduledActionResponse = res
    "DeleteScheduledActionResponse"
    "fixture/DeleteScheduledActionResponse.proto"
    autoScaling
    (Proxy :: Proxy DeleteScheduledAction)

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

testDescribeAutoScalingNotificationTypesResponse :: DescribeAutoScalingNotificationTypesResponse -> TestTree
testDescribeAutoScalingNotificationTypesResponse = res
    "DescribeAutoScalingNotificationTypesResponse"
    "fixture/DescribeAutoScalingNotificationTypesResponse.proto"
    autoScaling
    (Proxy :: Proxy DescribeAutoScalingNotificationTypes)

testDescribeScheduledActionsResponse :: DescribeScheduledActionsResponse -> TestTree
testDescribeScheduledActionsResponse = res
    "DescribeScheduledActionsResponse"
    "fixture/DescribeScheduledActionsResponse.proto"
    autoScaling
    (Proxy :: Proxy DescribeScheduledActions)

testCreateOrUpdateTagsResponse :: CreateOrUpdateTagsResponse -> TestTree
testCreateOrUpdateTagsResponse = res
    "CreateOrUpdateTagsResponse"
    "fixture/CreateOrUpdateTagsResponse.proto"
    autoScaling
    (Proxy :: Proxy CreateOrUpdateTags)

testCompleteLifecycleActionResponse :: CompleteLifecycleActionResponse -> TestTree
testCompleteLifecycleActionResponse = res
    "CompleteLifecycleActionResponse"
    "fixture/CompleteLifecycleActionResponse.proto"
    autoScaling
    (Proxy :: Proxy CompleteLifecycleAction)

testDeletePolicyResponse :: DeletePolicyResponse -> TestTree
testDeletePolicyResponse = res
    "DeletePolicyResponse"
    "fixture/DeletePolicyResponse.proto"
    autoScaling
    (Proxy :: Proxy DeletePolicy)

testAttachInstancesResponse :: AttachInstancesResponse -> TestTree
testAttachInstancesResponse = res
    "AttachInstancesResponse"
    "fixture/AttachInstancesResponse.proto"
    autoScaling
    (Proxy :: Proxy AttachInstances)

testUpdateAutoScalingGroupResponse :: UpdateAutoScalingGroupResponse -> TestTree
testUpdateAutoScalingGroupResponse = res
    "UpdateAutoScalingGroupResponse"
    "fixture/UpdateAutoScalingGroupResponse.proto"
    autoScaling
    (Proxy :: Proxy UpdateAutoScalingGroup)

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

testTerminateInstanceInAutoScalingGroupResponse :: TerminateInstanceInAutoScalingGroupResponse -> TestTree
testTerminateInstanceInAutoScalingGroupResponse = res
    "TerminateInstanceInAutoScalingGroupResponse"
    "fixture/TerminateInstanceInAutoScalingGroupResponse.proto"
    autoScaling
    (Proxy :: Proxy TerminateInstanceInAutoScalingGroup)

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

testDescribeLaunchConfigurationsResponse :: DescribeLaunchConfigurationsResponse -> TestTree
testDescribeLaunchConfigurationsResponse = res
    "DescribeLaunchConfigurationsResponse"
    "fixture/DescribeLaunchConfigurationsResponse.proto"
    autoScaling
    (Proxy :: Proxy DescribeLaunchConfigurations)

testDescribeScalingActivitiesResponse :: DescribeScalingActivitiesResponse -> TestTree
testDescribeScalingActivitiesResponse = res
    "DescribeScalingActivitiesResponse"
    "fixture/DescribeScalingActivitiesResponse.proto"
    autoScaling
    (Proxy :: Proxy DescribeScalingActivities)

testDescribeNotificationConfigurationsResponse :: DescribeNotificationConfigurationsResponse -> TestTree
testDescribeNotificationConfigurationsResponse = res
    "DescribeNotificationConfigurationsResponse"
    "fixture/DescribeNotificationConfigurationsResponse.proto"
    autoScaling
    (Proxy :: Proxy DescribeNotificationConfigurations)

testDescribeLifecycleHookTypesResponse :: DescribeLifecycleHookTypesResponse -> TestTree
testDescribeLifecycleHookTypesResponse = res
    "DescribeLifecycleHookTypesResponse"
    "fixture/DescribeLifecycleHookTypesResponse.proto"
    autoScaling
    (Proxy :: Proxy DescribeLifecycleHookTypes)

testDescribeAdjustmentTypesResponse :: DescribeAdjustmentTypesResponse -> TestTree
testDescribeAdjustmentTypesResponse = res
    "DescribeAdjustmentTypesResponse"
    "fixture/DescribeAdjustmentTypesResponse.proto"
    autoScaling
    (Proxy :: Proxy DescribeAdjustmentTypes)

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
