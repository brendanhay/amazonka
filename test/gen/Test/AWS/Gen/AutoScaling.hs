-- Module      : Test.AWS.Gen.AutoScaling
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

module Test.AWS.Gen.AutoScaling where

import           Data.Proxy
import           Network.AWS.AutoScaling
import           Test.AWS.Fixture
import           Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures = testGroup "SQS"
--     [ testGroup "request"
--         [ describeMetricCollectionTypesTest $
--             describeMetricCollectionTypes
--
--         , putScalingPolicyTest $
--             putScalingPolicy
--
--         , deleteNotificationConfigurationTest $
--             deleteNotificationConfiguration
--
--         , describeTagsTest $
--             describeTags
--
--         , deleteLaunchConfigurationTest $
--             deleteLaunchConfiguration
--
--         , describeLoadBalancersTest $
--             describeLoadBalancers
--
--         , putNotificationConfigurationTest $
--             putNotificationConfiguration
--
--         , setInstanceHealthTest $
--             setInstanceHealth
--
--         , enterStandbyTest $
--             enterStandby
--
--         , suspendProcessesTest $
--             suspendProcesses
--
--         , exitStandbyTest $
--             exitStandby
--
--         , describeTerminationPolicyTypesTest $
--             describeTerminationPolicyTypes
--
--         , describeAutoScalingInstancesTest $
--             describeAutoScalingInstances
--
--         , detachInstancesTest $
--             detachInstances
--
--         , disableMetricsCollectionTest $
--             disableMetricsCollection
--
--         , recordLifecycleActionHeartbeatTest $
--             recordLifecycleActionHeartbeat
--
--         , deleteTagsTest $
--             deleteTags
--
--         , describeScalingProcessTypesTest $
--             describeScalingProcessTypes
--
--         , enableMetricsCollectionTest $
--             enableMetricsCollection
--
--         , describeLifecycleHooksTest $
--             describeLifecycleHooks
--
--         , describeAutoScalingGroupsTest $
--             describeAutoScalingGroups
--
--         , setDesiredCapacityTest $
--             setDesiredCapacity
--
--         , detachLoadBalancersTest $
--             detachLoadBalancers
--
--         , deleteScheduledActionTest $
--             deleteScheduledAction
--
--         , createOrUpdateTagsTest $
--             createOrUpdateTags
--
--         , deletePolicyTest $
--             deletePolicy
--
--         , describeAutoScalingNotificationTypesTest $
--             describeAutoScalingNotificationTypes
--
--         , completeLifecycleActionTest $
--             completeLifecycleAction
--
--         , attachInstancesTest $
--             attachInstances
--
--         , describeScheduledActionsTest $
--             describeScheduledActions
--
--         , deleteAutoScalingGroupTest $
--             deleteAutoScalingGroup
--
--         , putLifecycleHookTest $
--             putLifecycleHook
--
--         , updateAutoScalingGroupTest $
--             updateAutoScalingGroup
--
--         , deleteLifecycleHookTest $
--             deleteLifecycleHook
--
--         , resumeProcessesTest $
--             resumeProcesses
--
--         , executePolicyTest $
--             executePolicy
--
--         , terminateInstanceInAutoScalingGroupTest $
--             terminateInstanceInAutoScalingGroup
--
--         , describeAccountLimitsTest $
--             describeAccountLimits
--
--         , attachLoadBalancersTest $
--             attachLoadBalancers
--
--         , putScheduledUpdateGroupActionTest $
--             putScheduledUpdateGroupAction
--
--         , describePoliciesTest $
--             describePolicies
--
--         , describeNotificationConfigurationsTest $
--             describeNotificationConfigurations
--
--         , describeLaunchConfigurationsTest $
--             describeLaunchConfigurations
--
--         , describeLifecycleHookTypesTest $
--             describeLifecycleHookTypes
--
--         , describeScalingActivitiesTest $
--             describeScalingActivities
--
--         , createAutoScalingGroupTest $
--             createAutoScalingGroup
--
--         , createLaunchConfigurationTest $
--             createLaunchConfiguration
--
--         , describeAdjustmentTypesTest $
--             describeAdjustmentTypes
--
--           ]

--     , testGroup "response"
--         [ describeMetricCollectionTypesResponseTest $
--             describeMetricCollectionTypesResponse
--
--         , putScalingPolicyResponseTest $
--             putScalingPolicyResponse
--
--         , deleteNotificationConfigurationResponseTest $
--             deleteNotificationConfigurationResponse
--
--         , describeTagsResponseTest $
--             describeTagsResponse
--
--         , deleteLaunchConfigurationResponseTest $
--             deleteLaunchConfigurationResponse
--
--         , describeLoadBalancersResponseTest $
--             describeLoadBalancersResponse
--
--         , putNotificationConfigurationResponseTest $
--             putNotificationConfigurationResponse
--
--         , setInstanceHealthResponseTest $
--             setInstanceHealthResponse
--
--         , enterStandbyResponseTest $
--             enterStandbyResponse
--
--         , suspendProcessesResponseTest $
--             suspendProcessesResponse
--
--         , exitStandbyResponseTest $
--             exitStandbyResponse
--
--         , describeTerminationPolicyTypesResponseTest $
--             describeTerminationPolicyTypesResponse
--
--         , describeAutoScalingInstancesResponseTest $
--             describeAutoScalingInstancesResponse
--
--         , detachInstancesResponseTest $
--             detachInstancesResponse
--
--         , disableMetricsCollectionResponseTest $
--             disableMetricsCollectionResponse
--
--         , recordLifecycleActionHeartbeatResponseTest $
--             recordLifecycleActionHeartbeatResponse
--
--         , deleteTagsResponseTest $
--             deleteTagsResponse
--
--         , describeScalingProcessTypesResponseTest $
--             describeScalingProcessTypesResponse
--
--         , enableMetricsCollectionResponseTest $
--             enableMetricsCollectionResponse
--
--         , describeLifecycleHooksResponseTest $
--             describeLifecycleHooksResponse
--
--         , describeAutoScalingGroupsResponseTest $
--             describeAutoScalingGroupsResponse
--
--         , setDesiredCapacityResponseTest $
--             setDesiredCapacityResponse
--
--         , detachLoadBalancersResponseTest $
--             detachLoadBalancersResponse
--
--         , deleteScheduledActionResponseTest $
--             deleteScheduledActionResponse
--
--         , createOrUpdateTagsResponseTest $
--             createOrUpdateTagsResponse
--
--         , deletePolicyResponseTest $
--             deletePolicyResponse
--
--         , describeAutoScalingNotificationTypesResponseTest $
--             describeAutoScalingNotificationTypesResponse
--
--         , completeLifecycleActionResponseTest $
--             completeLifecycleActionResponse
--
--         , attachInstancesResponseTest $
--             attachInstancesResponse
--
--         , describeScheduledActionsResponseTest $
--             describeScheduledActionsResponse
--
--         , deleteAutoScalingGroupResponseTest $
--             deleteAutoScalingGroupResponse
--
--         , putLifecycleHookResponseTest $
--             putLifecycleHookResponse
--
--         , updateAutoScalingGroupResponseTest $
--             updateAutoScalingGroupResponse
--
--         , deleteLifecycleHookResponseTest $
--             deleteLifecycleHookResponse
--
--         , resumeProcessesResponseTest $
--             resumeProcessesResponse
--
--         , executePolicyResponseTest $
--             executePolicyResponse
--
--         , terminateInstanceInAutoScalingGroupResponseTest $
--             terminateInstanceInAutoScalingGroupResponse
--
--         , describeAccountLimitsResponseTest $
--             describeAccountLimitsResponse
--
--         , attachLoadBalancersResponseTest $
--             attachLoadBalancersResponse
--
--         , putScheduledUpdateGroupActionResponseTest $
--             putScheduledUpdateGroupActionResponse
--
--         , describePoliciesResponseTest $
--             describePoliciesResponse
--
--         , describeNotificationConfigurationsResponseTest $
--             describeNotificationConfigurationsResponse
--
--         , describeLaunchConfigurationsResponseTest $
--             describeLaunchConfigurationsResponse
--
--         , describeLifecycleHookTypesResponseTest $
--             describeLifecycleHookTypesResponse
--
--         , describeScalingActivitiesResponseTest $
--             describeScalingActivitiesResponse
--
--         , createAutoScalingGroupResponseTest $
--             createAutoScalingGroupResponse
--
--         , createLaunchConfigurationResponseTest $
--             createLaunchConfigurationResponse
--
--         , describeAdjustmentTypesResponseTest $
--             describeAdjustmentTypesResponse
--
--           ]
--     ]

-- Requests

describeMetricCollectionTypesTest :: DescribeMetricCollectionTypes -> TestTree
describeMetricCollectionTypesTest = undefined

putScalingPolicyTest :: PutScalingPolicy -> TestTree
putScalingPolicyTest = undefined

deleteNotificationConfigurationTest :: DeleteNotificationConfiguration -> TestTree
deleteNotificationConfigurationTest = undefined

describeTagsTest :: DescribeTags -> TestTree
describeTagsTest = undefined

deleteLaunchConfigurationTest :: DeleteLaunchConfiguration -> TestTree
deleteLaunchConfigurationTest = undefined

describeLoadBalancersTest :: DescribeLoadBalancers -> TestTree
describeLoadBalancersTest = undefined

putNotificationConfigurationTest :: PutNotificationConfiguration -> TestTree
putNotificationConfigurationTest = undefined

setInstanceHealthTest :: SetInstanceHealth -> TestTree
setInstanceHealthTest = undefined

enterStandbyTest :: EnterStandby -> TestTree
enterStandbyTest = undefined

suspendProcessesTest :: SuspendProcesses -> TestTree
suspendProcessesTest = undefined

exitStandbyTest :: ExitStandby -> TestTree
exitStandbyTest = undefined

describeTerminationPolicyTypesTest :: DescribeTerminationPolicyTypes -> TestTree
describeTerminationPolicyTypesTest = undefined

describeAutoScalingInstancesTest :: DescribeAutoScalingInstances -> TestTree
describeAutoScalingInstancesTest = undefined

detachInstancesTest :: DetachInstances -> TestTree
detachInstancesTest = undefined

disableMetricsCollectionTest :: DisableMetricsCollection -> TestTree
disableMetricsCollectionTest = undefined

recordLifecycleActionHeartbeatTest :: RecordLifecycleActionHeartbeat -> TestTree
recordLifecycleActionHeartbeatTest = undefined

deleteTagsTest :: DeleteTags -> TestTree
deleteTagsTest = undefined

describeScalingProcessTypesTest :: DescribeScalingProcessTypes -> TestTree
describeScalingProcessTypesTest = undefined

enableMetricsCollectionTest :: EnableMetricsCollection -> TestTree
enableMetricsCollectionTest = undefined

describeLifecycleHooksTest :: DescribeLifecycleHooks -> TestTree
describeLifecycleHooksTest = undefined

describeAutoScalingGroupsTest :: DescribeAutoScalingGroups -> TestTree
describeAutoScalingGroupsTest = undefined

setDesiredCapacityTest :: SetDesiredCapacity -> TestTree
setDesiredCapacityTest = undefined

detachLoadBalancersTest :: DetachLoadBalancers -> TestTree
detachLoadBalancersTest = undefined

deleteScheduledActionTest :: DeleteScheduledAction -> TestTree
deleteScheduledActionTest = undefined

createOrUpdateTagsTest :: CreateOrUpdateTags -> TestTree
createOrUpdateTagsTest = undefined

deletePolicyTest :: DeletePolicy -> TestTree
deletePolicyTest = undefined

describeAutoScalingNotificationTypesTest :: DescribeAutoScalingNotificationTypes -> TestTree
describeAutoScalingNotificationTypesTest = undefined

completeLifecycleActionTest :: CompleteLifecycleAction -> TestTree
completeLifecycleActionTest = undefined

attachInstancesTest :: AttachInstances -> TestTree
attachInstancesTest = undefined

describeScheduledActionsTest :: DescribeScheduledActions -> TestTree
describeScheduledActionsTest = undefined

deleteAutoScalingGroupTest :: DeleteAutoScalingGroup -> TestTree
deleteAutoScalingGroupTest = undefined

putLifecycleHookTest :: PutLifecycleHook -> TestTree
putLifecycleHookTest = undefined

updateAutoScalingGroupTest :: UpdateAutoScalingGroup -> TestTree
updateAutoScalingGroupTest = undefined

deleteLifecycleHookTest :: DeleteLifecycleHook -> TestTree
deleteLifecycleHookTest = undefined

resumeProcessesTest :: ResumeProcesses -> TestTree
resumeProcessesTest = undefined

executePolicyTest :: ExecutePolicy -> TestTree
executePolicyTest = undefined

terminateInstanceInAutoScalingGroupTest :: TerminateInstanceInAutoScalingGroup -> TestTree
terminateInstanceInAutoScalingGroupTest = undefined

describeAccountLimitsTest :: DescribeAccountLimits -> TestTree
describeAccountLimitsTest = undefined

attachLoadBalancersTest :: AttachLoadBalancers -> TestTree
attachLoadBalancersTest = undefined

putScheduledUpdateGroupActionTest :: PutScheduledUpdateGroupAction -> TestTree
putScheduledUpdateGroupActionTest = undefined

describePoliciesTest :: DescribePolicies -> TestTree
describePoliciesTest = undefined

describeNotificationConfigurationsTest :: DescribeNotificationConfigurations -> TestTree
describeNotificationConfigurationsTest = undefined

describeLaunchConfigurationsTest :: DescribeLaunchConfigurations -> TestTree
describeLaunchConfigurationsTest = undefined

describeLifecycleHookTypesTest :: DescribeLifecycleHookTypes -> TestTree
describeLifecycleHookTypesTest = undefined

describeScalingActivitiesTest :: DescribeScalingActivities -> TestTree
describeScalingActivitiesTest = undefined

createAutoScalingGroupTest :: CreateAutoScalingGroup -> TestTree
createAutoScalingGroupTest = undefined

createLaunchConfigurationTest :: CreateLaunchConfiguration -> TestTree
createLaunchConfigurationTest = undefined

describeAdjustmentTypesTest :: DescribeAdjustmentTypes -> TestTree
describeAdjustmentTypesTest = undefined

-- Responses

describeMetricCollectionTypesResponseTest :: DescribeMetricCollectionTypesResponse -> TestTree
describeMetricCollectionTypesResponseTest = resp
    "DescribeMetricCollectionTypes"
    "fixture/AutoScaling/DescribeMetricCollectionTypesResponse"
    (Proxy :: Proxy DescribeMetricCollectionTypes)

putScalingPolicyResponseTest :: PutScalingPolicyResponse -> TestTree
putScalingPolicyResponseTest = resp
    "PutScalingPolicy"
    "fixture/AutoScaling/PutScalingPolicyResponse"
    (Proxy :: Proxy PutScalingPolicy)

deleteNotificationConfigurationResponseTest :: DeleteNotificationConfigurationResponse -> TestTree
deleteNotificationConfigurationResponseTest = resp
    "DeleteNotificationConfiguration"
    "fixture/AutoScaling/DeleteNotificationConfigurationResponse"
    (Proxy :: Proxy DeleteNotificationConfiguration)

describeTagsResponseTest :: DescribeTagsResponse -> TestTree
describeTagsResponseTest = resp
    "DescribeTags"
    "fixture/AutoScaling/DescribeTagsResponse"
    (Proxy :: Proxy DescribeTags)

deleteLaunchConfigurationResponseTest :: DeleteLaunchConfigurationResponse -> TestTree
deleteLaunchConfigurationResponseTest = resp
    "DeleteLaunchConfiguration"
    "fixture/AutoScaling/DeleteLaunchConfigurationResponse"
    (Proxy :: Proxy DeleteLaunchConfiguration)

describeLoadBalancersResponseTest :: DescribeLoadBalancersResponse -> TestTree
describeLoadBalancersResponseTest = resp
    "DescribeLoadBalancers"
    "fixture/AutoScaling/DescribeLoadBalancersResponse"
    (Proxy :: Proxy DescribeLoadBalancers)

putNotificationConfigurationResponseTest :: PutNotificationConfigurationResponse -> TestTree
putNotificationConfigurationResponseTest = resp
    "PutNotificationConfiguration"
    "fixture/AutoScaling/PutNotificationConfigurationResponse"
    (Proxy :: Proxy PutNotificationConfiguration)

setInstanceHealthResponseTest :: SetInstanceHealthResponse -> TestTree
setInstanceHealthResponseTest = resp
    "SetInstanceHealth"
    "fixture/AutoScaling/SetInstanceHealthResponse"
    (Proxy :: Proxy SetInstanceHealth)

enterStandbyResponseTest :: EnterStandbyResponse -> TestTree
enterStandbyResponseTest = resp
    "EnterStandby"
    "fixture/AutoScaling/EnterStandbyResponse"
    (Proxy :: Proxy EnterStandby)

suspendProcessesResponseTest :: SuspendProcessesResponse -> TestTree
suspendProcessesResponseTest = resp
    "SuspendProcesses"
    "fixture/AutoScaling/SuspendProcessesResponse"
    (Proxy :: Proxy SuspendProcesses)

exitStandbyResponseTest :: ExitStandbyResponse -> TestTree
exitStandbyResponseTest = resp
    "ExitStandby"
    "fixture/AutoScaling/ExitStandbyResponse"
    (Proxy :: Proxy ExitStandby)

describeTerminationPolicyTypesResponseTest :: DescribeTerminationPolicyTypesResponse -> TestTree
describeTerminationPolicyTypesResponseTest = resp
    "DescribeTerminationPolicyTypes"
    "fixture/AutoScaling/DescribeTerminationPolicyTypesResponse"
    (Proxy :: Proxy DescribeTerminationPolicyTypes)

describeAutoScalingInstancesResponseTest :: DescribeAutoScalingInstancesResponse -> TestTree
describeAutoScalingInstancesResponseTest = resp
    "DescribeAutoScalingInstances"
    "fixture/AutoScaling/DescribeAutoScalingInstancesResponse"
    (Proxy :: Proxy DescribeAutoScalingInstances)

detachInstancesResponseTest :: DetachInstancesResponse -> TestTree
detachInstancesResponseTest = resp
    "DetachInstances"
    "fixture/AutoScaling/DetachInstancesResponse"
    (Proxy :: Proxy DetachInstances)

disableMetricsCollectionResponseTest :: DisableMetricsCollectionResponse -> TestTree
disableMetricsCollectionResponseTest = resp
    "DisableMetricsCollection"
    "fixture/AutoScaling/DisableMetricsCollectionResponse"
    (Proxy :: Proxy DisableMetricsCollection)

recordLifecycleActionHeartbeatResponseTest :: RecordLifecycleActionHeartbeatResponse -> TestTree
recordLifecycleActionHeartbeatResponseTest = resp
    "RecordLifecycleActionHeartbeat"
    "fixture/AutoScaling/RecordLifecycleActionHeartbeatResponse"
    (Proxy :: Proxy RecordLifecycleActionHeartbeat)

deleteTagsResponseTest :: DeleteTagsResponse -> TestTree
deleteTagsResponseTest = resp
    "DeleteTags"
    "fixture/AutoScaling/DeleteTagsResponse"
    (Proxy :: Proxy DeleteTags)

describeScalingProcessTypesResponseTest :: DescribeScalingProcessTypesResponse -> TestTree
describeScalingProcessTypesResponseTest = resp
    "DescribeScalingProcessTypes"
    "fixture/AutoScaling/DescribeScalingProcessTypesResponse"
    (Proxy :: Proxy DescribeScalingProcessTypes)

enableMetricsCollectionResponseTest :: EnableMetricsCollectionResponse -> TestTree
enableMetricsCollectionResponseTest = resp
    "EnableMetricsCollection"
    "fixture/AutoScaling/EnableMetricsCollectionResponse"
    (Proxy :: Proxy EnableMetricsCollection)

describeLifecycleHooksResponseTest :: DescribeLifecycleHooksResponse -> TestTree
describeLifecycleHooksResponseTest = resp
    "DescribeLifecycleHooks"
    "fixture/AutoScaling/DescribeLifecycleHooksResponse"
    (Proxy :: Proxy DescribeLifecycleHooks)

describeAutoScalingGroupsResponseTest :: DescribeAutoScalingGroupsResponse -> TestTree
describeAutoScalingGroupsResponseTest = resp
    "DescribeAutoScalingGroups"
    "fixture/AutoScaling/DescribeAutoScalingGroupsResponse"
    (Proxy :: Proxy DescribeAutoScalingGroups)

setDesiredCapacityResponseTest :: SetDesiredCapacityResponse -> TestTree
setDesiredCapacityResponseTest = resp
    "SetDesiredCapacity"
    "fixture/AutoScaling/SetDesiredCapacityResponse"
    (Proxy :: Proxy SetDesiredCapacity)

detachLoadBalancersResponseTest :: DetachLoadBalancersResponse -> TestTree
detachLoadBalancersResponseTest = resp
    "DetachLoadBalancers"
    "fixture/AutoScaling/DetachLoadBalancersResponse"
    (Proxy :: Proxy DetachLoadBalancers)

deleteScheduledActionResponseTest :: DeleteScheduledActionResponse -> TestTree
deleteScheduledActionResponseTest = resp
    "DeleteScheduledAction"
    "fixture/AutoScaling/DeleteScheduledActionResponse"
    (Proxy :: Proxy DeleteScheduledAction)

createOrUpdateTagsResponseTest :: CreateOrUpdateTagsResponse -> TestTree
createOrUpdateTagsResponseTest = resp
    "CreateOrUpdateTags"
    "fixture/AutoScaling/CreateOrUpdateTagsResponse"
    (Proxy :: Proxy CreateOrUpdateTags)

deletePolicyResponseTest :: DeletePolicyResponse -> TestTree
deletePolicyResponseTest = resp
    "DeletePolicy"
    "fixture/AutoScaling/DeletePolicyResponse"
    (Proxy :: Proxy DeletePolicy)

describeAutoScalingNotificationTypesResponseTest :: DescribeAutoScalingNotificationTypesResponse -> TestTree
describeAutoScalingNotificationTypesResponseTest = resp
    "DescribeAutoScalingNotificationTypes"
    "fixture/AutoScaling/DescribeAutoScalingNotificationTypesResponse"
    (Proxy :: Proxy DescribeAutoScalingNotificationTypes)

completeLifecycleActionResponseTest :: CompleteLifecycleActionResponse -> TestTree
completeLifecycleActionResponseTest = resp
    "CompleteLifecycleAction"
    "fixture/AutoScaling/CompleteLifecycleActionResponse"
    (Proxy :: Proxy CompleteLifecycleAction)

attachInstancesResponseTest :: AttachInstancesResponse -> TestTree
attachInstancesResponseTest = resp
    "AttachInstances"
    "fixture/AutoScaling/AttachInstancesResponse"
    (Proxy :: Proxy AttachInstances)

describeScheduledActionsResponseTest :: DescribeScheduledActionsResponse -> TestTree
describeScheduledActionsResponseTest = resp
    "DescribeScheduledActions"
    "fixture/AutoScaling/DescribeScheduledActionsResponse"
    (Proxy :: Proxy DescribeScheduledActions)

deleteAutoScalingGroupResponseTest :: DeleteAutoScalingGroupResponse -> TestTree
deleteAutoScalingGroupResponseTest = resp
    "DeleteAutoScalingGroup"
    "fixture/AutoScaling/DeleteAutoScalingGroupResponse"
    (Proxy :: Proxy DeleteAutoScalingGroup)

putLifecycleHookResponseTest :: PutLifecycleHookResponse -> TestTree
putLifecycleHookResponseTest = resp
    "PutLifecycleHook"
    "fixture/AutoScaling/PutLifecycleHookResponse"
    (Proxy :: Proxy PutLifecycleHook)

updateAutoScalingGroupResponseTest :: UpdateAutoScalingGroupResponse -> TestTree
updateAutoScalingGroupResponseTest = resp
    "UpdateAutoScalingGroup"
    "fixture/AutoScaling/UpdateAutoScalingGroupResponse"
    (Proxy :: Proxy UpdateAutoScalingGroup)

deleteLifecycleHookResponseTest :: DeleteLifecycleHookResponse -> TestTree
deleteLifecycleHookResponseTest = resp
    "DeleteLifecycleHook"
    "fixture/AutoScaling/DeleteLifecycleHookResponse"
    (Proxy :: Proxy DeleteLifecycleHook)

resumeProcessesResponseTest :: ResumeProcessesResponse -> TestTree
resumeProcessesResponseTest = resp
    "ResumeProcesses"
    "fixture/AutoScaling/ResumeProcessesResponse"
    (Proxy :: Proxy ResumeProcesses)

executePolicyResponseTest :: ExecutePolicyResponse -> TestTree
executePolicyResponseTest = resp
    "ExecutePolicy"
    "fixture/AutoScaling/ExecutePolicyResponse"
    (Proxy :: Proxy ExecutePolicy)

terminateInstanceInAutoScalingGroupResponseTest :: TerminateInstanceInAutoScalingGroupResponse -> TestTree
terminateInstanceInAutoScalingGroupResponseTest = resp
    "TerminateInstanceInAutoScalingGroup"
    "fixture/AutoScaling/TerminateInstanceInAutoScalingGroupResponse"
    (Proxy :: Proxy TerminateInstanceInAutoScalingGroup)

describeAccountLimitsResponseTest :: DescribeAccountLimitsResponse -> TestTree
describeAccountLimitsResponseTest = resp
    "DescribeAccountLimits"
    "fixture/AutoScaling/DescribeAccountLimitsResponse"
    (Proxy :: Proxy DescribeAccountLimits)

attachLoadBalancersResponseTest :: AttachLoadBalancersResponse -> TestTree
attachLoadBalancersResponseTest = resp
    "AttachLoadBalancers"
    "fixture/AutoScaling/AttachLoadBalancersResponse"
    (Proxy :: Proxy AttachLoadBalancers)

putScheduledUpdateGroupActionResponseTest :: PutScheduledUpdateGroupActionResponse -> TestTree
putScheduledUpdateGroupActionResponseTest = resp
    "PutScheduledUpdateGroupAction"
    "fixture/AutoScaling/PutScheduledUpdateGroupActionResponse"
    (Proxy :: Proxy PutScheduledUpdateGroupAction)

describePoliciesResponseTest :: DescribePoliciesResponse -> TestTree
describePoliciesResponseTest = resp
    "DescribePolicies"
    "fixture/AutoScaling/DescribePoliciesResponse"
    (Proxy :: Proxy DescribePolicies)

describeNotificationConfigurationsResponseTest :: DescribeNotificationConfigurationsResponse -> TestTree
describeNotificationConfigurationsResponseTest = resp
    "DescribeNotificationConfigurations"
    "fixture/AutoScaling/DescribeNotificationConfigurationsResponse"
    (Proxy :: Proxy DescribeNotificationConfigurations)

describeLaunchConfigurationsResponseTest :: DescribeLaunchConfigurationsResponse -> TestTree
describeLaunchConfigurationsResponseTest = resp
    "DescribeLaunchConfigurations"
    "fixture/AutoScaling/DescribeLaunchConfigurationsResponse"
    (Proxy :: Proxy DescribeLaunchConfigurations)

describeLifecycleHookTypesResponseTest :: DescribeLifecycleHookTypesResponse -> TestTree
describeLifecycleHookTypesResponseTest = resp
    "DescribeLifecycleHookTypes"
    "fixture/AutoScaling/DescribeLifecycleHookTypesResponse"
    (Proxy :: Proxy DescribeLifecycleHookTypes)

describeScalingActivitiesResponseTest :: DescribeScalingActivitiesResponse -> TestTree
describeScalingActivitiesResponseTest = resp
    "DescribeScalingActivities"
    "fixture/AutoScaling/DescribeScalingActivitiesResponse"
    (Proxy :: Proxy DescribeScalingActivities)

createAutoScalingGroupResponseTest :: CreateAutoScalingGroupResponse -> TestTree
createAutoScalingGroupResponseTest = resp
    "CreateAutoScalingGroup"
    "fixture/AutoScaling/CreateAutoScalingGroupResponse"
    (Proxy :: Proxy CreateAutoScalingGroup)

createLaunchConfigurationResponseTest :: CreateLaunchConfigurationResponse -> TestTree
createLaunchConfigurationResponseTest = resp
    "CreateLaunchConfiguration"
    "fixture/AutoScaling/CreateLaunchConfigurationResponse"
    (Proxy :: Proxy CreateLaunchConfiguration)

describeAdjustmentTypesResponseTest :: DescribeAdjustmentTypesResponse -> TestTree
describeAdjustmentTypesResponseTest = resp
    "DescribeAdjustmentTypes"
    "fixture/AutoScaling/DescribeAdjustmentTypesResponse"
    (Proxy :: Proxy DescribeAdjustmentTypes)
