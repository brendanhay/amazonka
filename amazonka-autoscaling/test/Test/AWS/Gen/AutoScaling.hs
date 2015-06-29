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

import Data.Proxy
import Test.AWS.Fixture
import Test.Tasty
import Network.AWS.AutoScaling

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
    "DescribeMetricCollectionTypesResponse"
    "fixture/DescribeMetricCollectionTypesResponse"
    (Proxy :: Proxy DescribeMetricCollectionTypes)

putScalingPolicyResponseTest :: PutScalingPolicyResponse -> TestTree
putScalingPolicyResponseTest = resp
    "PutScalingPolicyResponse"
    "fixture/PutScalingPolicyResponse"
    (Proxy :: Proxy PutScalingPolicy)

deleteNotificationConfigurationResponseTest :: DeleteNotificationConfigurationResponse -> TestTree
deleteNotificationConfigurationResponseTest = resp
    "DeleteNotificationConfigurationResponse"
    "fixture/DeleteNotificationConfigurationResponse"
    (Proxy :: Proxy DeleteNotificationConfiguration)

describeTagsResponseTest :: DescribeTagsResponse -> TestTree
describeTagsResponseTest = resp
    "DescribeTagsResponse"
    "fixture/DescribeTagsResponse"
    (Proxy :: Proxy DescribeTags)

deleteLaunchConfigurationResponseTest :: DeleteLaunchConfigurationResponse -> TestTree
deleteLaunchConfigurationResponseTest = resp
    "DeleteLaunchConfigurationResponse"
    "fixture/DeleteLaunchConfigurationResponse"
    (Proxy :: Proxy DeleteLaunchConfiguration)

describeLoadBalancersResponseTest :: DescribeLoadBalancersResponse -> TestTree
describeLoadBalancersResponseTest = resp
    "DescribeLoadBalancersResponse"
    "fixture/DescribeLoadBalancersResponse"
    (Proxy :: Proxy DescribeLoadBalancers)

putNotificationConfigurationResponseTest :: PutNotificationConfigurationResponse -> TestTree
putNotificationConfigurationResponseTest = resp
    "PutNotificationConfigurationResponse"
    "fixture/PutNotificationConfigurationResponse"
    (Proxy :: Proxy PutNotificationConfiguration)

setInstanceHealthResponseTest :: SetInstanceHealthResponse -> TestTree
setInstanceHealthResponseTest = resp
    "SetInstanceHealthResponse"
    "fixture/SetInstanceHealthResponse"
    (Proxy :: Proxy SetInstanceHealth)

enterStandbyResponseTest :: EnterStandbyResponse -> TestTree
enterStandbyResponseTest = resp
    "EnterStandbyResponse"
    "fixture/EnterStandbyResponse"
    (Proxy :: Proxy EnterStandby)

suspendProcessesResponseTest :: SuspendProcessesResponse -> TestTree
suspendProcessesResponseTest = resp
    "SuspendProcessesResponse"
    "fixture/SuspendProcessesResponse"
    (Proxy :: Proxy SuspendProcesses)

exitStandbyResponseTest :: ExitStandbyResponse -> TestTree
exitStandbyResponseTest = resp
    "ExitStandbyResponse"
    "fixture/ExitStandbyResponse"
    (Proxy :: Proxy ExitStandby)

describeTerminationPolicyTypesResponseTest :: DescribeTerminationPolicyTypesResponse -> TestTree
describeTerminationPolicyTypesResponseTest = resp
    "DescribeTerminationPolicyTypesResponse"
    "fixture/DescribeTerminationPolicyTypesResponse"
    (Proxy :: Proxy DescribeTerminationPolicyTypes)

describeAutoScalingInstancesResponseTest :: DescribeAutoScalingInstancesResponse -> TestTree
describeAutoScalingInstancesResponseTest = resp
    "DescribeAutoScalingInstancesResponse"
    "fixture/DescribeAutoScalingInstancesResponse"
    (Proxy :: Proxy DescribeAutoScalingInstances)

detachInstancesResponseTest :: DetachInstancesResponse -> TestTree
detachInstancesResponseTest = resp
    "DetachInstancesResponse"
    "fixture/DetachInstancesResponse"
    (Proxy :: Proxy DetachInstances)

disableMetricsCollectionResponseTest :: DisableMetricsCollectionResponse -> TestTree
disableMetricsCollectionResponseTest = resp
    "DisableMetricsCollectionResponse"
    "fixture/DisableMetricsCollectionResponse"
    (Proxy :: Proxy DisableMetricsCollection)

recordLifecycleActionHeartbeatResponseTest :: RecordLifecycleActionHeartbeatResponse -> TestTree
recordLifecycleActionHeartbeatResponseTest = resp
    "RecordLifecycleActionHeartbeatResponse"
    "fixture/RecordLifecycleActionHeartbeatResponse"
    (Proxy :: Proxy RecordLifecycleActionHeartbeat)

deleteTagsResponseTest :: DeleteTagsResponse -> TestTree
deleteTagsResponseTest = resp
    "DeleteTagsResponse"
    "fixture/DeleteTagsResponse"
    (Proxy :: Proxy DeleteTags)

describeScalingProcessTypesResponseTest :: DescribeScalingProcessTypesResponse -> TestTree
describeScalingProcessTypesResponseTest = resp
    "DescribeScalingProcessTypesResponse"
    "fixture/DescribeScalingProcessTypesResponse"
    (Proxy :: Proxy DescribeScalingProcessTypes)

enableMetricsCollectionResponseTest :: EnableMetricsCollectionResponse -> TestTree
enableMetricsCollectionResponseTest = resp
    "EnableMetricsCollectionResponse"
    "fixture/EnableMetricsCollectionResponse"
    (Proxy :: Proxy EnableMetricsCollection)

describeLifecycleHooksResponseTest :: DescribeLifecycleHooksResponse -> TestTree
describeLifecycleHooksResponseTest = resp
    "DescribeLifecycleHooksResponse"
    "fixture/DescribeLifecycleHooksResponse"
    (Proxy :: Proxy DescribeLifecycleHooks)

describeAutoScalingGroupsResponseTest :: DescribeAutoScalingGroupsResponse -> TestTree
describeAutoScalingGroupsResponseTest = resp
    "DescribeAutoScalingGroupsResponse"
    "fixture/DescribeAutoScalingGroupsResponse"
    (Proxy :: Proxy DescribeAutoScalingGroups)

setDesiredCapacityResponseTest :: SetDesiredCapacityResponse -> TestTree
setDesiredCapacityResponseTest = resp
    "SetDesiredCapacityResponse"
    "fixture/SetDesiredCapacityResponse"
    (Proxy :: Proxy SetDesiredCapacity)

detachLoadBalancersResponseTest :: DetachLoadBalancersResponse -> TestTree
detachLoadBalancersResponseTest = resp
    "DetachLoadBalancersResponse"
    "fixture/DetachLoadBalancersResponse"
    (Proxy :: Proxy DetachLoadBalancers)

deleteScheduledActionResponseTest :: DeleteScheduledActionResponse -> TestTree
deleteScheduledActionResponseTest = resp
    "DeleteScheduledActionResponse"
    "fixture/DeleteScheduledActionResponse"
    (Proxy :: Proxy DeleteScheduledAction)

createOrUpdateTagsResponseTest :: CreateOrUpdateTagsResponse -> TestTree
createOrUpdateTagsResponseTest = resp
    "CreateOrUpdateTagsResponse"
    "fixture/CreateOrUpdateTagsResponse"
    (Proxy :: Proxy CreateOrUpdateTags)

deletePolicyResponseTest :: DeletePolicyResponse -> TestTree
deletePolicyResponseTest = resp
    "DeletePolicyResponse"
    "fixture/DeletePolicyResponse"
    (Proxy :: Proxy DeletePolicy)

describeAutoScalingNotificationTypesResponseTest :: DescribeAutoScalingNotificationTypesResponse -> TestTree
describeAutoScalingNotificationTypesResponseTest = resp
    "DescribeAutoScalingNotificationTypesResponse"
    "fixture/DescribeAutoScalingNotificationTypesResponse"
    (Proxy :: Proxy DescribeAutoScalingNotificationTypes)

completeLifecycleActionResponseTest :: CompleteLifecycleActionResponse -> TestTree
completeLifecycleActionResponseTest = resp
    "CompleteLifecycleActionResponse"
    "fixture/CompleteLifecycleActionResponse"
    (Proxy :: Proxy CompleteLifecycleAction)

attachInstancesResponseTest :: AttachInstancesResponse -> TestTree
attachInstancesResponseTest = resp
    "AttachInstancesResponse"
    "fixture/AttachInstancesResponse"
    (Proxy :: Proxy AttachInstances)

describeScheduledActionsResponseTest :: DescribeScheduledActionsResponse -> TestTree
describeScheduledActionsResponseTest = resp
    "DescribeScheduledActionsResponse"
    "fixture/DescribeScheduledActionsResponse"
    (Proxy :: Proxy DescribeScheduledActions)

deleteAutoScalingGroupResponseTest :: DeleteAutoScalingGroupResponse -> TestTree
deleteAutoScalingGroupResponseTest = resp
    "DeleteAutoScalingGroupResponse"
    "fixture/DeleteAutoScalingGroupResponse"
    (Proxy :: Proxy DeleteAutoScalingGroup)

putLifecycleHookResponseTest :: PutLifecycleHookResponse -> TestTree
putLifecycleHookResponseTest = resp
    "PutLifecycleHookResponse"
    "fixture/PutLifecycleHookResponse"
    (Proxy :: Proxy PutLifecycleHook)

updateAutoScalingGroupResponseTest :: UpdateAutoScalingGroupResponse -> TestTree
updateAutoScalingGroupResponseTest = resp
    "UpdateAutoScalingGroupResponse"
    "fixture/UpdateAutoScalingGroupResponse"
    (Proxy :: Proxy UpdateAutoScalingGroup)

deleteLifecycleHookResponseTest :: DeleteLifecycleHookResponse -> TestTree
deleteLifecycleHookResponseTest = resp
    "DeleteLifecycleHookResponse"
    "fixture/DeleteLifecycleHookResponse"
    (Proxy :: Proxy DeleteLifecycleHook)

resumeProcessesResponseTest :: ResumeProcessesResponse -> TestTree
resumeProcessesResponseTest = resp
    "ResumeProcessesResponse"
    "fixture/ResumeProcessesResponse"
    (Proxy :: Proxy ResumeProcesses)

executePolicyResponseTest :: ExecutePolicyResponse -> TestTree
executePolicyResponseTest = resp
    "ExecutePolicyResponse"
    "fixture/ExecutePolicyResponse"
    (Proxy :: Proxy ExecutePolicy)

terminateInstanceInAutoScalingGroupResponseTest :: TerminateInstanceInAutoScalingGroupResponse -> TestTree
terminateInstanceInAutoScalingGroupResponseTest = resp
    "TerminateInstanceInAutoScalingGroupResponse"
    "fixture/TerminateInstanceInAutoScalingGroupResponse"
    (Proxy :: Proxy TerminateInstanceInAutoScalingGroup)

describeAccountLimitsResponseTest :: DescribeAccountLimitsResponse -> TestTree
describeAccountLimitsResponseTest = resp
    "DescribeAccountLimitsResponse"
    "fixture/DescribeAccountLimitsResponse"
    (Proxy :: Proxy DescribeAccountLimits)

attachLoadBalancersResponseTest :: AttachLoadBalancersResponse -> TestTree
attachLoadBalancersResponseTest = resp
    "AttachLoadBalancersResponse"
    "fixture/AttachLoadBalancersResponse"
    (Proxy :: Proxy AttachLoadBalancers)

putScheduledUpdateGroupActionResponseTest :: PutScheduledUpdateGroupActionResponse -> TestTree
putScheduledUpdateGroupActionResponseTest = resp
    "PutScheduledUpdateGroupActionResponse"
    "fixture/PutScheduledUpdateGroupActionResponse"
    (Proxy :: Proxy PutScheduledUpdateGroupAction)

describePoliciesResponseTest :: DescribePoliciesResponse -> TestTree
describePoliciesResponseTest = resp
    "DescribePoliciesResponse"
    "fixture/DescribePoliciesResponse"
    (Proxy :: Proxy DescribePolicies)

describeNotificationConfigurationsResponseTest :: DescribeNotificationConfigurationsResponse -> TestTree
describeNotificationConfigurationsResponseTest = resp
    "DescribeNotificationConfigurationsResponse"
    "fixture/DescribeNotificationConfigurationsResponse"
    (Proxy :: Proxy DescribeNotificationConfigurations)

describeLaunchConfigurationsResponseTest :: DescribeLaunchConfigurationsResponse -> TestTree
describeLaunchConfigurationsResponseTest = resp
    "DescribeLaunchConfigurationsResponse"
    "fixture/DescribeLaunchConfigurationsResponse"
    (Proxy :: Proxy DescribeLaunchConfigurations)

describeLifecycleHookTypesResponseTest :: DescribeLifecycleHookTypesResponse -> TestTree
describeLifecycleHookTypesResponseTest = resp
    "DescribeLifecycleHookTypesResponse"
    "fixture/DescribeLifecycleHookTypesResponse"
    (Proxy :: Proxy DescribeLifecycleHookTypes)

describeScalingActivitiesResponseTest :: DescribeScalingActivitiesResponse -> TestTree
describeScalingActivitiesResponseTest = resp
    "DescribeScalingActivitiesResponse"
    "fixture/DescribeScalingActivitiesResponse"
    (Proxy :: Proxy DescribeScalingActivities)

createAutoScalingGroupResponseTest :: CreateAutoScalingGroupResponse -> TestTree
createAutoScalingGroupResponseTest = resp
    "CreateAutoScalingGroupResponse"
    "fixture/CreateAutoScalingGroupResponse"
    (Proxy :: Proxy CreateAutoScalingGroup)

createLaunchConfigurationResponseTest :: CreateLaunchConfigurationResponse -> TestTree
createLaunchConfigurationResponseTest = resp
    "CreateLaunchConfigurationResponse"
    "fixture/CreateLaunchConfigurationResponse"
    (Proxy :: Proxy CreateLaunchConfiguration)

describeAdjustmentTypesResponseTest :: DescribeAdjustmentTypesResponse -> TestTree
describeAdjustmentTypesResponseTest = resp
    "DescribeAdjustmentTypesResponse"
    "fixture/DescribeAdjustmentTypesResponse"
    (Proxy :: Proxy DescribeAdjustmentTypes)
