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
--         [ attachInstancesTest $
--             attachInstances
--
--         , attachLoadBalancersTest $
--             attachLoadBalancers
--
--         , completeLifecycleActionTest $
--             completeLifecycleAction
--
--         , createAutoScalingGroupTest $
--             createAutoScalingGroup
--
--         , createLaunchConfigurationTest $
--             createLaunchConfiguration
--
--         , createOrUpdateTagsTest $
--             createOrUpdateTags
--
--         , deleteAutoScalingGroupTest $
--             deleteAutoScalingGroup
--
--         , deleteLaunchConfigurationTest $
--             deleteLaunchConfiguration
--
--         , deleteLifecycleHookTest $
--             deleteLifecycleHook
--
--         , deleteNotificationConfigurationTest $
--             deleteNotificationConfiguration
--
--         , deletePolicyTest $
--             deletePolicy
--
--         , deleteScheduledActionTest $
--             deleteScheduledAction
--
--         , deleteTagsTest $
--             deleteTags
--
--         , describeAccountLimitsTest $
--             describeAccountLimits
--
--         , describeAdjustmentTypesTest $
--             describeAdjustmentTypes
--
--         , describeAutoScalingGroupsTest $
--             describeAutoScalingGroups
--
--         , describeAutoScalingInstancesTest $
--             describeAutoScalingInstances
--
--         , describeAutoScalingNotificationTypesTest $
--             describeAutoScalingNotificationTypes
--
--         , describeLaunchConfigurationsTest $
--             describeLaunchConfigurations
--
--         , describeLifecycleHookTypesTest $
--             describeLifecycleHookTypes
--
--         , describeLifecycleHooksTest $
--             describeLifecycleHooks
--
--         , describeLoadBalancersTest $
--             describeLoadBalancers
--
--         , describeMetricCollectionTypesTest $
--             describeMetricCollectionTypes
--
--         , describeNotificationConfigurationsTest $
--             describeNotificationConfigurations
--
--         , describePoliciesTest $
--             describePolicies
--
--         , describeScalingActivitiesTest $
--             describeScalingActivities
--
--         , describeScalingProcessTypesTest $
--             describeScalingProcessTypes
--
--         , describeScheduledActionsTest $
--             describeScheduledActions
--
--         , describeTagsTest $
--             describeTags
--
--         , describeTerminationPolicyTypesTest $
--             describeTerminationPolicyTypes
--
--         , detachInstancesTest $
--             detachInstances
--
--         , detachLoadBalancersTest $
--             detachLoadBalancers
--
--         , disableMetricsCollectionTest $
--             disableMetricsCollection
--
--         , enableMetricsCollectionTest $
--             enableMetricsCollection
--
--         , enterStandbyTest $
--             enterStandby
--
--         , executePolicyTest $
--             executePolicy
--
--         , exitStandbyTest $
--             exitStandby
--
--         , putLifecycleHookTest $
--             putLifecycleHook
--
--         , putNotificationConfigurationTest $
--             putNotificationConfiguration
--
--         , putScalingPolicyTest $
--             putScalingPolicy
--
--         , putScheduledUpdateGroupActionTest $
--             putScheduledUpdateGroupAction
--
--         , recordLifecycleActionHeartbeatTest $
--             recordLifecycleActionHeartbeat
--
--         , resumeProcessesTest $
--             resumeProcesses
--
--         , setDesiredCapacityTest $
--             setDesiredCapacity
--
--         , setInstanceHealthTest $
--             setInstanceHealth
--
--         , suspendProcessesTest $
--             suspendProcesses
--
--         , terminateInstanceInAutoScalingGroupTest $
--             terminateInstanceInAutoScalingGroup
--
--         , updateAutoScalingGroupTest $
--             updateAutoScalingGroup
--
--           ]

--     , testGroup "response"
--         [ attachInstancesResponseTest $
--             attachInstancesResponse
--
--         , attachLoadBalancersResponseTest $
--             attachLoadBalancersResponse
--
--         , completeLifecycleActionResponseTest $
--             completeLifecycleActionResponse
--
--         , createAutoScalingGroupResponseTest $
--             createAutoScalingGroupResponse
--
--         , createLaunchConfigurationResponseTest $
--             createLaunchConfigurationResponse
--
--         , createOrUpdateTagsResponseTest $
--             createOrUpdateTagsResponse
--
--         , deleteAutoScalingGroupResponseTest $
--             deleteAutoScalingGroupResponse
--
--         , deleteLaunchConfigurationResponseTest $
--             deleteLaunchConfigurationResponse
--
--         , deleteLifecycleHookResponseTest $
--             deleteLifecycleHookResponse
--
--         , deleteNotificationConfigurationResponseTest $
--             deleteNotificationConfigurationResponse
--
--         , deletePolicyResponseTest $
--             deletePolicyResponse
--
--         , deleteScheduledActionResponseTest $
--             deleteScheduledActionResponse
--
--         , deleteTagsResponseTest $
--             deleteTagsResponse
--
--         , describeAccountLimitsResponseTest $
--             describeAccountLimitsResponse
--
--         , describeAdjustmentTypesResponseTest $
--             describeAdjustmentTypesResponse
--
--         , describeAutoScalingGroupsResponseTest $
--             describeAutoScalingGroupsResponse
--
--         , describeAutoScalingInstancesResponseTest $
--             describeAutoScalingInstancesResponse
--
--         , describeAutoScalingNotificationTypesResponseTest $
--             describeAutoScalingNotificationTypesResponse
--
--         , describeLaunchConfigurationsResponseTest $
--             describeLaunchConfigurationsResponse
--
--         , describeLifecycleHookTypesResponseTest $
--             describeLifecycleHookTypesResponse
--
--         , describeLifecycleHooksResponseTest $
--             describeLifecycleHooksResponse
--
--         , describeLoadBalancersResponseTest $
--             describeLoadBalancersResponse
--
--         , describeMetricCollectionTypesResponseTest $
--             describeMetricCollectionTypesResponse
--
--         , describeNotificationConfigurationsResponseTest $
--             describeNotificationConfigurationsResponse
--
--         , describePoliciesResponseTest $
--             describePoliciesResponse
--
--         , describeScalingActivitiesResponseTest $
--             describeScalingActivitiesResponse
--
--         , describeScalingProcessTypesResponseTest $
--             describeScalingProcessTypesResponse
--
--         , describeScheduledActionsResponseTest $
--             describeScheduledActionsResponse
--
--         , describeTagsResponseTest $
--             describeTagsResponse
--
--         , describeTerminationPolicyTypesResponseTest $
--             describeTerminationPolicyTypesResponse
--
--         , detachInstancesResponseTest $
--             detachInstancesResponse
--
--         , detachLoadBalancersResponseTest $
--             detachLoadBalancersResponse
--
--         , disableMetricsCollectionResponseTest $
--             disableMetricsCollectionResponse
--
--         , enableMetricsCollectionResponseTest $
--             enableMetricsCollectionResponse
--
--         , enterStandbyResponseTest $
--             enterStandbyResponse
--
--         , executePolicyResponseTest $
--             executePolicyResponse
--
--         , exitStandbyResponseTest $
--             exitStandbyResponse
--
--         , putLifecycleHookResponseTest $
--             putLifecycleHookResponse
--
--         , putNotificationConfigurationResponseTest $
--             putNotificationConfigurationResponse
--
--         , putScalingPolicyResponseTest $
--             putScalingPolicyResponse
--
--         , putScheduledUpdateGroupActionResponseTest $
--             putScheduledUpdateGroupActionResponse
--
--         , recordLifecycleActionHeartbeatResponseTest $
--             recordLifecycleActionHeartbeatResponse
--
--         , resumeProcessesResponseTest $
--             resumeProcessesResponse
--
--         , setDesiredCapacityResponseTest $
--             setDesiredCapacityResponse
--
--         , setInstanceHealthResponseTest $
--             setInstanceHealthResponse
--
--         , suspendProcessesResponseTest $
--             suspendProcessesResponse
--
--         , terminateInstanceInAutoScalingGroupResponseTest $
--             terminateInstanceInAutoScalingGroupResponse
--
--         , updateAutoScalingGroupResponseTest $
--             updateAutoScalingGroupResponse
--
--           ]
--     ]

-- Requests

attachInstancesTest :: AttachInstances -> TestTree
attachInstancesTest = undefined

attachLoadBalancersTest :: AttachLoadBalancers -> TestTree
attachLoadBalancersTest = undefined

completeLifecycleActionTest :: CompleteLifecycleAction -> TestTree
completeLifecycleActionTest = undefined

createAutoScalingGroupTest :: CreateAutoScalingGroup -> TestTree
createAutoScalingGroupTest = undefined

createLaunchConfigurationTest :: CreateLaunchConfiguration -> TestTree
createLaunchConfigurationTest = undefined

createOrUpdateTagsTest :: CreateOrUpdateTags -> TestTree
createOrUpdateTagsTest = undefined

deleteAutoScalingGroupTest :: DeleteAutoScalingGroup -> TestTree
deleteAutoScalingGroupTest = undefined

deleteLaunchConfigurationTest :: DeleteLaunchConfiguration -> TestTree
deleteLaunchConfigurationTest = undefined

deleteLifecycleHookTest :: DeleteLifecycleHook -> TestTree
deleteLifecycleHookTest = undefined

deleteNotificationConfigurationTest :: DeleteNotificationConfiguration -> TestTree
deleteNotificationConfigurationTest = undefined

deletePolicyTest :: DeletePolicy -> TestTree
deletePolicyTest = undefined

deleteScheduledActionTest :: DeleteScheduledAction -> TestTree
deleteScheduledActionTest = undefined

deleteTagsTest :: DeleteTags -> TestTree
deleteTagsTest = undefined

describeAccountLimitsTest :: DescribeAccountLimits -> TestTree
describeAccountLimitsTest = undefined

describeAdjustmentTypesTest :: DescribeAdjustmentTypes -> TestTree
describeAdjustmentTypesTest = undefined

describeAutoScalingGroupsTest :: DescribeAutoScalingGroups -> TestTree
describeAutoScalingGroupsTest = undefined

describeAutoScalingInstancesTest :: DescribeAutoScalingInstances -> TestTree
describeAutoScalingInstancesTest = undefined

describeAutoScalingNotificationTypesTest :: DescribeAutoScalingNotificationTypes -> TestTree
describeAutoScalingNotificationTypesTest = undefined

describeLaunchConfigurationsTest :: DescribeLaunchConfigurations -> TestTree
describeLaunchConfigurationsTest = undefined

describeLifecycleHookTypesTest :: DescribeLifecycleHookTypes -> TestTree
describeLifecycleHookTypesTest = undefined

describeLifecycleHooksTest :: DescribeLifecycleHooks -> TestTree
describeLifecycleHooksTest = undefined

describeLoadBalancersTest :: DescribeLoadBalancers -> TestTree
describeLoadBalancersTest = undefined

describeMetricCollectionTypesTest :: DescribeMetricCollectionTypes -> TestTree
describeMetricCollectionTypesTest = undefined

describeNotificationConfigurationsTest :: DescribeNotificationConfigurations -> TestTree
describeNotificationConfigurationsTest = undefined

describePoliciesTest :: DescribePolicies -> TestTree
describePoliciesTest = undefined

describeScalingActivitiesTest :: DescribeScalingActivities -> TestTree
describeScalingActivitiesTest = undefined

describeScalingProcessTypesTest :: DescribeScalingProcessTypes -> TestTree
describeScalingProcessTypesTest = undefined

describeScheduledActionsTest :: DescribeScheduledActions -> TestTree
describeScheduledActionsTest = undefined

describeTagsTest :: DescribeTags -> TestTree
describeTagsTest = undefined

describeTerminationPolicyTypesTest :: DescribeTerminationPolicyTypes -> TestTree
describeTerminationPolicyTypesTest = undefined

detachInstancesTest :: DetachInstances -> TestTree
detachInstancesTest = undefined

detachLoadBalancersTest :: DetachLoadBalancers -> TestTree
detachLoadBalancersTest = undefined

disableMetricsCollectionTest :: DisableMetricsCollection -> TestTree
disableMetricsCollectionTest = undefined

enableMetricsCollectionTest :: EnableMetricsCollection -> TestTree
enableMetricsCollectionTest = undefined

enterStandbyTest :: EnterStandby -> TestTree
enterStandbyTest = undefined

executePolicyTest :: ExecutePolicy -> TestTree
executePolicyTest = undefined

exitStandbyTest :: ExitStandby -> TestTree
exitStandbyTest = undefined

putLifecycleHookTest :: PutLifecycleHook -> TestTree
putLifecycleHookTest = undefined

putNotificationConfigurationTest :: PutNotificationConfiguration -> TestTree
putNotificationConfigurationTest = undefined

putScalingPolicyTest :: PutScalingPolicy -> TestTree
putScalingPolicyTest = undefined

putScheduledUpdateGroupActionTest :: PutScheduledUpdateGroupAction -> TestTree
putScheduledUpdateGroupActionTest = undefined

recordLifecycleActionHeartbeatTest :: RecordLifecycleActionHeartbeat -> TestTree
recordLifecycleActionHeartbeatTest = undefined

resumeProcessesTest :: ResumeProcesses -> TestTree
resumeProcessesTest = undefined

setDesiredCapacityTest :: SetDesiredCapacity -> TestTree
setDesiredCapacityTest = undefined

setInstanceHealthTest :: SetInstanceHealth -> TestTree
setInstanceHealthTest = undefined

suspendProcessesTest :: SuspendProcesses -> TestTree
suspendProcessesTest = undefined

terminateInstanceInAutoScalingGroupTest :: TerminateInstanceInAutoScalingGroup -> TestTree
terminateInstanceInAutoScalingGroupTest = undefined

updateAutoScalingGroupTest :: UpdateAutoScalingGroup -> TestTree
updateAutoScalingGroupTest = undefined

-- Responses

attachInstancesResponseTest :: AttachInstancesResponse -> TestTree
attachInstancesResponseTest = resp
    "attachInstancesResponse"
    "fixture/AttachInstancesResponse"
    (Proxy :: Proxy AttachInstances)

attachLoadBalancersResponseTest :: AttachLoadBalancersResponse -> TestTree
attachLoadBalancersResponseTest = resp
    "attachLoadBalancersResponse"
    "fixture/AttachLoadBalancersResponse"
    (Proxy :: Proxy AttachLoadBalancers)

completeLifecycleActionResponseTest :: CompleteLifecycleActionResponse -> TestTree
completeLifecycleActionResponseTest = resp
    "completeLifecycleActionResponse"
    "fixture/CompleteLifecycleActionResponse"
    (Proxy :: Proxy CompleteLifecycleAction)

createAutoScalingGroupResponseTest :: CreateAutoScalingGroupResponse -> TestTree
createAutoScalingGroupResponseTest = resp
    "createAutoScalingGroupResponse"
    "fixture/CreateAutoScalingGroupResponse"
    (Proxy :: Proxy CreateAutoScalingGroup)

createLaunchConfigurationResponseTest :: CreateLaunchConfigurationResponse -> TestTree
createLaunchConfigurationResponseTest = resp
    "createLaunchConfigurationResponse"
    "fixture/CreateLaunchConfigurationResponse"
    (Proxy :: Proxy CreateLaunchConfiguration)

createOrUpdateTagsResponseTest :: CreateOrUpdateTagsResponse -> TestTree
createOrUpdateTagsResponseTest = resp
    "createOrUpdateTagsResponse"
    "fixture/CreateOrUpdateTagsResponse"
    (Proxy :: Proxy CreateOrUpdateTags)

deleteAutoScalingGroupResponseTest :: DeleteAutoScalingGroupResponse -> TestTree
deleteAutoScalingGroupResponseTest = resp
    "deleteAutoScalingGroupResponse"
    "fixture/DeleteAutoScalingGroupResponse"
    (Proxy :: Proxy DeleteAutoScalingGroup)

deleteLaunchConfigurationResponseTest :: DeleteLaunchConfigurationResponse -> TestTree
deleteLaunchConfigurationResponseTest = resp
    "deleteLaunchConfigurationResponse"
    "fixture/DeleteLaunchConfigurationResponse"
    (Proxy :: Proxy DeleteLaunchConfiguration)

deleteLifecycleHookResponseTest :: DeleteLifecycleHookResponse -> TestTree
deleteLifecycleHookResponseTest = resp
    "deleteLifecycleHookResponse"
    "fixture/DeleteLifecycleHookResponse"
    (Proxy :: Proxy DeleteLifecycleHook)

deleteNotificationConfigurationResponseTest :: DeleteNotificationConfigurationResponse -> TestTree
deleteNotificationConfigurationResponseTest = resp
    "deleteNotificationConfigurationResponse"
    "fixture/DeleteNotificationConfigurationResponse"
    (Proxy :: Proxy DeleteNotificationConfiguration)

deletePolicyResponseTest :: DeletePolicyResponse -> TestTree
deletePolicyResponseTest = resp
    "deletePolicyResponse"
    "fixture/DeletePolicyResponse"
    (Proxy :: Proxy DeletePolicy)

deleteScheduledActionResponseTest :: DeleteScheduledActionResponse -> TestTree
deleteScheduledActionResponseTest = resp
    "deleteScheduledActionResponse"
    "fixture/DeleteScheduledActionResponse"
    (Proxy :: Proxy DeleteScheduledAction)

deleteTagsResponseTest :: DeleteTagsResponse -> TestTree
deleteTagsResponseTest = resp
    "deleteTagsResponse"
    "fixture/DeleteTagsResponse"
    (Proxy :: Proxy DeleteTags)

describeAccountLimitsResponseTest :: DescribeAccountLimitsResponse -> TestTree
describeAccountLimitsResponseTest = resp
    "describeAccountLimitsResponse"
    "fixture/DescribeAccountLimitsResponse"
    (Proxy :: Proxy DescribeAccountLimits)

describeAdjustmentTypesResponseTest :: DescribeAdjustmentTypesResponse -> TestTree
describeAdjustmentTypesResponseTest = resp
    "describeAdjustmentTypesResponse"
    "fixture/DescribeAdjustmentTypesResponse"
    (Proxy :: Proxy DescribeAdjustmentTypes)

describeAutoScalingGroupsResponseTest :: DescribeAutoScalingGroupsResponse -> TestTree
describeAutoScalingGroupsResponseTest = resp
    "describeAutoScalingGroupsResponse"
    "fixture/DescribeAutoScalingGroupsResponse"
    (Proxy :: Proxy DescribeAutoScalingGroups)

describeAutoScalingInstancesResponseTest :: DescribeAutoScalingInstancesResponse -> TestTree
describeAutoScalingInstancesResponseTest = resp
    "describeAutoScalingInstancesResponse"
    "fixture/DescribeAutoScalingInstancesResponse"
    (Proxy :: Proxy DescribeAutoScalingInstances)

describeAutoScalingNotificationTypesResponseTest :: DescribeAutoScalingNotificationTypesResponse -> TestTree
describeAutoScalingNotificationTypesResponseTest = resp
    "describeAutoScalingNotificationTypesResponse"
    "fixture/DescribeAutoScalingNotificationTypesResponse"
    (Proxy :: Proxy DescribeAutoScalingNotificationTypes)

describeLaunchConfigurationsResponseTest :: DescribeLaunchConfigurationsResponse -> TestTree
describeLaunchConfigurationsResponseTest = resp
    "describeLaunchConfigurationsResponse"
    "fixture/DescribeLaunchConfigurationsResponse"
    (Proxy :: Proxy DescribeLaunchConfigurations)

describeLifecycleHookTypesResponseTest :: DescribeLifecycleHookTypesResponse -> TestTree
describeLifecycleHookTypesResponseTest = resp
    "describeLifecycleHookTypesResponse"
    "fixture/DescribeLifecycleHookTypesResponse"
    (Proxy :: Proxy DescribeLifecycleHookTypes)

describeLifecycleHooksResponseTest :: DescribeLifecycleHooksResponse -> TestTree
describeLifecycleHooksResponseTest = resp
    "describeLifecycleHooksResponse"
    "fixture/DescribeLifecycleHooksResponse"
    (Proxy :: Proxy DescribeLifecycleHooks)

describeLoadBalancersResponseTest :: DescribeLoadBalancersResponse -> TestTree
describeLoadBalancersResponseTest = resp
    "describeLoadBalancersResponse"
    "fixture/DescribeLoadBalancersResponse"
    (Proxy :: Proxy DescribeLoadBalancers)

describeMetricCollectionTypesResponseTest :: DescribeMetricCollectionTypesResponse -> TestTree
describeMetricCollectionTypesResponseTest = resp
    "describeMetricCollectionTypesResponse"
    "fixture/DescribeMetricCollectionTypesResponse"
    (Proxy :: Proxy DescribeMetricCollectionTypes)

describeNotificationConfigurationsResponseTest :: DescribeNotificationConfigurationsResponse -> TestTree
describeNotificationConfigurationsResponseTest = resp
    "describeNotificationConfigurationsResponse"
    "fixture/DescribeNotificationConfigurationsResponse"
    (Proxy :: Proxy DescribeNotificationConfigurations)

describePoliciesResponseTest :: DescribePoliciesResponse -> TestTree
describePoliciesResponseTest = resp
    "describePoliciesResponse"
    "fixture/DescribePoliciesResponse"
    (Proxy :: Proxy DescribePolicies)

describeScalingActivitiesResponseTest :: DescribeScalingActivitiesResponse -> TestTree
describeScalingActivitiesResponseTest = resp
    "describeScalingActivitiesResponse"
    "fixture/DescribeScalingActivitiesResponse"
    (Proxy :: Proxy DescribeScalingActivities)

describeScalingProcessTypesResponseTest :: DescribeScalingProcessTypesResponse -> TestTree
describeScalingProcessTypesResponseTest = resp
    "describeScalingProcessTypesResponse"
    "fixture/DescribeScalingProcessTypesResponse"
    (Proxy :: Proxy DescribeScalingProcessTypes)

describeScheduledActionsResponseTest :: DescribeScheduledActionsResponse -> TestTree
describeScheduledActionsResponseTest = resp
    "describeScheduledActionsResponse"
    "fixture/DescribeScheduledActionsResponse"
    (Proxy :: Proxy DescribeScheduledActions)

describeTagsResponseTest :: DescribeTagsResponse -> TestTree
describeTagsResponseTest = resp
    "describeTagsResponse"
    "fixture/DescribeTagsResponse"
    (Proxy :: Proxy DescribeTags)

describeTerminationPolicyTypesResponseTest :: DescribeTerminationPolicyTypesResponse -> TestTree
describeTerminationPolicyTypesResponseTest = resp
    "describeTerminationPolicyTypesResponse"
    "fixture/DescribeTerminationPolicyTypesResponse"
    (Proxy :: Proxy DescribeTerminationPolicyTypes)

detachInstancesResponseTest :: DetachInstancesResponse -> TestTree
detachInstancesResponseTest = resp
    "detachInstancesResponse"
    "fixture/DetachInstancesResponse"
    (Proxy :: Proxy DetachInstances)

detachLoadBalancersResponseTest :: DetachLoadBalancersResponse -> TestTree
detachLoadBalancersResponseTest = resp
    "detachLoadBalancersResponse"
    "fixture/DetachLoadBalancersResponse"
    (Proxy :: Proxy DetachLoadBalancers)

disableMetricsCollectionResponseTest :: DisableMetricsCollectionResponse -> TestTree
disableMetricsCollectionResponseTest = resp
    "disableMetricsCollectionResponse"
    "fixture/DisableMetricsCollectionResponse"
    (Proxy :: Proxy DisableMetricsCollection)

enableMetricsCollectionResponseTest :: EnableMetricsCollectionResponse -> TestTree
enableMetricsCollectionResponseTest = resp
    "enableMetricsCollectionResponse"
    "fixture/EnableMetricsCollectionResponse"
    (Proxy :: Proxy EnableMetricsCollection)

enterStandbyResponseTest :: EnterStandbyResponse -> TestTree
enterStandbyResponseTest = resp
    "enterStandbyResponse"
    "fixture/EnterStandbyResponse"
    (Proxy :: Proxy EnterStandby)

executePolicyResponseTest :: ExecutePolicyResponse -> TestTree
executePolicyResponseTest = resp
    "executePolicyResponse"
    "fixture/ExecutePolicyResponse"
    (Proxy :: Proxy ExecutePolicy)

exitStandbyResponseTest :: ExitStandbyResponse -> TestTree
exitStandbyResponseTest = resp
    "exitStandbyResponse"
    "fixture/ExitStandbyResponse"
    (Proxy :: Proxy ExitStandby)

putLifecycleHookResponseTest :: PutLifecycleHookResponse -> TestTree
putLifecycleHookResponseTest = resp
    "putLifecycleHookResponse"
    "fixture/PutLifecycleHookResponse"
    (Proxy :: Proxy PutLifecycleHook)

putNotificationConfigurationResponseTest :: PutNotificationConfigurationResponse -> TestTree
putNotificationConfigurationResponseTest = resp
    "putNotificationConfigurationResponse"
    "fixture/PutNotificationConfigurationResponse"
    (Proxy :: Proxy PutNotificationConfiguration)

putScalingPolicyResponseTest :: PutScalingPolicyResponse -> TestTree
putScalingPolicyResponseTest = resp
    "putScalingPolicyResponse"
    "fixture/PutScalingPolicyResponse"
    (Proxy :: Proxy PutScalingPolicy)

putScheduledUpdateGroupActionResponseTest :: PutScheduledUpdateGroupActionResponse -> TestTree
putScheduledUpdateGroupActionResponseTest = resp
    "putScheduledUpdateGroupActionResponse"
    "fixture/PutScheduledUpdateGroupActionResponse"
    (Proxy :: Proxy PutScheduledUpdateGroupAction)

recordLifecycleActionHeartbeatResponseTest :: RecordLifecycleActionHeartbeatResponse -> TestTree
recordLifecycleActionHeartbeatResponseTest = resp
    "recordLifecycleActionHeartbeatResponse"
    "fixture/RecordLifecycleActionHeartbeatResponse"
    (Proxy :: Proxy RecordLifecycleActionHeartbeat)

resumeProcessesResponseTest :: ResumeProcessesResponse -> TestTree
resumeProcessesResponseTest = resp
    "resumeProcessesResponse"
    "fixture/ResumeProcessesResponse"
    (Proxy :: Proxy ResumeProcesses)

setDesiredCapacityResponseTest :: SetDesiredCapacityResponse -> TestTree
setDesiredCapacityResponseTest = resp
    "setDesiredCapacityResponse"
    "fixture/SetDesiredCapacityResponse"
    (Proxy :: Proxy SetDesiredCapacity)

setInstanceHealthResponseTest :: SetInstanceHealthResponse -> TestTree
setInstanceHealthResponseTest = resp
    "setInstanceHealthResponse"
    "fixture/SetInstanceHealthResponse"
    (Proxy :: Proxy SetInstanceHealth)

suspendProcessesResponseTest :: SuspendProcessesResponse -> TestTree
suspendProcessesResponseTest = resp
    "suspendProcessesResponse"
    "fixture/SuspendProcessesResponse"
    (Proxy :: Proxy SuspendProcesses)

terminateInstanceInAutoScalingGroupResponseTest :: TerminateInstanceInAutoScalingGroupResponse -> TestTree
terminateInstanceInAutoScalingGroupResponseTest = resp
    "terminateInstanceInAutoScalingGroupResponse"
    "fixture/TerminateInstanceInAutoScalingGroupResponse"
    (Proxy :: Proxy TerminateInstanceInAutoScalingGroup)

updateAutoScalingGroupResponseTest :: UpdateAutoScalingGroupResponse -> TestTree
updateAutoScalingGroupResponseTest = resp
    "updateAutoScalingGroupResponse"
    "fixture/UpdateAutoScalingGroupResponse"
    (Proxy :: Proxy UpdateAutoScalingGroup)
