{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.AutoScaling
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Test.AWS.Gen.AutoScaling where

import Data.Proxy
import Network.AWS.AutoScaling
import Test.AWS.AutoScaling.Internal
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestDescribeMetricCollectionTypes $
--             describeMetricCollectionTypes
--
--         , requestDescribeLoadBalancers $
--             describeLoadBalancers
--
--         , requestPutNotificationConfiguration $
--             putNotificationConfiguration
--
--         , requestDescribeTags $
--             describeTags
--
--         , requestDeleteNotificationConfiguration $
--             deleteNotificationConfiguration
--
--         , requestPutScalingPolicy $
--             putScalingPolicy
--
--         , requestAttachLoadBalancerTargetGroups $
--             attachLoadBalancerTargetGroups
--
--         , requestDeleteLaunchConfiguration $
--             deleteLaunchConfiguration
--
--         , requestEnterStandby $
--             enterStandby
--
--         , requestSuspendProcesses $
--             suspendProcesses
--
--         , requestSetInstanceHealth $
--             setInstanceHealth
--
--         , requestExitStandby $
--             exitStandby
--
--         , requestDescribeTerminationPolicyTypes $
--             describeTerminationPolicyTypes
--
--         , requestDescribeAutoScalingInstances $
--             describeAutoScalingInstances
--
--         , requestRecordLifecycleActionHeartbeat $
--             recordLifecycleActionHeartbeat
--
--         , requestDisableMetricsCollection $
--             disableMetricsCollection
--
--         , requestDetachInstances $
--             detachInstances
--
--         , requestEnableMetricsCollection $
--             enableMetricsCollection
--
--         , requestDescribeScalingProcessTypes $
--             describeScalingProcessTypes
--
--         , requestDeleteTags $
--             deleteTags
--
--         , requestDetachLoadBalancerTargetGroups $
--             detachLoadBalancerTargetGroups
--
--         , requestDescribeLifecycleHooks $
--             describeLifecycleHooks
--
--         , requestDescribeAutoScalingGroups $
--             describeAutoScalingGroups
--
--         , requestDeleteScheduledAction $
--             deleteScheduledAction
--
--         , requestSetDesiredCapacity $
--             setDesiredCapacity
--
--         , requestDetachLoadBalancers $
--             detachLoadBalancers
--
--         , requestDescribeAutoScalingNotificationTypes $
--             describeAutoScalingNotificationTypes
--
--         , requestDescribeScheduledActions $
--             describeScheduledActions
--
--         , requestCreateOrUpdateTags $
--             createOrUpdateTags
--
--         , requestCompleteLifecycleAction $
--             completeLifecycleAction
--
--         , requestDeletePolicy $
--             deletePolicy
--
--         , requestAttachInstances $
--             attachInstances
--
--         , requestUpdateAutoScalingGroup $
--             updateAutoScalingGroup
--
--         , requestDeleteAutoScalingGroup $
--             deleteAutoScalingGroup
--
--         , requestPutLifecycleHook $
--             putLifecycleHook
--
--         , requestDeleteLifecycleHook $
--             deleteLifecycleHook
--
--         , requestResumeProcesses $
--             resumeProcesses
--
--         , requestExecutePolicy $
--             executePolicy
--
--         , requestDescribeAccountLimits $
--             describeAccountLimits
--
--         , requestAttachLoadBalancers $
--             attachLoadBalancers
--
--         , requestTerminateInstanceInAutoScalingGroup $
--             terminateInstanceInAutoScalingGroup
--
--         , requestDescribeLoadBalancerTargetGroups $
--             describeLoadBalancerTargetGroups
--
--         , requestPutScheduledUpdateGroupAction $
--             putScheduledUpdateGroupAction
--
--         , requestSetInstanceProtection $
--             setInstanceProtection
--
--         , requestDescribePolicies $
--             describePolicies
--
--         , requestDescribeLaunchConfigurations $
--             describeLaunchConfigurations
--
--         , requestDescribeScalingActivities $
--             describeScalingActivities
--
--         , requestDescribeNotificationConfigurations $
--             describeNotificationConfigurations
--
--         , requestDescribeLifecycleHookTypes $
--             describeLifecycleHookTypes
--
--         , requestDescribeAdjustmentTypes $
--             describeAdjustmentTypes
--
--         , requestCreateAutoScalingGroup $
--             createAutoScalingGroup
--
--         , requestCreateLaunchConfiguration $
--             createLaunchConfiguration
--
--           ]

--     , testGroup "response"
--         [ responseDescribeMetricCollectionTypes $
--             describeMetricCollectionTypesResponse
--
--         , responseDescribeLoadBalancers $
--             describeLoadBalancersResponse
--
--         , responsePutNotificationConfiguration $
--             putNotificationConfigurationResponse
--
--         , responseDescribeTags $
--             describeTagsResponse
--
--         , responseDeleteNotificationConfiguration $
--             deleteNotificationConfigurationResponse
--
--         , responsePutScalingPolicy $
--             putScalingPolicyResponse
--
--         , responseAttachLoadBalancerTargetGroups $
--             attachLoadBalancerTargetGroupsResponse
--
--         , responseDeleteLaunchConfiguration $
--             deleteLaunchConfigurationResponse
--
--         , responseEnterStandby $
--             enterStandbyResponse
--
--         , responseSuspendProcesses $
--             suspendProcessesResponse
--
--         , responseSetInstanceHealth $
--             setInstanceHealthResponse
--
--         , responseExitStandby $
--             exitStandbyResponse
--
--         , responseDescribeTerminationPolicyTypes $
--             describeTerminationPolicyTypesResponse
--
--         , responseDescribeAutoScalingInstances $
--             describeAutoScalingInstancesResponse
--
--         , responseRecordLifecycleActionHeartbeat $
--             recordLifecycleActionHeartbeatResponse
--
--         , responseDisableMetricsCollection $
--             disableMetricsCollectionResponse
--
--         , responseDetachInstances $
--             detachInstancesResponse
--
--         , responseEnableMetricsCollection $
--             enableMetricsCollectionResponse
--
--         , responseDescribeScalingProcessTypes $
--             describeScalingProcessTypesResponse
--
--         , responseDeleteTags $
--             deleteTagsResponse
--
--         , responseDetachLoadBalancerTargetGroups $
--             detachLoadBalancerTargetGroupsResponse
--
--         , responseDescribeLifecycleHooks $
--             describeLifecycleHooksResponse
--
--         , responseDescribeAutoScalingGroups $
--             describeAutoScalingGroupsResponse
--
--         , responseDeleteScheduledAction $
--             deleteScheduledActionResponse
--
--         , responseSetDesiredCapacity $
--             setDesiredCapacityResponse
--
--         , responseDetachLoadBalancers $
--             detachLoadBalancersResponse
--
--         , responseDescribeAutoScalingNotificationTypes $
--             describeAutoScalingNotificationTypesResponse
--
--         , responseDescribeScheduledActions $
--             describeScheduledActionsResponse
--
--         , responseCreateOrUpdateTags $
--             createOrUpdateTagsResponse
--
--         , responseCompleteLifecycleAction $
--             completeLifecycleActionResponse
--
--         , responseDeletePolicy $
--             deletePolicyResponse
--
--         , responseAttachInstances $
--             attachInstancesResponse
--
--         , responseUpdateAutoScalingGroup $
--             updateAutoScalingGroupResponse
--
--         , responseDeleteAutoScalingGroup $
--             deleteAutoScalingGroupResponse
--
--         , responsePutLifecycleHook $
--             putLifecycleHookResponse
--
--         , responseDeleteLifecycleHook $
--             deleteLifecycleHookResponse
--
--         , responseResumeProcesses $
--             resumeProcessesResponse
--
--         , responseExecutePolicy $
--             executePolicyResponse
--
--         , responseDescribeAccountLimits $
--             describeAccountLimitsResponse
--
--         , responseAttachLoadBalancers $
--             attachLoadBalancersResponse
--
--         , responseTerminateInstanceInAutoScalingGroup $
--             terminateInstanceInAutoScalingGroupResponse
--
--         , responseDescribeLoadBalancerTargetGroups $
--             describeLoadBalancerTargetGroupsResponse
--
--         , responsePutScheduledUpdateGroupAction $
--             putScheduledUpdateGroupActionResponse
--
--         , responseSetInstanceProtection $
--             setInstanceProtectionResponse
--
--         , responseDescribePolicies $
--             describePoliciesResponse
--
--         , responseDescribeLaunchConfigurations $
--             describeLaunchConfigurationsResponse
--
--         , responseDescribeScalingActivities $
--             describeScalingActivitiesResponse
--
--         , responseDescribeNotificationConfigurations $
--             describeNotificationConfigurationsResponse
--
--         , responseDescribeLifecycleHookTypes $
--             describeLifecycleHookTypesResponse
--
--         , responseDescribeAdjustmentTypes $
--             describeAdjustmentTypesResponse
--
--         , responseCreateAutoScalingGroup $
--             createAutoScalingGroupResponse
--
--         , responseCreateLaunchConfiguration $
--             createLaunchConfigurationResponse
--
--           ]
--     ]

-- Requests

requestDescribeMetricCollectionTypes :: DescribeMetricCollectionTypes -> TestTree
requestDescribeMetricCollectionTypes = req
    "DescribeMetricCollectionTypes"
    "fixture/DescribeMetricCollectionTypes.yaml"

requestDescribeLoadBalancers :: DescribeLoadBalancers -> TestTree
requestDescribeLoadBalancers = req
    "DescribeLoadBalancers"
    "fixture/DescribeLoadBalancers.yaml"

requestPutNotificationConfiguration :: PutNotificationConfiguration -> TestTree
requestPutNotificationConfiguration = req
    "PutNotificationConfiguration"
    "fixture/PutNotificationConfiguration.yaml"

requestDescribeTags :: DescribeTags -> TestTree
requestDescribeTags = req
    "DescribeTags"
    "fixture/DescribeTags.yaml"

requestDeleteNotificationConfiguration :: DeleteNotificationConfiguration -> TestTree
requestDeleteNotificationConfiguration = req
    "DeleteNotificationConfiguration"
    "fixture/DeleteNotificationConfiguration.yaml"

requestPutScalingPolicy :: PutScalingPolicy -> TestTree
requestPutScalingPolicy = req
    "PutScalingPolicy"
    "fixture/PutScalingPolicy.yaml"

requestAttachLoadBalancerTargetGroups :: AttachLoadBalancerTargetGroups -> TestTree
requestAttachLoadBalancerTargetGroups = req
    "AttachLoadBalancerTargetGroups"
    "fixture/AttachLoadBalancerTargetGroups.yaml"

requestDeleteLaunchConfiguration :: DeleteLaunchConfiguration -> TestTree
requestDeleteLaunchConfiguration = req
    "DeleteLaunchConfiguration"
    "fixture/DeleteLaunchConfiguration.yaml"

requestEnterStandby :: EnterStandby -> TestTree
requestEnterStandby = req
    "EnterStandby"
    "fixture/EnterStandby.yaml"

requestSuspendProcesses :: SuspendProcesses -> TestTree
requestSuspendProcesses = req
    "SuspendProcesses"
    "fixture/SuspendProcesses.yaml"

requestSetInstanceHealth :: SetInstanceHealth -> TestTree
requestSetInstanceHealth = req
    "SetInstanceHealth"
    "fixture/SetInstanceHealth.yaml"

requestExitStandby :: ExitStandby -> TestTree
requestExitStandby = req
    "ExitStandby"
    "fixture/ExitStandby.yaml"

requestDescribeTerminationPolicyTypes :: DescribeTerminationPolicyTypes -> TestTree
requestDescribeTerminationPolicyTypes = req
    "DescribeTerminationPolicyTypes"
    "fixture/DescribeTerminationPolicyTypes.yaml"

requestDescribeAutoScalingInstances :: DescribeAutoScalingInstances -> TestTree
requestDescribeAutoScalingInstances = req
    "DescribeAutoScalingInstances"
    "fixture/DescribeAutoScalingInstances.yaml"

requestRecordLifecycleActionHeartbeat :: RecordLifecycleActionHeartbeat -> TestTree
requestRecordLifecycleActionHeartbeat = req
    "RecordLifecycleActionHeartbeat"
    "fixture/RecordLifecycleActionHeartbeat.yaml"

requestDisableMetricsCollection :: DisableMetricsCollection -> TestTree
requestDisableMetricsCollection = req
    "DisableMetricsCollection"
    "fixture/DisableMetricsCollection.yaml"

requestDetachInstances :: DetachInstances -> TestTree
requestDetachInstances = req
    "DetachInstances"
    "fixture/DetachInstances.yaml"

requestEnableMetricsCollection :: EnableMetricsCollection -> TestTree
requestEnableMetricsCollection = req
    "EnableMetricsCollection"
    "fixture/EnableMetricsCollection.yaml"

requestDescribeScalingProcessTypes :: DescribeScalingProcessTypes -> TestTree
requestDescribeScalingProcessTypes = req
    "DescribeScalingProcessTypes"
    "fixture/DescribeScalingProcessTypes.yaml"

requestDeleteTags :: DeleteTags -> TestTree
requestDeleteTags = req
    "DeleteTags"
    "fixture/DeleteTags.yaml"

requestDetachLoadBalancerTargetGroups :: DetachLoadBalancerTargetGroups -> TestTree
requestDetachLoadBalancerTargetGroups = req
    "DetachLoadBalancerTargetGroups"
    "fixture/DetachLoadBalancerTargetGroups.yaml"

requestDescribeLifecycleHooks :: DescribeLifecycleHooks -> TestTree
requestDescribeLifecycleHooks = req
    "DescribeLifecycleHooks"
    "fixture/DescribeLifecycleHooks.yaml"

requestDescribeAutoScalingGroups :: DescribeAutoScalingGroups -> TestTree
requestDescribeAutoScalingGroups = req
    "DescribeAutoScalingGroups"
    "fixture/DescribeAutoScalingGroups.yaml"

requestDeleteScheduledAction :: DeleteScheduledAction -> TestTree
requestDeleteScheduledAction = req
    "DeleteScheduledAction"
    "fixture/DeleteScheduledAction.yaml"

requestSetDesiredCapacity :: SetDesiredCapacity -> TestTree
requestSetDesiredCapacity = req
    "SetDesiredCapacity"
    "fixture/SetDesiredCapacity.yaml"

requestDetachLoadBalancers :: DetachLoadBalancers -> TestTree
requestDetachLoadBalancers = req
    "DetachLoadBalancers"
    "fixture/DetachLoadBalancers.yaml"

requestDescribeAutoScalingNotificationTypes :: DescribeAutoScalingNotificationTypes -> TestTree
requestDescribeAutoScalingNotificationTypes = req
    "DescribeAutoScalingNotificationTypes"
    "fixture/DescribeAutoScalingNotificationTypes.yaml"

requestDescribeScheduledActions :: DescribeScheduledActions -> TestTree
requestDescribeScheduledActions = req
    "DescribeScheduledActions"
    "fixture/DescribeScheduledActions.yaml"

requestCreateOrUpdateTags :: CreateOrUpdateTags -> TestTree
requestCreateOrUpdateTags = req
    "CreateOrUpdateTags"
    "fixture/CreateOrUpdateTags.yaml"

requestCompleteLifecycleAction :: CompleteLifecycleAction -> TestTree
requestCompleteLifecycleAction = req
    "CompleteLifecycleAction"
    "fixture/CompleteLifecycleAction.yaml"

requestDeletePolicy :: DeletePolicy -> TestTree
requestDeletePolicy = req
    "DeletePolicy"
    "fixture/DeletePolicy.yaml"

requestAttachInstances :: AttachInstances -> TestTree
requestAttachInstances = req
    "AttachInstances"
    "fixture/AttachInstances.yaml"

requestUpdateAutoScalingGroup :: UpdateAutoScalingGroup -> TestTree
requestUpdateAutoScalingGroup = req
    "UpdateAutoScalingGroup"
    "fixture/UpdateAutoScalingGroup.yaml"

requestDeleteAutoScalingGroup :: DeleteAutoScalingGroup -> TestTree
requestDeleteAutoScalingGroup = req
    "DeleteAutoScalingGroup"
    "fixture/DeleteAutoScalingGroup.yaml"

requestPutLifecycleHook :: PutLifecycleHook -> TestTree
requestPutLifecycleHook = req
    "PutLifecycleHook"
    "fixture/PutLifecycleHook.yaml"

requestDeleteLifecycleHook :: DeleteLifecycleHook -> TestTree
requestDeleteLifecycleHook = req
    "DeleteLifecycleHook"
    "fixture/DeleteLifecycleHook.yaml"

requestResumeProcesses :: ResumeProcesses -> TestTree
requestResumeProcesses = req
    "ResumeProcesses"
    "fixture/ResumeProcesses.yaml"

requestExecutePolicy :: ExecutePolicy -> TestTree
requestExecutePolicy = req
    "ExecutePolicy"
    "fixture/ExecutePolicy.yaml"

requestDescribeAccountLimits :: DescribeAccountLimits -> TestTree
requestDescribeAccountLimits = req
    "DescribeAccountLimits"
    "fixture/DescribeAccountLimits.yaml"

requestAttachLoadBalancers :: AttachLoadBalancers -> TestTree
requestAttachLoadBalancers = req
    "AttachLoadBalancers"
    "fixture/AttachLoadBalancers.yaml"

requestTerminateInstanceInAutoScalingGroup :: TerminateInstanceInAutoScalingGroup -> TestTree
requestTerminateInstanceInAutoScalingGroup = req
    "TerminateInstanceInAutoScalingGroup"
    "fixture/TerminateInstanceInAutoScalingGroup.yaml"

requestDescribeLoadBalancerTargetGroups :: DescribeLoadBalancerTargetGroups -> TestTree
requestDescribeLoadBalancerTargetGroups = req
    "DescribeLoadBalancerTargetGroups"
    "fixture/DescribeLoadBalancerTargetGroups.yaml"

requestPutScheduledUpdateGroupAction :: PutScheduledUpdateGroupAction -> TestTree
requestPutScheduledUpdateGroupAction = req
    "PutScheduledUpdateGroupAction"
    "fixture/PutScheduledUpdateGroupAction.yaml"

requestSetInstanceProtection :: SetInstanceProtection -> TestTree
requestSetInstanceProtection = req
    "SetInstanceProtection"
    "fixture/SetInstanceProtection.yaml"

requestDescribePolicies :: DescribePolicies -> TestTree
requestDescribePolicies = req
    "DescribePolicies"
    "fixture/DescribePolicies.yaml"

requestDescribeLaunchConfigurations :: DescribeLaunchConfigurations -> TestTree
requestDescribeLaunchConfigurations = req
    "DescribeLaunchConfigurations"
    "fixture/DescribeLaunchConfigurations.yaml"

requestDescribeScalingActivities :: DescribeScalingActivities -> TestTree
requestDescribeScalingActivities = req
    "DescribeScalingActivities"
    "fixture/DescribeScalingActivities.yaml"

requestDescribeNotificationConfigurations :: DescribeNotificationConfigurations -> TestTree
requestDescribeNotificationConfigurations = req
    "DescribeNotificationConfigurations"
    "fixture/DescribeNotificationConfigurations.yaml"

requestDescribeLifecycleHookTypes :: DescribeLifecycleHookTypes -> TestTree
requestDescribeLifecycleHookTypes = req
    "DescribeLifecycleHookTypes"
    "fixture/DescribeLifecycleHookTypes.yaml"

requestDescribeAdjustmentTypes :: DescribeAdjustmentTypes -> TestTree
requestDescribeAdjustmentTypes = req
    "DescribeAdjustmentTypes"
    "fixture/DescribeAdjustmentTypes.yaml"

requestCreateAutoScalingGroup :: CreateAutoScalingGroup -> TestTree
requestCreateAutoScalingGroup = req
    "CreateAutoScalingGroup"
    "fixture/CreateAutoScalingGroup.yaml"

requestCreateLaunchConfiguration :: CreateLaunchConfiguration -> TestTree
requestCreateLaunchConfiguration = req
    "CreateLaunchConfiguration"
    "fixture/CreateLaunchConfiguration.yaml"

-- Responses

responseDescribeMetricCollectionTypes :: DescribeMetricCollectionTypesResponse -> TestTree
responseDescribeMetricCollectionTypes = res
    "DescribeMetricCollectionTypesResponse"
    "fixture/DescribeMetricCollectionTypesResponse.proto"
    autoScaling
    (Proxy :: Proxy DescribeMetricCollectionTypes)

responseDescribeLoadBalancers :: DescribeLoadBalancersResponse -> TestTree
responseDescribeLoadBalancers = res
    "DescribeLoadBalancersResponse"
    "fixture/DescribeLoadBalancersResponse.proto"
    autoScaling
    (Proxy :: Proxy DescribeLoadBalancers)

responsePutNotificationConfiguration :: PutNotificationConfigurationResponse -> TestTree
responsePutNotificationConfiguration = res
    "PutNotificationConfigurationResponse"
    "fixture/PutNotificationConfigurationResponse.proto"
    autoScaling
    (Proxy :: Proxy PutNotificationConfiguration)

responseDescribeTags :: DescribeTagsResponse -> TestTree
responseDescribeTags = res
    "DescribeTagsResponse"
    "fixture/DescribeTagsResponse.proto"
    autoScaling
    (Proxy :: Proxy DescribeTags)

responseDeleteNotificationConfiguration :: DeleteNotificationConfigurationResponse -> TestTree
responseDeleteNotificationConfiguration = res
    "DeleteNotificationConfigurationResponse"
    "fixture/DeleteNotificationConfigurationResponse.proto"
    autoScaling
    (Proxy :: Proxy DeleteNotificationConfiguration)

responsePutScalingPolicy :: PutScalingPolicyResponse -> TestTree
responsePutScalingPolicy = res
    "PutScalingPolicyResponse"
    "fixture/PutScalingPolicyResponse.proto"
    autoScaling
    (Proxy :: Proxy PutScalingPolicy)

responseAttachLoadBalancerTargetGroups :: AttachLoadBalancerTargetGroupsResponse -> TestTree
responseAttachLoadBalancerTargetGroups = res
    "AttachLoadBalancerTargetGroupsResponse"
    "fixture/AttachLoadBalancerTargetGroupsResponse.proto"
    autoScaling
    (Proxy :: Proxy AttachLoadBalancerTargetGroups)

responseDeleteLaunchConfiguration :: DeleteLaunchConfigurationResponse -> TestTree
responseDeleteLaunchConfiguration = res
    "DeleteLaunchConfigurationResponse"
    "fixture/DeleteLaunchConfigurationResponse.proto"
    autoScaling
    (Proxy :: Proxy DeleteLaunchConfiguration)

responseEnterStandby :: EnterStandbyResponse -> TestTree
responseEnterStandby = res
    "EnterStandbyResponse"
    "fixture/EnterStandbyResponse.proto"
    autoScaling
    (Proxy :: Proxy EnterStandby)

responseSuspendProcesses :: SuspendProcessesResponse -> TestTree
responseSuspendProcesses = res
    "SuspendProcessesResponse"
    "fixture/SuspendProcessesResponse.proto"
    autoScaling
    (Proxy :: Proxy SuspendProcesses)

responseSetInstanceHealth :: SetInstanceHealthResponse -> TestTree
responseSetInstanceHealth = res
    "SetInstanceHealthResponse"
    "fixture/SetInstanceHealthResponse.proto"
    autoScaling
    (Proxy :: Proxy SetInstanceHealth)

responseExitStandby :: ExitStandbyResponse -> TestTree
responseExitStandby = res
    "ExitStandbyResponse"
    "fixture/ExitStandbyResponse.proto"
    autoScaling
    (Proxy :: Proxy ExitStandby)

responseDescribeTerminationPolicyTypes :: DescribeTerminationPolicyTypesResponse -> TestTree
responseDescribeTerminationPolicyTypes = res
    "DescribeTerminationPolicyTypesResponse"
    "fixture/DescribeTerminationPolicyTypesResponse.proto"
    autoScaling
    (Proxy :: Proxy DescribeTerminationPolicyTypes)

responseDescribeAutoScalingInstances :: DescribeAutoScalingInstancesResponse -> TestTree
responseDescribeAutoScalingInstances = res
    "DescribeAutoScalingInstancesResponse"
    "fixture/DescribeAutoScalingInstancesResponse.proto"
    autoScaling
    (Proxy :: Proxy DescribeAutoScalingInstances)

responseRecordLifecycleActionHeartbeat :: RecordLifecycleActionHeartbeatResponse -> TestTree
responseRecordLifecycleActionHeartbeat = res
    "RecordLifecycleActionHeartbeatResponse"
    "fixture/RecordLifecycleActionHeartbeatResponse.proto"
    autoScaling
    (Proxy :: Proxy RecordLifecycleActionHeartbeat)

responseDisableMetricsCollection :: DisableMetricsCollectionResponse -> TestTree
responseDisableMetricsCollection = res
    "DisableMetricsCollectionResponse"
    "fixture/DisableMetricsCollectionResponse.proto"
    autoScaling
    (Proxy :: Proxy DisableMetricsCollection)

responseDetachInstances :: DetachInstancesResponse -> TestTree
responseDetachInstances = res
    "DetachInstancesResponse"
    "fixture/DetachInstancesResponse.proto"
    autoScaling
    (Proxy :: Proxy DetachInstances)

responseEnableMetricsCollection :: EnableMetricsCollectionResponse -> TestTree
responseEnableMetricsCollection = res
    "EnableMetricsCollectionResponse"
    "fixture/EnableMetricsCollectionResponse.proto"
    autoScaling
    (Proxy :: Proxy EnableMetricsCollection)

responseDescribeScalingProcessTypes :: DescribeScalingProcessTypesResponse -> TestTree
responseDescribeScalingProcessTypes = res
    "DescribeScalingProcessTypesResponse"
    "fixture/DescribeScalingProcessTypesResponse.proto"
    autoScaling
    (Proxy :: Proxy DescribeScalingProcessTypes)

responseDeleteTags :: DeleteTagsResponse -> TestTree
responseDeleteTags = res
    "DeleteTagsResponse"
    "fixture/DeleteTagsResponse.proto"
    autoScaling
    (Proxy :: Proxy DeleteTags)

responseDetachLoadBalancerTargetGroups :: DetachLoadBalancerTargetGroupsResponse -> TestTree
responseDetachLoadBalancerTargetGroups = res
    "DetachLoadBalancerTargetGroupsResponse"
    "fixture/DetachLoadBalancerTargetGroupsResponse.proto"
    autoScaling
    (Proxy :: Proxy DetachLoadBalancerTargetGroups)

responseDescribeLifecycleHooks :: DescribeLifecycleHooksResponse -> TestTree
responseDescribeLifecycleHooks = res
    "DescribeLifecycleHooksResponse"
    "fixture/DescribeLifecycleHooksResponse.proto"
    autoScaling
    (Proxy :: Proxy DescribeLifecycleHooks)

responseDescribeAutoScalingGroups :: DescribeAutoScalingGroupsResponse -> TestTree
responseDescribeAutoScalingGroups = res
    "DescribeAutoScalingGroupsResponse"
    "fixture/DescribeAutoScalingGroupsResponse.proto"
    autoScaling
    (Proxy :: Proxy DescribeAutoScalingGroups)

responseDeleteScheduledAction :: DeleteScheduledActionResponse -> TestTree
responseDeleteScheduledAction = res
    "DeleteScheduledActionResponse"
    "fixture/DeleteScheduledActionResponse.proto"
    autoScaling
    (Proxy :: Proxy DeleteScheduledAction)

responseSetDesiredCapacity :: SetDesiredCapacityResponse -> TestTree
responseSetDesiredCapacity = res
    "SetDesiredCapacityResponse"
    "fixture/SetDesiredCapacityResponse.proto"
    autoScaling
    (Proxy :: Proxy SetDesiredCapacity)

responseDetachLoadBalancers :: DetachLoadBalancersResponse -> TestTree
responseDetachLoadBalancers = res
    "DetachLoadBalancersResponse"
    "fixture/DetachLoadBalancersResponse.proto"
    autoScaling
    (Proxy :: Proxy DetachLoadBalancers)

responseDescribeAutoScalingNotificationTypes :: DescribeAutoScalingNotificationTypesResponse -> TestTree
responseDescribeAutoScalingNotificationTypes = res
    "DescribeAutoScalingNotificationTypesResponse"
    "fixture/DescribeAutoScalingNotificationTypesResponse.proto"
    autoScaling
    (Proxy :: Proxy DescribeAutoScalingNotificationTypes)

responseDescribeScheduledActions :: DescribeScheduledActionsResponse -> TestTree
responseDescribeScheduledActions = res
    "DescribeScheduledActionsResponse"
    "fixture/DescribeScheduledActionsResponse.proto"
    autoScaling
    (Proxy :: Proxy DescribeScheduledActions)

responseCreateOrUpdateTags :: CreateOrUpdateTagsResponse -> TestTree
responseCreateOrUpdateTags = res
    "CreateOrUpdateTagsResponse"
    "fixture/CreateOrUpdateTagsResponse.proto"
    autoScaling
    (Proxy :: Proxy CreateOrUpdateTags)

responseCompleteLifecycleAction :: CompleteLifecycleActionResponse -> TestTree
responseCompleteLifecycleAction = res
    "CompleteLifecycleActionResponse"
    "fixture/CompleteLifecycleActionResponse.proto"
    autoScaling
    (Proxy :: Proxy CompleteLifecycleAction)

responseDeletePolicy :: DeletePolicyResponse -> TestTree
responseDeletePolicy = res
    "DeletePolicyResponse"
    "fixture/DeletePolicyResponse.proto"
    autoScaling
    (Proxy :: Proxy DeletePolicy)

responseAttachInstances :: AttachInstancesResponse -> TestTree
responseAttachInstances = res
    "AttachInstancesResponse"
    "fixture/AttachInstancesResponse.proto"
    autoScaling
    (Proxy :: Proxy AttachInstances)

responseUpdateAutoScalingGroup :: UpdateAutoScalingGroupResponse -> TestTree
responseUpdateAutoScalingGroup = res
    "UpdateAutoScalingGroupResponse"
    "fixture/UpdateAutoScalingGroupResponse.proto"
    autoScaling
    (Proxy :: Proxy UpdateAutoScalingGroup)

responseDeleteAutoScalingGroup :: DeleteAutoScalingGroupResponse -> TestTree
responseDeleteAutoScalingGroup = res
    "DeleteAutoScalingGroupResponse"
    "fixture/DeleteAutoScalingGroupResponse.proto"
    autoScaling
    (Proxy :: Proxy DeleteAutoScalingGroup)

responsePutLifecycleHook :: PutLifecycleHookResponse -> TestTree
responsePutLifecycleHook = res
    "PutLifecycleHookResponse"
    "fixture/PutLifecycleHookResponse.proto"
    autoScaling
    (Proxy :: Proxy PutLifecycleHook)

responseDeleteLifecycleHook :: DeleteLifecycleHookResponse -> TestTree
responseDeleteLifecycleHook = res
    "DeleteLifecycleHookResponse"
    "fixture/DeleteLifecycleHookResponse.proto"
    autoScaling
    (Proxy :: Proxy DeleteLifecycleHook)

responseResumeProcesses :: ResumeProcessesResponse -> TestTree
responseResumeProcesses = res
    "ResumeProcessesResponse"
    "fixture/ResumeProcessesResponse.proto"
    autoScaling
    (Proxy :: Proxy ResumeProcesses)

responseExecutePolicy :: ExecutePolicyResponse -> TestTree
responseExecutePolicy = res
    "ExecutePolicyResponse"
    "fixture/ExecutePolicyResponse.proto"
    autoScaling
    (Proxy :: Proxy ExecutePolicy)

responseDescribeAccountLimits :: DescribeAccountLimitsResponse -> TestTree
responseDescribeAccountLimits = res
    "DescribeAccountLimitsResponse"
    "fixture/DescribeAccountLimitsResponse.proto"
    autoScaling
    (Proxy :: Proxy DescribeAccountLimits)

responseAttachLoadBalancers :: AttachLoadBalancersResponse -> TestTree
responseAttachLoadBalancers = res
    "AttachLoadBalancersResponse"
    "fixture/AttachLoadBalancersResponse.proto"
    autoScaling
    (Proxy :: Proxy AttachLoadBalancers)

responseTerminateInstanceInAutoScalingGroup :: TerminateInstanceInAutoScalingGroupResponse -> TestTree
responseTerminateInstanceInAutoScalingGroup = res
    "TerminateInstanceInAutoScalingGroupResponse"
    "fixture/TerminateInstanceInAutoScalingGroupResponse.proto"
    autoScaling
    (Proxy :: Proxy TerminateInstanceInAutoScalingGroup)

responseDescribeLoadBalancerTargetGroups :: DescribeLoadBalancerTargetGroupsResponse -> TestTree
responseDescribeLoadBalancerTargetGroups = res
    "DescribeLoadBalancerTargetGroupsResponse"
    "fixture/DescribeLoadBalancerTargetGroupsResponse.proto"
    autoScaling
    (Proxy :: Proxy DescribeLoadBalancerTargetGroups)

responsePutScheduledUpdateGroupAction :: PutScheduledUpdateGroupActionResponse -> TestTree
responsePutScheduledUpdateGroupAction = res
    "PutScheduledUpdateGroupActionResponse"
    "fixture/PutScheduledUpdateGroupActionResponse.proto"
    autoScaling
    (Proxy :: Proxy PutScheduledUpdateGroupAction)

responseSetInstanceProtection :: SetInstanceProtectionResponse -> TestTree
responseSetInstanceProtection = res
    "SetInstanceProtectionResponse"
    "fixture/SetInstanceProtectionResponse.proto"
    autoScaling
    (Proxy :: Proxy SetInstanceProtection)

responseDescribePolicies :: DescribePoliciesResponse -> TestTree
responseDescribePolicies = res
    "DescribePoliciesResponse"
    "fixture/DescribePoliciesResponse.proto"
    autoScaling
    (Proxy :: Proxy DescribePolicies)

responseDescribeLaunchConfigurations :: DescribeLaunchConfigurationsResponse -> TestTree
responseDescribeLaunchConfigurations = res
    "DescribeLaunchConfigurationsResponse"
    "fixture/DescribeLaunchConfigurationsResponse.proto"
    autoScaling
    (Proxy :: Proxy DescribeLaunchConfigurations)

responseDescribeScalingActivities :: DescribeScalingActivitiesResponse -> TestTree
responseDescribeScalingActivities = res
    "DescribeScalingActivitiesResponse"
    "fixture/DescribeScalingActivitiesResponse.proto"
    autoScaling
    (Proxy :: Proxy DescribeScalingActivities)

responseDescribeNotificationConfigurations :: DescribeNotificationConfigurationsResponse -> TestTree
responseDescribeNotificationConfigurations = res
    "DescribeNotificationConfigurationsResponse"
    "fixture/DescribeNotificationConfigurationsResponse.proto"
    autoScaling
    (Proxy :: Proxy DescribeNotificationConfigurations)

responseDescribeLifecycleHookTypes :: DescribeLifecycleHookTypesResponse -> TestTree
responseDescribeLifecycleHookTypes = res
    "DescribeLifecycleHookTypesResponse"
    "fixture/DescribeLifecycleHookTypesResponse.proto"
    autoScaling
    (Proxy :: Proxy DescribeLifecycleHookTypes)

responseDescribeAdjustmentTypes :: DescribeAdjustmentTypesResponse -> TestTree
responseDescribeAdjustmentTypes = res
    "DescribeAdjustmentTypesResponse"
    "fixture/DescribeAdjustmentTypesResponse.proto"
    autoScaling
    (Proxy :: Proxy DescribeAdjustmentTypes)

responseCreateAutoScalingGroup :: CreateAutoScalingGroupResponse -> TestTree
responseCreateAutoScalingGroup = res
    "CreateAutoScalingGroupResponse"
    "fixture/CreateAutoScalingGroupResponse.proto"
    autoScaling
    (Proxy :: Proxy CreateAutoScalingGroup)

responseCreateLaunchConfiguration :: CreateLaunchConfigurationResponse -> TestTree
responseCreateLaunchConfiguration = res
    "CreateLaunchConfigurationResponse"
    "fixture/CreateLaunchConfigurationResponse.proto"
    autoScaling
    (Proxy :: Proxy CreateLaunchConfiguration)
