{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.AutoScaling
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
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
--             mkDescribeMetricCollectionTypes
--
--         , requestDescribeLoadBalancers $
--             mkDescribeLoadBalancers
--
--         , requestPutNotificationConfiguration $
--             mkPutNotificationConfiguration
--
--         , requestDescribeTags $
--             mkDescribeTags
--
--         , requestDeleteNotificationConfiguration $
--             mkDeleteNotificationConfiguration
--
--         , requestPutScalingPolicy $
--             mkPutScalingPolicy
--
--         , requestStartInstanceRefresh $
--             mkStartInstanceRefresh
--
--         , requestAttachLoadBalancerTargetGroups $
--             mkAttachLoadBalancerTargetGroups
--
--         , requestDeleteLaunchConfiguration $
--             mkDeleteLaunchConfiguration
--
--         , requestEnterStandby $
--             mkEnterStandby
--
--         , requestSuspendProcesses $
--             mkSuspendProcesses
--
--         , requestSetInstanceHealth $
--             mkSetInstanceHealth
--
--         , requestExitStandby $
--             mkExitStandby
--
--         , requestDescribeTerminationPolicyTypes $
--             mkDescribeTerminationPolicyTypes
--
--         , requestCancelInstanceRefresh $
--             mkCancelInstanceRefresh
--
--         , requestDescribeAutoScalingInstances $
--             mkDescribeAutoScalingInstances
--
--         , requestRecordLifecycleActionHeartbeat $
--             mkRecordLifecycleActionHeartbeat
--
--         , requestDisableMetricsCollection $
--             mkDisableMetricsCollection
--
--         , requestDetachInstances $
--             mkDetachInstances
--
--         , requestEnableMetricsCollection $
--             mkEnableMetricsCollection
--
--         , requestDescribeScalingProcessTypes $
--             mkDescribeScalingProcessTypes
--
--         , requestDeleteTags $
--             mkDeleteTags
--
--         , requestDetachLoadBalancerTargetGroups $
--             mkDetachLoadBalancerTargetGroups
--
--         , requestDescribeLifecycleHooks $
--             mkDescribeLifecycleHooks
--
--         , requestDescribeAutoScalingGroups $
--             mkDescribeAutoScalingGroups
--
--         , requestDeleteScheduledAction $
--             mkDeleteScheduledAction
--
--         , requestSetDesiredCapacity $
--             mkSetDesiredCapacity
--
--         , requestDetachLoadBalancers $
--             mkDetachLoadBalancers
--
--         , requestDescribeAutoScalingNotificationTypes $
--             mkDescribeAutoScalingNotificationTypes
--
--         , requestDescribeScheduledActions $
--             mkDescribeScheduledActions
--
--         , requestCreateOrUpdateTags $
--             mkCreateOrUpdateTags
--
--         , requestCompleteLifecycleAction $
--             mkCompleteLifecycleAction
--
--         , requestDeletePolicy $
--             mkDeletePolicy
--
--         , requestAttachInstances $
--             mkAttachInstances
--
--         , requestUpdateAutoScalingGroup $
--             mkUpdateAutoScalingGroup
--
--         , requestDeleteAutoScalingGroup $
--             mkDeleteAutoScalingGroup
--
--         , requestPutLifecycleHook $
--             mkPutLifecycleHook
--
--         , requestBatchPutScheduledUpdateGroupAction $
--             mkBatchPutScheduledUpdateGroupAction
--
--         , requestDeleteLifecycleHook $
--             mkDeleteLifecycleHook
--
--         , requestResumeProcesses $
--             mkResumeProcesses
--
--         , requestExecutePolicy $
--             mkExecutePolicy
--
--         , requestDescribeInstanceRefreshes $
--             mkDescribeInstanceRefreshes
--
--         , requestDescribeAccountLimits $
--             mkDescribeAccountLimits
--
--         , requestAttachLoadBalancers $
--             mkAttachLoadBalancers
--
--         , requestBatchDeleteScheduledAction $
--             mkBatchDeleteScheduledAction
--
--         , requestTerminateInstanceInAutoScalingGroup $
--             mkTerminateInstanceInAutoScalingGroup
--
--         , requestDescribeLoadBalancerTargetGroups $
--             mkDescribeLoadBalancerTargetGroups
--
--         , requestPutScheduledUpdateGroupAction $
--             mkPutScheduledUpdateGroupAction
--
--         , requestSetInstanceProtection $
--             mkSetInstanceProtection
--
--         , requestDescribePolicies $
--             mkDescribePolicies
--
--         , requestDescribeLaunchConfigurations $
--             mkDescribeLaunchConfigurations
--
--         , requestDescribeScalingActivities $
--             mkDescribeScalingActivities
--
--         , requestDescribeNotificationConfigurations $
--             mkDescribeNotificationConfigurations
--
--         , requestDescribeLifecycleHookTypes $
--             mkDescribeLifecycleHookTypes
--
--         , requestDescribeAdjustmentTypes $
--             mkDescribeAdjustmentTypes
--
--         , requestCreateAutoScalingGroup $
--             mkCreateAutoScalingGroup
--
--         , requestCreateLaunchConfiguration $
--             mkCreateLaunchConfiguration
--
--           ]

--     , testGroup "response"
--         [ responseDescribeMetricCollectionTypes $
--             mkDescribeMetricCollectionTypesResponse
--
--         , responseDescribeLoadBalancers $
--             mkDescribeLoadBalancersResponse
--
--         , responsePutNotificationConfiguration $
--             mkPutNotificationConfigurationResponse
--
--         , responseDescribeTags $
--             mkDescribeTagsResponse
--
--         , responseDeleteNotificationConfiguration $
--             mkDeleteNotificationConfigurationResponse
--
--         , responsePutScalingPolicy $
--             mkPutScalingPolicyResponse
--
--         , responseStartInstanceRefresh $
--             mkStartInstanceRefreshResponse
--
--         , responseAttachLoadBalancerTargetGroups $
--             mkAttachLoadBalancerTargetGroupsResponse
--
--         , responseDeleteLaunchConfiguration $
--             mkDeleteLaunchConfigurationResponse
--
--         , responseEnterStandby $
--             mkEnterStandbyResponse
--
--         , responseSuspendProcesses $
--             mkSuspendProcessesResponse
--
--         , responseSetInstanceHealth $
--             mkSetInstanceHealthResponse
--
--         , responseExitStandby $
--             mkExitStandbyResponse
--
--         , responseDescribeTerminationPolicyTypes $
--             mkDescribeTerminationPolicyTypesResponse
--
--         , responseCancelInstanceRefresh $
--             mkCancelInstanceRefreshResponse
--
--         , responseDescribeAutoScalingInstances $
--             mkDescribeAutoScalingInstancesResponse
--
--         , responseRecordLifecycleActionHeartbeat $
--             mkRecordLifecycleActionHeartbeatResponse
--
--         , responseDisableMetricsCollection $
--             mkDisableMetricsCollectionResponse
--
--         , responseDetachInstances $
--             mkDetachInstancesResponse
--
--         , responseEnableMetricsCollection $
--             mkEnableMetricsCollectionResponse
--
--         , responseDescribeScalingProcessTypes $
--             mkDescribeScalingProcessTypesResponse
--
--         , responseDeleteTags $
--             mkDeleteTagsResponse
--
--         , responseDetachLoadBalancerTargetGroups $
--             mkDetachLoadBalancerTargetGroupsResponse
--
--         , responseDescribeLifecycleHooks $
--             mkDescribeLifecycleHooksResponse
--
--         , responseDescribeAutoScalingGroups $
--             mkDescribeAutoScalingGroupsResponse
--
--         , responseDeleteScheduledAction $
--             mkDeleteScheduledActionResponse
--
--         , responseSetDesiredCapacity $
--             mkSetDesiredCapacityResponse
--
--         , responseDetachLoadBalancers $
--             mkDetachLoadBalancersResponse
--
--         , responseDescribeAutoScalingNotificationTypes $
--             mkDescribeAutoScalingNotificationTypesResponse
--
--         , responseDescribeScheduledActions $
--             mkDescribeScheduledActionsResponse
--
--         , responseCreateOrUpdateTags $
--             mkCreateOrUpdateTagsResponse
--
--         , responseCompleteLifecycleAction $
--             mkCompleteLifecycleActionResponse
--
--         , responseDeletePolicy $
--             mkDeletePolicyResponse
--
--         , responseAttachInstances $
--             mkAttachInstancesResponse
--
--         , responseUpdateAutoScalingGroup $
--             mkUpdateAutoScalingGroupResponse
--
--         , responseDeleteAutoScalingGroup $
--             mkDeleteAutoScalingGroupResponse
--
--         , responsePutLifecycleHook $
--             mkPutLifecycleHookResponse
--
--         , responseBatchPutScheduledUpdateGroupAction $
--             mkBatchPutScheduledUpdateGroupActionResponse
--
--         , responseDeleteLifecycleHook $
--             mkDeleteLifecycleHookResponse
--
--         , responseResumeProcesses $
--             mkResumeProcessesResponse
--
--         , responseExecutePolicy $
--             mkExecutePolicyResponse
--
--         , responseDescribeInstanceRefreshes $
--             mkDescribeInstanceRefreshesResponse
--
--         , responseDescribeAccountLimits $
--             mkDescribeAccountLimitsResponse
--
--         , responseAttachLoadBalancers $
--             mkAttachLoadBalancersResponse
--
--         , responseBatchDeleteScheduledAction $
--             mkBatchDeleteScheduledActionResponse
--
--         , responseTerminateInstanceInAutoScalingGroup $
--             mkTerminateInstanceInAutoScalingGroupResponse
--
--         , responseDescribeLoadBalancerTargetGroups $
--             mkDescribeLoadBalancerTargetGroupsResponse
--
--         , responsePutScheduledUpdateGroupAction $
--             mkPutScheduledUpdateGroupActionResponse
--
--         , responseSetInstanceProtection $
--             mkSetInstanceProtectionResponse
--
--         , responseDescribePolicies $
--             mkDescribePoliciesResponse
--
--         , responseDescribeLaunchConfigurations $
--             mkDescribeLaunchConfigurationsResponse
--
--         , responseDescribeScalingActivities $
--             mkDescribeScalingActivitiesResponse
--
--         , responseDescribeNotificationConfigurations $
--             mkDescribeNotificationConfigurationsResponse
--
--         , responseDescribeLifecycleHookTypes $
--             mkDescribeLifecycleHookTypesResponse
--
--         , responseDescribeAdjustmentTypes $
--             mkDescribeAdjustmentTypesResponse
--
--         , responseCreateAutoScalingGroup $
--             mkCreateAutoScalingGroupResponse
--
--         , responseCreateLaunchConfiguration $
--             mkCreateLaunchConfigurationResponse
--
--           ]
--     ]

-- Requests

requestDescribeMetricCollectionTypes :: DescribeMetricCollectionTypes -> TestTree
requestDescribeMetricCollectionTypes =
  req
    "DescribeMetricCollectionTypes"
    "fixture/DescribeMetricCollectionTypes.yaml"

requestDescribeLoadBalancers :: DescribeLoadBalancers -> TestTree
requestDescribeLoadBalancers =
  req
    "DescribeLoadBalancers"
    "fixture/DescribeLoadBalancers.yaml"

requestPutNotificationConfiguration :: PutNotificationConfiguration -> TestTree
requestPutNotificationConfiguration =
  req
    "PutNotificationConfiguration"
    "fixture/PutNotificationConfiguration.yaml"

requestDescribeTags :: DescribeTags -> TestTree
requestDescribeTags =
  req
    "DescribeTags"
    "fixture/DescribeTags.yaml"

requestDeleteNotificationConfiguration :: DeleteNotificationConfiguration -> TestTree
requestDeleteNotificationConfiguration =
  req
    "DeleteNotificationConfiguration"
    "fixture/DeleteNotificationConfiguration.yaml"

requestPutScalingPolicy :: PutScalingPolicy -> TestTree
requestPutScalingPolicy =
  req
    "PutScalingPolicy"
    "fixture/PutScalingPolicy.yaml"

requestStartInstanceRefresh :: StartInstanceRefresh -> TestTree
requestStartInstanceRefresh =
  req
    "StartInstanceRefresh"
    "fixture/StartInstanceRefresh.yaml"

requestAttachLoadBalancerTargetGroups :: AttachLoadBalancerTargetGroups -> TestTree
requestAttachLoadBalancerTargetGroups =
  req
    "AttachLoadBalancerTargetGroups"
    "fixture/AttachLoadBalancerTargetGroups.yaml"

requestDeleteLaunchConfiguration :: DeleteLaunchConfiguration -> TestTree
requestDeleteLaunchConfiguration =
  req
    "DeleteLaunchConfiguration"
    "fixture/DeleteLaunchConfiguration.yaml"

requestEnterStandby :: EnterStandby -> TestTree
requestEnterStandby =
  req
    "EnterStandby"
    "fixture/EnterStandby.yaml"

requestSuspendProcesses :: SuspendProcesses -> TestTree
requestSuspendProcesses =
  req
    "SuspendProcesses"
    "fixture/SuspendProcesses.yaml"

requestSetInstanceHealth :: SetInstanceHealth -> TestTree
requestSetInstanceHealth =
  req
    "SetInstanceHealth"
    "fixture/SetInstanceHealth.yaml"

requestExitStandby :: ExitStandby -> TestTree
requestExitStandby =
  req
    "ExitStandby"
    "fixture/ExitStandby.yaml"

requestDescribeTerminationPolicyTypes :: DescribeTerminationPolicyTypes -> TestTree
requestDescribeTerminationPolicyTypes =
  req
    "DescribeTerminationPolicyTypes"
    "fixture/DescribeTerminationPolicyTypes.yaml"

requestCancelInstanceRefresh :: CancelInstanceRefresh -> TestTree
requestCancelInstanceRefresh =
  req
    "CancelInstanceRefresh"
    "fixture/CancelInstanceRefresh.yaml"

requestDescribeAutoScalingInstances :: DescribeAutoScalingInstances -> TestTree
requestDescribeAutoScalingInstances =
  req
    "DescribeAutoScalingInstances"
    "fixture/DescribeAutoScalingInstances.yaml"

requestRecordLifecycleActionHeartbeat :: RecordLifecycleActionHeartbeat -> TestTree
requestRecordLifecycleActionHeartbeat =
  req
    "RecordLifecycleActionHeartbeat"
    "fixture/RecordLifecycleActionHeartbeat.yaml"

requestDisableMetricsCollection :: DisableMetricsCollection -> TestTree
requestDisableMetricsCollection =
  req
    "DisableMetricsCollection"
    "fixture/DisableMetricsCollection.yaml"

requestDetachInstances :: DetachInstances -> TestTree
requestDetachInstances =
  req
    "DetachInstances"
    "fixture/DetachInstances.yaml"

requestEnableMetricsCollection :: EnableMetricsCollection -> TestTree
requestEnableMetricsCollection =
  req
    "EnableMetricsCollection"
    "fixture/EnableMetricsCollection.yaml"

requestDescribeScalingProcessTypes :: DescribeScalingProcessTypes -> TestTree
requestDescribeScalingProcessTypes =
  req
    "DescribeScalingProcessTypes"
    "fixture/DescribeScalingProcessTypes.yaml"

requestDeleteTags :: DeleteTags -> TestTree
requestDeleteTags =
  req
    "DeleteTags"
    "fixture/DeleteTags.yaml"

requestDetachLoadBalancerTargetGroups :: DetachLoadBalancerTargetGroups -> TestTree
requestDetachLoadBalancerTargetGroups =
  req
    "DetachLoadBalancerTargetGroups"
    "fixture/DetachLoadBalancerTargetGroups.yaml"

requestDescribeLifecycleHooks :: DescribeLifecycleHooks -> TestTree
requestDescribeLifecycleHooks =
  req
    "DescribeLifecycleHooks"
    "fixture/DescribeLifecycleHooks.yaml"

requestDescribeAutoScalingGroups :: DescribeAutoScalingGroups -> TestTree
requestDescribeAutoScalingGroups =
  req
    "DescribeAutoScalingGroups"
    "fixture/DescribeAutoScalingGroups.yaml"

requestDeleteScheduledAction :: DeleteScheduledAction -> TestTree
requestDeleteScheduledAction =
  req
    "DeleteScheduledAction"
    "fixture/DeleteScheduledAction.yaml"

requestSetDesiredCapacity :: SetDesiredCapacity -> TestTree
requestSetDesiredCapacity =
  req
    "SetDesiredCapacity"
    "fixture/SetDesiredCapacity.yaml"

requestDetachLoadBalancers :: DetachLoadBalancers -> TestTree
requestDetachLoadBalancers =
  req
    "DetachLoadBalancers"
    "fixture/DetachLoadBalancers.yaml"

requestDescribeAutoScalingNotificationTypes :: DescribeAutoScalingNotificationTypes -> TestTree
requestDescribeAutoScalingNotificationTypes =
  req
    "DescribeAutoScalingNotificationTypes"
    "fixture/DescribeAutoScalingNotificationTypes.yaml"

requestDescribeScheduledActions :: DescribeScheduledActions -> TestTree
requestDescribeScheduledActions =
  req
    "DescribeScheduledActions"
    "fixture/DescribeScheduledActions.yaml"

requestCreateOrUpdateTags :: CreateOrUpdateTags -> TestTree
requestCreateOrUpdateTags =
  req
    "CreateOrUpdateTags"
    "fixture/CreateOrUpdateTags.yaml"

requestCompleteLifecycleAction :: CompleteLifecycleAction -> TestTree
requestCompleteLifecycleAction =
  req
    "CompleteLifecycleAction"
    "fixture/CompleteLifecycleAction.yaml"

requestDeletePolicy :: DeletePolicy -> TestTree
requestDeletePolicy =
  req
    "DeletePolicy"
    "fixture/DeletePolicy.yaml"

requestAttachInstances :: AttachInstances -> TestTree
requestAttachInstances =
  req
    "AttachInstances"
    "fixture/AttachInstances.yaml"

requestUpdateAutoScalingGroup :: UpdateAutoScalingGroup -> TestTree
requestUpdateAutoScalingGroup =
  req
    "UpdateAutoScalingGroup"
    "fixture/UpdateAutoScalingGroup.yaml"

requestDeleteAutoScalingGroup :: DeleteAutoScalingGroup -> TestTree
requestDeleteAutoScalingGroup =
  req
    "DeleteAutoScalingGroup"
    "fixture/DeleteAutoScalingGroup.yaml"

requestPutLifecycleHook :: PutLifecycleHook -> TestTree
requestPutLifecycleHook =
  req
    "PutLifecycleHook"
    "fixture/PutLifecycleHook.yaml"

requestBatchPutScheduledUpdateGroupAction :: BatchPutScheduledUpdateGroupAction -> TestTree
requestBatchPutScheduledUpdateGroupAction =
  req
    "BatchPutScheduledUpdateGroupAction"
    "fixture/BatchPutScheduledUpdateGroupAction.yaml"

requestDeleteLifecycleHook :: DeleteLifecycleHook -> TestTree
requestDeleteLifecycleHook =
  req
    "DeleteLifecycleHook"
    "fixture/DeleteLifecycleHook.yaml"

requestResumeProcesses :: ResumeProcesses -> TestTree
requestResumeProcesses =
  req
    "ResumeProcesses"
    "fixture/ResumeProcesses.yaml"

requestExecutePolicy :: ExecutePolicy -> TestTree
requestExecutePolicy =
  req
    "ExecutePolicy"
    "fixture/ExecutePolicy.yaml"

requestDescribeInstanceRefreshes :: DescribeInstanceRefreshes -> TestTree
requestDescribeInstanceRefreshes =
  req
    "DescribeInstanceRefreshes"
    "fixture/DescribeInstanceRefreshes.yaml"

requestDescribeAccountLimits :: DescribeAccountLimits -> TestTree
requestDescribeAccountLimits =
  req
    "DescribeAccountLimits"
    "fixture/DescribeAccountLimits.yaml"

requestAttachLoadBalancers :: AttachLoadBalancers -> TestTree
requestAttachLoadBalancers =
  req
    "AttachLoadBalancers"
    "fixture/AttachLoadBalancers.yaml"

requestBatchDeleteScheduledAction :: BatchDeleteScheduledAction -> TestTree
requestBatchDeleteScheduledAction =
  req
    "BatchDeleteScheduledAction"
    "fixture/BatchDeleteScheduledAction.yaml"

requestTerminateInstanceInAutoScalingGroup :: TerminateInstanceInAutoScalingGroup -> TestTree
requestTerminateInstanceInAutoScalingGroup =
  req
    "TerminateInstanceInAutoScalingGroup"
    "fixture/TerminateInstanceInAutoScalingGroup.yaml"

requestDescribeLoadBalancerTargetGroups :: DescribeLoadBalancerTargetGroups -> TestTree
requestDescribeLoadBalancerTargetGroups =
  req
    "DescribeLoadBalancerTargetGroups"
    "fixture/DescribeLoadBalancerTargetGroups.yaml"

requestPutScheduledUpdateGroupAction :: PutScheduledUpdateGroupAction -> TestTree
requestPutScheduledUpdateGroupAction =
  req
    "PutScheduledUpdateGroupAction"
    "fixture/PutScheduledUpdateGroupAction.yaml"

requestSetInstanceProtection :: SetInstanceProtection -> TestTree
requestSetInstanceProtection =
  req
    "SetInstanceProtection"
    "fixture/SetInstanceProtection.yaml"

requestDescribePolicies :: DescribePolicies -> TestTree
requestDescribePolicies =
  req
    "DescribePolicies"
    "fixture/DescribePolicies.yaml"

requestDescribeLaunchConfigurations :: DescribeLaunchConfigurations -> TestTree
requestDescribeLaunchConfigurations =
  req
    "DescribeLaunchConfigurations"
    "fixture/DescribeLaunchConfigurations.yaml"

requestDescribeScalingActivities :: DescribeScalingActivities -> TestTree
requestDescribeScalingActivities =
  req
    "DescribeScalingActivities"
    "fixture/DescribeScalingActivities.yaml"

requestDescribeNotificationConfigurations :: DescribeNotificationConfigurations -> TestTree
requestDescribeNotificationConfigurations =
  req
    "DescribeNotificationConfigurations"
    "fixture/DescribeNotificationConfigurations.yaml"

requestDescribeLifecycleHookTypes :: DescribeLifecycleHookTypes -> TestTree
requestDescribeLifecycleHookTypes =
  req
    "DescribeLifecycleHookTypes"
    "fixture/DescribeLifecycleHookTypes.yaml"

requestDescribeAdjustmentTypes :: DescribeAdjustmentTypes -> TestTree
requestDescribeAdjustmentTypes =
  req
    "DescribeAdjustmentTypes"
    "fixture/DescribeAdjustmentTypes.yaml"

requestCreateAutoScalingGroup :: CreateAutoScalingGroup -> TestTree
requestCreateAutoScalingGroup =
  req
    "CreateAutoScalingGroup"
    "fixture/CreateAutoScalingGroup.yaml"

requestCreateLaunchConfiguration :: CreateLaunchConfiguration -> TestTree
requestCreateLaunchConfiguration =
  req
    "CreateLaunchConfiguration"
    "fixture/CreateLaunchConfiguration.yaml"

-- Responses

responseDescribeMetricCollectionTypes :: DescribeMetricCollectionTypesResponse -> TestTree
responseDescribeMetricCollectionTypes =
  res
    "DescribeMetricCollectionTypesResponse"
    "fixture/DescribeMetricCollectionTypesResponse.proto"
    autoScalingService
    (Proxy :: Proxy DescribeMetricCollectionTypes)

responseDescribeLoadBalancers :: DescribeLoadBalancersResponse -> TestTree
responseDescribeLoadBalancers =
  res
    "DescribeLoadBalancersResponse"
    "fixture/DescribeLoadBalancersResponse.proto"
    autoScalingService
    (Proxy :: Proxy DescribeLoadBalancers)

responsePutNotificationConfiguration :: PutNotificationConfigurationResponse -> TestTree
responsePutNotificationConfiguration =
  res
    "PutNotificationConfigurationResponse"
    "fixture/PutNotificationConfigurationResponse.proto"
    autoScalingService
    (Proxy :: Proxy PutNotificationConfiguration)

responseDescribeTags :: DescribeTagsResponse -> TestTree
responseDescribeTags =
  res
    "DescribeTagsResponse"
    "fixture/DescribeTagsResponse.proto"
    autoScalingService
    (Proxy :: Proxy DescribeTags)

responseDeleteNotificationConfiguration :: DeleteNotificationConfigurationResponse -> TestTree
responseDeleteNotificationConfiguration =
  res
    "DeleteNotificationConfigurationResponse"
    "fixture/DeleteNotificationConfigurationResponse.proto"
    autoScalingService
    (Proxy :: Proxy DeleteNotificationConfiguration)

responsePutScalingPolicy :: PutScalingPolicyResponse -> TestTree
responsePutScalingPolicy =
  res
    "PutScalingPolicyResponse"
    "fixture/PutScalingPolicyResponse.proto"
    autoScalingService
    (Proxy :: Proxy PutScalingPolicy)

responseStartInstanceRefresh :: StartInstanceRefreshResponse -> TestTree
responseStartInstanceRefresh =
  res
    "StartInstanceRefreshResponse"
    "fixture/StartInstanceRefreshResponse.proto"
    autoScalingService
    (Proxy :: Proxy StartInstanceRefresh)

responseAttachLoadBalancerTargetGroups :: AttachLoadBalancerTargetGroupsResponse -> TestTree
responseAttachLoadBalancerTargetGroups =
  res
    "AttachLoadBalancerTargetGroupsResponse"
    "fixture/AttachLoadBalancerTargetGroupsResponse.proto"
    autoScalingService
    (Proxy :: Proxy AttachLoadBalancerTargetGroups)

responseDeleteLaunchConfiguration :: DeleteLaunchConfigurationResponse -> TestTree
responseDeleteLaunchConfiguration =
  res
    "DeleteLaunchConfigurationResponse"
    "fixture/DeleteLaunchConfigurationResponse.proto"
    autoScalingService
    (Proxy :: Proxy DeleteLaunchConfiguration)

responseEnterStandby :: EnterStandbyResponse -> TestTree
responseEnterStandby =
  res
    "EnterStandbyResponse"
    "fixture/EnterStandbyResponse.proto"
    autoScalingService
    (Proxy :: Proxy EnterStandby)

responseSuspendProcesses :: SuspendProcessesResponse -> TestTree
responseSuspendProcesses =
  res
    "SuspendProcessesResponse"
    "fixture/SuspendProcessesResponse.proto"
    autoScalingService
    (Proxy :: Proxy SuspendProcesses)

responseSetInstanceHealth :: SetInstanceHealthResponse -> TestTree
responseSetInstanceHealth =
  res
    "SetInstanceHealthResponse"
    "fixture/SetInstanceHealthResponse.proto"
    autoScalingService
    (Proxy :: Proxy SetInstanceHealth)

responseExitStandby :: ExitStandbyResponse -> TestTree
responseExitStandby =
  res
    "ExitStandbyResponse"
    "fixture/ExitStandbyResponse.proto"
    autoScalingService
    (Proxy :: Proxy ExitStandby)

responseDescribeTerminationPolicyTypes :: DescribeTerminationPolicyTypesResponse -> TestTree
responseDescribeTerminationPolicyTypes =
  res
    "DescribeTerminationPolicyTypesResponse"
    "fixture/DescribeTerminationPolicyTypesResponse.proto"
    autoScalingService
    (Proxy :: Proxy DescribeTerminationPolicyTypes)

responseCancelInstanceRefresh :: CancelInstanceRefreshResponse -> TestTree
responseCancelInstanceRefresh =
  res
    "CancelInstanceRefreshResponse"
    "fixture/CancelInstanceRefreshResponse.proto"
    autoScalingService
    (Proxy :: Proxy CancelInstanceRefresh)

responseDescribeAutoScalingInstances :: DescribeAutoScalingInstancesResponse -> TestTree
responseDescribeAutoScalingInstances =
  res
    "DescribeAutoScalingInstancesResponse"
    "fixture/DescribeAutoScalingInstancesResponse.proto"
    autoScalingService
    (Proxy :: Proxy DescribeAutoScalingInstances)

responseRecordLifecycleActionHeartbeat :: RecordLifecycleActionHeartbeatResponse -> TestTree
responseRecordLifecycleActionHeartbeat =
  res
    "RecordLifecycleActionHeartbeatResponse"
    "fixture/RecordLifecycleActionHeartbeatResponse.proto"
    autoScalingService
    (Proxy :: Proxy RecordLifecycleActionHeartbeat)

responseDisableMetricsCollection :: DisableMetricsCollectionResponse -> TestTree
responseDisableMetricsCollection =
  res
    "DisableMetricsCollectionResponse"
    "fixture/DisableMetricsCollectionResponse.proto"
    autoScalingService
    (Proxy :: Proxy DisableMetricsCollection)

responseDetachInstances :: DetachInstancesResponse -> TestTree
responseDetachInstances =
  res
    "DetachInstancesResponse"
    "fixture/DetachInstancesResponse.proto"
    autoScalingService
    (Proxy :: Proxy DetachInstances)

responseEnableMetricsCollection :: EnableMetricsCollectionResponse -> TestTree
responseEnableMetricsCollection =
  res
    "EnableMetricsCollectionResponse"
    "fixture/EnableMetricsCollectionResponse.proto"
    autoScalingService
    (Proxy :: Proxy EnableMetricsCollection)

responseDescribeScalingProcessTypes :: DescribeScalingProcessTypesResponse -> TestTree
responseDescribeScalingProcessTypes =
  res
    "DescribeScalingProcessTypesResponse"
    "fixture/DescribeScalingProcessTypesResponse.proto"
    autoScalingService
    (Proxy :: Proxy DescribeScalingProcessTypes)

responseDeleteTags :: DeleteTagsResponse -> TestTree
responseDeleteTags =
  res
    "DeleteTagsResponse"
    "fixture/DeleteTagsResponse.proto"
    autoScalingService
    (Proxy :: Proxy DeleteTags)

responseDetachLoadBalancerTargetGroups :: DetachLoadBalancerTargetGroupsResponse -> TestTree
responseDetachLoadBalancerTargetGroups =
  res
    "DetachLoadBalancerTargetGroupsResponse"
    "fixture/DetachLoadBalancerTargetGroupsResponse.proto"
    autoScalingService
    (Proxy :: Proxy DetachLoadBalancerTargetGroups)

responseDescribeLifecycleHooks :: DescribeLifecycleHooksResponse -> TestTree
responseDescribeLifecycleHooks =
  res
    "DescribeLifecycleHooksResponse"
    "fixture/DescribeLifecycleHooksResponse.proto"
    autoScalingService
    (Proxy :: Proxy DescribeLifecycleHooks)

responseDescribeAutoScalingGroups :: DescribeAutoScalingGroupsResponse -> TestTree
responseDescribeAutoScalingGroups =
  res
    "DescribeAutoScalingGroupsResponse"
    "fixture/DescribeAutoScalingGroupsResponse.proto"
    autoScalingService
    (Proxy :: Proxy DescribeAutoScalingGroups)

responseDeleteScheduledAction :: DeleteScheduledActionResponse -> TestTree
responseDeleteScheduledAction =
  res
    "DeleteScheduledActionResponse"
    "fixture/DeleteScheduledActionResponse.proto"
    autoScalingService
    (Proxy :: Proxy DeleteScheduledAction)

responseSetDesiredCapacity :: SetDesiredCapacityResponse -> TestTree
responseSetDesiredCapacity =
  res
    "SetDesiredCapacityResponse"
    "fixture/SetDesiredCapacityResponse.proto"
    autoScalingService
    (Proxy :: Proxy SetDesiredCapacity)

responseDetachLoadBalancers :: DetachLoadBalancersResponse -> TestTree
responseDetachLoadBalancers =
  res
    "DetachLoadBalancersResponse"
    "fixture/DetachLoadBalancersResponse.proto"
    autoScalingService
    (Proxy :: Proxy DetachLoadBalancers)

responseDescribeAutoScalingNotificationTypes :: DescribeAutoScalingNotificationTypesResponse -> TestTree
responseDescribeAutoScalingNotificationTypes =
  res
    "DescribeAutoScalingNotificationTypesResponse"
    "fixture/DescribeAutoScalingNotificationTypesResponse.proto"
    autoScalingService
    (Proxy :: Proxy DescribeAutoScalingNotificationTypes)

responseDescribeScheduledActions :: DescribeScheduledActionsResponse -> TestTree
responseDescribeScheduledActions =
  res
    "DescribeScheduledActionsResponse"
    "fixture/DescribeScheduledActionsResponse.proto"
    autoScalingService
    (Proxy :: Proxy DescribeScheduledActions)

responseCreateOrUpdateTags :: CreateOrUpdateTagsResponse -> TestTree
responseCreateOrUpdateTags =
  res
    "CreateOrUpdateTagsResponse"
    "fixture/CreateOrUpdateTagsResponse.proto"
    autoScalingService
    (Proxy :: Proxy CreateOrUpdateTags)

responseCompleteLifecycleAction :: CompleteLifecycleActionResponse -> TestTree
responseCompleteLifecycleAction =
  res
    "CompleteLifecycleActionResponse"
    "fixture/CompleteLifecycleActionResponse.proto"
    autoScalingService
    (Proxy :: Proxy CompleteLifecycleAction)

responseDeletePolicy :: DeletePolicyResponse -> TestTree
responseDeletePolicy =
  res
    "DeletePolicyResponse"
    "fixture/DeletePolicyResponse.proto"
    autoScalingService
    (Proxy :: Proxy DeletePolicy)

responseAttachInstances :: AttachInstancesResponse -> TestTree
responseAttachInstances =
  res
    "AttachInstancesResponse"
    "fixture/AttachInstancesResponse.proto"
    autoScalingService
    (Proxy :: Proxy AttachInstances)

responseUpdateAutoScalingGroup :: UpdateAutoScalingGroupResponse -> TestTree
responseUpdateAutoScalingGroup =
  res
    "UpdateAutoScalingGroupResponse"
    "fixture/UpdateAutoScalingGroupResponse.proto"
    autoScalingService
    (Proxy :: Proxy UpdateAutoScalingGroup)

responseDeleteAutoScalingGroup :: DeleteAutoScalingGroupResponse -> TestTree
responseDeleteAutoScalingGroup =
  res
    "DeleteAutoScalingGroupResponse"
    "fixture/DeleteAutoScalingGroupResponse.proto"
    autoScalingService
    (Proxy :: Proxy DeleteAutoScalingGroup)

responsePutLifecycleHook :: PutLifecycleHookResponse -> TestTree
responsePutLifecycleHook =
  res
    "PutLifecycleHookResponse"
    "fixture/PutLifecycleHookResponse.proto"
    autoScalingService
    (Proxy :: Proxy PutLifecycleHook)

responseBatchPutScheduledUpdateGroupAction :: BatchPutScheduledUpdateGroupActionResponse -> TestTree
responseBatchPutScheduledUpdateGroupAction =
  res
    "BatchPutScheduledUpdateGroupActionResponse"
    "fixture/BatchPutScheduledUpdateGroupActionResponse.proto"
    autoScalingService
    (Proxy :: Proxy BatchPutScheduledUpdateGroupAction)

responseDeleteLifecycleHook :: DeleteLifecycleHookResponse -> TestTree
responseDeleteLifecycleHook =
  res
    "DeleteLifecycleHookResponse"
    "fixture/DeleteLifecycleHookResponse.proto"
    autoScalingService
    (Proxy :: Proxy DeleteLifecycleHook)

responseResumeProcesses :: ResumeProcessesResponse -> TestTree
responseResumeProcesses =
  res
    "ResumeProcessesResponse"
    "fixture/ResumeProcessesResponse.proto"
    autoScalingService
    (Proxy :: Proxy ResumeProcesses)

responseExecutePolicy :: ExecutePolicyResponse -> TestTree
responseExecutePolicy =
  res
    "ExecutePolicyResponse"
    "fixture/ExecutePolicyResponse.proto"
    autoScalingService
    (Proxy :: Proxy ExecutePolicy)

responseDescribeInstanceRefreshes :: DescribeInstanceRefreshesResponse -> TestTree
responseDescribeInstanceRefreshes =
  res
    "DescribeInstanceRefreshesResponse"
    "fixture/DescribeInstanceRefreshesResponse.proto"
    autoScalingService
    (Proxy :: Proxy DescribeInstanceRefreshes)

responseDescribeAccountLimits :: DescribeAccountLimitsResponse -> TestTree
responseDescribeAccountLimits =
  res
    "DescribeAccountLimitsResponse"
    "fixture/DescribeAccountLimitsResponse.proto"
    autoScalingService
    (Proxy :: Proxy DescribeAccountLimits)

responseAttachLoadBalancers :: AttachLoadBalancersResponse -> TestTree
responseAttachLoadBalancers =
  res
    "AttachLoadBalancersResponse"
    "fixture/AttachLoadBalancersResponse.proto"
    autoScalingService
    (Proxy :: Proxy AttachLoadBalancers)

responseBatchDeleteScheduledAction :: BatchDeleteScheduledActionResponse -> TestTree
responseBatchDeleteScheduledAction =
  res
    "BatchDeleteScheduledActionResponse"
    "fixture/BatchDeleteScheduledActionResponse.proto"
    autoScalingService
    (Proxy :: Proxy BatchDeleteScheduledAction)

responseTerminateInstanceInAutoScalingGroup :: TerminateInstanceInAutoScalingGroupResponse -> TestTree
responseTerminateInstanceInAutoScalingGroup =
  res
    "TerminateInstanceInAutoScalingGroupResponse"
    "fixture/TerminateInstanceInAutoScalingGroupResponse.proto"
    autoScalingService
    (Proxy :: Proxy TerminateInstanceInAutoScalingGroup)

responseDescribeLoadBalancerTargetGroups :: DescribeLoadBalancerTargetGroupsResponse -> TestTree
responseDescribeLoadBalancerTargetGroups =
  res
    "DescribeLoadBalancerTargetGroupsResponse"
    "fixture/DescribeLoadBalancerTargetGroupsResponse.proto"
    autoScalingService
    (Proxy :: Proxy DescribeLoadBalancerTargetGroups)

responsePutScheduledUpdateGroupAction :: PutScheduledUpdateGroupActionResponse -> TestTree
responsePutScheduledUpdateGroupAction =
  res
    "PutScheduledUpdateGroupActionResponse"
    "fixture/PutScheduledUpdateGroupActionResponse.proto"
    autoScalingService
    (Proxy :: Proxy PutScheduledUpdateGroupAction)

responseSetInstanceProtection :: SetInstanceProtectionResponse -> TestTree
responseSetInstanceProtection =
  res
    "SetInstanceProtectionResponse"
    "fixture/SetInstanceProtectionResponse.proto"
    autoScalingService
    (Proxy :: Proxy SetInstanceProtection)

responseDescribePolicies :: DescribePoliciesResponse -> TestTree
responseDescribePolicies =
  res
    "DescribePoliciesResponse"
    "fixture/DescribePoliciesResponse.proto"
    autoScalingService
    (Proxy :: Proxy DescribePolicies)

responseDescribeLaunchConfigurations :: DescribeLaunchConfigurationsResponse -> TestTree
responseDescribeLaunchConfigurations =
  res
    "DescribeLaunchConfigurationsResponse"
    "fixture/DescribeLaunchConfigurationsResponse.proto"
    autoScalingService
    (Proxy :: Proxy DescribeLaunchConfigurations)

responseDescribeScalingActivities :: DescribeScalingActivitiesResponse -> TestTree
responseDescribeScalingActivities =
  res
    "DescribeScalingActivitiesResponse"
    "fixture/DescribeScalingActivitiesResponse.proto"
    autoScalingService
    (Proxy :: Proxy DescribeScalingActivities)

responseDescribeNotificationConfigurations :: DescribeNotificationConfigurationsResponse -> TestTree
responseDescribeNotificationConfigurations =
  res
    "DescribeNotificationConfigurationsResponse"
    "fixture/DescribeNotificationConfigurationsResponse.proto"
    autoScalingService
    (Proxy :: Proxy DescribeNotificationConfigurations)

responseDescribeLifecycleHookTypes :: DescribeLifecycleHookTypesResponse -> TestTree
responseDescribeLifecycleHookTypes =
  res
    "DescribeLifecycleHookTypesResponse"
    "fixture/DescribeLifecycleHookTypesResponse.proto"
    autoScalingService
    (Proxy :: Proxy DescribeLifecycleHookTypes)

responseDescribeAdjustmentTypes :: DescribeAdjustmentTypesResponse -> TestTree
responseDescribeAdjustmentTypes =
  res
    "DescribeAdjustmentTypesResponse"
    "fixture/DescribeAdjustmentTypesResponse.proto"
    autoScalingService
    (Proxy :: Proxy DescribeAdjustmentTypes)

responseCreateAutoScalingGroup :: CreateAutoScalingGroupResponse -> TestTree
responseCreateAutoScalingGroup =
  res
    "CreateAutoScalingGroupResponse"
    "fixture/CreateAutoScalingGroupResponse.proto"
    autoScalingService
    (Proxy :: Proxy CreateAutoScalingGroup)

responseCreateLaunchConfiguration :: CreateLaunchConfigurationResponse -> TestTree
responseCreateLaunchConfiguration =
  res
    "CreateLaunchConfigurationResponse"
    "fixture/CreateLaunchConfigurationResponse.proto"
    autoScalingService
    (Proxy :: Proxy CreateLaunchConfiguration)
