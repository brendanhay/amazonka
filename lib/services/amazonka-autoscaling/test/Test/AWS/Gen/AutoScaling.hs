{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.AutoScaling
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.AWS.Gen.AutoScaling where

import qualified Data.Proxy as Proxy
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
--         [ requestPutWarmPool $
--             newPutWarmPool
--
--         , requestDescribeMetricCollectionTypes $
--             newDescribeMetricCollectionTypes
--
--         , requestDescribeLoadBalancers $
--             newDescribeLoadBalancers
--
--         , requestPutNotificationConfiguration $
--             newPutNotificationConfiguration
--
--         , requestDescribeTags $
--             newDescribeTags
--
--         , requestDeleteNotificationConfiguration $
--             newDeleteNotificationConfiguration
--
--         , requestDeleteWarmPool $
--             newDeleteWarmPool
--
--         , requestPutScalingPolicy $
--             newPutScalingPolicy
--
--         , requestStartInstanceRefresh $
--             newStartInstanceRefresh
--
--         , requestAttachLoadBalancerTargetGroups $
--             newAttachLoadBalancerTargetGroups
--
--         , requestDeleteLaunchConfiguration $
--             newDeleteLaunchConfiguration
--
--         , requestEnterStandby $
--             newEnterStandby
--
--         , requestSuspendProcesses $
--             newSuspendProcesses
--
--         , requestSetInstanceHealth $
--             newSetInstanceHealth
--
--         , requestExitStandby $
--             newExitStandby
--
--         , requestDescribeTerminationPolicyTypes $
--             newDescribeTerminationPolicyTypes
--
--         , requestCancelInstanceRefresh $
--             newCancelInstanceRefresh
--
--         , requestDescribeAutoScalingInstances $
--             newDescribeAutoScalingInstances
--
--         , requestRecordLifecycleActionHeartbeat $
--             newRecordLifecycleActionHeartbeat
--
--         , requestDisableMetricsCollection $
--             newDisableMetricsCollection
--
--         , requestDetachInstances $
--             newDetachInstances
--
--         , requestEnableMetricsCollection $
--             newEnableMetricsCollection
--
--         , requestDescribeScalingProcessTypes $
--             newDescribeScalingProcessTypes
--
--         , requestDescribeWarmPool $
--             newDescribeWarmPool
--
--         , requestDeleteTags $
--             newDeleteTags
--
--         , requestDetachLoadBalancerTargetGroups $
--             newDetachLoadBalancerTargetGroups
--
--         , requestDescribeLifecycleHooks $
--             newDescribeLifecycleHooks
--
--         , requestDescribeAutoScalingGroups $
--             newDescribeAutoScalingGroups
--
--         , requestDeleteScheduledAction $
--             newDeleteScheduledAction
--
--         , requestSetDesiredCapacity $
--             newSetDesiredCapacity
--
--         , requestDetachLoadBalancers $
--             newDetachLoadBalancers
--
--         , requestDescribeAutoScalingNotificationTypes $
--             newDescribeAutoScalingNotificationTypes
--
--         , requestDescribeScheduledActions $
--             newDescribeScheduledActions
--
--         , requestCreateOrUpdateTags $
--             newCreateOrUpdateTags
--
--         , requestCompleteLifecycleAction $
--             newCompleteLifecycleAction
--
--         , requestDeletePolicy $
--             newDeletePolicy
--
--         , requestAttachInstances $
--             newAttachInstances
--
--         , requestUpdateAutoScalingGroup $
--             newUpdateAutoScalingGroup
--
--         , requestDeleteAutoScalingGroup $
--             newDeleteAutoScalingGroup
--
--         , requestPutLifecycleHook $
--             newPutLifecycleHook
--
--         , requestBatchPutScheduledUpdateGroupAction $
--             newBatchPutScheduledUpdateGroupAction
--
--         , requestDeleteLifecycleHook $
--             newDeleteLifecycleHook
--
--         , requestResumeProcesses $
--             newResumeProcesses
--
--         , requestExecutePolicy $
--             newExecutePolicy
--
--         , requestGetPredictiveScalingForecast $
--             newGetPredictiveScalingForecast
--
--         , requestDescribeInstanceRefreshes $
--             newDescribeInstanceRefreshes
--
--         , requestDescribeAccountLimits $
--             newDescribeAccountLimits
--
--         , requestAttachLoadBalancers $
--             newAttachLoadBalancers
--
--         , requestBatchDeleteScheduledAction $
--             newBatchDeleteScheduledAction
--
--         , requestTerminateInstanceInAutoScalingGroup $
--             newTerminateInstanceInAutoScalingGroup
--
--         , requestDescribeLoadBalancerTargetGroups $
--             newDescribeLoadBalancerTargetGroups
--
--         , requestPutScheduledUpdateGroupAction $
--             newPutScheduledUpdateGroupAction
--
--         , requestSetInstanceProtection $
--             newSetInstanceProtection
--
--         , requestDescribePolicies $
--             newDescribePolicies
--
--         , requestDescribeLaunchConfigurations $
--             newDescribeLaunchConfigurations
--
--         , requestDescribeScalingActivities $
--             newDescribeScalingActivities
--
--         , requestDescribeNotificationConfigurations $
--             newDescribeNotificationConfigurations
--
--         , requestDescribeLifecycleHookTypes $
--             newDescribeLifecycleHookTypes
--
--         , requestDescribeAdjustmentTypes $
--             newDescribeAdjustmentTypes
--
--         , requestCreateAutoScalingGroup $
--             newCreateAutoScalingGroup
--
--         , requestCreateLaunchConfiguration $
--             newCreateLaunchConfiguration
--
--           ]

--     , testGroup "response"
--         [ responsePutWarmPool $
--             newPutWarmPoolResponse
--
--         , responseDescribeMetricCollectionTypes $
--             newDescribeMetricCollectionTypesResponse
--
--         , responseDescribeLoadBalancers $
--             newDescribeLoadBalancersResponse
--
--         , responsePutNotificationConfiguration $
--             newPutNotificationConfigurationResponse
--
--         , responseDescribeTags $
--             newDescribeTagsResponse
--
--         , responseDeleteNotificationConfiguration $
--             newDeleteNotificationConfigurationResponse
--
--         , responseDeleteWarmPool $
--             newDeleteWarmPoolResponse
--
--         , responsePutScalingPolicy $
--             newPutScalingPolicyResponse
--
--         , responseStartInstanceRefresh $
--             newStartInstanceRefreshResponse
--
--         , responseAttachLoadBalancerTargetGroups $
--             newAttachLoadBalancerTargetGroupsResponse
--
--         , responseDeleteLaunchConfiguration $
--             newDeleteLaunchConfigurationResponse
--
--         , responseEnterStandby $
--             newEnterStandbyResponse
--
--         , responseSuspendProcesses $
--             newSuspendProcessesResponse
--
--         , responseSetInstanceHealth $
--             newSetInstanceHealthResponse
--
--         , responseExitStandby $
--             newExitStandbyResponse
--
--         , responseDescribeTerminationPolicyTypes $
--             newDescribeTerminationPolicyTypesResponse
--
--         , responseCancelInstanceRefresh $
--             newCancelInstanceRefreshResponse
--
--         , responseDescribeAutoScalingInstances $
--             newDescribeAutoScalingInstancesResponse
--
--         , responseRecordLifecycleActionHeartbeat $
--             newRecordLifecycleActionHeartbeatResponse
--
--         , responseDisableMetricsCollection $
--             newDisableMetricsCollectionResponse
--
--         , responseDetachInstances $
--             newDetachInstancesResponse
--
--         , responseEnableMetricsCollection $
--             newEnableMetricsCollectionResponse
--
--         , responseDescribeScalingProcessTypes $
--             newDescribeScalingProcessTypesResponse
--
--         , responseDescribeWarmPool $
--             newDescribeWarmPoolResponse
--
--         , responseDeleteTags $
--             newDeleteTagsResponse
--
--         , responseDetachLoadBalancerTargetGroups $
--             newDetachLoadBalancerTargetGroupsResponse
--
--         , responseDescribeLifecycleHooks $
--             newDescribeLifecycleHooksResponse
--
--         , responseDescribeAutoScalingGroups $
--             newDescribeAutoScalingGroupsResponse
--
--         , responseDeleteScheduledAction $
--             newDeleteScheduledActionResponse
--
--         , responseSetDesiredCapacity $
--             newSetDesiredCapacityResponse
--
--         , responseDetachLoadBalancers $
--             newDetachLoadBalancersResponse
--
--         , responseDescribeAutoScalingNotificationTypes $
--             newDescribeAutoScalingNotificationTypesResponse
--
--         , responseDescribeScheduledActions $
--             newDescribeScheduledActionsResponse
--
--         , responseCreateOrUpdateTags $
--             newCreateOrUpdateTagsResponse
--
--         , responseCompleteLifecycleAction $
--             newCompleteLifecycleActionResponse
--
--         , responseDeletePolicy $
--             newDeletePolicyResponse
--
--         , responseAttachInstances $
--             newAttachInstancesResponse
--
--         , responseUpdateAutoScalingGroup $
--             newUpdateAutoScalingGroupResponse
--
--         , responseDeleteAutoScalingGroup $
--             newDeleteAutoScalingGroupResponse
--
--         , responsePutLifecycleHook $
--             newPutLifecycleHookResponse
--
--         , responseBatchPutScheduledUpdateGroupAction $
--             newBatchPutScheduledUpdateGroupActionResponse
--
--         , responseDeleteLifecycleHook $
--             newDeleteLifecycleHookResponse
--
--         , responseResumeProcesses $
--             newResumeProcessesResponse
--
--         , responseExecutePolicy $
--             newExecutePolicyResponse
--
--         , responseGetPredictiveScalingForecast $
--             newGetPredictiveScalingForecastResponse
--
--         , responseDescribeInstanceRefreshes $
--             newDescribeInstanceRefreshesResponse
--
--         , responseDescribeAccountLimits $
--             newDescribeAccountLimitsResponse
--
--         , responseAttachLoadBalancers $
--             newAttachLoadBalancersResponse
--
--         , responseBatchDeleteScheduledAction $
--             newBatchDeleteScheduledActionResponse
--
--         , responseTerminateInstanceInAutoScalingGroup $
--             newTerminateInstanceInAutoScalingGroupResponse
--
--         , responseDescribeLoadBalancerTargetGroups $
--             newDescribeLoadBalancerTargetGroupsResponse
--
--         , responsePutScheduledUpdateGroupAction $
--             newPutScheduledUpdateGroupActionResponse
--
--         , responseSetInstanceProtection $
--             newSetInstanceProtectionResponse
--
--         , responseDescribePolicies $
--             newDescribePoliciesResponse
--
--         , responseDescribeLaunchConfigurations $
--             newDescribeLaunchConfigurationsResponse
--
--         , responseDescribeScalingActivities $
--             newDescribeScalingActivitiesResponse
--
--         , responseDescribeNotificationConfigurations $
--             newDescribeNotificationConfigurationsResponse
--
--         , responseDescribeLifecycleHookTypes $
--             newDescribeLifecycleHookTypesResponse
--
--         , responseDescribeAdjustmentTypes $
--             newDescribeAdjustmentTypesResponse
--
--         , responseCreateAutoScalingGroup $
--             newCreateAutoScalingGroupResponse
--
--         , responseCreateLaunchConfiguration $
--             newCreateLaunchConfigurationResponse
--
--           ]
--     ]

-- Requests

requestPutWarmPool :: PutWarmPool -> TestTree
requestPutWarmPool =
  req
    "PutWarmPool"
    "fixture/PutWarmPool.yaml"

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

requestDeleteWarmPool :: DeleteWarmPool -> TestTree
requestDeleteWarmPool =
  req
    "DeleteWarmPool"
    "fixture/DeleteWarmPool.yaml"

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

requestDescribeWarmPool :: DescribeWarmPool -> TestTree
requestDescribeWarmPool =
  req
    "DescribeWarmPool"
    "fixture/DescribeWarmPool.yaml"

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

requestGetPredictiveScalingForecast :: GetPredictiveScalingForecast -> TestTree
requestGetPredictiveScalingForecast =
  req
    "GetPredictiveScalingForecast"
    "fixture/GetPredictiveScalingForecast.yaml"

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

responsePutWarmPool :: PutWarmPoolResponse -> TestTree
responsePutWarmPool =
  res
    "PutWarmPoolResponse"
    "fixture/PutWarmPoolResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutWarmPool)

responseDescribeMetricCollectionTypes :: DescribeMetricCollectionTypesResponse -> TestTree
responseDescribeMetricCollectionTypes =
  res
    "DescribeMetricCollectionTypesResponse"
    "fixture/DescribeMetricCollectionTypesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeMetricCollectionTypes)

responseDescribeLoadBalancers :: DescribeLoadBalancersResponse -> TestTree
responseDescribeLoadBalancers =
  res
    "DescribeLoadBalancersResponse"
    "fixture/DescribeLoadBalancersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeLoadBalancers)

responsePutNotificationConfiguration :: PutNotificationConfigurationResponse -> TestTree
responsePutNotificationConfiguration =
  res
    "PutNotificationConfigurationResponse"
    "fixture/PutNotificationConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutNotificationConfiguration)

responseDescribeTags :: DescribeTagsResponse -> TestTree
responseDescribeTags =
  res
    "DescribeTagsResponse"
    "fixture/DescribeTagsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeTags)

responseDeleteNotificationConfiguration :: DeleteNotificationConfigurationResponse -> TestTree
responseDeleteNotificationConfiguration =
  res
    "DeleteNotificationConfigurationResponse"
    "fixture/DeleteNotificationConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteNotificationConfiguration)

responseDeleteWarmPool :: DeleteWarmPoolResponse -> TestTree
responseDeleteWarmPool =
  res
    "DeleteWarmPoolResponse"
    "fixture/DeleteWarmPoolResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteWarmPool)

responsePutScalingPolicy :: PutScalingPolicyResponse -> TestTree
responsePutScalingPolicy =
  res
    "PutScalingPolicyResponse"
    "fixture/PutScalingPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutScalingPolicy)

responseStartInstanceRefresh :: StartInstanceRefreshResponse -> TestTree
responseStartInstanceRefresh =
  res
    "StartInstanceRefreshResponse"
    "fixture/StartInstanceRefreshResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartInstanceRefresh)

responseAttachLoadBalancerTargetGroups :: AttachLoadBalancerTargetGroupsResponse -> TestTree
responseAttachLoadBalancerTargetGroups =
  res
    "AttachLoadBalancerTargetGroupsResponse"
    "fixture/AttachLoadBalancerTargetGroupsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AttachLoadBalancerTargetGroups)

responseDeleteLaunchConfiguration :: DeleteLaunchConfigurationResponse -> TestTree
responseDeleteLaunchConfiguration =
  res
    "DeleteLaunchConfigurationResponse"
    "fixture/DeleteLaunchConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteLaunchConfiguration)

responseEnterStandby :: EnterStandbyResponse -> TestTree
responseEnterStandby =
  res
    "EnterStandbyResponse"
    "fixture/EnterStandbyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy EnterStandby)

responseSuspendProcesses :: SuspendProcessesResponse -> TestTree
responseSuspendProcesses =
  res
    "SuspendProcessesResponse"
    "fixture/SuspendProcessesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SuspendProcesses)

responseSetInstanceHealth :: SetInstanceHealthResponse -> TestTree
responseSetInstanceHealth =
  res
    "SetInstanceHealthResponse"
    "fixture/SetInstanceHealthResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SetInstanceHealth)

responseExitStandby :: ExitStandbyResponse -> TestTree
responseExitStandby =
  res
    "ExitStandbyResponse"
    "fixture/ExitStandbyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ExitStandby)

responseDescribeTerminationPolicyTypes :: DescribeTerminationPolicyTypesResponse -> TestTree
responseDescribeTerminationPolicyTypes =
  res
    "DescribeTerminationPolicyTypesResponse"
    "fixture/DescribeTerminationPolicyTypesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeTerminationPolicyTypes)

responseCancelInstanceRefresh :: CancelInstanceRefreshResponse -> TestTree
responseCancelInstanceRefresh =
  res
    "CancelInstanceRefreshResponse"
    "fixture/CancelInstanceRefreshResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CancelInstanceRefresh)

responseDescribeAutoScalingInstances :: DescribeAutoScalingInstancesResponse -> TestTree
responseDescribeAutoScalingInstances =
  res
    "DescribeAutoScalingInstancesResponse"
    "fixture/DescribeAutoScalingInstancesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeAutoScalingInstances)

responseRecordLifecycleActionHeartbeat :: RecordLifecycleActionHeartbeatResponse -> TestTree
responseRecordLifecycleActionHeartbeat =
  res
    "RecordLifecycleActionHeartbeatResponse"
    "fixture/RecordLifecycleActionHeartbeatResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RecordLifecycleActionHeartbeat)

responseDisableMetricsCollection :: DisableMetricsCollectionResponse -> TestTree
responseDisableMetricsCollection =
  res
    "DisableMetricsCollectionResponse"
    "fixture/DisableMetricsCollectionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisableMetricsCollection)

responseDetachInstances :: DetachInstancesResponse -> TestTree
responseDetachInstances =
  res
    "DetachInstancesResponse"
    "fixture/DetachInstancesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DetachInstances)

responseEnableMetricsCollection :: EnableMetricsCollectionResponse -> TestTree
responseEnableMetricsCollection =
  res
    "EnableMetricsCollectionResponse"
    "fixture/EnableMetricsCollectionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy EnableMetricsCollection)

responseDescribeScalingProcessTypes :: DescribeScalingProcessTypesResponse -> TestTree
responseDescribeScalingProcessTypes =
  res
    "DescribeScalingProcessTypesResponse"
    "fixture/DescribeScalingProcessTypesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeScalingProcessTypes)

responseDescribeWarmPool :: DescribeWarmPoolResponse -> TestTree
responseDescribeWarmPool =
  res
    "DescribeWarmPoolResponse"
    "fixture/DescribeWarmPoolResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeWarmPool)

responseDeleteTags :: DeleteTagsResponse -> TestTree
responseDeleteTags =
  res
    "DeleteTagsResponse"
    "fixture/DeleteTagsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteTags)

responseDetachLoadBalancerTargetGroups :: DetachLoadBalancerTargetGroupsResponse -> TestTree
responseDetachLoadBalancerTargetGroups =
  res
    "DetachLoadBalancerTargetGroupsResponse"
    "fixture/DetachLoadBalancerTargetGroupsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DetachLoadBalancerTargetGroups)

responseDescribeLifecycleHooks :: DescribeLifecycleHooksResponse -> TestTree
responseDescribeLifecycleHooks =
  res
    "DescribeLifecycleHooksResponse"
    "fixture/DescribeLifecycleHooksResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeLifecycleHooks)

responseDescribeAutoScalingGroups :: DescribeAutoScalingGroupsResponse -> TestTree
responseDescribeAutoScalingGroups =
  res
    "DescribeAutoScalingGroupsResponse"
    "fixture/DescribeAutoScalingGroupsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeAutoScalingGroups)

responseDeleteScheduledAction :: DeleteScheduledActionResponse -> TestTree
responseDeleteScheduledAction =
  res
    "DeleteScheduledActionResponse"
    "fixture/DeleteScheduledActionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteScheduledAction)

responseSetDesiredCapacity :: SetDesiredCapacityResponse -> TestTree
responseSetDesiredCapacity =
  res
    "SetDesiredCapacityResponse"
    "fixture/SetDesiredCapacityResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SetDesiredCapacity)

responseDetachLoadBalancers :: DetachLoadBalancersResponse -> TestTree
responseDetachLoadBalancers =
  res
    "DetachLoadBalancersResponse"
    "fixture/DetachLoadBalancersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DetachLoadBalancers)

responseDescribeAutoScalingNotificationTypes :: DescribeAutoScalingNotificationTypesResponse -> TestTree
responseDescribeAutoScalingNotificationTypes =
  res
    "DescribeAutoScalingNotificationTypesResponse"
    "fixture/DescribeAutoScalingNotificationTypesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeAutoScalingNotificationTypes)

responseDescribeScheduledActions :: DescribeScheduledActionsResponse -> TestTree
responseDescribeScheduledActions =
  res
    "DescribeScheduledActionsResponse"
    "fixture/DescribeScheduledActionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeScheduledActions)

responseCreateOrUpdateTags :: CreateOrUpdateTagsResponse -> TestTree
responseCreateOrUpdateTags =
  res
    "CreateOrUpdateTagsResponse"
    "fixture/CreateOrUpdateTagsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateOrUpdateTags)

responseCompleteLifecycleAction :: CompleteLifecycleActionResponse -> TestTree
responseCompleteLifecycleAction =
  res
    "CompleteLifecycleActionResponse"
    "fixture/CompleteLifecycleActionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CompleteLifecycleAction)

responseDeletePolicy :: DeletePolicyResponse -> TestTree
responseDeletePolicy =
  res
    "DeletePolicyResponse"
    "fixture/DeletePolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeletePolicy)

responseAttachInstances :: AttachInstancesResponse -> TestTree
responseAttachInstances =
  res
    "AttachInstancesResponse"
    "fixture/AttachInstancesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AttachInstances)

responseUpdateAutoScalingGroup :: UpdateAutoScalingGroupResponse -> TestTree
responseUpdateAutoScalingGroup =
  res
    "UpdateAutoScalingGroupResponse"
    "fixture/UpdateAutoScalingGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateAutoScalingGroup)

responseDeleteAutoScalingGroup :: DeleteAutoScalingGroupResponse -> TestTree
responseDeleteAutoScalingGroup =
  res
    "DeleteAutoScalingGroupResponse"
    "fixture/DeleteAutoScalingGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteAutoScalingGroup)

responsePutLifecycleHook :: PutLifecycleHookResponse -> TestTree
responsePutLifecycleHook =
  res
    "PutLifecycleHookResponse"
    "fixture/PutLifecycleHookResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutLifecycleHook)

responseBatchPutScheduledUpdateGroupAction :: BatchPutScheduledUpdateGroupActionResponse -> TestTree
responseBatchPutScheduledUpdateGroupAction =
  res
    "BatchPutScheduledUpdateGroupActionResponse"
    "fixture/BatchPutScheduledUpdateGroupActionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchPutScheduledUpdateGroupAction)

responseDeleteLifecycleHook :: DeleteLifecycleHookResponse -> TestTree
responseDeleteLifecycleHook =
  res
    "DeleteLifecycleHookResponse"
    "fixture/DeleteLifecycleHookResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteLifecycleHook)

responseResumeProcesses :: ResumeProcessesResponse -> TestTree
responseResumeProcesses =
  res
    "ResumeProcessesResponse"
    "fixture/ResumeProcessesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ResumeProcesses)

responseExecutePolicy :: ExecutePolicyResponse -> TestTree
responseExecutePolicy =
  res
    "ExecutePolicyResponse"
    "fixture/ExecutePolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ExecutePolicy)

responseGetPredictiveScalingForecast :: GetPredictiveScalingForecastResponse -> TestTree
responseGetPredictiveScalingForecast =
  res
    "GetPredictiveScalingForecastResponse"
    "fixture/GetPredictiveScalingForecastResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetPredictiveScalingForecast)

responseDescribeInstanceRefreshes :: DescribeInstanceRefreshesResponse -> TestTree
responseDescribeInstanceRefreshes =
  res
    "DescribeInstanceRefreshesResponse"
    "fixture/DescribeInstanceRefreshesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeInstanceRefreshes)

responseDescribeAccountLimits :: DescribeAccountLimitsResponse -> TestTree
responseDescribeAccountLimits =
  res
    "DescribeAccountLimitsResponse"
    "fixture/DescribeAccountLimitsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeAccountLimits)

responseAttachLoadBalancers :: AttachLoadBalancersResponse -> TestTree
responseAttachLoadBalancers =
  res
    "AttachLoadBalancersResponse"
    "fixture/AttachLoadBalancersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AttachLoadBalancers)

responseBatchDeleteScheduledAction :: BatchDeleteScheduledActionResponse -> TestTree
responseBatchDeleteScheduledAction =
  res
    "BatchDeleteScheduledActionResponse"
    "fixture/BatchDeleteScheduledActionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchDeleteScheduledAction)

responseTerminateInstanceInAutoScalingGroup :: TerminateInstanceInAutoScalingGroupResponse -> TestTree
responseTerminateInstanceInAutoScalingGroup =
  res
    "TerminateInstanceInAutoScalingGroupResponse"
    "fixture/TerminateInstanceInAutoScalingGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TerminateInstanceInAutoScalingGroup)

responseDescribeLoadBalancerTargetGroups :: DescribeLoadBalancerTargetGroupsResponse -> TestTree
responseDescribeLoadBalancerTargetGroups =
  res
    "DescribeLoadBalancerTargetGroupsResponse"
    "fixture/DescribeLoadBalancerTargetGroupsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeLoadBalancerTargetGroups)

responsePutScheduledUpdateGroupAction :: PutScheduledUpdateGroupActionResponse -> TestTree
responsePutScheduledUpdateGroupAction =
  res
    "PutScheduledUpdateGroupActionResponse"
    "fixture/PutScheduledUpdateGroupActionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutScheduledUpdateGroupAction)

responseSetInstanceProtection :: SetInstanceProtectionResponse -> TestTree
responseSetInstanceProtection =
  res
    "SetInstanceProtectionResponse"
    "fixture/SetInstanceProtectionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SetInstanceProtection)

responseDescribePolicies :: DescribePoliciesResponse -> TestTree
responseDescribePolicies =
  res
    "DescribePoliciesResponse"
    "fixture/DescribePoliciesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribePolicies)

responseDescribeLaunchConfigurations :: DescribeLaunchConfigurationsResponse -> TestTree
responseDescribeLaunchConfigurations =
  res
    "DescribeLaunchConfigurationsResponse"
    "fixture/DescribeLaunchConfigurationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeLaunchConfigurations)

responseDescribeScalingActivities :: DescribeScalingActivitiesResponse -> TestTree
responseDescribeScalingActivities =
  res
    "DescribeScalingActivitiesResponse"
    "fixture/DescribeScalingActivitiesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeScalingActivities)

responseDescribeNotificationConfigurations :: DescribeNotificationConfigurationsResponse -> TestTree
responseDescribeNotificationConfigurations =
  res
    "DescribeNotificationConfigurationsResponse"
    "fixture/DescribeNotificationConfigurationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeNotificationConfigurations)

responseDescribeLifecycleHookTypes :: DescribeLifecycleHookTypesResponse -> TestTree
responseDescribeLifecycleHookTypes =
  res
    "DescribeLifecycleHookTypesResponse"
    "fixture/DescribeLifecycleHookTypesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeLifecycleHookTypes)

responseDescribeAdjustmentTypes :: DescribeAdjustmentTypesResponse -> TestTree
responseDescribeAdjustmentTypes =
  res
    "DescribeAdjustmentTypesResponse"
    "fixture/DescribeAdjustmentTypesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeAdjustmentTypes)

responseCreateAutoScalingGroup :: CreateAutoScalingGroupResponse -> TestTree
responseCreateAutoScalingGroup =
  res
    "CreateAutoScalingGroupResponse"
    "fixture/CreateAutoScalingGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateAutoScalingGroup)

responseCreateLaunchConfiguration :: CreateLaunchConfigurationResponse -> TestTree
responseCreateLaunchConfiguration =
  res
    "CreateLaunchConfigurationResponse"
    "fixture/CreateLaunchConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateLaunchConfiguration)
