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
--         [ requestExecutePolicy $
--             newExecutePolicy
--
--         , requestSuspendProcesses $
--             newSuspendProcesses
--
--         , requestDescribeInstanceRefreshes $
--             newDescribeInstanceRefreshes
--
--         , requestEnterStandby $
--             newEnterStandby
--
--         , requestDeleteLifecycleHook $
--             newDeleteLifecycleHook
--
--         , requestAttachLoadBalancerTargetGroups $
--             newAttachLoadBalancerTargetGroups
--
--         , requestStartInstanceRefresh $
--             newStartInstanceRefresh
--
--         , requestResumeProcesses $
--             newResumeProcesses
--
--         , requestPutNotificationConfiguration $
--             newPutNotificationConfiguration
--
--         , requestDescribeTags $
--             newDescribeTags
--
--         , requestDeleteWarmPool $
--             newDeleteWarmPool
--
--         , requestPutScalingPolicy $
--             newPutScalingPolicy
--
--         , requestCreateOrUpdateTags $
--             newCreateOrUpdateTags
--
--         , requestDeletePolicy $
--             newDeletePolicy
--
--         , requestCreateLaunchConfiguration $
--             newCreateLaunchConfiguration
--
--         , requestDescribeAutoScalingNotificationTypes $
--             newDescribeAutoScalingNotificationTypes
--
--         , requestDescribeScheduledActions $
--             newDescribeScheduledActions
--
--         , requestDescribeAdjustmentTypes $
--             newDescribeAdjustmentTypes
--
--         , requestDetachLoadBalancers $
--             newDetachLoadBalancers
--
--         , requestDeleteScheduledAction $
--             newDeleteScheduledAction
--
--         , requestDescribeScalingActivities $
--             newDescribeScalingActivities
--
--         , requestDescribeLifecycleHooks $
--             newDescribeLifecycleHooks
--
--         , requestPutScheduledUpdateGroupAction $
--             newPutScheduledUpdateGroupAction
--
--         , requestDeleteTags $
--             newDeleteTags
--
--         , requestDetachLoadBalancerTargetGroups $
--             newDetachLoadBalancerTargetGroups
--
--         , requestDescribeWarmPool $
--             newDescribeWarmPool
--
--         , requestSetInstanceProtection $
--             newSetInstanceProtection
--
--         , requestDetachInstances $
--             newDetachInstances
--
--         , requestTerminateInstanceInAutoScalingGroup $
--             newTerminateInstanceInAutoScalingGroup
--
--         , requestAttachLoadBalancers $
--             newAttachLoadBalancers
--
--         , requestDescribeAccountLimits $
--             newDescribeAccountLimits
--
--         , requestDescribeTerminationPolicyTypes $
--             newDescribeTerminationPolicyTypes
--
--         , requestSetInstanceHealth $
--             newSetInstanceHealth
--
--         , requestExitStandby $
--             newExitStandby
--
--         , requestGetPredictiveScalingForecast $
--             newGetPredictiveScalingForecast
--
--         , requestPutWarmPool $
--             newPutWarmPool
--
--         , requestDeleteNotificationConfiguration $
--             newDeleteNotificationConfiguration
--
--         , requestPutLifecycleHook $
--             newPutLifecycleHook
--
--         , requestBatchPutScheduledUpdateGroupAction $
--             newBatchPutScheduledUpdateGroupAction
--
--         , requestDescribeMetricCollectionTypes $
--             newDescribeMetricCollectionTypes
--
--         , requestDeleteLaunchConfiguration $
--             newDeleteLaunchConfiguration
--
--         , requestUpdateAutoScalingGroup $
--             newUpdateAutoScalingGroup
--
--         , requestDeleteAutoScalingGroup $
--             newDeleteAutoScalingGroup
--
--         , requestDescribeLoadBalancers $
--             newDescribeLoadBalancers
--
--         , requestCreateAutoScalingGroup $
--             newCreateAutoScalingGroup
--
--         , requestAttachInstances $
--             newAttachInstances
--
--         , requestCompleteLifecycleAction $
--             newCompleteLifecycleAction
--
--         , requestSetDesiredCapacity $
--             newSetDesiredCapacity
--
--         , requestDescribeLifecycleHookTypes $
--             newDescribeLifecycleHookTypes
--
--         , requestDescribeNotificationConfigurations $
--             newDescribeNotificationConfigurations
--
--         , requestDescribePolicies $
--             newDescribePolicies
--
--         , requestDescribeLaunchConfigurations $
--             newDescribeLaunchConfigurations
--
--         , requestDescribeAutoScalingGroups $
--             newDescribeAutoScalingGroups
--
--         , requestEnableMetricsCollection $
--             newEnableMetricsCollection
--
--         , requestDescribeScalingProcessTypes $
--             newDescribeScalingProcessTypes
--
--         , requestRecordLifecycleActionHeartbeat $
--             newRecordLifecycleActionHeartbeat
--
--         , requestDisableMetricsCollection $
--             newDisableMetricsCollection
--
--         , requestDescribeAutoScalingInstances $
--             newDescribeAutoScalingInstances
--
--         , requestCancelInstanceRefresh $
--             newCancelInstanceRefresh
--
--         , requestBatchDeleteScheduledAction $
--             newBatchDeleteScheduledAction
--
--         , requestDescribeLoadBalancerTargetGroups $
--             newDescribeLoadBalancerTargetGroups
--
--           ]

--     , testGroup "response"
--         [ responseExecutePolicy $
--             newExecutePolicyResponse
--
--         , responseSuspendProcesses $
--             newSuspendProcessesResponse
--
--         , responseDescribeInstanceRefreshes $
--             newDescribeInstanceRefreshesResponse
--
--         , responseEnterStandby $
--             newEnterStandbyResponse
--
--         , responseDeleteLifecycleHook $
--             newDeleteLifecycleHookResponse
--
--         , responseAttachLoadBalancerTargetGroups $
--             newAttachLoadBalancerTargetGroupsResponse
--
--         , responseStartInstanceRefresh $
--             newStartInstanceRefreshResponse
--
--         , responseResumeProcesses $
--             newResumeProcessesResponse
--
--         , responsePutNotificationConfiguration $
--             newPutNotificationConfigurationResponse
--
--         , responseDescribeTags $
--             newDescribeTagsResponse
--
--         , responseDeleteWarmPool $
--             newDeleteWarmPoolResponse
--
--         , responsePutScalingPolicy $
--             newPutScalingPolicyResponse
--
--         , responseCreateOrUpdateTags $
--             newCreateOrUpdateTagsResponse
--
--         , responseDeletePolicy $
--             newDeletePolicyResponse
--
--         , responseCreateLaunchConfiguration $
--             newCreateLaunchConfigurationResponse
--
--         , responseDescribeAutoScalingNotificationTypes $
--             newDescribeAutoScalingNotificationTypesResponse
--
--         , responseDescribeScheduledActions $
--             newDescribeScheduledActionsResponse
--
--         , responseDescribeAdjustmentTypes $
--             newDescribeAdjustmentTypesResponse
--
--         , responseDetachLoadBalancers $
--             newDetachLoadBalancersResponse
--
--         , responseDeleteScheduledAction $
--             newDeleteScheduledActionResponse
--
--         , responseDescribeScalingActivities $
--             newDescribeScalingActivitiesResponse
--
--         , responseDescribeLifecycleHooks $
--             newDescribeLifecycleHooksResponse
--
--         , responsePutScheduledUpdateGroupAction $
--             newPutScheduledUpdateGroupActionResponse
--
--         , responseDeleteTags $
--             newDeleteTagsResponse
--
--         , responseDetachLoadBalancerTargetGroups $
--             newDetachLoadBalancerTargetGroupsResponse
--
--         , responseDescribeWarmPool $
--             newDescribeWarmPoolResponse
--
--         , responseSetInstanceProtection $
--             newSetInstanceProtectionResponse
--
--         , responseDetachInstances $
--             newDetachInstancesResponse
--
--         , responseTerminateInstanceInAutoScalingGroup $
--             newTerminateInstanceInAutoScalingGroupResponse
--
--         , responseAttachLoadBalancers $
--             newAttachLoadBalancersResponse
--
--         , responseDescribeAccountLimits $
--             newDescribeAccountLimitsResponse
--
--         , responseDescribeTerminationPolicyTypes $
--             newDescribeTerminationPolicyTypesResponse
--
--         , responseSetInstanceHealth $
--             newSetInstanceHealthResponse
--
--         , responseExitStandby $
--             newExitStandbyResponse
--
--         , responseGetPredictiveScalingForecast $
--             newGetPredictiveScalingForecastResponse
--
--         , responsePutWarmPool $
--             newPutWarmPoolResponse
--
--         , responseDeleteNotificationConfiguration $
--             newDeleteNotificationConfigurationResponse
--
--         , responsePutLifecycleHook $
--             newPutLifecycleHookResponse
--
--         , responseBatchPutScheduledUpdateGroupAction $
--             newBatchPutScheduledUpdateGroupActionResponse
--
--         , responseDescribeMetricCollectionTypes $
--             newDescribeMetricCollectionTypesResponse
--
--         , responseDeleteLaunchConfiguration $
--             newDeleteLaunchConfigurationResponse
--
--         , responseUpdateAutoScalingGroup $
--             newUpdateAutoScalingGroupResponse
--
--         , responseDeleteAutoScalingGroup $
--             newDeleteAutoScalingGroupResponse
--
--         , responseDescribeLoadBalancers $
--             newDescribeLoadBalancersResponse
--
--         , responseCreateAutoScalingGroup $
--             newCreateAutoScalingGroupResponse
--
--         , responseAttachInstances $
--             newAttachInstancesResponse
--
--         , responseCompleteLifecycleAction $
--             newCompleteLifecycleActionResponse
--
--         , responseSetDesiredCapacity $
--             newSetDesiredCapacityResponse
--
--         , responseDescribeLifecycleHookTypes $
--             newDescribeLifecycleHookTypesResponse
--
--         , responseDescribeNotificationConfigurations $
--             newDescribeNotificationConfigurationsResponse
--
--         , responseDescribePolicies $
--             newDescribePoliciesResponse
--
--         , responseDescribeLaunchConfigurations $
--             newDescribeLaunchConfigurationsResponse
--
--         , responseDescribeAutoScalingGroups $
--             newDescribeAutoScalingGroupsResponse
--
--         , responseEnableMetricsCollection $
--             newEnableMetricsCollectionResponse
--
--         , responseDescribeScalingProcessTypes $
--             newDescribeScalingProcessTypesResponse
--
--         , responseRecordLifecycleActionHeartbeat $
--             newRecordLifecycleActionHeartbeatResponse
--
--         , responseDisableMetricsCollection $
--             newDisableMetricsCollectionResponse
--
--         , responseDescribeAutoScalingInstances $
--             newDescribeAutoScalingInstancesResponse
--
--         , responseCancelInstanceRefresh $
--             newCancelInstanceRefreshResponse
--
--         , responseBatchDeleteScheduledAction $
--             newBatchDeleteScheduledActionResponse
--
--         , responseDescribeLoadBalancerTargetGroups $
--             newDescribeLoadBalancerTargetGroupsResponse
--
--           ]
--     ]

-- Requests

requestExecutePolicy :: ExecutePolicy -> TestTree
requestExecutePolicy =
  req
    "ExecutePolicy"
    "fixture/ExecutePolicy.yaml"

requestSuspendProcesses :: SuspendProcesses -> TestTree
requestSuspendProcesses =
  req
    "SuspendProcesses"
    "fixture/SuspendProcesses.yaml"

requestDescribeInstanceRefreshes :: DescribeInstanceRefreshes -> TestTree
requestDescribeInstanceRefreshes =
  req
    "DescribeInstanceRefreshes"
    "fixture/DescribeInstanceRefreshes.yaml"

requestEnterStandby :: EnterStandby -> TestTree
requestEnterStandby =
  req
    "EnterStandby"
    "fixture/EnterStandby.yaml"

requestDeleteLifecycleHook :: DeleteLifecycleHook -> TestTree
requestDeleteLifecycleHook =
  req
    "DeleteLifecycleHook"
    "fixture/DeleteLifecycleHook.yaml"

requestAttachLoadBalancerTargetGroups :: AttachLoadBalancerTargetGroups -> TestTree
requestAttachLoadBalancerTargetGroups =
  req
    "AttachLoadBalancerTargetGroups"
    "fixture/AttachLoadBalancerTargetGroups.yaml"

requestStartInstanceRefresh :: StartInstanceRefresh -> TestTree
requestStartInstanceRefresh =
  req
    "StartInstanceRefresh"
    "fixture/StartInstanceRefresh.yaml"

requestResumeProcesses :: ResumeProcesses -> TestTree
requestResumeProcesses =
  req
    "ResumeProcesses"
    "fixture/ResumeProcesses.yaml"

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

requestCreateOrUpdateTags :: CreateOrUpdateTags -> TestTree
requestCreateOrUpdateTags =
  req
    "CreateOrUpdateTags"
    "fixture/CreateOrUpdateTags.yaml"

requestDeletePolicy :: DeletePolicy -> TestTree
requestDeletePolicy =
  req
    "DeletePolicy"
    "fixture/DeletePolicy.yaml"

requestCreateLaunchConfiguration :: CreateLaunchConfiguration -> TestTree
requestCreateLaunchConfiguration =
  req
    "CreateLaunchConfiguration"
    "fixture/CreateLaunchConfiguration.yaml"

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

requestDescribeAdjustmentTypes :: DescribeAdjustmentTypes -> TestTree
requestDescribeAdjustmentTypes =
  req
    "DescribeAdjustmentTypes"
    "fixture/DescribeAdjustmentTypes.yaml"

requestDetachLoadBalancers :: DetachLoadBalancers -> TestTree
requestDetachLoadBalancers =
  req
    "DetachLoadBalancers"
    "fixture/DetachLoadBalancers.yaml"

requestDeleteScheduledAction :: DeleteScheduledAction -> TestTree
requestDeleteScheduledAction =
  req
    "DeleteScheduledAction"
    "fixture/DeleteScheduledAction.yaml"

requestDescribeScalingActivities :: DescribeScalingActivities -> TestTree
requestDescribeScalingActivities =
  req
    "DescribeScalingActivities"
    "fixture/DescribeScalingActivities.yaml"

requestDescribeLifecycleHooks :: DescribeLifecycleHooks -> TestTree
requestDescribeLifecycleHooks =
  req
    "DescribeLifecycleHooks"
    "fixture/DescribeLifecycleHooks.yaml"

requestPutScheduledUpdateGroupAction :: PutScheduledUpdateGroupAction -> TestTree
requestPutScheduledUpdateGroupAction =
  req
    "PutScheduledUpdateGroupAction"
    "fixture/PutScheduledUpdateGroupAction.yaml"

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

requestDescribeWarmPool :: DescribeWarmPool -> TestTree
requestDescribeWarmPool =
  req
    "DescribeWarmPool"
    "fixture/DescribeWarmPool.yaml"

requestSetInstanceProtection :: SetInstanceProtection -> TestTree
requestSetInstanceProtection =
  req
    "SetInstanceProtection"
    "fixture/SetInstanceProtection.yaml"

requestDetachInstances :: DetachInstances -> TestTree
requestDetachInstances =
  req
    "DetachInstances"
    "fixture/DetachInstances.yaml"

requestTerminateInstanceInAutoScalingGroup :: TerminateInstanceInAutoScalingGroup -> TestTree
requestTerminateInstanceInAutoScalingGroup =
  req
    "TerminateInstanceInAutoScalingGroup"
    "fixture/TerminateInstanceInAutoScalingGroup.yaml"

requestAttachLoadBalancers :: AttachLoadBalancers -> TestTree
requestAttachLoadBalancers =
  req
    "AttachLoadBalancers"
    "fixture/AttachLoadBalancers.yaml"

requestDescribeAccountLimits :: DescribeAccountLimits -> TestTree
requestDescribeAccountLimits =
  req
    "DescribeAccountLimits"
    "fixture/DescribeAccountLimits.yaml"

requestDescribeTerminationPolicyTypes :: DescribeTerminationPolicyTypes -> TestTree
requestDescribeTerminationPolicyTypes =
  req
    "DescribeTerminationPolicyTypes"
    "fixture/DescribeTerminationPolicyTypes.yaml"

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

requestGetPredictiveScalingForecast :: GetPredictiveScalingForecast -> TestTree
requestGetPredictiveScalingForecast =
  req
    "GetPredictiveScalingForecast"
    "fixture/GetPredictiveScalingForecast.yaml"

requestPutWarmPool :: PutWarmPool -> TestTree
requestPutWarmPool =
  req
    "PutWarmPool"
    "fixture/PutWarmPool.yaml"

requestDeleteNotificationConfiguration :: DeleteNotificationConfiguration -> TestTree
requestDeleteNotificationConfiguration =
  req
    "DeleteNotificationConfiguration"
    "fixture/DeleteNotificationConfiguration.yaml"

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

requestDescribeMetricCollectionTypes :: DescribeMetricCollectionTypes -> TestTree
requestDescribeMetricCollectionTypes =
  req
    "DescribeMetricCollectionTypes"
    "fixture/DescribeMetricCollectionTypes.yaml"

requestDeleteLaunchConfiguration :: DeleteLaunchConfiguration -> TestTree
requestDeleteLaunchConfiguration =
  req
    "DeleteLaunchConfiguration"
    "fixture/DeleteLaunchConfiguration.yaml"

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

requestDescribeLoadBalancers :: DescribeLoadBalancers -> TestTree
requestDescribeLoadBalancers =
  req
    "DescribeLoadBalancers"
    "fixture/DescribeLoadBalancers.yaml"

requestCreateAutoScalingGroup :: CreateAutoScalingGroup -> TestTree
requestCreateAutoScalingGroup =
  req
    "CreateAutoScalingGroup"
    "fixture/CreateAutoScalingGroup.yaml"

requestAttachInstances :: AttachInstances -> TestTree
requestAttachInstances =
  req
    "AttachInstances"
    "fixture/AttachInstances.yaml"

requestCompleteLifecycleAction :: CompleteLifecycleAction -> TestTree
requestCompleteLifecycleAction =
  req
    "CompleteLifecycleAction"
    "fixture/CompleteLifecycleAction.yaml"

requestSetDesiredCapacity :: SetDesiredCapacity -> TestTree
requestSetDesiredCapacity =
  req
    "SetDesiredCapacity"
    "fixture/SetDesiredCapacity.yaml"

requestDescribeLifecycleHookTypes :: DescribeLifecycleHookTypes -> TestTree
requestDescribeLifecycleHookTypes =
  req
    "DescribeLifecycleHookTypes"
    "fixture/DescribeLifecycleHookTypes.yaml"

requestDescribeNotificationConfigurations :: DescribeNotificationConfigurations -> TestTree
requestDescribeNotificationConfigurations =
  req
    "DescribeNotificationConfigurations"
    "fixture/DescribeNotificationConfigurations.yaml"

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

requestDescribeAutoScalingGroups :: DescribeAutoScalingGroups -> TestTree
requestDescribeAutoScalingGroups =
  req
    "DescribeAutoScalingGroups"
    "fixture/DescribeAutoScalingGroups.yaml"

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

requestDescribeAutoScalingInstances :: DescribeAutoScalingInstances -> TestTree
requestDescribeAutoScalingInstances =
  req
    "DescribeAutoScalingInstances"
    "fixture/DescribeAutoScalingInstances.yaml"

requestCancelInstanceRefresh :: CancelInstanceRefresh -> TestTree
requestCancelInstanceRefresh =
  req
    "CancelInstanceRefresh"
    "fixture/CancelInstanceRefresh.yaml"

requestBatchDeleteScheduledAction :: BatchDeleteScheduledAction -> TestTree
requestBatchDeleteScheduledAction =
  req
    "BatchDeleteScheduledAction"
    "fixture/BatchDeleteScheduledAction.yaml"

requestDescribeLoadBalancerTargetGroups :: DescribeLoadBalancerTargetGroups -> TestTree
requestDescribeLoadBalancerTargetGroups =
  req
    "DescribeLoadBalancerTargetGroups"
    "fixture/DescribeLoadBalancerTargetGroups.yaml"

-- Responses

responseExecutePolicy :: ExecutePolicyResponse -> TestTree
responseExecutePolicy =
  res
    "ExecutePolicyResponse"
    "fixture/ExecutePolicyResponse.proto"
    defaultService
    (Proxy :: Proxy ExecutePolicy)

responseSuspendProcesses :: SuspendProcessesResponse -> TestTree
responseSuspendProcesses =
  res
    "SuspendProcessesResponse"
    "fixture/SuspendProcessesResponse.proto"
    defaultService
    (Proxy :: Proxy SuspendProcesses)

responseDescribeInstanceRefreshes :: DescribeInstanceRefreshesResponse -> TestTree
responseDescribeInstanceRefreshes =
  res
    "DescribeInstanceRefreshesResponse"
    "fixture/DescribeInstanceRefreshesResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeInstanceRefreshes)

responseEnterStandby :: EnterStandbyResponse -> TestTree
responseEnterStandby =
  res
    "EnterStandbyResponse"
    "fixture/EnterStandbyResponse.proto"
    defaultService
    (Proxy :: Proxy EnterStandby)

responseDeleteLifecycleHook :: DeleteLifecycleHookResponse -> TestTree
responseDeleteLifecycleHook =
  res
    "DeleteLifecycleHookResponse"
    "fixture/DeleteLifecycleHookResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteLifecycleHook)

responseAttachLoadBalancerTargetGroups :: AttachLoadBalancerTargetGroupsResponse -> TestTree
responseAttachLoadBalancerTargetGroups =
  res
    "AttachLoadBalancerTargetGroupsResponse"
    "fixture/AttachLoadBalancerTargetGroupsResponse.proto"
    defaultService
    (Proxy :: Proxy AttachLoadBalancerTargetGroups)

responseStartInstanceRefresh :: StartInstanceRefreshResponse -> TestTree
responseStartInstanceRefresh =
  res
    "StartInstanceRefreshResponse"
    "fixture/StartInstanceRefreshResponse.proto"
    defaultService
    (Proxy :: Proxy StartInstanceRefresh)

responseResumeProcesses :: ResumeProcessesResponse -> TestTree
responseResumeProcesses =
  res
    "ResumeProcessesResponse"
    "fixture/ResumeProcessesResponse.proto"
    defaultService
    (Proxy :: Proxy ResumeProcesses)

responsePutNotificationConfiguration :: PutNotificationConfigurationResponse -> TestTree
responsePutNotificationConfiguration =
  res
    "PutNotificationConfigurationResponse"
    "fixture/PutNotificationConfigurationResponse.proto"
    defaultService
    (Proxy :: Proxy PutNotificationConfiguration)

responseDescribeTags :: DescribeTagsResponse -> TestTree
responseDescribeTags =
  res
    "DescribeTagsResponse"
    "fixture/DescribeTagsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeTags)

responseDeleteWarmPool :: DeleteWarmPoolResponse -> TestTree
responseDeleteWarmPool =
  res
    "DeleteWarmPoolResponse"
    "fixture/DeleteWarmPoolResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteWarmPool)

responsePutScalingPolicy :: PutScalingPolicyResponse -> TestTree
responsePutScalingPolicy =
  res
    "PutScalingPolicyResponse"
    "fixture/PutScalingPolicyResponse.proto"
    defaultService
    (Proxy :: Proxy PutScalingPolicy)

responseCreateOrUpdateTags :: CreateOrUpdateTagsResponse -> TestTree
responseCreateOrUpdateTags =
  res
    "CreateOrUpdateTagsResponse"
    "fixture/CreateOrUpdateTagsResponse.proto"
    defaultService
    (Proxy :: Proxy CreateOrUpdateTags)

responseDeletePolicy :: DeletePolicyResponse -> TestTree
responseDeletePolicy =
  res
    "DeletePolicyResponse"
    "fixture/DeletePolicyResponse.proto"
    defaultService
    (Proxy :: Proxy DeletePolicy)

responseCreateLaunchConfiguration :: CreateLaunchConfigurationResponse -> TestTree
responseCreateLaunchConfiguration =
  res
    "CreateLaunchConfigurationResponse"
    "fixture/CreateLaunchConfigurationResponse.proto"
    defaultService
    (Proxy :: Proxy CreateLaunchConfiguration)

responseDescribeAutoScalingNotificationTypes :: DescribeAutoScalingNotificationTypesResponse -> TestTree
responseDescribeAutoScalingNotificationTypes =
  res
    "DescribeAutoScalingNotificationTypesResponse"
    "fixture/DescribeAutoScalingNotificationTypesResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeAutoScalingNotificationTypes)

responseDescribeScheduledActions :: DescribeScheduledActionsResponse -> TestTree
responseDescribeScheduledActions =
  res
    "DescribeScheduledActionsResponse"
    "fixture/DescribeScheduledActionsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeScheduledActions)

responseDescribeAdjustmentTypes :: DescribeAdjustmentTypesResponse -> TestTree
responseDescribeAdjustmentTypes =
  res
    "DescribeAdjustmentTypesResponse"
    "fixture/DescribeAdjustmentTypesResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeAdjustmentTypes)

responseDetachLoadBalancers :: DetachLoadBalancersResponse -> TestTree
responseDetachLoadBalancers =
  res
    "DetachLoadBalancersResponse"
    "fixture/DetachLoadBalancersResponse.proto"
    defaultService
    (Proxy :: Proxy DetachLoadBalancers)

responseDeleteScheduledAction :: DeleteScheduledActionResponse -> TestTree
responseDeleteScheduledAction =
  res
    "DeleteScheduledActionResponse"
    "fixture/DeleteScheduledActionResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteScheduledAction)

responseDescribeScalingActivities :: DescribeScalingActivitiesResponse -> TestTree
responseDescribeScalingActivities =
  res
    "DescribeScalingActivitiesResponse"
    "fixture/DescribeScalingActivitiesResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeScalingActivities)

responseDescribeLifecycleHooks :: DescribeLifecycleHooksResponse -> TestTree
responseDescribeLifecycleHooks =
  res
    "DescribeLifecycleHooksResponse"
    "fixture/DescribeLifecycleHooksResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeLifecycleHooks)

responsePutScheduledUpdateGroupAction :: PutScheduledUpdateGroupActionResponse -> TestTree
responsePutScheduledUpdateGroupAction =
  res
    "PutScheduledUpdateGroupActionResponse"
    "fixture/PutScheduledUpdateGroupActionResponse.proto"
    defaultService
    (Proxy :: Proxy PutScheduledUpdateGroupAction)

responseDeleteTags :: DeleteTagsResponse -> TestTree
responseDeleteTags =
  res
    "DeleteTagsResponse"
    "fixture/DeleteTagsResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteTags)

responseDetachLoadBalancerTargetGroups :: DetachLoadBalancerTargetGroupsResponse -> TestTree
responseDetachLoadBalancerTargetGroups =
  res
    "DetachLoadBalancerTargetGroupsResponse"
    "fixture/DetachLoadBalancerTargetGroupsResponse.proto"
    defaultService
    (Proxy :: Proxy DetachLoadBalancerTargetGroups)

responseDescribeWarmPool :: DescribeWarmPoolResponse -> TestTree
responseDescribeWarmPool =
  res
    "DescribeWarmPoolResponse"
    "fixture/DescribeWarmPoolResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeWarmPool)

responseSetInstanceProtection :: SetInstanceProtectionResponse -> TestTree
responseSetInstanceProtection =
  res
    "SetInstanceProtectionResponse"
    "fixture/SetInstanceProtectionResponse.proto"
    defaultService
    (Proxy :: Proxy SetInstanceProtection)

responseDetachInstances :: DetachInstancesResponse -> TestTree
responseDetachInstances =
  res
    "DetachInstancesResponse"
    "fixture/DetachInstancesResponse.proto"
    defaultService
    (Proxy :: Proxy DetachInstances)

responseTerminateInstanceInAutoScalingGroup :: TerminateInstanceInAutoScalingGroupResponse -> TestTree
responseTerminateInstanceInAutoScalingGroup =
  res
    "TerminateInstanceInAutoScalingGroupResponse"
    "fixture/TerminateInstanceInAutoScalingGroupResponse.proto"
    defaultService
    (Proxy :: Proxy TerminateInstanceInAutoScalingGroup)

responseAttachLoadBalancers :: AttachLoadBalancersResponse -> TestTree
responseAttachLoadBalancers =
  res
    "AttachLoadBalancersResponse"
    "fixture/AttachLoadBalancersResponse.proto"
    defaultService
    (Proxy :: Proxy AttachLoadBalancers)

responseDescribeAccountLimits :: DescribeAccountLimitsResponse -> TestTree
responseDescribeAccountLimits =
  res
    "DescribeAccountLimitsResponse"
    "fixture/DescribeAccountLimitsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeAccountLimits)

responseDescribeTerminationPolicyTypes :: DescribeTerminationPolicyTypesResponse -> TestTree
responseDescribeTerminationPolicyTypes =
  res
    "DescribeTerminationPolicyTypesResponse"
    "fixture/DescribeTerminationPolicyTypesResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeTerminationPolicyTypes)

responseSetInstanceHealth :: SetInstanceHealthResponse -> TestTree
responseSetInstanceHealth =
  res
    "SetInstanceHealthResponse"
    "fixture/SetInstanceHealthResponse.proto"
    defaultService
    (Proxy :: Proxy SetInstanceHealth)

responseExitStandby :: ExitStandbyResponse -> TestTree
responseExitStandby =
  res
    "ExitStandbyResponse"
    "fixture/ExitStandbyResponse.proto"
    defaultService
    (Proxy :: Proxy ExitStandby)

responseGetPredictiveScalingForecast :: GetPredictiveScalingForecastResponse -> TestTree
responseGetPredictiveScalingForecast =
  res
    "GetPredictiveScalingForecastResponse"
    "fixture/GetPredictiveScalingForecastResponse.proto"
    defaultService
    (Proxy :: Proxy GetPredictiveScalingForecast)

responsePutWarmPool :: PutWarmPoolResponse -> TestTree
responsePutWarmPool =
  res
    "PutWarmPoolResponse"
    "fixture/PutWarmPoolResponse.proto"
    defaultService
    (Proxy :: Proxy PutWarmPool)

responseDeleteNotificationConfiguration :: DeleteNotificationConfigurationResponse -> TestTree
responseDeleteNotificationConfiguration =
  res
    "DeleteNotificationConfigurationResponse"
    "fixture/DeleteNotificationConfigurationResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteNotificationConfiguration)

responsePutLifecycleHook :: PutLifecycleHookResponse -> TestTree
responsePutLifecycleHook =
  res
    "PutLifecycleHookResponse"
    "fixture/PutLifecycleHookResponse.proto"
    defaultService
    (Proxy :: Proxy PutLifecycleHook)

responseBatchPutScheduledUpdateGroupAction :: BatchPutScheduledUpdateGroupActionResponse -> TestTree
responseBatchPutScheduledUpdateGroupAction =
  res
    "BatchPutScheduledUpdateGroupActionResponse"
    "fixture/BatchPutScheduledUpdateGroupActionResponse.proto"
    defaultService
    (Proxy :: Proxy BatchPutScheduledUpdateGroupAction)

responseDescribeMetricCollectionTypes :: DescribeMetricCollectionTypesResponse -> TestTree
responseDescribeMetricCollectionTypes =
  res
    "DescribeMetricCollectionTypesResponse"
    "fixture/DescribeMetricCollectionTypesResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeMetricCollectionTypes)

responseDeleteLaunchConfiguration :: DeleteLaunchConfigurationResponse -> TestTree
responseDeleteLaunchConfiguration =
  res
    "DeleteLaunchConfigurationResponse"
    "fixture/DeleteLaunchConfigurationResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteLaunchConfiguration)

responseUpdateAutoScalingGroup :: UpdateAutoScalingGroupResponse -> TestTree
responseUpdateAutoScalingGroup =
  res
    "UpdateAutoScalingGroupResponse"
    "fixture/UpdateAutoScalingGroupResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateAutoScalingGroup)

responseDeleteAutoScalingGroup :: DeleteAutoScalingGroupResponse -> TestTree
responseDeleteAutoScalingGroup =
  res
    "DeleteAutoScalingGroupResponse"
    "fixture/DeleteAutoScalingGroupResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteAutoScalingGroup)

responseDescribeLoadBalancers :: DescribeLoadBalancersResponse -> TestTree
responseDescribeLoadBalancers =
  res
    "DescribeLoadBalancersResponse"
    "fixture/DescribeLoadBalancersResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeLoadBalancers)

responseCreateAutoScalingGroup :: CreateAutoScalingGroupResponse -> TestTree
responseCreateAutoScalingGroup =
  res
    "CreateAutoScalingGroupResponse"
    "fixture/CreateAutoScalingGroupResponse.proto"
    defaultService
    (Proxy :: Proxy CreateAutoScalingGroup)

responseAttachInstances :: AttachInstancesResponse -> TestTree
responseAttachInstances =
  res
    "AttachInstancesResponse"
    "fixture/AttachInstancesResponse.proto"
    defaultService
    (Proxy :: Proxy AttachInstances)

responseCompleteLifecycleAction :: CompleteLifecycleActionResponse -> TestTree
responseCompleteLifecycleAction =
  res
    "CompleteLifecycleActionResponse"
    "fixture/CompleteLifecycleActionResponse.proto"
    defaultService
    (Proxy :: Proxy CompleteLifecycleAction)

responseSetDesiredCapacity :: SetDesiredCapacityResponse -> TestTree
responseSetDesiredCapacity =
  res
    "SetDesiredCapacityResponse"
    "fixture/SetDesiredCapacityResponse.proto"
    defaultService
    (Proxy :: Proxy SetDesiredCapacity)

responseDescribeLifecycleHookTypes :: DescribeLifecycleHookTypesResponse -> TestTree
responseDescribeLifecycleHookTypes =
  res
    "DescribeLifecycleHookTypesResponse"
    "fixture/DescribeLifecycleHookTypesResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeLifecycleHookTypes)

responseDescribeNotificationConfigurations :: DescribeNotificationConfigurationsResponse -> TestTree
responseDescribeNotificationConfigurations =
  res
    "DescribeNotificationConfigurationsResponse"
    "fixture/DescribeNotificationConfigurationsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeNotificationConfigurations)

responseDescribePolicies :: DescribePoliciesResponse -> TestTree
responseDescribePolicies =
  res
    "DescribePoliciesResponse"
    "fixture/DescribePoliciesResponse.proto"
    defaultService
    (Proxy :: Proxy DescribePolicies)

responseDescribeLaunchConfigurations :: DescribeLaunchConfigurationsResponse -> TestTree
responseDescribeLaunchConfigurations =
  res
    "DescribeLaunchConfigurationsResponse"
    "fixture/DescribeLaunchConfigurationsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeLaunchConfigurations)

responseDescribeAutoScalingGroups :: DescribeAutoScalingGroupsResponse -> TestTree
responseDescribeAutoScalingGroups =
  res
    "DescribeAutoScalingGroupsResponse"
    "fixture/DescribeAutoScalingGroupsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeAutoScalingGroups)

responseEnableMetricsCollection :: EnableMetricsCollectionResponse -> TestTree
responseEnableMetricsCollection =
  res
    "EnableMetricsCollectionResponse"
    "fixture/EnableMetricsCollectionResponse.proto"
    defaultService
    (Proxy :: Proxy EnableMetricsCollection)

responseDescribeScalingProcessTypes :: DescribeScalingProcessTypesResponse -> TestTree
responseDescribeScalingProcessTypes =
  res
    "DescribeScalingProcessTypesResponse"
    "fixture/DescribeScalingProcessTypesResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeScalingProcessTypes)

responseRecordLifecycleActionHeartbeat :: RecordLifecycleActionHeartbeatResponse -> TestTree
responseRecordLifecycleActionHeartbeat =
  res
    "RecordLifecycleActionHeartbeatResponse"
    "fixture/RecordLifecycleActionHeartbeatResponse.proto"
    defaultService
    (Proxy :: Proxy RecordLifecycleActionHeartbeat)

responseDisableMetricsCollection :: DisableMetricsCollectionResponse -> TestTree
responseDisableMetricsCollection =
  res
    "DisableMetricsCollectionResponse"
    "fixture/DisableMetricsCollectionResponse.proto"
    defaultService
    (Proxy :: Proxy DisableMetricsCollection)

responseDescribeAutoScalingInstances :: DescribeAutoScalingInstancesResponse -> TestTree
responseDescribeAutoScalingInstances =
  res
    "DescribeAutoScalingInstancesResponse"
    "fixture/DescribeAutoScalingInstancesResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeAutoScalingInstances)

responseCancelInstanceRefresh :: CancelInstanceRefreshResponse -> TestTree
responseCancelInstanceRefresh =
  res
    "CancelInstanceRefreshResponse"
    "fixture/CancelInstanceRefreshResponse.proto"
    defaultService
    (Proxy :: Proxy CancelInstanceRefresh)

responseBatchDeleteScheduledAction :: BatchDeleteScheduledActionResponse -> TestTree
responseBatchDeleteScheduledAction =
  res
    "BatchDeleteScheduledActionResponse"
    "fixture/BatchDeleteScheduledActionResponse.proto"
    defaultService
    (Proxy :: Proxy BatchDeleteScheduledAction)

responseDescribeLoadBalancerTargetGroups :: DescribeLoadBalancerTargetGroupsResponse -> TestTree
responseDescribeLoadBalancerTargetGroups =
  res
    "DescribeLoadBalancerTargetGroupsResponse"
    "fixture/DescribeLoadBalancerTargetGroupsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeLoadBalancerTargetGroups)
