{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.AutoScaling
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.AutoScaling where

import Amazonka.AutoScaling
import qualified Data.Proxy as Proxy
import Test.Amazonka.AutoScaling.Internal
import Test.Amazonka.Fixture
import Test.Amazonka.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestAttachInstances $
--             newAttachInstances
--
--         , requestAttachLoadBalancerTargetGroups $
--             newAttachLoadBalancerTargetGroups
--
--         , requestAttachLoadBalancers $
--             newAttachLoadBalancers
--
--         , requestAttachTrafficSources $
--             newAttachTrafficSources
--
--         , requestBatchDeleteScheduledAction $
--             newBatchDeleteScheduledAction
--
--         , requestBatchPutScheduledUpdateGroupAction $
--             newBatchPutScheduledUpdateGroupAction
--
--         , requestCancelInstanceRefresh $
--             newCancelInstanceRefresh
--
--         , requestCompleteLifecycleAction $
--             newCompleteLifecycleAction
--
--         , requestCreateAutoScalingGroup $
--             newCreateAutoScalingGroup
--
--         , requestCreateLaunchConfiguration $
--             newCreateLaunchConfiguration
--
--         , requestCreateOrUpdateTags $
--             newCreateOrUpdateTags
--
--         , requestDeleteAutoScalingGroup $
--             newDeleteAutoScalingGroup
--
--         , requestDeleteLaunchConfiguration $
--             newDeleteLaunchConfiguration
--
--         , requestDeleteLifecycleHook $
--             newDeleteLifecycleHook
--
--         , requestDeleteNotificationConfiguration $
--             newDeleteNotificationConfiguration
--
--         , requestDeletePolicy $
--             newDeletePolicy
--
--         , requestDeleteScheduledAction $
--             newDeleteScheduledAction
--
--         , requestDeleteTags $
--             newDeleteTags
--
--         , requestDeleteWarmPool $
--             newDeleteWarmPool
--
--         , requestDescribeAccountLimits $
--             newDescribeAccountLimits
--
--         , requestDescribeAdjustmentTypes $
--             newDescribeAdjustmentTypes
--
--         , requestDescribeAutoScalingGroups $
--             newDescribeAutoScalingGroups
--
--         , requestDescribeAutoScalingInstances $
--             newDescribeAutoScalingInstances
--
--         , requestDescribeAutoScalingNotificationTypes $
--             newDescribeAutoScalingNotificationTypes
--
--         , requestDescribeInstanceRefreshes $
--             newDescribeInstanceRefreshes
--
--         , requestDescribeLaunchConfigurations $
--             newDescribeLaunchConfigurations
--
--         , requestDescribeLifecycleHookTypes $
--             newDescribeLifecycleHookTypes
--
--         , requestDescribeLifecycleHooks $
--             newDescribeLifecycleHooks
--
--         , requestDescribeLoadBalancerTargetGroups $
--             newDescribeLoadBalancerTargetGroups
--
--         , requestDescribeLoadBalancers $
--             newDescribeLoadBalancers
--
--         , requestDescribeMetricCollectionTypes $
--             newDescribeMetricCollectionTypes
--
--         , requestDescribeNotificationConfigurations $
--             newDescribeNotificationConfigurations
--
--         , requestDescribePolicies $
--             newDescribePolicies
--
--         , requestDescribeScalingActivities $
--             newDescribeScalingActivities
--
--         , requestDescribeScalingProcessTypes $
--             newDescribeScalingProcessTypes
--
--         , requestDescribeScheduledActions $
--             newDescribeScheduledActions
--
--         , requestDescribeTags $
--             newDescribeTags
--
--         , requestDescribeTerminationPolicyTypes $
--             newDescribeTerminationPolicyTypes
--
--         , requestDescribeTrafficSources $
--             newDescribeTrafficSources
--
--         , requestDescribeWarmPool $
--             newDescribeWarmPool
--
--         , requestDetachInstances $
--             newDetachInstances
--
--         , requestDetachLoadBalancerTargetGroups $
--             newDetachLoadBalancerTargetGroups
--
--         , requestDetachLoadBalancers $
--             newDetachLoadBalancers
--
--         , requestDetachTrafficSources $
--             newDetachTrafficSources
--
--         , requestDisableMetricsCollection $
--             newDisableMetricsCollection
--
--         , requestEnableMetricsCollection $
--             newEnableMetricsCollection
--
--         , requestEnterStandby $
--             newEnterStandby
--
--         , requestExecutePolicy $
--             newExecutePolicy
--
--         , requestExitStandby $
--             newExitStandby
--
--         , requestGetPredictiveScalingForecast $
--             newGetPredictiveScalingForecast
--
--         , requestPutLifecycleHook $
--             newPutLifecycleHook
--
--         , requestPutNotificationConfiguration $
--             newPutNotificationConfiguration
--
--         , requestPutScalingPolicy $
--             newPutScalingPolicy
--
--         , requestPutScheduledUpdateGroupAction $
--             newPutScheduledUpdateGroupAction
--
--         , requestPutWarmPool $
--             newPutWarmPool
--
--         , requestRecordLifecycleActionHeartbeat $
--             newRecordLifecycleActionHeartbeat
--
--         , requestResumeProcesses $
--             newResumeProcesses
--
--         , requestRollbackInstanceRefresh $
--             newRollbackInstanceRefresh
--
--         , requestSetDesiredCapacity $
--             newSetDesiredCapacity
--
--         , requestSetInstanceHealth $
--             newSetInstanceHealth
--
--         , requestSetInstanceProtection $
--             newSetInstanceProtection
--
--         , requestStartInstanceRefresh $
--             newStartInstanceRefresh
--
--         , requestSuspendProcesses $
--             newSuspendProcesses
--
--         , requestTerminateInstanceInAutoScalingGroup $
--             newTerminateInstanceInAutoScalingGroup
--
--         , requestUpdateAutoScalingGroup $
--             newUpdateAutoScalingGroup
--
--           ]

--     , testGroup "response"
--         [ responseAttachInstances $
--             newAttachInstancesResponse
--
--         , responseAttachLoadBalancerTargetGroups $
--             newAttachLoadBalancerTargetGroupsResponse
--
--         , responseAttachLoadBalancers $
--             newAttachLoadBalancersResponse
--
--         , responseAttachTrafficSources $
--             newAttachTrafficSourcesResponse
--
--         , responseBatchDeleteScheduledAction $
--             newBatchDeleteScheduledActionResponse
--
--         , responseBatchPutScheduledUpdateGroupAction $
--             newBatchPutScheduledUpdateGroupActionResponse
--
--         , responseCancelInstanceRefresh $
--             newCancelInstanceRefreshResponse
--
--         , responseCompleteLifecycleAction $
--             newCompleteLifecycleActionResponse
--
--         , responseCreateAutoScalingGroup $
--             newCreateAutoScalingGroupResponse
--
--         , responseCreateLaunchConfiguration $
--             newCreateLaunchConfigurationResponse
--
--         , responseCreateOrUpdateTags $
--             newCreateOrUpdateTagsResponse
--
--         , responseDeleteAutoScalingGroup $
--             newDeleteAutoScalingGroupResponse
--
--         , responseDeleteLaunchConfiguration $
--             newDeleteLaunchConfigurationResponse
--
--         , responseDeleteLifecycleHook $
--             newDeleteLifecycleHookResponse
--
--         , responseDeleteNotificationConfiguration $
--             newDeleteNotificationConfigurationResponse
--
--         , responseDeletePolicy $
--             newDeletePolicyResponse
--
--         , responseDeleteScheduledAction $
--             newDeleteScheduledActionResponse
--
--         , responseDeleteTags $
--             newDeleteTagsResponse
--
--         , responseDeleteWarmPool $
--             newDeleteWarmPoolResponse
--
--         , responseDescribeAccountLimits $
--             newDescribeAccountLimitsResponse
--
--         , responseDescribeAdjustmentTypes $
--             newDescribeAdjustmentTypesResponse
--
--         , responseDescribeAutoScalingGroups $
--             newDescribeAutoScalingGroupsResponse
--
--         , responseDescribeAutoScalingInstances $
--             newDescribeAutoScalingInstancesResponse
--
--         , responseDescribeAutoScalingNotificationTypes $
--             newDescribeAutoScalingNotificationTypesResponse
--
--         , responseDescribeInstanceRefreshes $
--             newDescribeInstanceRefreshesResponse
--
--         , responseDescribeLaunchConfigurations $
--             newDescribeLaunchConfigurationsResponse
--
--         , responseDescribeLifecycleHookTypes $
--             newDescribeLifecycleHookTypesResponse
--
--         , responseDescribeLifecycleHooks $
--             newDescribeLifecycleHooksResponse
--
--         , responseDescribeLoadBalancerTargetGroups $
--             newDescribeLoadBalancerTargetGroupsResponse
--
--         , responseDescribeLoadBalancers $
--             newDescribeLoadBalancersResponse
--
--         , responseDescribeMetricCollectionTypes $
--             newDescribeMetricCollectionTypesResponse
--
--         , responseDescribeNotificationConfigurations $
--             newDescribeNotificationConfigurationsResponse
--
--         , responseDescribePolicies $
--             newDescribePoliciesResponse
--
--         , responseDescribeScalingActivities $
--             newDescribeScalingActivitiesResponse
--
--         , responseDescribeScalingProcessTypes $
--             newDescribeScalingProcessTypesResponse
--
--         , responseDescribeScheduledActions $
--             newDescribeScheduledActionsResponse
--
--         , responseDescribeTags $
--             newDescribeTagsResponse
--
--         , responseDescribeTerminationPolicyTypes $
--             newDescribeTerminationPolicyTypesResponse
--
--         , responseDescribeTrafficSources $
--             newDescribeTrafficSourcesResponse
--
--         , responseDescribeWarmPool $
--             newDescribeWarmPoolResponse
--
--         , responseDetachInstances $
--             newDetachInstancesResponse
--
--         , responseDetachLoadBalancerTargetGroups $
--             newDetachLoadBalancerTargetGroupsResponse
--
--         , responseDetachLoadBalancers $
--             newDetachLoadBalancersResponse
--
--         , responseDetachTrafficSources $
--             newDetachTrafficSourcesResponse
--
--         , responseDisableMetricsCollection $
--             newDisableMetricsCollectionResponse
--
--         , responseEnableMetricsCollection $
--             newEnableMetricsCollectionResponse
--
--         , responseEnterStandby $
--             newEnterStandbyResponse
--
--         , responseExecutePolicy $
--             newExecutePolicyResponse
--
--         , responseExitStandby $
--             newExitStandbyResponse
--
--         , responseGetPredictiveScalingForecast $
--             newGetPredictiveScalingForecastResponse
--
--         , responsePutLifecycleHook $
--             newPutLifecycleHookResponse
--
--         , responsePutNotificationConfiguration $
--             newPutNotificationConfigurationResponse
--
--         , responsePutScalingPolicy $
--             newPutScalingPolicyResponse
--
--         , responsePutScheduledUpdateGroupAction $
--             newPutScheduledUpdateGroupActionResponse
--
--         , responsePutWarmPool $
--             newPutWarmPoolResponse
--
--         , responseRecordLifecycleActionHeartbeat $
--             newRecordLifecycleActionHeartbeatResponse
--
--         , responseResumeProcesses $
--             newResumeProcessesResponse
--
--         , responseRollbackInstanceRefresh $
--             newRollbackInstanceRefreshResponse
--
--         , responseSetDesiredCapacity $
--             newSetDesiredCapacityResponse
--
--         , responseSetInstanceHealth $
--             newSetInstanceHealthResponse
--
--         , responseSetInstanceProtection $
--             newSetInstanceProtectionResponse
--
--         , responseStartInstanceRefresh $
--             newStartInstanceRefreshResponse
--
--         , responseSuspendProcesses $
--             newSuspendProcessesResponse
--
--         , responseTerminateInstanceInAutoScalingGroup $
--             newTerminateInstanceInAutoScalingGroupResponse
--
--         , responseUpdateAutoScalingGroup $
--             newUpdateAutoScalingGroupResponse
--
--           ]
--     ]

-- Requests

requestAttachInstances :: AttachInstances -> TestTree
requestAttachInstances =
  req
    "AttachInstances"
    "fixture/AttachInstances.yaml"

requestAttachLoadBalancerTargetGroups :: AttachLoadBalancerTargetGroups -> TestTree
requestAttachLoadBalancerTargetGroups =
  req
    "AttachLoadBalancerTargetGroups"
    "fixture/AttachLoadBalancerTargetGroups.yaml"

requestAttachLoadBalancers :: AttachLoadBalancers -> TestTree
requestAttachLoadBalancers =
  req
    "AttachLoadBalancers"
    "fixture/AttachLoadBalancers.yaml"

requestAttachTrafficSources :: AttachTrafficSources -> TestTree
requestAttachTrafficSources =
  req
    "AttachTrafficSources"
    "fixture/AttachTrafficSources.yaml"

requestBatchDeleteScheduledAction :: BatchDeleteScheduledAction -> TestTree
requestBatchDeleteScheduledAction =
  req
    "BatchDeleteScheduledAction"
    "fixture/BatchDeleteScheduledAction.yaml"

requestBatchPutScheduledUpdateGroupAction :: BatchPutScheduledUpdateGroupAction -> TestTree
requestBatchPutScheduledUpdateGroupAction =
  req
    "BatchPutScheduledUpdateGroupAction"
    "fixture/BatchPutScheduledUpdateGroupAction.yaml"

requestCancelInstanceRefresh :: CancelInstanceRefresh -> TestTree
requestCancelInstanceRefresh =
  req
    "CancelInstanceRefresh"
    "fixture/CancelInstanceRefresh.yaml"

requestCompleteLifecycleAction :: CompleteLifecycleAction -> TestTree
requestCompleteLifecycleAction =
  req
    "CompleteLifecycleAction"
    "fixture/CompleteLifecycleAction.yaml"

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

requestCreateOrUpdateTags :: CreateOrUpdateTags -> TestTree
requestCreateOrUpdateTags =
  req
    "CreateOrUpdateTags"
    "fixture/CreateOrUpdateTags.yaml"

requestDeleteAutoScalingGroup :: DeleteAutoScalingGroup -> TestTree
requestDeleteAutoScalingGroup =
  req
    "DeleteAutoScalingGroup"
    "fixture/DeleteAutoScalingGroup.yaml"

requestDeleteLaunchConfiguration :: DeleteLaunchConfiguration -> TestTree
requestDeleteLaunchConfiguration =
  req
    "DeleteLaunchConfiguration"
    "fixture/DeleteLaunchConfiguration.yaml"

requestDeleteLifecycleHook :: DeleteLifecycleHook -> TestTree
requestDeleteLifecycleHook =
  req
    "DeleteLifecycleHook"
    "fixture/DeleteLifecycleHook.yaml"

requestDeleteNotificationConfiguration :: DeleteNotificationConfiguration -> TestTree
requestDeleteNotificationConfiguration =
  req
    "DeleteNotificationConfiguration"
    "fixture/DeleteNotificationConfiguration.yaml"

requestDeletePolicy :: DeletePolicy -> TestTree
requestDeletePolicy =
  req
    "DeletePolicy"
    "fixture/DeletePolicy.yaml"

requestDeleteScheduledAction :: DeleteScheduledAction -> TestTree
requestDeleteScheduledAction =
  req
    "DeleteScheduledAction"
    "fixture/DeleteScheduledAction.yaml"

requestDeleteTags :: DeleteTags -> TestTree
requestDeleteTags =
  req
    "DeleteTags"
    "fixture/DeleteTags.yaml"

requestDeleteWarmPool :: DeleteWarmPool -> TestTree
requestDeleteWarmPool =
  req
    "DeleteWarmPool"
    "fixture/DeleteWarmPool.yaml"

requestDescribeAccountLimits :: DescribeAccountLimits -> TestTree
requestDescribeAccountLimits =
  req
    "DescribeAccountLimits"
    "fixture/DescribeAccountLimits.yaml"

requestDescribeAdjustmentTypes :: DescribeAdjustmentTypes -> TestTree
requestDescribeAdjustmentTypes =
  req
    "DescribeAdjustmentTypes"
    "fixture/DescribeAdjustmentTypes.yaml"

requestDescribeAutoScalingGroups :: DescribeAutoScalingGroups -> TestTree
requestDescribeAutoScalingGroups =
  req
    "DescribeAutoScalingGroups"
    "fixture/DescribeAutoScalingGroups.yaml"

requestDescribeAutoScalingInstances :: DescribeAutoScalingInstances -> TestTree
requestDescribeAutoScalingInstances =
  req
    "DescribeAutoScalingInstances"
    "fixture/DescribeAutoScalingInstances.yaml"

requestDescribeAutoScalingNotificationTypes :: DescribeAutoScalingNotificationTypes -> TestTree
requestDescribeAutoScalingNotificationTypes =
  req
    "DescribeAutoScalingNotificationTypes"
    "fixture/DescribeAutoScalingNotificationTypes.yaml"

requestDescribeInstanceRefreshes :: DescribeInstanceRefreshes -> TestTree
requestDescribeInstanceRefreshes =
  req
    "DescribeInstanceRefreshes"
    "fixture/DescribeInstanceRefreshes.yaml"

requestDescribeLaunchConfigurations :: DescribeLaunchConfigurations -> TestTree
requestDescribeLaunchConfigurations =
  req
    "DescribeLaunchConfigurations"
    "fixture/DescribeLaunchConfigurations.yaml"

requestDescribeLifecycleHookTypes :: DescribeLifecycleHookTypes -> TestTree
requestDescribeLifecycleHookTypes =
  req
    "DescribeLifecycleHookTypes"
    "fixture/DescribeLifecycleHookTypes.yaml"

requestDescribeLifecycleHooks :: DescribeLifecycleHooks -> TestTree
requestDescribeLifecycleHooks =
  req
    "DescribeLifecycleHooks"
    "fixture/DescribeLifecycleHooks.yaml"

requestDescribeLoadBalancerTargetGroups :: DescribeLoadBalancerTargetGroups -> TestTree
requestDescribeLoadBalancerTargetGroups =
  req
    "DescribeLoadBalancerTargetGroups"
    "fixture/DescribeLoadBalancerTargetGroups.yaml"

requestDescribeLoadBalancers :: DescribeLoadBalancers -> TestTree
requestDescribeLoadBalancers =
  req
    "DescribeLoadBalancers"
    "fixture/DescribeLoadBalancers.yaml"

requestDescribeMetricCollectionTypes :: DescribeMetricCollectionTypes -> TestTree
requestDescribeMetricCollectionTypes =
  req
    "DescribeMetricCollectionTypes"
    "fixture/DescribeMetricCollectionTypes.yaml"

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

requestDescribeScalingActivities :: DescribeScalingActivities -> TestTree
requestDescribeScalingActivities =
  req
    "DescribeScalingActivities"
    "fixture/DescribeScalingActivities.yaml"

requestDescribeScalingProcessTypes :: DescribeScalingProcessTypes -> TestTree
requestDescribeScalingProcessTypes =
  req
    "DescribeScalingProcessTypes"
    "fixture/DescribeScalingProcessTypes.yaml"

requestDescribeScheduledActions :: DescribeScheduledActions -> TestTree
requestDescribeScheduledActions =
  req
    "DescribeScheduledActions"
    "fixture/DescribeScheduledActions.yaml"

requestDescribeTags :: DescribeTags -> TestTree
requestDescribeTags =
  req
    "DescribeTags"
    "fixture/DescribeTags.yaml"

requestDescribeTerminationPolicyTypes :: DescribeTerminationPolicyTypes -> TestTree
requestDescribeTerminationPolicyTypes =
  req
    "DescribeTerminationPolicyTypes"
    "fixture/DescribeTerminationPolicyTypes.yaml"

requestDescribeTrafficSources :: DescribeTrafficSources -> TestTree
requestDescribeTrafficSources =
  req
    "DescribeTrafficSources"
    "fixture/DescribeTrafficSources.yaml"

requestDescribeWarmPool :: DescribeWarmPool -> TestTree
requestDescribeWarmPool =
  req
    "DescribeWarmPool"
    "fixture/DescribeWarmPool.yaml"

requestDetachInstances :: DetachInstances -> TestTree
requestDetachInstances =
  req
    "DetachInstances"
    "fixture/DetachInstances.yaml"

requestDetachLoadBalancerTargetGroups :: DetachLoadBalancerTargetGroups -> TestTree
requestDetachLoadBalancerTargetGroups =
  req
    "DetachLoadBalancerTargetGroups"
    "fixture/DetachLoadBalancerTargetGroups.yaml"

requestDetachLoadBalancers :: DetachLoadBalancers -> TestTree
requestDetachLoadBalancers =
  req
    "DetachLoadBalancers"
    "fixture/DetachLoadBalancers.yaml"

requestDetachTrafficSources :: DetachTrafficSources -> TestTree
requestDetachTrafficSources =
  req
    "DetachTrafficSources"
    "fixture/DetachTrafficSources.yaml"

requestDisableMetricsCollection :: DisableMetricsCollection -> TestTree
requestDisableMetricsCollection =
  req
    "DisableMetricsCollection"
    "fixture/DisableMetricsCollection.yaml"

requestEnableMetricsCollection :: EnableMetricsCollection -> TestTree
requestEnableMetricsCollection =
  req
    "EnableMetricsCollection"
    "fixture/EnableMetricsCollection.yaml"

requestEnterStandby :: EnterStandby -> TestTree
requestEnterStandby =
  req
    "EnterStandby"
    "fixture/EnterStandby.yaml"

requestExecutePolicy :: ExecutePolicy -> TestTree
requestExecutePolicy =
  req
    "ExecutePolicy"
    "fixture/ExecutePolicy.yaml"

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

requestPutLifecycleHook :: PutLifecycleHook -> TestTree
requestPutLifecycleHook =
  req
    "PutLifecycleHook"
    "fixture/PutLifecycleHook.yaml"

requestPutNotificationConfiguration :: PutNotificationConfiguration -> TestTree
requestPutNotificationConfiguration =
  req
    "PutNotificationConfiguration"
    "fixture/PutNotificationConfiguration.yaml"

requestPutScalingPolicy :: PutScalingPolicy -> TestTree
requestPutScalingPolicy =
  req
    "PutScalingPolicy"
    "fixture/PutScalingPolicy.yaml"

requestPutScheduledUpdateGroupAction :: PutScheduledUpdateGroupAction -> TestTree
requestPutScheduledUpdateGroupAction =
  req
    "PutScheduledUpdateGroupAction"
    "fixture/PutScheduledUpdateGroupAction.yaml"

requestPutWarmPool :: PutWarmPool -> TestTree
requestPutWarmPool =
  req
    "PutWarmPool"
    "fixture/PutWarmPool.yaml"

requestRecordLifecycleActionHeartbeat :: RecordLifecycleActionHeartbeat -> TestTree
requestRecordLifecycleActionHeartbeat =
  req
    "RecordLifecycleActionHeartbeat"
    "fixture/RecordLifecycleActionHeartbeat.yaml"

requestResumeProcesses :: ResumeProcesses -> TestTree
requestResumeProcesses =
  req
    "ResumeProcesses"
    "fixture/ResumeProcesses.yaml"

requestRollbackInstanceRefresh :: RollbackInstanceRefresh -> TestTree
requestRollbackInstanceRefresh =
  req
    "RollbackInstanceRefresh"
    "fixture/RollbackInstanceRefresh.yaml"

requestSetDesiredCapacity :: SetDesiredCapacity -> TestTree
requestSetDesiredCapacity =
  req
    "SetDesiredCapacity"
    "fixture/SetDesiredCapacity.yaml"

requestSetInstanceHealth :: SetInstanceHealth -> TestTree
requestSetInstanceHealth =
  req
    "SetInstanceHealth"
    "fixture/SetInstanceHealth.yaml"

requestSetInstanceProtection :: SetInstanceProtection -> TestTree
requestSetInstanceProtection =
  req
    "SetInstanceProtection"
    "fixture/SetInstanceProtection.yaml"

requestStartInstanceRefresh :: StartInstanceRefresh -> TestTree
requestStartInstanceRefresh =
  req
    "StartInstanceRefresh"
    "fixture/StartInstanceRefresh.yaml"

requestSuspendProcesses :: SuspendProcesses -> TestTree
requestSuspendProcesses =
  req
    "SuspendProcesses"
    "fixture/SuspendProcesses.yaml"

requestTerminateInstanceInAutoScalingGroup :: TerminateInstanceInAutoScalingGroup -> TestTree
requestTerminateInstanceInAutoScalingGroup =
  req
    "TerminateInstanceInAutoScalingGroup"
    "fixture/TerminateInstanceInAutoScalingGroup.yaml"

requestUpdateAutoScalingGroup :: UpdateAutoScalingGroup -> TestTree
requestUpdateAutoScalingGroup =
  req
    "UpdateAutoScalingGroup"
    "fixture/UpdateAutoScalingGroup.yaml"

-- Responses

responseAttachInstances :: AttachInstancesResponse -> TestTree
responseAttachInstances =
  res
    "AttachInstancesResponse"
    "fixture/AttachInstancesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AttachInstances)

responseAttachLoadBalancerTargetGroups :: AttachLoadBalancerTargetGroupsResponse -> TestTree
responseAttachLoadBalancerTargetGroups =
  res
    "AttachLoadBalancerTargetGroupsResponse"
    "fixture/AttachLoadBalancerTargetGroupsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AttachLoadBalancerTargetGroups)

responseAttachLoadBalancers :: AttachLoadBalancersResponse -> TestTree
responseAttachLoadBalancers =
  res
    "AttachLoadBalancersResponse"
    "fixture/AttachLoadBalancersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AttachLoadBalancers)

responseAttachTrafficSources :: AttachTrafficSourcesResponse -> TestTree
responseAttachTrafficSources =
  res
    "AttachTrafficSourcesResponse"
    "fixture/AttachTrafficSourcesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AttachTrafficSources)

responseBatchDeleteScheduledAction :: BatchDeleteScheduledActionResponse -> TestTree
responseBatchDeleteScheduledAction =
  res
    "BatchDeleteScheduledActionResponse"
    "fixture/BatchDeleteScheduledActionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchDeleteScheduledAction)

responseBatchPutScheduledUpdateGroupAction :: BatchPutScheduledUpdateGroupActionResponse -> TestTree
responseBatchPutScheduledUpdateGroupAction =
  res
    "BatchPutScheduledUpdateGroupActionResponse"
    "fixture/BatchPutScheduledUpdateGroupActionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchPutScheduledUpdateGroupAction)

responseCancelInstanceRefresh :: CancelInstanceRefreshResponse -> TestTree
responseCancelInstanceRefresh =
  res
    "CancelInstanceRefreshResponse"
    "fixture/CancelInstanceRefreshResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CancelInstanceRefresh)

responseCompleteLifecycleAction :: CompleteLifecycleActionResponse -> TestTree
responseCompleteLifecycleAction =
  res
    "CompleteLifecycleActionResponse"
    "fixture/CompleteLifecycleActionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CompleteLifecycleAction)

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

responseCreateOrUpdateTags :: CreateOrUpdateTagsResponse -> TestTree
responseCreateOrUpdateTags =
  res
    "CreateOrUpdateTagsResponse"
    "fixture/CreateOrUpdateTagsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateOrUpdateTags)

responseDeleteAutoScalingGroup :: DeleteAutoScalingGroupResponse -> TestTree
responseDeleteAutoScalingGroup =
  res
    "DeleteAutoScalingGroupResponse"
    "fixture/DeleteAutoScalingGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteAutoScalingGroup)

responseDeleteLaunchConfiguration :: DeleteLaunchConfigurationResponse -> TestTree
responseDeleteLaunchConfiguration =
  res
    "DeleteLaunchConfigurationResponse"
    "fixture/DeleteLaunchConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteLaunchConfiguration)

responseDeleteLifecycleHook :: DeleteLifecycleHookResponse -> TestTree
responseDeleteLifecycleHook =
  res
    "DeleteLifecycleHookResponse"
    "fixture/DeleteLifecycleHookResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteLifecycleHook)

responseDeleteNotificationConfiguration :: DeleteNotificationConfigurationResponse -> TestTree
responseDeleteNotificationConfiguration =
  res
    "DeleteNotificationConfigurationResponse"
    "fixture/DeleteNotificationConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteNotificationConfiguration)

responseDeletePolicy :: DeletePolicyResponse -> TestTree
responseDeletePolicy =
  res
    "DeletePolicyResponse"
    "fixture/DeletePolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeletePolicy)

responseDeleteScheduledAction :: DeleteScheduledActionResponse -> TestTree
responseDeleteScheduledAction =
  res
    "DeleteScheduledActionResponse"
    "fixture/DeleteScheduledActionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteScheduledAction)

responseDeleteTags :: DeleteTagsResponse -> TestTree
responseDeleteTags =
  res
    "DeleteTagsResponse"
    "fixture/DeleteTagsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteTags)

responseDeleteWarmPool :: DeleteWarmPoolResponse -> TestTree
responseDeleteWarmPool =
  res
    "DeleteWarmPoolResponse"
    "fixture/DeleteWarmPoolResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteWarmPool)

responseDescribeAccountLimits :: DescribeAccountLimitsResponse -> TestTree
responseDescribeAccountLimits =
  res
    "DescribeAccountLimitsResponse"
    "fixture/DescribeAccountLimitsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeAccountLimits)

responseDescribeAdjustmentTypes :: DescribeAdjustmentTypesResponse -> TestTree
responseDescribeAdjustmentTypes =
  res
    "DescribeAdjustmentTypesResponse"
    "fixture/DescribeAdjustmentTypesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeAdjustmentTypes)

responseDescribeAutoScalingGroups :: DescribeAutoScalingGroupsResponse -> TestTree
responseDescribeAutoScalingGroups =
  res
    "DescribeAutoScalingGroupsResponse"
    "fixture/DescribeAutoScalingGroupsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeAutoScalingGroups)

responseDescribeAutoScalingInstances :: DescribeAutoScalingInstancesResponse -> TestTree
responseDescribeAutoScalingInstances =
  res
    "DescribeAutoScalingInstancesResponse"
    "fixture/DescribeAutoScalingInstancesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeAutoScalingInstances)

responseDescribeAutoScalingNotificationTypes :: DescribeAutoScalingNotificationTypesResponse -> TestTree
responseDescribeAutoScalingNotificationTypes =
  res
    "DescribeAutoScalingNotificationTypesResponse"
    "fixture/DescribeAutoScalingNotificationTypesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeAutoScalingNotificationTypes)

responseDescribeInstanceRefreshes :: DescribeInstanceRefreshesResponse -> TestTree
responseDescribeInstanceRefreshes =
  res
    "DescribeInstanceRefreshesResponse"
    "fixture/DescribeInstanceRefreshesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeInstanceRefreshes)

responseDescribeLaunchConfigurations :: DescribeLaunchConfigurationsResponse -> TestTree
responseDescribeLaunchConfigurations =
  res
    "DescribeLaunchConfigurationsResponse"
    "fixture/DescribeLaunchConfigurationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeLaunchConfigurations)

responseDescribeLifecycleHookTypes :: DescribeLifecycleHookTypesResponse -> TestTree
responseDescribeLifecycleHookTypes =
  res
    "DescribeLifecycleHookTypesResponse"
    "fixture/DescribeLifecycleHookTypesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeLifecycleHookTypes)

responseDescribeLifecycleHooks :: DescribeLifecycleHooksResponse -> TestTree
responseDescribeLifecycleHooks =
  res
    "DescribeLifecycleHooksResponse"
    "fixture/DescribeLifecycleHooksResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeLifecycleHooks)

responseDescribeLoadBalancerTargetGroups :: DescribeLoadBalancerTargetGroupsResponse -> TestTree
responseDescribeLoadBalancerTargetGroups =
  res
    "DescribeLoadBalancerTargetGroupsResponse"
    "fixture/DescribeLoadBalancerTargetGroupsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeLoadBalancerTargetGroups)

responseDescribeLoadBalancers :: DescribeLoadBalancersResponse -> TestTree
responseDescribeLoadBalancers =
  res
    "DescribeLoadBalancersResponse"
    "fixture/DescribeLoadBalancersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeLoadBalancers)

responseDescribeMetricCollectionTypes :: DescribeMetricCollectionTypesResponse -> TestTree
responseDescribeMetricCollectionTypes =
  res
    "DescribeMetricCollectionTypesResponse"
    "fixture/DescribeMetricCollectionTypesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeMetricCollectionTypes)

responseDescribeNotificationConfigurations :: DescribeNotificationConfigurationsResponse -> TestTree
responseDescribeNotificationConfigurations =
  res
    "DescribeNotificationConfigurationsResponse"
    "fixture/DescribeNotificationConfigurationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeNotificationConfigurations)

responseDescribePolicies :: DescribePoliciesResponse -> TestTree
responseDescribePolicies =
  res
    "DescribePoliciesResponse"
    "fixture/DescribePoliciesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribePolicies)

responseDescribeScalingActivities :: DescribeScalingActivitiesResponse -> TestTree
responseDescribeScalingActivities =
  res
    "DescribeScalingActivitiesResponse"
    "fixture/DescribeScalingActivitiesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeScalingActivities)

responseDescribeScalingProcessTypes :: DescribeScalingProcessTypesResponse -> TestTree
responseDescribeScalingProcessTypes =
  res
    "DescribeScalingProcessTypesResponse"
    "fixture/DescribeScalingProcessTypesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeScalingProcessTypes)

responseDescribeScheduledActions :: DescribeScheduledActionsResponse -> TestTree
responseDescribeScheduledActions =
  res
    "DescribeScheduledActionsResponse"
    "fixture/DescribeScheduledActionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeScheduledActions)

responseDescribeTags :: DescribeTagsResponse -> TestTree
responseDescribeTags =
  res
    "DescribeTagsResponse"
    "fixture/DescribeTagsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeTags)

responseDescribeTerminationPolicyTypes :: DescribeTerminationPolicyTypesResponse -> TestTree
responseDescribeTerminationPolicyTypes =
  res
    "DescribeTerminationPolicyTypesResponse"
    "fixture/DescribeTerminationPolicyTypesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeTerminationPolicyTypes)

responseDescribeTrafficSources :: DescribeTrafficSourcesResponse -> TestTree
responseDescribeTrafficSources =
  res
    "DescribeTrafficSourcesResponse"
    "fixture/DescribeTrafficSourcesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeTrafficSources)

responseDescribeWarmPool :: DescribeWarmPoolResponse -> TestTree
responseDescribeWarmPool =
  res
    "DescribeWarmPoolResponse"
    "fixture/DescribeWarmPoolResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeWarmPool)

responseDetachInstances :: DetachInstancesResponse -> TestTree
responseDetachInstances =
  res
    "DetachInstancesResponse"
    "fixture/DetachInstancesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DetachInstances)

responseDetachLoadBalancerTargetGroups :: DetachLoadBalancerTargetGroupsResponse -> TestTree
responseDetachLoadBalancerTargetGroups =
  res
    "DetachLoadBalancerTargetGroupsResponse"
    "fixture/DetachLoadBalancerTargetGroupsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DetachLoadBalancerTargetGroups)

responseDetachLoadBalancers :: DetachLoadBalancersResponse -> TestTree
responseDetachLoadBalancers =
  res
    "DetachLoadBalancersResponse"
    "fixture/DetachLoadBalancersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DetachLoadBalancers)

responseDetachTrafficSources :: DetachTrafficSourcesResponse -> TestTree
responseDetachTrafficSources =
  res
    "DetachTrafficSourcesResponse"
    "fixture/DetachTrafficSourcesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DetachTrafficSources)

responseDisableMetricsCollection :: DisableMetricsCollectionResponse -> TestTree
responseDisableMetricsCollection =
  res
    "DisableMetricsCollectionResponse"
    "fixture/DisableMetricsCollectionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisableMetricsCollection)

responseEnableMetricsCollection :: EnableMetricsCollectionResponse -> TestTree
responseEnableMetricsCollection =
  res
    "EnableMetricsCollectionResponse"
    "fixture/EnableMetricsCollectionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy EnableMetricsCollection)

responseEnterStandby :: EnterStandbyResponse -> TestTree
responseEnterStandby =
  res
    "EnterStandbyResponse"
    "fixture/EnterStandbyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy EnterStandby)

responseExecutePolicy :: ExecutePolicyResponse -> TestTree
responseExecutePolicy =
  res
    "ExecutePolicyResponse"
    "fixture/ExecutePolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ExecutePolicy)

responseExitStandby :: ExitStandbyResponse -> TestTree
responseExitStandby =
  res
    "ExitStandbyResponse"
    "fixture/ExitStandbyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ExitStandby)

responseGetPredictiveScalingForecast :: GetPredictiveScalingForecastResponse -> TestTree
responseGetPredictiveScalingForecast =
  res
    "GetPredictiveScalingForecastResponse"
    "fixture/GetPredictiveScalingForecastResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetPredictiveScalingForecast)

responsePutLifecycleHook :: PutLifecycleHookResponse -> TestTree
responsePutLifecycleHook =
  res
    "PutLifecycleHookResponse"
    "fixture/PutLifecycleHookResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutLifecycleHook)

responsePutNotificationConfiguration :: PutNotificationConfigurationResponse -> TestTree
responsePutNotificationConfiguration =
  res
    "PutNotificationConfigurationResponse"
    "fixture/PutNotificationConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutNotificationConfiguration)

responsePutScalingPolicy :: PutScalingPolicyResponse -> TestTree
responsePutScalingPolicy =
  res
    "PutScalingPolicyResponse"
    "fixture/PutScalingPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutScalingPolicy)

responsePutScheduledUpdateGroupAction :: PutScheduledUpdateGroupActionResponse -> TestTree
responsePutScheduledUpdateGroupAction =
  res
    "PutScheduledUpdateGroupActionResponse"
    "fixture/PutScheduledUpdateGroupActionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutScheduledUpdateGroupAction)

responsePutWarmPool :: PutWarmPoolResponse -> TestTree
responsePutWarmPool =
  res
    "PutWarmPoolResponse"
    "fixture/PutWarmPoolResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutWarmPool)

responseRecordLifecycleActionHeartbeat :: RecordLifecycleActionHeartbeatResponse -> TestTree
responseRecordLifecycleActionHeartbeat =
  res
    "RecordLifecycleActionHeartbeatResponse"
    "fixture/RecordLifecycleActionHeartbeatResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RecordLifecycleActionHeartbeat)

responseResumeProcesses :: ResumeProcessesResponse -> TestTree
responseResumeProcesses =
  res
    "ResumeProcessesResponse"
    "fixture/ResumeProcessesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ResumeProcesses)

responseRollbackInstanceRefresh :: RollbackInstanceRefreshResponse -> TestTree
responseRollbackInstanceRefresh =
  res
    "RollbackInstanceRefreshResponse"
    "fixture/RollbackInstanceRefreshResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RollbackInstanceRefresh)

responseSetDesiredCapacity :: SetDesiredCapacityResponse -> TestTree
responseSetDesiredCapacity =
  res
    "SetDesiredCapacityResponse"
    "fixture/SetDesiredCapacityResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SetDesiredCapacity)

responseSetInstanceHealth :: SetInstanceHealthResponse -> TestTree
responseSetInstanceHealth =
  res
    "SetInstanceHealthResponse"
    "fixture/SetInstanceHealthResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SetInstanceHealth)

responseSetInstanceProtection :: SetInstanceProtectionResponse -> TestTree
responseSetInstanceProtection =
  res
    "SetInstanceProtectionResponse"
    "fixture/SetInstanceProtectionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SetInstanceProtection)

responseStartInstanceRefresh :: StartInstanceRefreshResponse -> TestTree
responseStartInstanceRefresh =
  res
    "StartInstanceRefreshResponse"
    "fixture/StartInstanceRefreshResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartInstanceRefresh)

responseSuspendProcesses :: SuspendProcessesResponse -> TestTree
responseSuspendProcesses =
  res
    "SuspendProcessesResponse"
    "fixture/SuspendProcessesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SuspendProcesses)

responseTerminateInstanceInAutoScalingGroup :: TerminateInstanceInAutoScalingGroupResponse -> TestTree
responseTerminateInstanceInAutoScalingGroup =
  res
    "TerminateInstanceInAutoScalingGroupResponse"
    "fixture/TerminateInstanceInAutoScalingGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TerminateInstanceInAutoScalingGroup)

responseUpdateAutoScalingGroup :: UpdateAutoScalingGroupResponse -> TestTree
responseUpdateAutoScalingGroup =
  res
    "UpdateAutoScalingGroupResponse"
    "fixture/UpdateAutoScalingGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateAutoScalingGroup)
