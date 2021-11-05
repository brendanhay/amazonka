{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.CodeGuruProfiler
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.AWS.Gen.CodeGuruProfiler where

import Amazonka.CodeGuruProfiler
import qualified Data.Proxy as Proxy
import Test.AWS.CodeGuruProfiler.Internal
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
--         [ requestGetRecommendations $
--             newGetRecommendations
--
--         , requestAddNotificationChannels $
--             newAddNotificationChannels
--
--         , requestDescribeProfilingGroup $
--             newDescribeProfilingGroup
--
--         , requestPutPermission $
--             newPutPermission
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestRemovePermission $
--             newRemovePermission
--
--         , requestSubmitFeedback $
--             newSubmitFeedback
--
--         , requestCreateProfilingGroup $
--             newCreateProfilingGroup
--
--         , requestRemoveNotificationChannel $
--             newRemoveNotificationChannel
--
--         , requestUpdateProfilingGroup $
--             newUpdateProfilingGroup
--
--         , requestDeleteProfilingGroup $
--             newDeleteProfilingGroup
--
--         , requestListFindingsReports $
--             newListFindingsReports
--
--         , requestListProfileTimes $
--             newListProfileTimes
--
--         , requestPostAgentProfile $
--             newPostAgentProfile
--
--         , requestGetProfile $
--             newGetProfile
--
--         , requestListProfilingGroups $
--             newListProfilingGroups
--
--         , requestTagResource $
--             newTagResource
--
--         , requestGetNotificationConfiguration $
--             newGetNotificationConfiguration
--
--         , requestBatchGetFrameMetricData $
--             newBatchGetFrameMetricData
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestGetFindingsReportAccountSummary $
--             newGetFindingsReportAccountSummary
--
--         , requestGetPolicy $
--             newGetPolicy
--
--         , requestConfigureAgent $
--             newConfigureAgent
--
--           ]

--     , testGroup "response"
--         [ responseGetRecommendations $
--             newGetRecommendationsResponse
--
--         , responseAddNotificationChannels $
--             newAddNotificationChannelsResponse
--
--         , responseDescribeProfilingGroup $
--             newDescribeProfilingGroupResponse
--
--         , responsePutPermission $
--             newPutPermissionResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseRemovePermission $
--             newRemovePermissionResponse
--
--         , responseSubmitFeedback $
--             newSubmitFeedbackResponse
--
--         , responseCreateProfilingGroup $
--             newCreateProfilingGroupResponse
--
--         , responseRemoveNotificationChannel $
--             newRemoveNotificationChannelResponse
--
--         , responseUpdateProfilingGroup $
--             newUpdateProfilingGroupResponse
--
--         , responseDeleteProfilingGroup $
--             newDeleteProfilingGroupResponse
--
--         , responseListFindingsReports $
--             newListFindingsReportsResponse
--
--         , responseListProfileTimes $
--             newListProfileTimesResponse
--
--         , responsePostAgentProfile $
--             newPostAgentProfileResponse
--
--         , responseGetProfile $
--             newGetProfileResponse
--
--         , responseListProfilingGroups $
--             newListProfilingGroupsResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseGetNotificationConfiguration $
--             newGetNotificationConfigurationResponse
--
--         , responseBatchGetFrameMetricData $
--             newBatchGetFrameMetricDataResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseGetFindingsReportAccountSummary $
--             newGetFindingsReportAccountSummaryResponse
--
--         , responseGetPolicy $
--             newGetPolicyResponse
--
--         , responseConfigureAgent $
--             newConfigureAgentResponse
--
--           ]
--     ]

-- Requests

requestGetRecommendations :: GetRecommendations -> TestTree
requestGetRecommendations =
  req
    "GetRecommendations"
    "fixture/GetRecommendations.yaml"

requestAddNotificationChannels :: AddNotificationChannels -> TestTree
requestAddNotificationChannels =
  req
    "AddNotificationChannels"
    "fixture/AddNotificationChannels.yaml"

requestDescribeProfilingGroup :: DescribeProfilingGroup -> TestTree
requestDescribeProfilingGroup =
  req
    "DescribeProfilingGroup"
    "fixture/DescribeProfilingGroup.yaml"

requestPutPermission :: PutPermission -> TestTree
requestPutPermission =
  req
    "PutPermission"
    "fixture/PutPermission.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestRemovePermission :: RemovePermission -> TestTree
requestRemovePermission =
  req
    "RemovePermission"
    "fixture/RemovePermission.yaml"

requestSubmitFeedback :: SubmitFeedback -> TestTree
requestSubmitFeedback =
  req
    "SubmitFeedback"
    "fixture/SubmitFeedback.yaml"

requestCreateProfilingGroup :: CreateProfilingGroup -> TestTree
requestCreateProfilingGroup =
  req
    "CreateProfilingGroup"
    "fixture/CreateProfilingGroup.yaml"

requestRemoveNotificationChannel :: RemoveNotificationChannel -> TestTree
requestRemoveNotificationChannel =
  req
    "RemoveNotificationChannel"
    "fixture/RemoveNotificationChannel.yaml"

requestUpdateProfilingGroup :: UpdateProfilingGroup -> TestTree
requestUpdateProfilingGroup =
  req
    "UpdateProfilingGroup"
    "fixture/UpdateProfilingGroup.yaml"

requestDeleteProfilingGroup :: DeleteProfilingGroup -> TestTree
requestDeleteProfilingGroup =
  req
    "DeleteProfilingGroup"
    "fixture/DeleteProfilingGroup.yaml"

requestListFindingsReports :: ListFindingsReports -> TestTree
requestListFindingsReports =
  req
    "ListFindingsReports"
    "fixture/ListFindingsReports.yaml"

requestListProfileTimes :: ListProfileTimes -> TestTree
requestListProfileTimes =
  req
    "ListProfileTimes"
    "fixture/ListProfileTimes.yaml"

requestPostAgentProfile :: PostAgentProfile -> TestTree
requestPostAgentProfile =
  req
    "PostAgentProfile"
    "fixture/PostAgentProfile.yaml"

requestGetProfile :: GetProfile -> TestTree
requestGetProfile =
  req
    "GetProfile"
    "fixture/GetProfile.yaml"

requestListProfilingGroups :: ListProfilingGroups -> TestTree
requestListProfilingGroups =
  req
    "ListProfilingGroups"
    "fixture/ListProfilingGroups.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestGetNotificationConfiguration :: GetNotificationConfiguration -> TestTree
requestGetNotificationConfiguration =
  req
    "GetNotificationConfiguration"
    "fixture/GetNotificationConfiguration.yaml"

requestBatchGetFrameMetricData :: BatchGetFrameMetricData -> TestTree
requestBatchGetFrameMetricData =
  req
    "BatchGetFrameMetricData"
    "fixture/BatchGetFrameMetricData.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestGetFindingsReportAccountSummary :: GetFindingsReportAccountSummary -> TestTree
requestGetFindingsReportAccountSummary =
  req
    "GetFindingsReportAccountSummary"
    "fixture/GetFindingsReportAccountSummary.yaml"

requestGetPolicy :: GetPolicy -> TestTree
requestGetPolicy =
  req
    "GetPolicy"
    "fixture/GetPolicy.yaml"

requestConfigureAgent :: ConfigureAgent -> TestTree
requestConfigureAgent =
  req
    "ConfigureAgent"
    "fixture/ConfigureAgent.yaml"

-- Responses

responseGetRecommendations :: GetRecommendationsResponse -> TestTree
responseGetRecommendations =
  res
    "GetRecommendationsResponse"
    "fixture/GetRecommendationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetRecommendations)

responseAddNotificationChannels :: AddNotificationChannelsResponse -> TestTree
responseAddNotificationChannels =
  res
    "AddNotificationChannelsResponse"
    "fixture/AddNotificationChannelsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AddNotificationChannels)

responseDescribeProfilingGroup :: DescribeProfilingGroupResponse -> TestTree
responseDescribeProfilingGroup =
  res
    "DescribeProfilingGroupResponse"
    "fixture/DescribeProfilingGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeProfilingGroup)

responsePutPermission :: PutPermissionResponse -> TestTree
responsePutPermission =
  res
    "PutPermissionResponse"
    "fixture/PutPermissionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutPermission)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responseRemovePermission :: RemovePermissionResponse -> TestTree
responseRemovePermission =
  res
    "RemovePermissionResponse"
    "fixture/RemovePermissionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RemovePermission)

responseSubmitFeedback :: SubmitFeedbackResponse -> TestTree
responseSubmitFeedback =
  res
    "SubmitFeedbackResponse"
    "fixture/SubmitFeedbackResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SubmitFeedback)

responseCreateProfilingGroup :: CreateProfilingGroupResponse -> TestTree
responseCreateProfilingGroup =
  res
    "CreateProfilingGroupResponse"
    "fixture/CreateProfilingGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateProfilingGroup)

responseRemoveNotificationChannel :: RemoveNotificationChannelResponse -> TestTree
responseRemoveNotificationChannel =
  res
    "RemoveNotificationChannelResponse"
    "fixture/RemoveNotificationChannelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RemoveNotificationChannel)

responseUpdateProfilingGroup :: UpdateProfilingGroupResponse -> TestTree
responseUpdateProfilingGroup =
  res
    "UpdateProfilingGroupResponse"
    "fixture/UpdateProfilingGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateProfilingGroup)

responseDeleteProfilingGroup :: DeleteProfilingGroupResponse -> TestTree
responseDeleteProfilingGroup =
  res
    "DeleteProfilingGroupResponse"
    "fixture/DeleteProfilingGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteProfilingGroup)

responseListFindingsReports :: ListFindingsReportsResponse -> TestTree
responseListFindingsReports =
  res
    "ListFindingsReportsResponse"
    "fixture/ListFindingsReportsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListFindingsReports)

responseListProfileTimes :: ListProfileTimesResponse -> TestTree
responseListProfileTimes =
  res
    "ListProfileTimesResponse"
    "fixture/ListProfileTimesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListProfileTimes)

responsePostAgentProfile :: PostAgentProfileResponse -> TestTree
responsePostAgentProfile =
  res
    "PostAgentProfileResponse"
    "fixture/PostAgentProfileResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PostAgentProfile)

responseGetProfile :: GetProfileResponse -> TestTree
responseGetProfile =
  res
    "GetProfileResponse"
    "fixture/GetProfileResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetProfile)

responseListProfilingGroups :: ListProfilingGroupsResponse -> TestTree
responseListProfilingGroups =
  res
    "ListProfilingGroupsResponse"
    "fixture/ListProfilingGroupsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListProfilingGroups)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TagResource)

responseGetNotificationConfiguration :: GetNotificationConfigurationResponse -> TestTree
responseGetNotificationConfiguration =
  res
    "GetNotificationConfigurationResponse"
    "fixture/GetNotificationConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetNotificationConfiguration)

responseBatchGetFrameMetricData :: BatchGetFrameMetricDataResponse -> TestTree
responseBatchGetFrameMetricData =
  res
    "BatchGetFrameMetricDataResponse"
    "fixture/BatchGetFrameMetricDataResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchGetFrameMetricData)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UntagResource)

responseGetFindingsReportAccountSummary :: GetFindingsReportAccountSummaryResponse -> TestTree
responseGetFindingsReportAccountSummary =
  res
    "GetFindingsReportAccountSummaryResponse"
    "fixture/GetFindingsReportAccountSummaryResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetFindingsReportAccountSummary)

responseGetPolicy :: GetPolicyResponse -> TestTree
responseGetPolicy =
  res
    "GetPolicyResponse"
    "fixture/GetPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetPolicy)

responseConfigureAgent :: ConfigureAgentResponse -> TestTree
responseConfigureAgent =
  res
    "ConfigureAgentResponse"
    "fixture/ConfigureAgentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ConfigureAgent)
