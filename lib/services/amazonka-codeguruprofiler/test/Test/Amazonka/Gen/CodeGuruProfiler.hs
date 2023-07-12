{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.CodeGuruProfiler
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.CodeGuruProfiler where

import Amazonka.CodeGuruProfiler
import qualified Data.Proxy as Proxy
import Test.Amazonka.CodeGuruProfiler.Internal
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
--         [ requestAddNotificationChannels $
--             newAddNotificationChannels
--
--         , requestBatchGetFrameMetricData $
--             newBatchGetFrameMetricData
--
--         , requestConfigureAgent $
--             newConfigureAgent
--
--         , requestCreateProfilingGroup $
--             newCreateProfilingGroup
--
--         , requestDeleteProfilingGroup $
--             newDeleteProfilingGroup
--
--         , requestDescribeProfilingGroup $
--             newDescribeProfilingGroup
--
--         , requestGetFindingsReportAccountSummary $
--             newGetFindingsReportAccountSummary
--
--         , requestGetNotificationConfiguration $
--             newGetNotificationConfiguration
--
--         , requestGetPolicy $
--             newGetPolicy
--
--         , requestGetProfile $
--             newGetProfile
--
--         , requestGetRecommendations $
--             newGetRecommendations
--
--         , requestListFindingsReports $
--             newListFindingsReports
--
--         , requestListProfileTimes $
--             newListProfileTimes
--
--         , requestListProfilingGroups $
--             newListProfilingGroups
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestPostAgentProfile $
--             newPostAgentProfile
--
--         , requestPutPermission $
--             newPutPermission
--
--         , requestRemoveNotificationChannel $
--             newRemoveNotificationChannel
--
--         , requestRemovePermission $
--             newRemovePermission
--
--         , requestSubmitFeedback $
--             newSubmitFeedback
--
--         , requestTagResource $
--             newTagResource
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestUpdateProfilingGroup $
--             newUpdateProfilingGroup
--
--           ]

--     , testGroup "response"
--         [ responseAddNotificationChannels $
--             newAddNotificationChannelsResponse
--
--         , responseBatchGetFrameMetricData $
--             newBatchGetFrameMetricDataResponse
--
--         , responseConfigureAgent $
--             newConfigureAgentResponse
--
--         , responseCreateProfilingGroup $
--             newCreateProfilingGroupResponse
--
--         , responseDeleteProfilingGroup $
--             newDeleteProfilingGroupResponse
--
--         , responseDescribeProfilingGroup $
--             newDescribeProfilingGroupResponse
--
--         , responseGetFindingsReportAccountSummary $
--             newGetFindingsReportAccountSummaryResponse
--
--         , responseGetNotificationConfiguration $
--             newGetNotificationConfigurationResponse
--
--         , responseGetPolicy $
--             newGetPolicyResponse
--
--         , responseGetProfile $
--             newGetProfileResponse
--
--         , responseGetRecommendations $
--             newGetRecommendationsResponse
--
--         , responseListFindingsReports $
--             newListFindingsReportsResponse
--
--         , responseListProfileTimes $
--             newListProfileTimesResponse
--
--         , responseListProfilingGroups $
--             newListProfilingGroupsResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responsePostAgentProfile $
--             newPostAgentProfileResponse
--
--         , responsePutPermission $
--             newPutPermissionResponse
--
--         , responseRemoveNotificationChannel $
--             newRemoveNotificationChannelResponse
--
--         , responseRemovePermission $
--             newRemovePermissionResponse
--
--         , responseSubmitFeedback $
--             newSubmitFeedbackResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseUpdateProfilingGroup $
--             newUpdateProfilingGroupResponse
--
--           ]
--     ]

-- Requests

requestAddNotificationChannels :: AddNotificationChannels -> TestTree
requestAddNotificationChannels =
  req
    "AddNotificationChannels"
    "fixture/AddNotificationChannels.yaml"

requestBatchGetFrameMetricData :: BatchGetFrameMetricData -> TestTree
requestBatchGetFrameMetricData =
  req
    "BatchGetFrameMetricData"
    "fixture/BatchGetFrameMetricData.yaml"

requestConfigureAgent :: ConfigureAgent -> TestTree
requestConfigureAgent =
  req
    "ConfigureAgent"
    "fixture/ConfigureAgent.yaml"

requestCreateProfilingGroup :: CreateProfilingGroup -> TestTree
requestCreateProfilingGroup =
  req
    "CreateProfilingGroup"
    "fixture/CreateProfilingGroup.yaml"

requestDeleteProfilingGroup :: DeleteProfilingGroup -> TestTree
requestDeleteProfilingGroup =
  req
    "DeleteProfilingGroup"
    "fixture/DeleteProfilingGroup.yaml"

requestDescribeProfilingGroup :: DescribeProfilingGroup -> TestTree
requestDescribeProfilingGroup =
  req
    "DescribeProfilingGroup"
    "fixture/DescribeProfilingGroup.yaml"

requestGetFindingsReportAccountSummary :: GetFindingsReportAccountSummary -> TestTree
requestGetFindingsReportAccountSummary =
  req
    "GetFindingsReportAccountSummary"
    "fixture/GetFindingsReportAccountSummary.yaml"

requestGetNotificationConfiguration :: GetNotificationConfiguration -> TestTree
requestGetNotificationConfiguration =
  req
    "GetNotificationConfiguration"
    "fixture/GetNotificationConfiguration.yaml"

requestGetPolicy :: GetPolicy -> TestTree
requestGetPolicy =
  req
    "GetPolicy"
    "fixture/GetPolicy.yaml"

requestGetProfile :: GetProfile -> TestTree
requestGetProfile =
  req
    "GetProfile"
    "fixture/GetProfile.yaml"

requestGetRecommendations :: GetRecommendations -> TestTree
requestGetRecommendations =
  req
    "GetRecommendations"
    "fixture/GetRecommendations.yaml"

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

requestListProfilingGroups :: ListProfilingGroups -> TestTree
requestListProfilingGroups =
  req
    "ListProfilingGroups"
    "fixture/ListProfilingGroups.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestPostAgentProfile :: PostAgentProfile -> TestTree
requestPostAgentProfile =
  req
    "PostAgentProfile"
    "fixture/PostAgentProfile.yaml"

requestPutPermission :: PutPermission -> TestTree
requestPutPermission =
  req
    "PutPermission"
    "fixture/PutPermission.yaml"

requestRemoveNotificationChannel :: RemoveNotificationChannel -> TestTree
requestRemoveNotificationChannel =
  req
    "RemoveNotificationChannel"
    "fixture/RemoveNotificationChannel.yaml"

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

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestUpdateProfilingGroup :: UpdateProfilingGroup -> TestTree
requestUpdateProfilingGroup =
  req
    "UpdateProfilingGroup"
    "fixture/UpdateProfilingGroup.yaml"

-- Responses

responseAddNotificationChannels :: AddNotificationChannelsResponse -> TestTree
responseAddNotificationChannels =
  res
    "AddNotificationChannelsResponse"
    "fixture/AddNotificationChannelsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AddNotificationChannels)

responseBatchGetFrameMetricData :: BatchGetFrameMetricDataResponse -> TestTree
responseBatchGetFrameMetricData =
  res
    "BatchGetFrameMetricDataResponse"
    "fixture/BatchGetFrameMetricDataResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchGetFrameMetricData)

responseConfigureAgent :: ConfigureAgentResponse -> TestTree
responseConfigureAgent =
  res
    "ConfigureAgentResponse"
    "fixture/ConfigureAgentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ConfigureAgent)

responseCreateProfilingGroup :: CreateProfilingGroupResponse -> TestTree
responseCreateProfilingGroup =
  res
    "CreateProfilingGroupResponse"
    "fixture/CreateProfilingGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateProfilingGroup)

responseDeleteProfilingGroup :: DeleteProfilingGroupResponse -> TestTree
responseDeleteProfilingGroup =
  res
    "DeleteProfilingGroupResponse"
    "fixture/DeleteProfilingGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteProfilingGroup)

responseDescribeProfilingGroup :: DescribeProfilingGroupResponse -> TestTree
responseDescribeProfilingGroup =
  res
    "DescribeProfilingGroupResponse"
    "fixture/DescribeProfilingGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeProfilingGroup)

responseGetFindingsReportAccountSummary :: GetFindingsReportAccountSummaryResponse -> TestTree
responseGetFindingsReportAccountSummary =
  res
    "GetFindingsReportAccountSummaryResponse"
    "fixture/GetFindingsReportAccountSummaryResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetFindingsReportAccountSummary)

responseGetNotificationConfiguration :: GetNotificationConfigurationResponse -> TestTree
responseGetNotificationConfiguration =
  res
    "GetNotificationConfigurationResponse"
    "fixture/GetNotificationConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetNotificationConfiguration)

responseGetPolicy :: GetPolicyResponse -> TestTree
responseGetPolicy =
  res
    "GetPolicyResponse"
    "fixture/GetPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetPolicy)

responseGetProfile :: GetProfileResponse -> TestTree
responseGetProfile =
  res
    "GetProfileResponse"
    "fixture/GetProfileResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetProfile)

responseGetRecommendations :: GetRecommendationsResponse -> TestTree
responseGetRecommendations =
  res
    "GetRecommendationsResponse"
    "fixture/GetRecommendationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetRecommendations)

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

responseListProfilingGroups :: ListProfilingGroupsResponse -> TestTree
responseListProfilingGroups =
  res
    "ListProfilingGroupsResponse"
    "fixture/ListProfilingGroupsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListProfilingGroups)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responsePostAgentProfile :: PostAgentProfileResponse -> TestTree
responsePostAgentProfile =
  res
    "PostAgentProfileResponse"
    "fixture/PostAgentProfileResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PostAgentProfile)

responsePutPermission :: PutPermissionResponse -> TestTree
responsePutPermission =
  res
    "PutPermissionResponse"
    "fixture/PutPermissionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutPermission)

responseRemoveNotificationChannel :: RemoveNotificationChannelResponse -> TestTree
responseRemoveNotificationChannel =
  res
    "RemoveNotificationChannelResponse"
    "fixture/RemoveNotificationChannelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RemoveNotificationChannel)

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

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TagResource)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UntagResource)

responseUpdateProfilingGroup :: UpdateProfilingGroupResponse -> TestTree
responseUpdateProfilingGroup =
  res
    "UpdateProfilingGroupResponse"
    "fixture/UpdateProfilingGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateProfilingGroup)
