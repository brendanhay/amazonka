{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.ConnectCampaigns
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.ConnectCampaigns where

import Amazonka.ConnectCampaigns
import qualified Data.Proxy as Proxy
import Test.Amazonka.ConnectCampaigns.Internal
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
--         [ requestCreateCampaign $
--             newCreateCampaign
--
--         , requestDeleteCampaign $
--             newDeleteCampaign
--
--         , requestDeleteConnectInstanceConfig $
--             newDeleteConnectInstanceConfig
--
--         , requestDeleteInstanceOnboardingJob $
--             newDeleteInstanceOnboardingJob
--
--         , requestDescribeCampaign $
--             newDescribeCampaign
--
--         , requestGetCampaignState $
--             newGetCampaignState
--
--         , requestGetCampaignStateBatch $
--             newGetCampaignStateBatch
--
--         , requestGetConnectInstanceConfig $
--             newGetConnectInstanceConfig
--
--         , requestGetInstanceOnboardingJobStatus $
--             newGetInstanceOnboardingJobStatus
--
--         , requestListCampaigns $
--             newListCampaigns
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestPauseCampaign $
--             newPauseCampaign
--
--         , requestPutDialRequestBatch $
--             newPutDialRequestBatch
--
--         , requestResumeCampaign $
--             newResumeCampaign
--
--         , requestStartCampaign $
--             newStartCampaign
--
--         , requestStartInstanceOnboardingJob $
--             newStartInstanceOnboardingJob
--
--         , requestStopCampaign $
--             newStopCampaign
--
--         , requestTagResource $
--             newTagResource
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestUpdateCampaignDialerConfig $
--             newUpdateCampaignDialerConfig
--
--         , requestUpdateCampaignName $
--             newUpdateCampaignName
--
--         , requestUpdateCampaignOutboundCallConfig $
--             newUpdateCampaignOutboundCallConfig
--
--           ]

--     , testGroup "response"
--         [ responseCreateCampaign $
--             newCreateCampaignResponse
--
--         , responseDeleteCampaign $
--             newDeleteCampaignResponse
--
--         , responseDeleteConnectInstanceConfig $
--             newDeleteConnectInstanceConfigResponse
--
--         , responseDeleteInstanceOnboardingJob $
--             newDeleteInstanceOnboardingJobResponse
--
--         , responseDescribeCampaign $
--             newDescribeCampaignResponse
--
--         , responseGetCampaignState $
--             newGetCampaignStateResponse
--
--         , responseGetCampaignStateBatch $
--             newGetCampaignStateBatchResponse
--
--         , responseGetConnectInstanceConfig $
--             newGetConnectInstanceConfigResponse
--
--         , responseGetInstanceOnboardingJobStatus $
--             newGetInstanceOnboardingJobStatusResponse
--
--         , responseListCampaigns $
--             newListCampaignsResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responsePauseCampaign $
--             newPauseCampaignResponse
--
--         , responsePutDialRequestBatch $
--             newPutDialRequestBatchResponse
--
--         , responseResumeCampaign $
--             newResumeCampaignResponse
--
--         , responseStartCampaign $
--             newStartCampaignResponse
--
--         , responseStartInstanceOnboardingJob $
--             newStartInstanceOnboardingJobResponse
--
--         , responseStopCampaign $
--             newStopCampaignResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseUpdateCampaignDialerConfig $
--             newUpdateCampaignDialerConfigResponse
--
--         , responseUpdateCampaignName $
--             newUpdateCampaignNameResponse
--
--         , responseUpdateCampaignOutboundCallConfig $
--             newUpdateCampaignOutboundCallConfigResponse
--
--           ]
--     ]

-- Requests

requestCreateCampaign :: CreateCampaign -> TestTree
requestCreateCampaign =
  req
    "CreateCampaign"
    "fixture/CreateCampaign.yaml"

requestDeleteCampaign :: DeleteCampaign -> TestTree
requestDeleteCampaign =
  req
    "DeleteCampaign"
    "fixture/DeleteCampaign.yaml"

requestDeleteConnectInstanceConfig :: DeleteConnectInstanceConfig -> TestTree
requestDeleteConnectInstanceConfig =
  req
    "DeleteConnectInstanceConfig"
    "fixture/DeleteConnectInstanceConfig.yaml"

requestDeleteInstanceOnboardingJob :: DeleteInstanceOnboardingJob -> TestTree
requestDeleteInstanceOnboardingJob =
  req
    "DeleteInstanceOnboardingJob"
    "fixture/DeleteInstanceOnboardingJob.yaml"

requestDescribeCampaign :: DescribeCampaign -> TestTree
requestDescribeCampaign =
  req
    "DescribeCampaign"
    "fixture/DescribeCampaign.yaml"

requestGetCampaignState :: GetCampaignState -> TestTree
requestGetCampaignState =
  req
    "GetCampaignState"
    "fixture/GetCampaignState.yaml"

requestGetCampaignStateBatch :: GetCampaignStateBatch -> TestTree
requestGetCampaignStateBatch =
  req
    "GetCampaignStateBatch"
    "fixture/GetCampaignStateBatch.yaml"

requestGetConnectInstanceConfig :: GetConnectInstanceConfig -> TestTree
requestGetConnectInstanceConfig =
  req
    "GetConnectInstanceConfig"
    "fixture/GetConnectInstanceConfig.yaml"

requestGetInstanceOnboardingJobStatus :: GetInstanceOnboardingJobStatus -> TestTree
requestGetInstanceOnboardingJobStatus =
  req
    "GetInstanceOnboardingJobStatus"
    "fixture/GetInstanceOnboardingJobStatus.yaml"

requestListCampaigns :: ListCampaigns -> TestTree
requestListCampaigns =
  req
    "ListCampaigns"
    "fixture/ListCampaigns.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestPauseCampaign :: PauseCampaign -> TestTree
requestPauseCampaign =
  req
    "PauseCampaign"
    "fixture/PauseCampaign.yaml"

requestPutDialRequestBatch :: PutDialRequestBatch -> TestTree
requestPutDialRequestBatch =
  req
    "PutDialRequestBatch"
    "fixture/PutDialRequestBatch.yaml"

requestResumeCampaign :: ResumeCampaign -> TestTree
requestResumeCampaign =
  req
    "ResumeCampaign"
    "fixture/ResumeCampaign.yaml"

requestStartCampaign :: StartCampaign -> TestTree
requestStartCampaign =
  req
    "StartCampaign"
    "fixture/StartCampaign.yaml"

requestStartInstanceOnboardingJob :: StartInstanceOnboardingJob -> TestTree
requestStartInstanceOnboardingJob =
  req
    "StartInstanceOnboardingJob"
    "fixture/StartInstanceOnboardingJob.yaml"

requestStopCampaign :: StopCampaign -> TestTree
requestStopCampaign =
  req
    "StopCampaign"
    "fixture/StopCampaign.yaml"

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

requestUpdateCampaignDialerConfig :: UpdateCampaignDialerConfig -> TestTree
requestUpdateCampaignDialerConfig =
  req
    "UpdateCampaignDialerConfig"
    "fixture/UpdateCampaignDialerConfig.yaml"

requestUpdateCampaignName :: UpdateCampaignName -> TestTree
requestUpdateCampaignName =
  req
    "UpdateCampaignName"
    "fixture/UpdateCampaignName.yaml"

requestUpdateCampaignOutboundCallConfig :: UpdateCampaignOutboundCallConfig -> TestTree
requestUpdateCampaignOutboundCallConfig =
  req
    "UpdateCampaignOutboundCallConfig"
    "fixture/UpdateCampaignOutboundCallConfig.yaml"

-- Responses

responseCreateCampaign :: CreateCampaignResponse -> TestTree
responseCreateCampaign =
  res
    "CreateCampaignResponse"
    "fixture/CreateCampaignResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateCampaign)

responseDeleteCampaign :: DeleteCampaignResponse -> TestTree
responseDeleteCampaign =
  res
    "DeleteCampaignResponse"
    "fixture/DeleteCampaignResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteCampaign)

responseDeleteConnectInstanceConfig :: DeleteConnectInstanceConfigResponse -> TestTree
responseDeleteConnectInstanceConfig =
  res
    "DeleteConnectInstanceConfigResponse"
    "fixture/DeleteConnectInstanceConfigResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteConnectInstanceConfig)

responseDeleteInstanceOnboardingJob :: DeleteInstanceOnboardingJobResponse -> TestTree
responseDeleteInstanceOnboardingJob =
  res
    "DeleteInstanceOnboardingJobResponse"
    "fixture/DeleteInstanceOnboardingJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteInstanceOnboardingJob)

responseDescribeCampaign :: DescribeCampaignResponse -> TestTree
responseDescribeCampaign =
  res
    "DescribeCampaignResponse"
    "fixture/DescribeCampaignResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeCampaign)

responseGetCampaignState :: GetCampaignStateResponse -> TestTree
responseGetCampaignState =
  res
    "GetCampaignStateResponse"
    "fixture/GetCampaignStateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetCampaignState)

responseGetCampaignStateBatch :: GetCampaignStateBatchResponse -> TestTree
responseGetCampaignStateBatch =
  res
    "GetCampaignStateBatchResponse"
    "fixture/GetCampaignStateBatchResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetCampaignStateBatch)

responseGetConnectInstanceConfig :: GetConnectInstanceConfigResponse -> TestTree
responseGetConnectInstanceConfig =
  res
    "GetConnectInstanceConfigResponse"
    "fixture/GetConnectInstanceConfigResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetConnectInstanceConfig)

responseGetInstanceOnboardingJobStatus :: GetInstanceOnboardingJobStatusResponse -> TestTree
responseGetInstanceOnboardingJobStatus =
  res
    "GetInstanceOnboardingJobStatusResponse"
    "fixture/GetInstanceOnboardingJobStatusResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetInstanceOnboardingJobStatus)

responseListCampaigns :: ListCampaignsResponse -> TestTree
responseListCampaigns =
  res
    "ListCampaignsResponse"
    "fixture/ListCampaignsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListCampaigns)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responsePauseCampaign :: PauseCampaignResponse -> TestTree
responsePauseCampaign =
  res
    "PauseCampaignResponse"
    "fixture/PauseCampaignResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PauseCampaign)

responsePutDialRequestBatch :: PutDialRequestBatchResponse -> TestTree
responsePutDialRequestBatch =
  res
    "PutDialRequestBatchResponse"
    "fixture/PutDialRequestBatchResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutDialRequestBatch)

responseResumeCampaign :: ResumeCampaignResponse -> TestTree
responseResumeCampaign =
  res
    "ResumeCampaignResponse"
    "fixture/ResumeCampaignResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ResumeCampaign)

responseStartCampaign :: StartCampaignResponse -> TestTree
responseStartCampaign =
  res
    "StartCampaignResponse"
    "fixture/StartCampaignResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartCampaign)

responseStartInstanceOnboardingJob :: StartInstanceOnboardingJobResponse -> TestTree
responseStartInstanceOnboardingJob =
  res
    "StartInstanceOnboardingJobResponse"
    "fixture/StartInstanceOnboardingJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartInstanceOnboardingJob)

responseStopCampaign :: StopCampaignResponse -> TestTree
responseStopCampaign =
  res
    "StopCampaignResponse"
    "fixture/StopCampaignResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StopCampaign)

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

responseUpdateCampaignDialerConfig :: UpdateCampaignDialerConfigResponse -> TestTree
responseUpdateCampaignDialerConfig =
  res
    "UpdateCampaignDialerConfigResponse"
    "fixture/UpdateCampaignDialerConfigResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateCampaignDialerConfig)

responseUpdateCampaignName :: UpdateCampaignNameResponse -> TestTree
responseUpdateCampaignName =
  res
    "UpdateCampaignNameResponse"
    "fixture/UpdateCampaignNameResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateCampaignName)

responseUpdateCampaignOutboundCallConfig :: UpdateCampaignOutboundCallConfigResponse -> TestTree
responseUpdateCampaignOutboundCallConfig =
  res
    "UpdateCampaignOutboundCallConfigResponse"
    "fixture/UpdateCampaignOutboundCallConfigResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateCampaignOutboundCallConfig)
