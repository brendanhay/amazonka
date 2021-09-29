{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.MediaConvert
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.AWS.Gen.MediaConvert where

import Data.Proxy
import Network.AWS.MediaConvert
import Test.AWS.Fixture
import Test.AWS.MediaConvert.Internal
import Test.AWS.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestDisassociateCertificate $
--             newDisassociateCertificate
--
--         , requestUpdatePreset $
--             newUpdatePreset
--
--         , requestDeletePreset $
--             newDeletePreset
--
--         , requestListPresets $
--             newListPresets
--
--         , requestCreatePreset $
--             newCreatePreset
--
--         , requestGetJob $
--             newGetJob
--
--         , requestCancelJob $
--             newCancelJob
--
--         , requestListJobTemplates $
--             newListJobTemplates
--
--         , requestCreateJobTemplate $
--             newCreateJobTemplate
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestListQueues $
--             newListQueues
--
--         , requestTagResource $
--             newTagResource
--
--         , requestGetQueue $
--             newGetQueue
--
--         , requestAssociateCertificate $
--             newAssociateCertificate
--
--         , requestGetJobTemplate $
--             newGetJobTemplate
--
--         , requestUpdateJobTemplate $
--             newUpdateJobTemplate
--
--         , requestDeleteJobTemplate $
--             newDeleteJobTemplate
--
--         , requestListJobs $
--             newListJobs
--
--         , requestCreateJob $
--             newCreateJob
--
--         , requestGetPreset $
--             newGetPreset
--
--         , requestUpdateQueue $
--             newUpdateQueue
--
--         , requestDeleteQueue $
--             newDeleteQueue
--
--         , requestCreateQueue $
--             newCreateQueue
--
--         , requestDescribeEndpoints $
--             newDescribeEndpoints
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--           ]

--     , testGroup "response"
--         [ responseDisassociateCertificate $
--             newDisassociateCertificateResponse
--
--         , responseUpdatePreset $
--             newUpdatePresetResponse
--
--         , responseDeletePreset $
--             newDeletePresetResponse
--
--         , responseListPresets $
--             newListPresetsResponse
--
--         , responseCreatePreset $
--             newCreatePresetResponse
--
--         , responseGetJob $
--             newGetJobResponse
--
--         , responseCancelJob $
--             newCancelJobResponse
--
--         , responseListJobTemplates $
--             newListJobTemplatesResponse
--
--         , responseCreateJobTemplate $
--             newCreateJobTemplateResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseListQueues $
--             newListQueuesResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseGetQueue $
--             newGetQueueResponse
--
--         , responseAssociateCertificate $
--             newAssociateCertificateResponse
--
--         , responseGetJobTemplate $
--             newGetJobTemplateResponse
--
--         , responseUpdateJobTemplate $
--             newUpdateJobTemplateResponse
--
--         , responseDeleteJobTemplate $
--             newDeleteJobTemplateResponse
--
--         , responseListJobs $
--             newListJobsResponse
--
--         , responseCreateJob $
--             newCreateJobResponse
--
--         , responseGetPreset $
--             newGetPresetResponse
--
--         , responseUpdateQueue $
--             newUpdateQueueResponse
--
--         , responseDeleteQueue $
--             newDeleteQueueResponse
--
--         , responseCreateQueue $
--             newCreateQueueResponse
--
--         , responseDescribeEndpoints $
--             newDescribeEndpointsResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--           ]
--     ]

-- Requests

requestDisassociateCertificate :: DisassociateCertificate -> TestTree
requestDisassociateCertificate =
  req
    "DisassociateCertificate"
    "fixture/DisassociateCertificate.yaml"

requestUpdatePreset :: UpdatePreset -> TestTree
requestUpdatePreset =
  req
    "UpdatePreset"
    "fixture/UpdatePreset.yaml"

requestDeletePreset :: DeletePreset -> TestTree
requestDeletePreset =
  req
    "DeletePreset"
    "fixture/DeletePreset.yaml"

requestListPresets :: ListPresets -> TestTree
requestListPresets =
  req
    "ListPresets"
    "fixture/ListPresets.yaml"

requestCreatePreset :: CreatePreset -> TestTree
requestCreatePreset =
  req
    "CreatePreset"
    "fixture/CreatePreset.yaml"

requestGetJob :: GetJob -> TestTree
requestGetJob =
  req
    "GetJob"
    "fixture/GetJob.yaml"

requestCancelJob :: CancelJob -> TestTree
requestCancelJob =
  req
    "CancelJob"
    "fixture/CancelJob.yaml"

requestListJobTemplates :: ListJobTemplates -> TestTree
requestListJobTemplates =
  req
    "ListJobTemplates"
    "fixture/ListJobTemplates.yaml"

requestCreateJobTemplate :: CreateJobTemplate -> TestTree
requestCreateJobTemplate =
  req
    "CreateJobTemplate"
    "fixture/CreateJobTemplate.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestListQueues :: ListQueues -> TestTree
requestListQueues =
  req
    "ListQueues"
    "fixture/ListQueues.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestGetQueue :: GetQueue -> TestTree
requestGetQueue =
  req
    "GetQueue"
    "fixture/GetQueue.yaml"

requestAssociateCertificate :: AssociateCertificate -> TestTree
requestAssociateCertificate =
  req
    "AssociateCertificate"
    "fixture/AssociateCertificate.yaml"

requestGetJobTemplate :: GetJobTemplate -> TestTree
requestGetJobTemplate =
  req
    "GetJobTemplate"
    "fixture/GetJobTemplate.yaml"

requestUpdateJobTemplate :: UpdateJobTemplate -> TestTree
requestUpdateJobTemplate =
  req
    "UpdateJobTemplate"
    "fixture/UpdateJobTemplate.yaml"

requestDeleteJobTemplate :: DeleteJobTemplate -> TestTree
requestDeleteJobTemplate =
  req
    "DeleteJobTemplate"
    "fixture/DeleteJobTemplate.yaml"

requestListJobs :: ListJobs -> TestTree
requestListJobs =
  req
    "ListJobs"
    "fixture/ListJobs.yaml"

requestCreateJob :: CreateJob -> TestTree
requestCreateJob =
  req
    "CreateJob"
    "fixture/CreateJob.yaml"

requestGetPreset :: GetPreset -> TestTree
requestGetPreset =
  req
    "GetPreset"
    "fixture/GetPreset.yaml"

requestUpdateQueue :: UpdateQueue -> TestTree
requestUpdateQueue =
  req
    "UpdateQueue"
    "fixture/UpdateQueue.yaml"

requestDeleteQueue :: DeleteQueue -> TestTree
requestDeleteQueue =
  req
    "DeleteQueue"
    "fixture/DeleteQueue.yaml"

requestCreateQueue :: CreateQueue -> TestTree
requestCreateQueue =
  req
    "CreateQueue"
    "fixture/CreateQueue.yaml"

requestDescribeEndpoints :: DescribeEndpoints -> TestTree
requestDescribeEndpoints =
  req
    "DescribeEndpoints"
    "fixture/DescribeEndpoints.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

-- Responses

responseDisassociateCertificate :: DisassociateCertificateResponse -> TestTree
responseDisassociateCertificate =
  res
    "DisassociateCertificateResponse"
    "fixture/DisassociateCertificateResponse.proto"
    defaultService
    (Proxy :: Proxy DisassociateCertificate)

responseUpdatePreset :: UpdatePresetResponse -> TestTree
responseUpdatePreset =
  res
    "UpdatePresetResponse"
    "fixture/UpdatePresetResponse.proto"
    defaultService
    (Proxy :: Proxy UpdatePreset)

responseDeletePreset :: DeletePresetResponse -> TestTree
responseDeletePreset =
  res
    "DeletePresetResponse"
    "fixture/DeletePresetResponse.proto"
    defaultService
    (Proxy :: Proxy DeletePreset)

responseListPresets :: ListPresetsResponse -> TestTree
responseListPresets =
  res
    "ListPresetsResponse"
    "fixture/ListPresetsResponse.proto"
    defaultService
    (Proxy :: Proxy ListPresets)

responseCreatePreset :: CreatePresetResponse -> TestTree
responseCreatePreset =
  res
    "CreatePresetResponse"
    "fixture/CreatePresetResponse.proto"
    defaultService
    (Proxy :: Proxy CreatePreset)

responseGetJob :: GetJobResponse -> TestTree
responseGetJob =
  res
    "GetJobResponse"
    "fixture/GetJobResponse.proto"
    defaultService
    (Proxy :: Proxy GetJob)

responseCancelJob :: CancelJobResponse -> TestTree
responseCancelJob =
  res
    "CancelJobResponse"
    "fixture/CancelJobResponse.proto"
    defaultService
    (Proxy :: Proxy CancelJob)

responseListJobTemplates :: ListJobTemplatesResponse -> TestTree
responseListJobTemplates =
  res
    "ListJobTemplatesResponse"
    "fixture/ListJobTemplatesResponse.proto"
    defaultService
    (Proxy :: Proxy ListJobTemplates)

responseCreateJobTemplate :: CreateJobTemplateResponse -> TestTree
responseCreateJobTemplate =
  res
    "CreateJobTemplateResponse"
    "fixture/CreateJobTemplateResponse.proto"
    defaultService
    (Proxy :: Proxy CreateJobTemplate)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy :: Proxy UntagResource)

responseListQueues :: ListQueuesResponse -> TestTree
responseListQueues =
  res
    "ListQueuesResponse"
    "fixture/ListQueuesResponse.proto"
    defaultService
    (Proxy :: Proxy ListQueues)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy :: Proxy TagResource)

responseGetQueue :: GetQueueResponse -> TestTree
responseGetQueue =
  res
    "GetQueueResponse"
    "fixture/GetQueueResponse.proto"
    defaultService
    (Proxy :: Proxy GetQueue)

responseAssociateCertificate :: AssociateCertificateResponse -> TestTree
responseAssociateCertificate =
  res
    "AssociateCertificateResponse"
    "fixture/AssociateCertificateResponse.proto"
    defaultService
    (Proxy :: Proxy AssociateCertificate)

responseGetJobTemplate :: GetJobTemplateResponse -> TestTree
responseGetJobTemplate =
  res
    "GetJobTemplateResponse"
    "fixture/GetJobTemplateResponse.proto"
    defaultService
    (Proxy :: Proxy GetJobTemplate)

responseUpdateJobTemplate :: UpdateJobTemplateResponse -> TestTree
responseUpdateJobTemplate =
  res
    "UpdateJobTemplateResponse"
    "fixture/UpdateJobTemplateResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateJobTemplate)

responseDeleteJobTemplate :: DeleteJobTemplateResponse -> TestTree
responseDeleteJobTemplate =
  res
    "DeleteJobTemplateResponse"
    "fixture/DeleteJobTemplateResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteJobTemplate)

responseListJobs :: ListJobsResponse -> TestTree
responseListJobs =
  res
    "ListJobsResponse"
    "fixture/ListJobsResponse.proto"
    defaultService
    (Proxy :: Proxy ListJobs)

responseCreateJob :: CreateJobResponse -> TestTree
responseCreateJob =
  res
    "CreateJobResponse"
    "fixture/CreateJobResponse.proto"
    defaultService
    (Proxy :: Proxy CreateJob)

responseGetPreset :: GetPresetResponse -> TestTree
responseGetPreset =
  res
    "GetPresetResponse"
    "fixture/GetPresetResponse.proto"
    defaultService
    (Proxy :: Proxy GetPreset)

responseUpdateQueue :: UpdateQueueResponse -> TestTree
responseUpdateQueue =
  res
    "UpdateQueueResponse"
    "fixture/UpdateQueueResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateQueue)

responseDeleteQueue :: DeleteQueueResponse -> TestTree
responseDeleteQueue =
  res
    "DeleteQueueResponse"
    "fixture/DeleteQueueResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteQueue)

responseCreateQueue :: CreateQueueResponse -> TestTree
responseCreateQueue =
  res
    "CreateQueueResponse"
    "fixture/CreateQueueResponse.proto"
    defaultService
    (Proxy :: Proxy CreateQueue)

responseDescribeEndpoints :: DescribeEndpointsResponse -> TestTree
responseDescribeEndpoints =
  res
    "DescribeEndpointsResponse"
    "fixture/DescribeEndpointsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeEndpoints)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy :: Proxy ListTagsForResource)
