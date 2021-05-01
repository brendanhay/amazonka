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
--         , requestListPresets $
--             newListPresets
--
--         , requestDeletePreset $
--             newDeletePreset
--
--         , requestCancelJob $
--             newCancelJob
--
--         , requestCreatePreset $
--             newCreatePreset
--
--         , requestGetJob $
--             newGetJob
--
--         , requestListJobTemplates $
--             newListJobTemplates
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestCreateJobTemplate $
--             newCreateJobTemplate
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
--         , requestGetJobTemplate $
--             newGetJobTemplate
--
--         , requestAssociateCertificate $
--             newAssociateCertificate
--
--         , requestListJobs $
--             newListJobs
--
--         , requestUpdateJobTemplate $
--             newUpdateJobTemplate
--
--         , requestDeleteJobTemplate $
--             newDeleteJobTemplate
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
--         , responseListPresets $
--             newListPresetsResponse
--
--         , responseDeletePreset $
--             newDeletePresetResponse
--
--         , responseCancelJob $
--             newCancelJobResponse
--
--         , responseCreatePreset $
--             newCreatePresetResponse
--
--         , responseGetJob $
--             newGetJobResponse
--
--         , responseListJobTemplates $
--             newListJobTemplatesResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseCreateJobTemplate $
--             newCreateJobTemplateResponse
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
--         , responseGetJobTemplate $
--             newGetJobTemplateResponse
--
--         , responseAssociateCertificate $
--             newAssociateCertificateResponse
--
--         , responseListJobs $
--             newListJobsResponse
--
--         , responseUpdateJobTemplate $
--             newUpdateJobTemplateResponse
--
--         , responseDeleteJobTemplate $
--             newDeleteJobTemplateResponse
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

requestListPresets :: ListPresets -> TestTree
requestListPresets =
  req
    "ListPresets"
    "fixture/ListPresets.yaml"

requestDeletePreset :: DeletePreset -> TestTree
requestDeletePreset =
  req
    "DeletePreset"
    "fixture/DeletePreset.yaml"

requestCancelJob :: CancelJob -> TestTree
requestCancelJob =
  req
    "CancelJob"
    "fixture/CancelJob.yaml"

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

requestListJobTemplates :: ListJobTemplates -> TestTree
requestListJobTemplates =
  req
    "ListJobTemplates"
    "fixture/ListJobTemplates.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestCreateJobTemplate :: CreateJobTemplate -> TestTree
requestCreateJobTemplate =
  req
    "CreateJobTemplate"
    "fixture/CreateJobTemplate.yaml"

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

requestGetJobTemplate :: GetJobTemplate -> TestTree
requestGetJobTemplate =
  req
    "GetJobTemplate"
    "fixture/GetJobTemplate.yaml"

requestAssociateCertificate :: AssociateCertificate -> TestTree
requestAssociateCertificate =
  req
    "AssociateCertificate"
    "fixture/AssociateCertificate.yaml"

requestListJobs :: ListJobs -> TestTree
requestListJobs =
  req
    "ListJobs"
    "fixture/ListJobs.yaml"

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

responseListPresets :: ListPresetsResponse -> TestTree
responseListPresets =
  res
    "ListPresetsResponse"
    "fixture/ListPresetsResponse.proto"
    defaultService
    (Proxy :: Proxy ListPresets)

responseDeletePreset :: DeletePresetResponse -> TestTree
responseDeletePreset =
  res
    "DeletePresetResponse"
    "fixture/DeletePresetResponse.proto"
    defaultService
    (Proxy :: Proxy DeletePreset)

responseCancelJob :: CancelJobResponse -> TestTree
responseCancelJob =
  res
    "CancelJobResponse"
    "fixture/CancelJobResponse.proto"
    defaultService
    (Proxy :: Proxy CancelJob)

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

responseListJobTemplates :: ListJobTemplatesResponse -> TestTree
responseListJobTemplates =
  res
    "ListJobTemplatesResponse"
    "fixture/ListJobTemplatesResponse.proto"
    defaultService
    (Proxy :: Proxy ListJobTemplates)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy :: Proxy UntagResource)

responseCreateJobTemplate :: CreateJobTemplateResponse -> TestTree
responseCreateJobTemplate =
  res
    "CreateJobTemplateResponse"
    "fixture/CreateJobTemplateResponse.proto"
    defaultService
    (Proxy :: Proxy CreateJobTemplate)

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

responseGetJobTemplate :: GetJobTemplateResponse -> TestTree
responseGetJobTemplate =
  res
    "GetJobTemplateResponse"
    "fixture/GetJobTemplateResponse.proto"
    defaultService
    (Proxy :: Proxy GetJobTemplate)

responseAssociateCertificate :: AssociateCertificateResponse -> TestTree
responseAssociateCertificate =
  res
    "AssociateCertificateResponse"
    "fixture/AssociateCertificateResponse.proto"
    defaultService
    (Proxy :: Proxy AssociateCertificate)

responseListJobs :: ListJobsResponse -> TestTree
responseListJobs =
  res
    "ListJobsResponse"
    "fixture/ListJobsResponse.proto"
    defaultService
    (Proxy :: Proxy ListJobs)

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
