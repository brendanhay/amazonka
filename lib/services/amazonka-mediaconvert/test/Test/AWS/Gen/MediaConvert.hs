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

import Amazonka.MediaConvert
import qualified Data.Proxy as Proxy
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
--         [ requestDeletePreset $
--             newDeletePreset
--
--         , requestUpdatePreset $
--             newUpdatePreset
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestListQueues $
--             newListQueues
--
--         , requestDeleteQueue $
--             newDeleteQueue
--
--         , requestUpdateQueue $
--             newUpdateQueue
--
--         , requestGetPreset $
--             newGetPreset
--
--         , requestCreateJob $
--             newCreateJob
--
--         , requestListJobs $
--             newListJobs
--
--         , requestPutPolicy $
--             newPutPolicy
--
--         , requestGetJob $
--             newGetJob
--
--         , requestDeletePolicy $
--             newDeletePolicy
--
--         , requestCreatePreset $
--             newCreatePreset
--
--         , requestListPresets $
--             newListPresets
--
--         , requestDisassociateCertificate $
--             newDisassociateCertificate
--
--         , requestGetQueue $
--             newGetQueue
--
--         , requestDescribeEndpoints $
--             newDescribeEndpoints
--
--         , requestCreateQueue $
--             newCreateQueue
--
--         , requestTagResource $
--             newTagResource
--
--         , requestCreateJobTemplate $
--             newCreateJobTemplate
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestDeleteJobTemplate $
--             newDeleteJobTemplate
--
--         , requestUpdateJobTemplate $
--             newUpdateJobTemplate
--
--         , requestGetPolicy $
--             newGetPolicy
--
--         , requestListJobTemplates $
--             newListJobTemplates
--
--         , requestGetJobTemplate $
--             newGetJobTemplate
--
--         , requestAssociateCertificate $
--             newAssociateCertificate
--
--         , requestCancelJob $
--             newCancelJob
--
--           ]

--     , testGroup "response"
--         [ responseDeletePreset $
--             newDeletePresetResponse
--
--         , responseUpdatePreset $
--             newUpdatePresetResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseListQueues $
--             newListQueuesResponse
--
--         , responseDeleteQueue $
--             newDeleteQueueResponse
--
--         , responseUpdateQueue $
--             newUpdateQueueResponse
--
--         , responseGetPreset $
--             newGetPresetResponse
--
--         , responseCreateJob $
--             newCreateJobResponse
--
--         , responseListJobs $
--             newListJobsResponse
--
--         , responsePutPolicy $
--             newPutPolicyResponse
--
--         , responseGetJob $
--             newGetJobResponse
--
--         , responseDeletePolicy $
--             newDeletePolicyResponse
--
--         , responseCreatePreset $
--             newCreatePresetResponse
--
--         , responseListPresets $
--             newListPresetsResponse
--
--         , responseDisassociateCertificate $
--             newDisassociateCertificateResponse
--
--         , responseGetQueue $
--             newGetQueueResponse
--
--         , responseDescribeEndpoints $
--             newDescribeEndpointsResponse
--
--         , responseCreateQueue $
--             newCreateQueueResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseCreateJobTemplate $
--             newCreateJobTemplateResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseDeleteJobTemplate $
--             newDeleteJobTemplateResponse
--
--         , responseUpdateJobTemplate $
--             newUpdateJobTemplateResponse
--
--         , responseGetPolicy $
--             newGetPolicyResponse
--
--         , responseListJobTemplates $
--             newListJobTemplatesResponse
--
--         , responseGetJobTemplate $
--             newGetJobTemplateResponse
--
--         , responseAssociateCertificate $
--             newAssociateCertificateResponse
--
--         , responseCancelJob $
--             newCancelJobResponse
--
--           ]
--     ]

-- Requests

requestDeletePreset :: DeletePreset -> TestTree
requestDeletePreset =
  req
    "DeletePreset"
    "fixture/DeletePreset.yaml"

requestUpdatePreset :: UpdatePreset -> TestTree
requestUpdatePreset =
  req
    "UpdatePreset"
    "fixture/UpdatePreset.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestListQueues :: ListQueues -> TestTree
requestListQueues =
  req
    "ListQueues"
    "fixture/ListQueues.yaml"

requestDeleteQueue :: DeleteQueue -> TestTree
requestDeleteQueue =
  req
    "DeleteQueue"
    "fixture/DeleteQueue.yaml"

requestUpdateQueue :: UpdateQueue -> TestTree
requestUpdateQueue =
  req
    "UpdateQueue"
    "fixture/UpdateQueue.yaml"

requestGetPreset :: GetPreset -> TestTree
requestGetPreset =
  req
    "GetPreset"
    "fixture/GetPreset.yaml"

requestCreateJob :: CreateJob -> TestTree
requestCreateJob =
  req
    "CreateJob"
    "fixture/CreateJob.yaml"

requestListJobs :: ListJobs -> TestTree
requestListJobs =
  req
    "ListJobs"
    "fixture/ListJobs.yaml"

requestPutPolicy :: PutPolicy -> TestTree
requestPutPolicy =
  req
    "PutPolicy"
    "fixture/PutPolicy.yaml"

requestGetJob :: GetJob -> TestTree
requestGetJob =
  req
    "GetJob"
    "fixture/GetJob.yaml"

requestDeletePolicy :: DeletePolicy -> TestTree
requestDeletePolicy =
  req
    "DeletePolicy"
    "fixture/DeletePolicy.yaml"

requestCreatePreset :: CreatePreset -> TestTree
requestCreatePreset =
  req
    "CreatePreset"
    "fixture/CreatePreset.yaml"

requestListPresets :: ListPresets -> TestTree
requestListPresets =
  req
    "ListPresets"
    "fixture/ListPresets.yaml"

requestDisassociateCertificate :: DisassociateCertificate -> TestTree
requestDisassociateCertificate =
  req
    "DisassociateCertificate"
    "fixture/DisassociateCertificate.yaml"

requestGetQueue :: GetQueue -> TestTree
requestGetQueue =
  req
    "GetQueue"
    "fixture/GetQueue.yaml"

requestDescribeEndpoints :: DescribeEndpoints -> TestTree
requestDescribeEndpoints =
  req
    "DescribeEndpoints"
    "fixture/DescribeEndpoints.yaml"

requestCreateQueue :: CreateQueue -> TestTree
requestCreateQueue =
  req
    "CreateQueue"
    "fixture/CreateQueue.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

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

requestDeleteJobTemplate :: DeleteJobTemplate -> TestTree
requestDeleteJobTemplate =
  req
    "DeleteJobTemplate"
    "fixture/DeleteJobTemplate.yaml"

requestUpdateJobTemplate :: UpdateJobTemplate -> TestTree
requestUpdateJobTemplate =
  req
    "UpdateJobTemplate"
    "fixture/UpdateJobTemplate.yaml"

requestGetPolicy :: GetPolicy -> TestTree
requestGetPolicy =
  req
    "GetPolicy"
    "fixture/GetPolicy.yaml"

requestListJobTemplates :: ListJobTemplates -> TestTree
requestListJobTemplates =
  req
    "ListJobTemplates"
    "fixture/ListJobTemplates.yaml"

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

requestCancelJob :: CancelJob -> TestTree
requestCancelJob =
  req
    "CancelJob"
    "fixture/CancelJob.yaml"

-- Responses

responseDeletePreset :: DeletePresetResponse -> TestTree
responseDeletePreset =
  res
    "DeletePresetResponse"
    "fixture/DeletePresetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeletePreset)

responseUpdatePreset :: UpdatePresetResponse -> TestTree
responseUpdatePreset =
  res
    "UpdatePresetResponse"
    "fixture/UpdatePresetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdatePreset)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responseListQueues :: ListQueuesResponse -> TestTree
responseListQueues =
  res
    "ListQueuesResponse"
    "fixture/ListQueuesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListQueues)

responseDeleteQueue :: DeleteQueueResponse -> TestTree
responseDeleteQueue =
  res
    "DeleteQueueResponse"
    "fixture/DeleteQueueResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteQueue)

responseUpdateQueue :: UpdateQueueResponse -> TestTree
responseUpdateQueue =
  res
    "UpdateQueueResponse"
    "fixture/UpdateQueueResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateQueue)

responseGetPreset :: GetPresetResponse -> TestTree
responseGetPreset =
  res
    "GetPresetResponse"
    "fixture/GetPresetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetPreset)

responseCreateJob :: CreateJobResponse -> TestTree
responseCreateJob =
  res
    "CreateJobResponse"
    "fixture/CreateJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateJob)

responseListJobs :: ListJobsResponse -> TestTree
responseListJobs =
  res
    "ListJobsResponse"
    "fixture/ListJobsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListJobs)

responsePutPolicy :: PutPolicyResponse -> TestTree
responsePutPolicy =
  res
    "PutPolicyResponse"
    "fixture/PutPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutPolicy)

responseGetJob :: GetJobResponse -> TestTree
responseGetJob =
  res
    "GetJobResponse"
    "fixture/GetJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetJob)

responseDeletePolicy :: DeletePolicyResponse -> TestTree
responseDeletePolicy =
  res
    "DeletePolicyResponse"
    "fixture/DeletePolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeletePolicy)

responseCreatePreset :: CreatePresetResponse -> TestTree
responseCreatePreset =
  res
    "CreatePresetResponse"
    "fixture/CreatePresetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreatePreset)

responseListPresets :: ListPresetsResponse -> TestTree
responseListPresets =
  res
    "ListPresetsResponse"
    "fixture/ListPresetsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListPresets)

responseDisassociateCertificate :: DisassociateCertificateResponse -> TestTree
responseDisassociateCertificate =
  res
    "DisassociateCertificateResponse"
    "fixture/DisassociateCertificateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisassociateCertificate)

responseGetQueue :: GetQueueResponse -> TestTree
responseGetQueue =
  res
    "GetQueueResponse"
    "fixture/GetQueueResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetQueue)

responseDescribeEndpoints :: DescribeEndpointsResponse -> TestTree
responseDescribeEndpoints =
  res
    "DescribeEndpointsResponse"
    "fixture/DescribeEndpointsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeEndpoints)

responseCreateQueue :: CreateQueueResponse -> TestTree
responseCreateQueue =
  res
    "CreateQueueResponse"
    "fixture/CreateQueueResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateQueue)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TagResource)

responseCreateJobTemplate :: CreateJobTemplateResponse -> TestTree
responseCreateJobTemplate =
  res
    "CreateJobTemplateResponse"
    "fixture/CreateJobTemplateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateJobTemplate)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UntagResource)

responseDeleteJobTemplate :: DeleteJobTemplateResponse -> TestTree
responseDeleteJobTemplate =
  res
    "DeleteJobTemplateResponse"
    "fixture/DeleteJobTemplateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteJobTemplate)

responseUpdateJobTemplate :: UpdateJobTemplateResponse -> TestTree
responseUpdateJobTemplate =
  res
    "UpdateJobTemplateResponse"
    "fixture/UpdateJobTemplateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateJobTemplate)

responseGetPolicy :: GetPolicyResponse -> TestTree
responseGetPolicy =
  res
    "GetPolicyResponse"
    "fixture/GetPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetPolicy)

responseListJobTemplates :: ListJobTemplatesResponse -> TestTree
responseListJobTemplates =
  res
    "ListJobTemplatesResponse"
    "fixture/ListJobTemplatesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListJobTemplates)

responseGetJobTemplate :: GetJobTemplateResponse -> TestTree
responseGetJobTemplate =
  res
    "GetJobTemplateResponse"
    "fixture/GetJobTemplateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetJobTemplate)

responseAssociateCertificate :: AssociateCertificateResponse -> TestTree
responseAssociateCertificate =
  res
    "AssociateCertificateResponse"
    "fixture/AssociateCertificateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssociateCertificate)

responseCancelJob :: CancelJobResponse -> TestTree
responseCancelJob =
  res
    "CancelJobResponse"
    "fixture/CancelJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CancelJob)
