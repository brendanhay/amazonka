{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.MediaConvert
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.MediaConvert where

import Amazonka.MediaConvert
import qualified Data.Proxy as Proxy
import Test.Amazonka.Fixture
import Test.Amazonka.MediaConvert.Internal
import Test.Amazonka.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestAssociateCertificate $
--             newAssociateCertificate
--
--         , requestCancelJob $
--             newCancelJob
--
--         , requestCreateJob $
--             newCreateJob
--
--         , requestCreateJobTemplate $
--             newCreateJobTemplate
--
--         , requestCreatePreset $
--             newCreatePreset
--
--         , requestCreateQueue $
--             newCreateQueue
--
--         , requestDeleteJobTemplate $
--             newDeleteJobTemplate
--
--         , requestDeletePolicy $
--             newDeletePolicy
--
--         , requestDeletePreset $
--             newDeletePreset
--
--         , requestDeleteQueue $
--             newDeleteQueue
--
--         , requestDescribeEndpoints $
--             newDescribeEndpoints
--
--         , requestDisassociateCertificate $
--             newDisassociateCertificate
--
--         , requestGetJob $
--             newGetJob
--
--         , requestGetJobTemplate $
--             newGetJobTemplate
--
--         , requestGetPolicy $
--             newGetPolicy
--
--         , requestGetPreset $
--             newGetPreset
--
--         , requestGetQueue $
--             newGetQueue
--
--         , requestListJobTemplates $
--             newListJobTemplates
--
--         , requestListJobs $
--             newListJobs
--
--         , requestListPresets $
--             newListPresets
--
--         , requestListQueues $
--             newListQueues
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestPutPolicy $
--             newPutPolicy
--
--         , requestTagResource $
--             newTagResource
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestUpdateJobTemplate $
--             newUpdateJobTemplate
--
--         , requestUpdatePreset $
--             newUpdatePreset
--
--         , requestUpdateQueue $
--             newUpdateQueue
--
--           ]

--     , testGroup "response"
--         [ responseAssociateCertificate $
--             newAssociateCertificateResponse
--
--         , responseCancelJob $
--             newCancelJobResponse
--
--         , responseCreateJob $
--             newCreateJobResponse
--
--         , responseCreateJobTemplate $
--             newCreateJobTemplateResponse
--
--         , responseCreatePreset $
--             newCreatePresetResponse
--
--         , responseCreateQueue $
--             newCreateQueueResponse
--
--         , responseDeleteJobTemplate $
--             newDeleteJobTemplateResponse
--
--         , responseDeletePolicy $
--             newDeletePolicyResponse
--
--         , responseDeletePreset $
--             newDeletePresetResponse
--
--         , responseDeleteQueue $
--             newDeleteQueueResponse
--
--         , responseDescribeEndpoints $
--             newDescribeEndpointsResponse
--
--         , responseDisassociateCertificate $
--             newDisassociateCertificateResponse
--
--         , responseGetJob $
--             newGetJobResponse
--
--         , responseGetJobTemplate $
--             newGetJobTemplateResponse
--
--         , responseGetPolicy $
--             newGetPolicyResponse
--
--         , responseGetPreset $
--             newGetPresetResponse
--
--         , responseGetQueue $
--             newGetQueueResponse
--
--         , responseListJobTemplates $
--             newListJobTemplatesResponse
--
--         , responseListJobs $
--             newListJobsResponse
--
--         , responseListPresets $
--             newListPresetsResponse
--
--         , responseListQueues $
--             newListQueuesResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responsePutPolicy $
--             newPutPolicyResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseUpdateJobTemplate $
--             newUpdateJobTemplateResponse
--
--         , responseUpdatePreset $
--             newUpdatePresetResponse
--
--         , responseUpdateQueue $
--             newUpdateQueueResponse
--
--           ]
--     ]

-- Requests

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

requestCreateJob :: CreateJob -> TestTree
requestCreateJob =
  req
    "CreateJob"
    "fixture/CreateJob.yaml"

requestCreateJobTemplate :: CreateJobTemplate -> TestTree
requestCreateJobTemplate =
  req
    "CreateJobTemplate"
    "fixture/CreateJobTemplate.yaml"

requestCreatePreset :: CreatePreset -> TestTree
requestCreatePreset =
  req
    "CreatePreset"
    "fixture/CreatePreset.yaml"

requestCreateQueue :: CreateQueue -> TestTree
requestCreateQueue =
  req
    "CreateQueue"
    "fixture/CreateQueue.yaml"

requestDeleteJobTemplate :: DeleteJobTemplate -> TestTree
requestDeleteJobTemplate =
  req
    "DeleteJobTemplate"
    "fixture/DeleteJobTemplate.yaml"

requestDeletePolicy :: DeletePolicy -> TestTree
requestDeletePolicy =
  req
    "DeletePolicy"
    "fixture/DeletePolicy.yaml"

requestDeletePreset :: DeletePreset -> TestTree
requestDeletePreset =
  req
    "DeletePreset"
    "fixture/DeletePreset.yaml"

requestDeleteQueue :: DeleteQueue -> TestTree
requestDeleteQueue =
  req
    "DeleteQueue"
    "fixture/DeleteQueue.yaml"

requestDescribeEndpoints :: DescribeEndpoints -> TestTree
requestDescribeEndpoints =
  req
    "DescribeEndpoints"
    "fixture/DescribeEndpoints.yaml"

requestDisassociateCertificate :: DisassociateCertificate -> TestTree
requestDisassociateCertificate =
  req
    "DisassociateCertificate"
    "fixture/DisassociateCertificate.yaml"

requestGetJob :: GetJob -> TestTree
requestGetJob =
  req
    "GetJob"
    "fixture/GetJob.yaml"

requestGetJobTemplate :: GetJobTemplate -> TestTree
requestGetJobTemplate =
  req
    "GetJobTemplate"
    "fixture/GetJobTemplate.yaml"

requestGetPolicy :: GetPolicy -> TestTree
requestGetPolicy =
  req
    "GetPolicy"
    "fixture/GetPolicy.yaml"

requestGetPreset :: GetPreset -> TestTree
requestGetPreset =
  req
    "GetPreset"
    "fixture/GetPreset.yaml"

requestGetQueue :: GetQueue -> TestTree
requestGetQueue =
  req
    "GetQueue"
    "fixture/GetQueue.yaml"

requestListJobTemplates :: ListJobTemplates -> TestTree
requestListJobTemplates =
  req
    "ListJobTemplates"
    "fixture/ListJobTemplates.yaml"

requestListJobs :: ListJobs -> TestTree
requestListJobs =
  req
    "ListJobs"
    "fixture/ListJobs.yaml"

requestListPresets :: ListPresets -> TestTree
requestListPresets =
  req
    "ListPresets"
    "fixture/ListPresets.yaml"

requestListQueues :: ListQueues -> TestTree
requestListQueues =
  req
    "ListQueues"
    "fixture/ListQueues.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestPutPolicy :: PutPolicy -> TestTree
requestPutPolicy =
  req
    "PutPolicy"
    "fixture/PutPolicy.yaml"

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

requestUpdateJobTemplate :: UpdateJobTemplate -> TestTree
requestUpdateJobTemplate =
  req
    "UpdateJobTemplate"
    "fixture/UpdateJobTemplate.yaml"

requestUpdatePreset :: UpdatePreset -> TestTree
requestUpdatePreset =
  req
    "UpdatePreset"
    "fixture/UpdatePreset.yaml"

requestUpdateQueue :: UpdateQueue -> TestTree
requestUpdateQueue =
  req
    "UpdateQueue"
    "fixture/UpdateQueue.yaml"

-- Responses

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

responseCreateJob :: CreateJobResponse -> TestTree
responseCreateJob =
  res
    "CreateJobResponse"
    "fixture/CreateJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateJob)

responseCreateJobTemplate :: CreateJobTemplateResponse -> TestTree
responseCreateJobTemplate =
  res
    "CreateJobTemplateResponse"
    "fixture/CreateJobTemplateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateJobTemplate)

responseCreatePreset :: CreatePresetResponse -> TestTree
responseCreatePreset =
  res
    "CreatePresetResponse"
    "fixture/CreatePresetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreatePreset)

responseCreateQueue :: CreateQueueResponse -> TestTree
responseCreateQueue =
  res
    "CreateQueueResponse"
    "fixture/CreateQueueResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateQueue)

responseDeleteJobTemplate :: DeleteJobTemplateResponse -> TestTree
responseDeleteJobTemplate =
  res
    "DeleteJobTemplateResponse"
    "fixture/DeleteJobTemplateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteJobTemplate)

responseDeletePolicy :: DeletePolicyResponse -> TestTree
responseDeletePolicy =
  res
    "DeletePolicyResponse"
    "fixture/DeletePolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeletePolicy)

responseDeletePreset :: DeletePresetResponse -> TestTree
responseDeletePreset =
  res
    "DeletePresetResponse"
    "fixture/DeletePresetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeletePreset)

responseDeleteQueue :: DeleteQueueResponse -> TestTree
responseDeleteQueue =
  res
    "DeleteQueueResponse"
    "fixture/DeleteQueueResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteQueue)

responseDescribeEndpoints :: DescribeEndpointsResponse -> TestTree
responseDescribeEndpoints =
  res
    "DescribeEndpointsResponse"
    "fixture/DescribeEndpointsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeEndpoints)

responseDisassociateCertificate :: DisassociateCertificateResponse -> TestTree
responseDisassociateCertificate =
  res
    "DisassociateCertificateResponse"
    "fixture/DisassociateCertificateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisassociateCertificate)

responseGetJob :: GetJobResponse -> TestTree
responseGetJob =
  res
    "GetJobResponse"
    "fixture/GetJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetJob)

responseGetJobTemplate :: GetJobTemplateResponse -> TestTree
responseGetJobTemplate =
  res
    "GetJobTemplateResponse"
    "fixture/GetJobTemplateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetJobTemplate)

responseGetPolicy :: GetPolicyResponse -> TestTree
responseGetPolicy =
  res
    "GetPolicyResponse"
    "fixture/GetPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetPolicy)

responseGetPreset :: GetPresetResponse -> TestTree
responseGetPreset =
  res
    "GetPresetResponse"
    "fixture/GetPresetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetPreset)

responseGetQueue :: GetQueueResponse -> TestTree
responseGetQueue =
  res
    "GetQueueResponse"
    "fixture/GetQueueResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetQueue)

responseListJobTemplates :: ListJobTemplatesResponse -> TestTree
responseListJobTemplates =
  res
    "ListJobTemplatesResponse"
    "fixture/ListJobTemplatesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListJobTemplates)

responseListJobs :: ListJobsResponse -> TestTree
responseListJobs =
  res
    "ListJobsResponse"
    "fixture/ListJobsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListJobs)

responseListPresets :: ListPresetsResponse -> TestTree
responseListPresets =
  res
    "ListPresetsResponse"
    "fixture/ListPresetsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListPresets)

responseListQueues :: ListQueuesResponse -> TestTree
responseListQueues =
  res
    "ListQueuesResponse"
    "fixture/ListQueuesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListQueues)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responsePutPolicy :: PutPolicyResponse -> TestTree
responsePutPolicy =
  res
    "PutPolicyResponse"
    "fixture/PutPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutPolicy)

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

responseUpdateJobTemplate :: UpdateJobTemplateResponse -> TestTree
responseUpdateJobTemplate =
  res
    "UpdateJobTemplateResponse"
    "fixture/UpdateJobTemplateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateJobTemplate)

responseUpdatePreset :: UpdatePresetResponse -> TestTree
responseUpdatePreset =
  res
    "UpdatePresetResponse"
    "fixture/UpdatePresetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdatePreset)

responseUpdateQueue :: UpdateQueueResponse -> TestTree
responseUpdateQueue =
  res
    "UpdateQueueResponse"
    "fixture/UpdateQueueResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateQueue)
