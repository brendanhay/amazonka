{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.MediaConvert
-- Copyright   : (c) 2013-2020 Brendan Hay
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
--         [ requestDeletePreset $
--             mkDeletePreset
--
--         , requestUpdatePreset $
--             mkUpdatePreset
--
--         , requestListTagsForResource $
--             mkListTagsForResource
--
--         , requestListQueues $
--             mkListQueues
--
--         , requestDeleteQueue $
--             mkDeleteQueue
--
--         , requestUpdateQueue $
--             mkUpdateQueue
--
--         , requestGetPreset $
--             mkGetPreset
--
--         , requestCreateJob $
--             mkCreateJob
--
--         , requestListJobs $
--             mkListJobs
--
--         , requestGetJob $
--             mkGetJob
--
--         , requestCreatePreset $
--             mkCreatePreset
--
--         , requestListPresets $
--             mkListPresets
--
--         , requestDisassociateCertificate $
--             mkDisassociateCertificate
--
--         , requestGetQueue $
--             mkGetQueue
--
--         , requestDescribeEndpoints $
--             mkDescribeEndpoints
--
--         , requestCreateQueue $
--             mkCreateQueue
--
--         , requestTagResource $
--             mkTagResource
--
--         , requestCreateJobTemplate $
--             mkCreateJobTemplate
--
--         , requestUntagResource $
--             mkUntagResource
--
--         , requestDeleteJobTemplate $
--             mkDeleteJobTemplate
--
--         , requestUpdateJobTemplate $
--             mkUpdateJobTemplate
--
--         , requestListJobTemplates $
--             mkListJobTemplates
--
--         , requestGetJobTemplate $
--             mkGetJobTemplate
--
--         , requestAssociateCertificate $
--             mkAssociateCertificate
--
--         , requestCancelJob $
--             mkCancelJob
--
--           ]

--     , testGroup "response"
--         [ responseDeletePreset $
--             mkDeletePresetResponse
--
--         , responseUpdatePreset $
--             mkUpdatePresetResponse
--
--         , responseListTagsForResource $
--             mkListTagsForResourceResponse
--
--         , responseListQueues $
--             mkListQueuesResponse
--
--         , responseDeleteQueue $
--             mkDeleteQueueResponse
--
--         , responseUpdateQueue $
--             mkUpdateQueueResponse
--
--         , responseGetPreset $
--             mkGetPresetResponse
--
--         , responseCreateJob $
--             mkCreateJobResponse
--
--         , responseListJobs $
--             mkListJobsResponse
--
--         , responseGetJob $
--             mkGetJobResponse
--
--         , responseCreatePreset $
--             mkCreatePresetResponse
--
--         , responseListPresets $
--             mkListPresetsResponse
--
--         , responseDisassociateCertificate $
--             mkDisassociateCertificateResponse
--
--         , responseGetQueue $
--             mkGetQueueResponse
--
--         , responseDescribeEndpoints $
--             mkDescribeEndpointsResponse
--
--         , responseCreateQueue $
--             mkCreateQueueResponse
--
--         , responseTagResource $
--             mkTagResourceResponse
--
--         , responseCreateJobTemplate $
--             mkCreateJobTemplateResponse
--
--         , responseUntagResource $
--             mkUntagResourceResponse
--
--         , responseDeleteJobTemplate $
--             mkDeleteJobTemplateResponse
--
--         , responseUpdateJobTemplate $
--             mkUpdateJobTemplateResponse
--
--         , responseListJobTemplates $
--             mkListJobTemplatesResponse
--
--         , responseGetJobTemplate $
--             mkGetJobTemplateResponse
--
--         , responseAssociateCertificate $
--             mkAssociateCertificateResponse
--
--         , responseCancelJob $
--             mkCancelJobResponse
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

requestGetJob :: GetJob -> TestTree
requestGetJob =
  req
    "GetJob"
    "fixture/GetJob.yaml"

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
    mediaConvertService
    (Proxy :: Proxy DeletePreset)

responseUpdatePreset :: UpdatePresetResponse -> TestTree
responseUpdatePreset =
  res
    "UpdatePresetResponse"
    "fixture/UpdatePresetResponse.proto"
    mediaConvertService
    (Proxy :: Proxy UpdatePreset)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    mediaConvertService
    (Proxy :: Proxy ListTagsForResource)

responseListQueues :: ListQueuesResponse -> TestTree
responseListQueues =
  res
    "ListQueuesResponse"
    "fixture/ListQueuesResponse.proto"
    mediaConvertService
    (Proxy :: Proxy ListQueues)

responseDeleteQueue :: DeleteQueueResponse -> TestTree
responseDeleteQueue =
  res
    "DeleteQueueResponse"
    "fixture/DeleteQueueResponse.proto"
    mediaConvertService
    (Proxy :: Proxy DeleteQueue)

responseUpdateQueue :: UpdateQueueResponse -> TestTree
responseUpdateQueue =
  res
    "UpdateQueueResponse"
    "fixture/UpdateQueueResponse.proto"
    mediaConvertService
    (Proxy :: Proxy UpdateQueue)

responseGetPreset :: GetPresetResponse -> TestTree
responseGetPreset =
  res
    "GetPresetResponse"
    "fixture/GetPresetResponse.proto"
    mediaConvertService
    (Proxy :: Proxy GetPreset)

responseCreateJob :: CreateJobResponse -> TestTree
responseCreateJob =
  res
    "CreateJobResponse"
    "fixture/CreateJobResponse.proto"
    mediaConvertService
    (Proxy :: Proxy CreateJob)

responseListJobs :: ListJobsResponse -> TestTree
responseListJobs =
  res
    "ListJobsResponse"
    "fixture/ListJobsResponse.proto"
    mediaConvertService
    (Proxy :: Proxy ListJobs)

responseGetJob :: GetJobResponse -> TestTree
responseGetJob =
  res
    "GetJobResponse"
    "fixture/GetJobResponse.proto"
    mediaConvertService
    (Proxy :: Proxy GetJob)

responseCreatePreset :: CreatePresetResponse -> TestTree
responseCreatePreset =
  res
    "CreatePresetResponse"
    "fixture/CreatePresetResponse.proto"
    mediaConvertService
    (Proxy :: Proxy CreatePreset)

responseListPresets :: ListPresetsResponse -> TestTree
responseListPresets =
  res
    "ListPresetsResponse"
    "fixture/ListPresetsResponse.proto"
    mediaConvertService
    (Proxy :: Proxy ListPresets)

responseDisassociateCertificate :: DisassociateCertificateResponse -> TestTree
responseDisassociateCertificate =
  res
    "DisassociateCertificateResponse"
    "fixture/DisassociateCertificateResponse.proto"
    mediaConvertService
    (Proxy :: Proxy DisassociateCertificate)

responseGetQueue :: GetQueueResponse -> TestTree
responseGetQueue =
  res
    "GetQueueResponse"
    "fixture/GetQueueResponse.proto"
    mediaConvertService
    (Proxy :: Proxy GetQueue)

responseDescribeEndpoints :: DescribeEndpointsResponse -> TestTree
responseDescribeEndpoints =
  res
    "DescribeEndpointsResponse"
    "fixture/DescribeEndpointsResponse.proto"
    mediaConvertService
    (Proxy :: Proxy DescribeEndpoints)

responseCreateQueue :: CreateQueueResponse -> TestTree
responseCreateQueue =
  res
    "CreateQueueResponse"
    "fixture/CreateQueueResponse.proto"
    mediaConvertService
    (Proxy :: Proxy CreateQueue)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    mediaConvertService
    (Proxy :: Proxy TagResource)

responseCreateJobTemplate :: CreateJobTemplateResponse -> TestTree
responseCreateJobTemplate =
  res
    "CreateJobTemplateResponse"
    "fixture/CreateJobTemplateResponse.proto"
    mediaConvertService
    (Proxy :: Proxy CreateJobTemplate)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    mediaConvertService
    (Proxy :: Proxy UntagResource)

responseDeleteJobTemplate :: DeleteJobTemplateResponse -> TestTree
responseDeleteJobTemplate =
  res
    "DeleteJobTemplateResponse"
    "fixture/DeleteJobTemplateResponse.proto"
    mediaConvertService
    (Proxy :: Proxy DeleteJobTemplate)

responseUpdateJobTemplate :: UpdateJobTemplateResponse -> TestTree
responseUpdateJobTemplate =
  res
    "UpdateJobTemplateResponse"
    "fixture/UpdateJobTemplateResponse.proto"
    mediaConvertService
    (Proxy :: Proxy UpdateJobTemplate)

responseListJobTemplates :: ListJobTemplatesResponse -> TestTree
responseListJobTemplates =
  res
    "ListJobTemplatesResponse"
    "fixture/ListJobTemplatesResponse.proto"
    mediaConvertService
    (Proxy :: Proxy ListJobTemplates)

responseGetJobTemplate :: GetJobTemplateResponse -> TestTree
responseGetJobTemplate =
  res
    "GetJobTemplateResponse"
    "fixture/GetJobTemplateResponse.proto"
    mediaConvertService
    (Proxy :: Proxy GetJobTemplate)

responseAssociateCertificate :: AssociateCertificateResponse -> TestTree
responseAssociateCertificate =
  res
    "AssociateCertificateResponse"
    "fixture/AssociateCertificateResponse.proto"
    mediaConvertService
    (Proxy :: Proxy AssociateCertificate)

responseCancelJob :: CancelJobResponse -> TestTree
responseCancelJob =
  res
    "CancelJobResponse"
    "fixture/CancelJobResponse.proto"
    mediaConvertService
    (Proxy :: Proxy CancelJob)
