{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.ElasticTranscoder
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.AWS.Gen.ElasticTranscoder where

import Data.Proxy
import Network.AWS.ElasticTranscoder
import Test.AWS.ElasticTranscoder.Internal
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
--         [ requestUpdatePipelineStatus $
--             newUpdatePipelineStatus
--
--         , requestListJobsByPipeline $
--             newListJobsByPipeline
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
--         , requestCancelJob $
--             newCancelJob
--
--         , requestCreatePipeline $
--             newCreatePipeline
--
--         , requestDeletePipeline $
--             newDeletePipeline
--
--         , requestUpdatePipeline $
--             newUpdatePipeline
--
--         , requestReadPreset $
--             newReadPreset
--
--         , requestListJobsByStatus $
--             newListJobsByStatus
--
--         , requestCreateJob $
--             newCreateJob
--
--         , requestListPipelines $
--             newListPipelines
--
--         , requestReadPipeline $
--             newReadPipeline
--
--         , requestUpdatePipelineNotifications $
--             newUpdatePipelineNotifications
--
--         , requestReadJob $
--             newReadJob
--
--           ]

--     , testGroup "response"
--         [ responseUpdatePipelineStatus $
--             newUpdatePipelineStatusResponse
--
--         , responseListJobsByPipeline $
--             newListJobsByPipelineResponse
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
--         , responseCancelJob $
--             newCancelJobResponse
--
--         , responseCreatePipeline $
--             newCreatePipelineResponse
--
--         , responseDeletePipeline $
--             newDeletePipelineResponse
--
--         , responseUpdatePipeline $
--             newUpdatePipelineResponse
--
--         , responseReadPreset $
--             newReadPresetResponse
--
--         , responseListJobsByStatus $
--             newListJobsByStatusResponse
--
--         , responseCreateJob $
--             newCreateJobResponse
--
--         , responseListPipelines $
--             newListPipelinesResponse
--
--         , responseReadPipeline $
--             newReadPipelineResponse
--
--         , responseUpdatePipelineNotifications $
--             newUpdatePipelineNotificationsResponse
--
--         , responseReadJob $
--             newReadJobResponse
--
--           ]
--     ]

-- Requests

requestUpdatePipelineStatus :: UpdatePipelineStatus -> TestTree
requestUpdatePipelineStatus =
  req
    "UpdatePipelineStatus"
    "fixture/UpdatePipelineStatus.yaml"

requestListJobsByPipeline :: ListJobsByPipeline -> TestTree
requestListJobsByPipeline =
  req
    "ListJobsByPipeline"
    "fixture/ListJobsByPipeline.yaml"

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

requestCancelJob :: CancelJob -> TestTree
requestCancelJob =
  req
    "CancelJob"
    "fixture/CancelJob.yaml"

requestCreatePipeline :: CreatePipeline -> TestTree
requestCreatePipeline =
  req
    "CreatePipeline"
    "fixture/CreatePipeline.yaml"

requestDeletePipeline :: DeletePipeline -> TestTree
requestDeletePipeline =
  req
    "DeletePipeline"
    "fixture/DeletePipeline.yaml"

requestUpdatePipeline :: UpdatePipeline -> TestTree
requestUpdatePipeline =
  req
    "UpdatePipeline"
    "fixture/UpdatePipeline.yaml"

requestReadPreset :: ReadPreset -> TestTree
requestReadPreset =
  req
    "ReadPreset"
    "fixture/ReadPreset.yaml"

requestListJobsByStatus :: ListJobsByStatus -> TestTree
requestListJobsByStatus =
  req
    "ListJobsByStatus"
    "fixture/ListJobsByStatus.yaml"

requestCreateJob :: CreateJob -> TestTree
requestCreateJob =
  req
    "CreateJob"
    "fixture/CreateJob.yaml"

requestListPipelines :: ListPipelines -> TestTree
requestListPipelines =
  req
    "ListPipelines"
    "fixture/ListPipelines.yaml"

requestReadPipeline :: ReadPipeline -> TestTree
requestReadPipeline =
  req
    "ReadPipeline"
    "fixture/ReadPipeline.yaml"

requestUpdatePipelineNotifications :: UpdatePipelineNotifications -> TestTree
requestUpdatePipelineNotifications =
  req
    "UpdatePipelineNotifications"
    "fixture/UpdatePipelineNotifications.yaml"

requestReadJob :: ReadJob -> TestTree
requestReadJob =
  req
    "ReadJob"
    "fixture/ReadJob.yaml"

-- Responses

responseUpdatePipelineStatus :: UpdatePipelineStatusResponse -> TestTree
responseUpdatePipelineStatus =
  res
    "UpdatePipelineStatusResponse"
    "fixture/UpdatePipelineStatusResponse.proto"
    defaultService
    (Proxy :: Proxy UpdatePipelineStatus)

responseListJobsByPipeline :: ListJobsByPipelineResponse -> TestTree
responseListJobsByPipeline =
  res
    "ListJobsByPipelineResponse"
    "fixture/ListJobsByPipelineResponse.proto"
    defaultService
    (Proxy :: Proxy ListJobsByPipeline)

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

responseCancelJob :: CancelJobResponse -> TestTree
responseCancelJob =
  res
    "CancelJobResponse"
    "fixture/CancelJobResponse.proto"
    defaultService
    (Proxy :: Proxy CancelJob)

responseCreatePipeline :: CreatePipelineResponse -> TestTree
responseCreatePipeline =
  res
    "CreatePipelineResponse"
    "fixture/CreatePipelineResponse.proto"
    defaultService
    (Proxy :: Proxy CreatePipeline)

responseDeletePipeline :: DeletePipelineResponse -> TestTree
responseDeletePipeline =
  res
    "DeletePipelineResponse"
    "fixture/DeletePipelineResponse.proto"
    defaultService
    (Proxy :: Proxy DeletePipeline)

responseUpdatePipeline :: UpdatePipelineResponse -> TestTree
responseUpdatePipeline =
  res
    "UpdatePipelineResponse"
    "fixture/UpdatePipelineResponse.proto"
    defaultService
    (Proxy :: Proxy UpdatePipeline)

responseReadPreset :: ReadPresetResponse -> TestTree
responseReadPreset =
  res
    "ReadPresetResponse"
    "fixture/ReadPresetResponse.proto"
    defaultService
    (Proxy :: Proxy ReadPreset)

responseListJobsByStatus :: ListJobsByStatusResponse -> TestTree
responseListJobsByStatus =
  res
    "ListJobsByStatusResponse"
    "fixture/ListJobsByStatusResponse.proto"
    defaultService
    (Proxy :: Proxy ListJobsByStatus)

responseCreateJob :: CreateJobResponse -> TestTree
responseCreateJob =
  res
    "CreateJobResponse"
    "fixture/CreateJobResponse.proto"
    defaultService
    (Proxy :: Proxy CreateJob)

responseListPipelines :: ListPipelinesResponse -> TestTree
responseListPipelines =
  res
    "ListPipelinesResponse"
    "fixture/ListPipelinesResponse.proto"
    defaultService
    (Proxy :: Proxy ListPipelines)

responseReadPipeline :: ReadPipelineResponse -> TestTree
responseReadPipeline =
  res
    "ReadPipelineResponse"
    "fixture/ReadPipelineResponse.proto"
    defaultService
    (Proxy :: Proxy ReadPipeline)

responseUpdatePipelineNotifications :: UpdatePipelineNotificationsResponse -> TestTree
responseUpdatePipelineNotifications =
  res
    "UpdatePipelineNotificationsResponse"
    "fixture/UpdatePipelineNotificationsResponse.proto"
    defaultService
    (Proxy :: Proxy UpdatePipelineNotifications)

responseReadJob :: ReadJobResponse -> TestTree
responseReadJob =
  res
    "ReadJobResponse"
    "fixture/ReadJobResponse.proto"
    defaultService
    (Proxy :: Proxy ReadJob)
