{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.ElasticTranscoder
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.ElasticTranscoder where

import Amazonka.ElasticTranscoder
import qualified Data.Proxy as Proxy
import Test.Amazonka.ElasticTranscoder.Internal
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
--         [ requestCancelJob $
--             newCancelJob
--
--         , requestCreateJob $
--             newCreateJob
--
--         , requestCreatePipeline $
--             newCreatePipeline
--
--         , requestCreatePreset $
--             newCreatePreset
--
--         , requestDeletePipeline $
--             newDeletePipeline
--
--         , requestDeletePreset $
--             newDeletePreset
--
--         , requestListJobsByPipeline $
--             newListJobsByPipeline
--
--         , requestListJobsByStatus $
--             newListJobsByStatus
--
--         , requestListPipelines $
--             newListPipelines
--
--         , requestListPresets $
--             newListPresets
--
--         , requestReadJob $
--             newReadJob
--
--         , requestReadPipeline $
--             newReadPipeline
--
--         , requestReadPreset $
--             newReadPreset
--
--         , requestUpdatePipeline $
--             newUpdatePipeline
--
--         , requestUpdatePipelineNotifications $
--             newUpdatePipelineNotifications
--
--         , requestUpdatePipelineStatus $
--             newUpdatePipelineStatus
--
--           ]

--     , testGroup "response"
--         [ responseCancelJob $
--             newCancelJobResponse
--
--         , responseCreateJob $
--             newCreateJobResponse
--
--         , responseCreatePipeline $
--             newCreatePipelineResponse
--
--         , responseCreatePreset $
--             newCreatePresetResponse
--
--         , responseDeletePipeline $
--             newDeletePipelineResponse
--
--         , responseDeletePreset $
--             newDeletePresetResponse
--
--         , responseListJobsByPipeline $
--             newListJobsByPipelineResponse
--
--         , responseListJobsByStatus $
--             newListJobsByStatusResponse
--
--         , responseListPipelines $
--             newListPipelinesResponse
--
--         , responseListPresets $
--             newListPresetsResponse
--
--         , responseReadJob $
--             newReadJobResponse
--
--         , responseReadPipeline $
--             newReadPipelineResponse
--
--         , responseReadPreset $
--             newReadPresetResponse
--
--         , responseUpdatePipeline $
--             newUpdatePipelineResponse
--
--         , responseUpdatePipelineNotifications $
--             newUpdatePipelineNotificationsResponse
--
--         , responseUpdatePipelineStatus $
--             newUpdatePipelineStatusResponse
--
--           ]
--     ]

-- Requests

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

requestCreatePipeline :: CreatePipeline -> TestTree
requestCreatePipeline =
  req
    "CreatePipeline"
    "fixture/CreatePipeline.yaml"

requestCreatePreset :: CreatePreset -> TestTree
requestCreatePreset =
  req
    "CreatePreset"
    "fixture/CreatePreset.yaml"

requestDeletePipeline :: DeletePipeline -> TestTree
requestDeletePipeline =
  req
    "DeletePipeline"
    "fixture/DeletePipeline.yaml"

requestDeletePreset :: DeletePreset -> TestTree
requestDeletePreset =
  req
    "DeletePreset"
    "fixture/DeletePreset.yaml"

requestListJobsByPipeline :: ListJobsByPipeline -> TestTree
requestListJobsByPipeline =
  req
    "ListJobsByPipeline"
    "fixture/ListJobsByPipeline.yaml"

requestListJobsByStatus :: ListJobsByStatus -> TestTree
requestListJobsByStatus =
  req
    "ListJobsByStatus"
    "fixture/ListJobsByStatus.yaml"

requestListPipelines :: ListPipelines -> TestTree
requestListPipelines =
  req
    "ListPipelines"
    "fixture/ListPipelines.yaml"

requestListPresets :: ListPresets -> TestTree
requestListPresets =
  req
    "ListPresets"
    "fixture/ListPresets.yaml"

requestReadJob :: ReadJob -> TestTree
requestReadJob =
  req
    "ReadJob"
    "fixture/ReadJob.yaml"

requestReadPipeline :: ReadPipeline -> TestTree
requestReadPipeline =
  req
    "ReadPipeline"
    "fixture/ReadPipeline.yaml"

requestReadPreset :: ReadPreset -> TestTree
requestReadPreset =
  req
    "ReadPreset"
    "fixture/ReadPreset.yaml"

requestUpdatePipeline :: UpdatePipeline -> TestTree
requestUpdatePipeline =
  req
    "UpdatePipeline"
    "fixture/UpdatePipeline.yaml"

requestUpdatePipelineNotifications :: UpdatePipelineNotifications -> TestTree
requestUpdatePipelineNotifications =
  req
    "UpdatePipelineNotifications"
    "fixture/UpdatePipelineNotifications.yaml"

requestUpdatePipelineStatus :: UpdatePipelineStatus -> TestTree
requestUpdatePipelineStatus =
  req
    "UpdatePipelineStatus"
    "fixture/UpdatePipelineStatus.yaml"

-- Responses

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

responseCreatePipeline :: CreatePipelineResponse -> TestTree
responseCreatePipeline =
  res
    "CreatePipelineResponse"
    "fixture/CreatePipelineResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreatePipeline)

responseCreatePreset :: CreatePresetResponse -> TestTree
responseCreatePreset =
  res
    "CreatePresetResponse"
    "fixture/CreatePresetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreatePreset)

responseDeletePipeline :: DeletePipelineResponse -> TestTree
responseDeletePipeline =
  res
    "DeletePipelineResponse"
    "fixture/DeletePipelineResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeletePipeline)

responseDeletePreset :: DeletePresetResponse -> TestTree
responseDeletePreset =
  res
    "DeletePresetResponse"
    "fixture/DeletePresetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeletePreset)

responseListJobsByPipeline :: ListJobsByPipelineResponse -> TestTree
responseListJobsByPipeline =
  res
    "ListJobsByPipelineResponse"
    "fixture/ListJobsByPipelineResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListJobsByPipeline)

responseListJobsByStatus :: ListJobsByStatusResponse -> TestTree
responseListJobsByStatus =
  res
    "ListJobsByStatusResponse"
    "fixture/ListJobsByStatusResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListJobsByStatus)

responseListPipelines :: ListPipelinesResponse -> TestTree
responseListPipelines =
  res
    "ListPipelinesResponse"
    "fixture/ListPipelinesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListPipelines)

responseListPresets :: ListPresetsResponse -> TestTree
responseListPresets =
  res
    "ListPresetsResponse"
    "fixture/ListPresetsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListPresets)

responseReadJob :: ReadJobResponse -> TestTree
responseReadJob =
  res
    "ReadJobResponse"
    "fixture/ReadJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ReadJob)

responseReadPipeline :: ReadPipelineResponse -> TestTree
responseReadPipeline =
  res
    "ReadPipelineResponse"
    "fixture/ReadPipelineResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ReadPipeline)

responseReadPreset :: ReadPresetResponse -> TestTree
responseReadPreset =
  res
    "ReadPresetResponse"
    "fixture/ReadPresetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ReadPreset)

responseUpdatePipeline :: UpdatePipelineResponse -> TestTree
responseUpdatePipeline =
  res
    "UpdatePipelineResponse"
    "fixture/UpdatePipelineResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdatePipeline)

responseUpdatePipelineNotifications :: UpdatePipelineNotificationsResponse -> TestTree
responseUpdatePipelineNotifications =
  res
    "UpdatePipelineNotificationsResponse"
    "fixture/UpdatePipelineNotificationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdatePipelineNotifications)

responseUpdatePipelineStatus :: UpdatePipelineStatusResponse -> TestTree
responseUpdatePipelineStatus =
  res
    "UpdatePipelineStatusResponse"
    "fixture/UpdatePipelineStatusResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdatePipelineStatus)
