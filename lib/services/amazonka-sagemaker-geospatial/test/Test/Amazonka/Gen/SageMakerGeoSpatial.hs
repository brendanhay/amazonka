{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.SageMakerGeoSpatial
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.SageMakerGeoSpatial where

import Amazonka.SageMakerGeoSpatial
import qualified Data.Proxy as Proxy
import Test.Amazonka.Fixture
import Test.Amazonka.Prelude
import Test.Amazonka.SageMakerGeoSpatial.Internal
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestDeleteEarthObservationJob $
--             newDeleteEarthObservationJob
--
--         , requestDeleteVectorEnrichmentJob $
--             newDeleteVectorEnrichmentJob
--
--         , requestExportEarthObservationJob $
--             newExportEarthObservationJob
--
--         , requestExportVectorEnrichmentJob $
--             newExportVectorEnrichmentJob
--
--         , requestGetEarthObservationJob $
--             newGetEarthObservationJob
--
--         , requestGetRasterDataCollection $
--             newGetRasterDataCollection
--
--         , requestGetTile $
--             newGetTile
--
--         , requestGetVectorEnrichmentJob $
--             newGetVectorEnrichmentJob
--
--         , requestListEarthObservationJobs $
--             newListEarthObservationJobs
--
--         , requestListRasterDataCollections $
--             newListRasterDataCollections
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestListVectorEnrichmentJobs $
--             newListVectorEnrichmentJobs
--
--         , requestSearchRasterDataCollection $
--             newSearchRasterDataCollection
--
--         , requestStartEarthObservationJob $
--             newStartEarthObservationJob
--
--         , requestStartVectorEnrichmentJob $
--             newStartVectorEnrichmentJob
--
--         , requestStopEarthObservationJob $
--             newStopEarthObservationJob
--
--         , requestStopVectorEnrichmentJob $
--             newStopVectorEnrichmentJob
--
--         , requestTagResource $
--             newTagResource
--
--         , requestUntagResource $
--             newUntagResource
--
--           ]

--     , testGroup "response"
--         [ responseDeleteEarthObservationJob $
--             newDeleteEarthObservationJobResponse
--
--         , responseDeleteVectorEnrichmentJob $
--             newDeleteVectorEnrichmentJobResponse
--
--         , responseExportEarthObservationJob $
--             newExportEarthObservationJobResponse
--
--         , responseExportVectorEnrichmentJob $
--             newExportVectorEnrichmentJobResponse
--
--         , responseGetEarthObservationJob $
--             newGetEarthObservationJobResponse
--
--         , responseGetRasterDataCollection $
--             newGetRasterDataCollectionResponse
--
--         , responseGetTile $
--             newGetTileResponse
--
--         , responseGetVectorEnrichmentJob $
--             newGetVectorEnrichmentJobResponse
--
--         , responseListEarthObservationJobs $
--             newListEarthObservationJobsResponse
--
--         , responseListRasterDataCollections $
--             newListRasterDataCollectionsResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseListVectorEnrichmentJobs $
--             newListVectorEnrichmentJobsResponse
--
--         , responseSearchRasterDataCollection $
--             newSearchRasterDataCollectionResponse
--
--         , responseStartEarthObservationJob $
--             newStartEarthObservationJobResponse
--
--         , responseStartVectorEnrichmentJob $
--             newStartVectorEnrichmentJobResponse
--
--         , responseStopEarthObservationJob $
--             newStopEarthObservationJobResponse
--
--         , responseStopVectorEnrichmentJob $
--             newStopVectorEnrichmentJobResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--           ]
--     ]

-- Requests

requestDeleteEarthObservationJob :: DeleteEarthObservationJob -> TestTree
requestDeleteEarthObservationJob =
  req
    "DeleteEarthObservationJob"
    "fixture/DeleteEarthObservationJob.yaml"

requestDeleteVectorEnrichmentJob :: DeleteVectorEnrichmentJob -> TestTree
requestDeleteVectorEnrichmentJob =
  req
    "DeleteVectorEnrichmentJob"
    "fixture/DeleteVectorEnrichmentJob.yaml"

requestExportEarthObservationJob :: ExportEarthObservationJob -> TestTree
requestExportEarthObservationJob =
  req
    "ExportEarthObservationJob"
    "fixture/ExportEarthObservationJob.yaml"

requestExportVectorEnrichmentJob :: ExportVectorEnrichmentJob -> TestTree
requestExportVectorEnrichmentJob =
  req
    "ExportVectorEnrichmentJob"
    "fixture/ExportVectorEnrichmentJob.yaml"

requestGetEarthObservationJob :: GetEarthObservationJob -> TestTree
requestGetEarthObservationJob =
  req
    "GetEarthObservationJob"
    "fixture/GetEarthObservationJob.yaml"

requestGetRasterDataCollection :: GetRasterDataCollection -> TestTree
requestGetRasterDataCollection =
  req
    "GetRasterDataCollection"
    "fixture/GetRasterDataCollection.yaml"

requestGetTile :: GetTile -> TestTree
requestGetTile =
  req
    "GetTile"
    "fixture/GetTile.yaml"

requestGetVectorEnrichmentJob :: GetVectorEnrichmentJob -> TestTree
requestGetVectorEnrichmentJob =
  req
    "GetVectorEnrichmentJob"
    "fixture/GetVectorEnrichmentJob.yaml"

requestListEarthObservationJobs :: ListEarthObservationJobs -> TestTree
requestListEarthObservationJobs =
  req
    "ListEarthObservationJobs"
    "fixture/ListEarthObservationJobs.yaml"

requestListRasterDataCollections :: ListRasterDataCollections -> TestTree
requestListRasterDataCollections =
  req
    "ListRasterDataCollections"
    "fixture/ListRasterDataCollections.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestListVectorEnrichmentJobs :: ListVectorEnrichmentJobs -> TestTree
requestListVectorEnrichmentJobs =
  req
    "ListVectorEnrichmentJobs"
    "fixture/ListVectorEnrichmentJobs.yaml"

requestSearchRasterDataCollection :: SearchRasterDataCollection -> TestTree
requestSearchRasterDataCollection =
  req
    "SearchRasterDataCollection"
    "fixture/SearchRasterDataCollection.yaml"

requestStartEarthObservationJob :: StartEarthObservationJob -> TestTree
requestStartEarthObservationJob =
  req
    "StartEarthObservationJob"
    "fixture/StartEarthObservationJob.yaml"

requestStartVectorEnrichmentJob :: StartVectorEnrichmentJob -> TestTree
requestStartVectorEnrichmentJob =
  req
    "StartVectorEnrichmentJob"
    "fixture/StartVectorEnrichmentJob.yaml"

requestStopEarthObservationJob :: StopEarthObservationJob -> TestTree
requestStopEarthObservationJob =
  req
    "StopEarthObservationJob"
    "fixture/StopEarthObservationJob.yaml"

requestStopVectorEnrichmentJob :: StopVectorEnrichmentJob -> TestTree
requestStopVectorEnrichmentJob =
  req
    "StopVectorEnrichmentJob"
    "fixture/StopVectorEnrichmentJob.yaml"

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

-- Responses

responseDeleteEarthObservationJob :: DeleteEarthObservationJobResponse -> TestTree
responseDeleteEarthObservationJob =
  res
    "DeleteEarthObservationJobResponse"
    "fixture/DeleteEarthObservationJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteEarthObservationJob)

responseDeleteVectorEnrichmentJob :: DeleteVectorEnrichmentJobResponse -> TestTree
responseDeleteVectorEnrichmentJob =
  res
    "DeleteVectorEnrichmentJobResponse"
    "fixture/DeleteVectorEnrichmentJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteVectorEnrichmentJob)

responseExportEarthObservationJob :: ExportEarthObservationJobResponse -> TestTree
responseExportEarthObservationJob =
  res
    "ExportEarthObservationJobResponse"
    "fixture/ExportEarthObservationJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ExportEarthObservationJob)

responseExportVectorEnrichmentJob :: ExportVectorEnrichmentJobResponse -> TestTree
responseExportVectorEnrichmentJob =
  res
    "ExportVectorEnrichmentJobResponse"
    "fixture/ExportVectorEnrichmentJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ExportVectorEnrichmentJob)

responseGetEarthObservationJob :: GetEarthObservationJobResponse -> TestTree
responseGetEarthObservationJob =
  res
    "GetEarthObservationJobResponse"
    "fixture/GetEarthObservationJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetEarthObservationJob)

responseGetRasterDataCollection :: GetRasterDataCollectionResponse -> TestTree
responseGetRasterDataCollection =
  res
    "GetRasterDataCollectionResponse"
    "fixture/GetRasterDataCollectionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetRasterDataCollection)

responseGetVectorEnrichmentJob :: GetVectorEnrichmentJobResponse -> TestTree
responseGetVectorEnrichmentJob =
  res
    "GetVectorEnrichmentJobResponse"
    "fixture/GetVectorEnrichmentJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetVectorEnrichmentJob)

responseListEarthObservationJobs :: ListEarthObservationJobsResponse -> TestTree
responseListEarthObservationJobs =
  res
    "ListEarthObservationJobsResponse"
    "fixture/ListEarthObservationJobsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListEarthObservationJobs)

responseListRasterDataCollections :: ListRasterDataCollectionsResponse -> TestTree
responseListRasterDataCollections =
  res
    "ListRasterDataCollectionsResponse"
    "fixture/ListRasterDataCollectionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListRasterDataCollections)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responseListVectorEnrichmentJobs :: ListVectorEnrichmentJobsResponse -> TestTree
responseListVectorEnrichmentJobs =
  res
    "ListVectorEnrichmentJobsResponse"
    "fixture/ListVectorEnrichmentJobsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListVectorEnrichmentJobs)

responseSearchRasterDataCollection :: SearchRasterDataCollectionResponse -> TestTree
responseSearchRasterDataCollection =
  res
    "SearchRasterDataCollectionResponse"
    "fixture/SearchRasterDataCollectionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SearchRasterDataCollection)

responseStartEarthObservationJob :: StartEarthObservationJobResponse -> TestTree
responseStartEarthObservationJob =
  res
    "StartEarthObservationJobResponse"
    "fixture/StartEarthObservationJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartEarthObservationJob)

responseStartVectorEnrichmentJob :: StartVectorEnrichmentJobResponse -> TestTree
responseStartVectorEnrichmentJob =
  res
    "StartVectorEnrichmentJobResponse"
    "fixture/StartVectorEnrichmentJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartVectorEnrichmentJob)

responseStopEarthObservationJob :: StopEarthObservationJobResponse -> TestTree
responseStopEarthObservationJob =
  res
    "StopEarthObservationJobResponse"
    "fixture/StopEarthObservationJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StopEarthObservationJob)

responseStopVectorEnrichmentJob :: StopVectorEnrichmentJobResponse -> TestTree
responseStopVectorEnrichmentJob =
  res
    "StopVectorEnrichmentJobResponse"
    "fixture/StopVectorEnrichmentJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StopVectorEnrichmentJob)

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
