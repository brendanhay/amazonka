{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.HealthLake
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.HealthLake where

import Amazonka.HealthLake
import qualified Data.Proxy as Proxy
import Test.Amazonka.Fixture
import Test.Amazonka.HealthLake.Internal
import Test.Amazonka.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestCreateFHIRDatastore $
--             newCreateFHIRDatastore
--
--         , requestDeleteFHIRDatastore $
--             newDeleteFHIRDatastore
--
--         , requestDescribeFHIRDatastore $
--             newDescribeFHIRDatastore
--
--         , requestDescribeFHIRExportJob $
--             newDescribeFHIRExportJob
--
--         , requestDescribeFHIRImportJob $
--             newDescribeFHIRImportJob
--
--         , requestListFHIRDatastores $
--             newListFHIRDatastores
--
--         , requestListFHIRExportJobs $
--             newListFHIRExportJobs
--
--         , requestListFHIRImportJobs $
--             newListFHIRImportJobs
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestStartFHIRExportJob $
--             newStartFHIRExportJob
--
--         , requestStartFHIRImportJob $
--             newStartFHIRImportJob
--
--         , requestTagResource $
--             newTagResource
--
--         , requestUntagResource $
--             newUntagResource
--
--           ]

--     , testGroup "response"
--         [ responseCreateFHIRDatastore $
--             newCreateFHIRDatastoreResponse
--
--         , responseDeleteFHIRDatastore $
--             newDeleteFHIRDatastoreResponse
--
--         , responseDescribeFHIRDatastore $
--             newDescribeFHIRDatastoreResponse
--
--         , responseDescribeFHIRExportJob $
--             newDescribeFHIRExportJobResponse
--
--         , responseDescribeFHIRImportJob $
--             newDescribeFHIRImportJobResponse
--
--         , responseListFHIRDatastores $
--             newListFHIRDatastoresResponse
--
--         , responseListFHIRExportJobs $
--             newListFHIRExportJobsResponse
--
--         , responseListFHIRImportJobs $
--             newListFHIRImportJobsResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseStartFHIRExportJob $
--             newStartFHIRExportJobResponse
--
--         , responseStartFHIRImportJob $
--             newStartFHIRImportJobResponse
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

requestCreateFHIRDatastore :: CreateFHIRDatastore -> TestTree
requestCreateFHIRDatastore =
  req
    "CreateFHIRDatastore"
    "fixture/CreateFHIRDatastore.yaml"

requestDeleteFHIRDatastore :: DeleteFHIRDatastore -> TestTree
requestDeleteFHIRDatastore =
  req
    "DeleteFHIRDatastore"
    "fixture/DeleteFHIRDatastore.yaml"

requestDescribeFHIRDatastore :: DescribeFHIRDatastore -> TestTree
requestDescribeFHIRDatastore =
  req
    "DescribeFHIRDatastore"
    "fixture/DescribeFHIRDatastore.yaml"

requestDescribeFHIRExportJob :: DescribeFHIRExportJob -> TestTree
requestDescribeFHIRExportJob =
  req
    "DescribeFHIRExportJob"
    "fixture/DescribeFHIRExportJob.yaml"

requestDescribeFHIRImportJob :: DescribeFHIRImportJob -> TestTree
requestDescribeFHIRImportJob =
  req
    "DescribeFHIRImportJob"
    "fixture/DescribeFHIRImportJob.yaml"

requestListFHIRDatastores :: ListFHIRDatastores -> TestTree
requestListFHIRDatastores =
  req
    "ListFHIRDatastores"
    "fixture/ListFHIRDatastores.yaml"

requestListFHIRExportJobs :: ListFHIRExportJobs -> TestTree
requestListFHIRExportJobs =
  req
    "ListFHIRExportJobs"
    "fixture/ListFHIRExportJobs.yaml"

requestListFHIRImportJobs :: ListFHIRImportJobs -> TestTree
requestListFHIRImportJobs =
  req
    "ListFHIRImportJobs"
    "fixture/ListFHIRImportJobs.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestStartFHIRExportJob :: StartFHIRExportJob -> TestTree
requestStartFHIRExportJob =
  req
    "StartFHIRExportJob"
    "fixture/StartFHIRExportJob.yaml"

requestStartFHIRImportJob :: StartFHIRImportJob -> TestTree
requestStartFHIRImportJob =
  req
    "StartFHIRImportJob"
    "fixture/StartFHIRImportJob.yaml"

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

responseCreateFHIRDatastore :: CreateFHIRDatastoreResponse -> TestTree
responseCreateFHIRDatastore =
  res
    "CreateFHIRDatastoreResponse"
    "fixture/CreateFHIRDatastoreResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateFHIRDatastore)

responseDeleteFHIRDatastore :: DeleteFHIRDatastoreResponse -> TestTree
responseDeleteFHIRDatastore =
  res
    "DeleteFHIRDatastoreResponse"
    "fixture/DeleteFHIRDatastoreResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteFHIRDatastore)

responseDescribeFHIRDatastore :: DescribeFHIRDatastoreResponse -> TestTree
responseDescribeFHIRDatastore =
  res
    "DescribeFHIRDatastoreResponse"
    "fixture/DescribeFHIRDatastoreResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeFHIRDatastore)

responseDescribeFHIRExportJob :: DescribeFHIRExportJobResponse -> TestTree
responseDescribeFHIRExportJob =
  res
    "DescribeFHIRExportJobResponse"
    "fixture/DescribeFHIRExportJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeFHIRExportJob)

responseDescribeFHIRImportJob :: DescribeFHIRImportJobResponse -> TestTree
responseDescribeFHIRImportJob =
  res
    "DescribeFHIRImportJobResponse"
    "fixture/DescribeFHIRImportJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeFHIRImportJob)

responseListFHIRDatastores :: ListFHIRDatastoresResponse -> TestTree
responseListFHIRDatastores =
  res
    "ListFHIRDatastoresResponse"
    "fixture/ListFHIRDatastoresResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListFHIRDatastores)

responseListFHIRExportJobs :: ListFHIRExportJobsResponse -> TestTree
responseListFHIRExportJobs =
  res
    "ListFHIRExportJobsResponse"
    "fixture/ListFHIRExportJobsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListFHIRExportJobs)

responseListFHIRImportJobs :: ListFHIRImportJobsResponse -> TestTree
responseListFHIRImportJobs =
  res
    "ListFHIRImportJobsResponse"
    "fixture/ListFHIRImportJobsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListFHIRImportJobs)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responseStartFHIRExportJob :: StartFHIRExportJobResponse -> TestTree
responseStartFHIRExportJob =
  res
    "StartFHIRExportJobResponse"
    "fixture/StartFHIRExportJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartFHIRExportJob)

responseStartFHIRImportJob :: StartFHIRImportJobResponse -> TestTree
responseStartFHIRImportJob =
  res
    "StartFHIRImportJobResponse"
    "fixture/StartFHIRImportJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartFHIRImportJob)

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
