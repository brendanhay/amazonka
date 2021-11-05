{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.HealthLake
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.AWS.Gen.HealthLake where

import qualified Data.Proxy as Proxy
import Network.AWS.HealthLake
import Test.AWS.Fixture
import Test.AWS.HealthLake.Internal
import Test.AWS.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestStartFHIRImportJob $
--             newStartFHIRImportJob
--
--         , requestDescribeFHIRDatastore $
--             newDescribeFHIRDatastore
--
--         , requestDescribeFHIRImportJob $
--             newDescribeFHIRImportJob
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestDeleteFHIRDatastore $
--             newDeleteFHIRDatastore
--
--         , requestDescribeFHIRExportJob $
--             newDescribeFHIRExportJob
--
--         , requestCreateFHIRDatastore $
--             newCreateFHIRDatastore
--
--         , requestListFHIRExportJobs $
--             newListFHIRExportJobs
--
--         , requestTagResource $
--             newTagResource
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestListFHIRDatastores $
--             newListFHIRDatastores
--
--         , requestStartFHIRExportJob $
--             newStartFHIRExportJob
--
--         , requestListFHIRImportJobs $
--             newListFHIRImportJobs
--
--           ]

--     , testGroup "response"
--         [ responseStartFHIRImportJob $
--             newStartFHIRImportJobResponse
--
--         , responseDescribeFHIRDatastore $
--             newDescribeFHIRDatastoreResponse
--
--         , responseDescribeFHIRImportJob $
--             newDescribeFHIRImportJobResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseDeleteFHIRDatastore $
--             newDeleteFHIRDatastoreResponse
--
--         , responseDescribeFHIRExportJob $
--             newDescribeFHIRExportJobResponse
--
--         , responseCreateFHIRDatastore $
--             newCreateFHIRDatastoreResponse
--
--         , responseListFHIRExportJobs $
--             newListFHIRExportJobsResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseListFHIRDatastores $
--             newListFHIRDatastoresResponse
--
--         , responseStartFHIRExportJob $
--             newStartFHIRExportJobResponse
--
--         , responseListFHIRImportJobs $
--             newListFHIRImportJobsResponse
--
--           ]
--     ]

-- Requests

requestStartFHIRImportJob :: StartFHIRImportJob -> TestTree
requestStartFHIRImportJob =
  req
    "StartFHIRImportJob"
    "fixture/StartFHIRImportJob.yaml"

requestDescribeFHIRDatastore :: DescribeFHIRDatastore -> TestTree
requestDescribeFHIRDatastore =
  req
    "DescribeFHIRDatastore"
    "fixture/DescribeFHIRDatastore.yaml"

requestDescribeFHIRImportJob :: DescribeFHIRImportJob -> TestTree
requestDescribeFHIRImportJob =
  req
    "DescribeFHIRImportJob"
    "fixture/DescribeFHIRImportJob.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestDeleteFHIRDatastore :: DeleteFHIRDatastore -> TestTree
requestDeleteFHIRDatastore =
  req
    "DeleteFHIRDatastore"
    "fixture/DeleteFHIRDatastore.yaml"

requestDescribeFHIRExportJob :: DescribeFHIRExportJob -> TestTree
requestDescribeFHIRExportJob =
  req
    "DescribeFHIRExportJob"
    "fixture/DescribeFHIRExportJob.yaml"

requestCreateFHIRDatastore :: CreateFHIRDatastore -> TestTree
requestCreateFHIRDatastore =
  req
    "CreateFHIRDatastore"
    "fixture/CreateFHIRDatastore.yaml"

requestListFHIRExportJobs :: ListFHIRExportJobs -> TestTree
requestListFHIRExportJobs =
  req
    "ListFHIRExportJobs"
    "fixture/ListFHIRExportJobs.yaml"

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

requestListFHIRDatastores :: ListFHIRDatastores -> TestTree
requestListFHIRDatastores =
  req
    "ListFHIRDatastores"
    "fixture/ListFHIRDatastores.yaml"

requestStartFHIRExportJob :: StartFHIRExportJob -> TestTree
requestStartFHIRExportJob =
  req
    "StartFHIRExportJob"
    "fixture/StartFHIRExportJob.yaml"

requestListFHIRImportJobs :: ListFHIRImportJobs -> TestTree
requestListFHIRImportJobs =
  req
    "ListFHIRImportJobs"
    "fixture/ListFHIRImportJobs.yaml"

-- Responses

responseStartFHIRImportJob :: StartFHIRImportJobResponse -> TestTree
responseStartFHIRImportJob =
  res
    "StartFHIRImportJobResponse"
    "fixture/StartFHIRImportJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartFHIRImportJob)

responseDescribeFHIRDatastore :: DescribeFHIRDatastoreResponse -> TestTree
responseDescribeFHIRDatastore =
  res
    "DescribeFHIRDatastoreResponse"
    "fixture/DescribeFHIRDatastoreResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeFHIRDatastore)

responseDescribeFHIRImportJob :: DescribeFHIRImportJobResponse -> TestTree
responseDescribeFHIRImportJob =
  res
    "DescribeFHIRImportJobResponse"
    "fixture/DescribeFHIRImportJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeFHIRImportJob)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responseDeleteFHIRDatastore :: DeleteFHIRDatastoreResponse -> TestTree
responseDeleteFHIRDatastore =
  res
    "DeleteFHIRDatastoreResponse"
    "fixture/DeleteFHIRDatastoreResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteFHIRDatastore)

responseDescribeFHIRExportJob :: DescribeFHIRExportJobResponse -> TestTree
responseDescribeFHIRExportJob =
  res
    "DescribeFHIRExportJobResponse"
    "fixture/DescribeFHIRExportJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeFHIRExportJob)

responseCreateFHIRDatastore :: CreateFHIRDatastoreResponse -> TestTree
responseCreateFHIRDatastore =
  res
    "CreateFHIRDatastoreResponse"
    "fixture/CreateFHIRDatastoreResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateFHIRDatastore)

responseListFHIRExportJobs :: ListFHIRExportJobsResponse -> TestTree
responseListFHIRExportJobs =
  res
    "ListFHIRExportJobsResponse"
    "fixture/ListFHIRExportJobsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListFHIRExportJobs)

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

responseListFHIRDatastores :: ListFHIRDatastoresResponse -> TestTree
responseListFHIRDatastores =
  res
    "ListFHIRDatastoresResponse"
    "fixture/ListFHIRDatastoresResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListFHIRDatastores)

responseStartFHIRExportJob :: StartFHIRExportJobResponse -> TestTree
responseStartFHIRExportJob =
  res
    "StartFHIRExportJobResponse"
    "fixture/StartFHIRExportJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartFHIRExportJob)

responseListFHIRImportJobs :: ListFHIRImportJobsResponse -> TestTree
responseListFHIRImportJobs =
  res
    "ListFHIRImportJobsResponse"
    "fixture/ListFHIRImportJobsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListFHIRImportJobs)
