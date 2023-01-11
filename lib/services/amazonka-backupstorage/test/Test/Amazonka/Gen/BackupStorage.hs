{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.BackupStorage
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.BackupStorage where

import Amazonka.BackupStorage
import qualified Data.Proxy as Proxy
import Test.Amazonka.BackupStorage.Internal
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
--         [ requestDeleteObject $
--             newDeleteObject
--
--         , requestGetChunk $
--             newGetChunk
--
--         , requestGetObjectMetadata $
--             newGetObjectMetadata
--
--         , requestListChunks $
--             newListChunks
--
--         , requestListObjects $
--             newListObjects
--
--         , requestNotifyObjectComplete $
--             newNotifyObjectComplete
--
--         , requestPutChunk $
--             newPutChunk
--
--         , requestPutObject $
--             newPutObject
--
--         , requestStartObject $
--             newStartObject
--
--           ]

--     , testGroup "response"
--         [ responseDeleteObject $
--             newDeleteObjectResponse
--
--         , responseGetChunk $
--             newGetChunkResponse
--
--         , responseGetObjectMetadata $
--             newGetObjectMetadataResponse
--
--         , responseListChunks $
--             newListChunksResponse
--
--         , responseListObjects $
--             newListObjectsResponse
--
--         , responseNotifyObjectComplete $
--             newNotifyObjectCompleteResponse
--
--         , responsePutChunk $
--             newPutChunkResponse
--
--         , responsePutObject $
--             newPutObjectResponse
--
--         , responseStartObject $
--             newStartObjectResponse
--
--           ]
--     ]

-- Requests

requestDeleteObject :: DeleteObject -> TestTree
requestDeleteObject =
  req
    "DeleteObject"
    "fixture/DeleteObject.yaml"

requestGetChunk :: GetChunk -> TestTree
requestGetChunk =
  req
    "GetChunk"
    "fixture/GetChunk.yaml"

requestGetObjectMetadata :: GetObjectMetadata -> TestTree
requestGetObjectMetadata =
  req
    "GetObjectMetadata"
    "fixture/GetObjectMetadata.yaml"

requestListChunks :: ListChunks -> TestTree
requestListChunks =
  req
    "ListChunks"
    "fixture/ListChunks.yaml"

requestListObjects :: ListObjects -> TestTree
requestListObjects =
  req
    "ListObjects"
    "fixture/ListObjects.yaml"

requestStartObject :: StartObject -> TestTree
requestStartObject =
  req
    "StartObject"
    "fixture/StartObject.yaml"

-- Responses

responseDeleteObject :: DeleteObjectResponse -> TestTree
responseDeleteObject =
  res
    "DeleteObjectResponse"
    "fixture/DeleteObjectResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteObject)

responseListChunks :: ListChunksResponse -> TestTree
responseListChunks =
  res
    "ListChunksResponse"
    "fixture/ListChunksResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListChunks)

responseListObjects :: ListObjectsResponse -> TestTree
responseListObjects =
  res
    "ListObjectsResponse"
    "fixture/ListObjectsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListObjects)

responseNotifyObjectComplete :: NotifyObjectCompleteResponse -> TestTree
responseNotifyObjectComplete =
  res
    "NotifyObjectCompleteResponse"
    "fixture/NotifyObjectCompleteResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy NotifyObjectComplete)

responsePutChunk :: PutChunkResponse -> TestTree
responsePutChunk =
  res
    "PutChunkResponse"
    "fixture/PutChunkResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutChunk)

responsePutObject :: PutObjectResponse -> TestTree
responsePutObject =
  res
    "PutObjectResponse"
    "fixture/PutObjectResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutObject)

responseStartObject :: StartObjectResponse -> TestTree
responseStartObject =
  res
    "StartObjectResponse"
    "fixture/StartObjectResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartObject)
