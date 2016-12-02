{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.Rekognition
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Test.AWS.Gen.Rekognition where

import Data.Proxy
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.Tasty
import Network.AWS.Rekognition
import Test.AWS.Rekognition.Internal

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestListCollections $
--             listCollections
--
--         , requestDeleteCollection $
--             deleteCollection
--
--         , requestCreateCollection $
--             createCollection
--
--         , requestDetectLabels $
--             detectLabels
--
--         , requestSearchFacesByImage $
--             searchFacesByImage
--
--         , requestSearchFaces $
--             searchFaces
--
--         , requestIndexFaces $
--             indexFaces
--
--         , requestCompareFaces $
--             compareFaces
--
--         , requestDetectFaces $
--             detectFaces
--
--         , requestListFaces $
--             listFaces
--
--         , requestDeleteFaces $
--             deleteFaces
--
--           ]

--     , testGroup "response"
--         [ responseListCollections $
--             listCollectionsResponse
--
--         , responseDeleteCollection $
--             deleteCollectionResponse
--
--         , responseCreateCollection $
--             createCollectionResponse
--
--         , responseDetectLabels $
--             detectLabelsResponse
--
--         , responseSearchFacesByImage $
--             searchFacesByImageResponse
--
--         , responseSearchFaces $
--             searchFacesResponse
--
--         , responseIndexFaces $
--             indexFacesResponse
--
--         , responseCompareFaces $
--             compareFacesResponse
--
--         , responseDetectFaces $
--             detectFacesResponse
--
--         , responseListFaces $
--             listFacesResponse
--
--         , responseDeleteFaces $
--             deleteFacesResponse
--
--           ]
--     ]

-- Requests

requestListCollections :: ListCollections -> TestTree
requestListCollections = req
    "ListCollections"
    "fixture/ListCollections.yaml"

requestDeleteCollection :: DeleteCollection -> TestTree
requestDeleteCollection = req
    "DeleteCollection"
    "fixture/DeleteCollection.yaml"

requestCreateCollection :: CreateCollection -> TestTree
requestCreateCollection = req
    "CreateCollection"
    "fixture/CreateCollection.yaml"

requestDetectLabels :: DetectLabels -> TestTree
requestDetectLabels = req
    "DetectLabels"
    "fixture/DetectLabels.yaml"

requestSearchFacesByImage :: SearchFacesByImage -> TestTree
requestSearchFacesByImage = req
    "SearchFacesByImage"
    "fixture/SearchFacesByImage.yaml"

requestSearchFaces :: SearchFaces -> TestTree
requestSearchFaces = req
    "SearchFaces"
    "fixture/SearchFaces.yaml"

requestIndexFaces :: IndexFaces -> TestTree
requestIndexFaces = req
    "IndexFaces"
    "fixture/IndexFaces.yaml"

requestCompareFaces :: CompareFaces -> TestTree
requestCompareFaces = req
    "CompareFaces"
    "fixture/CompareFaces.yaml"

requestDetectFaces :: DetectFaces -> TestTree
requestDetectFaces = req
    "DetectFaces"
    "fixture/DetectFaces.yaml"

requestListFaces :: ListFaces -> TestTree
requestListFaces = req
    "ListFaces"
    "fixture/ListFaces.yaml"

requestDeleteFaces :: DeleteFaces -> TestTree
requestDeleteFaces = req
    "DeleteFaces"
    "fixture/DeleteFaces.yaml"

-- Responses

responseListCollections :: ListCollectionsResponse -> TestTree
responseListCollections = res
    "ListCollectionsResponse"
    "fixture/ListCollectionsResponse.proto"
    rekognition
    (Proxy :: Proxy ListCollections)

responseDeleteCollection :: DeleteCollectionResponse -> TestTree
responseDeleteCollection = res
    "DeleteCollectionResponse"
    "fixture/DeleteCollectionResponse.proto"
    rekognition
    (Proxy :: Proxy DeleteCollection)

responseCreateCollection :: CreateCollectionResponse -> TestTree
responseCreateCollection = res
    "CreateCollectionResponse"
    "fixture/CreateCollectionResponse.proto"
    rekognition
    (Proxy :: Proxy CreateCollection)

responseDetectLabels :: DetectLabelsResponse -> TestTree
responseDetectLabels = res
    "DetectLabelsResponse"
    "fixture/DetectLabelsResponse.proto"
    rekognition
    (Proxy :: Proxy DetectLabels)

responseSearchFacesByImage :: SearchFacesByImageResponse -> TestTree
responseSearchFacesByImage = res
    "SearchFacesByImageResponse"
    "fixture/SearchFacesByImageResponse.proto"
    rekognition
    (Proxy :: Proxy SearchFacesByImage)

responseSearchFaces :: SearchFacesResponse -> TestTree
responseSearchFaces = res
    "SearchFacesResponse"
    "fixture/SearchFacesResponse.proto"
    rekognition
    (Proxy :: Proxy SearchFaces)

responseIndexFaces :: IndexFacesResponse -> TestTree
responseIndexFaces = res
    "IndexFacesResponse"
    "fixture/IndexFacesResponse.proto"
    rekognition
    (Proxy :: Proxy IndexFaces)

responseCompareFaces :: CompareFacesResponse -> TestTree
responseCompareFaces = res
    "CompareFacesResponse"
    "fixture/CompareFacesResponse.proto"
    rekognition
    (Proxy :: Proxy CompareFaces)

responseDetectFaces :: DetectFacesResponse -> TestTree
responseDetectFaces = res
    "DetectFacesResponse"
    "fixture/DetectFacesResponse.proto"
    rekognition
    (Proxy :: Proxy DetectFaces)

responseListFaces :: ListFacesResponse -> TestTree
responseListFaces = res
    "ListFacesResponse"
    "fixture/ListFacesResponse.proto"
    rekognition
    (Proxy :: Proxy ListFaces)

responseDeleteFaces :: DeleteFacesResponse -> TestTree
responseDeleteFaces = res
    "DeleteFacesResponse"
    "fixture/DeleteFacesResponse.proto"
    rekognition
    (Proxy :: Proxy DeleteFaces)
