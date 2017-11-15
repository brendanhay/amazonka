{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.Rekognition
-- Copyright   : (c) 2013-2017 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Test.AWS.Gen.Rekognition where

import Data.Proxy
import Network.AWS.Rekognition
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.AWS.Rekognition.Internal
import Test.Tasty

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
--         , requestRecognizeCelebrities $
--             recognizeCelebrities
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
--         , requestGetCelebrityInfo $
--             getCelebrityInfo
--
--         , requestDetectModerationLabels $
--             detectModerationLabels
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
--         , responseRecognizeCelebrities $
--             recognizeCelebritiesResponse
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
--         , responseGetCelebrityInfo $
--             getCelebrityInfoResponse
--
--         , responseDetectModerationLabels $
--             detectModerationLabelsResponse
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

requestRecognizeCelebrities :: RecognizeCelebrities -> TestTree
requestRecognizeCelebrities = req
    "RecognizeCelebrities"
    "fixture/RecognizeCelebrities.yaml"

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

requestGetCelebrityInfo :: GetCelebrityInfo -> TestTree
requestGetCelebrityInfo = req
    "GetCelebrityInfo"
    "fixture/GetCelebrityInfo.yaml"

requestDetectModerationLabels :: DetectModerationLabels -> TestTree
requestDetectModerationLabels = req
    "DetectModerationLabels"
    "fixture/DetectModerationLabels.yaml"

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

responseRecognizeCelebrities :: RecognizeCelebritiesResponse -> TestTree
responseRecognizeCelebrities = res
    "RecognizeCelebritiesResponse"
    "fixture/RecognizeCelebritiesResponse.proto"
    rekognition
    (Proxy :: Proxy RecognizeCelebrities)

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

responseGetCelebrityInfo :: GetCelebrityInfoResponse -> TestTree
responseGetCelebrityInfo = res
    "GetCelebrityInfoResponse"
    "fixture/GetCelebrityInfoResponse.proto"
    rekognition
    (Proxy :: Proxy GetCelebrityInfo)

responseDetectModerationLabels :: DetectModerationLabelsResponse -> TestTree
responseDetectModerationLabels = res
    "DetectModerationLabelsResponse"
    "fixture/DetectModerationLabelsResponse.proto"
    rekognition
    (Proxy :: Proxy DetectModerationLabels)
