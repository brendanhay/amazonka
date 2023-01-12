{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.KinesisVideoArchivedMedia
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.KinesisVideoArchivedMedia where

import Amazonka.KinesisVideoArchivedMedia
import qualified Data.Proxy as Proxy
import Test.Amazonka.Fixture
import Test.Amazonka.KinesisVideoArchivedMedia.Internal
import Test.Amazonka.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestGetClip $
--             newGetClip
--
--         , requestGetDASHStreamingSessionURL $
--             newGetDASHStreamingSessionURL
--
--         , requestGetHLSStreamingSessionURL $
--             newGetHLSStreamingSessionURL
--
--         , requestGetImages $
--             newGetImages
--
--         , requestGetMediaForFragmentList $
--             newGetMediaForFragmentList
--
--         , requestListFragments $
--             newListFragments
--
--           ]

--     , testGroup "response"
--         [ responseGetClip $
--             newGetClipResponse
--
--         , responseGetDASHStreamingSessionURL $
--             newGetDASHStreamingSessionURLResponse
--
--         , responseGetHLSStreamingSessionURL $
--             newGetHLSStreamingSessionURLResponse
--
--         , responseGetImages $
--             newGetImagesResponse
--
--         , responseGetMediaForFragmentList $
--             newGetMediaForFragmentListResponse
--
--         , responseListFragments $
--             newListFragmentsResponse
--
--           ]
--     ]

-- Requests

requestGetClip :: GetClip -> TestTree
requestGetClip =
  req
    "GetClip"
    "fixture/GetClip.yaml"

requestGetDASHStreamingSessionURL :: GetDASHStreamingSessionURL -> TestTree
requestGetDASHStreamingSessionURL =
  req
    "GetDASHStreamingSessionURL"
    "fixture/GetDASHStreamingSessionURL.yaml"

requestGetHLSStreamingSessionURL :: GetHLSStreamingSessionURL -> TestTree
requestGetHLSStreamingSessionURL =
  req
    "GetHLSStreamingSessionURL"
    "fixture/GetHLSStreamingSessionURL.yaml"

requestGetImages :: GetImages -> TestTree
requestGetImages =
  req
    "GetImages"
    "fixture/GetImages.yaml"

requestGetMediaForFragmentList :: GetMediaForFragmentList -> TestTree
requestGetMediaForFragmentList =
  req
    "GetMediaForFragmentList"
    "fixture/GetMediaForFragmentList.yaml"

requestListFragments :: ListFragments -> TestTree
requestListFragments =
  req
    "ListFragments"
    "fixture/ListFragments.yaml"

-- Responses

responseGetDASHStreamingSessionURL :: GetDASHStreamingSessionURLResponse -> TestTree
responseGetDASHStreamingSessionURL =
  res
    "GetDASHStreamingSessionURLResponse"
    "fixture/GetDASHStreamingSessionURLResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetDASHStreamingSessionURL)

responseGetHLSStreamingSessionURL :: GetHLSStreamingSessionURLResponse -> TestTree
responseGetHLSStreamingSessionURL =
  res
    "GetHLSStreamingSessionURLResponse"
    "fixture/GetHLSStreamingSessionURLResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetHLSStreamingSessionURL)

responseGetImages :: GetImagesResponse -> TestTree
responseGetImages =
  res
    "GetImagesResponse"
    "fixture/GetImagesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetImages)

responseListFragments :: ListFragmentsResponse -> TestTree
responseListFragments =
  res
    "ListFragmentsResponse"
    "fixture/ListFragmentsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListFragments)
