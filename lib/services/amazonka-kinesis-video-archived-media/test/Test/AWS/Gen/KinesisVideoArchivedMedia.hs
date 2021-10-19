{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.KinesisVideoArchivedMedia
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.AWS.Gen.KinesisVideoArchivedMedia where

import Data.Proxy
import Network.AWS.KinesisVideoArchivedMedia
import Test.AWS.Fixture
import Test.AWS.KinesisVideoArchivedMedia.Internal
import Test.AWS.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestGetHLSStreamingSessionURL $
--             newGetHLSStreamingSessionURL
--
--         , requestGetClip $
--             newGetClip
--
--         , requestGetMediaForFragmentList $
--             newGetMediaForFragmentList
--
--         , requestListFragments $
--             newListFragments
--
--         , requestGetDASHStreamingSessionURL $
--             newGetDASHStreamingSessionURL
--
--           ]

--     , testGroup "response"
--         [ responseGetHLSStreamingSessionURL $
--             newGetHLSStreamingSessionURLResponse
--
--         , responseGetClip $
--             newGetClipResponse
--
--         , responseGetMediaForFragmentList $
--             newGetMediaForFragmentListResponse
--
--         , responseListFragments $
--             newListFragmentsResponse
--
--         , responseGetDASHStreamingSessionURL $
--             newGetDASHStreamingSessionURLResponse
--
--           ]
--     ]

-- Requests

requestGetHLSStreamingSessionURL :: GetHLSStreamingSessionURL -> TestTree
requestGetHLSStreamingSessionURL =
  req
    "GetHLSStreamingSessionURL"
    "fixture/GetHLSStreamingSessionURL.yaml"

requestGetClip :: GetClip -> TestTree
requestGetClip =
  req
    "GetClip"
    "fixture/GetClip.yaml"

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

requestGetDASHStreamingSessionURL :: GetDASHStreamingSessionURL -> TestTree
requestGetDASHStreamingSessionURL =
  req
    "GetDASHStreamingSessionURL"
    "fixture/GetDASHStreamingSessionURL.yaml"

-- Responses

responseGetHLSStreamingSessionURL :: GetHLSStreamingSessionURLResponse -> TestTree
responseGetHLSStreamingSessionURL =
  res
    "GetHLSStreamingSessionURLResponse"
    "fixture/GetHLSStreamingSessionURLResponse.proto"
    defaultService
    (Proxy :: Proxy GetHLSStreamingSessionURL)

responseListFragments :: ListFragmentsResponse -> TestTree
responseListFragments =
  res
    "ListFragmentsResponse"
    "fixture/ListFragmentsResponse.proto"
    defaultService
    (Proxy :: Proxy ListFragments)

responseGetDASHStreamingSessionURL :: GetDASHStreamingSessionURLResponse -> TestTree
responseGetDASHStreamingSessionURL =
  res
    "GetDASHStreamingSessionURLResponse"
    "fixture/GetDASHStreamingSessionURLResponse.proto"
    defaultService
    (Proxy :: Proxy GetDASHStreamingSessionURL)
