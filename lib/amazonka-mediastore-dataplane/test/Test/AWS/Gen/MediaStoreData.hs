{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.MediaStoreData
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Test.AWS.Gen.MediaStoreData where

import Data.Proxy
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.Tasty
import Network.AWS.MediaStoreData
import Test.AWS.MediaStoreData.Internal

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestPutObject $
--             mkPutObject
--
--         , requestDeleteObject $
--             mkDeleteObject
--
--         , requestDescribeObject $
--             mkDescribeObject
--
--         , requestGetObject $
--             mkGetObject
--
--         , requestListItems $
--             mkListItems
--
--           ]

--     , testGroup "response"
--         [ responsePutObject $
--             mkPutObjectResponse
--
--         , responseDeleteObject $
--             mkDeleteObjectResponse
--
--         , responseDescribeObject $
--             mkDescribeObjectResponse
--
--         , responseGetObject $
--             mkGetObjectResponse
--
--         , responseListItems $
--             mkListItemsResponse
--
--           ]
--     ]

-- Requests

requestDeleteObject :: DeleteObject -> TestTree
requestDeleteObject = req
    "DeleteObject"
    "fixture/DeleteObject.yaml"

requestDescribeObject :: DescribeObject -> TestTree
requestDescribeObject = req
    "DescribeObject"
    "fixture/DescribeObject.yaml"

requestGetObject :: GetObject -> TestTree
requestGetObject = req
    "GetObject"
    "fixture/GetObject.yaml"

requestListItems :: ListItems -> TestTree
requestListItems = req
    "ListItems"
    "fixture/ListItems.yaml"

-- Responses

responsePutObject :: PutObjectResponse -> TestTree
responsePutObject = res
    "PutObjectResponse"
    "fixture/PutObjectResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy PutObject)

responseDeleteObject :: DeleteObjectResponse -> TestTree
responseDeleteObject = res
    "DeleteObjectResponse"
    "fixture/DeleteObjectResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteObject)

responseDescribeObject :: DescribeObjectResponse -> TestTree
responseDescribeObject = res
    "DescribeObjectResponse"
    "fixture/DescribeObjectResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeObject)

responseListItems :: ListItemsResponse -> TestTree
responseListItems = res
    "ListItemsResponse"
    "fixture/ListItemsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListItems)
