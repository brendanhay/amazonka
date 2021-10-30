{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.MediaStoreData
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.AWS.Gen.MediaStoreData where

import qualified Data.Proxy as Proxy
import Network.AWS.MediaStoreData
import Test.AWS.Fixture
import Test.AWS.MediaStoreData.Internal
import Test.AWS.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestPutObject $
--             newPutObject
--
--         , requestDeleteObject $
--             newDeleteObject
--
--         , requestDescribeObject $
--             newDescribeObject
--
--         , requestGetObject $
--             newGetObject
--
--         , requestListItems $
--             newListItems
--
--           ]

--     , testGroup "response"
--         [ responsePutObject $
--             newPutObjectResponse
--
--         , responseDeleteObject $
--             newDeleteObjectResponse
--
--         , responseDescribeObject $
--             newDescribeObjectResponse
--
--         , responseGetObject $
--             newGetObjectResponse
--
--         , responseListItems $
--             newListItemsResponse
--
--           ]
--     ]

-- Requests

requestDeleteObject :: DeleteObject -> TestTree
requestDeleteObject =
  req
    "DeleteObject"
    "fixture/DeleteObject.yaml"

requestDescribeObject :: DescribeObject -> TestTree
requestDescribeObject =
  req
    "DescribeObject"
    "fixture/DescribeObject.yaml"

requestGetObject :: GetObject -> TestTree
requestGetObject =
  req
    "GetObject"
    "fixture/GetObject.yaml"

requestListItems :: ListItems -> TestTree
requestListItems =
  req
    "ListItems"
    "fixture/ListItems.yaml"

-- Responses

responsePutObject :: PutObjectResponse -> TestTree
responsePutObject =
  res
    "PutObjectResponse"
    "fixture/PutObjectResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutObject)

responseDeleteObject :: DeleteObjectResponse -> TestTree
responseDeleteObject =
  res
    "DeleteObjectResponse"
    "fixture/DeleteObjectResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteObject)

responseDescribeObject :: DescribeObjectResponse -> TestTree
responseDescribeObject =
  res
    "DescribeObjectResponse"
    "fixture/DescribeObjectResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeObject)

responseListItems :: ListItemsResponse -> TestTree
responseListItems =
  res
    "ListItemsResponse"
    "fixture/ListItemsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListItems)
