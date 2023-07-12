{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.MediaStoreData
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.MediaStoreData where

import Amazonka.MediaStoreData
import qualified Data.Proxy as Proxy
import Test.Amazonka.Fixture
import Test.Amazonka.MediaStoreData.Internal
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
--         , requestDescribeObject $
--             newDescribeObject
--
--         , requestGetObject $
--             newGetObject
--
--         , requestListItems $
--             newListItems
--
--         , requestPutObject $
--             newPutObject
--
--           ]

--     , testGroup "response"
--         [ responseDeleteObject $
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
--         , responsePutObject $
--             newPutObjectResponse
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

responsePutObject :: PutObjectResponse -> TestTree
responsePutObject =
  res
    "PutObjectResponse"
    "fixture/PutObjectResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutObject)
