{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.KeySpaces
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.KeySpaces where

import Amazonka.KeySpaces
import qualified Data.Proxy as Proxy
import Test.Amazonka.Fixture
import Test.Amazonka.KeySpaces.Internal
import Test.Amazonka.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestCreateKeyspace $
--             newCreateKeyspace
--
--         , requestCreateTable $
--             newCreateTable
--
--         , requestDeleteKeyspace $
--             newDeleteKeyspace
--
--         , requestDeleteTable $
--             newDeleteTable
--
--         , requestGetKeyspace $
--             newGetKeyspace
--
--         , requestGetTable $
--             newGetTable
--
--         , requestListKeyspaces $
--             newListKeyspaces
--
--         , requestListTables $
--             newListTables
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestRestoreTable $
--             newRestoreTable
--
--         , requestTagResource $
--             newTagResource
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestUpdateTable $
--             newUpdateTable
--
--           ]

--     , testGroup "response"
--         [ responseCreateKeyspace $
--             newCreateKeyspaceResponse
--
--         , responseCreateTable $
--             newCreateTableResponse
--
--         , responseDeleteKeyspace $
--             newDeleteKeyspaceResponse
--
--         , responseDeleteTable $
--             newDeleteTableResponse
--
--         , responseGetKeyspace $
--             newGetKeyspaceResponse
--
--         , responseGetTable $
--             newGetTableResponse
--
--         , responseListKeyspaces $
--             newListKeyspacesResponse
--
--         , responseListTables $
--             newListTablesResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseRestoreTable $
--             newRestoreTableResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseUpdateTable $
--             newUpdateTableResponse
--
--           ]
--     ]

-- Requests

requestCreateKeyspace :: CreateKeyspace -> TestTree
requestCreateKeyspace =
  req
    "CreateKeyspace"
    "fixture/CreateKeyspace.yaml"

requestCreateTable :: CreateTable -> TestTree
requestCreateTable =
  req
    "CreateTable"
    "fixture/CreateTable.yaml"

requestDeleteKeyspace :: DeleteKeyspace -> TestTree
requestDeleteKeyspace =
  req
    "DeleteKeyspace"
    "fixture/DeleteKeyspace.yaml"

requestDeleteTable :: DeleteTable -> TestTree
requestDeleteTable =
  req
    "DeleteTable"
    "fixture/DeleteTable.yaml"

requestGetKeyspace :: GetKeyspace -> TestTree
requestGetKeyspace =
  req
    "GetKeyspace"
    "fixture/GetKeyspace.yaml"

requestGetTable :: GetTable -> TestTree
requestGetTable =
  req
    "GetTable"
    "fixture/GetTable.yaml"

requestListKeyspaces :: ListKeyspaces -> TestTree
requestListKeyspaces =
  req
    "ListKeyspaces"
    "fixture/ListKeyspaces.yaml"

requestListTables :: ListTables -> TestTree
requestListTables =
  req
    "ListTables"
    "fixture/ListTables.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestRestoreTable :: RestoreTable -> TestTree
requestRestoreTable =
  req
    "RestoreTable"
    "fixture/RestoreTable.yaml"

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

requestUpdateTable :: UpdateTable -> TestTree
requestUpdateTable =
  req
    "UpdateTable"
    "fixture/UpdateTable.yaml"

-- Responses

responseCreateKeyspace :: CreateKeyspaceResponse -> TestTree
responseCreateKeyspace =
  res
    "CreateKeyspaceResponse"
    "fixture/CreateKeyspaceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateKeyspace)

responseCreateTable :: CreateTableResponse -> TestTree
responseCreateTable =
  res
    "CreateTableResponse"
    "fixture/CreateTableResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateTable)

responseDeleteKeyspace :: DeleteKeyspaceResponse -> TestTree
responseDeleteKeyspace =
  res
    "DeleteKeyspaceResponse"
    "fixture/DeleteKeyspaceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteKeyspace)

responseDeleteTable :: DeleteTableResponse -> TestTree
responseDeleteTable =
  res
    "DeleteTableResponse"
    "fixture/DeleteTableResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteTable)

responseGetKeyspace :: GetKeyspaceResponse -> TestTree
responseGetKeyspace =
  res
    "GetKeyspaceResponse"
    "fixture/GetKeyspaceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetKeyspace)

responseGetTable :: GetTableResponse -> TestTree
responseGetTable =
  res
    "GetTableResponse"
    "fixture/GetTableResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetTable)

responseListKeyspaces :: ListKeyspacesResponse -> TestTree
responseListKeyspaces =
  res
    "ListKeyspacesResponse"
    "fixture/ListKeyspacesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListKeyspaces)

responseListTables :: ListTablesResponse -> TestTree
responseListTables =
  res
    "ListTablesResponse"
    "fixture/ListTablesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTables)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responseRestoreTable :: RestoreTableResponse -> TestTree
responseRestoreTable =
  res
    "RestoreTableResponse"
    "fixture/RestoreTableResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RestoreTable)

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

responseUpdateTable :: UpdateTableResponse -> TestTree
responseUpdateTable =
  res
    "UpdateTableResponse"
    "fixture/UpdateTableResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateTable)
