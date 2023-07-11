{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.ResourceExplorer2
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.ResourceExplorer2 where

import Amazonka.ResourceExplorer2
import qualified Data.Proxy as Proxy
import Test.Amazonka.Fixture
import Test.Amazonka.Prelude
import Test.Amazonka.ResourceExplorer2.Internal
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestAssociateDefaultView $
--             newAssociateDefaultView
--
--         , requestBatchGetView $
--             newBatchGetView
--
--         , requestCreateIndex $
--             newCreateIndex
--
--         , requestCreateView $
--             newCreateView
--
--         , requestDeleteIndex $
--             newDeleteIndex
--
--         , requestDeleteView $
--             newDeleteView
--
--         , requestDisassociateDefaultView $
--             newDisassociateDefaultView
--
--         , requestGetDefaultView $
--             newGetDefaultView
--
--         , requestGetIndex $
--             newGetIndex
--
--         , requestGetView $
--             newGetView
--
--         , requestListIndexes $
--             newListIndexes
--
--         , requestListSupportedResourceTypes $
--             newListSupportedResourceTypes
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestListViews $
--             newListViews
--
--         , requestSearch $
--             newSearch
--
--         , requestTagResource $
--             newTagResource
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestUpdateIndexType $
--             newUpdateIndexType
--
--         , requestUpdateView $
--             newUpdateView
--
--           ]

--     , testGroup "response"
--         [ responseAssociateDefaultView $
--             newAssociateDefaultViewResponse
--
--         , responseBatchGetView $
--             newBatchGetViewResponse
--
--         , responseCreateIndex $
--             newCreateIndexResponse
--
--         , responseCreateView $
--             newCreateViewResponse
--
--         , responseDeleteIndex $
--             newDeleteIndexResponse
--
--         , responseDeleteView $
--             newDeleteViewResponse
--
--         , responseDisassociateDefaultView $
--             newDisassociateDefaultViewResponse
--
--         , responseGetDefaultView $
--             newGetDefaultViewResponse
--
--         , responseGetIndex $
--             newGetIndexResponse
--
--         , responseGetView $
--             newGetViewResponse
--
--         , responseListIndexes $
--             newListIndexesResponse
--
--         , responseListSupportedResourceTypes $
--             newListSupportedResourceTypesResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseListViews $
--             newListViewsResponse
--
--         , responseSearch $
--             newSearchResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseUpdateIndexType $
--             newUpdateIndexTypeResponse
--
--         , responseUpdateView $
--             newUpdateViewResponse
--
--           ]
--     ]

-- Requests

requestAssociateDefaultView :: AssociateDefaultView -> TestTree
requestAssociateDefaultView =
  req
    "AssociateDefaultView"
    "fixture/AssociateDefaultView.yaml"

requestBatchGetView :: BatchGetView -> TestTree
requestBatchGetView =
  req
    "BatchGetView"
    "fixture/BatchGetView.yaml"

requestCreateIndex :: CreateIndex -> TestTree
requestCreateIndex =
  req
    "CreateIndex"
    "fixture/CreateIndex.yaml"

requestCreateView :: CreateView -> TestTree
requestCreateView =
  req
    "CreateView"
    "fixture/CreateView.yaml"

requestDeleteIndex :: DeleteIndex -> TestTree
requestDeleteIndex =
  req
    "DeleteIndex"
    "fixture/DeleteIndex.yaml"

requestDeleteView :: DeleteView -> TestTree
requestDeleteView =
  req
    "DeleteView"
    "fixture/DeleteView.yaml"

requestDisassociateDefaultView :: DisassociateDefaultView -> TestTree
requestDisassociateDefaultView =
  req
    "DisassociateDefaultView"
    "fixture/DisassociateDefaultView.yaml"

requestGetDefaultView :: GetDefaultView -> TestTree
requestGetDefaultView =
  req
    "GetDefaultView"
    "fixture/GetDefaultView.yaml"

requestGetIndex :: GetIndex -> TestTree
requestGetIndex =
  req
    "GetIndex"
    "fixture/GetIndex.yaml"

requestGetView :: GetView -> TestTree
requestGetView =
  req
    "GetView"
    "fixture/GetView.yaml"

requestListIndexes :: ListIndexes -> TestTree
requestListIndexes =
  req
    "ListIndexes"
    "fixture/ListIndexes.yaml"

requestListSupportedResourceTypes :: ListSupportedResourceTypes -> TestTree
requestListSupportedResourceTypes =
  req
    "ListSupportedResourceTypes"
    "fixture/ListSupportedResourceTypes.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestListViews :: ListViews -> TestTree
requestListViews =
  req
    "ListViews"
    "fixture/ListViews.yaml"

requestSearch :: Search -> TestTree
requestSearch =
  req
    "Search"
    "fixture/Search.yaml"

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

requestUpdateIndexType :: UpdateIndexType -> TestTree
requestUpdateIndexType =
  req
    "UpdateIndexType"
    "fixture/UpdateIndexType.yaml"

requestUpdateView :: UpdateView -> TestTree
requestUpdateView =
  req
    "UpdateView"
    "fixture/UpdateView.yaml"

-- Responses

responseAssociateDefaultView :: AssociateDefaultViewResponse -> TestTree
responseAssociateDefaultView =
  res
    "AssociateDefaultViewResponse"
    "fixture/AssociateDefaultViewResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssociateDefaultView)

responseBatchGetView :: BatchGetViewResponse -> TestTree
responseBatchGetView =
  res
    "BatchGetViewResponse"
    "fixture/BatchGetViewResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchGetView)

responseCreateIndex :: CreateIndexResponse -> TestTree
responseCreateIndex =
  res
    "CreateIndexResponse"
    "fixture/CreateIndexResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateIndex)

responseCreateView :: CreateViewResponse -> TestTree
responseCreateView =
  res
    "CreateViewResponse"
    "fixture/CreateViewResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateView)

responseDeleteIndex :: DeleteIndexResponse -> TestTree
responseDeleteIndex =
  res
    "DeleteIndexResponse"
    "fixture/DeleteIndexResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteIndex)

responseDeleteView :: DeleteViewResponse -> TestTree
responseDeleteView =
  res
    "DeleteViewResponse"
    "fixture/DeleteViewResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteView)

responseDisassociateDefaultView :: DisassociateDefaultViewResponse -> TestTree
responseDisassociateDefaultView =
  res
    "DisassociateDefaultViewResponse"
    "fixture/DisassociateDefaultViewResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisassociateDefaultView)

responseGetDefaultView :: GetDefaultViewResponse -> TestTree
responseGetDefaultView =
  res
    "GetDefaultViewResponse"
    "fixture/GetDefaultViewResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetDefaultView)

responseGetIndex :: GetIndexResponse -> TestTree
responseGetIndex =
  res
    "GetIndexResponse"
    "fixture/GetIndexResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetIndex)

responseGetView :: GetViewResponse -> TestTree
responseGetView =
  res
    "GetViewResponse"
    "fixture/GetViewResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetView)

responseListIndexes :: ListIndexesResponse -> TestTree
responseListIndexes =
  res
    "ListIndexesResponse"
    "fixture/ListIndexesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListIndexes)

responseListSupportedResourceTypes :: ListSupportedResourceTypesResponse -> TestTree
responseListSupportedResourceTypes =
  res
    "ListSupportedResourceTypesResponse"
    "fixture/ListSupportedResourceTypesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListSupportedResourceTypes)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responseListViews :: ListViewsResponse -> TestTree
responseListViews =
  res
    "ListViewsResponse"
    "fixture/ListViewsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListViews)

responseSearch :: SearchResponse -> TestTree
responseSearch =
  res
    "SearchResponse"
    "fixture/SearchResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy Search)

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

responseUpdateIndexType :: UpdateIndexTypeResponse -> TestTree
responseUpdateIndexType =
  res
    "UpdateIndexTypeResponse"
    "fixture/UpdateIndexTypeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateIndexType)

responseUpdateView :: UpdateViewResponse -> TestTree
responseUpdateView =
  res
    "UpdateViewResponse"
    "fixture/UpdateViewResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateView)
