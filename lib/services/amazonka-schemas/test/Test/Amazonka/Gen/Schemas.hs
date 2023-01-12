{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.Schemas
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.Schemas where

import Amazonka.Schemas
import qualified Data.Proxy as Proxy
import Test.Amazonka.Fixture
import Test.Amazonka.Prelude
import Test.Amazonka.Schemas.Internal
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestCreateDiscoverer $
--             newCreateDiscoverer
--
--         , requestCreateRegistry $
--             newCreateRegistry
--
--         , requestCreateSchema $
--             newCreateSchema
--
--         , requestDeleteDiscoverer $
--             newDeleteDiscoverer
--
--         , requestDeleteRegistry $
--             newDeleteRegistry
--
--         , requestDeleteResourcePolicy $
--             newDeleteResourcePolicy
--
--         , requestDeleteSchema $
--             newDeleteSchema
--
--         , requestDeleteSchemaVersion $
--             newDeleteSchemaVersion
--
--         , requestDescribeCodeBinding $
--             newDescribeCodeBinding
--
--         , requestDescribeDiscoverer $
--             newDescribeDiscoverer
--
--         , requestDescribeRegistry $
--             newDescribeRegistry
--
--         , requestDescribeSchema $
--             newDescribeSchema
--
--         , requestExportSchema $
--             newExportSchema
--
--         , requestGetCodeBindingSource $
--             newGetCodeBindingSource
--
--         , requestGetDiscoveredSchema $
--             newGetDiscoveredSchema
--
--         , requestGetResourcePolicy $
--             newGetResourcePolicy
--
--         , requestListDiscoverers $
--             newListDiscoverers
--
--         , requestListRegistries $
--             newListRegistries
--
--         , requestListSchemaVersions $
--             newListSchemaVersions
--
--         , requestListSchemas $
--             newListSchemas
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestPutCodeBinding $
--             newPutCodeBinding
--
--         , requestPutResourcePolicy $
--             newPutResourcePolicy
--
--         , requestSearchSchemas $
--             newSearchSchemas
--
--         , requestStartDiscoverer $
--             newStartDiscoverer
--
--         , requestStopDiscoverer $
--             newStopDiscoverer
--
--         , requestTagResource $
--             newTagResource
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestUpdateDiscoverer $
--             newUpdateDiscoverer
--
--         , requestUpdateRegistry $
--             newUpdateRegistry
--
--         , requestUpdateSchema $
--             newUpdateSchema
--
--           ]

--     , testGroup "response"
--         [ responseCreateDiscoverer $
--             newCreateDiscovererResponse
--
--         , responseCreateRegistry $
--             newCreateRegistryResponse
--
--         , responseCreateSchema $
--             newCreateSchemaResponse
--
--         , responseDeleteDiscoverer $
--             newDeleteDiscovererResponse
--
--         , responseDeleteRegistry $
--             newDeleteRegistryResponse
--
--         , responseDeleteResourcePolicy $
--             newDeleteResourcePolicyResponse
--
--         , responseDeleteSchema $
--             newDeleteSchemaResponse
--
--         , responseDeleteSchemaVersion $
--             newDeleteSchemaVersionResponse
--
--         , responseDescribeCodeBinding $
--             newDescribeCodeBindingResponse
--
--         , responseDescribeDiscoverer $
--             newDescribeDiscovererResponse
--
--         , responseDescribeRegistry $
--             newDescribeRegistryResponse
--
--         , responseDescribeSchema $
--             newDescribeSchemaResponse
--
--         , responseExportSchema $
--             newExportSchemaResponse
--
--         , responseGetCodeBindingSource $
--             newGetCodeBindingSourceResponse
--
--         , responseGetDiscoveredSchema $
--             newGetDiscoveredSchemaResponse
--
--         , responseGetResourcePolicy $
--             newGetResourcePolicyResponse
--
--         , responseListDiscoverers $
--             newListDiscoverersResponse
--
--         , responseListRegistries $
--             newListRegistriesResponse
--
--         , responseListSchemaVersions $
--             newListSchemaVersionsResponse
--
--         , responseListSchemas $
--             newListSchemasResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responsePutCodeBinding $
--             newPutCodeBindingResponse
--
--         , responsePutResourcePolicy $
--             newPutResourcePolicyResponse
--
--         , responseSearchSchemas $
--             newSearchSchemasResponse
--
--         , responseStartDiscoverer $
--             newStartDiscovererResponse
--
--         , responseStopDiscoverer $
--             newStopDiscovererResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseUpdateDiscoverer $
--             newUpdateDiscovererResponse
--
--         , responseUpdateRegistry $
--             newUpdateRegistryResponse
--
--         , responseUpdateSchema $
--             newUpdateSchemaResponse
--
--           ]
--     ]

-- Requests

requestCreateDiscoverer :: CreateDiscoverer -> TestTree
requestCreateDiscoverer =
  req
    "CreateDiscoverer"
    "fixture/CreateDiscoverer.yaml"

requestCreateRegistry :: CreateRegistry -> TestTree
requestCreateRegistry =
  req
    "CreateRegistry"
    "fixture/CreateRegistry.yaml"

requestCreateSchema :: CreateSchema -> TestTree
requestCreateSchema =
  req
    "CreateSchema"
    "fixture/CreateSchema.yaml"

requestDeleteDiscoverer :: DeleteDiscoverer -> TestTree
requestDeleteDiscoverer =
  req
    "DeleteDiscoverer"
    "fixture/DeleteDiscoverer.yaml"

requestDeleteRegistry :: DeleteRegistry -> TestTree
requestDeleteRegistry =
  req
    "DeleteRegistry"
    "fixture/DeleteRegistry.yaml"

requestDeleteResourcePolicy :: DeleteResourcePolicy -> TestTree
requestDeleteResourcePolicy =
  req
    "DeleteResourcePolicy"
    "fixture/DeleteResourcePolicy.yaml"

requestDeleteSchema :: DeleteSchema -> TestTree
requestDeleteSchema =
  req
    "DeleteSchema"
    "fixture/DeleteSchema.yaml"

requestDeleteSchemaVersion :: DeleteSchemaVersion -> TestTree
requestDeleteSchemaVersion =
  req
    "DeleteSchemaVersion"
    "fixture/DeleteSchemaVersion.yaml"

requestDescribeCodeBinding :: DescribeCodeBinding -> TestTree
requestDescribeCodeBinding =
  req
    "DescribeCodeBinding"
    "fixture/DescribeCodeBinding.yaml"

requestDescribeDiscoverer :: DescribeDiscoverer -> TestTree
requestDescribeDiscoverer =
  req
    "DescribeDiscoverer"
    "fixture/DescribeDiscoverer.yaml"

requestDescribeRegistry :: DescribeRegistry -> TestTree
requestDescribeRegistry =
  req
    "DescribeRegistry"
    "fixture/DescribeRegistry.yaml"

requestDescribeSchema :: DescribeSchema -> TestTree
requestDescribeSchema =
  req
    "DescribeSchema"
    "fixture/DescribeSchema.yaml"

requestExportSchema :: ExportSchema -> TestTree
requestExportSchema =
  req
    "ExportSchema"
    "fixture/ExportSchema.yaml"

requestGetCodeBindingSource :: GetCodeBindingSource -> TestTree
requestGetCodeBindingSource =
  req
    "GetCodeBindingSource"
    "fixture/GetCodeBindingSource.yaml"

requestGetDiscoveredSchema :: GetDiscoveredSchema -> TestTree
requestGetDiscoveredSchema =
  req
    "GetDiscoveredSchema"
    "fixture/GetDiscoveredSchema.yaml"

requestGetResourcePolicy :: GetResourcePolicy -> TestTree
requestGetResourcePolicy =
  req
    "GetResourcePolicy"
    "fixture/GetResourcePolicy.yaml"

requestListDiscoverers :: ListDiscoverers -> TestTree
requestListDiscoverers =
  req
    "ListDiscoverers"
    "fixture/ListDiscoverers.yaml"

requestListRegistries :: ListRegistries -> TestTree
requestListRegistries =
  req
    "ListRegistries"
    "fixture/ListRegistries.yaml"

requestListSchemaVersions :: ListSchemaVersions -> TestTree
requestListSchemaVersions =
  req
    "ListSchemaVersions"
    "fixture/ListSchemaVersions.yaml"

requestListSchemas :: ListSchemas -> TestTree
requestListSchemas =
  req
    "ListSchemas"
    "fixture/ListSchemas.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestPutCodeBinding :: PutCodeBinding -> TestTree
requestPutCodeBinding =
  req
    "PutCodeBinding"
    "fixture/PutCodeBinding.yaml"

requestPutResourcePolicy :: PutResourcePolicy -> TestTree
requestPutResourcePolicy =
  req
    "PutResourcePolicy"
    "fixture/PutResourcePolicy.yaml"

requestSearchSchemas :: SearchSchemas -> TestTree
requestSearchSchemas =
  req
    "SearchSchemas"
    "fixture/SearchSchemas.yaml"

requestStartDiscoverer :: StartDiscoverer -> TestTree
requestStartDiscoverer =
  req
    "StartDiscoverer"
    "fixture/StartDiscoverer.yaml"

requestStopDiscoverer :: StopDiscoverer -> TestTree
requestStopDiscoverer =
  req
    "StopDiscoverer"
    "fixture/StopDiscoverer.yaml"

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

requestUpdateDiscoverer :: UpdateDiscoverer -> TestTree
requestUpdateDiscoverer =
  req
    "UpdateDiscoverer"
    "fixture/UpdateDiscoverer.yaml"

requestUpdateRegistry :: UpdateRegistry -> TestTree
requestUpdateRegistry =
  req
    "UpdateRegistry"
    "fixture/UpdateRegistry.yaml"

requestUpdateSchema :: UpdateSchema -> TestTree
requestUpdateSchema =
  req
    "UpdateSchema"
    "fixture/UpdateSchema.yaml"

-- Responses

responseCreateDiscoverer :: CreateDiscovererResponse -> TestTree
responseCreateDiscoverer =
  res
    "CreateDiscovererResponse"
    "fixture/CreateDiscovererResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateDiscoverer)

responseCreateRegistry :: CreateRegistryResponse -> TestTree
responseCreateRegistry =
  res
    "CreateRegistryResponse"
    "fixture/CreateRegistryResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateRegistry)

responseCreateSchema :: CreateSchemaResponse -> TestTree
responseCreateSchema =
  res
    "CreateSchemaResponse"
    "fixture/CreateSchemaResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateSchema)

responseDeleteDiscoverer :: DeleteDiscovererResponse -> TestTree
responseDeleteDiscoverer =
  res
    "DeleteDiscovererResponse"
    "fixture/DeleteDiscovererResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteDiscoverer)

responseDeleteRegistry :: DeleteRegistryResponse -> TestTree
responseDeleteRegistry =
  res
    "DeleteRegistryResponse"
    "fixture/DeleteRegistryResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteRegistry)

responseDeleteResourcePolicy :: DeleteResourcePolicyResponse -> TestTree
responseDeleteResourcePolicy =
  res
    "DeleteResourcePolicyResponse"
    "fixture/DeleteResourcePolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteResourcePolicy)

responseDeleteSchema :: DeleteSchemaResponse -> TestTree
responseDeleteSchema =
  res
    "DeleteSchemaResponse"
    "fixture/DeleteSchemaResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteSchema)

responseDeleteSchemaVersion :: DeleteSchemaVersionResponse -> TestTree
responseDeleteSchemaVersion =
  res
    "DeleteSchemaVersionResponse"
    "fixture/DeleteSchemaVersionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteSchemaVersion)

responseDescribeCodeBinding :: DescribeCodeBindingResponse -> TestTree
responseDescribeCodeBinding =
  res
    "DescribeCodeBindingResponse"
    "fixture/DescribeCodeBindingResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeCodeBinding)

responseDescribeDiscoverer :: DescribeDiscovererResponse -> TestTree
responseDescribeDiscoverer =
  res
    "DescribeDiscovererResponse"
    "fixture/DescribeDiscovererResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeDiscoverer)

responseDescribeRegistry :: DescribeRegistryResponse -> TestTree
responseDescribeRegistry =
  res
    "DescribeRegistryResponse"
    "fixture/DescribeRegistryResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeRegistry)

responseDescribeSchema :: DescribeSchemaResponse -> TestTree
responseDescribeSchema =
  res
    "DescribeSchemaResponse"
    "fixture/DescribeSchemaResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeSchema)

responseExportSchema :: ExportSchemaResponse -> TestTree
responseExportSchema =
  res
    "ExportSchemaResponse"
    "fixture/ExportSchemaResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ExportSchema)

responseGetCodeBindingSource :: GetCodeBindingSourceResponse -> TestTree
responseGetCodeBindingSource =
  res
    "GetCodeBindingSourceResponse"
    "fixture/GetCodeBindingSourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetCodeBindingSource)

responseGetDiscoveredSchema :: GetDiscoveredSchemaResponse -> TestTree
responseGetDiscoveredSchema =
  res
    "GetDiscoveredSchemaResponse"
    "fixture/GetDiscoveredSchemaResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetDiscoveredSchema)

responseGetResourcePolicy :: GetResourcePolicyResponse -> TestTree
responseGetResourcePolicy =
  res
    "GetResourcePolicyResponse"
    "fixture/GetResourcePolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetResourcePolicy)

responseListDiscoverers :: ListDiscoverersResponse -> TestTree
responseListDiscoverers =
  res
    "ListDiscoverersResponse"
    "fixture/ListDiscoverersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListDiscoverers)

responseListRegistries :: ListRegistriesResponse -> TestTree
responseListRegistries =
  res
    "ListRegistriesResponse"
    "fixture/ListRegistriesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListRegistries)

responseListSchemaVersions :: ListSchemaVersionsResponse -> TestTree
responseListSchemaVersions =
  res
    "ListSchemaVersionsResponse"
    "fixture/ListSchemaVersionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListSchemaVersions)

responseListSchemas :: ListSchemasResponse -> TestTree
responseListSchemas =
  res
    "ListSchemasResponse"
    "fixture/ListSchemasResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListSchemas)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responsePutCodeBinding :: PutCodeBindingResponse -> TestTree
responsePutCodeBinding =
  res
    "PutCodeBindingResponse"
    "fixture/PutCodeBindingResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutCodeBinding)

responsePutResourcePolicy :: PutResourcePolicyResponse -> TestTree
responsePutResourcePolicy =
  res
    "PutResourcePolicyResponse"
    "fixture/PutResourcePolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutResourcePolicy)

responseSearchSchemas :: SearchSchemasResponse -> TestTree
responseSearchSchemas =
  res
    "SearchSchemasResponse"
    "fixture/SearchSchemasResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SearchSchemas)

responseStartDiscoverer :: StartDiscovererResponse -> TestTree
responseStartDiscoverer =
  res
    "StartDiscovererResponse"
    "fixture/StartDiscovererResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartDiscoverer)

responseStopDiscoverer :: StopDiscovererResponse -> TestTree
responseStopDiscoverer =
  res
    "StopDiscovererResponse"
    "fixture/StopDiscovererResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StopDiscoverer)

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

responseUpdateDiscoverer :: UpdateDiscovererResponse -> TestTree
responseUpdateDiscoverer =
  res
    "UpdateDiscovererResponse"
    "fixture/UpdateDiscovererResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateDiscoverer)

responseUpdateRegistry :: UpdateRegistryResponse -> TestTree
responseUpdateRegistry =
  res
    "UpdateRegistryResponse"
    "fixture/UpdateRegistryResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateRegistry)

responseUpdateSchema :: UpdateSchemaResponse -> TestTree
responseUpdateSchema =
  res
    "UpdateSchemaResponse"
    "fixture/UpdateSchemaResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateSchema)
