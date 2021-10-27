{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.Schemas
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.AWS.Gen.Schemas where

import Data.Proxy
import Network.AWS.Schemas
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.AWS.Schemas.Internal
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestUpdateRegistry $
--             newUpdateRegistry
--
--         , requestDeleteRegistry $
--             newDeleteRegistry
--
--         , requestSearchSchemas $
--             newSearchSchemas
--
--         , requestStopDiscoverer $
--             newStopDiscoverer
--
--         , requestDeleteSchemaVersion $
--             newDeleteSchemaVersion
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestListSchemaVersions $
--             newListSchemaVersions
--
--         , requestExportSchema $
--             newExportSchema
--
--         , requestGetDiscoveredSchema $
--             newGetDiscoveredSchema
--
--         , requestDeleteDiscoverer $
--             newDeleteDiscoverer
--
--         , requestUpdateDiscoverer $
--             newUpdateDiscoverer
--
--         , requestListDiscoverers $
--             newListDiscoverers
--
--         , requestListSchemas $
--             newListSchemas
--
--         , requestCreateDiscoverer $
--             newCreateDiscoverer
--
--         , requestDescribeRegistry $
--             newDescribeRegistry
--
--         , requestCreateRegistry $
--             newCreateRegistry
--
--         , requestListRegistries $
--             newListRegistries
--
--         , requestDescribeDiscoverer $
--             newDescribeDiscoverer
--
--         , requestGetResourcePolicy $
--             newGetResourcePolicy
--
--         , requestStartDiscoverer $
--             newStartDiscoverer
--
--         , requestDescribeSchema $
--             newDescribeSchema
--
--         , requestGetCodeBindingSource $
--             newGetCodeBindingSource
--
--         , requestPutCodeBinding $
--             newPutCodeBinding
--
--         , requestCreateSchema $
--             newCreateSchema
--
--         , requestTagResource $
--             newTagResource
--
--         , requestUpdateSchema $
--             newUpdateSchema
--
--         , requestDeleteSchema $
--             newDeleteSchema
--
--         , requestPutResourcePolicy $
--             newPutResourcePolicy
--
--         , requestDeleteResourcePolicy $
--             newDeleteResourcePolicy
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestDescribeCodeBinding $
--             newDescribeCodeBinding
--
--           ]

--     , testGroup "response"
--         [ responseUpdateRegistry $
--             newUpdateRegistryResponse
--
--         , responseDeleteRegistry $
--             newDeleteRegistryResponse
--
--         , responseSearchSchemas $
--             newSearchSchemasResponse
--
--         , responseStopDiscoverer $
--             newStopDiscovererResponse
--
--         , responseDeleteSchemaVersion $
--             newDeleteSchemaVersionResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseListSchemaVersions $
--             newListSchemaVersionsResponse
--
--         , responseExportSchema $
--             newExportSchemaResponse
--
--         , responseGetDiscoveredSchema $
--             newGetDiscoveredSchemaResponse
--
--         , responseDeleteDiscoverer $
--             newDeleteDiscovererResponse
--
--         , responseUpdateDiscoverer $
--             newUpdateDiscovererResponse
--
--         , responseListDiscoverers $
--             newListDiscoverersResponse
--
--         , responseListSchemas $
--             newListSchemasResponse
--
--         , responseCreateDiscoverer $
--             newCreateDiscovererResponse
--
--         , responseDescribeRegistry $
--             newDescribeRegistryResponse
--
--         , responseCreateRegistry $
--             newCreateRegistryResponse
--
--         , responseListRegistries $
--             newListRegistriesResponse
--
--         , responseDescribeDiscoverer $
--             newDescribeDiscovererResponse
--
--         , responseGetResourcePolicy $
--             newGetResourcePolicyResponse
--
--         , responseStartDiscoverer $
--             newStartDiscovererResponse
--
--         , responseDescribeSchema $
--             newDescribeSchemaResponse
--
--         , responseGetCodeBindingSource $
--             newGetCodeBindingSourceResponse
--
--         , responsePutCodeBinding $
--             newPutCodeBindingResponse
--
--         , responseCreateSchema $
--             newCreateSchemaResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseUpdateSchema $
--             newUpdateSchemaResponse
--
--         , responseDeleteSchema $
--             newDeleteSchemaResponse
--
--         , responsePutResourcePolicy $
--             newPutResourcePolicyResponse
--
--         , responseDeleteResourcePolicy $
--             newDeleteResourcePolicyResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseDescribeCodeBinding $
--             newDescribeCodeBindingResponse
--
--           ]
--     ]

-- Requests

requestUpdateRegistry :: UpdateRegistry -> TestTree
requestUpdateRegistry =
  req
    "UpdateRegistry"
    "fixture/UpdateRegistry.yaml"

requestDeleteRegistry :: DeleteRegistry -> TestTree
requestDeleteRegistry =
  req
    "DeleteRegistry"
    "fixture/DeleteRegistry.yaml"

requestSearchSchemas :: SearchSchemas -> TestTree
requestSearchSchemas =
  req
    "SearchSchemas"
    "fixture/SearchSchemas.yaml"

requestStopDiscoverer :: StopDiscoverer -> TestTree
requestStopDiscoverer =
  req
    "StopDiscoverer"
    "fixture/StopDiscoverer.yaml"

requestDeleteSchemaVersion :: DeleteSchemaVersion -> TestTree
requestDeleteSchemaVersion =
  req
    "DeleteSchemaVersion"
    "fixture/DeleteSchemaVersion.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestListSchemaVersions :: ListSchemaVersions -> TestTree
requestListSchemaVersions =
  req
    "ListSchemaVersions"
    "fixture/ListSchemaVersions.yaml"

requestExportSchema :: ExportSchema -> TestTree
requestExportSchema =
  req
    "ExportSchema"
    "fixture/ExportSchema.yaml"

requestGetDiscoveredSchema :: GetDiscoveredSchema -> TestTree
requestGetDiscoveredSchema =
  req
    "GetDiscoveredSchema"
    "fixture/GetDiscoveredSchema.yaml"

requestDeleteDiscoverer :: DeleteDiscoverer -> TestTree
requestDeleteDiscoverer =
  req
    "DeleteDiscoverer"
    "fixture/DeleteDiscoverer.yaml"

requestUpdateDiscoverer :: UpdateDiscoverer -> TestTree
requestUpdateDiscoverer =
  req
    "UpdateDiscoverer"
    "fixture/UpdateDiscoverer.yaml"

requestListDiscoverers :: ListDiscoverers -> TestTree
requestListDiscoverers =
  req
    "ListDiscoverers"
    "fixture/ListDiscoverers.yaml"

requestListSchemas :: ListSchemas -> TestTree
requestListSchemas =
  req
    "ListSchemas"
    "fixture/ListSchemas.yaml"

requestCreateDiscoverer :: CreateDiscoverer -> TestTree
requestCreateDiscoverer =
  req
    "CreateDiscoverer"
    "fixture/CreateDiscoverer.yaml"

requestDescribeRegistry :: DescribeRegistry -> TestTree
requestDescribeRegistry =
  req
    "DescribeRegistry"
    "fixture/DescribeRegistry.yaml"

requestCreateRegistry :: CreateRegistry -> TestTree
requestCreateRegistry =
  req
    "CreateRegistry"
    "fixture/CreateRegistry.yaml"

requestListRegistries :: ListRegistries -> TestTree
requestListRegistries =
  req
    "ListRegistries"
    "fixture/ListRegistries.yaml"

requestDescribeDiscoverer :: DescribeDiscoverer -> TestTree
requestDescribeDiscoverer =
  req
    "DescribeDiscoverer"
    "fixture/DescribeDiscoverer.yaml"

requestGetResourcePolicy :: GetResourcePolicy -> TestTree
requestGetResourcePolicy =
  req
    "GetResourcePolicy"
    "fixture/GetResourcePolicy.yaml"

requestStartDiscoverer :: StartDiscoverer -> TestTree
requestStartDiscoverer =
  req
    "StartDiscoverer"
    "fixture/StartDiscoverer.yaml"

requestDescribeSchema :: DescribeSchema -> TestTree
requestDescribeSchema =
  req
    "DescribeSchema"
    "fixture/DescribeSchema.yaml"

requestGetCodeBindingSource :: GetCodeBindingSource -> TestTree
requestGetCodeBindingSource =
  req
    "GetCodeBindingSource"
    "fixture/GetCodeBindingSource.yaml"

requestPutCodeBinding :: PutCodeBinding -> TestTree
requestPutCodeBinding =
  req
    "PutCodeBinding"
    "fixture/PutCodeBinding.yaml"

requestCreateSchema :: CreateSchema -> TestTree
requestCreateSchema =
  req
    "CreateSchema"
    "fixture/CreateSchema.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestUpdateSchema :: UpdateSchema -> TestTree
requestUpdateSchema =
  req
    "UpdateSchema"
    "fixture/UpdateSchema.yaml"

requestDeleteSchema :: DeleteSchema -> TestTree
requestDeleteSchema =
  req
    "DeleteSchema"
    "fixture/DeleteSchema.yaml"

requestPutResourcePolicy :: PutResourcePolicy -> TestTree
requestPutResourcePolicy =
  req
    "PutResourcePolicy"
    "fixture/PutResourcePolicy.yaml"

requestDeleteResourcePolicy :: DeleteResourcePolicy -> TestTree
requestDeleteResourcePolicy =
  req
    "DeleteResourcePolicy"
    "fixture/DeleteResourcePolicy.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestDescribeCodeBinding :: DescribeCodeBinding -> TestTree
requestDescribeCodeBinding =
  req
    "DescribeCodeBinding"
    "fixture/DescribeCodeBinding.yaml"

-- Responses

responseUpdateRegistry :: UpdateRegistryResponse -> TestTree
responseUpdateRegistry =
  res
    "UpdateRegistryResponse"
    "fixture/UpdateRegistryResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateRegistry)

responseDeleteRegistry :: DeleteRegistryResponse -> TestTree
responseDeleteRegistry =
  res
    "DeleteRegistryResponse"
    "fixture/DeleteRegistryResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteRegistry)

responseSearchSchemas :: SearchSchemasResponse -> TestTree
responseSearchSchemas =
  res
    "SearchSchemasResponse"
    "fixture/SearchSchemasResponse.proto"
    defaultService
    (Proxy :: Proxy SearchSchemas)

responseStopDiscoverer :: StopDiscovererResponse -> TestTree
responseStopDiscoverer =
  res
    "StopDiscovererResponse"
    "fixture/StopDiscovererResponse.proto"
    defaultService
    (Proxy :: Proxy StopDiscoverer)

responseDeleteSchemaVersion :: DeleteSchemaVersionResponse -> TestTree
responseDeleteSchemaVersion =
  res
    "DeleteSchemaVersionResponse"
    "fixture/DeleteSchemaVersionResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteSchemaVersion)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy :: Proxy ListTagsForResource)

responseListSchemaVersions :: ListSchemaVersionsResponse -> TestTree
responseListSchemaVersions =
  res
    "ListSchemaVersionsResponse"
    "fixture/ListSchemaVersionsResponse.proto"
    defaultService
    (Proxy :: Proxy ListSchemaVersions)

responseExportSchema :: ExportSchemaResponse -> TestTree
responseExportSchema =
  res
    "ExportSchemaResponse"
    "fixture/ExportSchemaResponse.proto"
    defaultService
    (Proxy :: Proxy ExportSchema)

responseGetDiscoveredSchema :: GetDiscoveredSchemaResponse -> TestTree
responseGetDiscoveredSchema =
  res
    "GetDiscoveredSchemaResponse"
    "fixture/GetDiscoveredSchemaResponse.proto"
    defaultService
    (Proxy :: Proxy GetDiscoveredSchema)

responseDeleteDiscoverer :: DeleteDiscovererResponse -> TestTree
responseDeleteDiscoverer =
  res
    "DeleteDiscovererResponse"
    "fixture/DeleteDiscovererResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteDiscoverer)

responseUpdateDiscoverer :: UpdateDiscovererResponse -> TestTree
responseUpdateDiscoverer =
  res
    "UpdateDiscovererResponse"
    "fixture/UpdateDiscovererResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateDiscoverer)

responseListDiscoverers :: ListDiscoverersResponse -> TestTree
responseListDiscoverers =
  res
    "ListDiscoverersResponse"
    "fixture/ListDiscoverersResponse.proto"
    defaultService
    (Proxy :: Proxy ListDiscoverers)

responseListSchemas :: ListSchemasResponse -> TestTree
responseListSchemas =
  res
    "ListSchemasResponse"
    "fixture/ListSchemasResponse.proto"
    defaultService
    (Proxy :: Proxy ListSchemas)

responseCreateDiscoverer :: CreateDiscovererResponse -> TestTree
responseCreateDiscoverer =
  res
    "CreateDiscovererResponse"
    "fixture/CreateDiscovererResponse.proto"
    defaultService
    (Proxy :: Proxy CreateDiscoverer)

responseDescribeRegistry :: DescribeRegistryResponse -> TestTree
responseDescribeRegistry =
  res
    "DescribeRegistryResponse"
    "fixture/DescribeRegistryResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeRegistry)

responseCreateRegistry :: CreateRegistryResponse -> TestTree
responseCreateRegistry =
  res
    "CreateRegistryResponse"
    "fixture/CreateRegistryResponse.proto"
    defaultService
    (Proxy :: Proxy CreateRegistry)

responseListRegistries :: ListRegistriesResponse -> TestTree
responseListRegistries =
  res
    "ListRegistriesResponse"
    "fixture/ListRegistriesResponse.proto"
    defaultService
    (Proxy :: Proxy ListRegistries)

responseDescribeDiscoverer :: DescribeDiscovererResponse -> TestTree
responseDescribeDiscoverer =
  res
    "DescribeDiscovererResponse"
    "fixture/DescribeDiscovererResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeDiscoverer)

responseGetResourcePolicy :: GetResourcePolicyResponse -> TestTree
responseGetResourcePolicy =
  res
    "GetResourcePolicyResponse"
    "fixture/GetResourcePolicyResponse.proto"
    defaultService
    (Proxy :: Proxy GetResourcePolicy)

responseStartDiscoverer :: StartDiscovererResponse -> TestTree
responseStartDiscoverer =
  res
    "StartDiscovererResponse"
    "fixture/StartDiscovererResponse.proto"
    defaultService
    (Proxy :: Proxy StartDiscoverer)

responseDescribeSchema :: DescribeSchemaResponse -> TestTree
responseDescribeSchema =
  res
    "DescribeSchemaResponse"
    "fixture/DescribeSchemaResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeSchema)

responseGetCodeBindingSource :: GetCodeBindingSourceResponse -> TestTree
responseGetCodeBindingSource =
  res
    "GetCodeBindingSourceResponse"
    "fixture/GetCodeBindingSourceResponse.proto"
    defaultService
    (Proxy :: Proxy GetCodeBindingSource)

responsePutCodeBinding :: PutCodeBindingResponse -> TestTree
responsePutCodeBinding =
  res
    "PutCodeBindingResponse"
    "fixture/PutCodeBindingResponse.proto"
    defaultService
    (Proxy :: Proxy PutCodeBinding)

responseCreateSchema :: CreateSchemaResponse -> TestTree
responseCreateSchema =
  res
    "CreateSchemaResponse"
    "fixture/CreateSchemaResponse.proto"
    defaultService
    (Proxy :: Proxy CreateSchema)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy :: Proxy TagResource)

responseUpdateSchema :: UpdateSchemaResponse -> TestTree
responseUpdateSchema =
  res
    "UpdateSchemaResponse"
    "fixture/UpdateSchemaResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateSchema)

responseDeleteSchema :: DeleteSchemaResponse -> TestTree
responseDeleteSchema =
  res
    "DeleteSchemaResponse"
    "fixture/DeleteSchemaResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteSchema)

responsePutResourcePolicy :: PutResourcePolicyResponse -> TestTree
responsePutResourcePolicy =
  res
    "PutResourcePolicyResponse"
    "fixture/PutResourcePolicyResponse.proto"
    defaultService
    (Proxy :: Proxy PutResourcePolicy)

responseDeleteResourcePolicy :: DeleteResourcePolicyResponse -> TestTree
responseDeleteResourcePolicy =
  res
    "DeleteResourcePolicyResponse"
    "fixture/DeleteResourcePolicyResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteResourcePolicy)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy :: Proxy UntagResource)

responseDescribeCodeBinding :: DescribeCodeBindingResponse -> TestTree
responseDescribeCodeBinding =
  res
    "DescribeCodeBindingResponse"
    "fixture/DescribeCodeBindingResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeCodeBinding)
