{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.LakeFormation
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.LakeFormation where

import Amazonka.LakeFormation
import qualified Data.Proxy as Proxy
import Test.Amazonka.Fixture
import Test.Amazonka.LakeFormation.Internal
import Test.Amazonka.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestBatchRevokePermissions $
--             newBatchRevokePermissions
--
--         , requestDescribeResource $
--             newDescribeResource
--
--         , requestBatchGrantPermissions $
--             newBatchGrantPermissions
--
--         , requestGetEffectivePermissionsForPath $
--             newGetEffectivePermissionsForPath
--
--         , requestRevokePermissions $
--             newRevokePermissions
--
--         , requestUpdateResource $
--             newUpdateResource
--
--         , requestAddLFTagsToResource $
--             newAddLFTagsToResource
--
--         , requestSearchTablesByLFTags $
--             newSearchTablesByLFTags
--
--         , requestListResources $
--             newListResources
--
--         , requestGetLFTag $
--             newGetLFTag
--
--         , requestRemoveLFTagsFromResource $
--             newRemoveLFTagsFromResource
--
--         , requestUpdateLFTag $
--             newUpdateLFTag
--
--         , requestDeleteLFTag $
--             newDeleteLFTag
--
--         , requestCreateLFTag $
--             newCreateLFTag
--
--         , requestGetResourceLFTags $
--             newGetResourceLFTags
--
--         , requestPutDataLakeSettings $
--             newPutDataLakeSettings
--
--         , requestListPermissions $
--             newListPermissions
--
--         , requestDeregisterResource $
--             newDeregisterResource
--
--         , requestGetDataLakeSettings $
--             newGetDataLakeSettings
--
--         , requestSearchDatabasesByLFTags $
--             newSearchDatabasesByLFTags
--
--         , requestRegisterResource $
--             newRegisterResource
--
--         , requestGrantPermissions $
--             newGrantPermissions
--
--         , requestListLFTags $
--             newListLFTags
--
--           ]

--     , testGroup "response"
--         [ responseBatchRevokePermissions $
--             newBatchRevokePermissionsResponse
--
--         , responseDescribeResource $
--             newDescribeResourceResponse
--
--         , responseBatchGrantPermissions $
--             newBatchGrantPermissionsResponse
--
--         , responseGetEffectivePermissionsForPath $
--             newGetEffectivePermissionsForPathResponse
--
--         , responseRevokePermissions $
--             newRevokePermissionsResponse
--
--         , responseUpdateResource $
--             newUpdateResourceResponse
--
--         , responseAddLFTagsToResource $
--             newAddLFTagsToResourceResponse
--
--         , responseSearchTablesByLFTags $
--             newSearchTablesByLFTagsResponse
--
--         , responseListResources $
--             newListResourcesResponse
--
--         , responseGetLFTag $
--             newGetLFTagResponse
--
--         , responseRemoveLFTagsFromResource $
--             newRemoveLFTagsFromResourceResponse
--
--         , responseUpdateLFTag $
--             newUpdateLFTagResponse
--
--         , responseDeleteLFTag $
--             newDeleteLFTagResponse
--
--         , responseCreateLFTag $
--             newCreateLFTagResponse
--
--         , responseGetResourceLFTags $
--             newGetResourceLFTagsResponse
--
--         , responsePutDataLakeSettings $
--             newPutDataLakeSettingsResponse
--
--         , responseListPermissions $
--             newListPermissionsResponse
--
--         , responseDeregisterResource $
--             newDeregisterResourceResponse
--
--         , responseGetDataLakeSettings $
--             newGetDataLakeSettingsResponse
--
--         , responseSearchDatabasesByLFTags $
--             newSearchDatabasesByLFTagsResponse
--
--         , responseRegisterResource $
--             newRegisterResourceResponse
--
--         , responseGrantPermissions $
--             newGrantPermissionsResponse
--
--         , responseListLFTags $
--             newListLFTagsResponse
--
--           ]
--     ]

-- Requests

requestBatchRevokePermissions :: BatchRevokePermissions -> TestTree
requestBatchRevokePermissions =
  req
    "BatchRevokePermissions"
    "fixture/BatchRevokePermissions.yaml"

requestDescribeResource :: DescribeResource -> TestTree
requestDescribeResource =
  req
    "DescribeResource"
    "fixture/DescribeResource.yaml"

requestBatchGrantPermissions :: BatchGrantPermissions -> TestTree
requestBatchGrantPermissions =
  req
    "BatchGrantPermissions"
    "fixture/BatchGrantPermissions.yaml"

requestGetEffectivePermissionsForPath :: GetEffectivePermissionsForPath -> TestTree
requestGetEffectivePermissionsForPath =
  req
    "GetEffectivePermissionsForPath"
    "fixture/GetEffectivePermissionsForPath.yaml"

requestRevokePermissions :: RevokePermissions -> TestTree
requestRevokePermissions =
  req
    "RevokePermissions"
    "fixture/RevokePermissions.yaml"

requestUpdateResource :: UpdateResource -> TestTree
requestUpdateResource =
  req
    "UpdateResource"
    "fixture/UpdateResource.yaml"

requestAddLFTagsToResource :: AddLFTagsToResource -> TestTree
requestAddLFTagsToResource =
  req
    "AddLFTagsToResource"
    "fixture/AddLFTagsToResource.yaml"

requestSearchTablesByLFTags :: SearchTablesByLFTags -> TestTree
requestSearchTablesByLFTags =
  req
    "SearchTablesByLFTags"
    "fixture/SearchTablesByLFTags.yaml"

requestListResources :: ListResources -> TestTree
requestListResources =
  req
    "ListResources"
    "fixture/ListResources.yaml"

requestGetLFTag :: GetLFTag -> TestTree
requestGetLFTag =
  req
    "GetLFTag"
    "fixture/GetLFTag.yaml"

requestRemoveLFTagsFromResource :: RemoveLFTagsFromResource -> TestTree
requestRemoveLFTagsFromResource =
  req
    "RemoveLFTagsFromResource"
    "fixture/RemoveLFTagsFromResource.yaml"

requestUpdateLFTag :: UpdateLFTag -> TestTree
requestUpdateLFTag =
  req
    "UpdateLFTag"
    "fixture/UpdateLFTag.yaml"

requestDeleteLFTag :: DeleteLFTag -> TestTree
requestDeleteLFTag =
  req
    "DeleteLFTag"
    "fixture/DeleteLFTag.yaml"

requestCreateLFTag :: CreateLFTag -> TestTree
requestCreateLFTag =
  req
    "CreateLFTag"
    "fixture/CreateLFTag.yaml"

requestGetResourceLFTags :: GetResourceLFTags -> TestTree
requestGetResourceLFTags =
  req
    "GetResourceLFTags"
    "fixture/GetResourceLFTags.yaml"

requestPutDataLakeSettings :: PutDataLakeSettings -> TestTree
requestPutDataLakeSettings =
  req
    "PutDataLakeSettings"
    "fixture/PutDataLakeSettings.yaml"

requestListPermissions :: ListPermissions -> TestTree
requestListPermissions =
  req
    "ListPermissions"
    "fixture/ListPermissions.yaml"

requestDeregisterResource :: DeregisterResource -> TestTree
requestDeregisterResource =
  req
    "DeregisterResource"
    "fixture/DeregisterResource.yaml"

requestGetDataLakeSettings :: GetDataLakeSettings -> TestTree
requestGetDataLakeSettings =
  req
    "GetDataLakeSettings"
    "fixture/GetDataLakeSettings.yaml"

requestSearchDatabasesByLFTags :: SearchDatabasesByLFTags -> TestTree
requestSearchDatabasesByLFTags =
  req
    "SearchDatabasesByLFTags"
    "fixture/SearchDatabasesByLFTags.yaml"

requestRegisterResource :: RegisterResource -> TestTree
requestRegisterResource =
  req
    "RegisterResource"
    "fixture/RegisterResource.yaml"

requestGrantPermissions :: GrantPermissions -> TestTree
requestGrantPermissions =
  req
    "GrantPermissions"
    "fixture/GrantPermissions.yaml"

requestListLFTags :: ListLFTags -> TestTree
requestListLFTags =
  req
    "ListLFTags"
    "fixture/ListLFTags.yaml"

-- Responses

responseBatchRevokePermissions :: BatchRevokePermissionsResponse -> TestTree
responseBatchRevokePermissions =
  res
    "BatchRevokePermissionsResponse"
    "fixture/BatchRevokePermissionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchRevokePermissions)

responseDescribeResource :: DescribeResourceResponse -> TestTree
responseDescribeResource =
  res
    "DescribeResourceResponse"
    "fixture/DescribeResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeResource)

responseBatchGrantPermissions :: BatchGrantPermissionsResponse -> TestTree
responseBatchGrantPermissions =
  res
    "BatchGrantPermissionsResponse"
    "fixture/BatchGrantPermissionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchGrantPermissions)

responseGetEffectivePermissionsForPath :: GetEffectivePermissionsForPathResponse -> TestTree
responseGetEffectivePermissionsForPath =
  res
    "GetEffectivePermissionsForPathResponse"
    "fixture/GetEffectivePermissionsForPathResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetEffectivePermissionsForPath)

responseRevokePermissions :: RevokePermissionsResponse -> TestTree
responseRevokePermissions =
  res
    "RevokePermissionsResponse"
    "fixture/RevokePermissionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RevokePermissions)

responseUpdateResource :: UpdateResourceResponse -> TestTree
responseUpdateResource =
  res
    "UpdateResourceResponse"
    "fixture/UpdateResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateResource)

responseAddLFTagsToResource :: AddLFTagsToResourceResponse -> TestTree
responseAddLFTagsToResource =
  res
    "AddLFTagsToResourceResponse"
    "fixture/AddLFTagsToResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AddLFTagsToResource)

responseSearchTablesByLFTags :: SearchTablesByLFTagsResponse -> TestTree
responseSearchTablesByLFTags =
  res
    "SearchTablesByLFTagsResponse"
    "fixture/SearchTablesByLFTagsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SearchTablesByLFTags)

responseListResources :: ListResourcesResponse -> TestTree
responseListResources =
  res
    "ListResourcesResponse"
    "fixture/ListResourcesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListResources)

responseGetLFTag :: GetLFTagResponse -> TestTree
responseGetLFTag =
  res
    "GetLFTagResponse"
    "fixture/GetLFTagResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetLFTag)

responseRemoveLFTagsFromResource :: RemoveLFTagsFromResourceResponse -> TestTree
responseRemoveLFTagsFromResource =
  res
    "RemoveLFTagsFromResourceResponse"
    "fixture/RemoveLFTagsFromResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RemoveLFTagsFromResource)

responseUpdateLFTag :: UpdateLFTagResponse -> TestTree
responseUpdateLFTag =
  res
    "UpdateLFTagResponse"
    "fixture/UpdateLFTagResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateLFTag)

responseDeleteLFTag :: DeleteLFTagResponse -> TestTree
responseDeleteLFTag =
  res
    "DeleteLFTagResponse"
    "fixture/DeleteLFTagResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteLFTag)

responseCreateLFTag :: CreateLFTagResponse -> TestTree
responseCreateLFTag =
  res
    "CreateLFTagResponse"
    "fixture/CreateLFTagResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateLFTag)

responseGetResourceLFTags :: GetResourceLFTagsResponse -> TestTree
responseGetResourceLFTags =
  res
    "GetResourceLFTagsResponse"
    "fixture/GetResourceLFTagsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetResourceLFTags)

responsePutDataLakeSettings :: PutDataLakeSettingsResponse -> TestTree
responsePutDataLakeSettings =
  res
    "PutDataLakeSettingsResponse"
    "fixture/PutDataLakeSettingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutDataLakeSettings)

responseListPermissions :: ListPermissionsResponse -> TestTree
responseListPermissions =
  res
    "ListPermissionsResponse"
    "fixture/ListPermissionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListPermissions)

responseDeregisterResource :: DeregisterResourceResponse -> TestTree
responseDeregisterResource =
  res
    "DeregisterResourceResponse"
    "fixture/DeregisterResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeregisterResource)

responseGetDataLakeSettings :: GetDataLakeSettingsResponse -> TestTree
responseGetDataLakeSettings =
  res
    "GetDataLakeSettingsResponse"
    "fixture/GetDataLakeSettingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetDataLakeSettings)

responseSearchDatabasesByLFTags :: SearchDatabasesByLFTagsResponse -> TestTree
responseSearchDatabasesByLFTags =
  res
    "SearchDatabasesByLFTagsResponse"
    "fixture/SearchDatabasesByLFTagsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SearchDatabasesByLFTags)

responseRegisterResource :: RegisterResourceResponse -> TestTree
responseRegisterResource =
  res
    "RegisterResourceResponse"
    "fixture/RegisterResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RegisterResource)

responseGrantPermissions :: GrantPermissionsResponse -> TestTree
responseGrantPermissions =
  res
    "GrantPermissionsResponse"
    "fixture/GrantPermissionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GrantPermissions)

responseListLFTags :: ListLFTagsResponse -> TestTree
responseListLFTags =
  res
    "ListLFTagsResponse"
    "fixture/ListLFTagsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListLFTags)
