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
--         [ requestAddLFTagsToResource $
--             newAddLFTagsToResource
--
--         , requestBatchGrantPermissions $
--             newBatchGrantPermissions
--
--         , requestBatchRevokePermissions $
--             newBatchRevokePermissions
--
--         , requestCreateLFTag $
--             newCreateLFTag
--
--         , requestDeleteLFTag $
--             newDeleteLFTag
--
--         , requestDeregisterResource $
--             newDeregisterResource
--
--         , requestDescribeResource $
--             newDescribeResource
--
--         , requestGetDataLakeSettings $
--             newGetDataLakeSettings
--
--         , requestGetEffectivePermissionsForPath $
--             newGetEffectivePermissionsForPath
--
--         , requestGetLFTag $
--             newGetLFTag
--
--         , requestGetResourceLFTags $
--             newGetResourceLFTags
--
--         , requestGrantPermissions $
--             newGrantPermissions
--
--         , requestListLFTags $
--             newListLFTags
--
--         , requestListPermissions $
--             newListPermissions
--
--         , requestListResources $
--             newListResources
--
--         , requestPutDataLakeSettings $
--             newPutDataLakeSettings
--
--         , requestRegisterResource $
--             newRegisterResource
--
--         , requestRemoveLFTagsFromResource $
--             newRemoveLFTagsFromResource
--
--         , requestRevokePermissions $
--             newRevokePermissions
--
--         , requestSearchDatabasesByLFTags $
--             newSearchDatabasesByLFTags
--
--         , requestSearchTablesByLFTags $
--             newSearchTablesByLFTags
--
--         , requestUpdateLFTag $
--             newUpdateLFTag
--
--         , requestUpdateResource $
--             newUpdateResource
--
--           ]

--     , testGroup "response"
--         [ responseAddLFTagsToResource $
--             newAddLFTagsToResourceResponse
--
--         , responseBatchGrantPermissions $
--             newBatchGrantPermissionsResponse
--
--         , responseBatchRevokePermissions $
--             newBatchRevokePermissionsResponse
--
--         , responseCreateLFTag $
--             newCreateLFTagResponse
--
--         , responseDeleteLFTag $
--             newDeleteLFTagResponse
--
--         , responseDeregisterResource $
--             newDeregisterResourceResponse
--
--         , responseDescribeResource $
--             newDescribeResourceResponse
--
--         , responseGetDataLakeSettings $
--             newGetDataLakeSettingsResponse
--
--         , responseGetEffectivePermissionsForPath $
--             newGetEffectivePermissionsForPathResponse
--
--         , responseGetLFTag $
--             newGetLFTagResponse
--
--         , responseGetResourceLFTags $
--             newGetResourceLFTagsResponse
--
--         , responseGrantPermissions $
--             newGrantPermissionsResponse
--
--         , responseListLFTags $
--             newListLFTagsResponse
--
--         , responseListPermissions $
--             newListPermissionsResponse
--
--         , responseListResources $
--             newListResourcesResponse
--
--         , responsePutDataLakeSettings $
--             newPutDataLakeSettingsResponse
--
--         , responseRegisterResource $
--             newRegisterResourceResponse
--
--         , responseRemoveLFTagsFromResource $
--             newRemoveLFTagsFromResourceResponse
--
--         , responseRevokePermissions $
--             newRevokePermissionsResponse
--
--         , responseSearchDatabasesByLFTags $
--             newSearchDatabasesByLFTagsResponse
--
--         , responseSearchTablesByLFTags $
--             newSearchTablesByLFTagsResponse
--
--         , responseUpdateLFTag $
--             newUpdateLFTagResponse
--
--         , responseUpdateResource $
--             newUpdateResourceResponse
--
--           ]
--     ]

-- Requests

requestAddLFTagsToResource :: AddLFTagsToResource -> TestTree
requestAddLFTagsToResource =
  req
    "AddLFTagsToResource"
    "fixture/AddLFTagsToResource.yaml"

requestBatchGrantPermissions :: BatchGrantPermissions -> TestTree
requestBatchGrantPermissions =
  req
    "BatchGrantPermissions"
    "fixture/BatchGrantPermissions.yaml"

requestBatchRevokePermissions :: BatchRevokePermissions -> TestTree
requestBatchRevokePermissions =
  req
    "BatchRevokePermissions"
    "fixture/BatchRevokePermissions.yaml"

requestCreateLFTag :: CreateLFTag -> TestTree
requestCreateLFTag =
  req
    "CreateLFTag"
    "fixture/CreateLFTag.yaml"

requestDeleteLFTag :: DeleteLFTag -> TestTree
requestDeleteLFTag =
  req
    "DeleteLFTag"
    "fixture/DeleteLFTag.yaml"

requestDeregisterResource :: DeregisterResource -> TestTree
requestDeregisterResource =
  req
    "DeregisterResource"
    "fixture/DeregisterResource.yaml"

requestDescribeResource :: DescribeResource -> TestTree
requestDescribeResource =
  req
    "DescribeResource"
    "fixture/DescribeResource.yaml"

requestGetDataLakeSettings :: GetDataLakeSettings -> TestTree
requestGetDataLakeSettings =
  req
    "GetDataLakeSettings"
    "fixture/GetDataLakeSettings.yaml"

requestGetEffectivePermissionsForPath :: GetEffectivePermissionsForPath -> TestTree
requestGetEffectivePermissionsForPath =
  req
    "GetEffectivePermissionsForPath"
    "fixture/GetEffectivePermissionsForPath.yaml"

requestGetLFTag :: GetLFTag -> TestTree
requestGetLFTag =
  req
    "GetLFTag"
    "fixture/GetLFTag.yaml"

requestGetResourceLFTags :: GetResourceLFTags -> TestTree
requestGetResourceLFTags =
  req
    "GetResourceLFTags"
    "fixture/GetResourceLFTags.yaml"

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

requestListPermissions :: ListPermissions -> TestTree
requestListPermissions =
  req
    "ListPermissions"
    "fixture/ListPermissions.yaml"

requestListResources :: ListResources -> TestTree
requestListResources =
  req
    "ListResources"
    "fixture/ListResources.yaml"

requestPutDataLakeSettings :: PutDataLakeSettings -> TestTree
requestPutDataLakeSettings =
  req
    "PutDataLakeSettings"
    "fixture/PutDataLakeSettings.yaml"

requestRegisterResource :: RegisterResource -> TestTree
requestRegisterResource =
  req
    "RegisterResource"
    "fixture/RegisterResource.yaml"

requestRemoveLFTagsFromResource :: RemoveLFTagsFromResource -> TestTree
requestRemoveLFTagsFromResource =
  req
    "RemoveLFTagsFromResource"
    "fixture/RemoveLFTagsFromResource.yaml"

requestRevokePermissions :: RevokePermissions -> TestTree
requestRevokePermissions =
  req
    "RevokePermissions"
    "fixture/RevokePermissions.yaml"

requestSearchDatabasesByLFTags :: SearchDatabasesByLFTags -> TestTree
requestSearchDatabasesByLFTags =
  req
    "SearchDatabasesByLFTags"
    "fixture/SearchDatabasesByLFTags.yaml"

requestSearchTablesByLFTags :: SearchTablesByLFTags -> TestTree
requestSearchTablesByLFTags =
  req
    "SearchTablesByLFTags"
    "fixture/SearchTablesByLFTags.yaml"

requestUpdateLFTag :: UpdateLFTag -> TestTree
requestUpdateLFTag =
  req
    "UpdateLFTag"
    "fixture/UpdateLFTag.yaml"

requestUpdateResource :: UpdateResource -> TestTree
requestUpdateResource =
  req
    "UpdateResource"
    "fixture/UpdateResource.yaml"

-- Responses

responseAddLFTagsToResource :: AddLFTagsToResourceResponse -> TestTree
responseAddLFTagsToResource =
  res
    "AddLFTagsToResourceResponse"
    "fixture/AddLFTagsToResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AddLFTagsToResource)

responseBatchGrantPermissions :: BatchGrantPermissionsResponse -> TestTree
responseBatchGrantPermissions =
  res
    "BatchGrantPermissionsResponse"
    "fixture/BatchGrantPermissionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchGrantPermissions)

responseBatchRevokePermissions :: BatchRevokePermissionsResponse -> TestTree
responseBatchRevokePermissions =
  res
    "BatchRevokePermissionsResponse"
    "fixture/BatchRevokePermissionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchRevokePermissions)

responseCreateLFTag :: CreateLFTagResponse -> TestTree
responseCreateLFTag =
  res
    "CreateLFTagResponse"
    "fixture/CreateLFTagResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateLFTag)

responseDeleteLFTag :: DeleteLFTagResponse -> TestTree
responseDeleteLFTag =
  res
    "DeleteLFTagResponse"
    "fixture/DeleteLFTagResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteLFTag)

responseDeregisterResource :: DeregisterResourceResponse -> TestTree
responseDeregisterResource =
  res
    "DeregisterResourceResponse"
    "fixture/DeregisterResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeregisterResource)

responseDescribeResource :: DescribeResourceResponse -> TestTree
responseDescribeResource =
  res
    "DescribeResourceResponse"
    "fixture/DescribeResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeResource)

responseGetDataLakeSettings :: GetDataLakeSettingsResponse -> TestTree
responseGetDataLakeSettings =
  res
    "GetDataLakeSettingsResponse"
    "fixture/GetDataLakeSettingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetDataLakeSettings)

responseGetEffectivePermissionsForPath :: GetEffectivePermissionsForPathResponse -> TestTree
responseGetEffectivePermissionsForPath =
  res
    "GetEffectivePermissionsForPathResponse"
    "fixture/GetEffectivePermissionsForPathResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetEffectivePermissionsForPath)

responseGetLFTag :: GetLFTagResponse -> TestTree
responseGetLFTag =
  res
    "GetLFTagResponse"
    "fixture/GetLFTagResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetLFTag)

responseGetResourceLFTags :: GetResourceLFTagsResponse -> TestTree
responseGetResourceLFTags =
  res
    "GetResourceLFTagsResponse"
    "fixture/GetResourceLFTagsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetResourceLFTags)

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

responseListPermissions :: ListPermissionsResponse -> TestTree
responseListPermissions =
  res
    "ListPermissionsResponse"
    "fixture/ListPermissionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListPermissions)

responseListResources :: ListResourcesResponse -> TestTree
responseListResources =
  res
    "ListResourcesResponse"
    "fixture/ListResourcesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListResources)

responsePutDataLakeSettings :: PutDataLakeSettingsResponse -> TestTree
responsePutDataLakeSettings =
  res
    "PutDataLakeSettingsResponse"
    "fixture/PutDataLakeSettingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutDataLakeSettings)

responseRegisterResource :: RegisterResourceResponse -> TestTree
responseRegisterResource =
  res
    "RegisterResourceResponse"
    "fixture/RegisterResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RegisterResource)

responseRemoveLFTagsFromResource :: RemoveLFTagsFromResourceResponse -> TestTree
responseRemoveLFTagsFromResource =
  res
    "RemoveLFTagsFromResourceResponse"
    "fixture/RemoveLFTagsFromResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RemoveLFTagsFromResource)

responseRevokePermissions :: RevokePermissionsResponse -> TestTree
responseRevokePermissions =
  res
    "RevokePermissionsResponse"
    "fixture/RevokePermissionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RevokePermissions)

responseSearchDatabasesByLFTags :: SearchDatabasesByLFTagsResponse -> TestTree
responseSearchDatabasesByLFTags =
  res
    "SearchDatabasesByLFTagsResponse"
    "fixture/SearchDatabasesByLFTagsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SearchDatabasesByLFTags)

responseSearchTablesByLFTags :: SearchTablesByLFTagsResponse -> TestTree
responseSearchTablesByLFTags =
  res
    "SearchTablesByLFTagsResponse"
    "fixture/SearchTablesByLFTagsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SearchTablesByLFTags)

responseUpdateLFTag :: UpdateLFTagResponse -> TestTree
responseUpdateLFTag =
  res
    "UpdateLFTagResponse"
    "fixture/UpdateLFTagResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateLFTag)

responseUpdateResource :: UpdateResourceResponse -> TestTree
responseUpdateResource =
  res
    "UpdateResourceResponse"
    "fixture/UpdateResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateResource)
