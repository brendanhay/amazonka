{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.SSMSAP
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.SSMSAP where

import Amazonka.SSMSAP
import qualified Data.Proxy as Proxy
import Test.Amazonka.Fixture
import Test.Amazonka.Prelude
import Test.Amazonka.SSMSAP.Internal
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestDeleteResourcePermission $
--             newDeleteResourcePermission
--
--         , requestDeregisterApplication $
--             newDeregisterApplication
--
--         , requestGetApplication $
--             newGetApplication
--
--         , requestGetComponent $
--             newGetComponent
--
--         , requestGetDatabase $
--             newGetDatabase
--
--         , requestGetOperation $
--             newGetOperation
--
--         , requestGetResourcePermission $
--             newGetResourcePermission
--
--         , requestListApplications $
--             newListApplications
--
--         , requestListComponents $
--             newListComponents
--
--         , requestListDatabases $
--             newListDatabases
--
--         , requestListOperations $
--             newListOperations
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestPutResourcePermission $
--             newPutResourcePermission
--
--         , requestRegisterApplication $
--             newRegisterApplication
--
--         , requestTagResource $
--             newTagResource
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestUpdateApplicationSettings $
--             newUpdateApplicationSettings
--
--           ]

--     , testGroup "response"
--         [ responseDeleteResourcePermission $
--             newDeleteResourcePermissionResponse
--
--         , responseDeregisterApplication $
--             newDeregisterApplicationResponse
--
--         , responseGetApplication $
--             newGetApplicationResponse
--
--         , responseGetComponent $
--             newGetComponentResponse
--
--         , responseGetDatabase $
--             newGetDatabaseResponse
--
--         , responseGetOperation $
--             newGetOperationResponse
--
--         , responseGetResourcePermission $
--             newGetResourcePermissionResponse
--
--         , responseListApplications $
--             newListApplicationsResponse
--
--         , responseListComponents $
--             newListComponentsResponse
--
--         , responseListDatabases $
--             newListDatabasesResponse
--
--         , responseListOperations $
--             newListOperationsResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responsePutResourcePermission $
--             newPutResourcePermissionResponse
--
--         , responseRegisterApplication $
--             newRegisterApplicationResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseUpdateApplicationSettings $
--             newUpdateApplicationSettingsResponse
--
--           ]
--     ]

-- Requests

requestDeleteResourcePermission :: DeleteResourcePermission -> TestTree
requestDeleteResourcePermission =
  req
    "DeleteResourcePermission"
    "fixture/DeleteResourcePermission.yaml"

requestDeregisterApplication :: DeregisterApplication -> TestTree
requestDeregisterApplication =
  req
    "DeregisterApplication"
    "fixture/DeregisterApplication.yaml"

requestGetApplication :: GetApplication -> TestTree
requestGetApplication =
  req
    "GetApplication"
    "fixture/GetApplication.yaml"

requestGetComponent :: GetComponent -> TestTree
requestGetComponent =
  req
    "GetComponent"
    "fixture/GetComponent.yaml"

requestGetDatabase :: GetDatabase -> TestTree
requestGetDatabase =
  req
    "GetDatabase"
    "fixture/GetDatabase.yaml"

requestGetOperation :: GetOperation -> TestTree
requestGetOperation =
  req
    "GetOperation"
    "fixture/GetOperation.yaml"

requestGetResourcePermission :: GetResourcePermission -> TestTree
requestGetResourcePermission =
  req
    "GetResourcePermission"
    "fixture/GetResourcePermission.yaml"

requestListApplications :: ListApplications -> TestTree
requestListApplications =
  req
    "ListApplications"
    "fixture/ListApplications.yaml"

requestListComponents :: ListComponents -> TestTree
requestListComponents =
  req
    "ListComponents"
    "fixture/ListComponents.yaml"

requestListDatabases :: ListDatabases -> TestTree
requestListDatabases =
  req
    "ListDatabases"
    "fixture/ListDatabases.yaml"

requestListOperations :: ListOperations -> TestTree
requestListOperations =
  req
    "ListOperations"
    "fixture/ListOperations.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestPutResourcePermission :: PutResourcePermission -> TestTree
requestPutResourcePermission =
  req
    "PutResourcePermission"
    "fixture/PutResourcePermission.yaml"

requestRegisterApplication :: RegisterApplication -> TestTree
requestRegisterApplication =
  req
    "RegisterApplication"
    "fixture/RegisterApplication.yaml"

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

requestUpdateApplicationSettings :: UpdateApplicationSettings -> TestTree
requestUpdateApplicationSettings =
  req
    "UpdateApplicationSettings"
    "fixture/UpdateApplicationSettings.yaml"

-- Responses

responseDeleteResourcePermission :: DeleteResourcePermissionResponse -> TestTree
responseDeleteResourcePermission =
  res
    "DeleteResourcePermissionResponse"
    "fixture/DeleteResourcePermissionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteResourcePermission)

responseDeregisterApplication :: DeregisterApplicationResponse -> TestTree
responseDeregisterApplication =
  res
    "DeregisterApplicationResponse"
    "fixture/DeregisterApplicationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeregisterApplication)

responseGetApplication :: GetApplicationResponse -> TestTree
responseGetApplication =
  res
    "GetApplicationResponse"
    "fixture/GetApplicationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetApplication)

responseGetComponent :: GetComponentResponse -> TestTree
responseGetComponent =
  res
    "GetComponentResponse"
    "fixture/GetComponentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetComponent)

responseGetDatabase :: GetDatabaseResponse -> TestTree
responseGetDatabase =
  res
    "GetDatabaseResponse"
    "fixture/GetDatabaseResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetDatabase)

responseGetOperation :: GetOperationResponse -> TestTree
responseGetOperation =
  res
    "GetOperationResponse"
    "fixture/GetOperationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetOperation)

responseGetResourcePermission :: GetResourcePermissionResponse -> TestTree
responseGetResourcePermission =
  res
    "GetResourcePermissionResponse"
    "fixture/GetResourcePermissionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetResourcePermission)

responseListApplications :: ListApplicationsResponse -> TestTree
responseListApplications =
  res
    "ListApplicationsResponse"
    "fixture/ListApplicationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListApplications)

responseListComponents :: ListComponentsResponse -> TestTree
responseListComponents =
  res
    "ListComponentsResponse"
    "fixture/ListComponentsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListComponents)

responseListDatabases :: ListDatabasesResponse -> TestTree
responseListDatabases =
  res
    "ListDatabasesResponse"
    "fixture/ListDatabasesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListDatabases)

responseListOperations :: ListOperationsResponse -> TestTree
responseListOperations =
  res
    "ListOperationsResponse"
    "fixture/ListOperationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListOperations)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responsePutResourcePermission :: PutResourcePermissionResponse -> TestTree
responsePutResourcePermission =
  res
    "PutResourcePermissionResponse"
    "fixture/PutResourcePermissionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutResourcePermission)

responseRegisterApplication :: RegisterApplicationResponse -> TestTree
responseRegisterApplication =
  res
    "RegisterApplicationResponse"
    "fixture/RegisterApplicationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RegisterApplication)

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

responseUpdateApplicationSettings :: UpdateApplicationSettingsResponse -> TestTree
responseUpdateApplicationSettings =
  res
    "UpdateApplicationSettingsResponse"
    "fixture/UpdateApplicationSettingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateApplicationSettings)
