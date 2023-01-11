{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.AmplifyBackend
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.AmplifyBackend where

import Amazonka.AmplifyBackend
import qualified Data.Proxy as Proxy
import Test.Amazonka.AmplifyBackend.Internal
import Test.Amazonka.Fixture
import Test.Amazonka.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestCloneBackend $
--             newCloneBackend
--
--         , requestCreateBackend $
--             newCreateBackend
--
--         , requestCreateBackendAPI $
--             newCreateBackendAPI
--
--         , requestCreateBackendAuth $
--             newCreateBackendAuth
--
--         , requestCreateBackendConfig $
--             newCreateBackendConfig
--
--         , requestCreateBackendStorage $
--             newCreateBackendStorage
--
--         , requestCreateToken $
--             newCreateToken
--
--         , requestDeleteBackend $
--             newDeleteBackend
--
--         , requestDeleteBackendAPI $
--             newDeleteBackendAPI
--
--         , requestDeleteBackendAuth $
--             newDeleteBackendAuth
--
--         , requestDeleteBackendStorage $
--             newDeleteBackendStorage
--
--         , requestDeleteToken $
--             newDeleteToken
--
--         , requestGenerateBackendAPIModels $
--             newGenerateBackendAPIModels
--
--         , requestGetBackend $
--             newGetBackend
--
--         , requestGetBackendAPI $
--             newGetBackendAPI
--
--         , requestGetBackendAPIModels $
--             newGetBackendAPIModels
--
--         , requestGetBackendAuth $
--             newGetBackendAuth
--
--         , requestGetBackendJob $
--             newGetBackendJob
--
--         , requestGetBackendStorage $
--             newGetBackendStorage
--
--         , requestGetToken $
--             newGetToken
--
--         , requestImportBackendAuth $
--             newImportBackendAuth
--
--         , requestImportBackendStorage $
--             newImportBackendStorage
--
--         , requestListBackendJobs $
--             newListBackendJobs
--
--         , requestListS3Buckets $
--             newListS3Buckets
--
--         , requestRemoveAllBackends $
--             newRemoveAllBackends
--
--         , requestRemoveBackendConfig $
--             newRemoveBackendConfig
--
--         , requestUpdateBackendAPI $
--             newUpdateBackendAPI
--
--         , requestUpdateBackendAuth $
--             newUpdateBackendAuth
--
--         , requestUpdateBackendConfig $
--             newUpdateBackendConfig
--
--         , requestUpdateBackendJob $
--             newUpdateBackendJob
--
--         , requestUpdateBackendStorage $
--             newUpdateBackendStorage
--
--           ]

--     , testGroup "response"
--         [ responseCloneBackend $
--             newCloneBackendResponse
--
--         , responseCreateBackend $
--             newCreateBackendResponse
--
--         , responseCreateBackendAPI $
--             newCreateBackendAPIResponse
--
--         , responseCreateBackendAuth $
--             newCreateBackendAuthResponse
--
--         , responseCreateBackendConfig $
--             newCreateBackendConfigResponse
--
--         , responseCreateBackendStorage $
--             newCreateBackendStorageResponse
--
--         , responseCreateToken $
--             newCreateTokenResponse
--
--         , responseDeleteBackend $
--             newDeleteBackendResponse
--
--         , responseDeleteBackendAPI $
--             newDeleteBackendAPIResponse
--
--         , responseDeleteBackendAuth $
--             newDeleteBackendAuthResponse
--
--         , responseDeleteBackendStorage $
--             newDeleteBackendStorageResponse
--
--         , responseDeleteToken $
--             newDeleteTokenResponse
--
--         , responseGenerateBackendAPIModels $
--             newGenerateBackendAPIModelsResponse
--
--         , responseGetBackend $
--             newGetBackendResponse
--
--         , responseGetBackendAPI $
--             newGetBackendAPIResponse
--
--         , responseGetBackendAPIModels $
--             newGetBackendAPIModelsResponse
--
--         , responseGetBackendAuth $
--             newGetBackendAuthResponse
--
--         , responseGetBackendJob $
--             newGetBackendJobResponse
--
--         , responseGetBackendStorage $
--             newGetBackendStorageResponse
--
--         , responseGetToken $
--             newGetTokenResponse
--
--         , responseImportBackendAuth $
--             newImportBackendAuthResponse
--
--         , responseImportBackendStorage $
--             newImportBackendStorageResponse
--
--         , responseListBackendJobs $
--             newListBackendJobsResponse
--
--         , responseListS3Buckets $
--             newListS3BucketsResponse
--
--         , responseRemoveAllBackends $
--             newRemoveAllBackendsResponse
--
--         , responseRemoveBackendConfig $
--             newRemoveBackendConfigResponse
--
--         , responseUpdateBackendAPI $
--             newUpdateBackendAPIResponse
--
--         , responseUpdateBackendAuth $
--             newUpdateBackendAuthResponse
--
--         , responseUpdateBackendConfig $
--             newUpdateBackendConfigResponse
--
--         , responseUpdateBackendJob $
--             newUpdateBackendJobResponse
--
--         , responseUpdateBackendStorage $
--             newUpdateBackendStorageResponse
--
--           ]
--     ]

-- Requests

requestCloneBackend :: CloneBackend -> TestTree
requestCloneBackend =
  req
    "CloneBackend"
    "fixture/CloneBackend.yaml"

requestCreateBackend :: CreateBackend -> TestTree
requestCreateBackend =
  req
    "CreateBackend"
    "fixture/CreateBackend.yaml"

requestCreateBackendAPI :: CreateBackendAPI -> TestTree
requestCreateBackendAPI =
  req
    "CreateBackendAPI"
    "fixture/CreateBackendAPI.yaml"

requestCreateBackendAuth :: CreateBackendAuth -> TestTree
requestCreateBackendAuth =
  req
    "CreateBackendAuth"
    "fixture/CreateBackendAuth.yaml"

requestCreateBackendConfig :: CreateBackendConfig -> TestTree
requestCreateBackendConfig =
  req
    "CreateBackendConfig"
    "fixture/CreateBackendConfig.yaml"

requestCreateBackendStorage :: CreateBackendStorage -> TestTree
requestCreateBackendStorage =
  req
    "CreateBackendStorage"
    "fixture/CreateBackendStorage.yaml"

requestCreateToken :: CreateToken -> TestTree
requestCreateToken =
  req
    "CreateToken"
    "fixture/CreateToken.yaml"

requestDeleteBackend :: DeleteBackend -> TestTree
requestDeleteBackend =
  req
    "DeleteBackend"
    "fixture/DeleteBackend.yaml"

requestDeleteBackendAPI :: DeleteBackendAPI -> TestTree
requestDeleteBackendAPI =
  req
    "DeleteBackendAPI"
    "fixture/DeleteBackendAPI.yaml"

requestDeleteBackendAuth :: DeleteBackendAuth -> TestTree
requestDeleteBackendAuth =
  req
    "DeleteBackendAuth"
    "fixture/DeleteBackendAuth.yaml"

requestDeleteBackendStorage :: DeleteBackendStorage -> TestTree
requestDeleteBackendStorage =
  req
    "DeleteBackendStorage"
    "fixture/DeleteBackendStorage.yaml"

requestDeleteToken :: DeleteToken -> TestTree
requestDeleteToken =
  req
    "DeleteToken"
    "fixture/DeleteToken.yaml"

requestGenerateBackendAPIModels :: GenerateBackendAPIModels -> TestTree
requestGenerateBackendAPIModels =
  req
    "GenerateBackendAPIModels"
    "fixture/GenerateBackendAPIModels.yaml"

requestGetBackend :: GetBackend -> TestTree
requestGetBackend =
  req
    "GetBackend"
    "fixture/GetBackend.yaml"

requestGetBackendAPI :: GetBackendAPI -> TestTree
requestGetBackendAPI =
  req
    "GetBackendAPI"
    "fixture/GetBackendAPI.yaml"

requestGetBackendAPIModels :: GetBackendAPIModels -> TestTree
requestGetBackendAPIModels =
  req
    "GetBackendAPIModels"
    "fixture/GetBackendAPIModels.yaml"

requestGetBackendAuth :: GetBackendAuth -> TestTree
requestGetBackendAuth =
  req
    "GetBackendAuth"
    "fixture/GetBackendAuth.yaml"

requestGetBackendJob :: GetBackendJob -> TestTree
requestGetBackendJob =
  req
    "GetBackendJob"
    "fixture/GetBackendJob.yaml"

requestGetBackendStorage :: GetBackendStorage -> TestTree
requestGetBackendStorage =
  req
    "GetBackendStorage"
    "fixture/GetBackendStorage.yaml"

requestGetToken :: GetToken -> TestTree
requestGetToken =
  req
    "GetToken"
    "fixture/GetToken.yaml"

requestImportBackendAuth :: ImportBackendAuth -> TestTree
requestImportBackendAuth =
  req
    "ImportBackendAuth"
    "fixture/ImportBackendAuth.yaml"

requestImportBackendStorage :: ImportBackendStorage -> TestTree
requestImportBackendStorage =
  req
    "ImportBackendStorage"
    "fixture/ImportBackendStorage.yaml"

requestListBackendJobs :: ListBackendJobs -> TestTree
requestListBackendJobs =
  req
    "ListBackendJobs"
    "fixture/ListBackendJobs.yaml"

requestListS3Buckets :: ListS3Buckets -> TestTree
requestListS3Buckets =
  req
    "ListS3Buckets"
    "fixture/ListS3Buckets.yaml"

requestRemoveAllBackends :: RemoveAllBackends -> TestTree
requestRemoveAllBackends =
  req
    "RemoveAllBackends"
    "fixture/RemoveAllBackends.yaml"

requestRemoveBackendConfig :: RemoveBackendConfig -> TestTree
requestRemoveBackendConfig =
  req
    "RemoveBackendConfig"
    "fixture/RemoveBackendConfig.yaml"

requestUpdateBackendAPI :: UpdateBackendAPI -> TestTree
requestUpdateBackendAPI =
  req
    "UpdateBackendAPI"
    "fixture/UpdateBackendAPI.yaml"

requestUpdateBackendAuth :: UpdateBackendAuth -> TestTree
requestUpdateBackendAuth =
  req
    "UpdateBackendAuth"
    "fixture/UpdateBackendAuth.yaml"

requestUpdateBackendConfig :: UpdateBackendConfig -> TestTree
requestUpdateBackendConfig =
  req
    "UpdateBackendConfig"
    "fixture/UpdateBackendConfig.yaml"

requestUpdateBackendJob :: UpdateBackendJob -> TestTree
requestUpdateBackendJob =
  req
    "UpdateBackendJob"
    "fixture/UpdateBackendJob.yaml"

requestUpdateBackendStorage :: UpdateBackendStorage -> TestTree
requestUpdateBackendStorage =
  req
    "UpdateBackendStorage"
    "fixture/UpdateBackendStorage.yaml"

-- Responses

responseCloneBackend :: CloneBackendResponse -> TestTree
responseCloneBackend =
  res
    "CloneBackendResponse"
    "fixture/CloneBackendResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CloneBackend)

responseCreateBackend :: CreateBackendResponse -> TestTree
responseCreateBackend =
  res
    "CreateBackendResponse"
    "fixture/CreateBackendResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateBackend)

responseCreateBackendAPI :: CreateBackendAPIResponse -> TestTree
responseCreateBackendAPI =
  res
    "CreateBackendAPIResponse"
    "fixture/CreateBackendAPIResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateBackendAPI)

responseCreateBackendAuth :: CreateBackendAuthResponse -> TestTree
responseCreateBackendAuth =
  res
    "CreateBackendAuthResponse"
    "fixture/CreateBackendAuthResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateBackendAuth)

responseCreateBackendConfig :: CreateBackendConfigResponse -> TestTree
responseCreateBackendConfig =
  res
    "CreateBackendConfigResponse"
    "fixture/CreateBackendConfigResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateBackendConfig)

responseCreateBackendStorage :: CreateBackendStorageResponse -> TestTree
responseCreateBackendStorage =
  res
    "CreateBackendStorageResponse"
    "fixture/CreateBackendStorageResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateBackendStorage)

responseCreateToken :: CreateTokenResponse -> TestTree
responseCreateToken =
  res
    "CreateTokenResponse"
    "fixture/CreateTokenResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateToken)

responseDeleteBackend :: DeleteBackendResponse -> TestTree
responseDeleteBackend =
  res
    "DeleteBackendResponse"
    "fixture/DeleteBackendResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteBackend)

responseDeleteBackendAPI :: DeleteBackendAPIResponse -> TestTree
responseDeleteBackendAPI =
  res
    "DeleteBackendAPIResponse"
    "fixture/DeleteBackendAPIResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteBackendAPI)

responseDeleteBackendAuth :: DeleteBackendAuthResponse -> TestTree
responseDeleteBackendAuth =
  res
    "DeleteBackendAuthResponse"
    "fixture/DeleteBackendAuthResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteBackendAuth)

responseDeleteBackendStorage :: DeleteBackendStorageResponse -> TestTree
responseDeleteBackendStorage =
  res
    "DeleteBackendStorageResponse"
    "fixture/DeleteBackendStorageResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteBackendStorage)

responseDeleteToken :: DeleteTokenResponse -> TestTree
responseDeleteToken =
  res
    "DeleteTokenResponse"
    "fixture/DeleteTokenResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteToken)

responseGenerateBackendAPIModels :: GenerateBackendAPIModelsResponse -> TestTree
responseGenerateBackendAPIModels =
  res
    "GenerateBackendAPIModelsResponse"
    "fixture/GenerateBackendAPIModelsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GenerateBackendAPIModels)

responseGetBackend :: GetBackendResponse -> TestTree
responseGetBackend =
  res
    "GetBackendResponse"
    "fixture/GetBackendResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetBackend)

responseGetBackendAPI :: GetBackendAPIResponse -> TestTree
responseGetBackendAPI =
  res
    "GetBackendAPIResponse"
    "fixture/GetBackendAPIResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetBackendAPI)

responseGetBackendAPIModels :: GetBackendAPIModelsResponse -> TestTree
responseGetBackendAPIModels =
  res
    "GetBackendAPIModelsResponse"
    "fixture/GetBackendAPIModelsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetBackendAPIModels)

responseGetBackendAuth :: GetBackendAuthResponse -> TestTree
responseGetBackendAuth =
  res
    "GetBackendAuthResponse"
    "fixture/GetBackendAuthResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetBackendAuth)

responseGetBackendJob :: GetBackendJobResponse -> TestTree
responseGetBackendJob =
  res
    "GetBackendJobResponse"
    "fixture/GetBackendJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetBackendJob)

responseGetBackendStorage :: GetBackendStorageResponse -> TestTree
responseGetBackendStorage =
  res
    "GetBackendStorageResponse"
    "fixture/GetBackendStorageResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetBackendStorage)

responseGetToken :: GetTokenResponse -> TestTree
responseGetToken =
  res
    "GetTokenResponse"
    "fixture/GetTokenResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetToken)

responseImportBackendAuth :: ImportBackendAuthResponse -> TestTree
responseImportBackendAuth =
  res
    "ImportBackendAuthResponse"
    "fixture/ImportBackendAuthResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ImportBackendAuth)

responseImportBackendStorage :: ImportBackendStorageResponse -> TestTree
responseImportBackendStorage =
  res
    "ImportBackendStorageResponse"
    "fixture/ImportBackendStorageResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ImportBackendStorage)

responseListBackendJobs :: ListBackendJobsResponse -> TestTree
responseListBackendJobs =
  res
    "ListBackendJobsResponse"
    "fixture/ListBackendJobsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListBackendJobs)

responseListS3Buckets :: ListS3BucketsResponse -> TestTree
responseListS3Buckets =
  res
    "ListS3BucketsResponse"
    "fixture/ListS3BucketsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListS3Buckets)

responseRemoveAllBackends :: RemoveAllBackendsResponse -> TestTree
responseRemoveAllBackends =
  res
    "RemoveAllBackendsResponse"
    "fixture/RemoveAllBackendsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RemoveAllBackends)

responseRemoveBackendConfig :: RemoveBackendConfigResponse -> TestTree
responseRemoveBackendConfig =
  res
    "RemoveBackendConfigResponse"
    "fixture/RemoveBackendConfigResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RemoveBackendConfig)

responseUpdateBackendAPI :: UpdateBackendAPIResponse -> TestTree
responseUpdateBackendAPI =
  res
    "UpdateBackendAPIResponse"
    "fixture/UpdateBackendAPIResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateBackendAPI)

responseUpdateBackendAuth :: UpdateBackendAuthResponse -> TestTree
responseUpdateBackendAuth =
  res
    "UpdateBackendAuthResponse"
    "fixture/UpdateBackendAuthResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateBackendAuth)

responseUpdateBackendConfig :: UpdateBackendConfigResponse -> TestTree
responseUpdateBackendConfig =
  res
    "UpdateBackendConfigResponse"
    "fixture/UpdateBackendConfigResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateBackendConfig)

responseUpdateBackendJob :: UpdateBackendJobResponse -> TestTree
responseUpdateBackendJob =
  res
    "UpdateBackendJobResponse"
    "fixture/UpdateBackendJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateBackendJob)

responseUpdateBackendStorage :: UpdateBackendStorageResponse -> TestTree
responseUpdateBackendStorage =
  res
    "UpdateBackendStorageResponse"
    "fixture/UpdateBackendStorageResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateBackendStorage)
