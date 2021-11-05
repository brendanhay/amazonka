{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.AmplifyBackend
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.AWS.Gen.AmplifyBackend where

import qualified Data.Proxy as Proxy
import Network.AWS.AmplifyBackend
import Test.AWS.AmplifyBackend.Internal
import Test.AWS.Fixture
import Test.AWS.Prelude
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
--         , requestDeleteToken $
--             newDeleteToken
--
--         , requestCreateBackendConfig $
--             newCreateBackendConfig
--
--         , requestListBackendJobs $
--             newListBackendJobs
--
--         , requestGetBackend $
--             newGetBackend
--
--         , requestGetBackendAuth $
--             newGetBackendAuth
--
--         , requestCreateBackendAuth $
--             newCreateBackendAuth
--
--         , requestRemoveBackendConfig $
--             newRemoveBackendConfig
--
--         , requestCreateBackend $
--             newCreateBackend
--
--         , requestGenerateBackendAPIModels $
--             newGenerateBackendAPIModels
--
--         , requestUpdateBackendConfig $
--             newUpdateBackendConfig
--
--         , requestCreateToken $
--             newCreateToken
--
--         , requestGetBackendAPI $
--             newGetBackendAPI
--
--         , requestUpdateBackendJob $
--             newUpdateBackendJob
--
--         , requestUpdateBackendAPI $
--             newUpdateBackendAPI
--
--         , requestDeleteBackendAPI $
--             newDeleteBackendAPI
--
--         , requestGetBackendJob $
--             newGetBackendJob
--
--         , requestDeleteBackend $
--             newDeleteBackend
--
--         , requestDeleteBackendAuth $
--             newDeleteBackendAuth
--
--         , requestUpdateBackendAuth $
--             newUpdateBackendAuth
--
--         , requestGetToken $
--             newGetToken
--
--         , requestRemoveAllBackends $
--             newRemoveAllBackends
--
--         , requestCreateBackendAPI $
--             newCreateBackendAPI
--
--         , requestGetBackendAPIModels $
--             newGetBackendAPIModels
--
--         , requestImportBackendAuth $
--             newImportBackendAuth
--
--           ]

--     , testGroup "response"
--         [ responseCloneBackend $
--             newCloneBackendResponse
--
--         , responseDeleteToken $
--             newDeleteTokenResponse
--
--         , responseCreateBackendConfig $
--             newCreateBackendConfigResponse
--
--         , responseListBackendJobs $
--             newListBackendJobsResponse
--
--         , responseGetBackend $
--             newGetBackendResponse
--
--         , responseGetBackendAuth $
--             newGetBackendAuthResponse
--
--         , responseCreateBackendAuth $
--             newCreateBackendAuthResponse
--
--         , responseRemoveBackendConfig $
--             newRemoveBackendConfigResponse
--
--         , responseCreateBackend $
--             newCreateBackendResponse
--
--         , responseGenerateBackendAPIModels $
--             newGenerateBackendAPIModelsResponse
--
--         , responseUpdateBackendConfig $
--             newUpdateBackendConfigResponse
--
--         , responseCreateToken $
--             newCreateTokenResponse
--
--         , responseGetBackendAPI $
--             newGetBackendAPIResponse
--
--         , responseUpdateBackendJob $
--             newUpdateBackendJobResponse
--
--         , responseUpdateBackendAPI $
--             newUpdateBackendAPIResponse
--
--         , responseDeleteBackendAPI $
--             newDeleteBackendAPIResponse
--
--         , responseGetBackendJob $
--             newGetBackendJobResponse
--
--         , responseDeleteBackend $
--             newDeleteBackendResponse
--
--         , responseDeleteBackendAuth $
--             newDeleteBackendAuthResponse
--
--         , responseUpdateBackendAuth $
--             newUpdateBackendAuthResponse
--
--         , responseGetToken $
--             newGetTokenResponse
--
--         , responseRemoveAllBackends $
--             newRemoveAllBackendsResponse
--
--         , responseCreateBackendAPI $
--             newCreateBackendAPIResponse
--
--         , responseGetBackendAPIModels $
--             newGetBackendAPIModelsResponse
--
--         , responseImportBackendAuth $
--             newImportBackendAuthResponse
--
--           ]
--     ]

-- Requests

requestCloneBackend :: CloneBackend -> TestTree
requestCloneBackend =
  req
    "CloneBackend"
    "fixture/CloneBackend.yaml"

requestDeleteToken :: DeleteToken -> TestTree
requestDeleteToken =
  req
    "DeleteToken"
    "fixture/DeleteToken.yaml"

requestCreateBackendConfig :: CreateBackendConfig -> TestTree
requestCreateBackendConfig =
  req
    "CreateBackendConfig"
    "fixture/CreateBackendConfig.yaml"

requestListBackendJobs :: ListBackendJobs -> TestTree
requestListBackendJobs =
  req
    "ListBackendJobs"
    "fixture/ListBackendJobs.yaml"

requestGetBackend :: GetBackend -> TestTree
requestGetBackend =
  req
    "GetBackend"
    "fixture/GetBackend.yaml"

requestGetBackendAuth :: GetBackendAuth -> TestTree
requestGetBackendAuth =
  req
    "GetBackendAuth"
    "fixture/GetBackendAuth.yaml"

requestCreateBackendAuth :: CreateBackendAuth -> TestTree
requestCreateBackendAuth =
  req
    "CreateBackendAuth"
    "fixture/CreateBackendAuth.yaml"

requestRemoveBackendConfig :: RemoveBackendConfig -> TestTree
requestRemoveBackendConfig =
  req
    "RemoveBackendConfig"
    "fixture/RemoveBackendConfig.yaml"

requestCreateBackend :: CreateBackend -> TestTree
requestCreateBackend =
  req
    "CreateBackend"
    "fixture/CreateBackend.yaml"

requestGenerateBackendAPIModels :: GenerateBackendAPIModels -> TestTree
requestGenerateBackendAPIModels =
  req
    "GenerateBackendAPIModels"
    "fixture/GenerateBackendAPIModels.yaml"

requestUpdateBackendConfig :: UpdateBackendConfig -> TestTree
requestUpdateBackendConfig =
  req
    "UpdateBackendConfig"
    "fixture/UpdateBackendConfig.yaml"

requestCreateToken :: CreateToken -> TestTree
requestCreateToken =
  req
    "CreateToken"
    "fixture/CreateToken.yaml"

requestGetBackendAPI :: GetBackendAPI -> TestTree
requestGetBackendAPI =
  req
    "GetBackendAPI"
    "fixture/GetBackendAPI.yaml"

requestUpdateBackendJob :: UpdateBackendJob -> TestTree
requestUpdateBackendJob =
  req
    "UpdateBackendJob"
    "fixture/UpdateBackendJob.yaml"

requestUpdateBackendAPI :: UpdateBackendAPI -> TestTree
requestUpdateBackendAPI =
  req
    "UpdateBackendAPI"
    "fixture/UpdateBackendAPI.yaml"

requestDeleteBackendAPI :: DeleteBackendAPI -> TestTree
requestDeleteBackendAPI =
  req
    "DeleteBackendAPI"
    "fixture/DeleteBackendAPI.yaml"

requestGetBackendJob :: GetBackendJob -> TestTree
requestGetBackendJob =
  req
    "GetBackendJob"
    "fixture/GetBackendJob.yaml"

requestDeleteBackend :: DeleteBackend -> TestTree
requestDeleteBackend =
  req
    "DeleteBackend"
    "fixture/DeleteBackend.yaml"

requestDeleteBackendAuth :: DeleteBackendAuth -> TestTree
requestDeleteBackendAuth =
  req
    "DeleteBackendAuth"
    "fixture/DeleteBackendAuth.yaml"

requestUpdateBackendAuth :: UpdateBackendAuth -> TestTree
requestUpdateBackendAuth =
  req
    "UpdateBackendAuth"
    "fixture/UpdateBackendAuth.yaml"

requestGetToken :: GetToken -> TestTree
requestGetToken =
  req
    "GetToken"
    "fixture/GetToken.yaml"

requestRemoveAllBackends :: RemoveAllBackends -> TestTree
requestRemoveAllBackends =
  req
    "RemoveAllBackends"
    "fixture/RemoveAllBackends.yaml"

requestCreateBackendAPI :: CreateBackendAPI -> TestTree
requestCreateBackendAPI =
  req
    "CreateBackendAPI"
    "fixture/CreateBackendAPI.yaml"

requestGetBackendAPIModels :: GetBackendAPIModels -> TestTree
requestGetBackendAPIModels =
  req
    "GetBackendAPIModels"
    "fixture/GetBackendAPIModels.yaml"

requestImportBackendAuth :: ImportBackendAuth -> TestTree
requestImportBackendAuth =
  req
    "ImportBackendAuth"
    "fixture/ImportBackendAuth.yaml"

-- Responses

responseCloneBackend :: CloneBackendResponse -> TestTree
responseCloneBackend =
  res
    "CloneBackendResponse"
    "fixture/CloneBackendResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CloneBackend)

responseDeleteToken :: DeleteTokenResponse -> TestTree
responseDeleteToken =
  res
    "DeleteTokenResponse"
    "fixture/DeleteTokenResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteToken)

responseCreateBackendConfig :: CreateBackendConfigResponse -> TestTree
responseCreateBackendConfig =
  res
    "CreateBackendConfigResponse"
    "fixture/CreateBackendConfigResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateBackendConfig)

responseListBackendJobs :: ListBackendJobsResponse -> TestTree
responseListBackendJobs =
  res
    "ListBackendJobsResponse"
    "fixture/ListBackendJobsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListBackendJobs)

responseGetBackend :: GetBackendResponse -> TestTree
responseGetBackend =
  res
    "GetBackendResponse"
    "fixture/GetBackendResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetBackend)

responseGetBackendAuth :: GetBackendAuthResponse -> TestTree
responseGetBackendAuth =
  res
    "GetBackendAuthResponse"
    "fixture/GetBackendAuthResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetBackendAuth)

responseCreateBackendAuth :: CreateBackendAuthResponse -> TestTree
responseCreateBackendAuth =
  res
    "CreateBackendAuthResponse"
    "fixture/CreateBackendAuthResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateBackendAuth)

responseRemoveBackendConfig :: RemoveBackendConfigResponse -> TestTree
responseRemoveBackendConfig =
  res
    "RemoveBackendConfigResponse"
    "fixture/RemoveBackendConfigResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RemoveBackendConfig)

responseCreateBackend :: CreateBackendResponse -> TestTree
responseCreateBackend =
  res
    "CreateBackendResponse"
    "fixture/CreateBackendResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateBackend)

responseGenerateBackendAPIModels :: GenerateBackendAPIModelsResponse -> TestTree
responseGenerateBackendAPIModels =
  res
    "GenerateBackendAPIModelsResponse"
    "fixture/GenerateBackendAPIModelsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GenerateBackendAPIModels)

responseUpdateBackendConfig :: UpdateBackendConfigResponse -> TestTree
responseUpdateBackendConfig =
  res
    "UpdateBackendConfigResponse"
    "fixture/UpdateBackendConfigResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateBackendConfig)

responseCreateToken :: CreateTokenResponse -> TestTree
responseCreateToken =
  res
    "CreateTokenResponse"
    "fixture/CreateTokenResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateToken)

responseGetBackendAPI :: GetBackendAPIResponse -> TestTree
responseGetBackendAPI =
  res
    "GetBackendAPIResponse"
    "fixture/GetBackendAPIResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetBackendAPI)

responseUpdateBackendJob :: UpdateBackendJobResponse -> TestTree
responseUpdateBackendJob =
  res
    "UpdateBackendJobResponse"
    "fixture/UpdateBackendJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateBackendJob)

responseUpdateBackendAPI :: UpdateBackendAPIResponse -> TestTree
responseUpdateBackendAPI =
  res
    "UpdateBackendAPIResponse"
    "fixture/UpdateBackendAPIResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateBackendAPI)

responseDeleteBackendAPI :: DeleteBackendAPIResponse -> TestTree
responseDeleteBackendAPI =
  res
    "DeleteBackendAPIResponse"
    "fixture/DeleteBackendAPIResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteBackendAPI)

responseGetBackendJob :: GetBackendJobResponse -> TestTree
responseGetBackendJob =
  res
    "GetBackendJobResponse"
    "fixture/GetBackendJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetBackendJob)

responseDeleteBackend :: DeleteBackendResponse -> TestTree
responseDeleteBackend =
  res
    "DeleteBackendResponse"
    "fixture/DeleteBackendResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteBackend)

responseDeleteBackendAuth :: DeleteBackendAuthResponse -> TestTree
responseDeleteBackendAuth =
  res
    "DeleteBackendAuthResponse"
    "fixture/DeleteBackendAuthResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteBackendAuth)

responseUpdateBackendAuth :: UpdateBackendAuthResponse -> TestTree
responseUpdateBackendAuth =
  res
    "UpdateBackendAuthResponse"
    "fixture/UpdateBackendAuthResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateBackendAuth)

responseGetToken :: GetTokenResponse -> TestTree
responseGetToken =
  res
    "GetTokenResponse"
    "fixture/GetTokenResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetToken)

responseRemoveAllBackends :: RemoveAllBackendsResponse -> TestTree
responseRemoveAllBackends =
  res
    "RemoveAllBackendsResponse"
    "fixture/RemoveAllBackendsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RemoveAllBackends)

responseCreateBackendAPI :: CreateBackendAPIResponse -> TestTree
responseCreateBackendAPI =
  res
    "CreateBackendAPIResponse"
    "fixture/CreateBackendAPIResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateBackendAPI)

responseGetBackendAPIModels :: GetBackendAPIModelsResponse -> TestTree
responseGetBackendAPIModels =
  res
    "GetBackendAPIModelsResponse"
    "fixture/GetBackendAPIModelsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetBackendAPIModels)

responseImportBackendAuth :: ImportBackendAuthResponse -> TestTree
responseImportBackendAuth =
  res
    "ImportBackendAuthResponse"
    "fixture/ImportBackendAuthResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ImportBackendAuth)
