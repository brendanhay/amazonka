{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.ServerlessApplicationRepository
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.ServerlessApplicationRepository where

import Amazonka.ServerlessApplicationRepository
import qualified Data.Proxy as Proxy
import Test.Amazonka.Fixture
import Test.Amazonka.Prelude
import Test.Amazonka.ServerlessApplicationRepository.Internal
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestCreateApplication $
--             newCreateApplication
--
--         , requestCreateApplicationVersion $
--             newCreateApplicationVersion
--
--         , requestCreateCloudFormationChangeSet $
--             newCreateCloudFormationChangeSet
--
--         , requestCreateCloudFormationTemplate $
--             newCreateCloudFormationTemplate
--
--         , requestDeleteApplication $
--             newDeleteApplication
--
--         , requestGetApplication $
--             newGetApplication
--
--         , requestGetApplicationPolicy $
--             newGetApplicationPolicy
--
--         , requestGetCloudFormationTemplate $
--             newGetCloudFormationTemplate
--
--         , requestListApplicationDependencies $
--             newListApplicationDependencies
--
--         , requestListApplicationVersions $
--             newListApplicationVersions
--
--         , requestListApplications $
--             newListApplications
--
--         , requestPutApplicationPolicy $
--             newPutApplicationPolicy
--
--         , requestUnshareApplication $
--             newUnshareApplication
--
--         , requestUpdateApplication $
--             newUpdateApplication
--
--           ]

--     , testGroup "response"
--         [ responseCreateApplication $
--             newCreateApplicationResponse
--
--         , responseCreateApplicationVersion $
--             newCreateApplicationVersionResponse
--
--         , responseCreateCloudFormationChangeSet $
--             newCreateCloudFormationChangeSetResponse
--
--         , responseCreateCloudFormationTemplate $
--             newCreateCloudFormationTemplateResponse
--
--         , responseDeleteApplication $
--             newDeleteApplicationResponse
--
--         , responseGetApplication $
--             newGetApplicationResponse
--
--         , responseGetApplicationPolicy $
--             newGetApplicationPolicyResponse
--
--         , responseGetCloudFormationTemplate $
--             newGetCloudFormationTemplateResponse
--
--         , responseListApplicationDependencies $
--             newListApplicationDependenciesResponse
--
--         , responseListApplicationVersions $
--             newListApplicationVersionsResponse
--
--         , responseListApplications $
--             newListApplicationsResponse
--
--         , responsePutApplicationPolicy $
--             newPutApplicationPolicyResponse
--
--         , responseUnshareApplication $
--             newUnshareApplicationResponse
--
--         , responseUpdateApplication $
--             newUpdateApplicationResponse
--
--           ]
--     ]

-- Requests

requestCreateApplication :: CreateApplication -> TestTree
requestCreateApplication =
  req
    "CreateApplication"
    "fixture/CreateApplication.yaml"

requestCreateApplicationVersion :: CreateApplicationVersion -> TestTree
requestCreateApplicationVersion =
  req
    "CreateApplicationVersion"
    "fixture/CreateApplicationVersion.yaml"

requestCreateCloudFormationChangeSet :: CreateCloudFormationChangeSet -> TestTree
requestCreateCloudFormationChangeSet =
  req
    "CreateCloudFormationChangeSet"
    "fixture/CreateCloudFormationChangeSet.yaml"

requestCreateCloudFormationTemplate :: CreateCloudFormationTemplate -> TestTree
requestCreateCloudFormationTemplate =
  req
    "CreateCloudFormationTemplate"
    "fixture/CreateCloudFormationTemplate.yaml"

requestDeleteApplication :: DeleteApplication -> TestTree
requestDeleteApplication =
  req
    "DeleteApplication"
    "fixture/DeleteApplication.yaml"

requestGetApplication :: GetApplication -> TestTree
requestGetApplication =
  req
    "GetApplication"
    "fixture/GetApplication.yaml"

requestGetApplicationPolicy :: GetApplicationPolicy -> TestTree
requestGetApplicationPolicy =
  req
    "GetApplicationPolicy"
    "fixture/GetApplicationPolicy.yaml"

requestGetCloudFormationTemplate :: GetCloudFormationTemplate -> TestTree
requestGetCloudFormationTemplate =
  req
    "GetCloudFormationTemplate"
    "fixture/GetCloudFormationTemplate.yaml"

requestListApplicationDependencies :: ListApplicationDependencies -> TestTree
requestListApplicationDependencies =
  req
    "ListApplicationDependencies"
    "fixture/ListApplicationDependencies.yaml"

requestListApplicationVersions :: ListApplicationVersions -> TestTree
requestListApplicationVersions =
  req
    "ListApplicationVersions"
    "fixture/ListApplicationVersions.yaml"

requestListApplications :: ListApplications -> TestTree
requestListApplications =
  req
    "ListApplications"
    "fixture/ListApplications.yaml"

requestPutApplicationPolicy :: PutApplicationPolicy -> TestTree
requestPutApplicationPolicy =
  req
    "PutApplicationPolicy"
    "fixture/PutApplicationPolicy.yaml"

requestUnshareApplication :: UnshareApplication -> TestTree
requestUnshareApplication =
  req
    "UnshareApplication"
    "fixture/UnshareApplication.yaml"

requestUpdateApplication :: UpdateApplication -> TestTree
requestUpdateApplication =
  req
    "UpdateApplication"
    "fixture/UpdateApplication.yaml"

-- Responses

responseCreateApplication :: CreateApplicationResponse -> TestTree
responseCreateApplication =
  res
    "CreateApplicationResponse"
    "fixture/CreateApplicationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateApplication)

responseCreateApplicationVersion :: CreateApplicationVersionResponse -> TestTree
responseCreateApplicationVersion =
  res
    "CreateApplicationVersionResponse"
    "fixture/CreateApplicationVersionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateApplicationVersion)

responseCreateCloudFormationChangeSet :: CreateCloudFormationChangeSetResponse -> TestTree
responseCreateCloudFormationChangeSet =
  res
    "CreateCloudFormationChangeSetResponse"
    "fixture/CreateCloudFormationChangeSetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateCloudFormationChangeSet)

responseCreateCloudFormationTemplate :: CreateCloudFormationTemplateResponse -> TestTree
responseCreateCloudFormationTemplate =
  res
    "CreateCloudFormationTemplateResponse"
    "fixture/CreateCloudFormationTemplateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateCloudFormationTemplate)

responseDeleteApplication :: DeleteApplicationResponse -> TestTree
responseDeleteApplication =
  res
    "DeleteApplicationResponse"
    "fixture/DeleteApplicationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteApplication)

responseGetApplication :: GetApplicationResponse -> TestTree
responseGetApplication =
  res
    "GetApplicationResponse"
    "fixture/GetApplicationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetApplication)

responseGetApplicationPolicy :: GetApplicationPolicyResponse -> TestTree
responseGetApplicationPolicy =
  res
    "GetApplicationPolicyResponse"
    "fixture/GetApplicationPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetApplicationPolicy)

responseGetCloudFormationTemplate :: GetCloudFormationTemplateResponse -> TestTree
responseGetCloudFormationTemplate =
  res
    "GetCloudFormationTemplateResponse"
    "fixture/GetCloudFormationTemplateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetCloudFormationTemplate)

responseListApplicationDependencies :: ListApplicationDependenciesResponse -> TestTree
responseListApplicationDependencies =
  res
    "ListApplicationDependenciesResponse"
    "fixture/ListApplicationDependenciesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListApplicationDependencies)

responseListApplicationVersions :: ListApplicationVersionsResponse -> TestTree
responseListApplicationVersions =
  res
    "ListApplicationVersionsResponse"
    "fixture/ListApplicationVersionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListApplicationVersions)

responseListApplications :: ListApplicationsResponse -> TestTree
responseListApplications =
  res
    "ListApplicationsResponse"
    "fixture/ListApplicationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListApplications)

responsePutApplicationPolicy :: PutApplicationPolicyResponse -> TestTree
responsePutApplicationPolicy =
  res
    "PutApplicationPolicyResponse"
    "fixture/PutApplicationPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutApplicationPolicy)

responseUnshareApplication :: UnshareApplicationResponse -> TestTree
responseUnshareApplication =
  res
    "UnshareApplicationResponse"
    "fixture/UnshareApplicationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UnshareApplication)

responseUpdateApplication :: UpdateApplicationResponse -> TestTree
responseUpdateApplication =
  res
    "UpdateApplicationResponse"
    "fixture/UpdateApplicationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateApplication)
