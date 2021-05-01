{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.ServerlessApplicationRepository
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.AWS.Gen.ServerlessApplicationRepository where

import Data.Proxy
import Network.AWS.ServerlessApplicationRepository
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.AWS.ServerlessApplicationRepository.Internal
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestGetApplicationPolicy $
--             newGetApplicationPolicy
--
--         , requestGetCloudFormationTemplate $
--             newGetCloudFormationTemplate
--
--         , requestUnshareApplication $
--             newUnshareApplication
--
--         , requestCreateApplicationVersion $
--             newCreateApplicationVersion
--
--         , requestCreateApplication $
--             newCreateApplication
--
--         , requestCreateCloudFormationChangeSet $
--             newCreateCloudFormationChangeSet
--
--         , requestGetApplication $
--             newGetApplication
--
--         , requestListApplicationDependencies $
--             newListApplicationDependencies
--
--         , requestListApplicationVersions $
--             newListApplicationVersions
--
--         , requestCreateCloudFormationTemplate $
--             newCreateCloudFormationTemplate
--
--         , requestPutApplicationPolicy $
--             newPutApplicationPolicy
--
--         , requestDeleteApplication $
--             newDeleteApplication
--
--         , requestListApplications $
--             newListApplications
--
--         , requestUpdateApplication $
--             newUpdateApplication
--
--           ]

--     , testGroup "response"
--         [ responseGetApplicationPolicy $
--             newGetApplicationPolicyResponse
--
--         , responseGetCloudFormationTemplate $
--             newGetCloudFormationTemplateResponse
--
--         , responseUnshareApplication $
--             newUnshareApplicationResponse
--
--         , responseCreateApplicationVersion $
--             newCreateApplicationVersionResponse
--
--         , responseCreateApplication $
--             newCreateApplicationResponse
--
--         , responseCreateCloudFormationChangeSet $
--             newCreateCloudFormationChangeSetResponse
--
--         , responseGetApplication $
--             newGetApplicationResponse
--
--         , responseListApplicationDependencies $
--             newListApplicationDependenciesResponse
--
--         , responseListApplicationVersions $
--             newListApplicationVersionsResponse
--
--         , responseCreateCloudFormationTemplate $
--             newCreateCloudFormationTemplateResponse
--
--         , responsePutApplicationPolicy $
--             newPutApplicationPolicyResponse
--
--         , responseDeleteApplication $
--             newDeleteApplicationResponse
--
--         , responseListApplications $
--             newListApplicationsResponse
--
--         , responseUpdateApplication $
--             newUpdateApplicationResponse
--
--           ]
--     ]

-- Requests

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

requestUnshareApplication :: UnshareApplication -> TestTree
requestUnshareApplication =
  req
    "UnshareApplication"
    "fixture/UnshareApplication.yaml"

requestCreateApplicationVersion :: CreateApplicationVersion -> TestTree
requestCreateApplicationVersion =
  req
    "CreateApplicationVersion"
    "fixture/CreateApplicationVersion.yaml"

requestCreateApplication :: CreateApplication -> TestTree
requestCreateApplication =
  req
    "CreateApplication"
    "fixture/CreateApplication.yaml"

requestCreateCloudFormationChangeSet :: CreateCloudFormationChangeSet -> TestTree
requestCreateCloudFormationChangeSet =
  req
    "CreateCloudFormationChangeSet"
    "fixture/CreateCloudFormationChangeSet.yaml"

requestGetApplication :: GetApplication -> TestTree
requestGetApplication =
  req
    "GetApplication"
    "fixture/GetApplication.yaml"

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

requestCreateCloudFormationTemplate :: CreateCloudFormationTemplate -> TestTree
requestCreateCloudFormationTemplate =
  req
    "CreateCloudFormationTemplate"
    "fixture/CreateCloudFormationTemplate.yaml"

requestPutApplicationPolicy :: PutApplicationPolicy -> TestTree
requestPutApplicationPolicy =
  req
    "PutApplicationPolicy"
    "fixture/PutApplicationPolicy.yaml"

requestDeleteApplication :: DeleteApplication -> TestTree
requestDeleteApplication =
  req
    "DeleteApplication"
    "fixture/DeleteApplication.yaml"

requestListApplications :: ListApplications -> TestTree
requestListApplications =
  req
    "ListApplications"
    "fixture/ListApplications.yaml"

requestUpdateApplication :: UpdateApplication -> TestTree
requestUpdateApplication =
  req
    "UpdateApplication"
    "fixture/UpdateApplication.yaml"

-- Responses

responseGetApplicationPolicy :: GetApplicationPolicyResponse -> TestTree
responseGetApplicationPolicy =
  res
    "GetApplicationPolicyResponse"
    "fixture/GetApplicationPolicyResponse.proto"
    defaultService
    (Proxy :: Proxy GetApplicationPolicy)

responseGetCloudFormationTemplate :: GetCloudFormationTemplateResponse -> TestTree
responseGetCloudFormationTemplate =
  res
    "GetCloudFormationTemplateResponse"
    "fixture/GetCloudFormationTemplateResponse.proto"
    defaultService
    (Proxy :: Proxy GetCloudFormationTemplate)

responseUnshareApplication :: UnshareApplicationResponse -> TestTree
responseUnshareApplication =
  res
    "UnshareApplicationResponse"
    "fixture/UnshareApplicationResponse.proto"
    defaultService
    (Proxy :: Proxy UnshareApplication)

responseCreateApplicationVersion :: CreateApplicationVersionResponse -> TestTree
responseCreateApplicationVersion =
  res
    "CreateApplicationVersionResponse"
    "fixture/CreateApplicationVersionResponse.proto"
    defaultService
    (Proxy :: Proxy CreateApplicationVersion)

responseCreateApplication :: CreateApplicationResponse -> TestTree
responseCreateApplication =
  res
    "CreateApplicationResponse"
    "fixture/CreateApplicationResponse.proto"
    defaultService
    (Proxy :: Proxy CreateApplication)

responseCreateCloudFormationChangeSet :: CreateCloudFormationChangeSetResponse -> TestTree
responseCreateCloudFormationChangeSet =
  res
    "CreateCloudFormationChangeSetResponse"
    "fixture/CreateCloudFormationChangeSetResponse.proto"
    defaultService
    (Proxy :: Proxy CreateCloudFormationChangeSet)

responseGetApplication :: GetApplicationResponse -> TestTree
responseGetApplication =
  res
    "GetApplicationResponse"
    "fixture/GetApplicationResponse.proto"
    defaultService
    (Proxy :: Proxy GetApplication)

responseListApplicationDependencies :: ListApplicationDependenciesResponse -> TestTree
responseListApplicationDependencies =
  res
    "ListApplicationDependenciesResponse"
    "fixture/ListApplicationDependenciesResponse.proto"
    defaultService
    (Proxy :: Proxy ListApplicationDependencies)

responseListApplicationVersions :: ListApplicationVersionsResponse -> TestTree
responseListApplicationVersions =
  res
    "ListApplicationVersionsResponse"
    "fixture/ListApplicationVersionsResponse.proto"
    defaultService
    (Proxy :: Proxy ListApplicationVersions)

responseCreateCloudFormationTemplate :: CreateCloudFormationTemplateResponse -> TestTree
responseCreateCloudFormationTemplate =
  res
    "CreateCloudFormationTemplateResponse"
    "fixture/CreateCloudFormationTemplateResponse.proto"
    defaultService
    (Proxy :: Proxy CreateCloudFormationTemplate)

responsePutApplicationPolicy :: PutApplicationPolicyResponse -> TestTree
responsePutApplicationPolicy =
  res
    "PutApplicationPolicyResponse"
    "fixture/PutApplicationPolicyResponse.proto"
    defaultService
    (Proxy :: Proxy PutApplicationPolicy)

responseDeleteApplication :: DeleteApplicationResponse -> TestTree
responseDeleteApplication =
  res
    "DeleteApplicationResponse"
    "fixture/DeleteApplicationResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteApplication)

responseListApplications :: ListApplicationsResponse -> TestTree
responseListApplications =
  res
    "ListApplicationsResponse"
    "fixture/ListApplicationsResponse.proto"
    defaultService
    (Proxy :: Proxy ListApplications)

responseUpdateApplication :: UpdateApplicationResponse -> TestTree
responseUpdateApplication =
  res
    "UpdateApplicationResponse"
    "fixture/UpdateApplicationResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateApplication)
