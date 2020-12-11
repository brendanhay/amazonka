{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.ServerlessApplicationRepository
-- Copyright   : (c) 2013-2020 Brendan Hay
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
--             mkGetApplicationPolicy
--
--         , requestCreateApplicationVersion $
--             mkCreateApplicationVersion
--
--         , requestUnshareApplication $
--             mkUnshareApplication
--
--         , requestDeleteApplication $
--             mkDeleteApplication
--
--         , requestUpdateApplication $
--             mkUpdateApplication
--
--         , requestCreateCloudFormationTemplate $
--             mkCreateCloudFormationTemplate
--
--         , requestCreateApplication $
--             mkCreateApplication
--
--         , requestListApplicationDependencies $
--             mkListApplicationDependencies
--
--         , requestListApplicationVersions $
--             mkListApplicationVersions
--
--         , requestGetApplication $
--             mkGetApplication
--
--         , requestGetCloudFormationTemplate $
--             mkGetCloudFormationTemplate
--
--         , requestCreateCloudFormationChangeSet $
--             mkCreateCloudFormationChangeSet
--
--         , requestPutApplicationPolicy $
--             mkPutApplicationPolicy
--
--         , requestListApplications $
--             mkListApplications
--
--           ]

--     , testGroup "response"
--         [ responseGetApplicationPolicy $
--             mkGetApplicationPolicyResponse
--
--         , responseCreateApplicationVersion $
--             mkCreateApplicationVersionResponse
--
--         , responseUnshareApplication $
--             mkUnshareApplicationResponse
--
--         , responseDeleteApplication $
--             mkDeleteApplicationResponse
--
--         , responseUpdateApplication $
--             mkUpdateApplicationResponse
--
--         , responseCreateCloudFormationTemplate $
--             mkCreateCloudFormationTemplateResponse
--
--         , responseCreateApplication $
--             mkCreateApplicationResponse
--
--         , responseListApplicationDependencies $
--             mkListApplicationDependenciesResponse
--
--         , responseListApplicationVersions $
--             mkListApplicationVersionsResponse
--
--         , responseGetApplication $
--             mkGetApplicationResponse
--
--         , responseGetCloudFormationTemplate $
--             mkGetCloudFormationTemplateResponse
--
--         , responseCreateCloudFormationChangeSet $
--             mkCreateCloudFormationChangeSetResponse
--
--         , responsePutApplicationPolicy $
--             mkPutApplicationPolicyResponse
--
--         , responseListApplications $
--             mkListApplicationsResponse
--
--           ]
--     ]

-- Requests

requestGetApplicationPolicy :: GetApplicationPolicy -> TestTree
requestGetApplicationPolicy =
  req
    "GetApplicationPolicy"
    "fixture/GetApplicationPolicy.yaml"

requestCreateApplicationVersion :: CreateApplicationVersion -> TestTree
requestCreateApplicationVersion =
  req
    "CreateApplicationVersion"
    "fixture/CreateApplicationVersion.yaml"

requestUnshareApplication :: UnshareApplication -> TestTree
requestUnshareApplication =
  req
    "UnshareApplication"
    "fixture/UnshareApplication.yaml"

requestDeleteApplication :: DeleteApplication -> TestTree
requestDeleteApplication =
  req
    "DeleteApplication"
    "fixture/DeleteApplication.yaml"

requestUpdateApplication :: UpdateApplication -> TestTree
requestUpdateApplication =
  req
    "UpdateApplication"
    "fixture/UpdateApplication.yaml"

requestCreateCloudFormationTemplate :: CreateCloudFormationTemplate -> TestTree
requestCreateCloudFormationTemplate =
  req
    "CreateCloudFormationTemplate"
    "fixture/CreateCloudFormationTemplate.yaml"

requestCreateApplication :: CreateApplication -> TestTree
requestCreateApplication =
  req
    "CreateApplication"
    "fixture/CreateApplication.yaml"

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

requestGetApplication :: GetApplication -> TestTree
requestGetApplication =
  req
    "GetApplication"
    "fixture/GetApplication.yaml"

requestGetCloudFormationTemplate :: GetCloudFormationTemplate -> TestTree
requestGetCloudFormationTemplate =
  req
    "GetCloudFormationTemplate"
    "fixture/GetCloudFormationTemplate.yaml"

requestCreateCloudFormationChangeSet :: CreateCloudFormationChangeSet -> TestTree
requestCreateCloudFormationChangeSet =
  req
    "CreateCloudFormationChangeSet"
    "fixture/CreateCloudFormationChangeSet.yaml"

requestPutApplicationPolicy :: PutApplicationPolicy -> TestTree
requestPutApplicationPolicy =
  req
    "PutApplicationPolicy"
    "fixture/PutApplicationPolicy.yaml"

requestListApplications :: ListApplications -> TestTree
requestListApplications =
  req
    "ListApplications"
    "fixture/ListApplications.yaml"

-- Responses

responseGetApplicationPolicy :: GetApplicationPolicyResponse -> TestTree
responseGetApplicationPolicy =
  res
    "GetApplicationPolicyResponse"
    "fixture/GetApplicationPolicyResponse.proto"
    serverlessApplicationRepositoryService
    (Proxy :: Proxy GetApplicationPolicy)

responseCreateApplicationVersion :: CreateApplicationVersionResponse -> TestTree
responseCreateApplicationVersion =
  res
    "CreateApplicationVersionResponse"
    "fixture/CreateApplicationVersionResponse.proto"
    serverlessApplicationRepositoryService
    (Proxy :: Proxy CreateApplicationVersion)

responseUnshareApplication :: UnshareApplicationResponse -> TestTree
responseUnshareApplication =
  res
    "UnshareApplicationResponse"
    "fixture/UnshareApplicationResponse.proto"
    serverlessApplicationRepositoryService
    (Proxy :: Proxy UnshareApplication)

responseDeleteApplication :: DeleteApplicationResponse -> TestTree
responseDeleteApplication =
  res
    "DeleteApplicationResponse"
    "fixture/DeleteApplicationResponse.proto"
    serverlessApplicationRepositoryService
    (Proxy :: Proxy DeleteApplication)

responseUpdateApplication :: UpdateApplicationResponse -> TestTree
responseUpdateApplication =
  res
    "UpdateApplicationResponse"
    "fixture/UpdateApplicationResponse.proto"
    serverlessApplicationRepositoryService
    (Proxy :: Proxy UpdateApplication)

responseCreateCloudFormationTemplate :: CreateCloudFormationTemplateResponse -> TestTree
responseCreateCloudFormationTemplate =
  res
    "CreateCloudFormationTemplateResponse"
    "fixture/CreateCloudFormationTemplateResponse.proto"
    serverlessApplicationRepositoryService
    (Proxy :: Proxy CreateCloudFormationTemplate)

responseCreateApplication :: CreateApplicationResponse -> TestTree
responseCreateApplication =
  res
    "CreateApplicationResponse"
    "fixture/CreateApplicationResponse.proto"
    serverlessApplicationRepositoryService
    (Proxy :: Proxy CreateApplication)

responseListApplicationDependencies :: ListApplicationDependenciesResponse -> TestTree
responseListApplicationDependencies =
  res
    "ListApplicationDependenciesResponse"
    "fixture/ListApplicationDependenciesResponse.proto"
    serverlessApplicationRepositoryService
    (Proxy :: Proxy ListApplicationDependencies)

responseListApplicationVersions :: ListApplicationVersionsResponse -> TestTree
responseListApplicationVersions =
  res
    "ListApplicationVersionsResponse"
    "fixture/ListApplicationVersionsResponse.proto"
    serverlessApplicationRepositoryService
    (Proxy :: Proxy ListApplicationVersions)

responseGetApplication :: GetApplicationResponse -> TestTree
responseGetApplication =
  res
    "GetApplicationResponse"
    "fixture/GetApplicationResponse.proto"
    serverlessApplicationRepositoryService
    (Proxy :: Proxy GetApplication)

responseGetCloudFormationTemplate :: GetCloudFormationTemplateResponse -> TestTree
responseGetCloudFormationTemplate =
  res
    "GetCloudFormationTemplateResponse"
    "fixture/GetCloudFormationTemplateResponse.proto"
    serverlessApplicationRepositoryService
    (Proxy :: Proxy GetCloudFormationTemplate)

responseCreateCloudFormationChangeSet :: CreateCloudFormationChangeSetResponse -> TestTree
responseCreateCloudFormationChangeSet =
  res
    "CreateCloudFormationChangeSetResponse"
    "fixture/CreateCloudFormationChangeSetResponse.proto"
    serverlessApplicationRepositoryService
    (Proxy :: Proxy CreateCloudFormationChangeSet)

responsePutApplicationPolicy :: PutApplicationPolicyResponse -> TestTree
responsePutApplicationPolicy =
  res
    "PutApplicationPolicyResponse"
    "fixture/PutApplicationPolicyResponse.proto"
    serverlessApplicationRepositoryService
    (Proxy :: Proxy PutApplicationPolicy)

responseListApplications :: ListApplicationsResponse -> TestTree
responseListApplications =
  res
    "ListApplicationsResponse"
    "fixture/ListApplicationsResponse.proto"
    serverlessApplicationRepositoryService
    (Proxy :: Proxy ListApplications)
