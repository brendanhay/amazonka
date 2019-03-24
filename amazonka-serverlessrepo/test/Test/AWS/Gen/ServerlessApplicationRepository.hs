{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.ServerlessApplicationRepository
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
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
--             getApplicationPolicy
--
--         , requestCreateApplicationVersion $
--             createApplicationVersion
--
--         , requestDeleteApplication $
--             deleteApplication
--
--         , requestUpdateApplication $
--             updateApplication
--
--         , requestCreateCloudFormationTemplate $
--             createCloudFormationTemplate
--
--         , requestCreateApplication $
--             createApplication
--
--         , requestListApplicationDependencies $
--             listApplicationDependencies
--
--         , requestListApplicationVersions $
--             listApplicationVersions
--
--         , requestGetApplication $
--             getApplication
--
--         , requestGetCloudFormationTemplate $
--             getCloudFormationTemplate
--
--         , requestCreateCloudFormationChangeSet $
--             createCloudFormationChangeSet
--
--         , requestPutApplicationPolicy $
--             putApplicationPolicy
--
--         , requestListApplications $
--             listApplications
--
--           ]

--     , testGroup "response"
--         [ responseGetApplicationPolicy $
--             getApplicationPolicyResponse
--
--         , responseCreateApplicationVersion $
--             createApplicationVersionResponse
--
--         , responseDeleteApplication $
--             deleteApplicationResponse
--
--         , responseUpdateApplication $
--             updateApplicationResponse
--
--         , responseCreateCloudFormationTemplate $
--             createCloudFormationTemplateResponse
--
--         , responseCreateApplication $
--             createApplicationResponse
--
--         , responseListApplicationDependencies $
--             listApplicationDependenciesResponse
--
--         , responseListApplicationVersions $
--             listApplicationVersionsResponse
--
--         , responseGetApplication $
--             getApplicationResponse
--
--         , responseGetCloudFormationTemplate $
--             getCloudFormationTemplateResponse
--
--         , responseCreateCloudFormationChangeSet $
--             createCloudFormationChangeSetResponse
--
--         , responsePutApplicationPolicy $
--             putApplicationPolicyResponse
--
--         , responseListApplications $
--             listApplicationsResponse
--
--           ]
--     ]

-- Requests

requestGetApplicationPolicy :: GetApplicationPolicy -> TestTree
requestGetApplicationPolicy = req
    "GetApplicationPolicy"
    "fixture/GetApplicationPolicy.yaml"

requestCreateApplicationVersion :: CreateApplicationVersion -> TestTree
requestCreateApplicationVersion = req
    "CreateApplicationVersion"
    "fixture/CreateApplicationVersion.yaml"

requestDeleteApplication :: DeleteApplication -> TestTree
requestDeleteApplication = req
    "DeleteApplication"
    "fixture/DeleteApplication.yaml"

requestUpdateApplication :: UpdateApplication -> TestTree
requestUpdateApplication = req
    "UpdateApplication"
    "fixture/UpdateApplication.yaml"

requestCreateCloudFormationTemplate :: CreateCloudFormationTemplate -> TestTree
requestCreateCloudFormationTemplate = req
    "CreateCloudFormationTemplate"
    "fixture/CreateCloudFormationTemplate.yaml"

requestCreateApplication :: CreateApplication -> TestTree
requestCreateApplication = req
    "CreateApplication"
    "fixture/CreateApplication.yaml"

requestListApplicationDependencies :: ListApplicationDependencies -> TestTree
requestListApplicationDependencies = req
    "ListApplicationDependencies"
    "fixture/ListApplicationDependencies.yaml"

requestListApplicationVersions :: ListApplicationVersions -> TestTree
requestListApplicationVersions = req
    "ListApplicationVersions"
    "fixture/ListApplicationVersions.yaml"

requestGetApplication :: GetApplication -> TestTree
requestGetApplication = req
    "GetApplication"
    "fixture/GetApplication.yaml"

requestGetCloudFormationTemplate :: GetCloudFormationTemplate -> TestTree
requestGetCloudFormationTemplate = req
    "GetCloudFormationTemplate"
    "fixture/GetCloudFormationTemplate.yaml"

requestCreateCloudFormationChangeSet :: CreateCloudFormationChangeSet -> TestTree
requestCreateCloudFormationChangeSet = req
    "CreateCloudFormationChangeSet"
    "fixture/CreateCloudFormationChangeSet.yaml"

requestPutApplicationPolicy :: PutApplicationPolicy -> TestTree
requestPutApplicationPolicy = req
    "PutApplicationPolicy"
    "fixture/PutApplicationPolicy.yaml"

requestListApplications :: ListApplications -> TestTree
requestListApplications = req
    "ListApplications"
    "fixture/ListApplications.yaml"

-- Responses

responseGetApplicationPolicy :: GetApplicationPolicyResponse -> TestTree
responseGetApplicationPolicy = res
    "GetApplicationPolicyResponse"
    "fixture/GetApplicationPolicyResponse.proto"
    serverlessApplicationRepository
    (Proxy :: Proxy GetApplicationPolicy)

responseCreateApplicationVersion :: CreateApplicationVersionResponse -> TestTree
responseCreateApplicationVersion = res
    "CreateApplicationVersionResponse"
    "fixture/CreateApplicationVersionResponse.proto"
    serverlessApplicationRepository
    (Proxy :: Proxy CreateApplicationVersion)

responseDeleteApplication :: DeleteApplicationResponse -> TestTree
responseDeleteApplication = res
    "DeleteApplicationResponse"
    "fixture/DeleteApplicationResponse.proto"
    serverlessApplicationRepository
    (Proxy :: Proxy DeleteApplication)

responseUpdateApplication :: UpdateApplicationResponse -> TestTree
responseUpdateApplication = res
    "UpdateApplicationResponse"
    "fixture/UpdateApplicationResponse.proto"
    serverlessApplicationRepository
    (Proxy :: Proxy UpdateApplication)

responseCreateCloudFormationTemplate :: CreateCloudFormationTemplateResponse -> TestTree
responseCreateCloudFormationTemplate = res
    "CreateCloudFormationTemplateResponse"
    "fixture/CreateCloudFormationTemplateResponse.proto"
    serverlessApplicationRepository
    (Proxy :: Proxy CreateCloudFormationTemplate)

responseCreateApplication :: CreateApplicationResponse -> TestTree
responseCreateApplication = res
    "CreateApplicationResponse"
    "fixture/CreateApplicationResponse.proto"
    serverlessApplicationRepository
    (Proxy :: Proxy CreateApplication)

responseListApplicationDependencies :: ListApplicationDependenciesResponse -> TestTree
responseListApplicationDependencies = res
    "ListApplicationDependenciesResponse"
    "fixture/ListApplicationDependenciesResponse.proto"
    serverlessApplicationRepository
    (Proxy :: Proxy ListApplicationDependencies)

responseListApplicationVersions :: ListApplicationVersionsResponse -> TestTree
responseListApplicationVersions = res
    "ListApplicationVersionsResponse"
    "fixture/ListApplicationVersionsResponse.proto"
    serverlessApplicationRepository
    (Proxy :: Proxy ListApplicationVersions)

responseGetApplication :: GetApplicationResponse -> TestTree
responseGetApplication = res
    "GetApplicationResponse"
    "fixture/GetApplicationResponse.proto"
    serverlessApplicationRepository
    (Proxy :: Proxy GetApplication)

responseGetCloudFormationTemplate :: GetCloudFormationTemplateResponse -> TestTree
responseGetCloudFormationTemplate = res
    "GetCloudFormationTemplateResponse"
    "fixture/GetCloudFormationTemplateResponse.proto"
    serverlessApplicationRepository
    (Proxy :: Proxy GetCloudFormationTemplate)

responseCreateCloudFormationChangeSet :: CreateCloudFormationChangeSetResponse -> TestTree
responseCreateCloudFormationChangeSet = res
    "CreateCloudFormationChangeSetResponse"
    "fixture/CreateCloudFormationChangeSetResponse.proto"
    serverlessApplicationRepository
    (Proxy :: Proxy CreateCloudFormationChangeSet)

responsePutApplicationPolicy :: PutApplicationPolicyResponse -> TestTree
responsePutApplicationPolicy = res
    "PutApplicationPolicyResponse"
    "fixture/PutApplicationPolicyResponse.proto"
    serverlessApplicationRepository
    (Proxy :: Proxy PutApplicationPolicy)

responseListApplications :: ListApplicationsResponse -> TestTree
responseListApplications = res
    "ListApplicationsResponse"
    "fixture/ListApplicationsResponse.proto"
    serverlessApplicationRepository
    (Proxy :: Proxy ListApplications)
