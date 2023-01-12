{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.MigrationHubReFactorSpaces
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.MigrationHubReFactorSpaces where

import Amazonka.MigrationHubReFactorSpaces
import qualified Data.Proxy as Proxy
import Test.Amazonka.Fixture
import Test.Amazonka.MigrationHubReFactorSpaces.Internal
import Test.Amazonka.Prelude
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
--         , requestCreateEnvironment $
--             newCreateEnvironment
--
--         , requestCreateRoute $
--             newCreateRoute
--
--         , requestCreateService $
--             newCreateService
--
--         , requestDeleteApplication $
--             newDeleteApplication
--
--         , requestDeleteEnvironment $
--             newDeleteEnvironment
--
--         , requestDeleteResourcePolicy $
--             newDeleteResourcePolicy
--
--         , requestDeleteRoute $
--             newDeleteRoute
--
--         , requestDeleteService $
--             newDeleteService
--
--         , requestGetApplication $
--             newGetApplication
--
--         , requestGetEnvironment $
--             newGetEnvironment
--
--         , requestGetResourcePolicy $
--             newGetResourcePolicy
--
--         , requestGetRoute $
--             newGetRoute
--
--         , requestGetService $
--             newGetService
--
--         , requestListApplications $
--             newListApplications
--
--         , requestListEnvironmentVpcs $
--             newListEnvironmentVpcs
--
--         , requestListEnvironments $
--             newListEnvironments
--
--         , requestListRoutes $
--             newListRoutes
--
--         , requestListServices $
--             newListServices
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestPutResourcePolicy $
--             newPutResourcePolicy
--
--         , requestTagResource $
--             newTagResource
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestUpdateRoute $
--             newUpdateRoute
--
--           ]

--     , testGroup "response"
--         [ responseCreateApplication $
--             newCreateApplicationResponse
--
--         , responseCreateEnvironment $
--             newCreateEnvironmentResponse
--
--         , responseCreateRoute $
--             newCreateRouteResponse
--
--         , responseCreateService $
--             newCreateServiceResponse
--
--         , responseDeleteApplication $
--             newDeleteApplicationResponse
--
--         , responseDeleteEnvironment $
--             newDeleteEnvironmentResponse
--
--         , responseDeleteResourcePolicy $
--             newDeleteResourcePolicyResponse
--
--         , responseDeleteRoute $
--             newDeleteRouteResponse
--
--         , responseDeleteService $
--             newDeleteServiceResponse
--
--         , responseGetApplication $
--             newGetApplicationResponse
--
--         , responseGetEnvironment $
--             newGetEnvironmentResponse
--
--         , responseGetResourcePolicy $
--             newGetResourcePolicyResponse
--
--         , responseGetRoute $
--             newGetRouteResponse
--
--         , responseGetService $
--             newGetServiceResponse
--
--         , responseListApplications $
--             newListApplicationsResponse
--
--         , responseListEnvironmentVpcs $
--             newListEnvironmentVpcsResponse
--
--         , responseListEnvironments $
--             newListEnvironmentsResponse
--
--         , responseListRoutes $
--             newListRoutesResponse
--
--         , responseListServices $
--             newListServicesResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responsePutResourcePolicy $
--             newPutResourcePolicyResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseUpdateRoute $
--             newUpdateRouteResponse
--
--           ]
--     ]

-- Requests

requestCreateApplication :: CreateApplication -> TestTree
requestCreateApplication =
  req
    "CreateApplication"
    "fixture/CreateApplication.yaml"

requestCreateEnvironment :: CreateEnvironment -> TestTree
requestCreateEnvironment =
  req
    "CreateEnvironment"
    "fixture/CreateEnvironment.yaml"

requestCreateRoute :: CreateRoute -> TestTree
requestCreateRoute =
  req
    "CreateRoute"
    "fixture/CreateRoute.yaml"

requestCreateService :: CreateService -> TestTree
requestCreateService =
  req
    "CreateService"
    "fixture/CreateService.yaml"

requestDeleteApplication :: DeleteApplication -> TestTree
requestDeleteApplication =
  req
    "DeleteApplication"
    "fixture/DeleteApplication.yaml"

requestDeleteEnvironment :: DeleteEnvironment -> TestTree
requestDeleteEnvironment =
  req
    "DeleteEnvironment"
    "fixture/DeleteEnvironment.yaml"

requestDeleteResourcePolicy :: DeleteResourcePolicy -> TestTree
requestDeleteResourcePolicy =
  req
    "DeleteResourcePolicy"
    "fixture/DeleteResourcePolicy.yaml"

requestDeleteRoute :: DeleteRoute -> TestTree
requestDeleteRoute =
  req
    "DeleteRoute"
    "fixture/DeleteRoute.yaml"

requestDeleteService :: DeleteService -> TestTree
requestDeleteService =
  req
    "DeleteService"
    "fixture/DeleteService.yaml"

requestGetApplication :: GetApplication -> TestTree
requestGetApplication =
  req
    "GetApplication"
    "fixture/GetApplication.yaml"

requestGetEnvironment :: GetEnvironment -> TestTree
requestGetEnvironment =
  req
    "GetEnvironment"
    "fixture/GetEnvironment.yaml"

requestGetResourcePolicy :: GetResourcePolicy -> TestTree
requestGetResourcePolicy =
  req
    "GetResourcePolicy"
    "fixture/GetResourcePolicy.yaml"

requestGetRoute :: GetRoute -> TestTree
requestGetRoute =
  req
    "GetRoute"
    "fixture/GetRoute.yaml"

requestGetService :: GetService -> TestTree
requestGetService =
  req
    "GetService"
    "fixture/GetService.yaml"

requestListApplications :: ListApplications -> TestTree
requestListApplications =
  req
    "ListApplications"
    "fixture/ListApplications.yaml"

requestListEnvironmentVpcs :: ListEnvironmentVpcs -> TestTree
requestListEnvironmentVpcs =
  req
    "ListEnvironmentVpcs"
    "fixture/ListEnvironmentVpcs.yaml"

requestListEnvironments :: ListEnvironments -> TestTree
requestListEnvironments =
  req
    "ListEnvironments"
    "fixture/ListEnvironments.yaml"

requestListRoutes :: ListRoutes -> TestTree
requestListRoutes =
  req
    "ListRoutes"
    "fixture/ListRoutes.yaml"

requestListServices :: ListServices -> TestTree
requestListServices =
  req
    "ListServices"
    "fixture/ListServices.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestPutResourcePolicy :: PutResourcePolicy -> TestTree
requestPutResourcePolicy =
  req
    "PutResourcePolicy"
    "fixture/PutResourcePolicy.yaml"

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

requestUpdateRoute :: UpdateRoute -> TestTree
requestUpdateRoute =
  req
    "UpdateRoute"
    "fixture/UpdateRoute.yaml"

-- Responses

responseCreateApplication :: CreateApplicationResponse -> TestTree
responseCreateApplication =
  res
    "CreateApplicationResponse"
    "fixture/CreateApplicationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateApplication)

responseCreateEnvironment :: CreateEnvironmentResponse -> TestTree
responseCreateEnvironment =
  res
    "CreateEnvironmentResponse"
    "fixture/CreateEnvironmentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateEnvironment)

responseCreateRoute :: CreateRouteResponse -> TestTree
responseCreateRoute =
  res
    "CreateRouteResponse"
    "fixture/CreateRouteResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateRoute)

responseCreateService :: CreateServiceResponse -> TestTree
responseCreateService =
  res
    "CreateServiceResponse"
    "fixture/CreateServiceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateService)

responseDeleteApplication :: DeleteApplicationResponse -> TestTree
responseDeleteApplication =
  res
    "DeleteApplicationResponse"
    "fixture/DeleteApplicationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteApplication)

responseDeleteEnvironment :: DeleteEnvironmentResponse -> TestTree
responseDeleteEnvironment =
  res
    "DeleteEnvironmentResponse"
    "fixture/DeleteEnvironmentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteEnvironment)

responseDeleteResourcePolicy :: DeleteResourcePolicyResponse -> TestTree
responseDeleteResourcePolicy =
  res
    "DeleteResourcePolicyResponse"
    "fixture/DeleteResourcePolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteResourcePolicy)

responseDeleteRoute :: DeleteRouteResponse -> TestTree
responseDeleteRoute =
  res
    "DeleteRouteResponse"
    "fixture/DeleteRouteResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteRoute)

responseDeleteService :: DeleteServiceResponse -> TestTree
responseDeleteService =
  res
    "DeleteServiceResponse"
    "fixture/DeleteServiceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteService)

responseGetApplication :: GetApplicationResponse -> TestTree
responseGetApplication =
  res
    "GetApplicationResponse"
    "fixture/GetApplicationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetApplication)

responseGetEnvironment :: GetEnvironmentResponse -> TestTree
responseGetEnvironment =
  res
    "GetEnvironmentResponse"
    "fixture/GetEnvironmentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetEnvironment)

responseGetResourcePolicy :: GetResourcePolicyResponse -> TestTree
responseGetResourcePolicy =
  res
    "GetResourcePolicyResponse"
    "fixture/GetResourcePolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetResourcePolicy)

responseGetRoute :: GetRouteResponse -> TestTree
responseGetRoute =
  res
    "GetRouteResponse"
    "fixture/GetRouteResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetRoute)

responseGetService :: GetServiceResponse -> TestTree
responseGetService =
  res
    "GetServiceResponse"
    "fixture/GetServiceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetService)

responseListApplications :: ListApplicationsResponse -> TestTree
responseListApplications =
  res
    "ListApplicationsResponse"
    "fixture/ListApplicationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListApplications)

responseListEnvironmentVpcs :: ListEnvironmentVpcsResponse -> TestTree
responseListEnvironmentVpcs =
  res
    "ListEnvironmentVpcsResponse"
    "fixture/ListEnvironmentVpcsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListEnvironmentVpcs)

responseListEnvironments :: ListEnvironmentsResponse -> TestTree
responseListEnvironments =
  res
    "ListEnvironmentsResponse"
    "fixture/ListEnvironmentsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListEnvironments)

responseListRoutes :: ListRoutesResponse -> TestTree
responseListRoutes =
  res
    "ListRoutesResponse"
    "fixture/ListRoutesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListRoutes)

responseListServices :: ListServicesResponse -> TestTree
responseListServices =
  res
    "ListServicesResponse"
    "fixture/ListServicesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListServices)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responsePutResourcePolicy :: PutResourcePolicyResponse -> TestTree
responsePutResourcePolicy =
  res
    "PutResourcePolicyResponse"
    "fixture/PutResourcePolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutResourcePolicy)

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

responseUpdateRoute :: UpdateRouteResponse -> TestTree
responseUpdateRoute =
  res
    "UpdateRouteResponse"
    "fixture/UpdateRouteResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateRoute)
