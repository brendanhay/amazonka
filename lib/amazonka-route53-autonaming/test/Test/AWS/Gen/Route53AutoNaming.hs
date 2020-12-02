{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.Route53AutoNaming
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.AWS.Gen.Route53AutoNaming where

import Data.Proxy
import Network.AWS.Route53AutoNaming
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.AWS.Route53AutoNaming.Internal
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestListServices $
--             listServices
--
--         , requestDeleteService $
--             deleteService
--
--         , requestUpdateService $
--             updateService
--
--         , requestListOperations $
--             listOperations
--
--         , requestCreateHTTPNamespace $
--             createHTTPNamespace
--
--         , requestCreatePublicDNSNamespace $
--             createPublicDNSNamespace
--
--         , requestGetInstance $
--             getInstance
--
--         , requestListNamespaces $
--             listNamespaces
--
--         , requestDeleteNamespace $
--             deleteNamespace
--
--         , requestListTagsForResource $
--             listTagsForResource
--
--         , requestDiscoverInstances $
--             discoverInstances
--
--         , requestGetInstancesHealthStatus $
--             getInstancesHealthStatus
--
--         , requestGetNamespace $
--             getNamespace
--
--         , requestRegisterInstance $
--             registerInstance
--
--         , requestTagResource $
--             tagResource
--
--         , requestListInstances $
--             listInstances
--
--         , requestGetOperation $
--             getOperation
--
--         , requestUpdateInstanceCustomHealthStatus $
--             updateInstanceCustomHealthStatus
--
--         , requestGetService $
--             getService
--
--         , requestCreatePrivateDNSNamespace $
--             createPrivateDNSNamespace
--
--         , requestUntagResource $
--             untagResource
--
--         , requestCreateService $
--             createService
--
--         , requestDeregisterInstance $
--             deregisterInstance
--
--           ]

--     , testGroup "response"
--         [ responseListServices $
--             listServicesResponse
--
--         , responseDeleteService $
--             deleteServiceResponse
--
--         , responseUpdateService $
--             updateServiceResponse
--
--         , responseListOperations $
--             listOperationsResponse
--
--         , responseCreateHTTPNamespace $
--             createHTTPNamespaceResponse
--
--         , responseCreatePublicDNSNamespace $
--             createPublicDNSNamespaceResponse
--
--         , responseGetInstance $
--             getInstanceResponse
--
--         , responseListNamespaces $
--             listNamespacesResponse
--
--         , responseDeleteNamespace $
--             deleteNamespaceResponse
--
--         , responseListTagsForResource $
--             listTagsForResourceResponse
--
--         , responseDiscoverInstances $
--             discoverInstancesResponse
--
--         , responseGetInstancesHealthStatus $
--             getInstancesHealthStatusResponse
--
--         , responseGetNamespace $
--             getNamespaceResponse
--
--         , responseRegisterInstance $
--             registerInstanceResponse
--
--         , responseTagResource $
--             tagResourceResponse
--
--         , responseListInstances $
--             listInstancesResponse
--
--         , responseGetOperation $
--             getOperationResponse
--
--         , responseUpdateInstanceCustomHealthStatus $
--             updateInstanceCustomHealthStatusResponse
--
--         , responseGetService $
--             getServiceResponse
--
--         , responseCreatePrivateDNSNamespace $
--             createPrivateDNSNamespaceResponse
--
--         , responseUntagResource $
--             untagResourceResponse
--
--         , responseCreateService $
--             createServiceResponse
--
--         , responseDeregisterInstance $
--             deregisterInstanceResponse
--
--           ]
--     ]

-- Requests

requestListServices :: ListServices -> TestTree
requestListServices =
  req
    "ListServices"
    "fixture/ListServices.yaml"

requestDeleteService :: DeleteService -> TestTree
requestDeleteService =
  req
    "DeleteService"
    "fixture/DeleteService.yaml"

requestUpdateService :: UpdateService -> TestTree
requestUpdateService =
  req
    "UpdateService"
    "fixture/UpdateService.yaml"

requestListOperations :: ListOperations -> TestTree
requestListOperations =
  req
    "ListOperations"
    "fixture/ListOperations.yaml"

requestCreateHTTPNamespace :: CreateHTTPNamespace -> TestTree
requestCreateHTTPNamespace =
  req
    "CreateHTTPNamespace"
    "fixture/CreateHTTPNamespace.yaml"

requestCreatePublicDNSNamespace :: CreatePublicDNSNamespace -> TestTree
requestCreatePublicDNSNamespace =
  req
    "CreatePublicDNSNamespace"
    "fixture/CreatePublicDNSNamespace.yaml"

requestGetInstance :: GetInstance -> TestTree
requestGetInstance =
  req
    "GetInstance"
    "fixture/GetInstance.yaml"

requestListNamespaces :: ListNamespaces -> TestTree
requestListNamespaces =
  req
    "ListNamespaces"
    "fixture/ListNamespaces.yaml"

requestDeleteNamespace :: DeleteNamespace -> TestTree
requestDeleteNamespace =
  req
    "DeleteNamespace"
    "fixture/DeleteNamespace.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestDiscoverInstances :: DiscoverInstances -> TestTree
requestDiscoverInstances =
  req
    "DiscoverInstances"
    "fixture/DiscoverInstances.yaml"

requestGetInstancesHealthStatus :: GetInstancesHealthStatus -> TestTree
requestGetInstancesHealthStatus =
  req
    "GetInstancesHealthStatus"
    "fixture/GetInstancesHealthStatus.yaml"

requestGetNamespace :: GetNamespace -> TestTree
requestGetNamespace =
  req
    "GetNamespace"
    "fixture/GetNamespace.yaml"

requestRegisterInstance :: RegisterInstance -> TestTree
requestRegisterInstance =
  req
    "RegisterInstance"
    "fixture/RegisterInstance.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestListInstances :: ListInstances -> TestTree
requestListInstances =
  req
    "ListInstances"
    "fixture/ListInstances.yaml"

requestGetOperation :: GetOperation -> TestTree
requestGetOperation =
  req
    "GetOperation"
    "fixture/GetOperation.yaml"

requestUpdateInstanceCustomHealthStatus :: UpdateInstanceCustomHealthStatus -> TestTree
requestUpdateInstanceCustomHealthStatus =
  req
    "UpdateInstanceCustomHealthStatus"
    "fixture/UpdateInstanceCustomHealthStatus.yaml"

requestGetService :: GetService -> TestTree
requestGetService =
  req
    "GetService"
    "fixture/GetService.yaml"

requestCreatePrivateDNSNamespace :: CreatePrivateDNSNamespace -> TestTree
requestCreatePrivateDNSNamespace =
  req
    "CreatePrivateDNSNamespace"
    "fixture/CreatePrivateDNSNamespace.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestCreateService :: CreateService -> TestTree
requestCreateService =
  req
    "CreateService"
    "fixture/CreateService.yaml"

requestDeregisterInstance :: DeregisterInstance -> TestTree
requestDeregisterInstance =
  req
    "DeregisterInstance"
    "fixture/DeregisterInstance.yaml"

-- Responses

responseListServices :: ListServicesResponse -> TestTree
responseListServices =
  res
    "ListServicesResponse"
    "fixture/ListServicesResponse.proto"
    route53AutoNaming
    (Proxy :: Proxy ListServices)

responseDeleteService :: DeleteServiceResponse -> TestTree
responseDeleteService =
  res
    "DeleteServiceResponse"
    "fixture/DeleteServiceResponse.proto"
    route53AutoNaming
    (Proxy :: Proxy DeleteService)

responseUpdateService :: UpdateServiceResponse -> TestTree
responseUpdateService =
  res
    "UpdateServiceResponse"
    "fixture/UpdateServiceResponse.proto"
    route53AutoNaming
    (Proxy :: Proxy UpdateService)

responseListOperations :: ListOperationsResponse -> TestTree
responseListOperations =
  res
    "ListOperationsResponse"
    "fixture/ListOperationsResponse.proto"
    route53AutoNaming
    (Proxy :: Proxy ListOperations)

responseCreateHTTPNamespace :: CreateHTTPNamespaceResponse -> TestTree
responseCreateHTTPNamespace =
  res
    "CreateHTTPNamespaceResponse"
    "fixture/CreateHTTPNamespaceResponse.proto"
    route53AutoNaming
    (Proxy :: Proxy CreateHTTPNamespace)

responseCreatePublicDNSNamespace :: CreatePublicDNSNamespaceResponse -> TestTree
responseCreatePublicDNSNamespace =
  res
    "CreatePublicDNSNamespaceResponse"
    "fixture/CreatePublicDNSNamespaceResponse.proto"
    route53AutoNaming
    (Proxy :: Proxy CreatePublicDNSNamespace)

responseGetInstance :: GetInstanceResponse -> TestTree
responseGetInstance =
  res
    "GetInstanceResponse"
    "fixture/GetInstanceResponse.proto"
    route53AutoNaming
    (Proxy :: Proxy GetInstance)

responseListNamespaces :: ListNamespacesResponse -> TestTree
responseListNamespaces =
  res
    "ListNamespacesResponse"
    "fixture/ListNamespacesResponse.proto"
    route53AutoNaming
    (Proxy :: Proxy ListNamespaces)

responseDeleteNamespace :: DeleteNamespaceResponse -> TestTree
responseDeleteNamespace =
  res
    "DeleteNamespaceResponse"
    "fixture/DeleteNamespaceResponse.proto"
    route53AutoNaming
    (Proxy :: Proxy DeleteNamespace)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    route53AutoNaming
    (Proxy :: Proxy ListTagsForResource)

responseDiscoverInstances :: DiscoverInstancesResponse -> TestTree
responseDiscoverInstances =
  res
    "DiscoverInstancesResponse"
    "fixture/DiscoverInstancesResponse.proto"
    route53AutoNaming
    (Proxy :: Proxy DiscoverInstances)

responseGetInstancesHealthStatus :: GetInstancesHealthStatusResponse -> TestTree
responseGetInstancesHealthStatus =
  res
    "GetInstancesHealthStatusResponse"
    "fixture/GetInstancesHealthStatusResponse.proto"
    route53AutoNaming
    (Proxy :: Proxy GetInstancesHealthStatus)

responseGetNamespace :: GetNamespaceResponse -> TestTree
responseGetNamespace =
  res
    "GetNamespaceResponse"
    "fixture/GetNamespaceResponse.proto"
    route53AutoNaming
    (Proxy :: Proxy GetNamespace)

responseRegisterInstance :: RegisterInstanceResponse -> TestTree
responseRegisterInstance =
  res
    "RegisterInstanceResponse"
    "fixture/RegisterInstanceResponse.proto"
    route53AutoNaming
    (Proxy :: Proxy RegisterInstance)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    route53AutoNaming
    (Proxy :: Proxy TagResource)

responseListInstances :: ListInstancesResponse -> TestTree
responseListInstances =
  res
    "ListInstancesResponse"
    "fixture/ListInstancesResponse.proto"
    route53AutoNaming
    (Proxy :: Proxy ListInstances)

responseGetOperation :: GetOperationResponse -> TestTree
responseGetOperation =
  res
    "GetOperationResponse"
    "fixture/GetOperationResponse.proto"
    route53AutoNaming
    (Proxy :: Proxy GetOperation)

responseUpdateInstanceCustomHealthStatus :: UpdateInstanceCustomHealthStatusResponse -> TestTree
responseUpdateInstanceCustomHealthStatus =
  res
    "UpdateInstanceCustomHealthStatusResponse"
    "fixture/UpdateInstanceCustomHealthStatusResponse.proto"
    route53AutoNaming
    (Proxy :: Proxy UpdateInstanceCustomHealthStatus)

responseGetService :: GetServiceResponse -> TestTree
responseGetService =
  res
    "GetServiceResponse"
    "fixture/GetServiceResponse.proto"
    route53AutoNaming
    (Proxy :: Proxy GetService)

responseCreatePrivateDNSNamespace :: CreatePrivateDNSNamespaceResponse -> TestTree
responseCreatePrivateDNSNamespace =
  res
    "CreatePrivateDNSNamespaceResponse"
    "fixture/CreatePrivateDNSNamespaceResponse.proto"
    route53AutoNaming
    (Proxy :: Proxy CreatePrivateDNSNamespace)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    route53AutoNaming
    (Proxy :: Proxy UntagResource)

responseCreateService :: CreateServiceResponse -> TestTree
responseCreateService =
  res
    "CreateServiceResponse"
    "fixture/CreateServiceResponse.proto"
    route53AutoNaming
    (Proxy :: Proxy CreateService)

responseDeregisterInstance :: DeregisterInstanceResponse -> TestTree
responseDeregisterInstance =
  res
    "DeregisterInstanceResponse"
    "fixture/DeregisterInstanceResponse.proto"
    route53AutoNaming
    (Proxy :: Proxy DeregisterInstance)
