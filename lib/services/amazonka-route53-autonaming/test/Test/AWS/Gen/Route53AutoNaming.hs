{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.Route53AutoNaming
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.AWS.Gen.Route53AutoNaming where

import qualified Data.Proxy as Proxy
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
--             newListServices
--
--         , requestDeleteService $
--             newDeleteService
--
--         , requestUpdateService $
--             newUpdateService
--
--         , requestListOperations $
--             newListOperations
--
--         , requestCreateHttpNamespace $
--             newCreateHttpNamespace
--
--         , requestCreatePublicDnsNamespace $
--             newCreatePublicDnsNamespace
--
--         , requestGetInstance $
--             newGetInstance
--
--         , requestListNamespaces $
--             newListNamespaces
--
--         , requestDeleteNamespace $
--             newDeleteNamespace
--
--         , requestUpdatePublicDnsNamespace $
--             newUpdatePublicDnsNamespace
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestDiscoverInstances $
--             newDiscoverInstances
--
--         , requestGetInstancesHealthStatus $
--             newGetInstancesHealthStatus
--
--         , requestUpdateHttpNamespace $
--             newUpdateHttpNamespace
--
--         , requestGetNamespace $
--             newGetNamespace
--
--         , requestRegisterInstance $
--             newRegisterInstance
--
--         , requestTagResource $
--             newTagResource
--
--         , requestListInstances $
--             newListInstances
--
--         , requestGetOperation $
--             newGetOperation
--
--         , requestUpdateInstanceCustomHealthStatus $
--             newUpdateInstanceCustomHealthStatus
--
--         , requestGetService $
--             newGetService
--
--         , requestCreatePrivateDnsNamespace $
--             newCreatePrivateDnsNamespace
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestUpdatePrivateDnsNamespace $
--             newUpdatePrivateDnsNamespace
--
--         , requestCreateService $
--             newCreateService
--
--         , requestDeregisterInstance $
--             newDeregisterInstance
--
--           ]

--     , testGroup "response"
--         [ responseListServices $
--             newListServicesResponse
--
--         , responseDeleteService $
--             newDeleteServiceResponse
--
--         , responseUpdateService $
--             newUpdateServiceResponse
--
--         , responseListOperations $
--             newListOperationsResponse
--
--         , responseCreateHttpNamespace $
--             newCreateHttpNamespaceResponse
--
--         , responseCreatePublicDnsNamespace $
--             newCreatePublicDnsNamespaceResponse
--
--         , responseGetInstance $
--             newGetInstanceResponse
--
--         , responseListNamespaces $
--             newListNamespacesResponse
--
--         , responseDeleteNamespace $
--             newDeleteNamespaceResponse
--
--         , responseUpdatePublicDnsNamespace $
--             newUpdatePublicDnsNamespaceResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseDiscoverInstances $
--             newDiscoverInstancesResponse
--
--         , responseGetInstancesHealthStatus $
--             newGetInstancesHealthStatusResponse
--
--         , responseUpdateHttpNamespace $
--             newUpdateHttpNamespaceResponse
--
--         , responseGetNamespace $
--             newGetNamespaceResponse
--
--         , responseRegisterInstance $
--             newRegisterInstanceResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseListInstances $
--             newListInstancesResponse
--
--         , responseGetOperation $
--             newGetOperationResponse
--
--         , responseUpdateInstanceCustomHealthStatus $
--             newUpdateInstanceCustomHealthStatusResponse
--
--         , responseGetService $
--             newGetServiceResponse
--
--         , responseCreatePrivateDnsNamespace $
--             newCreatePrivateDnsNamespaceResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseUpdatePrivateDnsNamespace $
--             newUpdatePrivateDnsNamespaceResponse
--
--         , responseCreateService $
--             newCreateServiceResponse
--
--         , responseDeregisterInstance $
--             newDeregisterInstanceResponse
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

requestCreateHttpNamespace :: CreateHttpNamespace -> TestTree
requestCreateHttpNamespace =
  req
    "CreateHttpNamespace"
    "fixture/CreateHttpNamespace.yaml"

requestCreatePublicDnsNamespace :: CreatePublicDnsNamespace -> TestTree
requestCreatePublicDnsNamespace =
  req
    "CreatePublicDnsNamespace"
    "fixture/CreatePublicDnsNamespace.yaml"

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

requestUpdatePublicDnsNamespace :: UpdatePublicDnsNamespace -> TestTree
requestUpdatePublicDnsNamespace =
  req
    "UpdatePublicDnsNamespace"
    "fixture/UpdatePublicDnsNamespace.yaml"

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

requestUpdateHttpNamespace :: UpdateHttpNamespace -> TestTree
requestUpdateHttpNamespace =
  req
    "UpdateHttpNamespace"
    "fixture/UpdateHttpNamespace.yaml"

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

requestCreatePrivateDnsNamespace :: CreatePrivateDnsNamespace -> TestTree
requestCreatePrivateDnsNamespace =
  req
    "CreatePrivateDnsNamespace"
    "fixture/CreatePrivateDnsNamespace.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestUpdatePrivateDnsNamespace :: UpdatePrivateDnsNamespace -> TestTree
requestUpdatePrivateDnsNamespace =
  req
    "UpdatePrivateDnsNamespace"
    "fixture/UpdatePrivateDnsNamespace.yaml"

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
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListServices)

responseDeleteService :: DeleteServiceResponse -> TestTree
responseDeleteService =
  res
    "DeleteServiceResponse"
    "fixture/DeleteServiceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteService)

responseUpdateService :: UpdateServiceResponse -> TestTree
responseUpdateService =
  res
    "UpdateServiceResponse"
    "fixture/UpdateServiceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateService)

responseListOperations :: ListOperationsResponse -> TestTree
responseListOperations =
  res
    "ListOperationsResponse"
    "fixture/ListOperationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListOperations)

responseCreateHttpNamespace :: CreateHttpNamespaceResponse -> TestTree
responseCreateHttpNamespace =
  res
    "CreateHttpNamespaceResponse"
    "fixture/CreateHttpNamespaceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateHttpNamespace)

responseCreatePublicDnsNamespace :: CreatePublicDnsNamespaceResponse -> TestTree
responseCreatePublicDnsNamespace =
  res
    "CreatePublicDnsNamespaceResponse"
    "fixture/CreatePublicDnsNamespaceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreatePublicDnsNamespace)

responseGetInstance :: GetInstanceResponse -> TestTree
responseGetInstance =
  res
    "GetInstanceResponse"
    "fixture/GetInstanceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetInstance)

responseListNamespaces :: ListNamespacesResponse -> TestTree
responseListNamespaces =
  res
    "ListNamespacesResponse"
    "fixture/ListNamespacesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListNamespaces)

responseDeleteNamespace :: DeleteNamespaceResponse -> TestTree
responseDeleteNamespace =
  res
    "DeleteNamespaceResponse"
    "fixture/DeleteNamespaceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteNamespace)

responseUpdatePublicDnsNamespace :: UpdatePublicDnsNamespaceResponse -> TestTree
responseUpdatePublicDnsNamespace =
  res
    "UpdatePublicDnsNamespaceResponse"
    "fixture/UpdatePublicDnsNamespaceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdatePublicDnsNamespace)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responseDiscoverInstances :: DiscoverInstancesResponse -> TestTree
responseDiscoverInstances =
  res
    "DiscoverInstancesResponse"
    "fixture/DiscoverInstancesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DiscoverInstances)

responseGetInstancesHealthStatus :: GetInstancesHealthStatusResponse -> TestTree
responseGetInstancesHealthStatus =
  res
    "GetInstancesHealthStatusResponse"
    "fixture/GetInstancesHealthStatusResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetInstancesHealthStatus)

responseUpdateHttpNamespace :: UpdateHttpNamespaceResponse -> TestTree
responseUpdateHttpNamespace =
  res
    "UpdateHttpNamespaceResponse"
    "fixture/UpdateHttpNamespaceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateHttpNamespace)

responseGetNamespace :: GetNamespaceResponse -> TestTree
responseGetNamespace =
  res
    "GetNamespaceResponse"
    "fixture/GetNamespaceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetNamespace)

responseRegisterInstance :: RegisterInstanceResponse -> TestTree
responseRegisterInstance =
  res
    "RegisterInstanceResponse"
    "fixture/RegisterInstanceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RegisterInstance)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TagResource)

responseListInstances :: ListInstancesResponse -> TestTree
responseListInstances =
  res
    "ListInstancesResponse"
    "fixture/ListInstancesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListInstances)

responseGetOperation :: GetOperationResponse -> TestTree
responseGetOperation =
  res
    "GetOperationResponse"
    "fixture/GetOperationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetOperation)

responseUpdateInstanceCustomHealthStatus :: UpdateInstanceCustomHealthStatusResponse -> TestTree
responseUpdateInstanceCustomHealthStatus =
  res
    "UpdateInstanceCustomHealthStatusResponse"
    "fixture/UpdateInstanceCustomHealthStatusResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateInstanceCustomHealthStatus)

responseGetService :: GetServiceResponse -> TestTree
responseGetService =
  res
    "GetServiceResponse"
    "fixture/GetServiceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetService)

responseCreatePrivateDnsNamespace :: CreatePrivateDnsNamespaceResponse -> TestTree
responseCreatePrivateDnsNamespace =
  res
    "CreatePrivateDnsNamespaceResponse"
    "fixture/CreatePrivateDnsNamespaceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreatePrivateDnsNamespace)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UntagResource)

responseUpdatePrivateDnsNamespace :: UpdatePrivateDnsNamespaceResponse -> TestTree
responseUpdatePrivateDnsNamespace =
  res
    "UpdatePrivateDnsNamespaceResponse"
    "fixture/UpdatePrivateDnsNamespaceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdatePrivateDnsNamespace)

responseCreateService :: CreateServiceResponse -> TestTree
responseCreateService =
  res
    "CreateServiceResponse"
    "fixture/CreateServiceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateService)

responseDeregisterInstance :: DeregisterInstanceResponse -> TestTree
responseDeregisterInstance =
  res
    "DeregisterInstanceResponse"
    "fixture/DeregisterInstanceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeregisterInstance)
