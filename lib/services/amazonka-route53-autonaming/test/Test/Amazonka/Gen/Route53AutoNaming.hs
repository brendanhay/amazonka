{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.Route53AutoNaming
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.Route53AutoNaming where

import Amazonka.Route53AutoNaming
import qualified Data.Proxy as Proxy
import Test.Amazonka.Fixture
import Test.Amazonka.Prelude
import Test.Amazonka.Route53AutoNaming.Internal
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestCreateHttpNamespace $
--             newCreateHttpNamespace
--
--         , requestCreatePrivateDnsNamespace $
--             newCreatePrivateDnsNamespace
--
--         , requestCreatePublicDnsNamespace $
--             newCreatePublicDnsNamespace
--
--         , requestCreateService $
--             newCreateService
--
--         , requestDeleteNamespace $
--             newDeleteNamespace
--
--         , requestDeleteService $
--             newDeleteService
--
--         , requestDeregisterInstance $
--             newDeregisterInstance
--
--         , requestDiscoverInstances $
--             newDiscoverInstances
--
--         , requestGetInstance $
--             newGetInstance
--
--         , requestGetInstancesHealthStatus $
--             newGetInstancesHealthStatus
--
--         , requestGetNamespace $
--             newGetNamespace
--
--         , requestGetOperation $
--             newGetOperation
--
--         , requestGetService $
--             newGetService
--
--         , requestListInstances $
--             newListInstances
--
--         , requestListNamespaces $
--             newListNamespaces
--
--         , requestListOperations $
--             newListOperations
--
--         , requestListServices $
--             newListServices
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestRegisterInstance $
--             newRegisterInstance
--
--         , requestTagResource $
--             newTagResource
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestUpdateHttpNamespace $
--             newUpdateHttpNamespace
--
--         , requestUpdateInstanceCustomHealthStatus $
--             newUpdateInstanceCustomHealthStatus
--
--         , requestUpdatePrivateDnsNamespace $
--             newUpdatePrivateDnsNamespace
--
--         , requestUpdatePublicDnsNamespace $
--             newUpdatePublicDnsNamespace
--
--         , requestUpdateService $
--             newUpdateService
--
--           ]

--     , testGroup "response"
--         [ responseCreateHttpNamespace $
--             newCreateHttpNamespaceResponse
--
--         , responseCreatePrivateDnsNamespace $
--             newCreatePrivateDnsNamespaceResponse
--
--         , responseCreatePublicDnsNamespace $
--             newCreatePublicDnsNamespaceResponse
--
--         , responseCreateService $
--             newCreateServiceResponse
--
--         , responseDeleteNamespace $
--             newDeleteNamespaceResponse
--
--         , responseDeleteService $
--             newDeleteServiceResponse
--
--         , responseDeregisterInstance $
--             newDeregisterInstanceResponse
--
--         , responseDiscoverInstances $
--             newDiscoverInstancesResponse
--
--         , responseGetInstance $
--             newGetInstanceResponse
--
--         , responseGetInstancesHealthStatus $
--             newGetInstancesHealthStatusResponse
--
--         , responseGetNamespace $
--             newGetNamespaceResponse
--
--         , responseGetOperation $
--             newGetOperationResponse
--
--         , responseGetService $
--             newGetServiceResponse
--
--         , responseListInstances $
--             newListInstancesResponse
--
--         , responseListNamespaces $
--             newListNamespacesResponse
--
--         , responseListOperations $
--             newListOperationsResponse
--
--         , responseListServices $
--             newListServicesResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseRegisterInstance $
--             newRegisterInstanceResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseUpdateHttpNamespace $
--             newUpdateHttpNamespaceResponse
--
--         , responseUpdateInstanceCustomHealthStatus $
--             newUpdateInstanceCustomHealthStatusResponse
--
--         , responseUpdatePrivateDnsNamespace $
--             newUpdatePrivateDnsNamespaceResponse
--
--         , responseUpdatePublicDnsNamespace $
--             newUpdatePublicDnsNamespaceResponse
--
--         , responseUpdateService $
--             newUpdateServiceResponse
--
--           ]
--     ]

-- Requests

requestCreateHttpNamespace :: CreateHttpNamespace -> TestTree
requestCreateHttpNamespace =
  req
    "CreateHttpNamespace"
    "fixture/CreateHttpNamespace.yaml"

requestCreatePrivateDnsNamespace :: CreatePrivateDnsNamespace -> TestTree
requestCreatePrivateDnsNamespace =
  req
    "CreatePrivateDnsNamespace"
    "fixture/CreatePrivateDnsNamespace.yaml"

requestCreatePublicDnsNamespace :: CreatePublicDnsNamespace -> TestTree
requestCreatePublicDnsNamespace =
  req
    "CreatePublicDnsNamespace"
    "fixture/CreatePublicDnsNamespace.yaml"

requestCreateService :: CreateService -> TestTree
requestCreateService =
  req
    "CreateService"
    "fixture/CreateService.yaml"

requestDeleteNamespace :: DeleteNamespace -> TestTree
requestDeleteNamespace =
  req
    "DeleteNamespace"
    "fixture/DeleteNamespace.yaml"

requestDeleteService :: DeleteService -> TestTree
requestDeleteService =
  req
    "DeleteService"
    "fixture/DeleteService.yaml"

requestDeregisterInstance :: DeregisterInstance -> TestTree
requestDeregisterInstance =
  req
    "DeregisterInstance"
    "fixture/DeregisterInstance.yaml"

requestDiscoverInstances :: DiscoverInstances -> TestTree
requestDiscoverInstances =
  req
    "DiscoverInstances"
    "fixture/DiscoverInstances.yaml"

requestGetInstance :: GetInstance -> TestTree
requestGetInstance =
  req
    "GetInstance"
    "fixture/GetInstance.yaml"

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

requestGetOperation :: GetOperation -> TestTree
requestGetOperation =
  req
    "GetOperation"
    "fixture/GetOperation.yaml"

requestGetService :: GetService -> TestTree
requestGetService =
  req
    "GetService"
    "fixture/GetService.yaml"

requestListInstances :: ListInstances -> TestTree
requestListInstances =
  req
    "ListInstances"
    "fixture/ListInstances.yaml"

requestListNamespaces :: ListNamespaces -> TestTree
requestListNamespaces =
  req
    "ListNamespaces"
    "fixture/ListNamespaces.yaml"

requestListOperations :: ListOperations -> TestTree
requestListOperations =
  req
    "ListOperations"
    "fixture/ListOperations.yaml"

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

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestUpdateHttpNamespace :: UpdateHttpNamespace -> TestTree
requestUpdateHttpNamespace =
  req
    "UpdateHttpNamespace"
    "fixture/UpdateHttpNamespace.yaml"

requestUpdateInstanceCustomHealthStatus :: UpdateInstanceCustomHealthStatus -> TestTree
requestUpdateInstanceCustomHealthStatus =
  req
    "UpdateInstanceCustomHealthStatus"
    "fixture/UpdateInstanceCustomHealthStatus.yaml"

requestUpdatePrivateDnsNamespace :: UpdatePrivateDnsNamespace -> TestTree
requestUpdatePrivateDnsNamespace =
  req
    "UpdatePrivateDnsNamespace"
    "fixture/UpdatePrivateDnsNamespace.yaml"

requestUpdatePublicDnsNamespace :: UpdatePublicDnsNamespace -> TestTree
requestUpdatePublicDnsNamespace =
  req
    "UpdatePublicDnsNamespace"
    "fixture/UpdatePublicDnsNamespace.yaml"

requestUpdateService :: UpdateService -> TestTree
requestUpdateService =
  req
    "UpdateService"
    "fixture/UpdateService.yaml"

-- Responses

responseCreateHttpNamespace :: CreateHttpNamespaceResponse -> TestTree
responseCreateHttpNamespace =
  res
    "CreateHttpNamespaceResponse"
    "fixture/CreateHttpNamespaceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateHttpNamespace)

responseCreatePrivateDnsNamespace :: CreatePrivateDnsNamespaceResponse -> TestTree
responseCreatePrivateDnsNamespace =
  res
    "CreatePrivateDnsNamespaceResponse"
    "fixture/CreatePrivateDnsNamespaceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreatePrivateDnsNamespace)

responseCreatePublicDnsNamespace :: CreatePublicDnsNamespaceResponse -> TestTree
responseCreatePublicDnsNamespace =
  res
    "CreatePublicDnsNamespaceResponse"
    "fixture/CreatePublicDnsNamespaceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreatePublicDnsNamespace)

responseCreateService :: CreateServiceResponse -> TestTree
responseCreateService =
  res
    "CreateServiceResponse"
    "fixture/CreateServiceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateService)

responseDeleteNamespace :: DeleteNamespaceResponse -> TestTree
responseDeleteNamespace =
  res
    "DeleteNamespaceResponse"
    "fixture/DeleteNamespaceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteNamespace)

responseDeleteService :: DeleteServiceResponse -> TestTree
responseDeleteService =
  res
    "DeleteServiceResponse"
    "fixture/DeleteServiceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteService)

responseDeregisterInstance :: DeregisterInstanceResponse -> TestTree
responseDeregisterInstance =
  res
    "DeregisterInstanceResponse"
    "fixture/DeregisterInstanceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeregisterInstance)

responseDiscoverInstances :: DiscoverInstancesResponse -> TestTree
responseDiscoverInstances =
  res
    "DiscoverInstancesResponse"
    "fixture/DiscoverInstancesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DiscoverInstances)

responseGetInstance :: GetInstanceResponse -> TestTree
responseGetInstance =
  res
    "GetInstanceResponse"
    "fixture/GetInstanceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetInstance)

responseGetInstancesHealthStatus :: GetInstancesHealthStatusResponse -> TestTree
responseGetInstancesHealthStatus =
  res
    "GetInstancesHealthStatusResponse"
    "fixture/GetInstancesHealthStatusResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetInstancesHealthStatus)

responseGetNamespace :: GetNamespaceResponse -> TestTree
responseGetNamespace =
  res
    "GetNamespaceResponse"
    "fixture/GetNamespaceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetNamespace)

responseGetOperation :: GetOperationResponse -> TestTree
responseGetOperation =
  res
    "GetOperationResponse"
    "fixture/GetOperationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetOperation)

responseGetService :: GetServiceResponse -> TestTree
responseGetService =
  res
    "GetServiceResponse"
    "fixture/GetServiceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetService)

responseListInstances :: ListInstancesResponse -> TestTree
responseListInstances =
  res
    "ListInstancesResponse"
    "fixture/ListInstancesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListInstances)

responseListNamespaces :: ListNamespacesResponse -> TestTree
responseListNamespaces =
  res
    "ListNamespacesResponse"
    "fixture/ListNamespacesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListNamespaces)

responseListOperations :: ListOperationsResponse -> TestTree
responseListOperations =
  res
    "ListOperationsResponse"
    "fixture/ListOperationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListOperations)

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

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UntagResource)

responseUpdateHttpNamespace :: UpdateHttpNamespaceResponse -> TestTree
responseUpdateHttpNamespace =
  res
    "UpdateHttpNamespaceResponse"
    "fixture/UpdateHttpNamespaceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateHttpNamespace)

responseUpdateInstanceCustomHealthStatus :: UpdateInstanceCustomHealthStatusResponse -> TestTree
responseUpdateInstanceCustomHealthStatus =
  res
    "UpdateInstanceCustomHealthStatusResponse"
    "fixture/UpdateInstanceCustomHealthStatusResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateInstanceCustomHealthStatus)

responseUpdatePrivateDnsNamespace :: UpdatePrivateDnsNamespaceResponse -> TestTree
responseUpdatePrivateDnsNamespace =
  res
    "UpdatePrivateDnsNamespaceResponse"
    "fixture/UpdatePrivateDnsNamespaceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdatePrivateDnsNamespace)

responseUpdatePublicDnsNamespace :: UpdatePublicDnsNamespaceResponse -> TestTree
responseUpdatePublicDnsNamespace =
  res
    "UpdatePublicDnsNamespaceResponse"
    "fixture/UpdatePublicDnsNamespaceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdatePublicDnsNamespace)

responseUpdateService :: UpdateServiceResponse -> TestTree
responseUpdateService =
  res
    "UpdateServiceResponse"
    "fixture/UpdateServiceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateService)
