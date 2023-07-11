{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.AppRunner
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.AppRunner where

import Amazonka.AppRunner
import qualified Data.Proxy as Proxy
import Test.Amazonka.AppRunner.Internal
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
--         [ requestAssociateCustomDomain $
--             newAssociateCustomDomain
--
--         , requestCreateAutoScalingConfiguration $
--             newCreateAutoScalingConfiguration
--
--         , requestCreateConnection $
--             newCreateConnection
--
--         , requestCreateObservabilityConfiguration $
--             newCreateObservabilityConfiguration
--
--         , requestCreateService $
--             newCreateService
--
--         , requestCreateVpcConnector $
--             newCreateVpcConnector
--
--         , requestCreateVpcIngressConnection $
--             newCreateVpcIngressConnection
--
--         , requestDeleteAutoScalingConfiguration $
--             newDeleteAutoScalingConfiguration
--
--         , requestDeleteConnection $
--             newDeleteConnection
--
--         , requestDeleteObservabilityConfiguration $
--             newDeleteObservabilityConfiguration
--
--         , requestDeleteService $
--             newDeleteService
--
--         , requestDeleteVpcConnector $
--             newDeleteVpcConnector
--
--         , requestDeleteVpcIngressConnection $
--             newDeleteVpcIngressConnection
--
--         , requestDescribeAutoScalingConfiguration $
--             newDescribeAutoScalingConfiguration
--
--         , requestDescribeCustomDomains $
--             newDescribeCustomDomains
--
--         , requestDescribeObservabilityConfiguration $
--             newDescribeObservabilityConfiguration
--
--         , requestDescribeService $
--             newDescribeService
--
--         , requestDescribeVpcConnector $
--             newDescribeVpcConnector
--
--         , requestDescribeVpcIngressConnection $
--             newDescribeVpcIngressConnection
--
--         , requestDisassociateCustomDomain $
--             newDisassociateCustomDomain
--
--         , requestListAutoScalingConfigurations $
--             newListAutoScalingConfigurations
--
--         , requestListConnections $
--             newListConnections
--
--         , requestListObservabilityConfigurations $
--             newListObservabilityConfigurations
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
--         , requestListVpcConnectors $
--             newListVpcConnectors
--
--         , requestListVpcIngressConnections $
--             newListVpcIngressConnections
--
--         , requestPauseService $
--             newPauseService
--
--         , requestResumeService $
--             newResumeService
--
--         , requestStartDeployment $
--             newStartDeployment
--
--         , requestTagResource $
--             newTagResource
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestUpdateService $
--             newUpdateService
--
--         , requestUpdateVpcIngressConnection $
--             newUpdateVpcIngressConnection
--
--           ]

--     , testGroup "response"
--         [ responseAssociateCustomDomain $
--             newAssociateCustomDomainResponse
--
--         , responseCreateAutoScalingConfiguration $
--             newCreateAutoScalingConfigurationResponse
--
--         , responseCreateConnection $
--             newCreateConnectionResponse
--
--         , responseCreateObservabilityConfiguration $
--             newCreateObservabilityConfigurationResponse
--
--         , responseCreateService $
--             newCreateServiceResponse
--
--         , responseCreateVpcConnector $
--             newCreateVpcConnectorResponse
--
--         , responseCreateVpcIngressConnection $
--             newCreateVpcIngressConnectionResponse
--
--         , responseDeleteAutoScalingConfiguration $
--             newDeleteAutoScalingConfigurationResponse
--
--         , responseDeleteConnection $
--             newDeleteConnectionResponse
--
--         , responseDeleteObservabilityConfiguration $
--             newDeleteObservabilityConfigurationResponse
--
--         , responseDeleteService $
--             newDeleteServiceResponse
--
--         , responseDeleteVpcConnector $
--             newDeleteVpcConnectorResponse
--
--         , responseDeleteVpcIngressConnection $
--             newDeleteVpcIngressConnectionResponse
--
--         , responseDescribeAutoScalingConfiguration $
--             newDescribeAutoScalingConfigurationResponse
--
--         , responseDescribeCustomDomains $
--             newDescribeCustomDomainsResponse
--
--         , responseDescribeObservabilityConfiguration $
--             newDescribeObservabilityConfigurationResponse
--
--         , responseDescribeService $
--             newDescribeServiceResponse
--
--         , responseDescribeVpcConnector $
--             newDescribeVpcConnectorResponse
--
--         , responseDescribeVpcIngressConnection $
--             newDescribeVpcIngressConnectionResponse
--
--         , responseDisassociateCustomDomain $
--             newDisassociateCustomDomainResponse
--
--         , responseListAutoScalingConfigurations $
--             newListAutoScalingConfigurationsResponse
--
--         , responseListConnections $
--             newListConnectionsResponse
--
--         , responseListObservabilityConfigurations $
--             newListObservabilityConfigurationsResponse
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
--         , responseListVpcConnectors $
--             newListVpcConnectorsResponse
--
--         , responseListVpcIngressConnections $
--             newListVpcIngressConnectionsResponse
--
--         , responsePauseService $
--             newPauseServiceResponse
--
--         , responseResumeService $
--             newResumeServiceResponse
--
--         , responseStartDeployment $
--             newStartDeploymentResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseUpdateService $
--             newUpdateServiceResponse
--
--         , responseUpdateVpcIngressConnection $
--             newUpdateVpcIngressConnectionResponse
--
--           ]
--     ]

-- Requests

requestAssociateCustomDomain :: AssociateCustomDomain -> TestTree
requestAssociateCustomDomain =
  req
    "AssociateCustomDomain"
    "fixture/AssociateCustomDomain.yaml"

requestCreateAutoScalingConfiguration :: CreateAutoScalingConfiguration -> TestTree
requestCreateAutoScalingConfiguration =
  req
    "CreateAutoScalingConfiguration"
    "fixture/CreateAutoScalingConfiguration.yaml"

requestCreateConnection :: CreateConnection -> TestTree
requestCreateConnection =
  req
    "CreateConnection"
    "fixture/CreateConnection.yaml"

requestCreateObservabilityConfiguration :: CreateObservabilityConfiguration -> TestTree
requestCreateObservabilityConfiguration =
  req
    "CreateObservabilityConfiguration"
    "fixture/CreateObservabilityConfiguration.yaml"

requestCreateService :: CreateService -> TestTree
requestCreateService =
  req
    "CreateService"
    "fixture/CreateService.yaml"

requestCreateVpcConnector :: CreateVpcConnector -> TestTree
requestCreateVpcConnector =
  req
    "CreateVpcConnector"
    "fixture/CreateVpcConnector.yaml"

requestCreateVpcIngressConnection :: CreateVpcIngressConnection -> TestTree
requestCreateVpcIngressConnection =
  req
    "CreateVpcIngressConnection"
    "fixture/CreateVpcIngressConnection.yaml"

requestDeleteAutoScalingConfiguration :: DeleteAutoScalingConfiguration -> TestTree
requestDeleteAutoScalingConfiguration =
  req
    "DeleteAutoScalingConfiguration"
    "fixture/DeleteAutoScalingConfiguration.yaml"

requestDeleteConnection :: DeleteConnection -> TestTree
requestDeleteConnection =
  req
    "DeleteConnection"
    "fixture/DeleteConnection.yaml"

requestDeleteObservabilityConfiguration :: DeleteObservabilityConfiguration -> TestTree
requestDeleteObservabilityConfiguration =
  req
    "DeleteObservabilityConfiguration"
    "fixture/DeleteObservabilityConfiguration.yaml"

requestDeleteService :: DeleteService -> TestTree
requestDeleteService =
  req
    "DeleteService"
    "fixture/DeleteService.yaml"

requestDeleteVpcConnector :: DeleteVpcConnector -> TestTree
requestDeleteVpcConnector =
  req
    "DeleteVpcConnector"
    "fixture/DeleteVpcConnector.yaml"

requestDeleteVpcIngressConnection :: DeleteVpcIngressConnection -> TestTree
requestDeleteVpcIngressConnection =
  req
    "DeleteVpcIngressConnection"
    "fixture/DeleteVpcIngressConnection.yaml"

requestDescribeAutoScalingConfiguration :: DescribeAutoScalingConfiguration -> TestTree
requestDescribeAutoScalingConfiguration =
  req
    "DescribeAutoScalingConfiguration"
    "fixture/DescribeAutoScalingConfiguration.yaml"

requestDescribeCustomDomains :: DescribeCustomDomains -> TestTree
requestDescribeCustomDomains =
  req
    "DescribeCustomDomains"
    "fixture/DescribeCustomDomains.yaml"

requestDescribeObservabilityConfiguration :: DescribeObservabilityConfiguration -> TestTree
requestDescribeObservabilityConfiguration =
  req
    "DescribeObservabilityConfiguration"
    "fixture/DescribeObservabilityConfiguration.yaml"

requestDescribeService :: DescribeService -> TestTree
requestDescribeService =
  req
    "DescribeService"
    "fixture/DescribeService.yaml"

requestDescribeVpcConnector :: DescribeVpcConnector -> TestTree
requestDescribeVpcConnector =
  req
    "DescribeVpcConnector"
    "fixture/DescribeVpcConnector.yaml"

requestDescribeVpcIngressConnection :: DescribeVpcIngressConnection -> TestTree
requestDescribeVpcIngressConnection =
  req
    "DescribeVpcIngressConnection"
    "fixture/DescribeVpcIngressConnection.yaml"

requestDisassociateCustomDomain :: DisassociateCustomDomain -> TestTree
requestDisassociateCustomDomain =
  req
    "DisassociateCustomDomain"
    "fixture/DisassociateCustomDomain.yaml"

requestListAutoScalingConfigurations :: ListAutoScalingConfigurations -> TestTree
requestListAutoScalingConfigurations =
  req
    "ListAutoScalingConfigurations"
    "fixture/ListAutoScalingConfigurations.yaml"

requestListConnections :: ListConnections -> TestTree
requestListConnections =
  req
    "ListConnections"
    "fixture/ListConnections.yaml"

requestListObservabilityConfigurations :: ListObservabilityConfigurations -> TestTree
requestListObservabilityConfigurations =
  req
    "ListObservabilityConfigurations"
    "fixture/ListObservabilityConfigurations.yaml"

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

requestListVpcConnectors :: ListVpcConnectors -> TestTree
requestListVpcConnectors =
  req
    "ListVpcConnectors"
    "fixture/ListVpcConnectors.yaml"

requestListVpcIngressConnections :: ListVpcIngressConnections -> TestTree
requestListVpcIngressConnections =
  req
    "ListVpcIngressConnections"
    "fixture/ListVpcIngressConnections.yaml"

requestPauseService :: PauseService -> TestTree
requestPauseService =
  req
    "PauseService"
    "fixture/PauseService.yaml"

requestResumeService :: ResumeService -> TestTree
requestResumeService =
  req
    "ResumeService"
    "fixture/ResumeService.yaml"

requestStartDeployment :: StartDeployment -> TestTree
requestStartDeployment =
  req
    "StartDeployment"
    "fixture/StartDeployment.yaml"

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

requestUpdateService :: UpdateService -> TestTree
requestUpdateService =
  req
    "UpdateService"
    "fixture/UpdateService.yaml"

requestUpdateVpcIngressConnection :: UpdateVpcIngressConnection -> TestTree
requestUpdateVpcIngressConnection =
  req
    "UpdateVpcIngressConnection"
    "fixture/UpdateVpcIngressConnection.yaml"

-- Responses

responseAssociateCustomDomain :: AssociateCustomDomainResponse -> TestTree
responseAssociateCustomDomain =
  res
    "AssociateCustomDomainResponse"
    "fixture/AssociateCustomDomainResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssociateCustomDomain)

responseCreateAutoScalingConfiguration :: CreateAutoScalingConfigurationResponse -> TestTree
responseCreateAutoScalingConfiguration =
  res
    "CreateAutoScalingConfigurationResponse"
    "fixture/CreateAutoScalingConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateAutoScalingConfiguration)

responseCreateConnection :: CreateConnectionResponse -> TestTree
responseCreateConnection =
  res
    "CreateConnectionResponse"
    "fixture/CreateConnectionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateConnection)

responseCreateObservabilityConfiguration :: CreateObservabilityConfigurationResponse -> TestTree
responseCreateObservabilityConfiguration =
  res
    "CreateObservabilityConfigurationResponse"
    "fixture/CreateObservabilityConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateObservabilityConfiguration)

responseCreateService :: CreateServiceResponse -> TestTree
responseCreateService =
  res
    "CreateServiceResponse"
    "fixture/CreateServiceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateService)

responseCreateVpcConnector :: CreateVpcConnectorResponse -> TestTree
responseCreateVpcConnector =
  res
    "CreateVpcConnectorResponse"
    "fixture/CreateVpcConnectorResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateVpcConnector)

responseCreateVpcIngressConnection :: CreateVpcIngressConnectionResponse -> TestTree
responseCreateVpcIngressConnection =
  res
    "CreateVpcIngressConnectionResponse"
    "fixture/CreateVpcIngressConnectionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateVpcIngressConnection)

responseDeleteAutoScalingConfiguration :: DeleteAutoScalingConfigurationResponse -> TestTree
responseDeleteAutoScalingConfiguration =
  res
    "DeleteAutoScalingConfigurationResponse"
    "fixture/DeleteAutoScalingConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteAutoScalingConfiguration)

responseDeleteConnection :: DeleteConnectionResponse -> TestTree
responseDeleteConnection =
  res
    "DeleteConnectionResponse"
    "fixture/DeleteConnectionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteConnection)

responseDeleteObservabilityConfiguration :: DeleteObservabilityConfigurationResponse -> TestTree
responseDeleteObservabilityConfiguration =
  res
    "DeleteObservabilityConfigurationResponse"
    "fixture/DeleteObservabilityConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteObservabilityConfiguration)

responseDeleteService :: DeleteServiceResponse -> TestTree
responseDeleteService =
  res
    "DeleteServiceResponse"
    "fixture/DeleteServiceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteService)

responseDeleteVpcConnector :: DeleteVpcConnectorResponse -> TestTree
responseDeleteVpcConnector =
  res
    "DeleteVpcConnectorResponse"
    "fixture/DeleteVpcConnectorResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteVpcConnector)

responseDeleteVpcIngressConnection :: DeleteVpcIngressConnectionResponse -> TestTree
responseDeleteVpcIngressConnection =
  res
    "DeleteVpcIngressConnectionResponse"
    "fixture/DeleteVpcIngressConnectionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteVpcIngressConnection)

responseDescribeAutoScalingConfiguration :: DescribeAutoScalingConfigurationResponse -> TestTree
responseDescribeAutoScalingConfiguration =
  res
    "DescribeAutoScalingConfigurationResponse"
    "fixture/DescribeAutoScalingConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeAutoScalingConfiguration)

responseDescribeCustomDomains :: DescribeCustomDomainsResponse -> TestTree
responseDescribeCustomDomains =
  res
    "DescribeCustomDomainsResponse"
    "fixture/DescribeCustomDomainsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeCustomDomains)

responseDescribeObservabilityConfiguration :: DescribeObservabilityConfigurationResponse -> TestTree
responseDescribeObservabilityConfiguration =
  res
    "DescribeObservabilityConfigurationResponse"
    "fixture/DescribeObservabilityConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeObservabilityConfiguration)

responseDescribeService :: DescribeServiceResponse -> TestTree
responseDescribeService =
  res
    "DescribeServiceResponse"
    "fixture/DescribeServiceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeService)

responseDescribeVpcConnector :: DescribeVpcConnectorResponse -> TestTree
responseDescribeVpcConnector =
  res
    "DescribeVpcConnectorResponse"
    "fixture/DescribeVpcConnectorResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeVpcConnector)

responseDescribeVpcIngressConnection :: DescribeVpcIngressConnectionResponse -> TestTree
responseDescribeVpcIngressConnection =
  res
    "DescribeVpcIngressConnectionResponse"
    "fixture/DescribeVpcIngressConnectionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeVpcIngressConnection)

responseDisassociateCustomDomain :: DisassociateCustomDomainResponse -> TestTree
responseDisassociateCustomDomain =
  res
    "DisassociateCustomDomainResponse"
    "fixture/DisassociateCustomDomainResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisassociateCustomDomain)

responseListAutoScalingConfigurations :: ListAutoScalingConfigurationsResponse -> TestTree
responseListAutoScalingConfigurations =
  res
    "ListAutoScalingConfigurationsResponse"
    "fixture/ListAutoScalingConfigurationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListAutoScalingConfigurations)

responseListConnections :: ListConnectionsResponse -> TestTree
responseListConnections =
  res
    "ListConnectionsResponse"
    "fixture/ListConnectionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListConnections)

responseListObservabilityConfigurations :: ListObservabilityConfigurationsResponse -> TestTree
responseListObservabilityConfigurations =
  res
    "ListObservabilityConfigurationsResponse"
    "fixture/ListObservabilityConfigurationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListObservabilityConfigurations)

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

responseListVpcConnectors :: ListVpcConnectorsResponse -> TestTree
responseListVpcConnectors =
  res
    "ListVpcConnectorsResponse"
    "fixture/ListVpcConnectorsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListVpcConnectors)

responseListVpcIngressConnections :: ListVpcIngressConnectionsResponse -> TestTree
responseListVpcIngressConnections =
  res
    "ListVpcIngressConnectionsResponse"
    "fixture/ListVpcIngressConnectionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListVpcIngressConnections)

responsePauseService :: PauseServiceResponse -> TestTree
responsePauseService =
  res
    "PauseServiceResponse"
    "fixture/PauseServiceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PauseService)

responseResumeService :: ResumeServiceResponse -> TestTree
responseResumeService =
  res
    "ResumeServiceResponse"
    "fixture/ResumeServiceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ResumeService)

responseStartDeployment :: StartDeploymentResponse -> TestTree
responseStartDeployment =
  res
    "StartDeploymentResponse"
    "fixture/StartDeploymentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartDeployment)

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

responseUpdateService :: UpdateServiceResponse -> TestTree
responseUpdateService =
  res
    "UpdateServiceResponse"
    "fixture/UpdateServiceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateService)

responseUpdateVpcIngressConnection :: UpdateVpcIngressConnectionResponse -> TestTree
responseUpdateVpcIngressConnection =
  res
    "UpdateVpcIngressConnectionResponse"
    "fixture/UpdateVpcIngressConnectionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateVpcIngressConnection)
