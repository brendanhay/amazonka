{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.AppRunner
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
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
--         , requestCreateService $
--             newCreateService
--
--         , requestDeleteAutoScalingConfiguration $
--             newDeleteAutoScalingConfiguration
--
--         , requestDeleteConnection $
--             newDeleteConnection
--
--         , requestDeleteService $
--             newDeleteService
--
--         , requestDescribeAutoScalingConfiguration $
--             newDescribeAutoScalingConfiguration
--
--         , requestDescribeCustomDomains $
--             newDescribeCustomDomains
--
--         , requestDescribeService $
--             newDescribeService
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
--         , requestListOperations $
--             newListOperations
--
--         , requestListServices $
--             newListServices
--
--         , requestListTagsForResource $
--             newListTagsForResource
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
--         , responseCreateService $
--             newCreateServiceResponse
--
--         , responseDeleteAutoScalingConfiguration $
--             newDeleteAutoScalingConfigurationResponse
--
--         , responseDeleteConnection $
--             newDeleteConnectionResponse
--
--         , responseDeleteService $
--             newDeleteServiceResponse
--
--         , responseDescribeAutoScalingConfiguration $
--             newDescribeAutoScalingConfigurationResponse
--
--         , responseDescribeCustomDomains $
--             newDescribeCustomDomainsResponse
--
--         , responseDescribeService $
--             newDescribeServiceResponse
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
--         , responseListOperations $
--             newListOperationsResponse
--
--         , responseListServices $
--             newListServicesResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
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

requestCreateService :: CreateService -> TestTree
requestCreateService =
  req
    "CreateService"
    "fixture/CreateService.yaml"

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

requestDeleteService :: DeleteService -> TestTree
requestDeleteService =
  req
    "DeleteService"
    "fixture/DeleteService.yaml"

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

requestDescribeService :: DescribeService -> TestTree
requestDescribeService =
  req
    "DescribeService"
    "fixture/DescribeService.yaml"

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

responseCreateService :: CreateServiceResponse -> TestTree
responseCreateService =
  res
    "CreateServiceResponse"
    "fixture/CreateServiceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateService)

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

responseDeleteService :: DeleteServiceResponse -> TestTree
responseDeleteService =
  res
    "DeleteServiceResponse"
    "fixture/DeleteServiceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteService)

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

responseDescribeService :: DescribeServiceResponse -> TestTree
responseDescribeService =
  res
    "DescribeServiceResponse"
    "fixture/DescribeServiceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeService)

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
