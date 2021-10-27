{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.AppRunner
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.AWS.Gen.AppRunner where

import Data.Proxy
import Network.AWS.AppRunner
import Test.AWS.AppRunner.Internal
import Test.AWS.Fixture
import Test.AWS.Prelude
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
--         , requestResumeService $
--             newResumeService
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
--         , requestAssociateCustomDomain $
--             newAssociateCustomDomain
--
--         , requestListConnections $
--             newListConnections
--
--         , requestDeleteConnection $
--             newDeleteConnection
--
--         , requestDescribeAutoScalingConfiguration $
--             newDescribeAutoScalingConfiguration
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestCreateConnection $
--             newCreateConnection
--
--         , requestDescribeCustomDomains $
--             newDescribeCustomDomains
--
--         , requestDescribeService $
--             newDescribeService
--
--         , requestDeleteAutoScalingConfiguration $
--             newDeleteAutoScalingConfiguration
--
--         , requestListAutoScalingConfigurations $
--             newListAutoScalingConfigurations
--
--         , requestDisassociateCustomDomain $
--             newDisassociateCustomDomain
--
--         , requestPauseService $
--             newPauseService
--
--         , requestTagResource $
--             newTagResource
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestCreateAutoScalingConfiguration $
--             newCreateAutoScalingConfiguration
--
--         , requestStartDeployment $
--             newStartDeployment
--
--         , requestCreateService $
--             newCreateService
--
--           ]

--     , testGroup "response"
--         [ responseListServices $
--             newListServicesResponse
--
--         , responseResumeService $
--             newResumeServiceResponse
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
--         , responseAssociateCustomDomain $
--             newAssociateCustomDomainResponse
--
--         , responseListConnections $
--             newListConnectionsResponse
--
--         , responseDeleteConnection $
--             newDeleteConnectionResponse
--
--         , responseDescribeAutoScalingConfiguration $
--             newDescribeAutoScalingConfigurationResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseCreateConnection $
--             newCreateConnectionResponse
--
--         , responseDescribeCustomDomains $
--             newDescribeCustomDomainsResponse
--
--         , responseDescribeService $
--             newDescribeServiceResponse
--
--         , responseDeleteAutoScalingConfiguration $
--             newDeleteAutoScalingConfigurationResponse
--
--         , responseListAutoScalingConfigurations $
--             newListAutoScalingConfigurationsResponse
--
--         , responseDisassociateCustomDomain $
--             newDisassociateCustomDomainResponse
--
--         , responsePauseService $
--             newPauseServiceResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseCreateAutoScalingConfiguration $
--             newCreateAutoScalingConfigurationResponse
--
--         , responseStartDeployment $
--             newStartDeploymentResponse
--
--         , responseCreateService $
--             newCreateServiceResponse
--
--           ]
--     ]

-- Requests

requestListServices :: ListServices -> TestTree
requestListServices =
  req
    "ListServices"
    "fixture/ListServices.yaml"

requestResumeService :: ResumeService -> TestTree
requestResumeService =
  req
    "ResumeService"
    "fixture/ResumeService.yaml"

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

requestAssociateCustomDomain :: AssociateCustomDomain -> TestTree
requestAssociateCustomDomain =
  req
    "AssociateCustomDomain"
    "fixture/AssociateCustomDomain.yaml"

requestListConnections :: ListConnections -> TestTree
requestListConnections =
  req
    "ListConnections"
    "fixture/ListConnections.yaml"

requestDeleteConnection :: DeleteConnection -> TestTree
requestDeleteConnection =
  req
    "DeleteConnection"
    "fixture/DeleteConnection.yaml"

requestDescribeAutoScalingConfiguration :: DescribeAutoScalingConfiguration -> TestTree
requestDescribeAutoScalingConfiguration =
  req
    "DescribeAutoScalingConfiguration"
    "fixture/DescribeAutoScalingConfiguration.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestCreateConnection :: CreateConnection -> TestTree
requestCreateConnection =
  req
    "CreateConnection"
    "fixture/CreateConnection.yaml"

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

requestDeleteAutoScalingConfiguration :: DeleteAutoScalingConfiguration -> TestTree
requestDeleteAutoScalingConfiguration =
  req
    "DeleteAutoScalingConfiguration"
    "fixture/DeleteAutoScalingConfiguration.yaml"

requestListAutoScalingConfigurations :: ListAutoScalingConfigurations -> TestTree
requestListAutoScalingConfigurations =
  req
    "ListAutoScalingConfigurations"
    "fixture/ListAutoScalingConfigurations.yaml"

requestDisassociateCustomDomain :: DisassociateCustomDomain -> TestTree
requestDisassociateCustomDomain =
  req
    "DisassociateCustomDomain"
    "fixture/DisassociateCustomDomain.yaml"

requestPauseService :: PauseService -> TestTree
requestPauseService =
  req
    "PauseService"
    "fixture/PauseService.yaml"

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

requestCreateAutoScalingConfiguration :: CreateAutoScalingConfiguration -> TestTree
requestCreateAutoScalingConfiguration =
  req
    "CreateAutoScalingConfiguration"
    "fixture/CreateAutoScalingConfiguration.yaml"

requestStartDeployment :: StartDeployment -> TestTree
requestStartDeployment =
  req
    "StartDeployment"
    "fixture/StartDeployment.yaml"

requestCreateService :: CreateService -> TestTree
requestCreateService =
  req
    "CreateService"
    "fixture/CreateService.yaml"

-- Responses

responseListServices :: ListServicesResponse -> TestTree
responseListServices =
  res
    "ListServicesResponse"
    "fixture/ListServicesResponse.proto"
    defaultService
    (Proxy :: Proxy ListServices)

responseResumeService :: ResumeServiceResponse -> TestTree
responseResumeService =
  res
    "ResumeServiceResponse"
    "fixture/ResumeServiceResponse.proto"
    defaultService
    (Proxy :: Proxy ResumeService)

responseDeleteService :: DeleteServiceResponse -> TestTree
responseDeleteService =
  res
    "DeleteServiceResponse"
    "fixture/DeleteServiceResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteService)

responseUpdateService :: UpdateServiceResponse -> TestTree
responseUpdateService =
  res
    "UpdateServiceResponse"
    "fixture/UpdateServiceResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateService)

responseListOperations :: ListOperationsResponse -> TestTree
responseListOperations =
  res
    "ListOperationsResponse"
    "fixture/ListOperationsResponse.proto"
    defaultService
    (Proxy :: Proxy ListOperations)

responseAssociateCustomDomain :: AssociateCustomDomainResponse -> TestTree
responseAssociateCustomDomain =
  res
    "AssociateCustomDomainResponse"
    "fixture/AssociateCustomDomainResponse.proto"
    defaultService
    (Proxy :: Proxy AssociateCustomDomain)

responseListConnections :: ListConnectionsResponse -> TestTree
responseListConnections =
  res
    "ListConnectionsResponse"
    "fixture/ListConnectionsResponse.proto"
    defaultService
    (Proxy :: Proxy ListConnections)

responseDeleteConnection :: DeleteConnectionResponse -> TestTree
responseDeleteConnection =
  res
    "DeleteConnectionResponse"
    "fixture/DeleteConnectionResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteConnection)

responseDescribeAutoScalingConfiguration :: DescribeAutoScalingConfigurationResponse -> TestTree
responseDescribeAutoScalingConfiguration =
  res
    "DescribeAutoScalingConfigurationResponse"
    "fixture/DescribeAutoScalingConfigurationResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeAutoScalingConfiguration)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy :: Proxy ListTagsForResource)

responseCreateConnection :: CreateConnectionResponse -> TestTree
responseCreateConnection =
  res
    "CreateConnectionResponse"
    "fixture/CreateConnectionResponse.proto"
    defaultService
    (Proxy :: Proxy CreateConnection)

responseDescribeCustomDomains :: DescribeCustomDomainsResponse -> TestTree
responseDescribeCustomDomains =
  res
    "DescribeCustomDomainsResponse"
    "fixture/DescribeCustomDomainsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeCustomDomains)

responseDescribeService :: DescribeServiceResponse -> TestTree
responseDescribeService =
  res
    "DescribeServiceResponse"
    "fixture/DescribeServiceResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeService)

responseDeleteAutoScalingConfiguration :: DeleteAutoScalingConfigurationResponse -> TestTree
responseDeleteAutoScalingConfiguration =
  res
    "DeleteAutoScalingConfigurationResponse"
    "fixture/DeleteAutoScalingConfigurationResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteAutoScalingConfiguration)

responseListAutoScalingConfigurations :: ListAutoScalingConfigurationsResponse -> TestTree
responseListAutoScalingConfigurations =
  res
    "ListAutoScalingConfigurationsResponse"
    "fixture/ListAutoScalingConfigurationsResponse.proto"
    defaultService
    (Proxy :: Proxy ListAutoScalingConfigurations)

responseDisassociateCustomDomain :: DisassociateCustomDomainResponse -> TestTree
responseDisassociateCustomDomain =
  res
    "DisassociateCustomDomainResponse"
    "fixture/DisassociateCustomDomainResponse.proto"
    defaultService
    (Proxy :: Proxy DisassociateCustomDomain)

responsePauseService :: PauseServiceResponse -> TestTree
responsePauseService =
  res
    "PauseServiceResponse"
    "fixture/PauseServiceResponse.proto"
    defaultService
    (Proxy :: Proxy PauseService)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy :: Proxy TagResource)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy :: Proxy UntagResource)

responseCreateAutoScalingConfiguration :: CreateAutoScalingConfigurationResponse -> TestTree
responseCreateAutoScalingConfiguration =
  res
    "CreateAutoScalingConfigurationResponse"
    "fixture/CreateAutoScalingConfigurationResponse.proto"
    defaultService
    (Proxy :: Proxy CreateAutoScalingConfiguration)

responseStartDeployment :: StartDeploymentResponse -> TestTree
responseStartDeployment =
  res
    "StartDeploymentResponse"
    "fixture/StartDeploymentResponse.proto"
    defaultService
    (Proxy :: Proxy StartDeployment)

responseCreateService :: CreateServiceResponse -> TestTree
responseCreateService =
  res
    "CreateServiceResponse"
    "fixture/CreateServiceResponse.proto"
    defaultService
    (Proxy :: Proxy CreateService)
