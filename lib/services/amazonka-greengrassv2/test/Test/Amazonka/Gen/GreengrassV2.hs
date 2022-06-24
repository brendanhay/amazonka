{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.GreengrassV2
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.GreengrassV2 where

import Amazonka.GreengrassV2
import qualified Data.Proxy as Proxy
import Test.Amazonka.Fixture
import Test.Amazonka.GreengrassV2.Internal
import Test.Amazonka.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestBatchAssociateClientDeviceWithCoreDevice $
--             newBatchAssociateClientDeviceWithCoreDevice
--
--         , requestBatchDisassociateClientDeviceFromCoreDevice $
--             newBatchDisassociateClientDeviceFromCoreDevice
--
--         , requestCancelDeployment $
--             newCancelDeployment
--
--         , requestCreateComponentVersion $
--             newCreateComponentVersion
--
--         , requestCreateDeployment $
--             newCreateDeployment
--
--         , requestDeleteComponent $
--             newDeleteComponent
--
--         , requestDeleteCoreDevice $
--             newDeleteCoreDevice
--
--         , requestDescribeComponent $
--             newDescribeComponent
--
--         , requestGetComponent $
--             newGetComponent
--
--         , requestGetComponentVersionArtifact $
--             newGetComponentVersionArtifact
--
--         , requestGetCoreDevice $
--             newGetCoreDevice
--
--         , requestGetDeployment $
--             newGetDeployment
--
--         , requestListClientDevicesAssociatedWithCoreDevice $
--             newListClientDevicesAssociatedWithCoreDevice
--
--         , requestListComponentVersions $
--             newListComponentVersions
--
--         , requestListComponents $
--             newListComponents
--
--         , requestListCoreDevices $
--             newListCoreDevices
--
--         , requestListDeployments $
--             newListDeployments
--
--         , requestListEffectiveDeployments $
--             newListEffectiveDeployments
--
--         , requestListInstalledComponents $
--             newListInstalledComponents
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestResolveComponentCandidates $
--             newResolveComponentCandidates
--
--         , requestTagResource $
--             newTagResource
--
--         , requestUntagResource $
--             newUntagResource
--
--           ]

--     , testGroup "response"
--         [ responseBatchAssociateClientDeviceWithCoreDevice $
--             newBatchAssociateClientDeviceWithCoreDeviceResponse
--
--         , responseBatchDisassociateClientDeviceFromCoreDevice $
--             newBatchDisassociateClientDeviceFromCoreDeviceResponse
--
--         , responseCancelDeployment $
--             newCancelDeploymentResponse
--
--         , responseCreateComponentVersion $
--             newCreateComponentVersionResponse
--
--         , responseCreateDeployment $
--             newCreateDeploymentResponse
--
--         , responseDeleteComponent $
--             newDeleteComponentResponse
--
--         , responseDeleteCoreDevice $
--             newDeleteCoreDeviceResponse
--
--         , responseDescribeComponent $
--             newDescribeComponentResponse
--
--         , responseGetComponent $
--             newGetComponentResponse
--
--         , responseGetComponentVersionArtifact $
--             newGetComponentVersionArtifactResponse
--
--         , responseGetCoreDevice $
--             newGetCoreDeviceResponse
--
--         , responseGetDeployment $
--             newGetDeploymentResponse
--
--         , responseListClientDevicesAssociatedWithCoreDevice $
--             newListClientDevicesAssociatedWithCoreDeviceResponse
--
--         , responseListComponentVersions $
--             newListComponentVersionsResponse
--
--         , responseListComponents $
--             newListComponentsResponse
--
--         , responseListCoreDevices $
--             newListCoreDevicesResponse
--
--         , responseListDeployments $
--             newListDeploymentsResponse
--
--         , responseListEffectiveDeployments $
--             newListEffectiveDeploymentsResponse
--
--         , responseListInstalledComponents $
--             newListInstalledComponentsResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseResolveComponentCandidates $
--             newResolveComponentCandidatesResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--           ]
--     ]

-- Requests

requestBatchAssociateClientDeviceWithCoreDevice :: BatchAssociateClientDeviceWithCoreDevice -> TestTree
requestBatchAssociateClientDeviceWithCoreDevice =
  req
    "BatchAssociateClientDeviceWithCoreDevice"
    "fixture/BatchAssociateClientDeviceWithCoreDevice.yaml"

requestBatchDisassociateClientDeviceFromCoreDevice :: BatchDisassociateClientDeviceFromCoreDevice -> TestTree
requestBatchDisassociateClientDeviceFromCoreDevice =
  req
    "BatchDisassociateClientDeviceFromCoreDevice"
    "fixture/BatchDisassociateClientDeviceFromCoreDevice.yaml"

requestCancelDeployment :: CancelDeployment -> TestTree
requestCancelDeployment =
  req
    "CancelDeployment"
    "fixture/CancelDeployment.yaml"

requestCreateComponentVersion :: CreateComponentVersion -> TestTree
requestCreateComponentVersion =
  req
    "CreateComponentVersion"
    "fixture/CreateComponentVersion.yaml"

requestCreateDeployment :: CreateDeployment -> TestTree
requestCreateDeployment =
  req
    "CreateDeployment"
    "fixture/CreateDeployment.yaml"

requestDeleteComponent :: DeleteComponent -> TestTree
requestDeleteComponent =
  req
    "DeleteComponent"
    "fixture/DeleteComponent.yaml"

requestDeleteCoreDevice :: DeleteCoreDevice -> TestTree
requestDeleteCoreDevice =
  req
    "DeleteCoreDevice"
    "fixture/DeleteCoreDevice.yaml"

requestDescribeComponent :: DescribeComponent -> TestTree
requestDescribeComponent =
  req
    "DescribeComponent"
    "fixture/DescribeComponent.yaml"

requestGetComponent :: GetComponent -> TestTree
requestGetComponent =
  req
    "GetComponent"
    "fixture/GetComponent.yaml"

requestGetComponentVersionArtifact :: GetComponentVersionArtifact -> TestTree
requestGetComponentVersionArtifact =
  req
    "GetComponentVersionArtifact"
    "fixture/GetComponentVersionArtifact.yaml"

requestGetCoreDevice :: GetCoreDevice -> TestTree
requestGetCoreDevice =
  req
    "GetCoreDevice"
    "fixture/GetCoreDevice.yaml"

requestGetDeployment :: GetDeployment -> TestTree
requestGetDeployment =
  req
    "GetDeployment"
    "fixture/GetDeployment.yaml"

requestListClientDevicesAssociatedWithCoreDevice :: ListClientDevicesAssociatedWithCoreDevice -> TestTree
requestListClientDevicesAssociatedWithCoreDevice =
  req
    "ListClientDevicesAssociatedWithCoreDevice"
    "fixture/ListClientDevicesAssociatedWithCoreDevice.yaml"

requestListComponentVersions :: ListComponentVersions -> TestTree
requestListComponentVersions =
  req
    "ListComponentVersions"
    "fixture/ListComponentVersions.yaml"

requestListComponents :: ListComponents -> TestTree
requestListComponents =
  req
    "ListComponents"
    "fixture/ListComponents.yaml"

requestListCoreDevices :: ListCoreDevices -> TestTree
requestListCoreDevices =
  req
    "ListCoreDevices"
    "fixture/ListCoreDevices.yaml"

requestListDeployments :: ListDeployments -> TestTree
requestListDeployments =
  req
    "ListDeployments"
    "fixture/ListDeployments.yaml"

requestListEffectiveDeployments :: ListEffectiveDeployments -> TestTree
requestListEffectiveDeployments =
  req
    "ListEffectiveDeployments"
    "fixture/ListEffectiveDeployments.yaml"

requestListInstalledComponents :: ListInstalledComponents -> TestTree
requestListInstalledComponents =
  req
    "ListInstalledComponents"
    "fixture/ListInstalledComponents.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestResolveComponentCandidates :: ResolveComponentCandidates -> TestTree
requestResolveComponentCandidates =
  req
    "ResolveComponentCandidates"
    "fixture/ResolveComponentCandidates.yaml"

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

-- Responses

responseBatchAssociateClientDeviceWithCoreDevice :: BatchAssociateClientDeviceWithCoreDeviceResponse -> TestTree
responseBatchAssociateClientDeviceWithCoreDevice =
  res
    "BatchAssociateClientDeviceWithCoreDeviceResponse"
    "fixture/BatchAssociateClientDeviceWithCoreDeviceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchAssociateClientDeviceWithCoreDevice)

responseBatchDisassociateClientDeviceFromCoreDevice :: BatchDisassociateClientDeviceFromCoreDeviceResponse -> TestTree
responseBatchDisassociateClientDeviceFromCoreDevice =
  res
    "BatchDisassociateClientDeviceFromCoreDeviceResponse"
    "fixture/BatchDisassociateClientDeviceFromCoreDeviceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchDisassociateClientDeviceFromCoreDevice)

responseCancelDeployment :: CancelDeploymentResponse -> TestTree
responseCancelDeployment =
  res
    "CancelDeploymentResponse"
    "fixture/CancelDeploymentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CancelDeployment)

responseCreateComponentVersion :: CreateComponentVersionResponse -> TestTree
responseCreateComponentVersion =
  res
    "CreateComponentVersionResponse"
    "fixture/CreateComponentVersionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateComponentVersion)

responseCreateDeployment :: CreateDeploymentResponse -> TestTree
responseCreateDeployment =
  res
    "CreateDeploymentResponse"
    "fixture/CreateDeploymentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateDeployment)

responseDeleteComponent :: DeleteComponentResponse -> TestTree
responseDeleteComponent =
  res
    "DeleteComponentResponse"
    "fixture/DeleteComponentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteComponent)

responseDeleteCoreDevice :: DeleteCoreDeviceResponse -> TestTree
responseDeleteCoreDevice =
  res
    "DeleteCoreDeviceResponse"
    "fixture/DeleteCoreDeviceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteCoreDevice)

responseDescribeComponent :: DescribeComponentResponse -> TestTree
responseDescribeComponent =
  res
    "DescribeComponentResponse"
    "fixture/DescribeComponentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeComponent)

responseGetComponent :: GetComponentResponse -> TestTree
responseGetComponent =
  res
    "GetComponentResponse"
    "fixture/GetComponentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetComponent)

responseGetComponentVersionArtifact :: GetComponentVersionArtifactResponse -> TestTree
responseGetComponentVersionArtifact =
  res
    "GetComponentVersionArtifactResponse"
    "fixture/GetComponentVersionArtifactResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetComponentVersionArtifact)

responseGetCoreDevice :: GetCoreDeviceResponse -> TestTree
responseGetCoreDevice =
  res
    "GetCoreDeviceResponse"
    "fixture/GetCoreDeviceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetCoreDevice)

responseGetDeployment :: GetDeploymentResponse -> TestTree
responseGetDeployment =
  res
    "GetDeploymentResponse"
    "fixture/GetDeploymentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetDeployment)

responseListClientDevicesAssociatedWithCoreDevice :: ListClientDevicesAssociatedWithCoreDeviceResponse -> TestTree
responseListClientDevicesAssociatedWithCoreDevice =
  res
    "ListClientDevicesAssociatedWithCoreDeviceResponse"
    "fixture/ListClientDevicesAssociatedWithCoreDeviceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListClientDevicesAssociatedWithCoreDevice)

responseListComponentVersions :: ListComponentVersionsResponse -> TestTree
responseListComponentVersions =
  res
    "ListComponentVersionsResponse"
    "fixture/ListComponentVersionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListComponentVersions)

responseListComponents :: ListComponentsResponse -> TestTree
responseListComponents =
  res
    "ListComponentsResponse"
    "fixture/ListComponentsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListComponents)

responseListCoreDevices :: ListCoreDevicesResponse -> TestTree
responseListCoreDevices =
  res
    "ListCoreDevicesResponse"
    "fixture/ListCoreDevicesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListCoreDevices)

responseListDeployments :: ListDeploymentsResponse -> TestTree
responseListDeployments =
  res
    "ListDeploymentsResponse"
    "fixture/ListDeploymentsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListDeployments)

responseListEffectiveDeployments :: ListEffectiveDeploymentsResponse -> TestTree
responseListEffectiveDeployments =
  res
    "ListEffectiveDeploymentsResponse"
    "fixture/ListEffectiveDeploymentsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListEffectiveDeployments)

responseListInstalledComponents :: ListInstalledComponentsResponse -> TestTree
responseListInstalledComponents =
  res
    "ListInstalledComponentsResponse"
    "fixture/ListInstalledComponentsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListInstalledComponents)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responseResolveComponentCandidates :: ResolveComponentCandidatesResponse -> TestTree
responseResolveComponentCandidates =
  res
    "ResolveComponentCandidatesResponse"
    "fixture/ResolveComponentCandidatesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ResolveComponentCandidates)

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
