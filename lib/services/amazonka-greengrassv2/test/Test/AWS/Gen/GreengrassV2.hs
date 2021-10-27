{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.GreengrassV2
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.AWS.Gen.GreengrassV2 where

import Data.Proxy
import Network.AWS.GreengrassV2
import Test.AWS.Fixture
import Test.AWS.GreengrassV2.Internal
import Test.AWS.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestListComponentVersions $
--             newListComponentVersions
--
--         , requestGetDeployment $
--             newGetDeployment
--
--         , requestDescribeComponent $
--             newDescribeComponent
--
--         , requestDeleteCoreDevice $
--             newDeleteCoreDevice
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestGetCoreDevice $
--             newGetCoreDevice
--
--         , requestGetComponentVersionArtifact $
--             newGetComponentVersionArtifact
--
--         , requestCreateDeployment $
--             newCreateDeployment
--
--         , requestBatchAssociateClientDeviceWithCoreDevice $
--             newBatchAssociateClientDeviceWithCoreDevice
--
--         , requestListInstalledComponents $
--             newListInstalledComponents
--
--         , requestCancelDeployment $
--             newCancelDeployment
--
--         , requestBatchDisassociateClientDeviceFromCoreDevice $
--             newBatchDisassociateClientDeviceFromCoreDevice
--
--         , requestListCoreDevices $
--             newListCoreDevices
--
--         , requestResolveComponentCandidates $
--             newResolveComponentCandidates
--
--         , requestListEffectiveDeployments $
--             newListEffectiveDeployments
--
--         , requestListDeployments $
--             newListDeployments
--
--         , requestTagResource $
--             newTagResource
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestListComponents $
--             newListComponents
--
--         , requestDeleteComponent $
--             newDeleteComponent
--
--         , requestCreateComponentVersion $
--             newCreateComponentVersion
--
--         , requestGetComponent $
--             newGetComponent
--
--         , requestListClientDevicesAssociatedWithCoreDevice $
--             newListClientDevicesAssociatedWithCoreDevice
--
--           ]

--     , testGroup "response"
--         [ responseListComponentVersions $
--             newListComponentVersionsResponse
--
--         , responseGetDeployment $
--             newGetDeploymentResponse
--
--         , responseDescribeComponent $
--             newDescribeComponentResponse
--
--         , responseDeleteCoreDevice $
--             newDeleteCoreDeviceResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseGetCoreDevice $
--             newGetCoreDeviceResponse
--
--         , responseGetComponentVersionArtifact $
--             newGetComponentVersionArtifactResponse
--
--         , responseCreateDeployment $
--             newCreateDeploymentResponse
--
--         , responseBatchAssociateClientDeviceWithCoreDevice $
--             newBatchAssociateClientDeviceWithCoreDeviceResponse
--
--         , responseListInstalledComponents $
--             newListInstalledComponentsResponse
--
--         , responseCancelDeployment $
--             newCancelDeploymentResponse
--
--         , responseBatchDisassociateClientDeviceFromCoreDevice $
--             newBatchDisassociateClientDeviceFromCoreDeviceResponse
--
--         , responseListCoreDevices $
--             newListCoreDevicesResponse
--
--         , responseResolveComponentCandidates $
--             newResolveComponentCandidatesResponse
--
--         , responseListEffectiveDeployments $
--             newListEffectiveDeploymentsResponse
--
--         , responseListDeployments $
--             newListDeploymentsResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseListComponents $
--             newListComponentsResponse
--
--         , responseDeleteComponent $
--             newDeleteComponentResponse
--
--         , responseCreateComponentVersion $
--             newCreateComponentVersionResponse
--
--         , responseGetComponent $
--             newGetComponentResponse
--
--         , responseListClientDevicesAssociatedWithCoreDevice $
--             newListClientDevicesAssociatedWithCoreDeviceResponse
--
--           ]
--     ]

-- Requests

requestListComponentVersions :: ListComponentVersions -> TestTree
requestListComponentVersions =
  req
    "ListComponentVersions"
    "fixture/ListComponentVersions.yaml"

requestGetDeployment :: GetDeployment -> TestTree
requestGetDeployment =
  req
    "GetDeployment"
    "fixture/GetDeployment.yaml"

requestDescribeComponent :: DescribeComponent -> TestTree
requestDescribeComponent =
  req
    "DescribeComponent"
    "fixture/DescribeComponent.yaml"

requestDeleteCoreDevice :: DeleteCoreDevice -> TestTree
requestDeleteCoreDevice =
  req
    "DeleteCoreDevice"
    "fixture/DeleteCoreDevice.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestGetCoreDevice :: GetCoreDevice -> TestTree
requestGetCoreDevice =
  req
    "GetCoreDevice"
    "fixture/GetCoreDevice.yaml"

requestGetComponentVersionArtifact :: GetComponentVersionArtifact -> TestTree
requestGetComponentVersionArtifact =
  req
    "GetComponentVersionArtifact"
    "fixture/GetComponentVersionArtifact.yaml"

requestCreateDeployment :: CreateDeployment -> TestTree
requestCreateDeployment =
  req
    "CreateDeployment"
    "fixture/CreateDeployment.yaml"

requestBatchAssociateClientDeviceWithCoreDevice :: BatchAssociateClientDeviceWithCoreDevice -> TestTree
requestBatchAssociateClientDeviceWithCoreDevice =
  req
    "BatchAssociateClientDeviceWithCoreDevice"
    "fixture/BatchAssociateClientDeviceWithCoreDevice.yaml"

requestListInstalledComponents :: ListInstalledComponents -> TestTree
requestListInstalledComponents =
  req
    "ListInstalledComponents"
    "fixture/ListInstalledComponents.yaml"

requestCancelDeployment :: CancelDeployment -> TestTree
requestCancelDeployment =
  req
    "CancelDeployment"
    "fixture/CancelDeployment.yaml"

requestBatchDisassociateClientDeviceFromCoreDevice :: BatchDisassociateClientDeviceFromCoreDevice -> TestTree
requestBatchDisassociateClientDeviceFromCoreDevice =
  req
    "BatchDisassociateClientDeviceFromCoreDevice"
    "fixture/BatchDisassociateClientDeviceFromCoreDevice.yaml"

requestListCoreDevices :: ListCoreDevices -> TestTree
requestListCoreDevices =
  req
    "ListCoreDevices"
    "fixture/ListCoreDevices.yaml"

requestResolveComponentCandidates :: ResolveComponentCandidates -> TestTree
requestResolveComponentCandidates =
  req
    "ResolveComponentCandidates"
    "fixture/ResolveComponentCandidates.yaml"

requestListEffectiveDeployments :: ListEffectiveDeployments -> TestTree
requestListEffectiveDeployments =
  req
    "ListEffectiveDeployments"
    "fixture/ListEffectiveDeployments.yaml"

requestListDeployments :: ListDeployments -> TestTree
requestListDeployments =
  req
    "ListDeployments"
    "fixture/ListDeployments.yaml"

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

requestListComponents :: ListComponents -> TestTree
requestListComponents =
  req
    "ListComponents"
    "fixture/ListComponents.yaml"

requestDeleteComponent :: DeleteComponent -> TestTree
requestDeleteComponent =
  req
    "DeleteComponent"
    "fixture/DeleteComponent.yaml"

requestCreateComponentVersion :: CreateComponentVersion -> TestTree
requestCreateComponentVersion =
  req
    "CreateComponentVersion"
    "fixture/CreateComponentVersion.yaml"

requestGetComponent :: GetComponent -> TestTree
requestGetComponent =
  req
    "GetComponent"
    "fixture/GetComponent.yaml"

requestListClientDevicesAssociatedWithCoreDevice :: ListClientDevicesAssociatedWithCoreDevice -> TestTree
requestListClientDevicesAssociatedWithCoreDevice =
  req
    "ListClientDevicesAssociatedWithCoreDevice"
    "fixture/ListClientDevicesAssociatedWithCoreDevice.yaml"

-- Responses

responseListComponentVersions :: ListComponentVersionsResponse -> TestTree
responseListComponentVersions =
  res
    "ListComponentVersionsResponse"
    "fixture/ListComponentVersionsResponse.proto"
    defaultService
    (Proxy :: Proxy ListComponentVersions)

responseGetDeployment :: GetDeploymentResponse -> TestTree
responseGetDeployment =
  res
    "GetDeploymentResponse"
    "fixture/GetDeploymentResponse.proto"
    defaultService
    (Proxy :: Proxy GetDeployment)

responseDescribeComponent :: DescribeComponentResponse -> TestTree
responseDescribeComponent =
  res
    "DescribeComponentResponse"
    "fixture/DescribeComponentResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeComponent)

responseDeleteCoreDevice :: DeleteCoreDeviceResponse -> TestTree
responseDeleteCoreDevice =
  res
    "DeleteCoreDeviceResponse"
    "fixture/DeleteCoreDeviceResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteCoreDevice)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy :: Proxy ListTagsForResource)

responseGetCoreDevice :: GetCoreDeviceResponse -> TestTree
responseGetCoreDevice =
  res
    "GetCoreDeviceResponse"
    "fixture/GetCoreDeviceResponse.proto"
    defaultService
    (Proxy :: Proxy GetCoreDevice)

responseGetComponentVersionArtifact :: GetComponentVersionArtifactResponse -> TestTree
responseGetComponentVersionArtifact =
  res
    "GetComponentVersionArtifactResponse"
    "fixture/GetComponentVersionArtifactResponse.proto"
    defaultService
    (Proxy :: Proxy GetComponentVersionArtifact)

responseCreateDeployment :: CreateDeploymentResponse -> TestTree
responseCreateDeployment =
  res
    "CreateDeploymentResponse"
    "fixture/CreateDeploymentResponse.proto"
    defaultService
    (Proxy :: Proxy CreateDeployment)

responseBatchAssociateClientDeviceWithCoreDevice :: BatchAssociateClientDeviceWithCoreDeviceResponse -> TestTree
responseBatchAssociateClientDeviceWithCoreDevice =
  res
    "BatchAssociateClientDeviceWithCoreDeviceResponse"
    "fixture/BatchAssociateClientDeviceWithCoreDeviceResponse.proto"
    defaultService
    (Proxy :: Proxy BatchAssociateClientDeviceWithCoreDevice)

responseListInstalledComponents :: ListInstalledComponentsResponse -> TestTree
responseListInstalledComponents =
  res
    "ListInstalledComponentsResponse"
    "fixture/ListInstalledComponentsResponse.proto"
    defaultService
    (Proxy :: Proxy ListInstalledComponents)

responseCancelDeployment :: CancelDeploymentResponse -> TestTree
responseCancelDeployment =
  res
    "CancelDeploymentResponse"
    "fixture/CancelDeploymentResponse.proto"
    defaultService
    (Proxy :: Proxy CancelDeployment)

responseBatchDisassociateClientDeviceFromCoreDevice :: BatchDisassociateClientDeviceFromCoreDeviceResponse -> TestTree
responseBatchDisassociateClientDeviceFromCoreDevice =
  res
    "BatchDisassociateClientDeviceFromCoreDeviceResponse"
    "fixture/BatchDisassociateClientDeviceFromCoreDeviceResponse.proto"
    defaultService
    (Proxy :: Proxy BatchDisassociateClientDeviceFromCoreDevice)

responseListCoreDevices :: ListCoreDevicesResponse -> TestTree
responseListCoreDevices =
  res
    "ListCoreDevicesResponse"
    "fixture/ListCoreDevicesResponse.proto"
    defaultService
    (Proxy :: Proxy ListCoreDevices)

responseResolveComponentCandidates :: ResolveComponentCandidatesResponse -> TestTree
responseResolveComponentCandidates =
  res
    "ResolveComponentCandidatesResponse"
    "fixture/ResolveComponentCandidatesResponse.proto"
    defaultService
    (Proxy :: Proxy ResolveComponentCandidates)

responseListEffectiveDeployments :: ListEffectiveDeploymentsResponse -> TestTree
responseListEffectiveDeployments =
  res
    "ListEffectiveDeploymentsResponse"
    "fixture/ListEffectiveDeploymentsResponse.proto"
    defaultService
    (Proxy :: Proxy ListEffectiveDeployments)

responseListDeployments :: ListDeploymentsResponse -> TestTree
responseListDeployments =
  res
    "ListDeploymentsResponse"
    "fixture/ListDeploymentsResponse.proto"
    defaultService
    (Proxy :: Proxy ListDeployments)

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

responseListComponents :: ListComponentsResponse -> TestTree
responseListComponents =
  res
    "ListComponentsResponse"
    "fixture/ListComponentsResponse.proto"
    defaultService
    (Proxy :: Proxy ListComponents)

responseDeleteComponent :: DeleteComponentResponse -> TestTree
responseDeleteComponent =
  res
    "DeleteComponentResponse"
    "fixture/DeleteComponentResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteComponent)

responseCreateComponentVersion :: CreateComponentVersionResponse -> TestTree
responseCreateComponentVersion =
  res
    "CreateComponentVersionResponse"
    "fixture/CreateComponentVersionResponse.proto"
    defaultService
    (Proxy :: Proxy CreateComponentVersion)

responseGetComponent :: GetComponentResponse -> TestTree
responseGetComponent =
  res
    "GetComponentResponse"
    "fixture/GetComponentResponse.proto"
    defaultService
    (Proxy :: Proxy GetComponent)

responseListClientDevicesAssociatedWithCoreDevice :: ListClientDevicesAssociatedWithCoreDeviceResponse -> TestTree
responseListClientDevicesAssociatedWithCoreDevice =
  res
    "ListClientDevicesAssociatedWithCoreDeviceResponse"
    "fixture/ListClientDevicesAssociatedWithCoreDeviceResponse.proto"
    defaultService
    (Proxy :: Proxy ListClientDevicesAssociatedWithCoreDevice)
