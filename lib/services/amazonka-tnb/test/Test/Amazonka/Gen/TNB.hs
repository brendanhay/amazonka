{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.TNB
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.TNB where

import Amazonka.TNB
import qualified Data.Proxy as Proxy
import Test.Amazonka.Fixture
import Test.Amazonka.Prelude
import Test.Amazonka.TNB.Internal
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestCancelSolNetworkOperation $
--             newCancelSolNetworkOperation
--
--         , requestCreateSolFunctionPackage $
--             newCreateSolFunctionPackage
--
--         , requestCreateSolNetworkInstance $
--             newCreateSolNetworkInstance
--
--         , requestCreateSolNetworkPackage $
--             newCreateSolNetworkPackage
--
--         , requestDeleteSolFunctionPackage $
--             newDeleteSolFunctionPackage
--
--         , requestDeleteSolNetworkInstance $
--             newDeleteSolNetworkInstance
--
--         , requestDeleteSolNetworkPackage $
--             newDeleteSolNetworkPackage
--
--         , requestGetSolFunctionInstance $
--             newGetSolFunctionInstance
--
--         , requestGetSolFunctionPackage $
--             newGetSolFunctionPackage
--
--         , requestGetSolFunctionPackageContent $
--             newGetSolFunctionPackageContent
--
--         , requestGetSolFunctionPackageDescriptor $
--             newGetSolFunctionPackageDescriptor
--
--         , requestGetSolNetworkInstance $
--             newGetSolNetworkInstance
--
--         , requestGetSolNetworkOperation $
--             newGetSolNetworkOperation
--
--         , requestGetSolNetworkPackage $
--             newGetSolNetworkPackage
--
--         , requestGetSolNetworkPackageContent $
--             newGetSolNetworkPackageContent
--
--         , requestGetSolNetworkPackageDescriptor $
--             newGetSolNetworkPackageDescriptor
--
--         , requestInstantiateSolNetworkInstance $
--             newInstantiateSolNetworkInstance
--
--         , requestListSolFunctionInstances $
--             newListSolFunctionInstances
--
--         , requestListSolFunctionPackages $
--             newListSolFunctionPackages
--
--         , requestListSolNetworkInstances $
--             newListSolNetworkInstances
--
--         , requestListSolNetworkOperations $
--             newListSolNetworkOperations
--
--         , requestListSolNetworkPackages $
--             newListSolNetworkPackages
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestPutSolFunctionPackageContent $
--             newPutSolFunctionPackageContent
--
--         , requestPutSolNetworkPackageContent $
--             newPutSolNetworkPackageContent
--
--         , requestTagResource $
--             newTagResource
--
--         , requestTerminateSolNetworkInstance $
--             newTerminateSolNetworkInstance
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestUpdateSolFunctionPackage $
--             newUpdateSolFunctionPackage
--
--         , requestUpdateSolNetworkInstance $
--             newUpdateSolNetworkInstance
--
--         , requestUpdateSolNetworkPackage $
--             newUpdateSolNetworkPackage
--
--         , requestValidateSolFunctionPackageContent $
--             newValidateSolFunctionPackageContent
--
--         , requestValidateSolNetworkPackageContent $
--             newValidateSolNetworkPackageContent
--
--           ]

--     , testGroup "response"
--         [ responseCancelSolNetworkOperation $
--             newCancelSolNetworkOperationResponse
--
--         , responseCreateSolFunctionPackage $
--             newCreateSolFunctionPackageResponse
--
--         , responseCreateSolNetworkInstance $
--             newCreateSolNetworkInstanceResponse
--
--         , responseCreateSolNetworkPackage $
--             newCreateSolNetworkPackageResponse
--
--         , responseDeleteSolFunctionPackage $
--             newDeleteSolFunctionPackageResponse
--
--         , responseDeleteSolNetworkInstance $
--             newDeleteSolNetworkInstanceResponse
--
--         , responseDeleteSolNetworkPackage $
--             newDeleteSolNetworkPackageResponse
--
--         , responseGetSolFunctionInstance $
--             newGetSolFunctionInstanceResponse
--
--         , responseGetSolFunctionPackage $
--             newGetSolFunctionPackageResponse
--
--         , responseGetSolFunctionPackageContent $
--             newGetSolFunctionPackageContentResponse
--
--         , responseGetSolFunctionPackageDescriptor $
--             newGetSolFunctionPackageDescriptorResponse
--
--         , responseGetSolNetworkInstance $
--             newGetSolNetworkInstanceResponse
--
--         , responseGetSolNetworkOperation $
--             newGetSolNetworkOperationResponse
--
--         , responseGetSolNetworkPackage $
--             newGetSolNetworkPackageResponse
--
--         , responseGetSolNetworkPackageContent $
--             newGetSolNetworkPackageContentResponse
--
--         , responseGetSolNetworkPackageDescriptor $
--             newGetSolNetworkPackageDescriptorResponse
--
--         , responseInstantiateSolNetworkInstance $
--             newInstantiateSolNetworkInstanceResponse
--
--         , responseListSolFunctionInstances $
--             newListSolFunctionInstancesResponse
--
--         , responseListSolFunctionPackages $
--             newListSolFunctionPackagesResponse
--
--         , responseListSolNetworkInstances $
--             newListSolNetworkInstancesResponse
--
--         , responseListSolNetworkOperations $
--             newListSolNetworkOperationsResponse
--
--         , responseListSolNetworkPackages $
--             newListSolNetworkPackagesResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responsePutSolFunctionPackageContent $
--             newPutSolFunctionPackageContentResponse
--
--         , responsePutSolNetworkPackageContent $
--             newPutSolNetworkPackageContentResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseTerminateSolNetworkInstance $
--             newTerminateSolNetworkInstanceResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseUpdateSolFunctionPackage $
--             newUpdateSolFunctionPackageResponse
--
--         , responseUpdateSolNetworkInstance $
--             newUpdateSolNetworkInstanceResponse
--
--         , responseUpdateSolNetworkPackage $
--             newUpdateSolNetworkPackageResponse
--
--         , responseValidateSolFunctionPackageContent $
--             newValidateSolFunctionPackageContentResponse
--
--         , responseValidateSolNetworkPackageContent $
--             newValidateSolNetworkPackageContentResponse
--
--           ]
--     ]

-- Requests

requestCancelSolNetworkOperation :: CancelSolNetworkOperation -> TestTree
requestCancelSolNetworkOperation =
  req
    "CancelSolNetworkOperation"
    "fixture/CancelSolNetworkOperation.yaml"

requestCreateSolFunctionPackage :: CreateSolFunctionPackage -> TestTree
requestCreateSolFunctionPackage =
  req
    "CreateSolFunctionPackage"
    "fixture/CreateSolFunctionPackage.yaml"

requestCreateSolNetworkInstance :: CreateSolNetworkInstance -> TestTree
requestCreateSolNetworkInstance =
  req
    "CreateSolNetworkInstance"
    "fixture/CreateSolNetworkInstance.yaml"

requestCreateSolNetworkPackage :: CreateSolNetworkPackage -> TestTree
requestCreateSolNetworkPackage =
  req
    "CreateSolNetworkPackage"
    "fixture/CreateSolNetworkPackage.yaml"

requestDeleteSolFunctionPackage :: DeleteSolFunctionPackage -> TestTree
requestDeleteSolFunctionPackage =
  req
    "DeleteSolFunctionPackage"
    "fixture/DeleteSolFunctionPackage.yaml"

requestDeleteSolNetworkInstance :: DeleteSolNetworkInstance -> TestTree
requestDeleteSolNetworkInstance =
  req
    "DeleteSolNetworkInstance"
    "fixture/DeleteSolNetworkInstance.yaml"

requestDeleteSolNetworkPackage :: DeleteSolNetworkPackage -> TestTree
requestDeleteSolNetworkPackage =
  req
    "DeleteSolNetworkPackage"
    "fixture/DeleteSolNetworkPackage.yaml"

requestGetSolFunctionInstance :: GetSolFunctionInstance -> TestTree
requestGetSolFunctionInstance =
  req
    "GetSolFunctionInstance"
    "fixture/GetSolFunctionInstance.yaml"

requestGetSolFunctionPackage :: GetSolFunctionPackage -> TestTree
requestGetSolFunctionPackage =
  req
    "GetSolFunctionPackage"
    "fixture/GetSolFunctionPackage.yaml"

requestGetSolFunctionPackageContent :: GetSolFunctionPackageContent -> TestTree
requestGetSolFunctionPackageContent =
  req
    "GetSolFunctionPackageContent"
    "fixture/GetSolFunctionPackageContent.yaml"

requestGetSolFunctionPackageDescriptor :: GetSolFunctionPackageDescriptor -> TestTree
requestGetSolFunctionPackageDescriptor =
  req
    "GetSolFunctionPackageDescriptor"
    "fixture/GetSolFunctionPackageDescriptor.yaml"

requestGetSolNetworkInstance :: GetSolNetworkInstance -> TestTree
requestGetSolNetworkInstance =
  req
    "GetSolNetworkInstance"
    "fixture/GetSolNetworkInstance.yaml"

requestGetSolNetworkOperation :: GetSolNetworkOperation -> TestTree
requestGetSolNetworkOperation =
  req
    "GetSolNetworkOperation"
    "fixture/GetSolNetworkOperation.yaml"

requestGetSolNetworkPackage :: GetSolNetworkPackage -> TestTree
requestGetSolNetworkPackage =
  req
    "GetSolNetworkPackage"
    "fixture/GetSolNetworkPackage.yaml"

requestGetSolNetworkPackageContent :: GetSolNetworkPackageContent -> TestTree
requestGetSolNetworkPackageContent =
  req
    "GetSolNetworkPackageContent"
    "fixture/GetSolNetworkPackageContent.yaml"

requestGetSolNetworkPackageDescriptor :: GetSolNetworkPackageDescriptor -> TestTree
requestGetSolNetworkPackageDescriptor =
  req
    "GetSolNetworkPackageDescriptor"
    "fixture/GetSolNetworkPackageDescriptor.yaml"

requestInstantiateSolNetworkInstance :: InstantiateSolNetworkInstance -> TestTree
requestInstantiateSolNetworkInstance =
  req
    "InstantiateSolNetworkInstance"
    "fixture/InstantiateSolNetworkInstance.yaml"

requestListSolFunctionInstances :: ListSolFunctionInstances -> TestTree
requestListSolFunctionInstances =
  req
    "ListSolFunctionInstances"
    "fixture/ListSolFunctionInstances.yaml"

requestListSolFunctionPackages :: ListSolFunctionPackages -> TestTree
requestListSolFunctionPackages =
  req
    "ListSolFunctionPackages"
    "fixture/ListSolFunctionPackages.yaml"

requestListSolNetworkInstances :: ListSolNetworkInstances -> TestTree
requestListSolNetworkInstances =
  req
    "ListSolNetworkInstances"
    "fixture/ListSolNetworkInstances.yaml"

requestListSolNetworkOperations :: ListSolNetworkOperations -> TestTree
requestListSolNetworkOperations =
  req
    "ListSolNetworkOperations"
    "fixture/ListSolNetworkOperations.yaml"

requestListSolNetworkPackages :: ListSolNetworkPackages -> TestTree
requestListSolNetworkPackages =
  req
    "ListSolNetworkPackages"
    "fixture/ListSolNetworkPackages.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestPutSolFunctionPackageContent :: PutSolFunctionPackageContent -> TestTree
requestPutSolFunctionPackageContent =
  req
    "PutSolFunctionPackageContent"
    "fixture/PutSolFunctionPackageContent.yaml"

requestPutSolNetworkPackageContent :: PutSolNetworkPackageContent -> TestTree
requestPutSolNetworkPackageContent =
  req
    "PutSolNetworkPackageContent"
    "fixture/PutSolNetworkPackageContent.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestTerminateSolNetworkInstance :: TerminateSolNetworkInstance -> TestTree
requestTerminateSolNetworkInstance =
  req
    "TerminateSolNetworkInstance"
    "fixture/TerminateSolNetworkInstance.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestUpdateSolFunctionPackage :: UpdateSolFunctionPackage -> TestTree
requestUpdateSolFunctionPackage =
  req
    "UpdateSolFunctionPackage"
    "fixture/UpdateSolFunctionPackage.yaml"

requestUpdateSolNetworkInstance :: UpdateSolNetworkInstance -> TestTree
requestUpdateSolNetworkInstance =
  req
    "UpdateSolNetworkInstance"
    "fixture/UpdateSolNetworkInstance.yaml"

requestUpdateSolNetworkPackage :: UpdateSolNetworkPackage -> TestTree
requestUpdateSolNetworkPackage =
  req
    "UpdateSolNetworkPackage"
    "fixture/UpdateSolNetworkPackage.yaml"

requestValidateSolFunctionPackageContent :: ValidateSolFunctionPackageContent -> TestTree
requestValidateSolFunctionPackageContent =
  req
    "ValidateSolFunctionPackageContent"
    "fixture/ValidateSolFunctionPackageContent.yaml"

requestValidateSolNetworkPackageContent :: ValidateSolNetworkPackageContent -> TestTree
requestValidateSolNetworkPackageContent =
  req
    "ValidateSolNetworkPackageContent"
    "fixture/ValidateSolNetworkPackageContent.yaml"

-- Responses

responseCancelSolNetworkOperation :: CancelSolNetworkOperationResponse -> TestTree
responseCancelSolNetworkOperation =
  res
    "CancelSolNetworkOperationResponse"
    "fixture/CancelSolNetworkOperationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CancelSolNetworkOperation)

responseCreateSolFunctionPackage :: CreateSolFunctionPackageResponse -> TestTree
responseCreateSolFunctionPackage =
  res
    "CreateSolFunctionPackageResponse"
    "fixture/CreateSolFunctionPackageResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateSolFunctionPackage)

responseCreateSolNetworkInstance :: CreateSolNetworkInstanceResponse -> TestTree
responseCreateSolNetworkInstance =
  res
    "CreateSolNetworkInstanceResponse"
    "fixture/CreateSolNetworkInstanceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateSolNetworkInstance)

responseCreateSolNetworkPackage :: CreateSolNetworkPackageResponse -> TestTree
responseCreateSolNetworkPackage =
  res
    "CreateSolNetworkPackageResponse"
    "fixture/CreateSolNetworkPackageResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateSolNetworkPackage)

responseDeleteSolFunctionPackage :: DeleteSolFunctionPackageResponse -> TestTree
responseDeleteSolFunctionPackage =
  res
    "DeleteSolFunctionPackageResponse"
    "fixture/DeleteSolFunctionPackageResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteSolFunctionPackage)

responseDeleteSolNetworkInstance :: DeleteSolNetworkInstanceResponse -> TestTree
responseDeleteSolNetworkInstance =
  res
    "DeleteSolNetworkInstanceResponse"
    "fixture/DeleteSolNetworkInstanceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteSolNetworkInstance)

responseDeleteSolNetworkPackage :: DeleteSolNetworkPackageResponse -> TestTree
responseDeleteSolNetworkPackage =
  res
    "DeleteSolNetworkPackageResponse"
    "fixture/DeleteSolNetworkPackageResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteSolNetworkPackage)

responseGetSolFunctionInstance :: GetSolFunctionInstanceResponse -> TestTree
responseGetSolFunctionInstance =
  res
    "GetSolFunctionInstanceResponse"
    "fixture/GetSolFunctionInstanceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetSolFunctionInstance)

responseGetSolFunctionPackage :: GetSolFunctionPackageResponse -> TestTree
responseGetSolFunctionPackage =
  res
    "GetSolFunctionPackageResponse"
    "fixture/GetSolFunctionPackageResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetSolFunctionPackage)

responseGetSolFunctionPackageContent :: GetSolFunctionPackageContentResponse -> TestTree
responseGetSolFunctionPackageContent =
  res
    "GetSolFunctionPackageContentResponse"
    "fixture/GetSolFunctionPackageContentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetSolFunctionPackageContent)

responseGetSolFunctionPackageDescriptor :: GetSolFunctionPackageDescriptorResponse -> TestTree
responseGetSolFunctionPackageDescriptor =
  res
    "GetSolFunctionPackageDescriptorResponse"
    "fixture/GetSolFunctionPackageDescriptorResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetSolFunctionPackageDescriptor)

responseGetSolNetworkInstance :: GetSolNetworkInstanceResponse -> TestTree
responseGetSolNetworkInstance =
  res
    "GetSolNetworkInstanceResponse"
    "fixture/GetSolNetworkInstanceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetSolNetworkInstance)

responseGetSolNetworkOperation :: GetSolNetworkOperationResponse -> TestTree
responseGetSolNetworkOperation =
  res
    "GetSolNetworkOperationResponse"
    "fixture/GetSolNetworkOperationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetSolNetworkOperation)

responseGetSolNetworkPackage :: GetSolNetworkPackageResponse -> TestTree
responseGetSolNetworkPackage =
  res
    "GetSolNetworkPackageResponse"
    "fixture/GetSolNetworkPackageResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetSolNetworkPackage)

responseGetSolNetworkPackageContent :: GetSolNetworkPackageContentResponse -> TestTree
responseGetSolNetworkPackageContent =
  res
    "GetSolNetworkPackageContentResponse"
    "fixture/GetSolNetworkPackageContentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetSolNetworkPackageContent)

responseGetSolNetworkPackageDescriptor :: GetSolNetworkPackageDescriptorResponse -> TestTree
responseGetSolNetworkPackageDescriptor =
  res
    "GetSolNetworkPackageDescriptorResponse"
    "fixture/GetSolNetworkPackageDescriptorResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetSolNetworkPackageDescriptor)

responseInstantiateSolNetworkInstance :: InstantiateSolNetworkInstanceResponse -> TestTree
responseInstantiateSolNetworkInstance =
  res
    "InstantiateSolNetworkInstanceResponse"
    "fixture/InstantiateSolNetworkInstanceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy InstantiateSolNetworkInstance)

responseListSolFunctionInstances :: ListSolFunctionInstancesResponse -> TestTree
responseListSolFunctionInstances =
  res
    "ListSolFunctionInstancesResponse"
    "fixture/ListSolFunctionInstancesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListSolFunctionInstances)

responseListSolFunctionPackages :: ListSolFunctionPackagesResponse -> TestTree
responseListSolFunctionPackages =
  res
    "ListSolFunctionPackagesResponse"
    "fixture/ListSolFunctionPackagesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListSolFunctionPackages)

responseListSolNetworkInstances :: ListSolNetworkInstancesResponse -> TestTree
responseListSolNetworkInstances =
  res
    "ListSolNetworkInstancesResponse"
    "fixture/ListSolNetworkInstancesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListSolNetworkInstances)

responseListSolNetworkOperations :: ListSolNetworkOperationsResponse -> TestTree
responseListSolNetworkOperations =
  res
    "ListSolNetworkOperationsResponse"
    "fixture/ListSolNetworkOperationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListSolNetworkOperations)

responseListSolNetworkPackages :: ListSolNetworkPackagesResponse -> TestTree
responseListSolNetworkPackages =
  res
    "ListSolNetworkPackagesResponse"
    "fixture/ListSolNetworkPackagesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListSolNetworkPackages)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responsePutSolFunctionPackageContent :: PutSolFunctionPackageContentResponse -> TestTree
responsePutSolFunctionPackageContent =
  res
    "PutSolFunctionPackageContentResponse"
    "fixture/PutSolFunctionPackageContentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutSolFunctionPackageContent)

responsePutSolNetworkPackageContent :: PutSolNetworkPackageContentResponse -> TestTree
responsePutSolNetworkPackageContent =
  res
    "PutSolNetworkPackageContentResponse"
    "fixture/PutSolNetworkPackageContentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutSolNetworkPackageContent)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TagResource)

responseTerminateSolNetworkInstance :: TerminateSolNetworkInstanceResponse -> TestTree
responseTerminateSolNetworkInstance =
  res
    "TerminateSolNetworkInstanceResponse"
    "fixture/TerminateSolNetworkInstanceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TerminateSolNetworkInstance)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UntagResource)

responseUpdateSolFunctionPackage :: UpdateSolFunctionPackageResponse -> TestTree
responseUpdateSolFunctionPackage =
  res
    "UpdateSolFunctionPackageResponse"
    "fixture/UpdateSolFunctionPackageResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateSolFunctionPackage)

responseUpdateSolNetworkInstance :: UpdateSolNetworkInstanceResponse -> TestTree
responseUpdateSolNetworkInstance =
  res
    "UpdateSolNetworkInstanceResponse"
    "fixture/UpdateSolNetworkInstanceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateSolNetworkInstance)

responseUpdateSolNetworkPackage :: UpdateSolNetworkPackageResponse -> TestTree
responseUpdateSolNetworkPackage =
  res
    "UpdateSolNetworkPackageResponse"
    "fixture/UpdateSolNetworkPackageResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateSolNetworkPackage)

responseValidateSolFunctionPackageContent :: ValidateSolFunctionPackageContentResponse -> TestTree
responseValidateSolFunctionPackageContent =
  res
    "ValidateSolFunctionPackageContentResponse"
    "fixture/ValidateSolFunctionPackageContentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ValidateSolFunctionPackageContent)

responseValidateSolNetworkPackageContent :: ValidateSolNetworkPackageContentResponse -> TestTree
responseValidateSolNetworkPackageContent =
  res
    "ValidateSolNetworkPackageContentResponse"
    "fixture/ValidateSolNetworkPackageContentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ValidateSolNetworkPackageContent)
