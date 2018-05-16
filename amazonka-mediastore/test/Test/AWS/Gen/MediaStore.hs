{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.MediaStore
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Test.AWS.Gen.MediaStore where

import Data.Proxy
import Network.AWS.MediaStore
import Test.AWS.Fixture
import Test.AWS.MediaStore.Internal
import Test.AWS.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestCreateContainer $
--             createContainer
--
--         , requestListContainers $
--             listContainers
--
--         , requestDeleteContainer $
--             deleteContainer
--
--         , requestPutCORSPolicy $
--             putCORSPolicy
--
--         , requestDeleteCORSPolicy $
--             deleteCORSPolicy
--
--         , requestDescribeContainer $
--             describeContainer
--
--         , requestGetCORSPolicy $
--             getCORSPolicy
--
--         , requestDeleteContainerPolicy $
--             deleteContainerPolicy
--
--         , requestPutContainerPolicy $
--             putContainerPolicy
--
--         , requestGetContainerPolicy $
--             getContainerPolicy
--
--           ]

--     , testGroup "response"
--         [ responseCreateContainer $
--             createContainerResponse
--
--         , responseListContainers $
--             listContainersResponse
--
--         , responseDeleteContainer $
--             deleteContainerResponse
--
--         , responsePutCORSPolicy $
--             putCORSPolicyResponse
--
--         , responseDeleteCORSPolicy $
--             deleteCORSPolicyResponse
--
--         , responseDescribeContainer $
--             describeContainerResponse
--
--         , responseGetCORSPolicy $
--             getCORSPolicyResponse
--
--         , responseDeleteContainerPolicy $
--             deleteContainerPolicyResponse
--
--         , responsePutContainerPolicy $
--             putContainerPolicyResponse
--
--         , responseGetContainerPolicy $
--             getContainerPolicyResponse
--
--           ]
--     ]

-- Requests

requestCreateContainer :: CreateContainer -> TestTree
requestCreateContainer = req
    "CreateContainer"
    "fixture/CreateContainer.yaml"

requestListContainers :: ListContainers -> TestTree
requestListContainers = req
    "ListContainers"
    "fixture/ListContainers.yaml"

requestDeleteContainer :: DeleteContainer -> TestTree
requestDeleteContainer = req
    "DeleteContainer"
    "fixture/DeleteContainer.yaml"

requestPutCORSPolicy :: PutCORSPolicy -> TestTree
requestPutCORSPolicy = req
    "PutCORSPolicy"
    "fixture/PutCORSPolicy.yaml"

requestDeleteCORSPolicy :: DeleteCORSPolicy -> TestTree
requestDeleteCORSPolicy = req
    "DeleteCORSPolicy"
    "fixture/DeleteCORSPolicy.yaml"

requestDescribeContainer :: DescribeContainer -> TestTree
requestDescribeContainer = req
    "DescribeContainer"
    "fixture/DescribeContainer.yaml"

requestGetCORSPolicy :: GetCORSPolicy -> TestTree
requestGetCORSPolicy = req
    "GetCORSPolicy"
    "fixture/GetCORSPolicy.yaml"

requestDeleteContainerPolicy :: DeleteContainerPolicy -> TestTree
requestDeleteContainerPolicy = req
    "DeleteContainerPolicy"
    "fixture/DeleteContainerPolicy.yaml"

requestPutContainerPolicy :: PutContainerPolicy -> TestTree
requestPutContainerPolicy = req
    "PutContainerPolicy"
    "fixture/PutContainerPolicy.yaml"

requestGetContainerPolicy :: GetContainerPolicy -> TestTree
requestGetContainerPolicy = req
    "GetContainerPolicy"
    "fixture/GetContainerPolicy.yaml"

-- Responses

responseCreateContainer :: CreateContainerResponse -> TestTree
responseCreateContainer = res
    "CreateContainerResponse"
    "fixture/CreateContainerResponse.proto"
    mediaStore
    (Proxy :: Proxy CreateContainer)

responseListContainers :: ListContainersResponse -> TestTree
responseListContainers = res
    "ListContainersResponse"
    "fixture/ListContainersResponse.proto"
    mediaStore
    (Proxy :: Proxy ListContainers)

responseDeleteContainer :: DeleteContainerResponse -> TestTree
responseDeleteContainer = res
    "DeleteContainerResponse"
    "fixture/DeleteContainerResponse.proto"
    mediaStore
    (Proxy :: Proxy DeleteContainer)

responsePutCORSPolicy :: PutCORSPolicyResponse -> TestTree
responsePutCORSPolicy = res
    "PutCORSPolicyResponse"
    "fixture/PutCORSPolicyResponse.proto"
    mediaStore
    (Proxy :: Proxy PutCORSPolicy)

responseDeleteCORSPolicy :: DeleteCORSPolicyResponse -> TestTree
responseDeleteCORSPolicy = res
    "DeleteCORSPolicyResponse"
    "fixture/DeleteCORSPolicyResponse.proto"
    mediaStore
    (Proxy :: Proxy DeleteCORSPolicy)

responseDescribeContainer :: DescribeContainerResponse -> TestTree
responseDescribeContainer = res
    "DescribeContainerResponse"
    "fixture/DescribeContainerResponse.proto"
    mediaStore
    (Proxy :: Proxy DescribeContainer)

responseGetCORSPolicy :: GetCORSPolicyResponse -> TestTree
responseGetCORSPolicy = res
    "GetCORSPolicyResponse"
    "fixture/GetCORSPolicyResponse.proto"
    mediaStore
    (Proxy :: Proxy GetCORSPolicy)

responseDeleteContainerPolicy :: DeleteContainerPolicyResponse -> TestTree
responseDeleteContainerPolicy = res
    "DeleteContainerPolicyResponse"
    "fixture/DeleteContainerPolicyResponse.proto"
    mediaStore
    (Proxy :: Proxy DeleteContainerPolicy)

responsePutContainerPolicy :: PutContainerPolicyResponse -> TestTree
responsePutContainerPolicy = res
    "PutContainerPolicyResponse"
    "fixture/PutContainerPolicyResponse.proto"
    mediaStore
    (Proxy :: Proxy PutContainerPolicy)

responseGetContainerPolicy :: GetContainerPolicyResponse -> TestTree
responseGetContainerPolicy = res
    "GetContainerPolicyResponse"
    "fixture/GetContainerPolicyResponse.proto"
    mediaStore
    (Proxy :: Proxy GetContainerPolicy)
