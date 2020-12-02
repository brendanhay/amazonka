{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.CloudHSMv2
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Test.AWS.Gen.CloudHSMv2 where

import Data.Proxy
import Network.AWS.CloudHSMv2
import Test.AWS.CloudHSMv2.Internal
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
--         [ requestDescribeClusters $
--             describeClusters
--
--         , requestInitializeCluster $
--             initializeCluster
--
--         , requestCreateHSM $
--             createHSM
--
--         , requestDescribeBackups $
--             describeBackups
--
--         , requestDeleteCluster $
--             deleteCluster
--
--         , requestCreateCluster $
--             createCluster
--
--         , requestDeleteHSM $
--             deleteHSM
--
--         , requestTagResource $
--             tagResource
--
--         , requestListTags $
--             listTags
--
--         , requestUntagResource $
--             untagResource
--
--           ]

--     , testGroup "response"
--         [ responseDescribeClusters $
--             describeClustersResponse
--
--         , responseInitializeCluster $
--             initializeClusterResponse
--
--         , responseCreateHSM $
--             createHSMResponse
--
--         , responseDescribeBackups $
--             describeBackupsResponse
--
--         , responseDeleteCluster $
--             deleteClusterResponse
--
--         , responseCreateCluster $
--             createClusterResponse
--
--         , responseDeleteHSM $
--             deleteHSMResponse
--
--         , responseTagResource $
--             tagResourceResponse
--
--         , responseListTags $
--             listTagsResponse
--
--         , responseUntagResource $
--             untagResourceResponse
--
--           ]
--     ]

-- Requests

requestDescribeClusters :: DescribeClusters -> TestTree
requestDescribeClusters = req
    "DescribeClusters"
    "fixture/DescribeClusters.yaml"

requestInitializeCluster :: InitializeCluster -> TestTree
requestInitializeCluster = req
    "InitializeCluster"
    "fixture/InitializeCluster.yaml"

requestCreateHSM :: CreateHSM -> TestTree
requestCreateHSM = req
    "CreateHSM"
    "fixture/CreateHSM.yaml"

requestDescribeBackups :: DescribeBackups -> TestTree
requestDescribeBackups = req
    "DescribeBackups"
    "fixture/DescribeBackups.yaml"

requestDeleteCluster :: DeleteCluster -> TestTree
requestDeleteCluster = req
    "DeleteCluster"
    "fixture/DeleteCluster.yaml"

requestCreateCluster :: CreateCluster -> TestTree
requestCreateCluster = req
    "CreateCluster"
    "fixture/CreateCluster.yaml"

requestDeleteHSM :: DeleteHSM -> TestTree
requestDeleteHSM = req
    "DeleteHSM"
    "fixture/DeleteHSM.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource = req
    "TagResource"
    "fixture/TagResource.yaml"

requestListTags :: ListTags -> TestTree
requestListTags = req
    "ListTags"
    "fixture/ListTags.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource = req
    "UntagResource"
    "fixture/UntagResource.yaml"

-- Responses

responseDescribeClusters :: DescribeClustersResponse -> TestTree
responseDescribeClusters = res
    "DescribeClustersResponse"
    "fixture/DescribeClustersResponse.proto"
    cloudHSMv2
    (Proxy :: Proxy DescribeClusters)

responseInitializeCluster :: InitializeClusterResponse -> TestTree
responseInitializeCluster = res
    "InitializeClusterResponse"
    "fixture/InitializeClusterResponse.proto"
    cloudHSMv2
    (Proxy :: Proxy InitializeCluster)

responseCreateHSM :: CreateHSMResponse -> TestTree
responseCreateHSM = res
    "CreateHSMResponse"
    "fixture/CreateHSMResponse.proto"
    cloudHSMv2
    (Proxy :: Proxy CreateHSM)

responseDescribeBackups :: DescribeBackupsResponse -> TestTree
responseDescribeBackups = res
    "DescribeBackupsResponse"
    "fixture/DescribeBackupsResponse.proto"
    cloudHSMv2
    (Proxy :: Proxy DescribeBackups)

responseDeleteCluster :: DeleteClusterResponse -> TestTree
responseDeleteCluster = res
    "DeleteClusterResponse"
    "fixture/DeleteClusterResponse.proto"
    cloudHSMv2
    (Proxy :: Proxy DeleteCluster)

responseCreateCluster :: CreateClusterResponse -> TestTree
responseCreateCluster = res
    "CreateClusterResponse"
    "fixture/CreateClusterResponse.proto"
    cloudHSMv2
    (Proxy :: Proxy CreateCluster)

responseDeleteHSM :: DeleteHSMResponse -> TestTree
responseDeleteHSM = res
    "DeleteHSMResponse"
    "fixture/DeleteHSMResponse.proto"
    cloudHSMv2
    (Proxy :: Proxy DeleteHSM)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource = res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    cloudHSMv2
    (Proxy :: Proxy TagResource)

responseListTags :: ListTagsResponse -> TestTree
responseListTags = res
    "ListTagsResponse"
    "fixture/ListTagsResponse.proto"
    cloudHSMv2
    (Proxy :: Proxy ListTags)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource = res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    cloudHSMv2
    (Proxy :: Proxy UntagResource)
