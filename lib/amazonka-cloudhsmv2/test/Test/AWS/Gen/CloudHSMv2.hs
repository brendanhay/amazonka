{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.CloudHSMv2
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
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
--             mkDescribeClusters
--
--         , requestDeleteBackup $
--             mkDeleteBackup
--
--         , requestInitializeCluster $
--             mkInitializeCluster
--
--         , requestCreateHSM $
--             mkCreateHSM
--
--         , requestDescribeBackups $
--             mkDescribeBackups
--
--         , requestCopyBackupToRegion $
--             mkCopyBackupToRegion
--
--         , requestDeleteCluster $
--             mkDeleteCluster
--
--         , requestCreateCluster $
--             mkCreateCluster
--
--         , requestRestoreBackup $
--             mkRestoreBackup
--
--         , requestDeleteHSM $
--             mkDeleteHSM
--
--         , requestModifyCluster $
--             mkModifyCluster
--
--         , requestTagResource $
--             mkTagResource
--
--         , requestListTags $
--             mkListTags
--
--         , requestUntagResource $
--             mkUntagResource
--
--         , requestModifyBackupAttributes $
--             mkModifyBackupAttributes
--
--           ]

--     , testGroup "response"
--         [ responseDescribeClusters $
--             mkDescribeClustersResponse
--
--         , responseDeleteBackup $
--             mkDeleteBackupResponse
--
--         , responseInitializeCluster $
--             mkInitializeClusterResponse
--
--         , responseCreateHSM $
--             mkCreateHSMResponse
--
--         , responseDescribeBackups $
--             mkDescribeBackupsResponse
--
--         , responseCopyBackupToRegion $
--             mkCopyBackupToRegionResponse
--
--         , responseDeleteCluster $
--             mkDeleteClusterResponse
--
--         , responseCreateCluster $
--             mkCreateClusterResponse
--
--         , responseRestoreBackup $
--             mkRestoreBackupResponse
--
--         , responseDeleteHSM $
--             mkDeleteHSMResponse
--
--         , responseModifyCluster $
--             mkModifyClusterResponse
--
--         , responseTagResource $
--             mkTagResourceResponse
--
--         , responseListTags $
--             mkListTagsResponse
--
--         , responseUntagResource $
--             mkUntagResourceResponse
--
--         , responseModifyBackupAttributes $
--             mkModifyBackupAttributesResponse
--
--           ]
--     ]

-- Requests

requestDescribeClusters :: DescribeClusters -> TestTree
requestDescribeClusters =
  req
    "DescribeClusters"
    "fixture/DescribeClusters.yaml"

requestDeleteBackup :: DeleteBackup -> TestTree
requestDeleteBackup =
  req
    "DeleteBackup"
    "fixture/DeleteBackup.yaml"

requestInitializeCluster :: InitializeCluster -> TestTree
requestInitializeCluster =
  req
    "InitializeCluster"
    "fixture/InitializeCluster.yaml"

requestCreateHSM :: CreateHSM -> TestTree
requestCreateHSM =
  req
    "CreateHSM"
    "fixture/CreateHSM.yaml"

requestDescribeBackups :: DescribeBackups -> TestTree
requestDescribeBackups =
  req
    "DescribeBackups"
    "fixture/DescribeBackups.yaml"

requestCopyBackupToRegion :: CopyBackupToRegion -> TestTree
requestCopyBackupToRegion =
  req
    "CopyBackupToRegion"
    "fixture/CopyBackupToRegion.yaml"

requestDeleteCluster :: DeleteCluster -> TestTree
requestDeleteCluster =
  req
    "DeleteCluster"
    "fixture/DeleteCluster.yaml"

requestCreateCluster :: CreateCluster -> TestTree
requestCreateCluster =
  req
    "CreateCluster"
    "fixture/CreateCluster.yaml"

requestRestoreBackup :: RestoreBackup -> TestTree
requestRestoreBackup =
  req
    "RestoreBackup"
    "fixture/RestoreBackup.yaml"

requestDeleteHSM :: DeleteHSM -> TestTree
requestDeleteHSM =
  req
    "DeleteHSM"
    "fixture/DeleteHSM.yaml"

requestModifyCluster :: ModifyCluster -> TestTree
requestModifyCluster =
  req
    "ModifyCluster"
    "fixture/ModifyCluster.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestListTags :: ListTags -> TestTree
requestListTags =
  req
    "ListTags"
    "fixture/ListTags.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestModifyBackupAttributes :: ModifyBackupAttributes -> TestTree
requestModifyBackupAttributes =
  req
    "ModifyBackupAttributes"
    "fixture/ModifyBackupAttributes.yaml"

-- Responses

responseDescribeClusters :: DescribeClustersResponse -> TestTree
responseDescribeClusters =
  res
    "DescribeClustersResponse"
    "fixture/DescribeClustersResponse.proto"
    cloudHSMv2Service
    (Proxy :: Proxy DescribeClusters)

responseDeleteBackup :: DeleteBackupResponse -> TestTree
responseDeleteBackup =
  res
    "DeleteBackupResponse"
    "fixture/DeleteBackupResponse.proto"
    cloudHSMv2Service
    (Proxy :: Proxy DeleteBackup)

responseInitializeCluster :: InitializeClusterResponse -> TestTree
responseInitializeCluster =
  res
    "InitializeClusterResponse"
    "fixture/InitializeClusterResponse.proto"
    cloudHSMv2Service
    (Proxy :: Proxy InitializeCluster)

responseCreateHSM :: CreateHSMResponse -> TestTree
responseCreateHSM =
  res
    "CreateHSMResponse"
    "fixture/CreateHSMResponse.proto"
    cloudHSMv2Service
    (Proxy :: Proxy CreateHSM)

responseDescribeBackups :: DescribeBackupsResponse -> TestTree
responseDescribeBackups =
  res
    "DescribeBackupsResponse"
    "fixture/DescribeBackupsResponse.proto"
    cloudHSMv2Service
    (Proxy :: Proxy DescribeBackups)

responseCopyBackupToRegion :: CopyBackupToRegionResponse -> TestTree
responseCopyBackupToRegion =
  res
    "CopyBackupToRegionResponse"
    "fixture/CopyBackupToRegionResponse.proto"
    cloudHSMv2Service
    (Proxy :: Proxy CopyBackupToRegion)

responseDeleteCluster :: DeleteClusterResponse -> TestTree
responseDeleteCluster =
  res
    "DeleteClusterResponse"
    "fixture/DeleteClusterResponse.proto"
    cloudHSMv2Service
    (Proxy :: Proxy DeleteCluster)

responseCreateCluster :: CreateClusterResponse -> TestTree
responseCreateCluster =
  res
    "CreateClusterResponse"
    "fixture/CreateClusterResponse.proto"
    cloudHSMv2Service
    (Proxy :: Proxy CreateCluster)

responseRestoreBackup :: RestoreBackupResponse -> TestTree
responseRestoreBackup =
  res
    "RestoreBackupResponse"
    "fixture/RestoreBackupResponse.proto"
    cloudHSMv2Service
    (Proxy :: Proxy RestoreBackup)

responseDeleteHSM :: DeleteHSMResponse -> TestTree
responseDeleteHSM =
  res
    "DeleteHSMResponse"
    "fixture/DeleteHSMResponse.proto"
    cloudHSMv2Service
    (Proxy :: Proxy DeleteHSM)

responseModifyCluster :: ModifyClusterResponse -> TestTree
responseModifyCluster =
  res
    "ModifyClusterResponse"
    "fixture/ModifyClusterResponse.proto"
    cloudHSMv2Service
    (Proxy :: Proxy ModifyCluster)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    cloudHSMv2Service
    (Proxy :: Proxy TagResource)

responseListTags :: ListTagsResponse -> TestTree
responseListTags =
  res
    "ListTagsResponse"
    "fixture/ListTagsResponse.proto"
    cloudHSMv2Service
    (Proxy :: Proxy ListTags)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    cloudHSMv2Service
    (Proxy :: Proxy UntagResource)

responseModifyBackupAttributes :: ModifyBackupAttributesResponse -> TestTree
responseModifyBackupAttributes =
  res
    "ModifyBackupAttributesResponse"
    "fixture/ModifyBackupAttributesResponse.proto"
    cloudHSMv2Service
    (Proxy :: Proxy ModifyBackupAttributes)
