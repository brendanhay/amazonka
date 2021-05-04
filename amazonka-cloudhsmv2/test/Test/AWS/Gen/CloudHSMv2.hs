{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.CloudHSMv2
-- Copyright   : (c) 2013-2021 Brendan Hay
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
--         [ requestDeleteHsm $
--             newDeleteHsm
--
--         , requestDeleteBackup $
--             newDeleteBackup
--
--         , requestDescribeClusters $
--             newDescribeClusters
--
--         , requestRestoreBackup $
--             newRestoreBackup
--
--         , requestCreateCluster $
--             newCreateCluster
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestCopyBackupToRegion $
--             newCopyBackupToRegion
--
--         , requestTagResource $
--             newTagResource
--
--         , requestModifyCluster $
--             newModifyCluster
--
--         , requestModifyBackupAttributes $
--             newModifyBackupAttributes
--
--         , requestDeleteCluster $
--             newDeleteCluster
--
--         , requestListTags $
--             newListTags
--
--         , requestDescribeBackups $
--             newDescribeBackups
--
--         , requestCreateHsm $
--             newCreateHsm
--
--         , requestInitializeCluster $
--             newInitializeCluster
--
--           ]

--     , testGroup "response"
--         [ responseDeleteHsm $
--             newDeleteHsmResponse
--
--         , responseDeleteBackup $
--             newDeleteBackupResponse
--
--         , responseDescribeClusters $
--             newDescribeClustersResponse
--
--         , responseRestoreBackup $
--             newRestoreBackupResponse
--
--         , responseCreateCluster $
--             newCreateClusterResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseCopyBackupToRegion $
--             newCopyBackupToRegionResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseModifyCluster $
--             newModifyClusterResponse
--
--         , responseModifyBackupAttributes $
--             newModifyBackupAttributesResponse
--
--         , responseDeleteCluster $
--             newDeleteClusterResponse
--
--         , responseListTags $
--             newListTagsResponse
--
--         , responseDescribeBackups $
--             newDescribeBackupsResponse
--
--         , responseCreateHsm $
--             newCreateHsmResponse
--
--         , responseInitializeCluster $
--             newInitializeClusterResponse
--
--           ]
--     ]

-- Requests

requestDeleteHsm :: DeleteHsm -> TestTree
requestDeleteHsm =
  req
    "DeleteHsm"
    "fixture/DeleteHsm.yaml"

requestDeleteBackup :: DeleteBackup -> TestTree
requestDeleteBackup =
  req
    "DeleteBackup"
    "fixture/DeleteBackup.yaml"

requestDescribeClusters :: DescribeClusters -> TestTree
requestDescribeClusters =
  req
    "DescribeClusters"
    "fixture/DescribeClusters.yaml"

requestRestoreBackup :: RestoreBackup -> TestTree
requestRestoreBackup =
  req
    "RestoreBackup"
    "fixture/RestoreBackup.yaml"

requestCreateCluster :: CreateCluster -> TestTree
requestCreateCluster =
  req
    "CreateCluster"
    "fixture/CreateCluster.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestCopyBackupToRegion :: CopyBackupToRegion -> TestTree
requestCopyBackupToRegion =
  req
    "CopyBackupToRegion"
    "fixture/CopyBackupToRegion.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestModifyCluster :: ModifyCluster -> TestTree
requestModifyCluster =
  req
    "ModifyCluster"
    "fixture/ModifyCluster.yaml"

requestModifyBackupAttributes :: ModifyBackupAttributes -> TestTree
requestModifyBackupAttributes =
  req
    "ModifyBackupAttributes"
    "fixture/ModifyBackupAttributes.yaml"

requestDeleteCluster :: DeleteCluster -> TestTree
requestDeleteCluster =
  req
    "DeleteCluster"
    "fixture/DeleteCluster.yaml"

requestListTags :: ListTags -> TestTree
requestListTags =
  req
    "ListTags"
    "fixture/ListTags.yaml"

requestDescribeBackups :: DescribeBackups -> TestTree
requestDescribeBackups =
  req
    "DescribeBackups"
    "fixture/DescribeBackups.yaml"

requestCreateHsm :: CreateHsm -> TestTree
requestCreateHsm =
  req
    "CreateHsm"
    "fixture/CreateHsm.yaml"

requestInitializeCluster :: InitializeCluster -> TestTree
requestInitializeCluster =
  req
    "InitializeCluster"
    "fixture/InitializeCluster.yaml"

-- Responses

responseDeleteHsm :: DeleteHsmResponse -> TestTree
responseDeleteHsm =
  res
    "DeleteHsmResponse"
    "fixture/DeleteHsmResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteHsm)

responseDeleteBackup :: DeleteBackupResponse -> TestTree
responseDeleteBackup =
  res
    "DeleteBackupResponse"
    "fixture/DeleteBackupResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteBackup)

responseDescribeClusters :: DescribeClustersResponse -> TestTree
responseDescribeClusters =
  res
    "DescribeClustersResponse"
    "fixture/DescribeClustersResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeClusters)

responseRestoreBackup :: RestoreBackupResponse -> TestTree
responseRestoreBackup =
  res
    "RestoreBackupResponse"
    "fixture/RestoreBackupResponse.proto"
    defaultService
    (Proxy :: Proxy RestoreBackup)

responseCreateCluster :: CreateClusterResponse -> TestTree
responseCreateCluster =
  res
    "CreateClusterResponse"
    "fixture/CreateClusterResponse.proto"
    defaultService
    (Proxy :: Proxy CreateCluster)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy :: Proxy UntagResource)

responseCopyBackupToRegion :: CopyBackupToRegionResponse -> TestTree
responseCopyBackupToRegion =
  res
    "CopyBackupToRegionResponse"
    "fixture/CopyBackupToRegionResponse.proto"
    defaultService
    (Proxy :: Proxy CopyBackupToRegion)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy :: Proxy TagResource)

responseModifyCluster :: ModifyClusterResponse -> TestTree
responseModifyCluster =
  res
    "ModifyClusterResponse"
    "fixture/ModifyClusterResponse.proto"
    defaultService
    (Proxy :: Proxy ModifyCluster)

responseModifyBackupAttributes :: ModifyBackupAttributesResponse -> TestTree
responseModifyBackupAttributes =
  res
    "ModifyBackupAttributesResponse"
    "fixture/ModifyBackupAttributesResponse.proto"
    defaultService
    (Proxy :: Proxy ModifyBackupAttributes)

responseDeleteCluster :: DeleteClusterResponse -> TestTree
responseDeleteCluster =
  res
    "DeleteClusterResponse"
    "fixture/DeleteClusterResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteCluster)

responseListTags :: ListTagsResponse -> TestTree
responseListTags =
  res
    "ListTagsResponse"
    "fixture/ListTagsResponse.proto"
    defaultService
    (Proxy :: Proxy ListTags)

responseDescribeBackups :: DescribeBackupsResponse -> TestTree
responseDescribeBackups =
  res
    "DescribeBackupsResponse"
    "fixture/DescribeBackupsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeBackups)

responseCreateHsm :: CreateHsmResponse -> TestTree
responseCreateHsm =
  res
    "CreateHsmResponse"
    "fixture/CreateHsmResponse.proto"
    defaultService
    (Proxy :: Proxy CreateHsm)

responseInitializeCluster :: InitializeClusterResponse -> TestTree
responseInitializeCluster =
  res
    "InitializeClusterResponse"
    "fixture/InitializeClusterResponse.proto"
    defaultService
    (Proxy :: Proxy InitializeCluster)
