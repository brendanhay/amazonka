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
--         , requestTagResource $
--             newTagResource
--
--         , requestCopyBackupToRegion $
--             newCopyBackupToRegion
--
--         , requestModifyCluster $
--             newModifyCluster
--
--         , requestModifyBackupAttributes $
--             newModifyBackupAttributes
--
--         , requestListTags $
--             newListTags
--
--         , requestDeleteCluster $
--             newDeleteCluster
--
--         , requestDescribeBackups $
--             newDescribeBackups
--
--         , requestInitializeCluster $
--             newInitializeCluster
--
--         , requestCreateHsm $
--             newCreateHsm
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
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseCopyBackupToRegion $
--             newCopyBackupToRegionResponse
--
--         , responseModifyCluster $
--             newModifyClusterResponse
--
--         , responseModifyBackupAttributes $
--             newModifyBackupAttributesResponse
--
--         , responseListTags $
--             newListTagsResponse
--
--         , responseDeleteCluster $
--             newDeleteClusterResponse
--
--         , responseDescribeBackups $
--             newDescribeBackupsResponse
--
--         , responseInitializeCluster $
--             newInitializeClusterResponse
--
--         , responseCreateHsm $
--             newCreateHsmResponse
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

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestCopyBackupToRegion :: CopyBackupToRegion -> TestTree
requestCopyBackupToRegion =
  req
    "CopyBackupToRegion"
    "fixture/CopyBackupToRegion.yaml"

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

requestListTags :: ListTags -> TestTree
requestListTags =
  req
    "ListTags"
    "fixture/ListTags.yaml"

requestDeleteCluster :: DeleteCluster -> TestTree
requestDeleteCluster =
  req
    "DeleteCluster"
    "fixture/DeleteCluster.yaml"

requestDescribeBackups :: DescribeBackups -> TestTree
requestDescribeBackups =
  req
    "DescribeBackups"
    "fixture/DescribeBackups.yaml"

requestInitializeCluster :: InitializeCluster -> TestTree
requestInitializeCluster =
  req
    "InitializeCluster"
    "fixture/InitializeCluster.yaml"

requestCreateHsm :: CreateHsm -> TestTree
requestCreateHsm =
  req
    "CreateHsm"
    "fixture/CreateHsm.yaml"

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

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy :: Proxy TagResource)

responseCopyBackupToRegion :: CopyBackupToRegionResponse -> TestTree
responseCopyBackupToRegion =
  res
    "CopyBackupToRegionResponse"
    "fixture/CopyBackupToRegionResponse.proto"
    defaultService
    (Proxy :: Proxy CopyBackupToRegion)

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

responseListTags :: ListTagsResponse -> TestTree
responseListTags =
  res
    "ListTagsResponse"
    "fixture/ListTagsResponse.proto"
    defaultService
    (Proxy :: Proxy ListTags)

responseDeleteCluster :: DeleteClusterResponse -> TestTree
responseDeleteCluster =
  res
    "DeleteClusterResponse"
    "fixture/DeleteClusterResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteCluster)

responseDescribeBackups :: DescribeBackupsResponse -> TestTree
responseDescribeBackups =
  res
    "DescribeBackupsResponse"
    "fixture/DescribeBackupsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeBackups)

responseInitializeCluster :: InitializeClusterResponse -> TestTree
responseInitializeCluster =
  res
    "InitializeClusterResponse"
    "fixture/InitializeClusterResponse.proto"
    defaultService
    (Proxy :: Proxy InitializeCluster)

responseCreateHsm :: CreateHsmResponse -> TestTree
responseCreateHsm =
  res
    "CreateHsmResponse"
    "fixture/CreateHsmResponse.proto"
    defaultService
    (Proxy :: Proxy CreateHsm)
