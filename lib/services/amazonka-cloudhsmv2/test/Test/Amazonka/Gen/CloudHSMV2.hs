{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.CloudHSMV2
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.CloudHSMV2 where

import Amazonka.CloudHSMV2
import qualified Data.Proxy as Proxy
import Test.Amazonka.CloudHSMV2.Internal
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
--         [ requestCopyBackupToRegion $
--             newCopyBackupToRegion
--
--         , requestCreateCluster $
--             newCreateCluster
--
--         , requestCreateHsm $
--             newCreateHsm
--
--         , requestDeleteBackup $
--             newDeleteBackup
--
--         , requestDeleteCluster $
--             newDeleteCluster
--
--         , requestDeleteHsm $
--             newDeleteHsm
--
--         , requestDescribeBackups $
--             newDescribeBackups
--
--         , requestDescribeClusters $
--             newDescribeClusters
--
--         , requestInitializeCluster $
--             newInitializeCluster
--
--         , requestListTags $
--             newListTags
--
--         , requestModifyBackupAttributes $
--             newModifyBackupAttributes
--
--         , requestModifyCluster $
--             newModifyCluster
--
--         , requestRestoreBackup $
--             newRestoreBackup
--
--         , requestTagResource $
--             newTagResource
--
--         , requestUntagResource $
--             newUntagResource
--
--           ]

--     , testGroup "response"
--         [ responseCopyBackupToRegion $
--             newCopyBackupToRegionResponse
--
--         , responseCreateCluster $
--             newCreateClusterResponse
--
--         , responseCreateHsm $
--             newCreateHsmResponse
--
--         , responseDeleteBackup $
--             newDeleteBackupResponse
--
--         , responseDeleteCluster $
--             newDeleteClusterResponse
--
--         , responseDeleteHsm $
--             newDeleteHsmResponse
--
--         , responseDescribeBackups $
--             newDescribeBackupsResponse
--
--         , responseDescribeClusters $
--             newDescribeClustersResponse
--
--         , responseInitializeCluster $
--             newInitializeClusterResponse
--
--         , responseListTags $
--             newListTagsResponse
--
--         , responseModifyBackupAttributes $
--             newModifyBackupAttributesResponse
--
--         , responseModifyCluster $
--             newModifyClusterResponse
--
--         , responseRestoreBackup $
--             newRestoreBackupResponse
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

requestCopyBackupToRegion :: CopyBackupToRegion -> TestTree
requestCopyBackupToRegion =
  req
    "CopyBackupToRegion"
    "fixture/CopyBackupToRegion.yaml"

requestCreateCluster :: CreateCluster -> TestTree
requestCreateCluster =
  req
    "CreateCluster"
    "fixture/CreateCluster.yaml"

requestCreateHsm :: CreateHsm -> TestTree
requestCreateHsm =
  req
    "CreateHsm"
    "fixture/CreateHsm.yaml"

requestDeleteBackup :: DeleteBackup -> TestTree
requestDeleteBackup =
  req
    "DeleteBackup"
    "fixture/DeleteBackup.yaml"

requestDeleteCluster :: DeleteCluster -> TestTree
requestDeleteCluster =
  req
    "DeleteCluster"
    "fixture/DeleteCluster.yaml"

requestDeleteHsm :: DeleteHsm -> TestTree
requestDeleteHsm =
  req
    "DeleteHsm"
    "fixture/DeleteHsm.yaml"

requestDescribeBackups :: DescribeBackups -> TestTree
requestDescribeBackups =
  req
    "DescribeBackups"
    "fixture/DescribeBackups.yaml"

requestDescribeClusters :: DescribeClusters -> TestTree
requestDescribeClusters =
  req
    "DescribeClusters"
    "fixture/DescribeClusters.yaml"

requestInitializeCluster :: InitializeCluster -> TestTree
requestInitializeCluster =
  req
    "InitializeCluster"
    "fixture/InitializeCluster.yaml"

requestListTags :: ListTags -> TestTree
requestListTags =
  req
    "ListTags"
    "fixture/ListTags.yaml"

requestModifyBackupAttributes :: ModifyBackupAttributes -> TestTree
requestModifyBackupAttributes =
  req
    "ModifyBackupAttributes"
    "fixture/ModifyBackupAttributes.yaml"

requestModifyCluster :: ModifyCluster -> TestTree
requestModifyCluster =
  req
    "ModifyCluster"
    "fixture/ModifyCluster.yaml"

requestRestoreBackup :: RestoreBackup -> TestTree
requestRestoreBackup =
  req
    "RestoreBackup"
    "fixture/RestoreBackup.yaml"

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

responseCopyBackupToRegion :: CopyBackupToRegionResponse -> TestTree
responseCopyBackupToRegion =
  res
    "CopyBackupToRegionResponse"
    "fixture/CopyBackupToRegionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CopyBackupToRegion)

responseCreateCluster :: CreateClusterResponse -> TestTree
responseCreateCluster =
  res
    "CreateClusterResponse"
    "fixture/CreateClusterResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateCluster)

responseCreateHsm :: CreateHsmResponse -> TestTree
responseCreateHsm =
  res
    "CreateHsmResponse"
    "fixture/CreateHsmResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateHsm)

responseDeleteBackup :: DeleteBackupResponse -> TestTree
responseDeleteBackup =
  res
    "DeleteBackupResponse"
    "fixture/DeleteBackupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteBackup)

responseDeleteCluster :: DeleteClusterResponse -> TestTree
responseDeleteCluster =
  res
    "DeleteClusterResponse"
    "fixture/DeleteClusterResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteCluster)

responseDeleteHsm :: DeleteHsmResponse -> TestTree
responseDeleteHsm =
  res
    "DeleteHsmResponse"
    "fixture/DeleteHsmResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteHsm)

responseDescribeBackups :: DescribeBackupsResponse -> TestTree
responseDescribeBackups =
  res
    "DescribeBackupsResponse"
    "fixture/DescribeBackupsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeBackups)

responseDescribeClusters :: DescribeClustersResponse -> TestTree
responseDescribeClusters =
  res
    "DescribeClustersResponse"
    "fixture/DescribeClustersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeClusters)

responseInitializeCluster :: InitializeClusterResponse -> TestTree
responseInitializeCluster =
  res
    "InitializeClusterResponse"
    "fixture/InitializeClusterResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy InitializeCluster)

responseListTags :: ListTagsResponse -> TestTree
responseListTags =
  res
    "ListTagsResponse"
    "fixture/ListTagsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTags)

responseModifyBackupAttributes :: ModifyBackupAttributesResponse -> TestTree
responseModifyBackupAttributes =
  res
    "ModifyBackupAttributesResponse"
    "fixture/ModifyBackupAttributesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ModifyBackupAttributes)

responseModifyCluster :: ModifyClusterResponse -> TestTree
responseModifyCluster =
  res
    "ModifyClusterResponse"
    "fixture/ModifyClusterResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ModifyCluster)

responseRestoreBackup :: RestoreBackupResponse -> TestTree
responseRestoreBackup =
  res
    "RestoreBackupResponse"
    "fixture/RestoreBackupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RestoreBackup)

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
