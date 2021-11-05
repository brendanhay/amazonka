{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.CloudHSMV2
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.AWS.Gen.CloudHSMV2 where

import Amazonka.CloudHSMV2
import qualified Data.Proxy as Proxy
import Test.AWS.CloudHSMV2.Internal
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
--             newDescribeClusters
--
--         , requestDeleteBackup $
--             newDeleteBackup
--
--         , requestInitializeCluster $
--             newInitializeCluster
--
--         , requestCreateHsm $
--             newCreateHsm
--
--         , requestDescribeBackups $
--             newDescribeBackups
--
--         , requestCopyBackupToRegion $
--             newCopyBackupToRegion
--
--         , requestDeleteCluster $
--             newDeleteCluster
--
--         , requestCreateCluster $
--             newCreateCluster
--
--         , requestRestoreBackup $
--             newRestoreBackup
--
--         , requestDeleteHsm $
--             newDeleteHsm
--
--         , requestModifyCluster $
--             newModifyCluster
--
--         , requestTagResource $
--             newTagResource
--
--         , requestListTags $
--             newListTags
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestModifyBackupAttributes $
--             newModifyBackupAttributes
--
--           ]

--     , testGroup "response"
--         [ responseDescribeClusters $
--             newDescribeClustersResponse
--
--         , responseDeleteBackup $
--             newDeleteBackupResponse
--
--         , responseInitializeCluster $
--             newInitializeClusterResponse
--
--         , responseCreateHsm $
--             newCreateHsmResponse
--
--         , responseDescribeBackups $
--             newDescribeBackupsResponse
--
--         , responseCopyBackupToRegion $
--             newCopyBackupToRegionResponse
--
--         , responseDeleteCluster $
--             newDeleteClusterResponse
--
--         , responseCreateCluster $
--             newCreateClusterResponse
--
--         , responseRestoreBackup $
--             newRestoreBackupResponse
--
--         , responseDeleteHsm $
--             newDeleteHsmResponse
--
--         , responseModifyCluster $
--             newModifyClusterResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseListTags $
--             newListTagsResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseModifyBackupAttributes $
--             newModifyBackupAttributesResponse
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

requestCreateHsm :: CreateHsm -> TestTree
requestCreateHsm =
  req
    "CreateHsm"
    "fixture/CreateHsm.yaml"

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

requestDeleteHsm :: DeleteHsm -> TestTree
requestDeleteHsm =
  req
    "DeleteHsm"
    "fixture/DeleteHsm.yaml"

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
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeClusters)

responseDeleteBackup :: DeleteBackupResponse -> TestTree
responseDeleteBackup =
  res
    "DeleteBackupResponse"
    "fixture/DeleteBackupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteBackup)

responseInitializeCluster :: InitializeClusterResponse -> TestTree
responseInitializeCluster =
  res
    "InitializeClusterResponse"
    "fixture/InitializeClusterResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy InitializeCluster)

responseCreateHsm :: CreateHsmResponse -> TestTree
responseCreateHsm =
  res
    "CreateHsmResponse"
    "fixture/CreateHsmResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateHsm)

responseDescribeBackups :: DescribeBackupsResponse -> TestTree
responseDescribeBackups =
  res
    "DescribeBackupsResponse"
    "fixture/DescribeBackupsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeBackups)

responseCopyBackupToRegion :: CopyBackupToRegionResponse -> TestTree
responseCopyBackupToRegion =
  res
    "CopyBackupToRegionResponse"
    "fixture/CopyBackupToRegionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CopyBackupToRegion)

responseDeleteCluster :: DeleteClusterResponse -> TestTree
responseDeleteCluster =
  res
    "DeleteClusterResponse"
    "fixture/DeleteClusterResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteCluster)

responseCreateCluster :: CreateClusterResponse -> TestTree
responseCreateCluster =
  res
    "CreateClusterResponse"
    "fixture/CreateClusterResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateCluster)

responseRestoreBackup :: RestoreBackupResponse -> TestTree
responseRestoreBackup =
  res
    "RestoreBackupResponse"
    "fixture/RestoreBackupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RestoreBackup)

responseDeleteHsm :: DeleteHsmResponse -> TestTree
responseDeleteHsm =
  res
    "DeleteHsmResponse"
    "fixture/DeleteHsmResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteHsm)

responseModifyCluster :: ModifyClusterResponse -> TestTree
responseModifyCluster =
  res
    "ModifyClusterResponse"
    "fixture/ModifyClusterResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ModifyCluster)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TagResource)

responseListTags :: ListTagsResponse -> TestTree
responseListTags =
  res
    "ListTagsResponse"
    "fixture/ListTagsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTags)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UntagResource)

responseModifyBackupAttributes :: ModifyBackupAttributesResponse -> TestTree
responseModifyBackupAttributes =
  res
    "ModifyBackupAttributesResponse"
    "fixture/ModifyBackupAttributesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ModifyBackupAttributes)
