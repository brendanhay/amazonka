{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.FSx
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.FSx where

import Amazonka.FSx
import qualified Data.Proxy as Proxy
import Test.Amazonka.FSx.Internal
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
--         [ requestAssociateFileSystemAliases $
--             newAssociateFileSystemAliases
--
--         , requestCancelDataRepositoryTask $
--             newCancelDataRepositoryTask
--
--         , requestCopyBackup $
--             newCopyBackup
--
--         , requestCreateBackup $
--             newCreateBackup
--
--         , requestCreateDataRepositoryAssociation $
--             newCreateDataRepositoryAssociation
--
--         , requestCreateDataRepositoryTask $
--             newCreateDataRepositoryTask
--
--         , requestCreateFileCache $
--             newCreateFileCache
--
--         , requestCreateFileSystem $
--             newCreateFileSystem
--
--         , requestCreateFileSystemFromBackup $
--             newCreateFileSystemFromBackup
--
--         , requestCreateSnapshot $
--             newCreateSnapshot
--
--         , requestCreateStorageVirtualMachine $
--             newCreateStorageVirtualMachine
--
--         , requestCreateVolume $
--             newCreateVolume
--
--         , requestCreateVolumeFromBackup $
--             newCreateVolumeFromBackup
--
--         , requestDeleteBackup $
--             newDeleteBackup
--
--         , requestDeleteDataRepositoryAssociation $
--             newDeleteDataRepositoryAssociation
--
--         , requestDeleteFileCache $
--             newDeleteFileCache
--
--         , requestDeleteFileSystem $
--             newDeleteFileSystem
--
--         , requestDeleteSnapshot $
--             newDeleteSnapshot
--
--         , requestDeleteStorageVirtualMachine $
--             newDeleteStorageVirtualMachine
--
--         , requestDeleteVolume $
--             newDeleteVolume
--
--         , requestDescribeBackups $
--             newDescribeBackups
--
--         , requestDescribeDataRepositoryAssociations $
--             newDescribeDataRepositoryAssociations
--
--         , requestDescribeDataRepositoryTasks $
--             newDescribeDataRepositoryTasks
--
--         , requestDescribeFileCaches $
--             newDescribeFileCaches
--
--         , requestDescribeFileSystemAliases $
--             newDescribeFileSystemAliases
--
--         , requestDescribeFileSystems $
--             newDescribeFileSystems
--
--         , requestDescribeSnapshots $
--             newDescribeSnapshots
--
--         , requestDescribeStorageVirtualMachines $
--             newDescribeStorageVirtualMachines
--
--         , requestDescribeVolumes $
--             newDescribeVolumes
--
--         , requestDisassociateFileSystemAliases $
--             newDisassociateFileSystemAliases
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestReleaseFileSystemNfsV3Locks $
--             newReleaseFileSystemNfsV3Locks
--
--         , requestRestoreVolumeFromSnapshot $
--             newRestoreVolumeFromSnapshot
--
--         , requestTagResource $
--             newTagResource
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestUpdateDataRepositoryAssociation $
--             newUpdateDataRepositoryAssociation
--
--         , requestUpdateFileCache $
--             newUpdateFileCache
--
--         , requestUpdateFileSystem $
--             newUpdateFileSystem
--
--         , requestUpdateSnapshot $
--             newUpdateSnapshot
--
--         , requestUpdateStorageVirtualMachine $
--             newUpdateStorageVirtualMachine
--
--         , requestUpdateVolume $
--             newUpdateVolume
--
--           ]

--     , testGroup "response"
--         [ responseAssociateFileSystemAliases $
--             newAssociateFileSystemAliasesResponse
--
--         , responseCancelDataRepositoryTask $
--             newCancelDataRepositoryTaskResponse
--
--         , responseCopyBackup $
--             newCopyBackupResponse
--
--         , responseCreateBackup $
--             newCreateBackupResponse
--
--         , responseCreateDataRepositoryAssociation $
--             newCreateDataRepositoryAssociationResponse
--
--         , responseCreateDataRepositoryTask $
--             newCreateDataRepositoryTaskResponse
--
--         , responseCreateFileCache $
--             newCreateFileCacheResponse
--
--         , responseCreateFileSystem $
--             newCreateFileSystemResponse
--
--         , responseCreateFileSystemFromBackup $
--             newCreateFileSystemFromBackupResponse
--
--         , responseCreateSnapshot $
--             newCreateSnapshotResponse
--
--         , responseCreateStorageVirtualMachine $
--             newCreateStorageVirtualMachineResponse
--
--         , responseCreateVolume $
--             newCreateVolumeResponse
--
--         , responseCreateVolumeFromBackup $
--             newCreateVolumeFromBackupResponse
--
--         , responseDeleteBackup $
--             newDeleteBackupResponse
--
--         , responseDeleteDataRepositoryAssociation $
--             newDeleteDataRepositoryAssociationResponse
--
--         , responseDeleteFileCache $
--             newDeleteFileCacheResponse
--
--         , responseDeleteFileSystem $
--             newDeleteFileSystemResponse
--
--         , responseDeleteSnapshot $
--             newDeleteSnapshotResponse
--
--         , responseDeleteStorageVirtualMachine $
--             newDeleteStorageVirtualMachineResponse
--
--         , responseDeleteVolume $
--             newDeleteVolumeResponse
--
--         , responseDescribeBackups $
--             newDescribeBackupsResponse
--
--         , responseDescribeDataRepositoryAssociations $
--             newDescribeDataRepositoryAssociationsResponse
--
--         , responseDescribeDataRepositoryTasks $
--             newDescribeDataRepositoryTasksResponse
--
--         , responseDescribeFileCaches $
--             newDescribeFileCachesResponse
--
--         , responseDescribeFileSystemAliases $
--             newDescribeFileSystemAliasesResponse
--
--         , responseDescribeFileSystems $
--             newDescribeFileSystemsResponse
--
--         , responseDescribeSnapshots $
--             newDescribeSnapshotsResponse
--
--         , responseDescribeStorageVirtualMachines $
--             newDescribeStorageVirtualMachinesResponse
--
--         , responseDescribeVolumes $
--             newDescribeVolumesResponse
--
--         , responseDisassociateFileSystemAliases $
--             newDisassociateFileSystemAliasesResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseReleaseFileSystemNfsV3Locks $
--             newReleaseFileSystemNfsV3LocksResponse
--
--         , responseRestoreVolumeFromSnapshot $
--             newRestoreVolumeFromSnapshotResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseUpdateDataRepositoryAssociation $
--             newUpdateDataRepositoryAssociationResponse
--
--         , responseUpdateFileCache $
--             newUpdateFileCacheResponse
--
--         , responseUpdateFileSystem $
--             newUpdateFileSystemResponse
--
--         , responseUpdateSnapshot $
--             newUpdateSnapshotResponse
--
--         , responseUpdateStorageVirtualMachine $
--             newUpdateStorageVirtualMachineResponse
--
--         , responseUpdateVolume $
--             newUpdateVolumeResponse
--
--           ]
--     ]

-- Requests

requestAssociateFileSystemAliases :: AssociateFileSystemAliases -> TestTree
requestAssociateFileSystemAliases =
  req
    "AssociateFileSystemAliases"
    "fixture/AssociateFileSystemAliases.yaml"

requestCancelDataRepositoryTask :: CancelDataRepositoryTask -> TestTree
requestCancelDataRepositoryTask =
  req
    "CancelDataRepositoryTask"
    "fixture/CancelDataRepositoryTask.yaml"

requestCopyBackup :: CopyBackup -> TestTree
requestCopyBackup =
  req
    "CopyBackup"
    "fixture/CopyBackup.yaml"

requestCreateBackup :: CreateBackup -> TestTree
requestCreateBackup =
  req
    "CreateBackup"
    "fixture/CreateBackup.yaml"

requestCreateDataRepositoryAssociation :: CreateDataRepositoryAssociation -> TestTree
requestCreateDataRepositoryAssociation =
  req
    "CreateDataRepositoryAssociation"
    "fixture/CreateDataRepositoryAssociation.yaml"

requestCreateDataRepositoryTask :: CreateDataRepositoryTask -> TestTree
requestCreateDataRepositoryTask =
  req
    "CreateDataRepositoryTask"
    "fixture/CreateDataRepositoryTask.yaml"

requestCreateFileCache :: CreateFileCache -> TestTree
requestCreateFileCache =
  req
    "CreateFileCache"
    "fixture/CreateFileCache.yaml"

requestCreateFileSystem :: CreateFileSystem -> TestTree
requestCreateFileSystem =
  req
    "CreateFileSystem"
    "fixture/CreateFileSystem.yaml"

requestCreateFileSystemFromBackup :: CreateFileSystemFromBackup -> TestTree
requestCreateFileSystemFromBackup =
  req
    "CreateFileSystemFromBackup"
    "fixture/CreateFileSystemFromBackup.yaml"

requestCreateSnapshot :: CreateSnapshot -> TestTree
requestCreateSnapshot =
  req
    "CreateSnapshot"
    "fixture/CreateSnapshot.yaml"

requestCreateStorageVirtualMachine :: CreateStorageVirtualMachine -> TestTree
requestCreateStorageVirtualMachine =
  req
    "CreateStorageVirtualMachine"
    "fixture/CreateStorageVirtualMachine.yaml"

requestCreateVolume :: CreateVolume -> TestTree
requestCreateVolume =
  req
    "CreateVolume"
    "fixture/CreateVolume.yaml"

requestCreateVolumeFromBackup :: CreateVolumeFromBackup -> TestTree
requestCreateVolumeFromBackup =
  req
    "CreateVolumeFromBackup"
    "fixture/CreateVolumeFromBackup.yaml"

requestDeleteBackup :: DeleteBackup -> TestTree
requestDeleteBackup =
  req
    "DeleteBackup"
    "fixture/DeleteBackup.yaml"

requestDeleteDataRepositoryAssociation :: DeleteDataRepositoryAssociation -> TestTree
requestDeleteDataRepositoryAssociation =
  req
    "DeleteDataRepositoryAssociation"
    "fixture/DeleteDataRepositoryAssociation.yaml"

requestDeleteFileCache :: DeleteFileCache -> TestTree
requestDeleteFileCache =
  req
    "DeleteFileCache"
    "fixture/DeleteFileCache.yaml"

requestDeleteFileSystem :: DeleteFileSystem -> TestTree
requestDeleteFileSystem =
  req
    "DeleteFileSystem"
    "fixture/DeleteFileSystem.yaml"

requestDeleteSnapshot :: DeleteSnapshot -> TestTree
requestDeleteSnapshot =
  req
    "DeleteSnapshot"
    "fixture/DeleteSnapshot.yaml"

requestDeleteStorageVirtualMachine :: DeleteStorageVirtualMachine -> TestTree
requestDeleteStorageVirtualMachine =
  req
    "DeleteStorageVirtualMachine"
    "fixture/DeleteStorageVirtualMachine.yaml"

requestDeleteVolume :: DeleteVolume -> TestTree
requestDeleteVolume =
  req
    "DeleteVolume"
    "fixture/DeleteVolume.yaml"

requestDescribeBackups :: DescribeBackups -> TestTree
requestDescribeBackups =
  req
    "DescribeBackups"
    "fixture/DescribeBackups.yaml"

requestDescribeDataRepositoryAssociations :: DescribeDataRepositoryAssociations -> TestTree
requestDescribeDataRepositoryAssociations =
  req
    "DescribeDataRepositoryAssociations"
    "fixture/DescribeDataRepositoryAssociations.yaml"

requestDescribeDataRepositoryTasks :: DescribeDataRepositoryTasks -> TestTree
requestDescribeDataRepositoryTasks =
  req
    "DescribeDataRepositoryTasks"
    "fixture/DescribeDataRepositoryTasks.yaml"

requestDescribeFileCaches :: DescribeFileCaches -> TestTree
requestDescribeFileCaches =
  req
    "DescribeFileCaches"
    "fixture/DescribeFileCaches.yaml"

requestDescribeFileSystemAliases :: DescribeFileSystemAliases -> TestTree
requestDescribeFileSystemAliases =
  req
    "DescribeFileSystemAliases"
    "fixture/DescribeFileSystemAliases.yaml"

requestDescribeFileSystems :: DescribeFileSystems -> TestTree
requestDescribeFileSystems =
  req
    "DescribeFileSystems"
    "fixture/DescribeFileSystems.yaml"

requestDescribeSnapshots :: DescribeSnapshots -> TestTree
requestDescribeSnapshots =
  req
    "DescribeSnapshots"
    "fixture/DescribeSnapshots.yaml"

requestDescribeStorageVirtualMachines :: DescribeStorageVirtualMachines -> TestTree
requestDescribeStorageVirtualMachines =
  req
    "DescribeStorageVirtualMachines"
    "fixture/DescribeStorageVirtualMachines.yaml"

requestDescribeVolumes :: DescribeVolumes -> TestTree
requestDescribeVolumes =
  req
    "DescribeVolumes"
    "fixture/DescribeVolumes.yaml"

requestDisassociateFileSystemAliases :: DisassociateFileSystemAliases -> TestTree
requestDisassociateFileSystemAliases =
  req
    "DisassociateFileSystemAliases"
    "fixture/DisassociateFileSystemAliases.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestReleaseFileSystemNfsV3Locks :: ReleaseFileSystemNfsV3Locks -> TestTree
requestReleaseFileSystemNfsV3Locks =
  req
    "ReleaseFileSystemNfsV3Locks"
    "fixture/ReleaseFileSystemNfsV3Locks.yaml"

requestRestoreVolumeFromSnapshot :: RestoreVolumeFromSnapshot -> TestTree
requestRestoreVolumeFromSnapshot =
  req
    "RestoreVolumeFromSnapshot"
    "fixture/RestoreVolumeFromSnapshot.yaml"

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

requestUpdateDataRepositoryAssociation :: UpdateDataRepositoryAssociation -> TestTree
requestUpdateDataRepositoryAssociation =
  req
    "UpdateDataRepositoryAssociation"
    "fixture/UpdateDataRepositoryAssociation.yaml"

requestUpdateFileCache :: UpdateFileCache -> TestTree
requestUpdateFileCache =
  req
    "UpdateFileCache"
    "fixture/UpdateFileCache.yaml"

requestUpdateFileSystem :: UpdateFileSystem -> TestTree
requestUpdateFileSystem =
  req
    "UpdateFileSystem"
    "fixture/UpdateFileSystem.yaml"

requestUpdateSnapshot :: UpdateSnapshot -> TestTree
requestUpdateSnapshot =
  req
    "UpdateSnapshot"
    "fixture/UpdateSnapshot.yaml"

requestUpdateStorageVirtualMachine :: UpdateStorageVirtualMachine -> TestTree
requestUpdateStorageVirtualMachine =
  req
    "UpdateStorageVirtualMachine"
    "fixture/UpdateStorageVirtualMachine.yaml"

requestUpdateVolume :: UpdateVolume -> TestTree
requestUpdateVolume =
  req
    "UpdateVolume"
    "fixture/UpdateVolume.yaml"

-- Responses

responseAssociateFileSystemAliases :: AssociateFileSystemAliasesResponse -> TestTree
responseAssociateFileSystemAliases =
  res
    "AssociateFileSystemAliasesResponse"
    "fixture/AssociateFileSystemAliasesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssociateFileSystemAliases)

responseCancelDataRepositoryTask :: CancelDataRepositoryTaskResponse -> TestTree
responseCancelDataRepositoryTask =
  res
    "CancelDataRepositoryTaskResponse"
    "fixture/CancelDataRepositoryTaskResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CancelDataRepositoryTask)

responseCopyBackup :: CopyBackupResponse -> TestTree
responseCopyBackup =
  res
    "CopyBackupResponse"
    "fixture/CopyBackupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CopyBackup)

responseCreateBackup :: CreateBackupResponse -> TestTree
responseCreateBackup =
  res
    "CreateBackupResponse"
    "fixture/CreateBackupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateBackup)

responseCreateDataRepositoryAssociation :: CreateDataRepositoryAssociationResponse -> TestTree
responseCreateDataRepositoryAssociation =
  res
    "CreateDataRepositoryAssociationResponse"
    "fixture/CreateDataRepositoryAssociationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateDataRepositoryAssociation)

responseCreateDataRepositoryTask :: CreateDataRepositoryTaskResponse -> TestTree
responseCreateDataRepositoryTask =
  res
    "CreateDataRepositoryTaskResponse"
    "fixture/CreateDataRepositoryTaskResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateDataRepositoryTask)

responseCreateFileCache :: CreateFileCacheResponse -> TestTree
responseCreateFileCache =
  res
    "CreateFileCacheResponse"
    "fixture/CreateFileCacheResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateFileCache)

responseCreateFileSystem :: CreateFileSystemResponse -> TestTree
responseCreateFileSystem =
  res
    "CreateFileSystemResponse"
    "fixture/CreateFileSystemResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateFileSystem)

responseCreateFileSystemFromBackup :: CreateFileSystemFromBackupResponse -> TestTree
responseCreateFileSystemFromBackup =
  res
    "CreateFileSystemFromBackupResponse"
    "fixture/CreateFileSystemFromBackupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateFileSystemFromBackup)

responseCreateSnapshot :: CreateSnapshotResponse -> TestTree
responseCreateSnapshot =
  res
    "CreateSnapshotResponse"
    "fixture/CreateSnapshotResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateSnapshot)

responseCreateStorageVirtualMachine :: CreateStorageVirtualMachineResponse -> TestTree
responseCreateStorageVirtualMachine =
  res
    "CreateStorageVirtualMachineResponse"
    "fixture/CreateStorageVirtualMachineResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateStorageVirtualMachine)

responseCreateVolume :: CreateVolumeResponse -> TestTree
responseCreateVolume =
  res
    "CreateVolumeResponse"
    "fixture/CreateVolumeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateVolume)

responseCreateVolumeFromBackup :: CreateVolumeFromBackupResponse -> TestTree
responseCreateVolumeFromBackup =
  res
    "CreateVolumeFromBackupResponse"
    "fixture/CreateVolumeFromBackupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateVolumeFromBackup)

responseDeleteBackup :: DeleteBackupResponse -> TestTree
responseDeleteBackup =
  res
    "DeleteBackupResponse"
    "fixture/DeleteBackupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteBackup)

responseDeleteDataRepositoryAssociation :: DeleteDataRepositoryAssociationResponse -> TestTree
responseDeleteDataRepositoryAssociation =
  res
    "DeleteDataRepositoryAssociationResponse"
    "fixture/DeleteDataRepositoryAssociationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteDataRepositoryAssociation)

responseDeleteFileCache :: DeleteFileCacheResponse -> TestTree
responseDeleteFileCache =
  res
    "DeleteFileCacheResponse"
    "fixture/DeleteFileCacheResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteFileCache)

responseDeleteFileSystem :: DeleteFileSystemResponse -> TestTree
responseDeleteFileSystem =
  res
    "DeleteFileSystemResponse"
    "fixture/DeleteFileSystemResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteFileSystem)

responseDeleteSnapshot :: DeleteSnapshotResponse -> TestTree
responseDeleteSnapshot =
  res
    "DeleteSnapshotResponse"
    "fixture/DeleteSnapshotResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteSnapshot)

responseDeleteStorageVirtualMachine :: DeleteStorageVirtualMachineResponse -> TestTree
responseDeleteStorageVirtualMachine =
  res
    "DeleteStorageVirtualMachineResponse"
    "fixture/DeleteStorageVirtualMachineResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteStorageVirtualMachine)

responseDeleteVolume :: DeleteVolumeResponse -> TestTree
responseDeleteVolume =
  res
    "DeleteVolumeResponse"
    "fixture/DeleteVolumeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteVolume)

responseDescribeBackups :: DescribeBackupsResponse -> TestTree
responseDescribeBackups =
  res
    "DescribeBackupsResponse"
    "fixture/DescribeBackupsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeBackups)

responseDescribeDataRepositoryAssociations :: DescribeDataRepositoryAssociationsResponse -> TestTree
responseDescribeDataRepositoryAssociations =
  res
    "DescribeDataRepositoryAssociationsResponse"
    "fixture/DescribeDataRepositoryAssociationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeDataRepositoryAssociations)

responseDescribeDataRepositoryTasks :: DescribeDataRepositoryTasksResponse -> TestTree
responseDescribeDataRepositoryTasks =
  res
    "DescribeDataRepositoryTasksResponse"
    "fixture/DescribeDataRepositoryTasksResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeDataRepositoryTasks)

responseDescribeFileCaches :: DescribeFileCachesResponse -> TestTree
responseDescribeFileCaches =
  res
    "DescribeFileCachesResponse"
    "fixture/DescribeFileCachesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeFileCaches)

responseDescribeFileSystemAliases :: DescribeFileSystemAliasesResponse -> TestTree
responseDescribeFileSystemAliases =
  res
    "DescribeFileSystemAliasesResponse"
    "fixture/DescribeFileSystemAliasesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeFileSystemAliases)

responseDescribeFileSystems :: DescribeFileSystemsResponse -> TestTree
responseDescribeFileSystems =
  res
    "DescribeFileSystemsResponse"
    "fixture/DescribeFileSystemsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeFileSystems)

responseDescribeSnapshots :: DescribeSnapshotsResponse -> TestTree
responseDescribeSnapshots =
  res
    "DescribeSnapshotsResponse"
    "fixture/DescribeSnapshotsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeSnapshots)

responseDescribeStorageVirtualMachines :: DescribeStorageVirtualMachinesResponse -> TestTree
responseDescribeStorageVirtualMachines =
  res
    "DescribeStorageVirtualMachinesResponse"
    "fixture/DescribeStorageVirtualMachinesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeStorageVirtualMachines)

responseDescribeVolumes :: DescribeVolumesResponse -> TestTree
responseDescribeVolumes =
  res
    "DescribeVolumesResponse"
    "fixture/DescribeVolumesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeVolumes)

responseDisassociateFileSystemAliases :: DisassociateFileSystemAliasesResponse -> TestTree
responseDisassociateFileSystemAliases =
  res
    "DisassociateFileSystemAliasesResponse"
    "fixture/DisassociateFileSystemAliasesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisassociateFileSystemAliases)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responseReleaseFileSystemNfsV3Locks :: ReleaseFileSystemNfsV3LocksResponse -> TestTree
responseReleaseFileSystemNfsV3Locks =
  res
    "ReleaseFileSystemNfsV3LocksResponse"
    "fixture/ReleaseFileSystemNfsV3LocksResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ReleaseFileSystemNfsV3Locks)

responseRestoreVolumeFromSnapshot :: RestoreVolumeFromSnapshotResponse -> TestTree
responseRestoreVolumeFromSnapshot =
  res
    "RestoreVolumeFromSnapshotResponse"
    "fixture/RestoreVolumeFromSnapshotResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RestoreVolumeFromSnapshot)

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

responseUpdateDataRepositoryAssociation :: UpdateDataRepositoryAssociationResponse -> TestTree
responseUpdateDataRepositoryAssociation =
  res
    "UpdateDataRepositoryAssociationResponse"
    "fixture/UpdateDataRepositoryAssociationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateDataRepositoryAssociation)

responseUpdateFileCache :: UpdateFileCacheResponse -> TestTree
responseUpdateFileCache =
  res
    "UpdateFileCacheResponse"
    "fixture/UpdateFileCacheResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateFileCache)

responseUpdateFileSystem :: UpdateFileSystemResponse -> TestTree
responseUpdateFileSystem =
  res
    "UpdateFileSystemResponse"
    "fixture/UpdateFileSystemResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateFileSystem)

responseUpdateSnapshot :: UpdateSnapshotResponse -> TestTree
responseUpdateSnapshot =
  res
    "UpdateSnapshotResponse"
    "fixture/UpdateSnapshotResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateSnapshot)

responseUpdateStorageVirtualMachine :: UpdateStorageVirtualMachineResponse -> TestTree
responseUpdateStorageVirtualMachine =
  res
    "UpdateStorageVirtualMachineResponse"
    "fixture/UpdateStorageVirtualMachineResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateStorageVirtualMachine)

responseUpdateVolume :: UpdateVolumeResponse -> TestTree
responseUpdateVolume =
  res
    "UpdateVolumeResponse"
    "fixture/UpdateVolumeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateVolume)
