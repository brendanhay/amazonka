{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.WorkSpaces
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.AWS.Gen.WorkSpaces where

import Data.Proxy
import Network.AWS.WorkSpaces
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.AWS.WorkSpaces.Internal
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestAuthorizeIpRules $
--             newAuthorizeIpRules
--
--         , requestImportWorkspaceImage $
--             newImportWorkspaceImage
--
--         , requestDescribeWorkspaceDirectories $
--             newDescribeWorkspaceDirectories
--
--         , requestDescribeWorkspaceBundles $
--             newDescribeWorkspaceBundles
--
--         , requestDeleteIpGroup $
--             newDeleteIpGroup
--
--         , requestTerminateWorkspaces $
--             newTerminateWorkspaces
--
--         , requestDisassociateIpGroups $
--             newDisassociateIpGroups
--
--         , requestDeregisterWorkspaceDirectory $
--             newDeregisterWorkspaceDirectory
--
--         , requestAssociateConnectionAlias $
--             newAssociateConnectionAlias
--
--         , requestCreateConnectionAlias $
--             newCreateConnectionAlias
--
--         , requestMigrateWorkspace $
--             newMigrateWorkspace
--
--         , requestDescribeTags $
--             newDescribeTags
--
--         , requestModifySelfservicePermissions $
--             newModifySelfservicePermissions
--
--         , requestDescribeWorkspacesConnectionStatus $
--             newDescribeWorkspacesConnectionStatus
--
--         , requestModifyAccount $
--             newModifyAccount
--
--         , requestDescribeConnectionAliases $
--             newDescribeConnectionAliases
--
--         , requestDeleteWorkspaceImage $
--             newDeleteWorkspaceImage
--
--         , requestUpdateRulesOfIpGroup $
--             newUpdateRulesOfIpGroup
--
--         , requestUpdateWorkspaceBundle $
--             newUpdateWorkspaceBundle
--
--         , requestDeleteWorkspaceBundle $
--             newDeleteWorkspaceBundle
--
--         , requestDeleteTags $
--             newDeleteTags
--
--         , requestListAvailableManagementCidrRanges $
--             newListAvailableManagementCidrRanges
--
--         , requestDescribeClientProperties $
--             newDescribeClientProperties
--
--         , requestCreateWorkspaceBundle $
--             newCreateWorkspaceBundle
--
--         , requestModifyWorkspaceCreationProperties $
--             newModifyWorkspaceCreationProperties
--
--         , requestModifyWorkspaceState $
--             newModifyWorkspaceState
--
--         , requestUpdateConnectionAliasPermission $
--             newUpdateConnectionAliasPermission
--
--         , requestCopyWorkspaceImage $
--             newCopyWorkspaceImage
--
--         , requestRebuildWorkspaces $
--             newRebuildWorkspaces
--
--         , requestDescribeWorkspaceSnapshots $
--             newDescribeWorkspaceSnapshots
--
--         , requestDescribeWorkspaceImagePermissions $
--             newDescribeWorkspaceImagePermissions
--
--         , requestRebootWorkspaces $
--             newRebootWorkspaces
--
--         , requestDescribeAccount $
--             newDescribeAccount
--
--         , requestRevokeIpRules $
--             newRevokeIpRules
--
--         , requestModifyWorkspaceProperties $
--             newModifyWorkspaceProperties
--
--         , requestDescribeWorkspaceImages $
--             newDescribeWorkspaceImages
--
--         , requestDescribeAccountModifications $
--             newDescribeAccountModifications
--
--         , requestAssociateIpGroups $
--             newAssociateIpGroups
--
--         , requestDeleteConnectionAlias $
--             newDeleteConnectionAlias
--
--         , requestStartWorkspaces $
--             newStartWorkspaces
--
--         , requestStopWorkspaces $
--             newStopWorkspaces
--
--         , requestDescribeWorkspaces $
--             newDescribeWorkspaces
--
--         , requestModifyClientProperties $
--             newModifyClientProperties
--
--         , requestUpdateWorkspaceImagePermission $
--             newUpdateWorkspaceImagePermission
--
--         , requestDescribeIpGroups $
--             newDescribeIpGroups
--
--         , requestModifyWorkspaceAccessProperties $
--             newModifyWorkspaceAccessProperties
--
--         , requestRestoreWorkspace $
--             newRestoreWorkspace
--
--         , requestRegisterWorkspaceDirectory $
--             newRegisterWorkspaceDirectory
--
--         , requestCreateTags $
--             newCreateTags
--
--         , requestDescribeConnectionAliasPermissions $
--             newDescribeConnectionAliasPermissions
--
--         , requestCreateIpGroup $
--             newCreateIpGroup
--
--         , requestDisassociateConnectionAlias $
--             newDisassociateConnectionAlias
--
--         , requestCreateWorkspaces $
--             newCreateWorkspaces
--
--           ]

--     , testGroup "response"
--         [ responseAuthorizeIpRules $
--             newAuthorizeIpRulesResponse
--
--         , responseImportWorkspaceImage $
--             newImportWorkspaceImageResponse
--
--         , responseDescribeWorkspaceDirectories $
--             newDescribeWorkspaceDirectoriesResponse
--
--         , responseDescribeWorkspaceBundles $
--             newDescribeWorkspaceBundlesResponse
--
--         , responseDeleteIpGroup $
--             newDeleteIpGroupResponse
--
--         , responseTerminateWorkspaces $
--             newTerminateWorkspacesResponse
--
--         , responseDisassociateIpGroups $
--             newDisassociateIpGroupsResponse
--
--         , responseDeregisterWorkspaceDirectory $
--             newDeregisterWorkspaceDirectoryResponse
--
--         , responseAssociateConnectionAlias $
--             newAssociateConnectionAliasResponse
--
--         , responseCreateConnectionAlias $
--             newCreateConnectionAliasResponse
--
--         , responseMigrateWorkspace $
--             newMigrateWorkspaceResponse
--
--         , responseDescribeTags $
--             newDescribeTagsResponse
--
--         , responseModifySelfservicePermissions $
--             newModifySelfservicePermissionsResponse
--
--         , responseDescribeWorkspacesConnectionStatus $
--             newDescribeWorkspacesConnectionStatusResponse
--
--         , responseModifyAccount $
--             newModifyAccountResponse
--
--         , responseDescribeConnectionAliases $
--             newDescribeConnectionAliasesResponse
--
--         , responseDeleteWorkspaceImage $
--             newDeleteWorkspaceImageResponse
--
--         , responseUpdateRulesOfIpGroup $
--             newUpdateRulesOfIpGroupResponse
--
--         , responseUpdateWorkspaceBundle $
--             newUpdateWorkspaceBundleResponse
--
--         , responseDeleteWorkspaceBundle $
--             newDeleteWorkspaceBundleResponse
--
--         , responseDeleteTags $
--             newDeleteTagsResponse
--
--         , responseListAvailableManagementCidrRanges $
--             newListAvailableManagementCidrRangesResponse
--
--         , responseDescribeClientProperties $
--             newDescribeClientPropertiesResponse
--
--         , responseCreateWorkspaceBundle $
--             newCreateWorkspaceBundleResponse
--
--         , responseModifyWorkspaceCreationProperties $
--             newModifyWorkspaceCreationPropertiesResponse
--
--         , responseModifyWorkspaceState $
--             newModifyWorkspaceStateResponse
--
--         , responseUpdateConnectionAliasPermission $
--             newUpdateConnectionAliasPermissionResponse
--
--         , responseCopyWorkspaceImage $
--             newCopyWorkspaceImageResponse
--
--         , responseRebuildWorkspaces $
--             newRebuildWorkspacesResponse
--
--         , responseDescribeWorkspaceSnapshots $
--             newDescribeWorkspaceSnapshotsResponse
--
--         , responseDescribeWorkspaceImagePermissions $
--             newDescribeWorkspaceImagePermissionsResponse
--
--         , responseRebootWorkspaces $
--             newRebootWorkspacesResponse
--
--         , responseDescribeAccount $
--             newDescribeAccountResponse
--
--         , responseRevokeIpRules $
--             newRevokeIpRulesResponse
--
--         , responseModifyWorkspaceProperties $
--             newModifyWorkspacePropertiesResponse
--
--         , responseDescribeWorkspaceImages $
--             newDescribeWorkspaceImagesResponse
--
--         , responseDescribeAccountModifications $
--             newDescribeAccountModificationsResponse
--
--         , responseAssociateIpGroups $
--             newAssociateIpGroupsResponse
--
--         , responseDeleteConnectionAlias $
--             newDeleteConnectionAliasResponse
--
--         , responseStartWorkspaces $
--             newStartWorkspacesResponse
--
--         , responseStopWorkspaces $
--             newStopWorkspacesResponse
--
--         , responseDescribeWorkspaces $
--             newDescribeWorkspacesResponse
--
--         , responseModifyClientProperties $
--             newModifyClientPropertiesResponse
--
--         , responseUpdateWorkspaceImagePermission $
--             newUpdateWorkspaceImagePermissionResponse
--
--         , responseDescribeIpGroups $
--             newDescribeIpGroupsResponse
--
--         , responseModifyWorkspaceAccessProperties $
--             newModifyWorkspaceAccessPropertiesResponse
--
--         , responseRestoreWorkspace $
--             newRestoreWorkspaceResponse
--
--         , responseRegisterWorkspaceDirectory $
--             newRegisterWorkspaceDirectoryResponse
--
--         , responseCreateTags $
--             newCreateTagsResponse
--
--         , responseDescribeConnectionAliasPermissions $
--             newDescribeConnectionAliasPermissionsResponse
--
--         , responseCreateIpGroup $
--             newCreateIpGroupResponse
--
--         , responseDisassociateConnectionAlias $
--             newDisassociateConnectionAliasResponse
--
--         , responseCreateWorkspaces $
--             newCreateWorkspacesResponse
--
--           ]
--     ]

-- Requests

requestAuthorizeIpRules :: AuthorizeIpRules -> TestTree
requestAuthorizeIpRules =
  req
    "AuthorizeIpRules"
    "fixture/AuthorizeIpRules.yaml"

requestImportWorkspaceImage :: ImportWorkspaceImage -> TestTree
requestImportWorkspaceImage =
  req
    "ImportWorkspaceImage"
    "fixture/ImportWorkspaceImage.yaml"

requestDescribeWorkspaceDirectories :: DescribeWorkspaceDirectories -> TestTree
requestDescribeWorkspaceDirectories =
  req
    "DescribeWorkspaceDirectories"
    "fixture/DescribeWorkspaceDirectories.yaml"

requestDescribeWorkspaceBundles :: DescribeWorkspaceBundles -> TestTree
requestDescribeWorkspaceBundles =
  req
    "DescribeWorkspaceBundles"
    "fixture/DescribeWorkspaceBundles.yaml"

requestDeleteIpGroup :: DeleteIpGroup -> TestTree
requestDeleteIpGroup =
  req
    "DeleteIpGroup"
    "fixture/DeleteIpGroup.yaml"

requestTerminateWorkspaces :: TerminateWorkspaces -> TestTree
requestTerminateWorkspaces =
  req
    "TerminateWorkspaces"
    "fixture/TerminateWorkspaces.yaml"

requestDisassociateIpGroups :: DisassociateIpGroups -> TestTree
requestDisassociateIpGroups =
  req
    "DisassociateIpGroups"
    "fixture/DisassociateIpGroups.yaml"

requestDeregisterWorkspaceDirectory :: DeregisterWorkspaceDirectory -> TestTree
requestDeregisterWorkspaceDirectory =
  req
    "DeregisterWorkspaceDirectory"
    "fixture/DeregisterWorkspaceDirectory.yaml"

requestAssociateConnectionAlias :: AssociateConnectionAlias -> TestTree
requestAssociateConnectionAlias =
  req
    "AssociateConnectionAlias"
    "fixture/AssociateConnectionAlias.yaml"

requestCreateConnectionAlias :: CreateConnectionAlias -> TestTree
requestCreateConnectionAlias =
  req
    "CreateConnectionAlias"
    "fixture/CreateConnectionAlias.yaml"

requestMigrateWorkspace :: MigrateWorkspace -> TestTree
requestMigrateWorkspace =
  req
    "MigrateWorkspace"
    "fixture/MigrateWorkspace.yaml"

requestDescribeTags :: DescribeTags -> TestTree
requestDescribeTags =
  req
    "DescribeTags"
    "fixture/DescribeTags.yaml"

requestModifySelfservicePermissions :: ModifySelfservicePermissions -> TestTree
requestModifySelfservicePermissions =
  req
    "ModifySelfservicePermissions"
    "fixture/ModifySelfservicePermissions.yaml"

requestDescribeWorkspacesConnectionStatus :: DescribeWorkspacesConnectionStatus -> TestTree
requestDescribeWorkspacesConnectionStatus =
  req
    "DescribeWorkspacesConnectionStatus"
    "fixture/DescribeWorkspacesConnectionStatus.yaml"

requestModifyAccount :: ModifyAccount -> TestTree
requestModifyAccount =
  req
    "ModifyAccount"
    "fixture/ModifyAccount.yaml"

requestDescribeConnectionAliases :: DescribeConnectionAliases -> TestTree
requestDescribeConnectionAliases =
  req
    "DescribeConnectionAliases"
    "fixture/DescribeConnectionAliases.yaml"

requestDeleteWorkspaceImage :: DeleteWorkspaceImage -> TestTree
requestDeleteWorkspaceImage =
  req
    "DeleteWorkspaceImage"
    "fixture/DeleteWorkspaceImage.yaml"

requestUpdateRulesOfIpGroup :: UpdateRulesOfIpGroup -> TestTree
requestUpdateRulesOfIpGroup =
  req
    "UpdateRulesOfIpGroup"
    "fixture/UpdateRulesOfIpGroup.yaml"

requestUpdateWorkspaceBundle :: UpdateWorkspaceBundle -> TestTree
requestUpdateWorkspaceBundle =
  req
    "UpdateWorkspaceBundle"
    "fixture/UpdateWorkspaceBundle.yaml"

requestDeleteWorkspaceBundle :: DeleteWorkspaceBundle -> TestTree
requestDeleteWorkspaceBundle =
  req
    "DeleteWorkspaceBundle"
    "fixture/DeleteWorkspaceBundle.yaml"

requestDeleteTags :: DeleteTags -> TestTree
requestDeleteTags =
  req
    "DeleteTags"
    "fixture/DeleteTags.yaml"

requestListAvailableManagementCidrRanges :: ListAvailableManagementCidrRanges -> TestTree
requestListAvailableManagementCidrRanges =
  req
    "ListAvailableManagementCidrRanges"
    "fixture/ListAvailableManagementCidrRanges.yaml"

requestDescribeClientProperties :: DescribeClientProperties -> TestTree
requestDescribeClientProperties =
  req
    "DescribeClientProperties"
    "fixture/DescribeClientProperties.yaml"

requestCreateWorkspaceBundle :: CreateWorkspaceBundle -> TestTree
requestCreateWorkspaceBundle =
  req
    "CreateWorkspaceBundle"
    "fixture/CreateWorkspaceBundle.yaml"

requestModifyWorkspaceCreationProperties :: ModifyWorkspaceCreationProperties -> TestTree
requestModifyWorkspaceCreationProperties =
  req
    "ModifyWorkspaceCreationProperties"
    "fixture/ModifyWorkspaceCreationProperties.yaml"

requestModifyWorkspaceState :: ModifyWorkspaceState -> TestTree
requestModifyWorkspaceState =
  req
    "ModifyWorkspaceState"
    "fixture/ModifyWorkspaceState.yaml"

requestUpdateConnectionAliasPermission :: UpdateConnectionAliasPermission -> TestTree
requestUpdateConnectionAliasPermission =
  req
    "UpdateConnectionAliasPermission"
    "fixture/UpdateConnectionAliasPermission.yaml"

requestCopyWorkspaceImage :: CopyWorkspaceImage -> TestTree
requestCopyWorkspaceImage =
  req
    "CopyWorkspaceImage"
    "fixture/CopyWorkspaceImage.yaml"

requestRebuildWorkspaces :: RebuildWorkspaces -> TestTree
requestRebuildWorkspaces =
  req
    "RebuildWorkspaces"
    "fixture/RebuildWorkspaces.yaml"

requestDescribeWorkspaceSnapshots :: DescribeWorkspaceSnapshots -> TestTree
requestDescribeWorkspaceSnapshots =
  req
    "DescribeWorkspaceSnapshots"
    "fixture/DescribeWorkspaceSnapshots.yaml"

requestDescribeWorkspaceImagePermissions :: DescribeWorkspaceImagePermissions -> TestTree
requestDescribeWorkspaceImagePermissions =
  req
    "DescribeWorkspaceImagePermissions"
    "fixture/DescribeWorkspaceImagePermissions.yaml"

requestRebootWorkspaces :: RebootWorkspaces -> TestTree
requestRebootWorkspaces =
  req
    "RebootWorkspaces"
    "fixture/RebootWorkspaces.yaml"

requestDescribeAccount :: DescribeAccount -> TestTree
requestDescribeAccount =
  req
    "DescribeAccount"
    "fixture/DescribeAccount.yaml"

requestRevokeIpRules :: RevokeIpRules -> TestTree
requestRevokeIpRules =
  req
    "RevokeIpRules"
    "fixture/RevokeIpRules.yaml"

requestModifyWorkspaceProperties :: ModifyWorkspaceProperties -> TestTree
requestModifyWorkspaceProperties =
  req
    "ModifyWorkspaceProperties"
    "fixture/ModifyWorkspaceProperties.yaml"

requestDescribeWorkspaceImages :: DescribeWorkspaceImages -> TestTree
requestDescribeWorkspaceImages =
  req
    "DescribeWorkspaceImages"
    "fixture/DescribeWorkspaceImages.yaml"

requestDescribeAccountModifications :: DescribeAccountModifications -> TestTree
requestDescribeAccountModifications =
  req
    "DescribeAccountModifications"
    "fixture/DescribeAccountModifications.yaml"

requestAssociateIpGroups :: AssociateIpGroups -> TestTree
requestAssociateIpGroups =
  req
    "AssociateIpGroups"
    "fixture/AssociateIpGroups.yaml"

requestDeleteConnectionAlias :: DeleteConnectionAlias -> TestTree
requestDeleteConnectionAlias =
  req
    "DeleteConnectionAlias"
    "fixture/DeleteConnectionAlias.yaml"

requestStartWorkspaces :: StartWorkspaces -> TestTree
requestStartWorkspaces =
  req
    "StartWorkspaces"
    "fixture/StartWorkspaces.yaml"

requestStopWorkspaces :: StopWorkspaces -> TestTree
requestStopWorkspaces =
  req
    "StopWorkspaces"
    "fixture/StopWorkspaces.yaml"

requestDescribeWorkspaces :: DescribeWorkspaces -> TestTree
requestDescribeWorkspaces =
  req
    "DescribeWorkspaces"
    "fixture/DescribeWorkspaces.yaml"

requestModifyClientProperties :: ModifyClientProperties -> TestTree
requestModifyClientProperties =
  req
    "ModifyClientProperties"
    "fixture/ModifyClientProperties.yaml"

requestUpdateWorkspaceImagePermission :: UpdateWorkspaceImagePermission -> TestTree
requestUpdateWorkspaceImagePermission =
  req
    "UpdateWorkspaceImagePermission"
    "fixture/UpdateWorkspaceImagePermission.yaml"

requestDescribeIpGroups :: DescribeIpGroups -> TestTree
requestDescribeIpGroups =
  req
    "DescribeIpGroups"
    "fixture/DescribeIpGroups.yaml"

requestModifyWorkspaceAccessProperties :: ModifyWorkspaceAccessProperties -> TestTree
requestModifyWorkspaceAccessProperties =
  req
    "ModifyWorkspaceAccessProperties"
    "fixture/ModifyWorkspaceAccessProperties.yaml"

requestRestoreWorkspace :: RestoreWorkspace -> TestTree
requestRestoreWorkspace =
  req
    "RestoreWorkspace"
    "fixture/RestoreWorkspace.yaml"

requestRegisterWorkspaceDirectory :: RegisterWorkspaceDirectory -> TestTree
requestRegisterWorkspaceDirectory =
  req
    "RegisterWorkspaceDirectory"
    "fixture/RegisterWorkspaceDirectory.yaml"

requestCreateTags :: CreateTags -> TestTree
requestCreateTags =
  req
    "CreateTags"
    "fixture/CreateTags.yaml"

requestDescribeConnectionAliasPermissions :: DescribeConnectionAliasPermissions -> TestTree
requestDescribeConnectionAliasPermissions =
  req
    "DescribeConnectionAliasPermissions"
    "fixture/DescribeConnectionAliasPermissions.yaml"

requestCreateIpGroup :: CreateIpGroup -> TestTree
requestCreateIpGroup =
  req
    "CreateIpGroup"
    "fixture/CreateIpGroup.yaml"

requestDisassociateConnectionAlias :: DisassociateConnectionAlias -> TestTree
requestDisassociateConnectionAlias =
  req
    "DisassociateConnectionAlias"
    "fixture/DisassociateConnectionAlias.yaml"

requestCreateWorkspaces :: CreateWorkspaces -> TestTree
requestCreateWorkspaces =
  req
    "CreateWorkspaces"
    "fixture/CreateWorkspaces.yaml"

-- Responses

responseAuthorizeIpRules :: AuthorizeIpRulesResponse -> TestTree
responseAuthorizeIpRules =
  res
    "AuthorizeIpRulesResponse"
    "fixture/AuthorizeIpRulesResponse.proto"
    defaultService
    (Proxy :: Proxy AuthorizeIpRules)

responseImportWorkspaceImage :: ImportWorkspaceImageResponse -> TestTree
responseImportWorkspaceImage =
  res
    "ImportWorkspaceImageResponse"
    "fixture/ImportWorkspaceImageResponse.proto"
    defaultService
    (Proxy :: Proxy ImportWorkspaceImage)

responseDescribeWorkspaceDirectories :: DescribeWorkspaceDirectoriesResponse -> TestTree
responseDescribeWorkspaceDirectories =
  res
    "DescribeWorkspaceDirectoriesResponse"
    "fixture/DescribeWorkspaceDirectoriesResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeWorkspaceDirectories)

responseDescribeWorkspaceBundles :: DescribeWorkspaceBundlesResponse -> TestTree
responseDescribeWorkspaceBundles =
  res
    "DescribeWorkspaceBundlesResponse"
    "fixture/DescribeWorkspaceBundlesResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeWorkspaceBundles)

responseDeleteIpGroup :: DeleteIpGroupResponse -> TestTree
responseDeleteIpGroup =
  res
    "DeleteIpGroupResponse"
    "fixture/DeleteIpGroupResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteIpGroup)

responseTerminateWorkspaces :: TerminateWorkspacesResponse -> TestTree
responseTerminateWorkspaces =
  res
    "TerminateWorkspacesResponse"
    "fixture/TerminateWorkspacesResponse.proto"
    defaultService
    (Proxy :: Proxy TerminateWorkspaces)

responseDisassociateIpGroups :: DisassociateIpGroupsResponse -> TestTree
responseDisassociateIpGroups =
  res
    "DisassociateIpGroupsResponse"
    "fixture/DisassociateIpGroupsResponse.proto"
    defaultService
    (Proxy :: Proxy DisassociateIpGroups)

responseDeregisterWorkspaceDirectory :: DeregisterWorkspaceDirectoryResponse -> TestTree
responseDeregisterWorkspaceDirectory =
  res
    "DeregisterWorkspaceDirectoryResponse"
    "fixture/DeregisterWorkspaceDirectoryResponse.proto"
    defaultService
    (Proxy :: Proxy DeregisterWorkspaceDirectory)

responseAssociateConnectionAlias :: AssociateConnectionAliasResponse -> TestTree
responseAssociateConnectionAlias =
  res
    "AssociateConnectionAliasResponse"
    "fixture/AssociateConnectionAliasResponse.proto"
    defaultService
    (Proxy :: Proxy AssociateConnectionAlias)

responseCreateConnectionAlias :: CreateConnectionAliasResponse -> TestTree
responseCreateConnectionAlias =
  res
    "CreateConnectionAliasResponse"
    "fixture/CreateConnectionAliasResponse.proto"
    defaultService
    (Proxy :: Proxy CreateConnectionAlias)

responseMigrateWorkspace :: MigrateWorkspaceResponse -> TestTree
responseMigrateWorkspace =
  res
    "MigrateWorkspaceResponse"
    "fixture/MigrateWorkspaceResponse.proto"
    defaultService
    (Proxy :: Proxy MigrateWorkspace)

responseDescribeTags :: DescribeTagsResponse -> TestTree
responseDescribeTags =
  res
    "DescribeTagsResponse"
    "fixture/DescribeTagsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeTags)

responseModifySelfservicePermissions :: ModifySelfservicePermissionsResponse -> TestTree
responseModifySelfservicePermissions =
  res
    "ModifySelfservicePermissionsResponse"
    "fixture/ModifySelfservicePermissionsResponse.proto"
    defaultService
    (Proxy :: Proxy ModifySelfservicePermissions)

responseDescribeWorkspacesConnectionStatus :: DescribeWorkspacesConnectionStatusResponse -> TestTree
responseDescribeWorkspacesConnectionStatus =
  res
    "DescribeWorkspacesConnectionStatusResponse"
    "fixture/DescribeWorkspacesConnectionStatusResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeWorkspacesConnectionStatus)

responseModifyAccount :: ModifyAccountResponse -> TestTree
responseModifyAccount =
  res
    "ModifyAccountResponse"
    "fixture/ModifyAccountResponse.proto"
    defaultService
    (Proxy :: Proxy ModifyAccount)

responseDescribeConnectionAliases :: DescribeConnectionAliasesResponse -> TestTree
responseDescribeConnectionAliases =
  res
    "DescribeConnectionAliasesResponse"
    "fixture/DescribeConnectionAliasesResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeConnectionAliases)

responseDeleteWorkspaceImage :: DeleteWorkspaceImageResponse -> TestTree
responseDeleteWorkspaceImage =
  res
    "DeleteWorkspaceImageResponse"
    "fixture/DeleteWorkspaceImageResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteWorkspaceImage)

responseUpdateRulesOfIpGroup :: UpdateRulesOfIpGroupResponse -> TestTree
responseUpdateRulesOfIpGroup =
  res
    "UpdateRulesOfIpGroupResponse"
    "fixture/UpdateRulesOfIpGroupResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateRulesOfIpGroup)

responseUpdateWorkspaceBundle :: UpdateWorkspaceBundleResponse -> TestTree
responseUpdateWorkspaceBundle =
  res
    "UpdateWorkspaceBundleResponse"
    "fixture/UpdateWorkspaceBundleResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateWorkspaceBundle)

responseDeleteWorkspaceBundle :: DeleteWorkspaceBundleResponse -> TestTree
responseDeleteWorkspaceBundle =
  res
    "DeleteWorkspaceBundleResponse"
    "fixture/DeleteWorkspaceBundleResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteWorkspaceBundle)

responseDeleteTags :: DeleteTagsResponse -> TestTree
responseDeleteTags =
  res
    "DeleteTagsResponse"
    "fixture/DeleteTagsResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteTags)

responseListAvailableManagementCidrRanges :: ListAvailableManagementCidrRangesResponse -> TestTree
responseListAvailableManagementCidrRanges =
  res
    "ListAvailableManagementCidrRangesResponse"
    "fixture/ListAvailableManagementCidrRangesResponse.proto"
    defaultService
    (Proxy :: Proxy ListAvailableManagementCidrRanges)

responseDescribeClientProperties :: DescribeClientPropertiesResponse -> TestTree
responseDescribeClientProperties =
  res
    "DescribeClientPropertiesResponse"
    "fixture/DescribeClientPropertiesResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeClientProperties)

responseCreateWorkspaceBundle :: CreateWorkspaceBundleResponse -> TestTree
responseCreateWorkspaceBundle =
  res
    "CreateWorkspaceBundleResponse"
    "fixture/CreateWorkspaceBundleResponse.proto"
    defaultService
    (Proxy :: Proxy CreateWorkspaceBundle)

responseModifyWorkspaceCreationProperties :: ModifyWorkspaceCreationPropertiesResponse -> TestTree
responseModifyWorkspaceCreationProperties =
  res
    "ModifyWorkspaceCreationPropertiesResponse"
    "fixture/ModifyWorkspaceCreationPropertiesResponse.proto"
    defaultService
    (Proxy :: Proxy ModifyWorkspaceCreationProperties)

responseModifyWorkspaceState :: ModifyWorkspaceStateResponse -> TestTree
responseModifyWorkspaceState =
  res
    "ModifyWorkspaceStateResponse"
    "fixture/ModifyWorkspaceStateResponse.proto"
    defaultService
    (Proxy :: Proxy ModifyWorkspaceState)

responseUpdateConnectionAliasPermission :: UpdateConnectionAliasPermissionResponse -> TestTree
responseUpdateConnectionAliasPermission =
  res
    "UpdateConnectionAliasPermissionResponse"
    "fixture/UpdateConnectionAliasPermissionResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateConnectionAliasPermission)

responseCopyWorkspaceImage :: CopyWorkspaceImageResponse -> TestTree
responseCopyWorkspaceImage =
  res
    "CopyWorkspaceImageResponse"
    "fixture/CopyWorkspaceImageResponse.proto"
    defaultService
    (Proxy :: Proxy CopyWorkspaceImage)

responseRebuildWorkspaces :: RebuildWorkspacesResponse -> TestTree
responseRebuildWorkspaces =
  res
    "RebuildWorkspacesResponse"
    "fixture/RebuildWorkspacesResponse.proto"
    defaultService
    (Proxy :: Proxy RebuildWorkspaces)

responseDescribeWorkspaceSnapshots :: DescribeWorkspaceSnapshotsResponse -> TestTree
responseDescribeWorkspaceSnapshots =
  res
    "DescribeWorkspaceSnapshotsResponse"
    "fixture/DescribeWorkspaceSnapshotsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeWorkspaceSnapshots)

responseDescribeWorkspaceImagePermissions :: DescribeWorkspaceImagePermissionsResponse -> TestTree
responseDescribeWorkspaceImagePermissions =
  res
    "DescribeWorkspaceImagePermissionsResponse"
    "fixture/DescribeWorkspaceImagePermissionsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeWorkspaceImagePermissions)

responseRebootWorkspaces :: RebootWorkspacesResponse -> TestTree
responseRebootWorkspaces =
  res
    "RebootWorkspacesResponse"
    "fixture/RebootWorkspacesResponse.proto"
    defaultService
    (Proxy :: Proxy RebootWorkspaces)

responseDescribeAccount :: DescribeAccountResponse -> TestTree
responseDescribeAccount =
  res
    "DescribeAccountResponse"
    "fixture/DescribeAccountResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeAccount)

responseRevokeIpRules :: RevokeIpRulesResponse -> TestTree
responseRevokeIpRules =
  res
    "RevokeIpRulesResponse"
    "fixture/RevokeIpRulesResponse.proto"
    defaultService
    (Proxy :: Proxy RevokeIpRules)

responseModifyWorkspaceProperties :: ModifyWorkspacePropertiesResponse -> TestTree
responseModifyWorkspaceProperties =
  res
    "ModifyWorkspacePropertiesResponse"
    "fixture/ModifyWorkspacePropertiesResponse.proto"
    defaultService
    (Proxy :: Proxy ModifyWorkspaceProperties)

responseDescribeWorkspaceImages :: DescribeWorkspaceImagesResponse -> TestTree
responseDescribeWorkspaceImages =
  res
    "DescribeWorkspaceImagesResponse"
    "fixture/DescribeWorkspaceImagesResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeWorkspaceImages)

responseDescribeAccountModifications :: DescribeAccountModificationsResponse -> TestTree
responseDescribeAccountModifications =
  res
    "DescribeAccountModificationsResponse"
    "fixture/DescribeAccountModificationsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeAccountModifications)

responseAssociateIpGroups :: AssociateIpGroupsResponse -> TestTree
responseAssociateIpGroups =
  res
    "AssociateIpGroupsResponse"
    "fixture/AssociateIpGroupsResponse.proto"
    defaultService
    (Proxy :: Proxy AssociateIpGroups)

responseDeleteConnectionAlias :: DeleteConnectionAliasResponse -> TestTree
responseDeleteConnectionAlias =
  res
    "DeleteConnectionAliasResponse"
    "fixture/DeleteConnectionAliasResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteConnectionAlias)

responseStartWorkspaces :: StartWorkspacesResponse -> TestTree
responseStartWorkspaces =
  res
    "StartWorkspacesResponse"
    "fixture/StartWorkspacesResponse.proto"
    defaultService
    (Proxy :: Proxy StartWorkspaces)

responseStopWorkspaces :: StopWorkspacesResponse -> TestTree
responseStopWorkspaces =
  res
    "StopWorkspacesResponse"
    "fixture/StopWorkspacesResponse.proto"
    defaultService
    (Proxy :: Proxy StopWorkspaces)

responseDescribeWorkspaces :: DescribeWorkspacesResponse -> TestTree
responseDescribeWorkspaces =
  res
    "DescribeWorkspacesResponse"
    "fixture/DescribeWorkspacesResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeWorkspaces)

responseModifyClientProperties :: ModifyClientPropertiesResponse -> TestTree
responseModifyClientProperties =
  res
    "ModifyClientPropertiesResponse"
    "fixture/ModifyClientPropertiesResponse.proto"
    defaultService
    (Proxy :: Proxy ModifyClientProperties)

responseUpdateWorkspaceImagePermission :: UpdateWorkspaceImagePermissionResponse -> TestTree
responseUpdateWorkspaceImagePermission =
  res
    "UpdateWorkspaceImagePermissionResponse"
    "fixture/UpdateWorkspaceImagePermissionResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateWorkspaceImagePermission)

responseDescribeIpGroups :: DescribeIpGroupsResponse -> TestTree
responseDescribeIpGroups =
  res
    "DescribeIpGroupsResponse"
    "fixture/DescribeIpGroupsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeIpGroups)

responseModifyWorkspaceAccessProperties :: ModifyWorkspaceAccessPropertiesResponse -> TestTree
responseModifyWorkspaceAccessProperties =
  res
    "ModifyWorkspaceAccessPropertiesResponse"
    "fixture/ModifyWorkspaceAccessPropertiesResponse.proto"
    defaultService
    (Proxy :: Proxy ModifyWorkspaceAccessProperties)

responseRestoreWorkspace :: RestoreWorkspaceResponse -> TestTree
responseRestoreWorkspace =
  res
    "RestoreWorkspaceResponse"
    "fixture/RestoreWorkspaceResponse.proto"
    defaultService
    (Proxy :: Proxy RestoreWorkspace)

responseRegisterWorkspaceDirectory :: RegisterWorkspaceDirectoryResponse -> TestTree
responseRegisterWorkspaceDirectory =
  res
    "RegisterWorkspaceDirectoryResponse"
    "fixture/RegisterWorkspaceDirectoryResponse.proto"
    defaultService
    (Proxy :: Proxy RegisterWorkspaceDirectory)

responseCreateTags :: CreateTagsResponse -> TestTree
responseCreateTags =
  res
    "CreateTagsResponse"
    "fixture/CreateTagsResponse.proto"
    defaultService
    (Proxy :: Proxy CreateTags)

responseDescribeConnectionAliasPermissions :: DescribeConnectionAliasPermissionsResponse -> TestTree
responseDescribeConnectionAliasPermissions =
  res
    "DescribeConnectionAliasPermissionsResponse"
    "fixture/DescribeConnectionAliasPermissionsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeConnectionAliasPermissions)

responseCreateIpGroup :: CreateIpGroupResponse -> TestTree
responseCreateIpGroup =
  res
    "CreateIpGroupResponse"
    "fixture/CreateIpGroupResponse.proto"
    defaultService
    (Proxy :: Proxy CreateIpGroup)

responseDisassociateConnectionAlias :: DisassociateConnectionAliasResponse -> TestTree
responseDisassociateConnectionAlias =
  res
    "DisassociateConnectionAliasResponse"
    "fixture/DisassociateConnectionAliasResponse.proto"
    defaultService
    (Proxy :: Proxy DisassociateConnectionAlias)

responseCreateWorkspaces :: CreateWorkspacesResponse -> TestTree
responseCreateWorkspaces =
  res
    "CreateWorkspacesResponse"
    "fixture/CreateWorkspacesResponse.proto"
    defaultService
    (Proxy :: Proxy CreateWorkspaces)
