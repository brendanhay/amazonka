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
--         [ requestDescribeWorkspaceDirectories $
--             newDescribeWorkspaceDirectories
--
--         , requestTerminateWorkspaces $
--             newTerminateWorkspaces
--
--         , requestDisassociateIpGroups $
--             newDisassociateIpGroups
--
--         , requestDescribeWorkspaceBundles $
--             newDescribeWorkspaceBundles
--
--         , requestAuthorizeIpRules $
--             newAuthorizeIpRules
--
--         , requestImportWorkspaceImage $
--             newImportWorkspaceImage
--
--         , requestDeleteIpGroup $
--             newDeleteIpGroup
--
--         , requestDeregisterWorkspaceDirectory $
--             newDeregisterWorkspaceDirectory
--
--         , requestAssociateConnectionAlias $
--             newAssociateConnectionAlias
--
--         , requestDescribeTags $
--             newDescribeTags
--
--         , requestCreateConnectionAlias $
--             newCreateConnectionAlias
--
--         , requestMigrateWorkspace $
--             newMigrateWorkspace
--
--         , requestModifyAccount $
--             newModifyAccount
--
--         , requestDescribeWorkspacesConnectionStatus $
--             newDescribeWorkspacesConnectionStatus
--
--         , requestModifySelfservicePermissions $
--             newModifySelfservicePermissions
--
--         , requestUpdateRulesOfIpGroup $
--             newUpdateRulesOfIpGroup
--
--         , requestDescribeConnectionAliases $
--             newDescribeConnectionAliases
--
--         , requestDeleteWorkspaceImage $
--             newDeleteWorkspaceImage
--
--         , requestDeleteTags $
--             newDeleteTags
--
--         , requestListAvailableManagementCidrRanges $
--             newListAvailableManagementCidrRanges
--
--         , requestModifyWorkspaceCreationProperties $
--             newModifyWorkspaceCreationProperties
--
--         , requestDescribeClientProperties $
--             newDescribeClientProperties
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
--         , requestDescribeWorkspaceImagePermissions $
--             newDescribeWorkspaceImagePermissions
--
--         , requestRebuildWorkspaces $
--             newRebuildWorkspaces
--
--         , requestRebootWorkspaces $
--             newRebootWorkspaces
--
--         , requestDescribeWorkspaceSnapshots $
--             newDescribeWorkspaceSnapshots
--
--         , requestDescribeAccount $
--             newDescribeAccount
--
--         , requestModifyWorkspaceProperties $
--             newModifyWorkspaceProperties
--
--         , requestRevokeIpRules $
--             newRevokeIpRules
--
--         , requestDescribeWorkspaceImages $
--             newDescribeWorkspaceImages
--
--         , requestDescribeAccountModifications $
--             newDescribeAccountModifications
--
--         , requestDeleteConnectionAlias $
--             newDeleteConnectionAlias
--
--         , requestAssociateIpGroups $
--             newAssociateIpGroups
--
--         , requestStopWorkspaces $
--             newStopWorkspaces
--
--         , requestStartWorkspaces $
--             newStartWorkspaces
--
--         , requestDescribeWorkspaces $
--             newDescribeWorkspaces
--
--         , requestUpdateWorkspaceImagePermission $
--             newUpdateWorkspaceImagePermission
--
--         , requestModifyClientProperties $
--             newModifyClientProperties
--
--         , requestModifyWorkspaceAccessProperties $
--             newModifyWorkspaceAccessProperties
--
--         , requestDescribeIpGroups $
--             newDescribeIpGroups
--
--         , requestRestoreWorkspace $
--             newRestoreWorkspace
--
--         , requestCreateTags $
--             newCreateTags
--
--         , requestDescribeConnectionAliasPermissions $
--             newDescribeConnectionAliasPermissions
--
--         , requestRegisterWorkspaceDirectory $
--             newRegisterWorkspaceDirectory
--
--         , requestCreateWorkspaces $
--             newCreateWorkspaces
--
--         , requestCreateIpGroup $
--             newCreateIpGroup
--
--         , requestDisassociateConnectionAlias $
--             newDisassociateConnectionAlias
--
--           ]

--     , testGroup "response"
--         [ responseDescribeWorkspaceDirectories $
--             newDescribeWorkspaceDirectoriesResponse
--
--         , responseTerminateWorkspaces $
--             newTerminateWorkspacesResponse
--
--         , responseDisassociateIpGroups $
--             newDisassociateIpGroupsResponse
--
--         , responseDescribeWorkspaceBundles $
--             newDescribeWorkspaceBundlesResponse
--
--         , responseAuthorizeIpRules $
--             newAuthorizeIpRulesResponse
--
--         , responseImportWorkspaceImage $
--             newImportWorkspaceImageResponse
--
--         , responseDeleteIpGroup $
--             newDeleteIpGroupResponse
--
--         , responseDeregisterWorkspaceDirectory $
--             newDeregisterWorkspaceDirectoryResponse
--
--         , responseAssociateConnectionAlias $
--             newAssociateConnectionAliasResponse
--
--         , responseDescribeTags $
--             newDescribeTagsResponse
--
--         , responseCreateConnectionAlias $
--             newCreateConnectionAliasResponse
--
--         , responseMigrateWorkspace $
--             newMigrateWorkspaceResponse
--
--         , responseModifyAccount $
--             newModifyAccountResponse
--
--         , responseDescribeWorkspacesConnectionStatus $
--             newDescribeWorkspacesConnectionStatusResponse
--
--         , responseModifySelfservicePermissions $
--             newModifySelfservicePermissionsResponse
--
--         , responseUpdateRulesOfIpGroup $
--             newUpdateRulesOfIpGroupResponse
--
--         , responseDescribeConnectionAliases $
--             newDescribeConnectionAliasesResponse
--
--         , responseDeleteWorkspaceImage $
--             newDeleteWorkspaceImageResponse
--
--         , responseDeleteTags $
--             newDeleteTagsResponse
--
--         , responseListAvailableManagementCidrRanges $
--             newListAvailableManagementCidrRangesResponse
--
--         , responseModifyWorkspaceCreationProperties $
--             newModifyWorkspaceCreationPropertiesResponse
--
--         , responseDescribeClientProperties $
--             newDescribeClientPropertiesResponse
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
--         , responseDescribeWorkspaceImagePermissions $
--             newDescribeWorkspaceImagePermissionsResponse
--
--         , responseRebuildWorkspaces $
--             newRebuildWorkspacesResponse
--
--         , responseRebootWorkspaces $
--             newRebootWorkspacesResponse
--
--         , responseDescribeWorkspaceSnapshots $
--             newDescribeWorkspaceSnapshotsResponse
--
--         , responseDescribeAccount $
--             newDescribeAccountResponse
--
--         , responseModifyWorkspaceProperties $
--             newModifyWorkspacePropertiesResponse
--
--         , responseRevokeIpRules $
--             newRevokeIpRulesResponse
--
--         , responseDescribeWorkspaceImages $
--             newDescribeWorkspaceImagesResponse
--
--         , responseDescribeAccountModifications $
--             newDescribeAccountModificationsResponse
--
--         , responseDeleteConnectionAlias $
--             newDeleteConnectionAliasResponse
--
--         , responseAssociateIpGroups $
--             newAssociateIpGroupsResponse
--
--         , responseStopWorkspaces $
--             newStopWorkspacesResponse
--
--         , responseStartWorkspaces $
--             newStartWorkspacesResponse
--
--         , responseDescribeWorkspaces $
--             newDescribeWorkspacesResponse
--
--         , responseUpdateWorkspaceImagePermission $
--             newUpdateWorkspaceImagePermissionResponse
--
--         , responseModifyClientProperties $
--             newModifyClientPropertiesResponse
--
--         , responseModifyWorkspaceAccessProperties $
--             newModifyWorkspaceAccessPropertiesResponse
--
--         , responseDescribeIpGroups $
--             newDescribeIpGroupsResponse
--
--         , responseRestoreWorkspace $
--             newRestoreWorkspaceResponse
--
--         , responseCreateTags $
--             newCreateTagsResponse
--
--         , responseDescribeConnectionAliasPermissions $
--             newDescribeConnectionAliasPermissionsResponse
--
--         , responseRegisterWorkspaceDirectory $
--             newRegisterWorkspaceDirectoryResponse
--
--         , responseCreateWorkspaces $
--             newCreateWorkspacesResponse
--
--         , responseCreateIpGroup $
--             newCreateIpGroupResponse
--
--         , responseDisassociateConnectionAlias $
--             newDisassociateConnectionAliasResponse
--
--           ]
--     ]

-- Requests

requestDescribeWorkspaceDirectories :: DescribeWorkspaceDirectories -> TestTree
requestDescribeWorkspaceDirectories =
  req
    "DescribeWorkspaceDirectories"
    "fixture/DescribeWorkspaceDirectories.yaml"

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

requestDescribeWorkspaceBundles :: DescribeWorkspaceBundles -> TestTree
requestDescribeWorkspaceBundles =
  req
    "DescribeWorkspaceBundles"
    "fixture/DescribeWorkspaceBundles.yaml"

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

requestDeleteIpGroup :: DeleteIpGroup -> TestTree
requestDeleteIpGroup =
  req
    "DeleteIpGroup"
    "fixture/DeleteIpGroup.yaml"

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

requestDescribeTags :: DescribeTags -> TestTree
requestDescribeTags =
  req
    "DescribeTags"
    "fixture/DescribeTags.yaml"

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

requestModifyAccount :: ModifyAccount -> TestTree
requestModifyAccount =
  req
    "ModifyAccount"
    "fixture/ModifyAccount.yaml"

requestDescribeWorkspacesConnectionStatus :: DescribeWorkspacesConnectionStatus -> TestTree
requestDescribeWorkspacesConnectionStatus =
  req
    "DescribeWorkspacesConnectionStatus"
    "fixture/DescribeWorkspacesConnectionStatus.yaml"

requestModifySelfservicePermissions :: ModifySelfservicePermissions -> TestTree
requestModifySelfservicePermissions =
  req
    "ModifySelfservicePermissions"
    "fixture/ModifySelfservicePermissions.yaml"

requestUpdateRulesOfIpGroup :: UpdateRulesOfIpGroup -> TestTree
requestUpdateRulesOfIpGroup =
  req
    "UpdateRulesOfIpGroup"
    "fixture/UpdateRulesOfIpGroup.yaml"

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

requestModifyWorkspaceCreationProperties :: ModifyWorkspaceCreationProperties -> TestTree
requestModifyWorkspaceCreationProperties =
  req
    "ModifyWorkspaceCreationProperties"
    "fixture/ModifyWorkspaceCreationProperties.yaml"

requestDescribeClientProperties :: DescribeClientProperties -> TestTree
requestDescribeClientProperties =
  req
    "DescribeClientProperties"
    "fixture/DescribeClientProperties.yaml"

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

requestDescribeWorkspaceImagePermissions :: DescribeWorkspaceImagePermissions -> TestTree
requestDescribeWorkspaceImagePermissions =
  req
    "DescribeWorkspaceImagePermissions"
    "fixture/DescribeWorkspaceImagePermissions.yaml"

requestRebuildWorkspaces :: RebuildWorkspaces -> TestTree
requestRebuildWorkspaces =
  req
    "RebuildWorkspaces"
    "fixture/RebuildWorkspaces.yaml"

requestRebootWorkspaces :: RebootWorkspaces -> TestTree
requestRebootWorkspaces =
  req
    "RebootWorkspaces"
    "fixture/RebootWorkspaces.yaml"

requestDescribeWorkspaceSnapshots :: DescribeWorkspaceSnapshots -> TestTree
requestDescribeWorkspaceSnapshots =
  req
    "DescribeWorkspaceSnapshots"
    "fixture/DescribeWorkspaceSnapshots.yaml"

requestDescribeAccount :: DescribeAccount -> TestTree
requestDescribeAccount =
  req
    "DescribeAccount"
    "fixture/DescribeAccount.yaml"

requestModifyWorkspaceProperties :: ModifyWorkspaceProperties -> TestTree
requestModifyWorkspaceProperties =
  req
    "ModifyWorkspaceProperties"
    "fixture/ModifyWorkspaceProperties.yaml"

requestRevokeIpRules :: RevokeIpRules -> TestTree
requestRevokeIpRules =
  req
    "RevokeIpRules"
    "fixture/RevokeIpRules.yaml"

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

requestDeleteConnectionAlias :: DeleteConnectionAlias -> TestTree
requestDeleteConnectionAlias =
  req
    "DeleteConnectionAlias"
    "fixture/DeleteConnectionAlias.yaml"

requestAssociateIpGroups :: AssociateIpGroups -> TestTree
requestAssociateIpGroups =
  req
    "AssociateIpGroups"
    "fixture/AssociateIpGroups.yaml"

requestStopWorkspaces :: StopWorkspaces -> TestTree
requestStopWorkspaces =
  req
    "StopWorkspaces"
    "fixture/StopWorkspaces.yaml"

requestStartWorkspaces :: StartWorkspaces -> TestTree
requestStartWorkspaces =
  req
    "StartWorkspaces"
    "fixture/StartWorkspaces.yaml"

requestDescribeWorkspaces :: DescribeWorkspaces -> TestTree
requestDescribeWorkspaces =
  req
    "DescribeWorkspaces"
    "fixture/DescribeWorkspaces.yaml"

requestUpdateWorkspaceImagePermission :: UpdateWorkspaceImagePermission -> TestTree
requestUpdateWorkspaceImagePermission =
  req
    "UpdateWorkspaceImagePermission"
    "fixture/UpdateWorkspaceImagePermission.yaml"

requestModifyClientProperties :: ModifyClientProperties -> TestTree
requestModifyClientProperties =
  req
    "ModifyClientProperties"
    "fixture/ModifyClientProperties.yaml"

requestModifyWorkspaceAccessProperties :: ModifyWorkspaceAccessProperties -> TestTree
requestModifyWorkspaceAccessProperties =
  req
    "ModifyWorkspaceAccessProperties"
    "fixture/ModifyWorkspaceAccessProperties.yaml"

requestDescribeIpGroups :: DescribeIpGroups -> TestTree
requestDescribeIpGroups =
  req
    "DescribeIpGroups"
    "fixture/DescribeIpGroups.yaml"

requestRestoreWorkspace :: RestoreWorkspace -> TestTree
requestRestoreWorkspace =
  req
    "RestoreWorkspace"
    "fixture/RestoreWorkspace.yaml"

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

requestRegisterWorkspaceDirectory :: RegisterWorkspaceDirectory -> TestTree
requestRegisterWorkspaceDirectory =
  req
    "RegisterWorkspaceDirectory"
    "fixture/RegisterWorkspaceDirectory.yaml"

requestCreateWorkspaces :: CreateWorkspaces -> TestTree
requestCreateWorkspaces =
  req
    "CreateWorkspaces"
    "fixture/CreateWorkspaces.yaml"

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

-- Responses

responseDescribeWorkspaceDirectories :: DescribeWorkspaceDirectoriesResponse -> TestTree
responseDescribeWorkspaceDirectories =
  res
    "DescribeWorkspaceDirectoriesResponse"
    "fixture/DescribeWorkspaceDirectoriesResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeWorkspaceDirectories)

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

responseDescribeWorkspaceBundles :: DescribeWorkspaceBundlesResponse -> TestTree
responseDescribeWorkspaceBundles =
  res
    "DescribeWorkspaceBundlesResponse"
    "fixture/DescribeWorkspaceBundlesResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeWorkspaceBundles)

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

responseDeleteIpGroup :: DeleteIpGroupResponse -> TestTree
responseDeleteIpGroup =
  res
    "DeleteIpGroupResponse"
    "fixture/DeleteIpGroupResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteIpGroup)

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

responseDescribeTags :: DescribeTagsResponse -> TestTree
responseDescribeTags =
  res
    "DescribeTagsResponse"
    "fixture/DescribeTagsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeTags)

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

responseModifyAccount :: ModifyAccountResponse -> TestTree
responseModifyAccount =
  res
    "ModifyAccountResponse"
    "fixture/ModifyAccountResponse.proto"
    defaultService
    (Proxy :: Proxy ModifyAccount)

responseDescribeWorkspacesConnectionStatus :: DescribeWorkspacesConnectionStatusResponse -> TestTree
responseDescribeWorkspacesConnectionStatus =
  res
    "DescribeWorkspacesConnectionStatusResponse"
    "fixture/DescribeWorkspacesConnectionStatusResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeWorkspacesConnectionStatus)

responseModifySelfservicePermissions :: ModifySelfservicePermissionsResponse -> TestTree
responseModifySelfservicePermissions =
  res
    "ModifySelfservicePermissionsResponse"
    "fixture/ModifySelfservicePermissionsResponse.proto"
    defaultService
    (Proxy :: Proxy ModifySelfservicePermissions)

responseUpdateRulesOfIpGroup :: UpdateRulesOfIpGroupResponse -> TestTree
responseUpdateRulesOfIpGroup =
  res
    "UpdateRulesOfIpGroupResponse"
    "fixture/UpdateRulesOfIpGroupResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateRulesOfIpGroup)

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

responseModifyWorkspaceCreationProperties :: ModifyWorkspaceCreationPropertiesResponse -> TestTree
responseModifyWorkspaceCreationProperties =
  res
    "ModifyWorkspaceCreationPropertiesResponse"
    "fixture/ModifyWorkspaceCreationPropertiesResponse.proto"
    defaultService
    (Proxy :: Proxy ModifyWorkspaceCreationProperties)

responseDescribeClientProperties :: DescribeClientPropertiesResponse -> TestTree
responseDescribeClientProperties =
  res
    "DescribeClientPropertiesResponse"
    "fixture/DescribeClientPropertiesResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeClientProperties)

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

responseDescribeWorkspaceImagePermissions :: DescribeWorkspaceImagePermissionsResponse -> TestTree
responseDescribeWorkspaceImagePermissions =
  res
    "DescribeWorkspaceImagePermissionsResponse"
    "fixture/DescribeWorkspaceImagePermissionsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeWorkspaceImagePermissions)

responseRebuildWorkspaces :: RebuildWorkspacesResponse -> TestTree
responseRebuildWorkspaces =
  res
    "RebuildWorkspacesResponse"
    "fixture/RebuildWorkspacesResponse.proto"
    defaultService
    (Proxy :: Proxy RebuildWorkspaces)

responseRebootWorkspaces :: RebootWorkspacesResponse -> TestTree
responseRebootWorkspaces =
  res
    "RebootWorkspacesResponse"
    "fixture/RebootWorkspacesResponse.proto"
    defaultService
    (Proxy :: Proxy RebootWorkspaces)

responseDescribeWorkspaceSnapshots :: DescribeWorkspaceSnapshotsResponse -> TestTree
responseDescribeWorkspaceSnapshots =
  res
    "DescribeWorkspaceSnapshotsResponse"
    "fixture/DescribeWorkspaceSnapshotsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeWorkspaceSnapshots)

responseDescribeAccount :: DescribeAccountResponse -> TestTree
responseDescribeAccount =
  res
    "DescribeAccountResponse"
    "fixture/DescribeAccountResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeAccount)

responseModifyWorkspaceProperties :: ModifyWorkspacePropertiesResponse -> TestTree
responseModifyWorkspaceProperties =
  res
    "ModifyWorkspacePropertiesResponse"
    "fixture/ModifyWorkspacePropertiesResponse.proto"
    defaultService
    (Proxy :: Proxy ModifyWorkspaceProperties)

responseRevokeIpRules :: RevokeIpRulesResponse -> TestTree
responseRevokeIpRules =
  res
    "RevokeIpRulesResponse"
    "fixture/RevokeIpRulesResponse.proto"
    defaultService
    (Proxy :: Proxy RevokeIpRules)

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

responseDeleteConnectionAlias :: DeleteConnectionAliasResponse -> TestTree
responseDeleteConnectionAlias =
  res
    "DeleteConnectionAliasResponse"
    "fixture/DeleteConnectionAliasResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteConnectionAlias)

responseAssociateIpGroups :: AssociateIpGroupsResponse -> TestTree
responseAssociateIpGroups =
  res
    "AssociateIpGroupsResponse"
    "fixture/AssociateIpGroupsResponse.proto"
    defaultService
    (Proxy :: Proxy AssociateIpGroups)

responseStopWorkspaces :: StopWorkspacesResponse -> TestTree
responseStopWorkspaces =
  res
    "StopWorkspacesResponse"
    "fixture/StopWorkspacesResponse.proto"
    defaultService
    (Proxy :: Proxy StopWorkspaces)

responseStartWorkspaces :: StartWorkspacesResponse -> TestTree
responseStartWorkspaces =
  res
    "StartWorkspacesResponse"
    "fixture/StartWorkspacesResponse.proto"
    defaultService
    (Proxy :: Proxy StartWorkspaces)

responseDescribeWorkspaces :: DescribeWorkspacesResponse -> TestTree
responseDescribeWorkspaces =
  res
    "DescribeWorkspacesResponse"
    "fixture/DescribeWorkspacesResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeWorkspaces)

responseUpdateWorkspaceImagePermission :: UpdateWorkspaceImagePermissionResponse -> TestTree
responseUpdateWorkspaceImagePermission =
  res
    "UpdateWorkspaceImagePermissionResponse"
    "fixture/UpdateWorkspaceImagePermissionResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateWorkspaceImagePermission)

responseModifyClientProperties :: ModifyClientPropertiesResponse -> TestTree
responseModifyClientProperties =
  res
    "ModifyClientPropertiesResponse"
    "fixture/ModifyClientPropertiesResponse.proto"
    defaultService
    (Proxy :: Proxy ModifyClientProperties)

responseModifyWorkspaceAccessProperties :: ModifyWorkspaceAccessPropertiesResponse -> TestTree
responseModifyWorkspaceAccessProperties =
  res
    "ModifyWorkspaceAccessPropertiesResponse"
    "fixture/ModifyWorkspaceAccessPropertiesResponse.proto"
    defaultService
    (Proxy :: Proxy ModifyWorkspaceAccessProperties)

responseDescribeIpGroups :: DescribeIpGroupsResponse -> TestTree
responseDescribeIpGroups =
  res
    "DescribeIpGroupsResponse"
    "fixture/DescribeIpGroupsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeIpGroups)

responseRestoreWorkspace :: RestoreWorkspaceResponse -> TestTree
responseRestoreWorkspace =
  res
    "RestoreWorkspaceResponse"
    "fixture/RestoreWorkspaceResponse.proto"
    defaultService
    (Proxy :: Proxy RestoreWorkspace)

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

responseRegisterWorkspaceDirectory :: RegisterWorkspaceDirectoryResponse -> TestTree
responseRegisterWorkspaceDirectory =
  res
    "RegisterWorkspaceDirectoryResponse"
    "fixture/RegisterWorkspaceDirectoryResponse.proto"
    defaultService
    (Proxy :: Proxy RegisterWorkspaceDirectory)

responseCreateWorkspaces :: CreateWorkspacesResponse -> TestTree
responseCreateWorkspaces =
  res
    "CreateWorkspacesResponse"
    "fixture/CreateWorkspacesResponse.proto"
    defaultService
    (Proxy :: Proxy CreateWorkspaces)

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
