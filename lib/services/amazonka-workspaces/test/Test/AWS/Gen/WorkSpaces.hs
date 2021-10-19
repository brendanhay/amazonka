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
--         [ requestAssociateConnectionAlias $
--             newAssociateConnectionAlias
--
--         , requestDescribeAccount $
--             newDescribeAccount
--
--         , requestRevokeIpRules $
--             newRevokeIpRules
--
--         , requestDescribeWorkspaceImages $
--             newDescribeWorkspaceImages
--
--         , requestModifyWorkspaceProperties $
--             newModifyWorkspaceProperties
--
--         , requestDeregisterWorkspaceDirectory $
--             newDeregisterWorkspaceDirectory
--
--         , requestMigrateWorkspace $
--             newMigrateWorkspace
--
--         , requestDescribeTags $
--             newDescribeTags
--
--         , requestDescribeWorkspaceDirectories $
--             newDescribeWorkspaceDirectories
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
--         , requestDescribeWorkspaceImagePermissions $
--             newDescribeWorkspaceImagePermissions
--
--         , requestRebuildWorkspaces $
--             newRebuildWorkspaces
--
--         , requestImportWorkspaceImage $
--             newImportWorkspaceImage
--
--         , requestModifyWorkspaceState $
--             newModifyWorkspaceState
--
--         , requestCreateIpGroup $
--             newCreateIpGroup
--
--         , requestDisassociateConnectionAlias $
--             newDisassociateConnectionAlias
--
--         , requestModifyWorkspaceCreationProperties $
--             newModifyWorkspaceCreationProperties
--
--         , requestRegisterWorkspaceDirectory $
--             newRegisterWorkspaceDirectory
--
--         , requestRestoreWorkspace $
--             newRestoreWorkspace
--
--         , requestDescribeConnectionAliasPermissions $
--             newDescribeConnectionAliasPermissions
--
--         , requestCreateTags $
--             newCreateTags
--
--         , requestCreateWorkspaceBundle $
--             newCreateWorkspaceBundle
--
--         , requestDeleteTags $
--             newDeleteTags
--
--         , requestModifyWorkspaceAccessProperties $
--             newModifyWorkspaceAccessProperties
--
--         , requestUpdateRulesOfIpGroup $
--             newUpdateRulesOfIpGroup
--
--         , requestDeleteWorkspaceImage $
--             newDeleteWorkspaceImage
--
--         , requestStopWorkspaces $
--             newStopWorkspaces
--
--         , requestAssociateIpGroups $
--             newAssociateIpGroups
--
--         , requestModifySelfservicePermissions $
--             newModifySelfservicePermissions
--
--         , requestDeleteConnectionAlias $
--             newDeleteConnectionAlias
--
--         , requestDescribeWorkspacesConnectionStatus $
--             newDescribeWorkspacesConnectionStatus
--
--         , requestCreateConnectionAlias $
--             newCreateConnectionAlias
--
--         , requestRebootWorkspaces $
--             newRebootWorkspaces
--
--         , requestDeleteIpGroup $
--             newDeleteIpGroup
--
--         , requestCopyWorkspaceImage $
--             newCopyWorkspaceImage
--
--         , requestDescribeWorkspaceSnapshots $
--             newDescribeWorkspaceSnapshots
--
--         , requestTerminateWorkspaces $
--             newTerminateWorkspaces
--
--         , requestUpdateConnectionAliasPermission $
--             newUpdateConnectionAliasPermission
--
--         , requestCreateWorkspaces $
--             newCreateWorkspaces
--
--         , requestDescribeClientProperties $
--             newDescribeClientProperties
--
--         , requestModifyClientProperties $
--             newModifyClientProperties
--
--         , requestDescribeIpGroups $
--             newDescribeIpGroups
--
--         , requestDeleteWorkspaceBundle $
--             newDeleteWorkspaceBundle
--
--         , requestUpdateWorkspaceBundle $
--             newUpdateWorkspaceBundle
--
--         , requestListAvailableManagementCidrRanges $
--             newListAvailableManagementCidrRanges
--
--         , requestUpdateWorkspaceImagePermission $
--             newUpdateWorkspaceImagePermission
--
--         , requestCreateUpdatedWorkspaceImage $
--             newCreateUpdatedWorkspaceImage
--
--         , requestDescribeWorkspaces $
--             newDescribeWorkspaces
--
--         , requestDescribeConnectionAliases $
--             newDescribeConnectionAliases
--
--         , requestStartWorkspaces $
--             newStartWorkspaces
--
--         , requestDescribeAccountModifications $
--             newDescribeAccountModifications
--
--         , requestModifyAccount $
--             newModifyAccount
--
--           ]

--     , testGroup "response"
--         [ responseAssociateConnectionAlias $
--             newAssociateConnectionAliasResponse
--
--         , responseDescribeAccount $
--             newDescribeAccountResponse
--
--         , responseRevokeIpRules $
--             newRevokeIpRulesResponse
--
--         , responseDescribeWorkspaceImages $
--             newDescribeWorkspaceImagesResponse
--
--         , responseModifyWorkspaceProperties $
--             newModifyWorkspacePropertiesResponse
--
--         , responseDeregisterWorkspaceDirectory $
--             newDeregisterWorkspaceDirectoryResponse
--
--         , responseMigrateWorkspace $
--             newMigrateWorkspaceResponse
--
--         , responseDescribeTags $
--             newDescribeTagsResponse
--
--         , responseDescribeWorkspaceDirectories $
--             newDescribeWorkspaceDirectoriesResponse
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
--         , responseDescribeWorkspaceImagePermissions $
--             newDescribeWorkspaceImagePermissionsResponse
--
--         , responseRebuildWorkspaces $
--             newRebuildWorkspacesResponse
--
--         , responseImportWorkspaceImage $
--             newImportWorkspaceImageResponse
--
--         , responseModifyWorkspaceState $
--             newModifyWorkspaceStateResponse
--
--         , responseCreateIpGroup $
--             newCreateIpGroupResponse
--
--         , responseDisassociateConnectionAlias $
--             newDisassociateConnectionAliasResponse
--
--         , responseModifyWorkspaceCreationProperties $
--             newModifyWorkspaceCreationPropertiesResponse
--
--         , responseRegisterWorkspaceDirectory $
--             newRegisterWorkspaceDirectoryResponse
--
--         , responseRestoreWorkspace $
--             newRestoreWorkspaceResponse
--
--         , responseDescribeConnectionAliasPermissions $
--             newDescribeConnectionAliasPermissionsResponse
--
--         , responseCreateTags $
--             newCreateTagsResponse
--
--         , responseCreateWorkspaceBundle $
--             newCreateWorkspaceBundleResponse
--
--         , responseDeleteTags $
--             newDeleteTagsResponse
--
--         , responseModifyWorkspaceAccessProperties $
--             newModifyWorkspaceAccessPropertiesResponse
--
--         , responseUpdateRulesOfIpGroup $
--             newUpdateRulesOfIpGroupResponse
--
--         , responseDeleteWorkspaceImage $
--             newDeleteWorkspaceImageResponse
--
--         , responseStopWorkspaces $
--             newStopWorkspacesResponse
--
--         , responseAssociateIpGroups $
--             newAssociateIpGroupsResponse
--
--         , responseModifySelfservicePermissions $
--             newModifySelfservicePermissionsResponse
--
--         , responseDeleteConnectionAlias $
--             newDeleteConnectionAliasResponse
--
--         , responseDescribeWorkspacesConnectionStatus $
--             newDescribeWorkspacesConnectionStatusResponse
--
--         , responseCreateConnectionAlias $
--             newCreateConnectionAliasResponse
--
--         , responseRebootWorkspaces $
--             newRebootWorkspacesResponse
--
--         , responseDeleteIpGroup $
--             newDeleteIpGroupResponse
--
--         , responseCopyWorkspaceImage $
--             newCopyWorkspaceImageResponse
--
--         , responseDescribeWorkspaceSnapshots $
--             newDescribeWorkspaceSnapshotsResponse
--
--         , responseTerminateWorkspaces $
--             newTerminateWorkspacesResponse
--
--         , responseUpdateConnectionAliasPermission $
--             newUpdateConnectionAliasPermissionResponse
--
--         , responseCreateWorkspaces $
--             newCreateWorkspacesResponse
--
--         , responseDescribeClientProperties $
--             newDescribeClientPropertiesResponse
--
--         , responseModifyClientProperties $
--             newModifyClientPropertiesResponse
--
--         , responseDescribeIpGroups $
--             newDescribeIpGroupsResponse
--
--         , responseDeleteWorkspaceBundle $
--             newDeleteWorkspaceBundleResponse
--
--         , responseUpdateWorkspaceBundle $
--             newUpdateWorkspaceBundleResponse
--
--         , responseListAvailableManagementCidrRanges $
--             newListAvailableManagementCidrRangesResponse
--
--         , responseUpdateWorkspaceImagePermission $
--             newUpdateWorkspaceImagePermissionResponse
--
--         , responseCreateUpdatedWorkspaceImage $
--             newCreateUpdatedWorkspaceImageResponse
--
--         , responseDescribeWorkspaces $
--             newDescribeWorkspacesResponse
--
--         , responseDescribeConnectionAliases $
--             newDescribeConnectionAliasesResponse
--
--         , responseStartWorkspaces $
--             newStartWorkspacesResponse
--
--         , responseDescribeAccountModifications $
--             newDescribeAccountModificationsResponse
--
--         , responseModifyAccount $
--             newModifyAccountResponse
--
--           ]
--     ]

-- Requests

requestAssociateConnectionAlias :: AssociateConnectionAlias -> TestTree
requestAssociateConnectionAlias =
  req
    "AssociateConnectionAlias"
    "fixture/AssociateConnectionAlias.yaml"

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

requestDescribeWorkspaceImages :: DescribeWorkspaceImages -> TestTree
requestDescribeWorkspaceImages =
  req
    "DescribeWorkspaceImages"
    "fixture/DescribeWorkspaceImages.yaml"

requestModifyWorkspaceProperties :: ModifyWorkspaceProperties -> TestTree
requestModifyWorkspaceProperties =
  req
    "ModifyWorkspaceProperties"
    "fixture/ModifyWorkspaceProperties.yaml"

requestDeregisterWorkspaceDirectory :: DeregisterWorkspaceDirectory -> TestTree
requestDeregisterWorkspaceDirectory =
  req
    "DeregisterWorkspaceDirectory"
    "fixture/DeregisterWorkspaceDirectory.yaml"

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

requestDescribeWorkspaceDirectories :: DescribeWorkspaceDirectories -> TestTree
requestDescribeWorkspaceDirectories =
  req
    "DescribeWorkspaceDirectories"
    "fixture/DescribeWorkspaceDirectories.yaml"

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

requestImportWorkspaceImage :: ImportWorkspaceImage -> TestTree
requestImportWorkspaceImage =
  req
    "ImportWorkspaceImage"
    "fixture/ImportWorkspaceImage.yaml"

requestModifyWorkspaceState :: ModifyWorkspaceState -> TestTree
requestModifyWorkspaceState =
  req
    "ModifyWorkspaceState"
    "fixture/ModifyWorkspaceState.yaml"

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

requestModifyWorkspaceCreationProperties :: ModifyWorkspaceCreationProperties -> TestTree
requestModifyWorkspaceCreationProperties =
  req
    "ModifyWorkspaceCreationProperties"
    "fixture/ModifyWorkspaceCreationProperties.yaml"

requestRegisterWorkspaceDirectory :: RegisterWorkspaceDirectory -> TestTree
requestRegisterWorkspaceDirectory =
  req
    "RegisterWorkspaceDirectory"
    "fixture/RegisterWorkspaceDirectory.yaml"

requestRestoreWorkspace :: RestoreWorkspace -> TestTree
requestRestoreWorkspace =
  req
    "RestoreWorkspace"
    "fixture/RestoreWorkspace.yaml"

requestDescribeConnectionAliasPermissions :: DescribeConnectionAliasPermissions -> TestTree
requestDescribeConnectionAliasPermissions =
  req
    "DescribeConnectionAliasPermissions"
    "fixture/DescribeConnectionAliasPermissions.yaml"

requestCreateTags :: CreateTags -> TestTree
requestCreateTags =
  req
    "CreateTags"
    "fixture/CreateTags.yaml"

requestCreateWorkspaceBundle :: CreateWorkspaceBundle -> TestTree
requestCreateWorkspaceBundle =
  req
    "CreateWorkspaceBundle"
    "fixture/CreateWorkspaceBundle.yaml"

requestDeleteTags :: DeleteTags -> TestTree
requestDeleteTags =
  req
    "DeleteTags"
    "fixture/DeleteTags.yaml"

requestModifyWorkspaceAccessProperties :: ModifyWorkspaceAccessProperties -> TestTree
requestModifyWorkspaceAccessProperties =
  req
    "ModifyWorkspaceAccessProperties"
    "fixture/ModifyWorkspaceAccessProperties.yaml"

requestUpdateRulesOfIpGroup :: UpdateRulesOfIpGroup -> TestTree
requestUpdateRulesOfIpGroup =
  req
    "UpdateRulesOfIpGroup"
    "fixture/UpdateRulesOfIpGroup.yaml"

requestDeleteWorkspaceImage :: DeleteWorkspaceImage -> TestTree
requestDeleteWorkspaceImage =
  req
    "DeleteWorkspaceImage"
    "fixture/DeleteWorkspaceImage.yaml"

requestStopWorkspaces :: StopWorkspaces -> TestTree
requestStopWorkspaces =
  req
    "StopWorkspaces"
    "fixture/StopWorkspaces.yaml"

requestAssociateIpGroups :: AssociateIpGroups -> TestTree
requestAssociateIpGroups =
  req
    "AssociateIpGroups"
    "fixture/AssociateIpGroups.yaml"

requestModifySelfservicePermissions :: ModifySelfservicePermissions -> TestTree
requestModifySelfservicePermissions =
  req
    "ModifySelfservicePermissions"
    "fixture/ModifySelfservicePermissions.yaml"

requestDeleteConnectionAlias :: DeleteConnectionAlias -> TestTree
requestDeleteConnectionAlias =
  req
    "DeleteConnectionAlias"
    "fixture/DeleteConnectionAlias.yaml"

requestDescribeWorkspacesConnectionStatus :: DescribeWorkspacesConnectionStatus -> TestTree
requestDescribeWorkspacesConnectionStatus =
  req
    "DescribeWorkspacesConnectionStatus"
    "fixture/DescribeWorkspacesConnectionStatus.yaml"

requestCreateConnectionAlias :: CreateConnectionAlias -> TestTree
requestCreateConnectionAlias =
  req
    "CreateConnectionAlias"
    "fixture/CreateConnectionAlias.yaml"

requestRebootWorkspaces :: RebootWorkspaces -> TestTree
requestRebootWorkspaces =
  req
    "RebootWorkspaces"
    "fixture/RebootWorkspaces.yaml"

requestDeleteIpGroup :: DeleteIpGroup -> TestTree
requestDeleteIpGroup =
  req
    "DeleteIpGroup"
    "fixture/DeleteIpGroup.yaml"

requestCopyWorkspaceImage :: CopyWorkspaceImage -> TestTree
requestCopyWorkspaceImage =
  req
    "CopyWorkspaceImage"
    "fixture/CopyWorkspaceImage.yaml"

requestDescribeWorkspaceSnapshots :: DescribeWorkspaceSnapshots -> TestTree
requestDescribeWorkspaceSnapshots =
  req
    "DescribeWorkspaceSnapshots"
    "fixture/DescribeWorkspaceSnapshots.yaml"

requestTerminateWorkspaces :: TerminateWorkspaces -> TestTree
requestTerminateWorkspaces =
  req
    "TerminateWorkspaces"
    "fixture/TerminateWorkspaces.yaml"

requestUpdateConnectionAliasPermission :: UpdateConnectionAliasPermission -> TestTree
requestUpdateConnectionAliasPermission =
  req
    "UpdateConnectionAliasPermission"
    "fixture/UpdateConnectionAliasPermission.yaml"

requestCreateWorkspaces :: CreateWorkspaces -> TestTree
requestCreateWorkspaces =
  req
    "CreateWorkspaces"
    "fixture/CreateWorkspaces.yaml"

requestDescribeClientProperties :: DescribeClientProperties -> TestTree
requestDescribeClientProperties =
  req
    "DescribeClientProperties"
    "fixture/DescribeClientProperties.yaml"

requestModifyClientProperties :: ModifyClientProperties -> TestTree
requestModifyClientProperties =
  req
    "ModifyClientProperties"
    "fixture/ModifyClientProperties.yaml"

requestDescribeIpGroups :: DescribeIpGroups -> TestTree
requestDescribeIpGroups =
  req
    "DescribeIpGroups"
    "fixture/DescribeIpGroups.yaml"

requestDeleteWorkspaceBundle :: DeleteWorkspaceBundle -> TestTree
requestDeleteWorkspaceBundle =
  req
    "DeleteWorkspaceBundle"
    "fixture/DeleteWorkspaceBundle.yaml"

requestUpdateWorkspaceBundle :: UpdateWorkspaceBundle -> TestTree
requestUpdateWorkspaceBundle =
  req
    "UpdateWorkspaceBundle"
    "fixture/UpdateWorkspaceBundle.yaml"

requestListAvailableManagementCidrRanges :: ListAvailableManagementCidrRanges -> TestTree
requestListAvailableManagementCidrRanges =
  req
    "ListAvailableManagementCidrRanges"
    "fixture/ListAvailableManagementCidrRanges.yaml"

requestUpdateWorkspaceImagePermission :: UpdateWorkspaceImagePermission -> TestTree
requestUpdateWorkspaceImagePermission =
  req
    "UpdateWorkspaceImagePermission"
    "fixture/UpdateWorkspaceImagePermission.yaml"

requestCreateUpdatedWorkspaceImage :: CreateUpdatedWorkspaceImage -> TestTree
requestCreateUpdatedWorkspaceImage =
  req
    "CreateUpdatedWorkspaceImage"
    "fixture/CreateUpdatedWorkspaceImage.yaml"

requestDescribeWorkspaces :: DescribeWorkspaces -> TestTree
requestDescribeWorkspaces =
  req
    "DescribeWorkspaces"
    "fixture/DescribeWorkspaces.yaml"

requestDescribeConnectionAliases :: DescribeConnectionAliases -> TestTree
requestDescribeConnectionAliases =
  req
    "DescribeConnectionAliases"
    "fixture/DescribeConnectionAliases.yaml"

requestStartWorkspaces :: StartWorkspaces -> TestTree
requestStartWorkspaces =
  req
    "StartWorkspaces"
    "fixture/StartWorkspaces.yaml"

requestDescribeAccountModifications :: DescribeAccountModifications -> TestTree
requestDescribeAccountModifications =
  req
    "DescribeAccountModifications"
    "fixture/DescribeAccountModifications.yaml"

requestModifyAccount :: ModifyAccount -> TestTree
requestModifyAccount =
  req
    "ModifyAccount"
    "fixture/ModifyAccount.yaml"

-- Responses

responseAssociateConnectionAlias :: AssociateConnectionAliasResponse -> TestTree
responseAssociateConnectionAlias =
  res
    "AssociateConnectionAliasResponse"
    "fixture/AssociateConnectionAliasResponse.proto"
    defaultService
    (Proxy :: Proxy AssociateConnectionAlias)

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

responseDescribeWorkspaceImages :: DescribeWorkspaceImagesResponse -> TestTree
responseDescribeWorkspaceImages =
  res
    "DescribeWorkspaceImagesResponse"
    "fixture/DescribeWorkspaceImagesResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeWorkspaceImages)

responseModifyWorkspaceProperties :: ModifyWorkspacePropertiesResponse -> TestTree
responseModifyWorkspaceProperties =
  res
    "ModifyWorkspacePropertiesResponse"
    "fixture/ModifyWorkspacePropertiesResponse.proto"
    defaultService
    (Proxy :: Proxy ModifyWorkspaceProperties)

responseDeregisterWorkspaceDirectory :: DeregisterWorkspaceDirectoryResponse -> TestTree
responseDeregisterWorkspaceDirectory =
  res
    "DeregisterWorkspaceDirectoryResponse"
    "fixture/DeregisterWorkspaceDirectoryResponse.proto"
    defaultService
    (Proxy :: Proxy DeregisterWorkspaceDirectory)

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

responseDescribeWorkspaceDirectories :: DescribeWorkspaceDirectoriesResponse -> TestTree
responseDescribeWorkspaceDirectories =
  res
    "DescribeWorkspaceDirectoriesResponse"
    "fixture/DescribeWorkspaceDirectoriesResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeWorkspaceDirectories)

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

responseImportWorkspaceImage :: ImportWorkspaceImageResponse -> TestTree
responseImportWorkspaceImage =
  res
    "ImportWorkspaceImageResponse"
    "fixture/ImportWorkspaceImageResponse.proto"
    defaultService
    (Proxy :: Proxy ImportWorkspaceImage)

responseModifyWorkspaceState :: ModifyWorkspaceStateResponse -> TestTree
responseModifyWorkspaceState =
  res
    "ModifyWorkspaceStateResponse"
    "fixture/ModifyWorkspaceStateResponse.proto"
    defaultService
    (Proxy :: Proxy ModifyWorkspaceState)

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

responseModifyWorkspaceCreationProperties :: ModifyWorkspaceCreationPropertiesResponse -> TestTree
responseModifyWorkspaceCreationProperties =
  res
    "ModifyWorkspaceCreationPropertiesResponse"
    "fixture/ModifyWorkspaceCreationPropertiesResponse.proto"
    defaultService
    (Proxy :: Proxy ModifyWorkspaceCreationProperties)

responseRegisterWorkspaceDirectory :: RegisterWorkspaceDirectoryResponse -> TestTree
responseRegisterWorkspaceDirectory =
  res
    "RegisterWorkspaceDirectoryResponse"
    "fixture/RegisterWorkspaceDirectoryResponse.proto"
    defaultService
    (Proxy :: Proxy RegisterWorkspaceDirectory)

responseRestoreWorkspace :: RestoreWorkspaceResponse -> TestTree
responseRestoreWorkspace =
  res
    "RestoreWorkspaceResponse"
    "fixture/RestoreWorkspaceResponse.proto"
    defaultService
    (Proxy :: Proxy RestoreWorkspace)

responseDescribeConnectionAliasPermissions :: DescribeConnectionAliasPermissionsResponse -> TestTree
responseDescribeConnectionAliasPermissions =
  res
    "DescribeConnectionAliasPermissionsResponse"
    "fixture/DescribeConnectionAliasPermissionsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeConnectionAliasPermissions)

responseCreateTags :: CreateTagsResponse -> TestTree
responseCreateTags =
  res
    "CreateTagsResponse"
    "fixture/CreateTagsResponse.proto"
    defaultService
    (Proxy :: Proxy CreateTags)

responseCreateWorkspaceBundle :: CreateWorkspaceBundleResponse -> TestTree
responseCreateWorkspaceBundle =
  res
    "CreateWorkspaceBundleResponse"
    "fixture/CreateWorkspaceBundleResponse.proto"
    defaultService
    (Proxy :: Proxy CreateWorkspaceBundle)

responseDeleteTags :: DeleteTagsResponse -> TestTree
responseDeleteTags =
  res
    "DeleteTagsResponse"
    "fixture/DeleteTagsResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteTags)

responseModifyWorkspaceAccessProperties :: ModifyWorkspaceAccessPropertiesResponse -> TestTree
responseModifyWorkspaceAccessProperties =
  res
    "ModifyWorkspaceAccessPropertiesResponse"
    "fixture/ModifyWorkspaceAccessPropertiesResponse.proto"
    defaultService
    (Proxy :: Proxy ModifyWorkspaceAccessProperties)

responseUpdateRulesOfIpGroup :: UpdateRulesOfIpGroupResponse -> TestTree
responseUpdateRulesOfIpGroup =
  res
    "UpdateRulesOfIpGroupResponse"
    "fixture/UpdateRulesOfIpGroupResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateRulesOfIpGroup)

responseDeleteWorkspaceImage :: DeleteWorkspaceImageResponse -> TestTree
responseDeleteWorkspaceImage =
  res
    "DeleteWorkspaceImageResponse"
    "fixture/DeleteWorkspaceImageResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteWorkspaceImage)

responseStopWorkspaces :: StopWorkspacesResponse -> TestTree
responseStopWorkspaces =
  res
    "StopWorkspacesResponse"
    "fixture/StopWorkspacesResponse.proto"
    defaultService
    (Proxy :: Proxy StopWorkspaces)

responseAssociateIpGroups :: AssociateIpGroupsResponse -> TestTree
responseAssociateIpGroups =
  res
    "AssociateIpGroupsResponse"
    "fixture/AssociateIpGroupsResponse.proto"
    defaultService
    (Proxy :: Proxy AssociateIpGroups)

responseModifySelfservicePermissions :: ModifySelfservicePermissionsResponse -> TestTree
responseModifySelfservicePermissions =
  res
    "ModifySelfservicePermissionsResponse"
    "fixture/ModifySelfservicePermissionsResponse.proto"
    defaultService
    (Proxy :: Proxy ModifySelfservicePermissions)

responseDeleteConnectionAlias :: DeleteConnectionAliasResponse -> TestTree
responseDeleteConnectionAlias =
  res
    "DeleteConnectionAliasResponse"
    "fixture/DeleteConnectionAliasResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteConnectionAlias)

responseDescribeWorkspacesConnectionStatus :: DescribeWorkspacesConnectionStatusResponse -> TestTree
responseDescribeWorkspacesConnectionStatus =
  res
    "DescribeWorkspacesConnectionStatusResponse"
    "fixture/DescribeWorkspacesConnectionStatusResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeWorkspacesConnectionStatus)

responseCreateConnectionAlias :: CreateConnectionAliasResponse -> TestTree
responseCreateConnectionAlias =
  res
    "CreateConnectionAliasResponse"
    "fixture/CreateConnectionAliasResponse.proto"
    defaultService
    (Proxy :: Proxy CreateConnectionAlias)

responseRebootWorkspaces :: RebootWorkspacesResponse -> TestTree
responseRebootWorkspaces =
  res
    "RebootWorkspacesResponse"
    "fixture/RebootWorkspacesResponse.proto"
    defaultService
    (Proxy :: Proxy RebootWorkspaces)

responseDeleteIpGroup :: DeleteIpGroupResponse -> TestTree
responseDeleteIpGroup =
  res
    "DeleteIpGroupResponse"
    "fixture/DeleteIpGroupResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteIpGroup)

responseCopyWorkspaceImage :: CopyWorkspaceImageResponse -> TestTree
responseCopyWorkspaceImage =
  res
    "CopyWorkspaceImageResponse"
    "fixture/CopyWorkspaceImageResponse.proto"
    defaultService
    (Proxy :: Proxy CopyWorkspaceImage)

responseDescribeWorkspaceSnapshots :: DescribeWorkspaceSnapshotsResponse -> TestTree
responseDescribeWorkspaceSnapshots =
  res
    "DescribeWorkspaceSnapshotsResponse"
    "fixture/DescribeWorkspaceSnapshotsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeWorkspaceSnapshots)

responseTerminateWorkspaces :: TerminateWorkspacesResponse -> TestTree
responseTerminateWorkspaces =
  res
    "TerminateWorkspacesResponse"
    "fixture/TerminateWorkspacesResponse.proto"
    defaultService
    (Proxy :: Proxy TerminateWorkspaces)

responseUpdateConnectionAliasPermission :: UpdateConnectionAliasPermissionResponse -> TestTree
responseUpdateConnectionAliasPermission =
  res
    "UpdateConnectionAliasPermissionResponse"
    "fixture/UpdateConnectionAliasPermissionResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateConnectionAliasPermission)

responseCreateWorkspaces :: CreateWorkspacesResponse -> TestTree
responseCreateWorkspaces =
  res
    "CreateWorkspacesResponse"
    "fixture/CreateWorkspacesResponse.proto"
    defaultService
    (Proxy :: Proxy CreateWorkspaces)

responseDescribeClientProperties :: DescribeClientPropertiesResponse -> TestTree
responseDescribeClientProperties =
  res
    "DescribeClientPropertiesResponse"
    "fixture/DescribeClientPropertiesResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeClientProperties)

responseModifyClientProperties :: ModifyClientPropertiesResponse -> TestTree
responseModifyClientProperties =
  res
    "ModifyClientPropertiesResponse"
    "fixture/ModifyClientPropertiesResponse.proto"
    defaultService
    (Proxy :: Proxy ModifyClientProperties)

responseDescribeIpGroups :: DescribeIpGroupsResponse -> TestTree
responseDescribeIpGroups =
  res
    "DescribeIpGroupsResponse"
    "fixture/DescribeIpGroupsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeIpGroups)

responseDeleteWorkspaceBundle :: DeleteWorkspaceBundleResponse -> TestTree
responseDeleteWorkspaceBundle =
  res
    "DeleteWorkspaceBundleResponse"
    "fixture/DeleteWorkspaceBundleResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteWorkspaceBundle)

responseUpdateWorkspaceBundle :: UpdateWorkspaceBundleResponse -> TestTree
responseUpdateWorkspaceBundle =
  res
    "UpdateWorkspaceBundleResponse"
    "fixture/UpdateWorkspaceBundleResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateWorkspaceBundle)

responseListAvailableManagementCidrRanges :: ListAvailableManagementCidrRangesResponse -> TestTree
responseListAvailableManagementCidrRanges =
  res
    "ListAvailableManagementCidrRangesResponse"
    "fixture/ListAvailableManagementCidrRangesResponse.proto"
    defaultService
    (Proxy :: Proxy ListAvailableManagementCidrRanges)

responseUpdateWorkspaceImagePermission :: UpdateWorkspaceImagePermissionResponse -> TestTree
responseUpdateWorkspaceImagePermission =
  res
    "UpdateWorkspaceImagePermissionResponse"
    "fixture/UpdateWorkspaceImagePermissionResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateWorkspaceImagePermission)

responseCreateUpdatedWorkspaceImage :: CreateUpdatedWorkspaceImageResponse -> TestTree
responseCreateUpdatedWorkspaceImage =
  res
    "CreateUpdatedWorkspaceImageResponse"
    "fixture/CreateUpdatedWorkspaceImageResponse.proto"
    defaultService
    (Proxy :: Proxy CreateUpdatedWorkspaceImage)

responseDescribeWorkspaces :: DescribeWorkspacesResponse -> TestTree
responseDescribeWorkspaces =
  res
    "DescribeWorkspacesResponse"
    "fixture/DescribeWorkspacesResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeWorkspaces)

responseDescribeConnectionAliases :: DescribeConnectionAliasesResponse -> TestTree
responseDescribeConnectionAliases =
  res
    "DescribeConnectionAliasesResponse"
    "fixture/DescribeConnectionAliasesResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeConnectionAliases)

responseStartWorkspaces :: StartWorkspacesResponse -> TestTree
responseStartWorkspaces =
  res
    "StartWorkspacesResponse"
    "fixture/StartWorkspacesResponse.proto"
    defaultService
    (Proxy :: Proxy StartWorkspaces)

responseDescribeAccountModifications :: DescribeAccountModificationsResponse -> TestTree
responseDescribeAccountModifications =
  res
    "DescribeAccountModificationsResponse"
    "fixture/DescribeAccountModificationsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeAccountModifications)

responseModifyAccount :: ModifyAccountResponse -> TestTree
responseModifyAccount =
  res
    "ModifyAccountResponse"
    "fixture/ModifyAccountResponse.proto"
    defaultService
    (Proxy :: Proxy ModifyAccount)
