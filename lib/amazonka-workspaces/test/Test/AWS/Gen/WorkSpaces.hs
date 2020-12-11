{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.WorkSpaces
-- Copyright   : (c) 2013-2020 Brendan Hay
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
--             mkAssociateConnectionAlias
--
--         , requestDescribeAccount $
--             mkDescribeAccount
--
--         , requestRevokeIPRules $
--             mkRevokeIPRules
--
--         , requestDescribeWorkspaceImages $
--             mkDescribeWorkspaceImages
--
--         , requestModifyWorkspaceProperties $
--             mkModifyWorkspaceProperties
--
--         , requestDeregisterWorkspaceDirectory $
--             mkDeregisterWorkspaceDirectory
--
--         , requestMigrateWorkspace $
--             mkMigrateWorkspace
--
--         , requestDescribeTags $
--             mkDescribeTags
--
--         , requestDescribeWorkspaceDirectories $
--             mkDescribeWorkspaceDirectories
--
--         , requestDisassociateIPGroups $
--             mkDisassociateIPGroups
--
--         , requestDescribeWorkspaceBundles $
--             mkDescribeWorkspaceBundles
--
--         , requestAuthorizeIPRules $
--             mkAuthorizeIPRules
--
--         , requestDescribeWorkspaceImagePermissions $
--             mkDescribeWorkspaceImagePermissions
--
--         , requestRebuildWorkspaces $
--             mkRebuildWorkspaces
--
--         , requestImportWorkspaceImage $
--             mkImportWorkspaceImage
--
--         , requestModifyWorkspaceState $
--             mkModifyWorkspaceState
--
--         , requestCreateIPGroup $
--             mkCreateIPGroup
--
--         , requestDisassociateConnectionAlias $
--             mkDisassociateConnectionAlias
--
--         , requestModifyWorkspaceCreationProperties $
--             mkModifyWorkspaceCreationProperties
--
--         , requestRegisterWorkspaceDirectory $
--             mkRegisterWorkspaceDirectory
--
--         , requestRestoreWorkspace $
--             mkRestoreWorkspace
--
--         , requestDescribeConnectionAliasPermissions $
--             mkDescribeConnectionAliasPermissions
--
--         , requestCreateTags $
--             mkCreateTags
--
--         , requestDeleteTags $
--             mkDeleteTags
--
--         , requestModifyWorkspaceAccessProperties $
--             mkModifyWorkspaceAccessProperties
--
--         , requestUpdateRulesOfIPGroup $
--             mkUpdateRulesOfIPGroup
--
--         , requestDeleteWorkspaceImage $
--             mkDeleteWorkspaceImage
--
--         , requestStopWorkspaces $
--             mkStopWorkspaces
--
--         , requestAssociateIPGroups $
--             mkAssociateIPGroups
--
--         , requestModifySelfservicePermissions $
--             mkModifySelfservicePermissions
--
--         , requestDeleteConnectionAlias $
--             mkDeleteConnectionAlias
--
--         , requestDescribeWorkspacesConnectionStatus $
--             mkDescribeWorkspacesConnectionStatus
--
--         , requestCreateConnectionAlias $
--             mkCreateConnectionAlias
--
--         , requestRebootWorkspaces $
--             mkRebootWorkspaces
--
--         , requestDeleteIPGroup $
--             mkDeleteIPGroup
--
--         , requestCopyWorkspaceImage $
--             mkCopyWorkspaceImage
--
--         , requestDescribeWorkspaceSnapshots $
--             mkDescribeWorkspaceSnapshots
--
--         , requestTerminateWorkspaces $
--             mkTerminateWorkspaces
--
--         , requestUpdateConnectionAliasPermission $
--             mkUpdateConnectionAliasPermission
--
--         , requestCreateWorkspaces $
--             mkCreateWorkspaces
--
--         , requestDescribeClientProperties $
--             mkDescribeClientProperties
--
--         , requestModifyClientProperties $
--             mkModifyClientProperties
--
--         , requestDescribeIPGroups $
--             mkDescribeIPGroups
--
--         , requestListAvailableManagementCidrRanges $
--             mkListAvailableManagementCidrRanges
--
--         , requestUpdateWorkspaceImagePermission $
--             mkUpdateWorkspaceImagePermission
--
--         , requestDescribeWorkspaces $
--             mkDescribeWorkspaces
--
--         , requestDescribeConnectionAliases $
--             mkDescribeConnectionAliases
--
--         , requestStartWorkspaces $
--             mkStartWorkspaces
--
--         , requestDescribeAccountModifications $
--             mkDescribeAccountModifications
--
--         , requestModifyAccount $
--             mkModifyAccount
--
--           ]

--     , testGroup "response"
--         [ responseAssociateConnectionAlias $
--             mkAssociateConnectionAliasResponse
--
--         , responseDescribeAccount $
--             mkDescribeAccountResponse
--
--         , responseRevokeIPRules $
--             mkRevokeIPRulesResponse
--
--         , responseDescribeWorkspaceImages $
--             mkDescribeWorkspaceImagesResponse
--
--         , responseModifyWorkspaceProperties $
--             mkModifyWorkspacePropertiesResponse
--
--         , responseDeregisterWorkspaceDirectory $
--             mkDeregisterWorkspaceDirectoryResponse
--
--         , responseMigrateWorkspace $
--             mkMigrateWorkspaceResponse
--
--         , responseDescribeTags $
--             mkDescribeTagsResponse
--
--         , responseDescribeWorkspaceDirectories $
--             mkDescribeWorkspaceDirectoriesResponse
--
--         , responseDisassociateIPGroups $
--             mkDisassociateIPGroupsResponse
--
--         , responseDescribeWorkspaceBundles $
--             mkDescribeWorkspaceBundlesResponse
--
--         , responseAuthorizeIPRules $
--             mkAuthorizeIPRulesResponse
--
--         , responseDescribeWorkspaceImagePermissions $
--             mkDescribeWorkspaceImagePermissionsResponse
--
--         , responseRebuildWorkspaces $
--             mkRebuildWorkspacesResponse
--
--         , responseImportWorkspaceImage $
--             mkImportWorkspaceImageResponse
--
--         , responseModifyWorkspaceState $
--             mkModifyWorkspaceStateResponse
--
--         , responseCreateIPGroup $
--             mkCreateIPGroupResponse
--
--         , responseDisassociateConnectionAlias $
--             mkDisassociateConnectionAliasResponse
--
--         , responseModifyWorkspaceCreationProperties $
--             mkModifyWorkspaceCreationPropertiesResponse
--
--         , responseRegisterWorkspaceDirectory $
--             mkRegisterWorkspaceDirectoryResponse
--
--         , responseRestoreWorkspace $
--             mkRestoreWorkspaceResponse
--
--         , responseDescribeConnectionAliasPermissions $
--             mkDescribeConnectionAliasPermissionsResponse
--
--         , responseCreateTags $
--             mkCreateTagsResponse
--
--         , responseDeleteTags $
--             mkDeleteTagsResponse
--
--         , responseModifyWorkspaceAccessProperties $
--             mkModifyWorkspaceAccessPropertiesResponse
--
--         , responseUpdateRulesOfIPGroup $
--             mkUpdateRulesOfIPGroupResponse
--
--         , responseDeleteWorkspaceImage $
--             mkDeleteWorkspaceImageResponse
--
--         , responseStopWorkspaces $
--             mkStopWorkspacesResponse
--
--         , responseAssociateIPGroups $
--             mkAssociateIPGroupsResponse
--
--         , responseModifySelfservicePermissions $
--             mkModifySelfservicePermissionsResponse
--
--         , responseDeleteConnectionAlias $
--             mkDeleteConnectionAliasResponse
--
--         , responseDescribeWorkspacesConnectionStatus $
--             mkDescribeWorkspacesConnectionStatusResponse
--
--         , responseCreateConnectionAlias $
--             mkCreateConnectionAliasResponse
--
--         , responseRebootWorkspaces $
--             mkRebootWorkspacesResponse
--
--         , responseDeleteIPGroup $
--             mkDeleteIPGroupResponse
--
--         , responseCopyWorkspaceImage $
--             mkCopyWorkspaceImageResponse
--
--         , responseDescribeWorkspaceSnapshots $
--             mkDescribeWorkspaceSnapshotsResponse
--
--         , responseTerminateWorkspaces $
--             mkTerminateWorkspacesResponse
--
--         , responseUpdateConnectionAliasPermission $
--             mkUpdateConnectionAliasPermissionResponse
--
--         , responseCreateWorkspaces $
--             mkCreateWorkspacesResponse
--
--         , responseDescribeClientProperties $
--             mkDescribeClientPropertiesResponse
--
--         , responseModifyClientProperties $
--             mkModifyClientPropertiesResponse
--
--         , responseDescribeIPGroups $
--             mkDescribeIPGroupsResponse
--
--         , responseListAvailableManagementCidrRanges $
--             mkListAvailableManagementCidrRangesResponse
--
--         , responseUpdateWorkspaceImagePermission $
--             mkUpdateWorkspaceImagePermissionResponse
--
--         , responseDescribeWorkspaces $
--             mkDescribeWorkspacesResponse
--
--         , responseDescribeConnectionAliases $
--             mkDescribeConnectionAliasesResponse
--
--         , responseStartWorkspaces $
--             mkStartWorkspacesResponse
--
--         , responseDescribeAccountModifications $
--             mkDescribeAccountModificationsResponse
--
--         , responseModifyAccount $
--             mkModifyAccountResponse
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

requestRevokeIPRules :: RevokeIPRules -> TestTree
requestRevokeIPRules =
  req
    "RevokeIPRules"
    "fixture/RevokeIPRules.yaml"

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

requestDisassociateIPGroups :: DisassociateIPGroups -> TestTree
requestDisassociateIPGroups =
  req
    "DisassociateIPGroups"
    "fixture/DisassociateIPGroups.yaml"

requestDescribeWorkspaceBundles :: DescribeWorkspaceBundles -> TestTree
requestDescribeWorkspaceBundles =
  req
    "DescribeWorkspaceBundles"
    "fixture/DescribeWorkspaceBundles.yaml"

requestAuthorizeIPRules :: AuthorizeIPRules -> TestTree
requestAuthorizeIPRules =
  req
    "AuthorizeIPRules"
    "fixture/AuthorizeIPRules.yaml"

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

requestCreateIPGroup :: CreateIPGroup -> TestTree
requestCreateIPGroup =
  req
    "CreateIPGroup"
    "fixture/CreateIPGroup.yaml"

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

requestUpdateRulesOfIPGroup :: UpdateRulesOfIPGroup -> TestTree
requestUpdateRulesOfIPGroup =
  req
    "UpdateRulesOfIPGroup"
    "fixture/UpdateRulesOfIPGroup.yaml"

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

requestAssociateIPGroups :: AssociateIPGroups -> TestTree
requestAssociateIPGroups =
  req
    "AssociateIPGroups"
    "fixture/AssociateIPGroups.yaml"

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

requestDeleteIPGroup :: DeleteIPGroup -> TestTree
requestDeleteIPGroup =
  req
    "DeleteIPGroup"
    "fixture/DeleteIPGroup.yaml"

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

requestDescribeIPGroups :: DescribeIPGroups -> TestTree
requestDescribeIPGroups =
  req
    "DescribeIPGroups"
    "fixture/DescribeIPGroups.yaml"

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
    workSpacesService
    (Proxy :: Proxy AssociateConnectionAlias)

responseDescribeAccount :: DescribeAccountResponse -> TestTree
responseDescribeAccount =
  res
    "DescribeAccountResponse"
    "fixture/DescribeAccountResponse.proto"
    workSpacesService
    (Proxy :: Proxy DescribeAccount)

responseRevokeIPRules :: RevokeIPRulesResponse -> TestTree
responseRevokeIPRules =
  res
    "RevokeIPRulesResponse"
    "fixture/RevokeIPRulesResponse.proto"
    workSpacesService
    (Proxy :: Proxy RevokeIPRules)

responseDescribeWorkspaceImages :: DescribeWorkspaceImagesResponse -> TestTree
responseDescribeWorkspaceImages =
  res
    "DescribeWorkspaceImagesResponse"
    "fixture/DescribeWorkspaceImagesResponse.proto"
    workSpacesService
    (Proxy :: Proxy DescribeWorkspaceImages)

responseModifyWorkspaceProperties :: ModifyWorkspacePropertiesResponse -> TestTree
responseModifyWorkspaceProperties =
  res
    "ModifyWorkspacePropertiesResponse"
    "fixture/ModifyWorkspacePropertiesResponse.proto"
    workSpacesService
    (Proxy :: Proxy ModifyWorkspaceProperties)

responseDeregisterWorkspaceDirectory :: DeregisterWorkspaceDirectoryResponse -> TestTree
responseDeregisterWorkspaceDirectory =
  res
    "DeregisterWorkspaceDirectoryResponse"
    "fixture/DeregisterWorkspaceDirectoryResponse.proto"
    workSpacesService
    (Proxy :: Proxy DeregisterWorkspaceDirectory)

responseMigrateWorkspace :: MigrateWorkspaceResponse -> TestTree
responseMigrateWorkspace =
  res
    "MigrateWorkspaceResponse"
    "fixture/MigrateWorkspaceResponse.proto"
    workSpacesService
    (Proxy :: Proxy MigrateWorkspace)

responseDescribeTags :: DescribeTagsResponse -> TestTree
responseDescribeTags =
  res
    "DescribeTagsResponse"
    "fixture/DescribeTagsResponse.proto"
    workSpacesService
    (Proxy :: Proxy DescribeTags)

responseDescribeWorkspaceDirectories :: DescribeWorkspaceDirectoriesResponse -> TestTree
responseDescribeWorkspaceDirectories =
  res
    "DescribeWorkspaceDirectoriesResponse"
    "fixture/DescribeWorkspaceDirectoriesResponse.proto"
    workSpacesService
    (Proxy :: Proxy DescribeWorkspaceDirectories)

responseDisassociateIPGroups :: DisassociateIPGroupsResponse -> TestTree
responseDisassociateIPGroups =
  res
    "DisassociateIPGroupsResponse"
    "fixture/DisassociateIPGroupsResponse.proto"
    workSpacesService
    (Proxy :: Proxy DisassociateIPGroups)

responseDescribeWorkspaceBundles :: DescribeWorkspaceBundlesResponse -> TestTree
responseDescribeWorkspaceBundles =
  res
    "DescribeWorkspaceBundlesResponse"
    "fixture/DescribeWorkspaceBundlesResponse.proto"
    workSpacesService
    (Proxy :: Proxy DescribeWorkspaceBundles)

responseAuthorizeIPRules :: AuthorizeIPRulesResponse -> TestTree
responseAuthorizeIPRules =
  res
    "AuthorizeIPRulesResponse"
    "fixture/AuthorizeIPRulesResponse.proto"
    workSpacesService
    (Proxy :: Proxy AuthorizeIPRules)

responseDescribeWorkspaceImagePermissions :: DescribeWorkspaceImagePermissionsResponse -> TestTree
responseDescribeWorkspaceImagePermissions =
  res
    "DescribeWorkspaceImagePermissionsResponse"
    "fixture/DescribeWorkspaceImagePermissionsResponse.proto"
    workSpacesService
    (Proxy :: Proxy DescribeWorkspaceImagePermissions)

responseRebuildWorkspaces :: RebuildWorkspacesResponse -> TestTree
responseRebuildWorkspaces =
  res
    "RebuildWorkspacesResponse"
    "fixture/RebuildWorkspacesResponse.proto"
    workSpacesService
    (Proxy :: Proxy RebuildWorkspaces)

responseImportWorkspaceImage :: ImportWorkspaceImageResponse -> TestTree
responseImportWorkspaceImage =
  res
    "ImportWorkspaceImageResponse"
    "fixture/ImportWorkspaceImageResponse.proto"
    workSpacesService
    (Proxy :: Proxy ImportWorkspaceImage)

responseModifyWorkspaceState :: ModifyWorkspaceStateResponse -> TestTree
responseModifyWorkspaceState =
  res
    "ModifyWorkspaceStateResponse"
    "fixture/ModifyWorkspaceStateResponse.proto"
    workSpacesService
    (Proxy :: Proxy ModifyWorkspaceState)

responseCreateIPGroup :: CreateIPGroupResponse -> TestTree
responseCreateIPGroup =
  res
    "CreateIPGroupResponse"
    "fixture/CreateIPGroupResponse.proto"
    workSpacesService
    (Proxy :: Proxy CreateIPGroup)

responseDisassociateConnectionAlias :: DisassociateConnectionAliasResponse -> TestTree
responseDisassociateConnectionAlias =
  res
    "DisassociateConnectionAliasResponse"
    "fixture/DisassociateConnectionAliasResponse.proto"
    workSpacesService
    (Proxy :: Proxy DisassociateConnectionAlias)

responseModifyWorkspaceCreationProperties :: ModifyWorkspaceCreationPropertiesResponse -> TestTree
responseModifyWorkspaceCreationProperties =
  res
    "ModifyWorkspaceCreationPropertiesResponse"
    "fixture/ModifyWorkspaceCreationPropertiesResponse.proto"
    workSpacesService
    (Proxy :: Proxy ModifyWorkspaceCreationProperties)

responseRegisterWorkspaceDirectory :: RegisterWorkspaceDirectoryResponse -> TestTree
responseRegisterWorkspaceDirectory =
  res
    "RegisterWorkspaceDirectoryResponse"
    "fixture/RegisterWorkspaceDirectoryResponse.proto"
    workSpacesService
    (Proxy :: Proxy RegisterWorkspaceDirectory)

responseRestoreWorkspace :: RestoreWorkspaceResponse -> TestTree
responseRestoreWorkspace =
  res
    "RestoreWorkspaceResponse"
    "fixture/RestoreWorkspaceResponse.proto"
    workSpacesService
    (Proxy :: Proxy RestoreWorkspace)

responseDescribeConnectionAliasPermissions :: DescribeConnectionAliasPermissionsResponse -> TestTree
responseDescribeConnectionAliasPermissions =
  res
    "DescribeConnectionAliasPermissionsResponse"
    "fixture/DescribeConnectionAliasPermissionsResponse.proto"
    workSpacesService
    (Proxy :: Proxy DescribeConnectionAliasPermissions)

responseCreateTags :: CreateTagsResponse -> TestTree
responseCreateTags =
  res
    "CreateTagsResponse"
    "fixture/CreateTagsResponse.proto"
    workSpacesService
    (Proxy :: Proxy CreateTags)

responseDeleteTags :: DeleteTagsResponse -> TestTree
responseDeleteTags =
  res
    "DeleteTagsResponse"
    "fixture/DeleteTagsResponse.proto"
    workSpacesService
    (Proxy :: Proxy DeleteTags)

responseModifyWorkspaceAccessProperties :: ModifyWorkspaceAccessPropertiesResponse -> TestTree
responseModifyWorkspaceAccessProperties =
  res
    "ModifyWorkspaceAccessPropertiesResponse"
    "fixture/ModifyWorkspaceAccessPropertiesResponse.proto"
    workSpacesService
    (Proxy :: Proxy ModifyWorkspaceAccessProperties)

responseUpdateRulesOfIPGroup :: UpdateRulesOfIPGroupResponse -> TestTree
responseUpdateRulesOfIPGroup =
  res
    "UpdateRulesOfIPGroupResponse"
    "fixture/UpdateRulesOfIPGroupResponse.proto"
    workSpacesService
    (Proxy :: Proxy UpdateRulesOfIPGroup)

responseDeleteWorkspaceImage :: DeleteWorkspaceImageResponse -> TestTree
responseDeleteWorkspaceImage =
  res
    "DeleteWorkspaceImageResponse"
    "fixture/DeleteWorkspaceImageResponse.proto"
    workSpacesService
    (Proxy :: Proxy DeleteWorkspaceImage)

responseStopWorkspaces :: StopWorkspacesResponse -> TestTree
responseStopWorkspaces =
  res
    "StopWorkspacesResponse"
    "fixture/StopWorkspacesResponse.proto"
    workSpacesService
    (Proxy :: Proxy StopWorkspaces)

responseAssociateIPGroups :: AssociateIPGroupsResponse -> TestTree
responseAssociateIPGroups =
  res
    "AssociateIPGroupsResponse"
    "fixture/AssociateIPGroupsResponse.proto"
    workSpacesService
    (Proxy :: Proxy AssociateIPGroups)

responseModifySelfservicePermissions :: ModifySelfservicePermissionsResponse -> TestTree
responseModifySelfservicePermissions =
  res
    "ModifySelfservicePermissionsResponse"
    "fixture/ModifySelfservicePermissionsResponse.proto"
    workSpacesService
    (Proxy :: Proxy ModifySelfservicePermissions)

responseDeleteConnectionAlias :: DeleteConnectionAliasResponse -> TestTree
responseDeleteConnectionAlias =
  res
    "DeleteConnectionAliasResponse"
    "fixture/DeleteConnectionAliasResponse.proto"
    workSpacesService
    (Proxy :: Proxy DeleteConnectionAlias)

responseDescribeWorkspacesConnectionStatus :: DescribeWorkspacesConnectionStatusResponse -> TestTree
responseDescribeWorkspacesConnectionStatus =
  res
    "DescribeWorkspacesConnectionStatusResponse"
    "fixture/DescribeWorkspacesConnectionStatusResponse.proto"
    workSpacesService
    (Proxy :: Proxy DescribeWorkspacesConnectionStatus)

responseCreateConnectionAlias :: CreateConnectionAliasResponse -> TestTree
responseCreateConnectionAlias =
  res
    "CreateConnectionAliasResponse"
    "fixture/CreateConnectionAliasResponse.proto"
    workSpacesService
    (Proxy :: Proxy CreateConnectionAlias)

responseRebootWorkspaces :: RebootWorkspacesResponse -> TestTree
responseRebootWorkspaces =
  res
    "RebootWorkspacesResponse"
    "fixture/RebootWorkspacesResponse.proto"
    workSpacesService
    (Proxy :: Proxy RebootWorkspaces)

responseDeleteIPGroup :: DeleteIPGroupResponse -> TestTree
responseDeleteIPGroup =
  res
    "DeleteIPGroupResponse"
    "fixture/DeleteIPGroupResponse.proto"
    workSpacesService
    (Proxy :: Proxy DeleteIPGroup)

responseCopyWorkspaceImage :: CopyWorkspaceImageResponse -> TestTree
responseCopyWorkspaceImage =
  res
    "CopyWorkspaceImageResponse"
    "fixture/CopyWorkspaceImageResponse.proto"
    workSpacesService
    (Proxy :: Proxy CopyWorkspaceImage)

responseDescribeWorkspaceSnapshots :: DescribeWorkspaceSnapshotsResponse -> TestTree
responseDescribeWorkspaceSnapshots =
  res
    "DescribeWorkspaceSnapshotsResponse"
    "fixture/DescribeWorkspaceSnapshotsResponse.proto"
    workSpacesService
    (Proxy :: Proxy DescribeWorkspaceSnapshots)

responseTerminateWorkspaces :: TerminateWorkspacesResponse -> TestTree
responseTerminateWorkspaces =
  res
    "TerminateWorkspacesResponse"
    "fixture/TerminateWorkspacesResponse.proto"
    workSpacesService
    (Proxy :: Proxy TerminateWorkspaces)

responseUpdateConnectionAliasPermission :: UpdateConnectionAliasPermissionResponse -> TestTree
responseUpdateConnectionAliasPermission =
  res
    "UpdateConnectionAliasPermissionResponse"
    "fixture/UpdateConnectionAliasPermissionResponse.proto"
    workSpacesService
    (Proxy :: Proxy UpdateConnectionAliasPermission)

responseCreateWorkspaces :: CreateWorkspacesResponse -> TestTree
responseCreateWorkspaces =
  res
    "CreateWorkspacesResponse"
    "fixture/CreateWorkspacesResponse.proto"
    workSpacesService
    (Proxy :: Proxy CreateWorkspaces)

responseDescribeClientProperties :: DescribeClientPropertiesResponse -> TestTree
responseDescribeClientProperties =
  res
    "DescribeClientPropertiesResponse"
    "fixture/DescribeClientPropertiesResponse.proto"
    workSpacesService
    (Proxy :: Proxy DescribeClientProperties)

responseModifyClientProperties :: ModifyClientPropertiesResponse -> TestTree
responseModifyClientProperties =
  res
    "ModifyClientPropertiesResponse"
    "fixture/ModifyClientPropertiesResponse.proto"
    workSpacesService
    (Proxy :: Proxy ModifyClientProperties)

responseDescribeIPGroups :: DescribeIPGroupsResponse -> TestTree
responseDescribeIPGroups =
  res
    "DescribeIPGroupsResponse"
    "fixture/DescribeIPGroupsResponse.proto"
    workSpacesService
    (Proxy :: Proxy DescribeIPGroups)

responseListAvailableManagementCidrRanges :: ListAvailableManagementCidrRangesResponse -> TestTree
responseListAvailableManagementCidrRanges =
  res
    "ListAvailableManagementCidrRangesResponse"
    "fixture/ListAvailableManagementCidrRangesResponse.proto"
    workSpacesService
    (Proxy :: Proxy ListAvailableManagementCidrRanges)

responseUpdateWorkspaceImagePermission :: UpdateWorkspaceImagePermissionResponse -> TestTree
responseUpdateWorkspaceImagePermission =
  res
    "UpdateWorkspaceImagePermissionResponse"
    "fixture/UpdateWorkspaceImagePermissionResponse.proto"
    workSpacesService
    (Proxy :: Proxy UpdateWorkspaceImagePermission)

responseDescribeWorkspaces :: DescribeWorkspacesResponse -> TestTree
responseDescribeWorkspaces =
  res
    "DescribeWorkspacesResponse"
    "fixture/DescribeWorkspacesResponse.proto"
    workSpacesService
    (Proxy :: Proxy DescribeWorkspaces)

responseDescribeConnectionAliases :: DescribeConnectionAliasesResponse -> TestTree
responseDescribeConnectionAliases =
  res
    "DescribeConnectionAliasesResponse"
    "fixture/DescribeConnectionAliasesResponse.proto"
    workSpacesService
    (Proxy :: Proxy DescribeConnectionAliases)

responseStartWorkspaces :: StartWorkspacesResponse -> TestTree
responseStartWorkspaces =
  res
    "StartWorkspacesResponse"
    "fixture/StartWorkspacesResponse.proto"
    workSpacesService
    (Proxy :: Proxy StartWorkspaces)

responseDescribeAccountModifications :: DescribeAccountModificationsResponse -> TestTree
responseDescribeAccountModifications =
  res
    "DescribeAccountModificationsResponse"
    "fixture/DescribeAccountModificationsResponse.proto"
    workSpacesService
    (Proxy :: Proxy DescribeAccountModifications)

responseModifyAccount :: ModifyAccountResponse -> TestTree
responseModifyAccount =
  res
    "ModifyAccountResponse"
    "fixture/ModifyAccountResponse.proto"
    workSpacesService
    (Proxy :: Proxy ModifyAccount)
