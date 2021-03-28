{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.WorkSpaces
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Test.AWS.Gen.WorkSpaces where

import Data.Proxy
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.Tasty
import Network.AWS.WorkSpaces
import Test.AWS.WorkSpaces.Internal

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
--         , requestRevokeIpRules $
--             mkRevokeIpRules
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
--         , requestDisassociateIpGroups $
--             mkDisassociateIpGroups
--
--         , requestDescribeWorkspaceBundles $
--             mkDescribeWorkspaceBundles
--
--         , requestAuthorizeIpRules $
--             mkAuthorizeIpRules
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
--         , requestCreateIpGroup $
--             mkCreateIpGroup
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
--         , requestUpdateRulesOfIpGroup $
--             mkUpdateRulesOfIpGroup
--
--         , requestDeleteWorkspaceImage $
--             mkDeleteWorkspaceImage
--
--         , requestStopWorkspaces $
--             mkStopWorkspaces
--
--         , requestAssociateIpGroups $
--             mkAssociateIpGroups
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
--         , requestDeleteIpGroup $
--             mkDeleteIpGroup
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
--         , requestDescribeIpGroups $
--             mkDescribeIpGroups
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
--         , responseRevokeIpRules $
--             mkRevokeIpRulesResponse
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
--         , responseDisassociateIpGroups $
--             mkDisassociateIpGroupsResponse
--
--         , responseDescribeWorkspaceBundles $
--             mkDescribeWorkspaceBundlesResponse
--
--         , responseAuthorizeIpRules $
--             mkAuthorizeIpRulesResponse
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
--         , responseCreateIpGroup $
--             mkCreateIpGroupResponse
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
--         , responseUpdateRulesOfIpGroup $
--             mkUpdateRulesOfIpGroupResponse
--
--         , responseDeleteWorkspaceImage $
--             mkDeleteWorkspaceImageResponse
--
--         , responseStopWorkspaces $
--             mkStopWorkspacesResponse
--
--         , responseAssociateIpGroups $
--             mkAssociateIpGroupsResponse
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
--         , responseDeleteIpGroup $
--             mkDeleteIpGroupResponse
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
--         , responseDescribeIpGroups $
--             mkDescribeIpGroupsResponse
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
requestAssociateConnectionAlias = req
    "AssociateConnectionAlias"
    "fixture/AssociateConnectionAlias.yaml"

requestDescribeAccount :: DescribeAccount -> TestTree
requestDescribeAccount = req
    "DescribeAccount"
    "fixture/DescribeAccount.yaml"

requestRevokeIpRules :: RevokeIpRules -> TestTree
requestRevokeIpRules = req
    "RevokeIpRules"
    "fixture/RevokeIpRules.yaml"

requestDescribeWorkspaceImages :: DescribeWorkspaceImages -> TestTree
requestDescribeWorkspaceImages = req
    "DescribeWorkspaceImages"
    "fixture/DescribeWorkspaceImages.yaml"

requestModifyWorkspaceProperties :: ModifyWorkspaceProperties -> TestTree
requestModifyWorkspaceProperties = req
    "ModifyWorkspaceProperties"
    "fixture/ModifyWorkspaceProperties.yaml"

requestDeregisterWorkspaceDirectory :: DeregisterWorkspaceDirectory -> TestTree
requestDeregisterWorkspaceDirectory = req
    "DeregisterWorkspaceDirectory"
    "fixture/DeregisterWorkspaceDirectory.yaml"

requestMigrateWorkspace :: MigrateWorkspace -> TestTree
requestMigrateWorkspace = req
    "MigrateWorkspace"
    "fixture/MigrateWorkspace.yaml"

requestDescribeTags :: DescribeTags -> TestTree
requestDescribeTags = req
    "DescribeTags"
    "fixture/DescribeTags.yaml"

requestDescribeWorkspaceDirectories :: DescribeWorkspaceDirectories -> TestTree
requestDescribeWorkspaceDirectories = req
    "DescribeWorkspaceDirectories"
    "fixture/DescribeWorkspaceDirectories.yaml"

requestDisassociateIpGroups :: DisassociateIpGroups -> TestTree
requestDisassociateIpGroups = req
    "DisassociateIpGroups"
    "fixture/DisassociateIpGroups.yaml"

requestDescribeWorkspaceBundles :: DescribeWorkspaceBundles -> TestTree
requestDescribeWorkspaceBundles = req
    "DescribeWorkspaceBundles"
    "fixture/DescribeWorkspaceBundles.yaml"

requestAuthorizeIpRules :: AuthorizeIpRules -> TestTree
requestAuthorizeIpRules = req
    "AuthorizeIpRules"
    "fixture/AuthorizeIpRules.yaml"

requestDescribeWorkspaceImagePermissions :: DescribeWorkspaceImagePermissions -> TestTree
requestDescribeWorkspaceImagePermissions = req
    "DescribeWorkspaceImagePermissions"
    "fixture/DescribeWorkspaceImagePermissions.yaml"

requestRebuildWorkspaces :: RebuildWorkspaces -> TestTree
requestRebuildWorkspaces = req
    "RebuildWorkspaces"
    "fixture/RebuildWorkspaces.yaml"

requestImportWorkspaceImage :: ImportWorkspaceImage -> TestTree
requestImportWorkspaceImage = req
    "ImportWorkspaceImage"
    "fixture/ImportWorkspaceImage.yaml"

requestModifyWorkspaceState :: ModifyWorkspaceState -> TestTree
requestModifyWorkspaceState = req
    "ModifyWorkspaceState"
    "fixture/ModifyWorkspaceState.yaml"

requestCreateIpGroup :: CreateIpGroup -> TestTree
requestCreateIpGroup = req
    "CreateIpGroup"
    "fixture/CreateIpGroup.yaml"

requestDisassociateConnectionAlias :: DisassociateConnectionAlias -> TestTree
requestDisassociateConnectionAlias = req
    "DisassociateConnectionAlias"
    "fixture/DisassociateConnectionAlias.yaml"

requestModifyWorkspaceCreationProperties :: ModifyWorkspaceCreationProperties -> TestTree
requestModifyWorkspaceCreationProperties = req
    "ModifyWorkspaceCreationProperties"
    "fixture/ModifyWorkspaceCreationProperties.yaml"

requestRegisterWorkspaceDirectory :: RegisterWorkspaceDirectory -> TestTree
requestRegisterWorkspaceDirectory = req
    "RegisterWorkspaceDirectory"
    "fixture/RegisterWorkspaceDirectory.yaml"

requestRestoreWorkspace :: RestoreWorkspace -> TestTree
requestRestoreWorkspace = req
    "RestoreWorkspace"
    "fixture/RestoreWorkspace.yaml"

requestDescribeConnectionAliasPermissions :: DescribeConnectionAliasPermissions -> TestTree
requestDescribeConnectionAliasPermissions = req
    "DescribeConnectionAliasPermissions"
    "fixture/DescribeConnectionAliasPermissions.yaml"

requestCreateTags :: CreateTags -> TestTree
requestCreateTags = req
    "CreateTags"
    "fixture/CreateTags.yaml"

requestDeleteTags :: DeleteTags -> TestTree
requestDeleteTags = req
    "DeleteTags"
    "fixture/DeleteTags.yaml"

requestModifyWorkspaceAccessProperties :: ModifyWorkspaceAccessProperties -> TestTree
requestModifyWorkspaceAccessProperties = req
    "ModifyWorkspaceAccessProperties"
    "fixture/ModifyWorkspaceAccessProperties.yaml"

requestUpdateRulesOfIpGroup :: UpdateRulesOfIpGroup -> TestTree
requestUpdateRulesOfIpGroup = req
    "UpdateRulesOfIpGroup"
    "fixture/UpdateRulesOfIpGroup.yaml"

requestDeleteWorkspaceImage :: DeleteWorkspaceImage -> TestTree
requestDeleteWorkspaceImage = req
    "DeleteWorkspaceImage"
    "fixture/DeleteWorkspaceImage.yaml"

requestStopWorkspaces :: StopWorkspaces -> TestTree
requestStopWorkspaces = req
    "StopWorkspaces"
    "fixture/StopWorkspaces.yaml"

requestAssociateIpGroups :: AssociateIpGroups -> TestTree
requestAssociateIpGroups = req
    "AssociateIpGroups"
    "fixture/AssociateIpGroups.yaml"

requestModifySelfservicePermissions :: ModifySelfservicePermissions -> TestTree
requestModifySelfservicePermissions = req
    "ModifySelfservicePermissions"
    "fixture/ModifySelfservicePermissions.yaml"

requestDeleteConnectionAlias :: DeleteConnectionAlias -> TestTree
requestDeleteConnectionAlias = req
    "DeleteConnectionAlias"
    "fixture/DeleteConnectionAlias.yaml"

requestDescribeWorkspacesConnectionStatus :: DescribeWorkspacesConnectionStatus -> TestTree
requestDescribeWorkspacesConnectionStatus = req
    "DescribeWorkspacesConnectionStatus"
    "fixture/DescribeWorkspacesConnectionStatus.yaml"

requestCreateConnectionAlias :: CreateConnectionAlias -> TestTree
requestCreateConnectionAlias = req
    "CreateConnectionAlias"
    "fixture/CreateConnectionAlias.yaml"

requestRebootWorkspaces :: RebootWorkspaces -> TestTree
requestRebootWorkspaces = req
    "RebootWorkspaces"
    "fixture/RebootWorkspaces.yaml"

requestDeleteIpGroup :: DeleteIpGroup -> TestTree
requestDeleteIpGroup = req
    "DeleteIpGroup"
    "fixture/DeleteIpGroup.yaml"

requestCopyWorkspaceImage :: CopyWorkspaceImage -> TestTree
requestCopyWorkspaceImage = req
    "CopyWorkspaceImage"
    "fixture/CopyWorkspaceImage.yaml"

requestDescribeWorkspaceSnapshots :: DescribeWorkspaceSnapshots -> TestTree
requestDescribeWorkspaceSnapshots = req
    "DescribeWorkspaceSnapshots"
    "fixture/DescribeWorkspaceSnapshots.yaml"

requestTerminateWorkspaces :: TerminateWorkspaces -> TestTree
requestTerminateWorkspaces = req
    "TerminateWorkspaces"
    "fixture/TerminateWorkspaces.yaml"

requestUpdateConnectionAliasPermission :: UpdateConnectionAliasPermission -> TestTree
requestUpdateConnectionAliasPermission = req
    "UpdateConnectionAliasPermission"
    "fixture/UpdateConnectionAliasPermission.yaml"

requestCreateWorkspaces :: CreateWorkspaces -> TestTree
requestCreateWorkspaces = req
    "CreateWorkspaces"
    "fixture/CreateWorkspaces.yaml"

requestDescribeClientProperties :: DescribeClientProperties -> TestTree
requestDescribeClientProperties = req
    "DescribeClientProperties"
    "fixture/DescribeClientProperties.yaml"

requestModifyClientProperties :: ModifyClientProperties -> TestTree
requestModifyClientProperties = req
    "ModifyClientProperties"
    "fixture/ModifyClientProperties.yaml"

requestDescribeIpGroups :: DescribeIpGroups -> TestTree
requestDescribeIpGroups = req
    "DescribeIpGroups"
    "fixture/DescribeIpGroups.yaml"

requestListAvailableManagementCidrRanges :: ListAvailableManagementCidrRanges -> TestTree
requestListAvailableManagementCidrRanges = req
    "ListAvailableManagementCidrRanges"
    "fixture/ListAvailableManagementCidrRanges.yaml"

requestUpdateWorkspaceImagePermission :: UpdateWorkspaceImagePermission -> TestTree
requestUpdateWorkspaceImagePermission = req
    "UpdateWorkspaceImagePermission"
    "fixture/UpdateWorkspaceImagePermission.yaml"

requestDescribeWorkspaces :: DescribeWorkspaces -> TestTree
requestDescribeWorkspaces = req
    "DescribeWorkspaces"
    "fixture/DescribeWorkspaces.yaml"

requestDescribeConnectionAliases :: DescribeConnectionAliases -> TestTree
requestDescribeConnectionAliases = req
    "DescribeConnectionAliases"
    "fixture/DescribeConnectionAliases.yaml"

requestStartWorkspaces :: StartWorkspaces -> TestTree
requestStartWorkspaces = req
    "StartWorkspaces"
    "fixture/StartWorkspaces.yaml"

requestDescribeAccountModifications :: DescribeAccountModifications -> TestTree
requestDescribeAccountModifications = req
    "DescribeAccountModifications"
    "fixture/DescribeAccountModifications.yaml"

requestModifyAccount :: ModifyAccount -> TestTree
requestModifyAccount = req
    "ModifyAccount"
    "fixture/ModifyAccount.yaml"

-- Responses

responseAssociateConnectionAlias :: AssociateConnectionAliasResponse -> TestTree
responseAssociateConnectionAlias = res
    "AssociateConnectionAliasResponse"
    "fixture/AssociateConnectionAliasResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy AssociateConnectionAlias)

responseDescribeAccount :: DescribeAccountResponse -> TestTree
responseDescribeAccount = res
    "DescribeAccountResponse"
    "fixture/DescribeAccountResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeAccount)

responseRevokeIpRules :: RevokeIpRulesResponse -> TestTree
responseRevokeIpRules = res
    "RevokeIpRulesResponse"
    "fixture/RevokeIpRulesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy RevokeIpRules)

responseDescribeWorkspaceImages :: DescribeWorkspaceImagesResponse -> TestTree
responseDescribeWorkspaceImages = res
    "DescribeWorkspaceImagesResponse"
    "fixture/DescribeWorkspaceImagesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeWorkspaceImages)

responseModifyWorkspaceProperties :: ModifyWorkspacePropertiesResponse -> TestTree
responseModifyWorkspaceProperties = res
    "ModifyWorkspacePropertiesResponse"
    "fixture/ModifyWorkspacePropertiesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ModifyWorkspaceProperties)

responseDeregisterWorkspaceDirectory :: DeregisterWorkspaceDirectoryResponse -> TestTree
responseDeregisterWorkspaceDirectory = res
    "DeregisterWorkspaceDirectoryResponse"
    "fixture/DeregisterWorkspaceDirectoryResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeregisterWorkspaceDirectory)

responseMigrateWorkspace :: MigrateWorkspaceResponse -> TestTree
responseMigrateWorkspace = res
    "MigrateWorkspaceResponse"
    "fixture/MigrateWorkspaceResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy MigrateWorkspace)

responseDescribeTags :: DescribeTagsResponse -> TestTree
responseDescribeTags = res
    "DescribeTagsResponse"
    "fixture/DescribeTagsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeTags)

responseDescribeWorkspaceDirectories :: DescribeWorkspaceDirectoriesResponse -> TestTree
responseDescribeWorkspaceDirectories = res
    "DescribeWorkspaceDirectoriesResponse"
    "fixture/DescribeWorkspaceDirectoriesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeWorkspaceDirectories)

responseDisassociateIpGroups :: DisassociateIpGroupsResponse -> TestTree
responseDisassociateIpGroups = res
    "DisassociateIpGroupsResponse"
    "fixture/DisassociateIpGroupsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DisassociateIpGroups)

responseDescribeWorkspaceBundles :: DescribeWorkspaceBundlesResponse -> TestTree
responseDescribeWorkspaceBundles = res
    "DescribeWorkspaceBundlesResponse"
    "fixture/DescribeWorkspaceBundlesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeWorkspaceBundles)

responseAuthorizeIpRules :: AuthorizeIpRulesResponse -> TestTree
responseAuthorizeIpRules = res
    "AuthorizeIpRulesResponse"
    "fixture/AuthorizeIpRulesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy AuthorizeIpRules)

responseDescribeWorkspaceImagePermissions :: DescribeWorkspaceImagePermissionsResponse -> TestTree
responseDescribeWorkspaceImagePermissions = res
    "DescribeWorkspaceImagePermissionsResponse"
    "fixture/DescribeWorkspaceImagePermissionsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeWorkspaceImagePermissions)

responseRebuildWorkspaces :: RebuildWorkspacesResponse -> TestTree
responseRebuildWorkspaces = res
    "RebuildWorkspacesResponse"
    "fixture/RebuildWorkspacesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy RebuildWorkspaces)

responseImportWorkspaceImage :: ImportWorkspaceImageResponse -> TestTree
responseImportWorkspaceImage = res
    "ImportWorkspaceImageResponse"
    "fixture/ImportWorkspaceImageResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ImportWorkspaceImage)

responseModifyWorkspaceState :: ModifyWorkspaceStateResponse -> TestTree
responseModifyWorkspaceState = res
    "ModifyWorkspaceStateResponse"
    "fixture/ModifyWorkspaceStateResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ModifyWorkspaceState)

responseCreateIpGroup :: CreateIpGroupResponse -> TestTree
responseCreateIpGroup = res
    "CreateIpGroupResponse"
    "fixture/CreateIpGroupResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateIpGroup)

responseDisassociateConnectionAlias :: DisassociateConnectionAliasResponse -> TestTree
responseDisassociateConnectionAlias = res
    "DisassociateConnectionAliasResponse"
    "fixture/DisassociateConnectionAliasResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DisassociateConnectionAlias)

responseModifyWorkspaceCreationProperties :: ModifyWorkspaceCreationPropertiesResponse -> TestTree
responseModifyWorkspaceCreationProperties = res
    "ModifyWorkspaceCreationPropertiesResponse"
    "fixture/ModifyWorkspaceCreationPropertiesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ModifyWorkspaceCreationProperties)

responseRegisterWorkspaceDirectory :: RegisterWorkspaceDirectoryResponse -> TestTree
responseRegisterWorkspaceDirectory = res
    "RegisterWorkspaceDirectoryResponse"
    "fixture/RegisterWorkspaceDirectoryResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy RegisterWorkspaceDirectory)

responseRestoreWorkspace :: RestoreWorkspaceResponse -> TestTree
responseRestoreWorkspace = res
    "RestoreWorkspaceResponse"
    "fixture/RestoreWorkspaceResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy RestoreWorkspace)

responseDescribeConnectionAliasPermissions :: DescribeConnectionAliasPermissionsResponse -> TestTree
responseDescribeConnectionAliasPermissions = res
    "DescribeConnectionAliasPermissionsResponse"
    "fixture/DescribeConnectionAliasPermissionsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeConnectionAliasPermissions)

responseCreateTags :: CreateTagsResponse -> TestTree
responseCreateTags = res
    "CreateTagsResponse"
    "fixture/CreateTagsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateTags)

responseDeleteTags :: DeleteTagsResponse -> TestTree
responseDeleteTags = res
    "DeleteTagsResponse"
    "fixture/DeleteTagsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteTags)

responseModifyWorkspaceAccessProperties :: ModifyWorkspaceAccessPropertiesResponse -> TestTree
responseModifyWorkspaceAccessProperties = res
    "ModifyWorkspaceAccessPropertiesResponse"
    "fixture/ModifyWorkspaceAccessPropertiesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ModifyWorkspaceAccessProperties)

responseUpdateRulesOfIpGroup :: UpdateRulesOfIpGroupResponse -> TestTree
responseUpdateRulesOfIpGroup = res
    "UpdateRulesOfIpGroupResponse"
    "fixture/UpdateRulesOfIpGroupResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateRulesOfIpGroup)

responseDeleteWorkspaceImage :: DeleteWorkspaceImageResponse -> TestTree
responseDeleteWorkspaceImage = res
    "DeleteWorkspaceImageResponse"
    "fixture/DeleteWorkspaceImageResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteWorkspaceImage)

responseStopWorkspaces :: StopWorkspacesResponse -> TestTree
responseStopWorkspaces = res
    "StopWorkspacesResponse"
    "fixture/StopWorkspacesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy StopWorkspaces)

responseAssociateIpGroups :: AssociateIpGroupsResponse -> TestTree
responseAssociateIpGroups = res
    "AssociateIpGroupsResponse"
    "fixture/AssociateIpGroupsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy AssociateIpGroups)

responseModifySelfservicePermissions :: ModifySelfservicePermissionsResponse -> TestTree
responseModifySelfservicePermissions = res
    "ModifySelfservicePermissionsResponse"
    "fixture/ModifySelfservicePermissionsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ModifySelfservicePermissions)

responseDeleteConnectionAlias :: DeleteConnectionAliasResponse -> TestTree
responseDeleteConnectionAlias = res
    "DeleteConnectionAliasResponse"
    "fixture/DeleteConnectionAliasResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteConnectionAlias)

responseDescribeWorkspacesConnectionStatus :: DescribeWorkspacesConnectionStatusResponse -> TestTree
responseDescribeWorkspacesConnectionStatus = res
    "DescribeWorkspacesConnectionStatusResponse"
    "fixture/DescribeWorkspacesConnectionStatusResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeWorkspacesConnectionStatus)

responseCreateConnectionAlias :: CreateConnectionAliasResponse -> TestTree
responseCreateConnectionAlias = res
    "CreateConnectionAliasResponse"
    "fixture/CreateConnectionAliasResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateConnectionAlias)

responseRebootWorkspaces :: RebootWorkspacesResponse -> TestTree
responseRebootWorkspaces = res
    "RebootWorkspacesResponse"
    "fixture/RebootWorkspacesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy RebootWorkspaces)

responseDeleteIpGroup :: DeleteIpGroupResponse -> TestTree
responseDeleteIpGroup = res
    "DeleteIpGroupResponse"
    "fixture/DeleteIpGroupResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteIpGroup)

responseCopyWorkspaceImage :: CopyWorkspaceImageResponse -> TestTree
responseCopyWorkspaceImage = res
    "CopyWorkspaceImageResponse"
    "fixture/CopyWorkspaceImageResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CopyWorkspaceImage)

responseDescribeWorkspaceSnapshots :: DescribeWorkspaceSnapshotsResponse -> TestTree
responseDescribeWorkspaceSnapshots = res
    "DescribeWorkspaceSnapshotsResponse"
    "fixture/DescribeWorkspaceSnapshotsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeWorkspaceSnapshots)

responseTerminateWorkspaces :: TerminateWorkspacesResponse -> TestTree
responseTerminateWorkspaces = res
    "TerminateWorkspacesResponse"
    "fixture/TerminateWorkspacesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy TerminateWorkspaces)

responseUpdateConnectionAliasPermission :: UpdateConnectionAliasPermissionResponse -> TestTree
responseUpdateConnectionAliasPermission = res
    "UpdateConnectionAliasPermissionResponse"
    "fixture/UpdateConnectionAliasPermissionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateConnectionAliasPermission)

responseCreateWorkspaces :: CreateWorkspacesResponse -> TestTree
responseCreateWorkspaces = res
    "CreateWorkspacesResponse"
    "fixture/CreateWorkspacesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateWorkspaces)

responseDescribeClientProperties :: DescribeClientPropertiesResponse -> TestTree
responseDescribeClientProperties = res
    "DescribeClientPropertiesResponse"
    "fixture/DescribeClientPropertiesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeClientProperties)

responseModifyClientProperties :: ModifyClientPropertiesResponse -> TestTree
responseModifyClientProperties = res
    "ModifyClientPropertiesResponse"
    "fixture/ModifyClientPropertiesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ModifyClientProperties)

responseDescribeIpGroups :: DescribeIpGroupsResponse -> TestTree
responseDescribeIpGroups = res
    "DescribeIpGroupsResponse"
    "fixture/DescribeIpGroupsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeIpGroups)

responseListAvailableManagementCidrRanges :: ListAvailableManagementCidrRangesResponse -> TestTree
responseListAvailableManagementCidrRanges = res
    "ListAvailableManagementCidrRangesResponse"
    "fixture/ListAvailableManagementCidrRangesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListAvailableManagementCidrRanges)

responseUpdateWorkspaceImagePermission :: UpdateWorkspaceImagePermissionResponse -> TestTree
responseUpdateWorkspaceImagePermission = res
    "UpdateWorkspaceImagePermissionResponse"
    "fixture/UpdateWorkspaceImagePermissionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateWorkspaceImagePermission)

responseDescribeWorkspaces :: DescribeWorkspacesResponse -> TestTree
responseDescribeWorkspaces = res
    "DescribeWorkspacesResponse"
    "fixture/DescribeWorkspacesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeWorkspaces)

responseDescribeConnectionAliases :: DescribeConnectionAliasesResponse -> TestTree
responseDescribeConnectionAliases = res
    "DescribeConnectionAliasesResponse"
    "fixture/DescribeConnectionAliasesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeConnectionAliases)

responseStartWorkspaces :: StartWorkspacesResponse -> TestTree
responseStartWorkspaces = res
    "StartWorkspacesResponse"
    "fixture/StartWorkspacesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy StartWorkspaces)

responseDescribeAccountModifications :: DescribeAccountModificationsResponse -> TestTree
responseDescribeAccountModifications = res
    "DescribeAccountModificationsResponse"
    "fixture/DescribeAccountModificationsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeAccountModifications)

responseModifyAccount :: ModifyAccountResponse -> TestTree
responseModifyAccount = res
    "ModifyAccountResponse"
    "fixture/ModifyAccountResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ModifyAccount)
