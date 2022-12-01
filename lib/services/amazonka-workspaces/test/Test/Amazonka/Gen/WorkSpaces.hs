{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.WorkSpaces
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.WorkSpaces where

import Amazonka.WorkSpaces
import qualified Data.Proxy as Proxy
import Test.Amazonka.Fixture
import Test.Amazonka.Prelude
import Test.Amazonka.WorkSpaces.Internal
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
--         , requestAssociateIpGroups $
--             newAssociateIpGroups
--
--         , requestAuthorizeIpRules $
--             newAuthorizeIpRules
--
--         , requestCopyWorkspaceImage $
--             newCopyWorkspaceImage
--
--         , requestCreateConnectClientAddIn $
--             newCreateConnectClientAddIn
--
--         , requestCreateConnectionAlias $
--             newCreateConnectionAlias
--
--         , requestCreateIpGroup $
--             newCreateIpGroup
--
--         , requestCreateStandbyWorkspaces $
--             newCreateStandbyWorkspaces
--
--         , requestCreateTags $
--             newCreateTags
--
--         , requestCreateUpdatedWorkspaceImage $
--             newCreateUpdatedWorkspaceImage
--
--         , requestCreateWorkspaceBundle $
--             newCreateWorkspaceBundle
--
--         , requestCreateWorkspaceImage $
--             newCreateWorkspaceImage
--
--         , requestCreateWorkspaces $
--             newCreateWorkspaces
--
--         , requestDeleteClientBranding $
--             newDeleteClientBranding
--
--         , requestDeleteConnectClientAddIn $
--             newDeleteConnectClientAddIn
--
--         , requestDeleteConnectionAlias $
--             newDeleteConnectionAlias
--
--         , requestDeleteIpGroup $
--             newDeleteIpGroup
--
--         , requestDeleteTags $
--             newDeleteTags
--
--         , requestDeleteWorkspaceBundle $
--             newDeleteWorkspaceBundle
--
--         , requestDeleteWorkspaceImage $
--             newDeleteWorkspaceImage
--
--         , requestDeregisterWorkspaceDirectory $
--             newDeregisterWorkspaceDirectory
--
--         , requestDescribeAccount $
--             newDescribeAccount
--
--         , requestDescribeAccountModifications $
--             newDescribeAccountModifications
--
--         , requestDescribeClientBranding $
--             newDescribeClientBranding
--
--         , requestDescribeClientProperties $
--             newDescribeClientProperties
--
--         , requestDescribeConnectClientAddIns $
--             newDescribeConnectClientAddIns
--
--         , requestDescribeConnectionAliasPermissions $
--             newDescribeConnectionAliasPermissions
--
--         , requestDescribeConnectionAliases $
--             newDescribeConnectionAliases
--
--         , requestDescribeIpGroups $
--             newDescribeIpGroups
--
--         , requestDescribeTags $
--             newDescribeTags
--
--         , requestDescribeWorkspaceBundles $
--             newDescribeWorkspaceBundles
--
--         , requestDescribeWorkspaceDirectories $
--             newDescribeWorkspaceDirectories
--
--         , requestDescribeWorkspaceImagePermissions $
--             newDescribeWorkspaceImagePermissions
--
--         , requestDescribeWorkspaceImages $
--             newDescribeWorkspaceImages
--
--         , requestDescribeWorkspaceSnapshots $
--             newDescribeWorkspaceSnapshots
--
--         , requestDescribeWorkspaces $
--             newDescribeWorkspaces
--
--         , requestDescribeWorkspacesConnectionStatus $
--             newDescribeWorkspacesConnectionStatus
--
--         , requestDisassociateConnectionAlias $
--             newDisassociateConnectionAlias
--
--         , requestDisassociateIpGroups $
--             newDisassociateIpGroups
--
--         , requestImportClientBranding $
--             newImportClientBranding
--
--         , requestImportWorkspaceImage $
--             newImportWorkspaceImage
--
--         , requestListAvailableManagementCidrRanges $
--             newListAvailableManagementCidrRanges
--
--         , requestMigrateWorkspace $
--             newMigrateWorkspace
--
--         , requestModifyAccount $
--             newModifyAccount
--
--         , requestModifyCertificateBasedAuthProperties $
--             newModifyCertificateBasedAuthProperties
--
--         , requestModifyClientProperties $
--             newModifyClientProperties
--
--         , requestModifySamlProperties $
--             newModifySamlProperties
--
--         , requestModifySelfservicePermissions $
--             newModifySelfservicePermissions
--
--         , requestModifyWorkspaceAccessProperties $
--             newModifyWorkspaceAccessProperties
--
--         , requestModifyWorkspaceCreationProperties $
--             newModifyWorkspaceCreationProperties
--
--         , requestModifyWorkspaceProperties $
--             newModifyWorkspaceProperties
--
--         , requestModifyWorkspaceState $
--             newModifyWorkspaceState
--
--         , requestRebootWorkspaces $
--             newRebootWorkspaces
--
--         , requestRebuildWorkspaces $
--             newRebuildWorkspaces
--
--         , requestRegisterWorkspaceDirectory $
--             newRegisterWorkspaceDirectory
--
--         , requestRestoreWorkspace $
--             newRestoreWorkspace
--
--         , requestRevokeIpRules $
--             newRevokeIpRules
--
--         , requestStartWorkspaces $
--             newStartWorkspaces
--
--         , requestStopWorkspaces $
--             newStopWorkspaces
--
--         , requestTerminateWorkspaces $
--             newTerminateWorkspaces
--
--         , requestUpdateConnectClientAddIn $
--             newUpdateConnectClientAddIn
--
--         , requestUpdateConnectionAliasPermission $
--             newUpdateConnectionAliasPermission
--
--         , requestUpdateRulesOfIpGroup $
--             newUpdateRulesOfIpGroup
--
--         , requestUpdateWorkspaceBundle $
--             newUpdateWorkspaceBundle
--
--         , requestUpdateWorkspaceImagePermission $
--             newUpdateWorkspaceImagePermission
--
--           ]

--     , testGroup "response"
--         [ responseAssociateConnectionAlias $
--             newAssociateConnectionAliasResponse
--
--         , responseAssociateIpGroups $
--             newAssociateIpGroupsResponse
--
--         , responseAuthorizeIpRules $
--             newAuthorizeIpRulesResponse
--
--         , responseCopyWorkspaceImage $
--             newCopyWorkspaceImageResponse
--
--         , responseCreateConnectClientAddIn $
--             newCreateConnectClientAddInResponse
--
--         , responseCreateConnectionAlias $
--             newCreateConnectionAliasResponse
--
--         , responseCreateIpGroup $
--             newCreateIpGroupResponse
--
--         , responseCreateStandbyWorkspaces $
--             newCreateStandbyWorkspacesResponse
--
--         , responseCreateTags $
--             newCreateTagsResponse
--
--         , responseCreateUpdatedWorkspaceImage $
--             newCreateUpdatedWorkspaceImageResponse
--
--         , responseCreateWorkspaceBundle $
--             newCreateWorkspaceBundleResponse
--
--         , responseCreateWorkspaceImage $
--             newCreateWorkspaceImageResponse
--
--         , responseCreateWorkspaces $
--             newCreateWorkspacesResponse
--
--         , responseDeleteClientBranding $
--             newDeleteClientBrandingResponse
--
--         , responseDeleteConnectClientAddIn $
--             newDeleteConnectClientAddInResponse
--
--         , responseDeleteConnectionAlias $
--             newDeleteConnectionAliasResponse
--
--         , responseDeleteIpGroup $
--             newDeleteIpGroupResponse
--
--         , responseDeleteTags $
--             newDeleteTagsResponse
--
--         , responseDeleteWorkspaceBundle $
--             newDeleteWorkspaceBundleResponse
--
--         , responseDeleteWorkspaceImage $
--             newDeleteWorkspaceImageResponse
--
--         , responseDeregisterWorkspaceDirectory $
--             newDeregisterWorkspaceDirectoryResponse
--
--         , responseDescribeAccount $
--             newDescribeAccountResponse
--
--         , responseDescribeAccountModifications $
--             newDescribeAccountModificationsResponse
--
--         , responseDescribeClientBranding $
--             newDescribeClientBrandingResponse
--
--         , responseDescribeClientProperties $
--             newDescribeClientPropertiesResponse
--
--         , responseDescribeConnectClientAddIns $
--             newDescribeConnectClientAddInsResponse
--
--         , responseDescribeConnectionAliasPermissions $
--             newDescribeConnectionAliasPermissionsResponse
--
--         , responseDescribeConnectionAliases $
--             newDescribeConnectionAliasesResponse
--
--         , responseDescribeIpGroups $
--             newDescribeIpGroupsResponse
--
--         , responseDescribeTags $
--             newDescribeTagsResponse
--
--         , responseDescribeWorkspaceBundles $
--             newDescribeWorkspaceBundlesResponse
--
--         , responseDescribeWorkspaceDirectories $
--             newDescribeWorkspaceDirectoriesResponse
--
--         , responseDescribeWorkspaceImagePermissions $
--             newDescribeWorkspaceImagePermissionsResponse
--
--         , responseDescribeWorkspaceImages $
--             newDescribeWorkspaceImagesResponse
--
--         , responseDescribeWorkspaceSnapshots $
--             newDescribeWorkspaceSnapshotsResponse
--
--         , responseDescribeWorkspaces $
--             newDescribeWorkspacesResponse
--
--         , responseDescribeWorkspacesConnectionStatus $
--             newDescribeWorkspacesConnectionStatusResponse
--
--         , responseDisassociateConnectionAlias $
--             newDisassociateConnectionAliasResponse
--
--         , responseDisassociateIpGroups $
--             newDisassociateIpGroupsResponse
--
--         , responseImportClientBranding $
--             newImportClientBrandingResponse
--
--         , responseImportWorkspaceImage $
--             newImportWorkspaceImageResponse
--
--         , responseListAvailableManagementCidrRanges $
--             newListAvailableManagementCidrRangesResponse
--
--         , responseMigrateWorkspace $
--             newMigrateWorkspaceResponse
--
--         , responseModifyAccount $
--             newModifyAccountResponse
--
--         , responseModifyCertificateBasedAuthProperties $
--             newModifyCertificateBasedAuthPropertiesResponse
--
--         , responseModifyClientProperties $
--             newModifyClientPropertiesResponse
--
--         , responseModifySamlProperties $
--             newModifySamlPropertiesResponse
--
--         , responseModifySelfservicePermissions $
--             newModifySelfservicePermissionsResponse
--
--         , responseModifyWorkspaceAccessProperties $
--             newModifyWorkspaceAccessPropertiesResponse
--
--         , responseModifyWorkspaceCreationProperties $
--             newModifyWorkspaceCreationPropertiesResponse
--
--         , responseModifyWorkspaceProperties $
--             newModifyWorkspacePropertiesResponse
--
--         , responseModifyWorkspaceState $
--             newModifyWorkspaceStateResponse
--
--         , responseRebootWorkspaces $
--             newRebootWorkspacesResponse
--
--         , responseRebuildWorkspaces $
--             newRebuildWorkspacesResponse
--
--         , responseRegisterWorkspaceDirectory $
--             newRegisterWorkspaceDirectoryResponse
--
--         , responseRestoreWorkspace $
--             newRestoreWorkspaceResponse
--
--         , responseRevokeIpRules $
--             newRevokeIpRulesResponse
--
--         , responseStartWorkspaces $
--             newStartWorkspacesResponse
--
--         , responseStopWorkspaces $
--             newStopWorkspacesResponse
--
--         , responseTerminateWorkspaces $
--             newTerminateWorkspacesResponse
--
--         , responseUpdateConnectClientAddIn $
--             newUpdateConnectClientAddInResponse
--
--         , responseUpdateConnectionAliasPermission $
--             newUpdateConnectionAliasPermissionResponse
--
--         , responseUpdateRulesOfIpGroup $
--             newUpdateRulesOfIpGroupResponse
--
--         , responseUpdateWorkspaceBundle $
--             newUpdateWorkspaceBundleResponse
--
--         , responseUpdateWorkspaceImagePermission $
--             newUpdateWorkspaceImagePermissionResponse
--
--           ]
--     ]

-- Requests

requestAssociateConnectionAlias :: AssociateConnectionAlias -> TestTree
requestAssociateConnectionAlias =
  req
    "AssociateConnectionAlias"
    "fixture/AssociateConnectionAlias.yaml"

requestAssociateIpGroups :: AssociateIpGroups -> TestTree
requestAssociateIpGroups =
  req
    "AssociateIpGroups"
    "fixture/AssociateIpGroups.yaml"

requestAuthorizeIpRules :: AuthorizeIpRules -> TestTree
requestAuthorizeIpRules =
  req
    "AuthorizeIpRules"
    "fixture/AuthorizeIpRules.yaml"

requestCopyWorkspaceImage :: CopyWorkspaceImage -> TestTree
requestCopyWorkspaceImage =
  req
    "CopyWorkspaceImage"
    "fixture/CopyWorkspaceImage.yaml"

requestCreateConnectClientAddIn :: CreateConnectClientAddIn -> TestTree
requestCreateConnectClientAddIn =
  req
    "CreateConnectClientAddIn"
    "fixture/CreateConnectClientAddIn.yaml"

requestCreateConnectionAlias :: CreateConnectionAlias -> TestTree
requestCreateConnectionAlias =
  req
    "CreateConnectionAlias"
    "fixture/CreateConnectionAlias.yaml"

requestCreateIpGroup :: CreateIpGroup -> TestTree
requestCreateIpGroup =
  req
    "CreateIpGroup"
    "fixture/CreateIpGroup.yaml"

requestCreateStandbyWorkspaces :: CreateStandbyWorkspaces -> TestTree
requestCreateStandbyWorkspaces =
  req
    "CreateStandbyWorkspaces"
    "fixture/CreateStandbyWorkspaces.yaml"

requestCreateTags :: CreateTags -> TestTree
requestCreateTags =
  req
    "CreateTags"
    "fixture/CreateTags.yaml"

requestCreateUpdatedWorkspaceImage :: CreateUpdatedWorkspaceImage -> TestTree
requestCreateUpdatedWorkspaceImage =
  req
    "CreateUpdatedWorkspaceImage"
    "fixture/CreateUpdatedWorkspaceImage.yaml"

requestCreateWorkspaceBundle :: CreateWorkspaceBundle -> TestTree
requestCreateWorkspaceBundle =
  req
    "CreateWorkspaceBundle"
    "fixture/CreateWorkspaceBundle.yaml"

requestCreateWorkspaceImage :: CreateWorkspaceImage -> TestTree
requestCreateWorkspaceImage =
  req
    "CreateWorkspaceImage"
    "fixture/CreateWorkspaceImage.yaml"

requestCreateWorkspaces :: CreateWorkspaces -> TestTree
requestCreateWorkspaces =
  req
    "CreateWorkspaces"
    "fixture/CreateWorkspaces.yaml"

requestDeleteClientBranding :: DeleteClientBranding -> TestTree
requestDeleteClientBranding =
  req
    "DeleteClientBranding"
    "fixture/DeleteClientBranding.yaml"

requestDeleteConnectClientAddIn :: DeleteConnectClientAddIn -> TestTree
requestDeleteConnectClientAddIn =
  req
    "DeleteConnectClientAddIn"
    "fixture/DeleteConnectClientAddIn.yaml"

requestDeleteConnectionAlias :: DeleteConnectionAlias -> TestTree
requestDeleteConnectionAlias =
  req
    "DeleteConnectionAlias"
    "fixture/DeleteConnectionAlias.yaml"

requestDeleteIpGroup :: DeleteIpGroup -> TestTree
requestDeleteIpGroup =
  req
    "DeleteIpGroup"
    "fixture/DeleteIpGroup.yaml"

requestDeleteTags :: DeleteTags -> TestTree
requestDeleteTags =
  req
    "DeleteTags"
    "fixture/DeleteTags.yaml"

requestDeleteWorkspaceBundle :: DeleteWorkspaceBundle -> TestTree
requestDeleteWorkspaceBundle =
  req
    "DeleteWorkspaceBundle"
    "fixture/DeleteWorkspaceBundle.yaml"

requestDeleteWorkspaceImage :: DeleteWorkspaceImage -> TestTree
requestDeleteWorkspaceImage =
  req
    "DeleteWorkspaceImage"
    "fixture/DeleteWorkspaceImage.yaml"

requestDeregisterWorkspaceDirectory :: DeregisterWorkspaceDirectory -> TestTree
requestDeregisterWorkspaceDirectory =
  req
    "DeregisterWorkspaceDirectory"
    "fixture/DeregisterWorkspaceDirectory.yaml"

requestDescribeAccount :: DescribeAccount -> TestTree
requestDescribeAccount =
  req
    "DescribeAccount"
    "fixture/DescribeAccount.yaml"

requestDescribeAccountModifications :: DescribeAccountModifications -> TestTree
requestDescribeAccountModifications =
  req
    "DescribeAccountModifications"
    "fixture/DescribeAccountModifications.yaml"

requestDescribeClientBranding :: DescribeClientBranding -> TestTree
requestDescribeClientBranding =
  req
    "DescribeClientBranding"
    "fixture/DescribeClientBranding.yaml"

requestDescribeClientProperties :: DescribeClientProperties -> TestTree
requestDescribeClientProperties =
  req
    "DescribeClientProperties"
    "fixture/DescribeClientProperties.yaml"

requestDescribeConnectClientAddIns :: DescribeConnectClientAddIns -> TestTree
requestDescribeConnectClientAddIns =
  req
    "DescribeConnectClientAddIns"
    "fixture/DescribeConnectClientAddIns.yaml"

requestDescribeConnectionAliasPermissions :: DescribeConnectionAliasPermissions -> TestTree
requestDescribeConnectionAliasPermissions =
  req
    "DescribeConnectionAliasPermissions"
    "fixture/DescribeConnectionAliasPermissions.yaml"

requestDescribeConnectionAliases :: DescribeConnectionAliases -> TestTree
requestDescribeConnectionAliases =
  req
    "DescribeConnectionAliases"
    "fixture/DescribeConnectionAliases.yaml"

requestDescribeIpGroups :: DescribeIpGroups -> TestTree
requestDescribeIpGroups =
  req
    "DescribeIpGroups"
    "fixture/DescribeIpGroups.yaml"

requestDescribeTags :: DescribeTags -> TestTree
requestDescribeTags =
  req
    "DescribeTags"
    "fixture/DescribeTags.yaml"

requestDescribeWorkspaceBundles :: DescribeWorkspaceBundles -> TestTree
requestDescribeWorkspaceBundles =
  req
    "DescribeWorkspaceBundles"
    "fixture/DescribeWorkspaceBundles.yaml"

requestDescribeWorkspaceDirectories :: DescribeWorkspaceDirectories -> TestTree
requestDescribeWorkspaceDirectories =
  req
    "DescribeWorkspaceDirectories"
    "fixture/DescribeWorkspaceDirectories.yaml"

requestDescribeWorkspaceImagePermissions :: DescribeWorkspaceImagePermissions -> TestTree
requestDescribeWorkspaceImagePermissions =
  req
    "DescribeWorkspaceImagePermissions"
    "fixture/DescribeWorkspaceImagePermissions.yaml"

requestDescribeWorkspaceImages :: DescribeWorkspaceImages -> TestTree
requestDescribeWorkspaceImages =
  req
    "DescribeWorkspaceImages"
    "fixture/DescribeWorkspaceImages.yaml"

requestDescribeWorkspaceSnapshots :: DescribeWorkspaceSnapshots -> TestTree
requestDescribeWorkspaceSnapshots =
  req
    "DescribeWorkspaceSnapshots"
    "fixture/DescribeWorkspaceSnapshots.yaml"

requestDescribeWorkspaces :: DescribeWorkspaces -> TestTree
requestDescribeWorkspaces =
  req
    "DescribeWorkspaces"
    "fixture/DescribeWorkspaces.yaml"

requestDescribeWorkspacesConnectionStatus :: DescribeWorkspacesConnectionStatus -> TestTree
requestDescribeWorkspacesConnectionStatus =
  req
    "DescribeWorkspacesConnectionStatus"
    "fixture/DescribeWorkspacesConnectionStatus.yaml"

requestDisassociateConnectionAlias :: DisassociateConnectionAlias -> TestTree
requestDisassociateConnectionAlias =
  req
    "DisassociateConnectionAlias"
    "fixture/DisassociateConnectionAlias.yaml"

requestDisassociateIpGroups :: DisassociateIpGroups -> TestTree
requestDisassociateIpGroups =
  req
    "DisassociateIpGroups"
    "fixture/DisassociateIpGroups.yaml"

requestImportClientBranding :: ImportClientBranding -> TestTree
requestImportClientBranding =
  req
    "ImportClientBranding"
    "fixture/ImportClientBranding.yaml"

requestImportWorkspaceImage :: ImportWorkspaceImage -> TestTree
requestImportWorkspaceImage =
  req
    "ImportWorkspaceImage"
    "fixture/ImportWorkspaceImage.yaml"

requestListAvailableManagementCidrRanges :: ListAvailableManagementCidrRanges -> TestTree
requestListAvailableManagementCidrRanges =
  req
    "ListAvailableManagementCidrRanges"
    "fixture/ListAvailableManagementCidrRanges.yaml"

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

requestModifyCertificateBasedAuthProperties :: ModifyCertificateBasedAuthProperties -> TestTree
requestModifyCertificateBasedAuthProperties =
  req
    "ModifyCertificateBasedAuthProperties"
    "fixture/ModifyCertificateBasedAuthProperties.yaml"

requestModifyClientProperties :: ModifyClientProperties -> TestTree
requestModifyClientProperties =
  req
    "ModifyClientProperties"
    "fixture/ModifyClientProperties.yaml"

requestModifySamlProperties :: ModifySamlProperties -> TestTree
requestModifySamlProperties =
  req
    "ModifySamlProperties"
    "fixture/ModifySamlProperties.yaml"

requestModifySelfservicePermissions :: ModifySelfservicePermissions -> TestTree
requestModifySelfservicePermissions =
  req
    "ModifySelfservicePermissions"
    "fixture/ModifySelfservicePermissions.yaml"

requestModifyWorkspaceAccessProperties :: ModifyWorkspaceAccessProperties -> TestTree
requestModifyWorkspaceAccessProperties =
  req
    "ModifyWorkspaceAccessProperties"
    "fixture/ModifyWorkspaceAccessProperties.yaml"

requestModifyWorkspaceCreationProperties :: ModifyWorkspaceCreationProperties -> TestTree
requestModifyWorkspaceCreationProperties =
  req
    "ModifyWorkspaceCreationProperties"
    "fixture/ModifyWorkspaceCreationProperties.yaml"

requestModifyWorkspaceProperties :: ModifyWorkspaceProperties -> TestTree
requestModifyWorkspaceProperties =
  req
    "ModifyWorkspaceProperties"
    "fixture/ModifyWorkspaceProperties.yaml"

requestModifyWorkspaceState :: ModifyWorkspaceState -> TestTree
requestModifyWorkspaceState =
  req
    "ModifyWorkspaceState"
    "fixture/ModifyWorkspaceState.yaml"

requestRebootWorkspaces :: RebootWorkspaces -> TestTree
requestRebootWorkspaces =
  req
    "RebootWorkspaces"
    "fixture/RebootWorkspaces.yaml"

requestRebuildWorkspaces :: RebuildWorkspaces -> TestTree
requestRebuildWorkspaces =
  req
    "RebuildWorkspaces"
    "fixture/RebuildWorkspaces.yaml"

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

requestRevokeIpRules :: RevokeIpRules -> TestTree
requestRevokeIpRules =
  req
    "RevokeIpRules"
    "fixture/RevokeIpRules.yaml"

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

requestTerminateWorkspaces :: TerminateWorkspaces -> TestTree
requestTerminateWorkspaces =
  req
    "TerminateWorkspaces"
    "fixture/TerminateWorkspaces.yaml"

requestUpdateConnectClientAddIn :: UpdateConnectClientAddIn -> TestTree
requestUpdateConnectClientAddIn =
  req
    "UpdateConnectClientAddIn"
    "fixture/UpdateConnectClientAddIn.yaml"

requestUpdateConnectionAliasPermission :: UpdateConnectionAliasPermission -> TestTree
requestUpdateConnectionAliasPermission =
  req
    "UpdateConnectionAliasPermission"
    "fixture/UpdateConnectionAliasPermission.yaml"

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

requestUpdateWorkspaceImagePermission :: UpdateWorkspaceImagePermission -> TestTree
requestUpdateWorkspaceImagePermission =
  req
    "UpdateWorkspaceImagePermission"
    "fixture/UpdateWorkspaceImagePermission.yaml"

-- Responses

responseAssociateConnectionAlias :: AssociateConnectionAliasResponse -> TestTree
responseAssociateConnectionAlias =
  res
    "AssociateConnectionAliasResponse"
    "fixture/AssociateConnectionAliasResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssociateConnectionAlias)

responseAssociateIpGroups :: AssociateIpGroupsResponse -> TestTree
responseAssociateIpGroups =
  res
    "AssociateIpGroupsResponse"
    "fixture/AssociateIpGroupsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssociateIpGroups)

responseAuthorizeIpRules :: AuthorizeIpRulesResponse -> TestTree
responseAuthorizeIpRules =
  res
    "AuthorizeIpRulesResponse"
    "fixture/AuthorizeIpRulesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AuthorizeIpRules)

responseCopyWorkspaceImage :: CopyWorkspaceImageResponse -> TestTree
responseCopyWorkspaceImage =
  res
    "CopyWorkspaceImageResponse"
    "fixture/CopyWorkspaceImageResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CopyWorkspaceImage)

responseCreateConnectClientAddIn :: CreateConnectClientAddInResponse -> TestTree
responseCreateConnectClientAddIn =
  res
    "CreateConnectClientAddInResponse"
    "fixture/CreateConnectClientAddInResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateConnectClientAddIn)

responseCreateConnectionAlias :: CreateConnectionAliasResponse -> TestTree
responseCreateConnectionAlias =
  res
    "CreateConnectionAliasResponse"
    "fixture/CreateConnectionAliasResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateConnectionAlias)

responseCreateIpGroup :: CreateIpGroupResponse -> TestTree
responseCreateIpGroup =
  res
    "CreateIpGroupResponse"
    "fixture/CreateIpGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateIpGroup)

responseCreateStandbyWorkspaces :: CreateStandbyWorkspacesResponse -> TestTree
responseCreateStandbyWorkspaces =
  res
    "CreateStandbyWorkspacesResponse"
    "fixture/CreateStandbyWorkspacesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateStandbyWorkspaces)

responseCreateTags :: CreateTagsResponse -> TestTree
responseCreateTags =
  res
    "CreateTagsResponse"
    "fixture/CreateTagsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateTags)

responseCreateUpdatedWorkspaceImage :: CreateUpdatedWorkspaceImageResponse -> TestTree
responseCreateUpdatedWorkspaceImage =
  res
    "CreateUpdatedWorkspaceImageResponse"
    "fixture/CreateUpdatedWorkspaceImageResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateUpdatedWorkspaceImage)

responseCreateWorkspaceBundle :: CreateWorkspaceBundleResponse -> TestTree
responseCreateWorkspaceBundle =
  res
    "CreateWorkspaceBundleResponse"
    "fixture/CreateWorkspaceBundleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateWorkspaceBundle)

responseCreateWorkspaceImage :: CreateWorkspaceImageResponse -> TestTree
responseCreateWorkspaceImage =
  res
    "CreateWorkspaceImageResponse"
    "fixture/CreateWorkspaceImageResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateWorkspaceImage)

responseCreateWorkspaces :: CreateWorkspacesResponse -> TestTree
responseCreateWorkspaces =
  res
    "CreateWorkspacesResponse"
    "fixture/CreateWorkspacesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateWorkspaces)

responseDeleteClientBranding :: DeleteClientBrandingResponse -> TestTree
responseDeleteClientBranding =
  res
    "DeleteClientBrandingResponse"
    "fixture/DeleteClientBrandingResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteClientBranding)

responseDeleteConnectClientAddIn :: DeleteConnectClientAddInResponse -> TestTree
responseDeleteConnectClientAddIn =
  res
    "DeleteConnectClientAddInResponse"
    "fixture/DeleteConnectClientAddInResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteConnectClientAddIn)

responseDeleteConnectionAlias :: DeleteConnectionAliasResponse -> TestTree
responseDeleteConnectionAlias =
  res
    "DeleteConnectionAliasResponse"
    "fixture/DeleteConnectionAliasResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteConnectionAlias)

responseDeleteIpGroup :: DeleteIpGroupResponse -> TestTree
responseDeleteIpGroup =
  res
    "DeleteIpGroupResponse"
    "fixture/DeleteIpGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteIpGroup)

responseDeleteTags :: DeleteTagsResponse -> TestTree
responseDeleteTags =
  res
    "DeleteTagsResponse"
    "fixture/DeleteTagsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteTags)

responseDeleteWorkspaceBundle :: DeleteWorkspaceBundleResponse -> TestTree
responseDeleteWorkspaceBundle =
  res
    "DeleteWorkspaceBundleResponse"
    "fixture/DeleteWorkspaceBundleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteWorkspaceBundle)

responseDeleteWorkspaceImage :: DeleteWorkspaceImageResponse -> TestTree
responseDeleteWorkspaceImage =
  res
    "DeleteWorkspaceImageResponse"
    "fixture/DeleteWorkspaceImageResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteWorkspaceImage)

responseDeregisterWorkspaceDirectory :: DeregisterWorkspaceDirectoryResponse -> TestTree
responseDeregisterWorkspaceDirectory =
  res
    "DeregisterWorkspaceDirectoryResponse"
    "fixture/DeregisterWorkspaceDirectoryResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeregisterWorkspaceDirectory)

responseDescribeAccount :: DescribeAccountResponse -> TestTree
responseDescribeAccount =
  res
    "DescribeAccountResponse"
    "fixture/DescribeAccountResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeAccount)

responseDescribeAccountModifications :: DescribeAccountModificationsResponse -> TestTree
responseDescribeAccountModifications =
  res
    "DescribeAccountModificationsResponse"
    "fixture/DescribeAccountModificationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeAccountModifications)

responseDescribeClientBranding :: DescribeClientBrandingResponse -> TestTree
responseDescribeClientBranding =
  res
    "DescribeClientBrandingResponse"
    "fixture/DescribeClientBrandingResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeClientBranding)

responseDescribeClientProperties :: DescribeClientPropertiesResponse -> TestTree
responseDescribeClientProperties =
  res
    "DescribeClientPropertiesResponse"
    "fixture/DescribeClientPropertiesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeClientProperties)

responseDescribeConnectClientAddIns :: DescribeConnectClientAddInsResponse -> TestTree
responseDescribeConnectClientAddIns =
  res
    "DescribeConnectClientAddInsResponse"
    "fixture/DescribeConnectClientAddInsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeConnectClientAddIns)

responseDescribeConnectionAliasPermissions :: DescribeConnectionAliasPermissionsResponse -> TestTree
responseDescribeConnectionAliasPermissions =
  res
    "DescribeConnectionAliasPermissionsResponse"
    "fixture/DescribeConnectionAliasPermissionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeConnectionAliasPermissions)

responseDescribeConnectionAliases :: DescribeConnectionAliasesResponse -> TestTree
responseDescribeConnectionAliases =
  res
    "DescribeConnectionAliasesResponse"
    "fixture/DescribeConnectionAliasesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeConnectionAliases)

responseDescribeIpGroups :: DescribeIpGroupsResponse -> TestTree
responseDescribeIpGroups =
  res
    "DescribeIpGroupsResponse"
    "fixture/DescribeIpGroupsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeIpGroups)

responseDescribeTags :: DescribeTagsResponse -> TestTree
responseDescribeTags =
  res
    "DescribeTagsResponse"
    "fixture/DescribeTagsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeTags)

responseDescribeWorkspaceBundles :: DescribeWorkspaceBundlesResponse -> TestTree
responseDescribeWorkspaceBundles =
  res
    "DescribeWorkspaceBundlesResponse"
    "fixture/DescribeWorkspaceBundlesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeWorkspaceBundles)

responseDescribeWorkspaceDirectories :: DescribeWorkspaceDirectoriesResponse -> TestTree
responseDescribeWorkspaceDirectories =
  res
    "DescribeWorkspaceDirectoriesResponse"
    "fixture/DescribeWorkspaceDirectoriesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeWorkspaceDirectories)

responseDescribeWorkspaceImagePermissions :: DescribeWorkspaceImagePermissionsResponse -> TestTree
responseDescribeWorkspaceImagePermissions =
  res
    "DescribeWorkspaceImagePermissionsResponse"
    "fixture/DescribeWorkspaceImagePermissionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeWorkspaceImagePermissions)

responseDescribeWorkspaceImages :: DescribeWorkspaceImagesResponse -> TestTree
responseDescribeWorkspaceImages =
  res
    "DescribeWorkspaceImagesResponse"
    "fixture/DescribeWorkspaceImagesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeWorkspaceImages)

responseDescribeWorkspaceSnapshots :: DescribeWorkspaceSnapshotsResponse -> TestTree
responseDescribeWorkspaceSnapshots =
  res
    "DescribeWorkspaceSnapshotsResponse"
    "fixture/DescribeWorkspaceSnapshotsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeWorkspaceSnapshots)

responseDescribeWorkspaces :: DescribeWorkspacesResponse -> TestTree
responseDescribeWorkspaces =
  res
    "DescribeWorkspacesResponse"
    "fixture/DescribeWorkspacesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeWorkspaces)

responseDescribeWorkspacesConnectionStatus :: DescribeWorkspacesConnectionStatusResponse -> TestTree
responseDescribeWorkspacesConnectionStatus =
  res
    "DescribeWorkspacesConnectionStatusResponse"
    "fixture/DescribeWorkspacesConnectionStatusResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeWorkspacesConnectionStatus)

responseDisassociateConnectionAlias :: DisassociateConnectionAliasResponse -> TestTree
responseDisassociateConnectionAlias =
  res
    "DisassociateConnectionAliasResponse"
    "fixture/DisassociateConnectionAliasResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisassociateConnectionAlias)

responseDisassociateIpGroups :: DisassociateIpGroupsResponse -> TestTree
responseDisassociateIpGroups =
  res
    "DisassociateIpGroupsResponse"
    "fixture/DisassociateIpGroupsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisassociateIpGroups)

responseImportClientBranding :: ImportClientBrandingResponse -> TestTree
responseImportClientBranding =
  res
    "ImportClientBrandingResponse"
    "fixture/ImportClientBrandingResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ImportClientBranding)

responseImportWorkspaceImage :: ImportWorkspaceImageResponse -> TestTree
responseImportWorkspaceImage =
  res
    "ImportWorkspaceImageResponse"
    "fixture/ImportWorkspaceImageResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ImportWorkspaceImage)

responseListAvailableManagementCidrRanges :: ListAvailableManagementCidrRangesResponse -> TestTree
responseListAvailableManagementCidrRanges =
  res
    "ListAvailableManagementCidrRangesResponse"
    "fixture/ListAvailableManagementCidrRangesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListAvailableManagementCidrRanges)

responseMigrateWorkspace :: MigrateWorkspaceResponse -> TestTree
responseMigrateWorkspace =
  res
    "MigrateWorkspaceResponse"
    "fixture/MigrateWorkspaceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy MigrateWorkspace)

responseModifyAccount :: ModifyAccountResponse -> TestTree
responseModifyAccount =
  res
    "ModifyAccountResponse"
    "fixture/ModifyAccountResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ModifyAccount)

responseModifyCertificateBasedAuthProperties :: ModifyCertificateBasedAuthPropertiesResponse -> TestTree
responseModifyCertificateBasedAuthProperties =
  res
    "ModifyCertificateBasedAuthPropertiesResponse"
    "fixture/ModifyCertificateBasedAuthPropertiesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ModifyCertificateBasedAuthProperties)

responseModifyClientProperties :: ModifyClientPropertiesResponse -> TestTree
responseModifyClientProperties =
  res
    "ModifyClientPropertiesResponse"
    "fixture/ModifyClientPropertiesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ModifyClientProperties)

responseModifySamlProperties :: ModifySamlPropertiesResponse -> TestTree
responseModifySamlProperties =
  res
    "ModifySamlPropertiesResponse"
    "fixture/ModifySamlPropertiesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ModifySamlProperties)

responseModifySelfservicePermissions :: ModifySelfservicePermissionsResponse -> TestTree
responseModifySelfservicePermissions =
  res
    "ModifySelfservicePermissionsResponse"
    "fixture/ModifySelfservicePermissionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ModifySelfservicePermissions)

responseModifyWorkspaceAccessProperties :: ModifyWorkspaceAccessPropertiesResponse -> TestTree
responseModifyWorkspaceAccessProperties =
  res
    "ModifyWorkspaceAccessPropertiesResponse"
    "fixture/ModifyWorkspaceAccessPropertiesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ModifyWorkspaceAccessProperties)

responseModifyWorkspaceCreationProperties :: ModifyWorkspaceCreationPropertiesResponse -> TestTree
responseModifyWorkspaceCreationProperties =
  res
    "ModifyWorkspaceCreationPropertiesResponse"
    "fixture/ModifyWorkspaceCreationPropertiesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ModifyWorkspaceCreationProperties)

responseModifyWorkspaceProperties :: ModifyWorkspacePropertiesResponse -> TestTree
responseModifyWorkspaceProperties =
  res
    "ModifyWorkspacePropertiesResponse"
    "fixture/ModifyWorkspacePropertiesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ModifyWorkspaceProperties)

responseModifyWorkspaceState :: ModifyWorkspaceStateResponse -> TestTree
responseModifyWorkspaceState =
  res
    "ModifyWorkspaceStateResponse"
    "fixture/ModifyWorkspaceStateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ModifyWorkspaceState)

responseRebootWorkspaces :: RebootWorkspacesResponse -> TestTree
responseRebootWorkspaces =
  res
    "RebootWorkspacesResponse"
    "fixture/RebootWorkspacesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RebootWorkspaces)

responseRebuildWorkspaces :: RebuildWorkspacesResponse -> TestTree
responseRebuildWorkspaces =
  res
    "RebuildWorkspacesResponse"
    "fixture/RebuildWorkspacesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RebuildWorkspaces)

responseRegisterWorkspaceDirectory :: RegisterWorkspaceDirectoryResponse -> TestTree
responseRegisterWorkspaceDirectory =
  res
    "RegisterWorkspaceDirectoryResponse"
    "fixture/RegisterWorkspaceDirectoryResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RegisterWorkspaceDirectory)

responseRestoreWorkspace :: RestoreWorkspaceResponse -> TestTree
responseRestoreWorkspace =
  res
    "RestoreWorkspaceResponse"
    "fixture/RestoreWorkspaceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RestoreWorkspace)

responseRevokeIpRules :: RevokeIpRulesResponse -> TestTree
responseRevokeIpRules =
  res
    "RevokeIpRulesResponse"
    "fixture/RevokeIpRulesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RevokeIpRules)

responseStartWorkspaces :: StartWorkspacesResponse -> TestTree
responseStartWorkspaces =
  res
    "StartWorkspacesResponse"
    "fixture/StartWorkspacesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartWorkspaces)

responseStopWorkspaces :: StopWorkspacesResponse -> TestTree
responseStopWorkspaces =
  res
    "StopWorkspacesResponse"
    "fixture/StopWorkspacesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StopWorkspaces)

responseTerminateWorkspaces :: TerminateWorkspacesResponse -> TestTree
responseTerminateWorkspaces =
  res
    "TerminateWorkspacesResponse"
    "fixture/TerminateWorkspacesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TerminateWorkspaces)

responseUpdateConnectClientAddIn :: UpdateConnectClientAddInResponse -> TestTree
responseUpdateConnectClientAddIn =
  res
    "UpdateConnectClientAddInResponse"
    "fixture/UpdateConnectClientAddInResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateConnectClientAddIn)

responseUpdateConnectionAliasPermission :: UpdateConnectionAliasPermissionResponse -> TestTree
responseUpdateConnectionAliasPermission =
  res
    "UpdateConnectionAliasPermissionResponse"
    "fixture/UpdateConnectionAliasPermissionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateConnectionAliasPermission)

responseUpdateRulesOfIpGroup :: UpdateRulesOfIpGroupResponse -> TestTree
responseUpdateRulesOfIpGroup =
  res
    "UpdateRulesOfIpGroupResponse"
    "fixture/UpdateRulesOfIpGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateRulesOfIpGroup)

responseUpdateWorkspaceBundle :: UpdateWorkspaceBundleResponse -> TestTree
responseUpdateWorkspaceBundle =
  res
    "UpdateWorkspaceBundleResponse"
    "fixture/UpdateWorkspaceBundleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateWorkspaceBundle)

responseUpdateWorkspaceImagePermission :: UpdateWorkspaceImagePermissionResponse -> TestTree
responseUpdateWorkspaceImagePermission =
  res
    "UpdateWorkspaceImagePermissionResponse"
    "fixture/UpdateWorkspaceImagePermissionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateWorkspaceImagePermission)
