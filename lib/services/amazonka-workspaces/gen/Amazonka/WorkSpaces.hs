{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.WorkSpaces
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2015-04-08@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- Amazon WorkSpaces Service
--
-- Amazon WorkSpaces enables you to provision virtual, cloud-based
-- Microsoft Windows or Amazon Linux desktops for your users, known as
-- /WorkSpaces/. WorkSpaces eliminates the need to procure and deploy
-- hardware or install complex software. You can quickly add or remove
-- users as your needs change. Users can access their virtual desktops from
-- multiple devices or web browsers.
--
-- This API Reference provides detailed information about the actions, data
-- types, parameters, and errors of the WorkSpaces service. For more
-- information about the supported Amazon Web Services Regions, endpoints,
-- and service quotas of the Amazon WorkSpaces service, see
-- <https://docs.aws.amazon.com/general/latest/gr/wsp.html WorkSpaces endpoints and quotas>
-- in the /Amazon Web Services General Reference/.
--
-- You can also manage your WorkSpaces resources using the WorkSpaces
-- console, Command Line Interface (CLI), and SDKs. For more information
-- about administering WorkSpaces, see the
-- <https://docs.aws.amazon.com/workspaces/latest/adminguide/ Amazon WorkSpaces Administration Guide>.
-- For more information about using the Amazon WorkSpaces client
-- application or web browser to access provisioned WorkSpaces, see the
-- <https://docs.aws.amazon.com/workspaces/latest/userguide/ Amazon WorkSpaces User Guide>.
-- For more information about using the CLI to manage your WorkSpaces
-- resources, see the
-- <https://docs.aws.amazon.com/cli/latest/reference/workspaces/index.html WorkSpaces section of the CLI Reference>.
module Amazonka.WorkSpaces
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** ResourceAlreadyExistsException
    _ResourceAlreadyExistsException,

    -- ** WorkspacesDefaultRoleNotFoundException
    _WorkspacesDefaultRoleNotFoundException,

    -- ** ResourceUnavailableException
    _ResourceUnavailableException,

    -- ** ResourceAssociatedException
    _ResourceAssociatedException,

    -- ** ResourceCreationFailedException
    _ResourceCreationFailedException,

    -- ** InvalidParameterValuesException
    _InvalidParameterValuesException,

    -- ** AccessDeniedException
    _AccessDeniedException,

    -- ** InvalidResourceStateException
    _InvalidResourceStateException,

    -- ** UnsupportedWorkspaceConfigurationException
    _UnsupportedWorkspaceConfigurationException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- ** ResourceLimitExceededException
    _ResourceLimitExceededException,

    -- ** OperationInProgressException
    _OperationInProgressException,

    -- ** UnsupportedNetworkConfigurationException
    _UnsupportedNetworkConfigurationException,

    -- ** OperationNotSupportedException
    _OperationNotSupportedException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** AssociateConnectionAlias
    AssociateConnectionAlias (AssociateConnectionAlias'),
    newAssociateConnectionAlias,
    AssociateConnectionAliasResponse (AssociateConnectionAliasResponse'),
    newAssociateConnectionAliasResponse,

    -- ** AssociateIpGroups
    AssociateIpGroups (AssociateIpGroups'),
    newAssociateIpGroups,
    AssociateIpGroupsResponse (AssociateIpGroupsResponse'),
    newAssociateIpGroupsResponse,

    -- ** AuthorizeIpRules
    AuthorizeIpRules (AuthorizeIpRules'),
    newAuthorizeIpRules,
    AuthorizeIpRulesResponse (AuthorizeIpRulesResponse'),
    newAuthorizeIpRulesResponse,

    -- ** CopyWorkspaceImage
    CopyWorkspaceImage (CopyWorkspaceImage'),
    newCopyWorkspaceImage,
    CopyWorkspaceImageResponse (CopyWorkspaceImageResponse'),
    newCopyWorkspaceImageResponse,

    -- ** CreateConnectClientAddIn
    CreateConnectClientAddIn (CreateConnectClientAddIn'),
    newCreateConnectClientAddIn,
    CreateConnectClientAddInResponse (CreateConnectClientAddInResponse'),
    newCreateConnectClientAddInResponse,

    -- ** CreateConnectionAlias
    CreateConnectionAlias (CreateConnectionAlias'),
    newCreateConnectionAlias,
    CreateConnectionAliasResponse (CreateConnectionAliasResponse'),
    newCreateConnectionAliasResponse,

    -- ** CreateIpGroup
    CreateIpGroup (CreateIpGroup'),
    newCreateIpGroup,
    CreateIpGroupResponse (CreateIpGroupResponse'),
    newCreateIpGroupResponse,

    -- ** CreateStandbyWorkspaces
    CreateStandbyWorkspaces (CreateStandbyWorkspaces'),
    newCreateStandbyWorkspaces,
    CreateStandbyWorkspacesResponse (CreateStandbyWorkspacesResponse'),
    newCreateStandbyWorkspacesResponse,

    -- ** CreateTags
    CreateTags (CreateTags'),
    newCreateTags,
    CreateTagsResponse (CreateTagsResponse'),
    newCreateTagsResponse,

    -- ** CreateUpdatedWorkspaceImage
    CreateUpdatedWorkspaceImage (CreateUpdatedWorkspaceImage'),
    newCreateUpdatedWorkspaceImage,
    CreateUpdatedWorkspaceImageResponse (CreateUpdatedWorkspaceImageResponse'),
    newCreateUpdatedWorkspaceImageResponse,

    -- ** CreateWorkspaceBundle
    CreateWorkspaceBundle (CreateWorkspaceBundle'),
    newCreateWorkspaceBundle,
    CreateWorkspaceBundleResponse (CreateWorkspaceBundleResponse'),
    newCreateWorkspaceBundleResponse,

    -- ** CreateWorkspaceImage
    CreateWorkspaceImage (CreateWorkspaceImage'),
    newCreateWorkspaceImage,
    CreateWorkspaceImageResponse (CreateWorkspaceImageResponse'),
    newCreateWorkspaceImageResponse,

    -- ** CreateWorkspaces
    CreateWorkspaces (CreateWorkspaces'),
    newCreateWorkspaces,
    CreateWorkspacesResponse (CreateWorkspacesResponse'),
    newCreateWorkspacesResponse,

    -- ** DeleteClientBranding
    DeleteClientBranding (DeleteClientBranding'),
    newDeleteClientBranding,
    DeleteClientBrandingResponse (DeleteClientBrandingResponse'),
    newDeleteClientBrandingResponse,

    -- ** DeleteConnectClientAddIn
    DeleteConnectClientAddIn (DeleteConnectClientAddIn'),
    newDeleteConnectClientAddIn,
    DeleteConnectClientAddInResponse (DeleteConnectClientAddInResponse'),
    newDeleteConnectClientAddInResponse,

    -- ** DeleteConnectionAlias
    DeleteConnectionAlias (DeleteConnectionAlias'),
    newDeleteConnectionAlias,
    DeleteConnectionAliasResponse (DeleteConnectionAliasResponse'),
    newDeleteConnectionAliasResponse,

    -- ** DeleteIpGroup
    DeleteIpGroup (DeleteIpGroup'),
    newDeleteIpGroup,
    DeleteIpGroupResponse (DeleteIpGroupResponse'),
    newDeleteIpGroupResponse,

    -- ** DeleteTags
    DeleteTags (DeleteTags'),
    newDeleteTags,
    DeleteTagsResponse (DeleteTagsResponse'),
    newDeleteTagsResponse,

    -- ** DeleteWorkspaceBundle
    DeleteWorkspaceBundle (DeleteWorkspaceBundle'),
    newDeleteWorkspaceBundle,
    DeleteWorkspaceBundleResponse (DeleteWorkspaceBundleResponse'),
    newDeleteWorkspaceBundleResponse,

    -- ** DeleteWorkspaceImage
    DeleteWorkspaceImage (DeleteWorkspaceImage'),
    newDeleteWorkspaceImage,
    DeleteWorkspaceImageResponse (DeleteWorkspaceImageResponse'),
    newDeleteWorkspaceImageResponse,

    -- ** DeregisterWorkspaceDirectory
    DeregisterWorkspaceDirectory (DeregisterWorkspaceDirectory'),
    newDeregisterWorkspaceDirectory,
    DeregisterWorkspaceDirectoryResponse (DeregisterWorkspaceDirectoryResponse'),
    newDeregisterWorkspaceDirectoryResponse,

    -- ** DescribeAccount
    DescribeAccount (DescribeAccount'),
    newDescribeAccount,
    DescribeAccountResponse (DescribeAccountResponse'),
    newDescribeAccountResponse,

    -- ** DescribeAccountModifications (Paginated)
    DescribeAccountModifications (DescribeAccountModifications'),
    newDescribeAccountModifications,
    DescribeAccountModificationsResponse (DescribeAccountModificationsResponse'),
    newDescribeAccountModificationsResponse,

    -- ** DescribeClientBranding
    DescribeClientBranding (DescribeClientBranding'),
    newDescribeClientBranding,
    DescribeClientBrandingResponse (DescribeClientBrandingResponse'),
    newDescribeClientBrandingResponse,

    -- ** DescribeClientProperties
    DescribeClientProperties (DescribeClientProperties'),
    newDescribeClientProperties,
    DescribeClientPropertiesResponse (DescribeClientPropertiesResponse'),
    newDescribeClientPropertiesResponse,

    -- ** DescribeConnectClientAddIns
    DescribeConnectClientAddIns (DescribeConnectClientAddIns'),
    newDescribeConnectClientAddIns,
    DescribeConnectClientAddInsResponse (DescribeConnectClientAddInsResponse'),
    newDescribeConnectClientAddInsResponse,

    -- ** DescribeConnectionAliasPermissions
    DescribeConnectionAliasPermissions (DescribeConnectionAliasPermissions'),
    newDescribeConnectionAliasPermissions,
    DescribeConnectionAliasPermissionsResponse (DescribeConnectionAliasPermissionsResponse'),
    newDescribeConnectionAliasPermissionsResponse,

    -- ** DescribeConnectionAliases
    DescribeConnectionAliases (DescribeConnectionAliases'),
    newDescribeConnectionAliases,
    DescribeConnectionAliasesResponse (DescribeConnectionAliasesResponse'),
    newDescribeConnectionAliasesResponse,

    -- ** DescribeIpGroups (Paginated)
    DescribeIpGroups (DescribeIpGroups'),
    newDescribeIpGroups,
    DescribeIpGroupsResponse (DescribeIpGroupsResponse'),
    newDescribeIpGroupsResponse,

    -- ** DescribeTags
    DescribeTags (DescribeTags'),
    newDescribeTags,
    DescribeTagsResponse (DescribeTagsResponse'),
    newDescribeTagsResponse,

    -- ** DescribeWorkspaceBundles (Paginated)
    DescribeWorkspaceBundles (DescribeWorkspaceBundles'),
    newDescribeWorkspaceBundles,
    DescribeWorkspaceBundlesResponse (DescribeWorkspaceBundlesResponse'),
    newDescribeWorkspaceBundlesResponse,

    -- ** DescribeWorkspaceDirectories (Paginated)
    DescribeWorkspaceDirectories (DescribeWorkspaceDirectories'),
    newDescribeWorkspaceDirectories,
    DescribeWorkspaceDirectoriesResponse (DescribeWorkspaceDirectoriesResponse'),
    newDescribeWorkspaceDirectoriesResponse,

    -- ** DescribeWorkspaceImagePermissions
    DescribeWorkspaceImagePermissions (DescribeWorkspaceImagePermissions'),
    newDescribeWorkspaceImagePermissions,
    DescribeWorkspaceImagePermissionsResponse (DescribeWorkspaceImagePermissionsResponse'),
    newDescribeWorkspaceImagePermissionsResponse,

    -- ** DescribeWorkspaceImages (Paginated)
    DescribeWorkspaceImages (DescribeWorkspaceImages'),
    newDescribeWorkspaceImages,
    DescribeWorkspaceImagesResponse (DescribeWorkspaceImagesResponse'),
    newDescribeWorkspaceImagesResponse,

    -- ** DescribeWorkspaceSnapshots
    DescribeWorkspaceSnapshots (DescribeWorkspaceSnapshots'),
    newDescribeWorkspaceSnapshots,
    DescribeWorkspaceSnapshotsResponse (DescribeWorkspaceSnapshotsResponse'),
    newDescribeWorkspaceSnapshotsResponse,

    -- ** DescribeWorkspaces (Paginated)
    DescribeWorkspaces (DescribeWorkspaces'),
    newDescribeWorkspaces,
    DescribeWorkspacesResponse (DescribeWorkspacesResponse'),
    newDescribeWorkspacesResponse,

    -- ** DescribeWorkspacesConnectionStatus (Paginated)
    DescribeWorkspacesConnectionStatus (DescribeWorkspacesConnectionStatus'),
    newDescribeWorkspacesConnectionStatus,
    DescribeWorkspacesConnectionStatusResponse (DescribeWorkspacesConnectionStatusResponse'),
    newDescribeWorkspacesConnectionStatusResponse,

    -- ** DisassociateConnectionAlias
    DisassociateConnectionAlias (DisassociateConnectionAlias'),
    newDisassociateConnectionAlias,
    DisassociateConnectionAliasResponse (DisassociateConnectionAliasResponse'),
    newDisassociateConnectionAliasResponse,

    -- ** DisassociateIpGroups
    DisassociateIpGroups (DisassociateIpGroups'),
    newDisassociateIpGroups,
    DisassociateIpGroupsResponse (DisassociateIpGroupsResponse'),
    newDisassociateIpGroupsResponse,

    -- ** ImportClientBranding
    ImportClientBranding (ImportClientBranding'),
    newImportClientBranding,
    ImportClientBrandingResponse (ImportClientBrandingResponse'),
    newImportClientBrandingResponse,

    -- ** ImportWorkspaceImage
    ImportWorkspaceImage (ImportWorkspaceImage'),
    newImportWorkspaceImage,
    ImportWorkspaceImageResponse (ImportWorkspaceImageResponse'),
    newImportWorkspaceImageResponse,

    -- ** ListAvailableManagementCidrRanges (Paginated)
    ListAvailableManagementCidrRanges (ListAvailableManagementCidrRanges'),
    newListAvailableManagementCidrRanges,
    ListAvailableManagementCidrRangesResponse (ListAvailableManagementCidrRangesResponse'),
    newListAvailableManagementCidrRangesResponse,

    -- ** MigrateWorkspace
    MigrateWorkspace (MigrateWorkspace'),
    newMigrateWorkspace,
    MigrateWorkspaceResponse (MigrateWorkspaceResponse'),
    newMigrateWorkspaceResponse,

    -- ** ModifyAccount
    ModifyAccount (ModifyAccount'),
    newModifyAccount,
    ModifyAccountResponse (ModifyAccountResponse'),
    newModifyAccountResponse,

    -- ** ModifyCertificateBasedAuthProperties
    ModifyCertificateBasedAuthProperties (ModifyCertificateBasedAuthProperties'),
    newModifyCertificateBasedAuthProperties,
    ModifyCertificateBasedAuthPropertiesResponse (ModifyCertificateBasedAuthPropertiesResponse'),
    newModifyCertificateBasedAuthPropertiesResponse,

    -- ** ModifyClientProperties
    ModifyClientProperties (ModifyClientProperties'),
    newModifyClientProperties,
    ModifyClientPropertiesResponse (ModifyClientPropertiesResponse'),
    newModifyClientPropertiesResponse,

    -- ** ModifySamlProperties
    ModifySamlProperties (ModifySamlProperties'),
    newModifySamlProperties,
    ModifySamlPropertiesResponse (ModifySamlPropertiesResponse'),
    newModifySamlPropertiesResponse,

    -- ** ModifySelfservicePermissions
    ModifySelfservicePermissions (ModifySelfservicePermissions'),
    newModifySelfservicePermissions,
    ModifySelfservicePermissionsResponse (ModifySelfservicePermissionsResponse'),
    newModifySelfservicePermissionsResponse,

    -- ** ModifyWorkspaceAccessProperties
    ModifyWorkspaceAccessProperties (ModifyWorkspaceAccessProperties'),
    newModifyWorkspaceAccessProperties,
    ModifyWorkspaceAccessPropertiesResponse (ModifyWorkspaceAccessPropertiesResponse'),
    newModifyWorkspaceAccessPropertiesResponse,

    -- ** ModifyWorkspaceCreationProperties
    ModifyWorkspaceCreationProperties (ModifyWorkspaceCreationProperties'),
    newModifyWorkspaceCreationProperties,
    ModifyWorkspaceCreationPropertiesResponse (ModifyWorkspaceCreationPropertiesResponse'),
    newModifyWorkspaceCreationPropertiesResponse,

    -- ** ModifyWorkspaceProperties
    ModifyWorkspaceProperties (ModifyWorkspaceProperties'),
    newModifyWorkspaceProperties,
    ModifyWorkspacePropertiesResponse (ModifyWorkspacePropertiesResponse'),
    newModifyWorkspacePropertiesResponse,

    -- ** ModifyWorkspaceState
    ModifyWorkspaceState (ModifyWorkspaceState'),
    newModifyWorkspaceState,
    ModifyWorkspaceStateResponse (ModifyWorkspaceStateResponse'),
    newModifyWorkspaceStateResponse,

    -- ** RebootWorkspaces
    RebootWorkspaces (RebootWorkspaces'),
    newRebootWorkspaces,
    RebootWorkspacesResponse (RebootWorkspacesResponse'),
    newRebootWorkspacesResponse,

    -- ** RebuildWorkspaces
    RebuildWorkspaces (RebuildWorkspaces'),
    newRebuildWorkspaces,
    RebuildWorkspacesResponse (RebuildWorkspacesResponse'),
    newRebuildWorkspacesResponse,

    -- ** RegisterWorkspaceDirectory
    RegisterWorkspaceDirectory (RegisterWorkspaceDirectory'),
    newRegisterWorkspaceDirectory,
    RegisterWorkspaceDirectoryResponse (RegisterWorkspaceDirectoryResponse'),
    newRegisterWorkspaceDirectoryResponse,

    -- ** RestoreWorkspace
    RestoreWorkspace (RestoreWorkspace'),
    newRestoreWorkspace,
    RestoreWorkspaceResponse (RestoreWorkspaceResponse'),
    newRestoreWorkspaceResponse,

    -- ** RevokeIpRules
    RevokeIpRules (RevokeIpRules'),
    newRevokeIpRules,
    RevokeIpRulesResponse (RevokeIpRulesResponse'),
    newRevokeIpRulesResponse,

    -- ** StartWorkspaces
    StartWorkspaces (StartWorkspaces'),
    newStartWorkspaces,
    StartWorkspacesResponse (StartWorkspacesResponse'),
    newStartWorkspacesResponse,

    -- ** StopWorkspaces
    StopWorkspaces (StopWorkspaces'),
    newStopWorkspaces,
    StopWorkspacesResponse (StopWorkspacesResponse'),
    newStopWorkspacesResponse,

    -- ** TerminateWorkspaces
    TerminateWorkspaces (TerminateWorkspaces'),
    newTerminateWorkspaces,
    TerminateWorkspacesResponse (TerminateWorkspacesResponse'),
    newTerminateWorkspacesResponse,

    -- ** UpdateConnectClientAddIn
    UpdateConnectClientAddIn (UpdateConnectClientAddIn'),
    newUpdateConnectClientAddIn,
    UpdateConnectClientAddInResponse (UpdateConnectClientAddInResponse'),
    newUpdateConnectClientAddInResponse,

    -- ** UpdateConnectionAliasPermission
    UpdateConnectionAliasPermission (UpdateConnectionAliasPermission'),
    newUpdateConnectionAliasPermission,
    UpdateConnectionAliasPermissionResponse (UpdateConnectionAliasPermissionResponse'),
    newUpdateConnectionAliasPermissionResponse,

    -- ** UpdateRulesOfIpGroup
    UpdateRulesOfIpGroup (UpdateRulesOfIpGroup'),
    newUpdateRulesOfIpGroup,
    UpdateRulesOfIpGroupResponse (UpdateRulesOfIpGroupResponse'),
    newUpdateRulesOfIpGroupResponse,

    -- ** UpdateWorkspaceBundle
    UpdateWorkspaceBundle (UpdateWorkspaceBundle'),
    newUpdateWorkspaceBundle,
    UpdateWorkspaceBundleResponse (UpdateWorkspaceBundleResponse'),
    newUpdateWorkspaceBundleResponse,

    -- ** UpdateWorkspaceImagePermission
    UpdateWorkspaceImagePermission (UpdateWorkspaceImagePermission'),
    newUpdateWorkspaceImagePermission,
    UpdateWorkspaceImagePermissionResponse (UpdateWorkspaceImagePermissionResponse'),
    newUpdateWorkspaceImagePermissionResponse,

    -- * Types

    -- ** AccessPropertyValue
    AccessPropertyValue (..),

    -- ** Application
    Application (..),

    -- ** AssociationStatus
    AssociationStatus (..),

    -- ** BundleType
    BundleType (..),

    -- ** CertificateBasedAuthStatusEnum
    CertificateBasedAuthStatusEnum (..),

    -- ** ClientDeviceType
    ClientDeviceType (..),

    -- ** Compute
    Compute (..),

    -- ** ConnectionAliasState
    ConnectionAliasState (..),

    -- ** ConnectionState
    ConnectionState (..),

    -- ** DedicatedTenancyModificationStateEnum
    DedicatedTenancyModificationStateEnum (..),

    -- ** DedicatedTenancySupportEnum
    DedicatedTenancySupportEnum (..),

    -- ** DedicatedTenancySupportResultEnum
    DedicatedTenancySupportResultEnum (..),

    -- ** DeletableCertificateBasedAuthProperty
    DeletableCertificateBasedAuthProperty (..),

    -- ** DeletableSamlProperty
    DeletableSamlProperty (..),

    -- ** ImageType
    ImageType (..),

    -- ** LogUploadEnum
    LogUploadEnum (..),

    -- ** ModificationResourceEnum
    ModificationResourceEnum (..),

    -- ** ModificationStateEnum
    ModificationStateEnum (..),

    -- ** OperatingSystemType
    OperatingSystemType (..),

    -- ** Protocol
    Protocol (..),

    -- ** ReconnectEnum
    ReconnectEnum (..),

    -- ** RunningMode
    RunningMode (..),

    -- ** SamlStatusEnum
    SamlStatusEnum (..),

    -- ** StandbyWorkspaceRelationshipType
    StandbyWorkspaceRelationshipType (..),

    -- ** TargetWorkspaceState
    TargetWorkspaceState (..),

    -- ** Tenancy
    Tenancy (..),

    -- ** WorkspaceBundleState
    WorkspaceBundleState (..),

    -- ** WorkspaceDirectoryState
    WorkspaceDirectoryState (..),

    -- ** WorkspaceDirectoryType
    WorkspaceDirectoryType (..),

    -- ** WorkspaceImageIngestionProcess
    WorkspaceImageIngestionProcess (..),

    -- ** WorkspaceImageRequiredTenancy
    WorkspaceImageRequiredTenancy (..),

    -- ** WorkspaceImageState
    WorkspaceImageState (..),

    -- ** WorkspaceState
    WorkspaceState (..),

    -- ** AccountModification
    AccountModification (AccountModification'),
    newAccountModification,

    -- ** CertificateBasedAuthProperties
    CertificateBasedAuthProperties (CertificateBasedAuthProperties'),
    newCertificateBasedAuthProperties,

    -- ** ClientProperties
    ClientProperties (ClientProperties'),
    newClientProperties,

    -- ** ClientPropertiesResult
    ClientPropertiesResult (ClientPropertiesResult'),
    newClientPropertiesResult,

    -- ** ComputeType
    ComputeType (ComputeType'),
    newComputeType,

    -- ** ConnectClientAddIn
    ConnectClientAddIn (ConnectClientAddIn'),
    newConnectClientAddIn,

    -- ** ConnectionAlias
    ConnectionAlias (ConnectionAlias'),
    newConnectionAlias,

    -- ** ConnectionAliasAssociation
    ConnectionAliasAssociation (ConnectionAliasAssociation'),
    newConnectionAliasAssociation,

    -- ** ConnectionAliasPermission
    ConnectionAliasPermission (ConnectionAliasPermission'),
    newConnectionAliasPermission,

    -- ** DefaultClientBrandingAttributes
    DefaultClientBrandingAttributes (DefaultClientBrandingAttributes'),
    newDefaultClientBrandingAttributes,

    -- ** DefaultImportClientBrandingAttributes
    DefaultImportClientBrandingAttributes (DefaultImportClientBrandingAttributes'),
    newDefaultImportClientBrandingAttributes,

    -- ** DefaultWorkspaceCreationProperties
    DefaultWorkspaceCreationProperties (DefaultWorkspaceCreationProperties'),
    newDefaultWorkspaceCreationProperties,

    -- ** FailedCreateStandbyWorkspacesRequest
    FailedCreateStandbyWorkspacesRequest (FailedCreateStandbyWorkspacesRequest'),
    newFailedCreateStandbyWorkspacesRequest,

    -- ** FailedCreateWorkspaceRequest
    FailedCreateWorkspaceRequest (FailedCreateWorkspaceRequest'),
    newFailedCreateWorkspaceRequest,

    -- ** FailedWorkspaceChangeRequest
    FailedWorkspaceChangeRequest (FailedWorkspaceChangeRequest'),
    newFailedWorkspaceChangeRequest,

    -- ** ImagePermission
    ImagePermission (ImagePermission'),
    newImagePermission,

    -- ** IosClientBrandingAttributes
    IosClientBrandingAttributes (IosClientBrandingAttributes'),
    newIosClientBrandingAttributes,

    -- ** IosImportClientBrandingAttributes
    IosImportClientBrandingAttributes (IosImportClientBrandingAttributes'),
    newIosImportClientBrandingAttributes,

    -- ** IpRuleItem
    IpRuleItem (IpRuleItem'),
    newIpRuleItem,

    -- ** ModificationState
    ModificationState (ModificationState'),
    newModificationState,

    -- ** OperatingSystem
    OperatingSystem (OperatingSystem'),
    newOperatingSystem,

    -- ** PendingCreateStandbyWorkspacesRequest
    PendingCreateStandbyWorkspacesRequest (PendingCreateStandbyWorkspacesRequest'),
    newPendingCreateStandbyWorkspacesRequest,

    -- ** RebootRequest
    RebootRequest (RebootRequest'),
    newRebootRequest,

    -- ** RebuildRequest
    RebuildRequest (RebuildRequest'),
    newRebuildRequest,

    -- ** RelatedWorkspaceProperties
    RelatedWorkspaceProperties (RelatedWorkspaceProperties'),
    newRelatedWorkspaceProperties,

    -- ** RootStorage
    RootStorage (RootStorage'),
    newRootStorage,

    -- ** SamlProperties
    SamlProperties (SamlProperties'),
    newSamlProperties,

    -- ** SelfservicePermissions
    SelfservicePermissions (SelfservicePermissions'),
    newSelfservicePermissions,

    -- ** Snapshot
    Snapshot (Snapshot'),
    newSnapshot,

    -- ** StandbyWorkspace
    StandbyWorkspace (StandbyWorkspace'),
    newStandbyWorkspace,

    -- ** StartRequest
    StartRequest (StartRequest'),
    newStartRequest,

    -- ** StopRequest
    StopRequest (StopRequest'),
    newStopRequest,

    -- ** Tag
    Tag (Tag'),
    newTag,

    -- ** TerminateRequest
    TerminateRequest (TerminateRequest'),
    newTerminateRequest,

    -- ** UpdateResult
    UpdateResult (UpdateResult'),
    newUpdateResult,

    -- ** UserStorage
    UserStorage (UserStorage'),
    newUserStorage,

    -- ** Workspace
    Workspace (Workspace'),
    newWorkspace,

    -- ** WorkspaceAccessProperties
    WorkspaceAccessProperties (WorkspaceAccessProperties'),
    newWorkspaceAccessProperties,

    -- ** WorkspaceBundle
    WorkspaceBundle (WorkspaceBundle'),
    newWorkspaceBundle,

    -- ** WorkspaceConnectionStatus
    WorkspaceConnectionStatus (WorkspaceConnectionStatus'),
    newWorkspaceConnectionStatus,

    -- ** WorkspaceCreationProperties
    WorkspaceCreationProperties (WorkspaceCreationProperties'),
    newWorkspaceCreationProperties,

    -- ** WorkspaceDirectory
    WorkspaceDirectory (WorkspaceDirectory'),
    newWorkspaceDirectory,

    -- ** WorkspaceImage
    WorkspaceImage (WorkspaceImage'),
    newWorkspaceImage,

    -- ** WorkspaceProperties
    WorkspaceProperties (WorkspaceProperties'),
    newWorkspaceProperties,

    -- ** WorkspaceRequest
    WorkspaceRequest (WorkspaceRequest'),
    newWorkspaceRequest,

    -- ** WorkspacesIpGroup
    WorkspacesIpGroup (WorkspacesIpGroup'),
    newWorkspacesIpGroup,
  )
where

import Amazonka.WorkSpaces.AssociateConnectionAlias
import Amazonka.WorkSpaces.AssociateIpGroups
import Amazonka.WorkSpaces.AuthorizeIpRules
import Amazonka.WorkSpaces.CopyWorkspaceImage
import Amazonka.WorkSpaces.CreateConnectClientAddIn
import Amazonka.WorkSpaces.CreateConnectionAlias
import Amazonka.WorkSpaces.CreateIpGroup
import Amazonka.WorkSpaces.CreateStandbyWorkspaces
import Amazonka.WorkSpaces.CreateTags
import Amazonka.WorkSpaces.CreateUpdatedWorkspaceImage
import Amazonka.WorkSpaces.CreateWorkspaceBundle
import Amazonka.WorkSpaces.CreateWorkspaceImage
import Amazonka.WorkSpaces.CreateWorkspaces
import Amazonka.WorkSpaces.DeleteClientBranding
import Amazonka.WorkSpaces.DeleteConnectClientAddIn
import Amazonka.WorkSpaces.DeleteConnectionAlias
import Amazonka.WorkSpaces.DeleteIpGroup
import Amazonka.WorkSpaces.DeleteTags
import Amazonka.WorkSpaces.DeleteWorkspaceBundle
import Amazonka.WorkSpaces.DeleteWorkspaceImage
import Amazonka.WorkSpaces.DeregisterWorkspaceDirectory
import Amazonka.WorkSpaces.DescribeAccount
import Amazonka.WorkSpaces.DescribeAccountModifications
import Amazonka.WorkSpaces.DescribeClientBranding
import Amazonka.WorkSpaces.DescribeClientProperties
import Amazonka.WorkSpaces.DescribeConnectClientAddIns
import Amazonka.WorkSpaces.DescribeConnectionAliasPermissions
import Amazonka.WorkSpaces.DescribeConnectionAliases
import Amazonka.WorkSpaces.DescribeIpGroups
import Amazonka.WorkSpaces.DescribeTags
import Amazonka.WorkSpaces.DescribeWorkspaceBundles
import Amazonka.WorkSpaces.DescribeWorkspaceDirectories
import Amazonka.WorkSpaces.DescribeWorkspaceImagePermissions
import Amazonka.WorkSpaces.DescribeWorkspaceImages
import Amazonka.WorkSpaces.DescribeWorkspaceSnapshots
import Amazonka.WorkSpaces.DescribeWorkspaces
import Amazonka.WorkSpaces.DescribeWorkspacesConnectionStatus
import Amazonka.WorkSpaces.DisassociateConnectionAlias
import Amazonka.WorkSpaces.DisassociateIpGroups
import Amazonka.WorkSpaces.ImportClientBranding
import Amazonka.WorkSpaces.ImportWorkspaceImage
import Amazonka.WorkSpaces.Lens
import Amazonka.WorkSpaces.ListAvailableManagementCidrRanges
import Amazonka.WorkSpaces.MigrateWorkspace
import Amazonka.WorkSpaces.ModifyAccount
import Amazonka.WorkSpaces.ModifyCertificateBasedAuthProperties
import Amazonka.WorkSpaces.ModifyClientProperties
import Amazonka.WorkSpaces.ModifySamlProperties
import Amazonka.WorkSpaces.ModifySelfservicePermissions
import Amazonka.WorkSpaces.ModifyWorkspaceAccessProperties
import Amazonka.WorkSpaces.ModifyWorkspaceCreationProperties
import Amazonka.WorkSpaces.ModifyWorkspaceProperties
import Amazonka.WorkSpaces.ModifyWorkspaceState
import Amazonka.WorkSpaces.RebootWorkspaces
import Amazonka.WorkSpaces.RebuildWorkspaces
import Amazonka.WorkSpaces.RegisterWorkspaceDirectory
import Amazonka.WorkSpaces.RestoreWorkspace
import Amazonka.WorkSpaces.RevokeIpRules
import Amazonka.WorkSpaces.StartWorkspaces
import Amazonka.WorkSpaces.StopWorkspaces
import Amazonka.WorkSpaces.TerminateWorkspaces
import Amazonka.WorkSpaces.Types
import Amazonka.WorkSpaces.UpdateConnectClientAddIn
import Amazonka.WorkSpaces.UpdateConnectionAliasPermission
import Amazonka.WorkSpaces.UpdateRulesOfIpGroup
import Amazonka.WorkSpaces.UpdateWorkspaceBundle
import Amazonka.WorkSpaces.UpdateWorkspaceImagePermission
import Amazonka.WorkSpaces.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'WorkSpaces'.

-- $operations
-- Some AWS operations return results that are incomplete and require subsequent
-- requests in order to obtain the entire result set. The process of sending
-- subsequent requests to continue where a previous request left off is called
-- pagination. For example, the 'ListObjects' operation of Amazon S3 returns up to
-- 1000 objects at a time, and you must send subsequent requests with the
-- appropriate Marker in order to retrieve the next page of results.
--
-- Operations that have an 'AWSPager' instance can transparently perform subsequent
-- requests, correctly setting Markers and other request facets to iterate through
-- the entire result set of a truncated API operation. Operations which support
-- this have an additional note in the documentation.
--
-- Many operations have the ability to filter results on the server side. See the
-- individual operation parameters for details.

-- $waiters
-- Waiters poll by repeatedly sending a request until some remote success condition
-- configured by the 'Wait' specification is fulfilled. The 'Wait' specification
-- determines how many attempts should be made, in addition to delay and retry strategies.
