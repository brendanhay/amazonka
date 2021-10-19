{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Network.AWS.WorkSpaces
-- Copyright   : (c) 2013-2021 Brendan Hay
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
-- Microsoft Windows and Amazon Linux desktops for your users.
module Network.AWS.WorkSpaces
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** UnsupportedNetworkConfigurationException
    _UnsupportedNetworkConfigurationException,

    -- ** AccessDeniedException
    _AccessDeniedException,

    -- ** ResourceCreationFailedException
    _ResourceCreationFailedException,

    -- ** ResourceUnavailableException
    _ResourceUnavailableException,

    -- ** InvalidParameterValuesException
    _InvalidParameterValuesException,

    -- ** ResourceAssociatedException
    _ResourceAssociatedException,

    -- ** OperationInProgressException
    _OperationInProgressException,

    -- ** ResourceAlreadyExistsException
    _ResourceAlreadyExistsException,

    -- ** ResourceLimitExceededException
    _ResourceLimitExceededException,

    -- ** InvalidResourceStateException
    _InvalidResourceStateException,

    -- ** OperationNotSupportedException
    _OperationNotSupportedException,

    -- ** UnsupportedWorkspaceConfigurationException
    _UnsupportedWorkspaceConfigurationException,

    -- ** WorkspacesDefaultRoleNotFoundException
    _WorkspacesDefaultRoleNotFoundException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** AssociateConnectionAlias
    AssociateConnectionAlias (AssociateConnectionAlias'),
    newAssociateConnectionAlias,
    AssociateConnectionAliasResponse (AssociateConnectionAliasResponse'),
    newAssociateConnectionAliasResponse,

    -- ** DescribeAccount
    DescribeAccount (DescribeAccount'),
    newDescribeAccount,
    DescribeAccountResponse (DescribeAccountResponse'),
    newDescribeAccountResponse,

    -- ** RevokeIpRules
    RevokeIpRules (RevokeIpRules'),
    newRevokeIpRules,
    RevokeIpRulesResponse (RevokeIpRulesResponse'),
    newRevokeIpRulesResponse,

    -- ** DescribeWorkspaceImages (Paginated)
    DescribeWorkspaceImages (DescribeWorkspaceImages'),
    newDescribeWorkspaceImages,
    DescribeWorkspaceImagesResponse (DescribeWorkspaceImagesResponse'),
    newDescribeWorkspaceImagesResponse,

    -- ** ModifyWorkspaceProperties
    ModifyWorkspaceProperties (ModifyWorkspaceProperties'),
    newModifyWorkspaceProperties,
    ModifyWorkspacePropertiesResponse (ModifyWorkspacePropertiesResponse'),
    newModifyWorkspacePropertiesResponse,

    -- ** DeregisterWorkspaceDirectory
    DeregisterWorkspaceDirectory (DeregisterWorkspaceDirectory'),
    newDeregisterWorkspaceDirectory,
    DeregisterWorkspaceDirectoryResponse (DeregisterWorkspaceDirectoryResponse'),
    newDeregisterWorkspaceDirectoryResponse,

    -- ** MigrateWorkspace
    MigrateWorkspace (MigrateWorkspace'),
    newMigrateWorkspace,
    MigrateWorkspaceResponse (MigrateWorkspaceResponse'),
    newMigrateWorkspaceResponse,

    -- ** DescribeTags
    DescribeTags (DescribeTags'),
    newDescribeTags,
    DescribeTagsResponse (DescribeTagsResponse'),
    newDescribeTagsResponse,

    -- ** DescribeWorkspaceDirectories (Paginated)
    DescribeWorkspaceDirectories (DescribeWorkspaceDirectories'),
    newDescribeWorkspaceDirectories,
    DescribeWorkspaceDirectoriesResponse (DescribeWorkspaceDirectoriesResponse'),
    newDescribeWorkspaceDirectoriesResponse,

    -- ** DisassociateIpGroups
    DisassociateIpGroups (DisassociateIpGroups'),
    newDisassociateIpGroups,
    DisassociateIpGroupsResponse (DisassociateIpGroupsResponse'),
    newDisassociateIpGroupsResponse,

    -- ** DescribeWorkspaceBundles (Paginated)
    DescribeWorkspaceBundles (DescribeWorkspaceBundles'),
    newDescribeWorkspaceBundles,
    DescribeWorkspaceBundlesResponse (DescribeWorkspaceBundlesResponse'),
    newDescribeWorkspaceBundlesResponse,

    -- ** AuthorizeIpRules
    AuthorizeIpRules (AuthorizeIpRules'),
    newAuthorizeIpRules,
    AuthorizeIpRulesResponse (AuthorizeIpRulesResponse'),
    newAuthorizeIpRulesResponse,

    -- ** DescribeWorkspaceImagePermissions
    DescribeWorkspaceImagePermissions (DescribeWorkspaceImagePermissions'),
    newDescribeWorkspaceImagePermissions,
    DescribeWorkspaceImagePermissionsResponse (DescribeWorkspaceImagePermissionsResponse'),
    newDescribeWorkspaceImagePermissionsResponse,

    -- ** RebuildWorkspaces
    RebuildWorkspaces (RebuildWorkspaces'),
    newRebuildWorkspaces,
    RebuildWorkspacesResponse (RebuildWorkspacesResponse'),
    newRebuildWorkspacesResponse,

    -- ** ImportWorkspaceImage
    ImportWorkspaceImage (ImportWorkspaceImage'),
    newImportWorkspaceImage,
    ImportWorkspaceImageResponse (ImportWorkspaceImageResponse'),
    newImportWorkspaceImageResponse,

    -- ** ModifyWorkspaceState
    ModifyWorkspaceState (ModifyWorkspaceState'),
    newModifyWorkspaceState,
    ModifyWorkspaceStateResponse (ModifyWorkspaceStateResponse'),
    newModifyWorkspaceStateResponse,

    -- ** CreateIpGroup
    CreateIpGroup (CreateIpGroup'),
    newCreateIpGroup,
    CreateIpGroupResponse (CreateIpGroupResponse'),
    newCreateIpGroupResponse,

    -- ** DisassociateConnectionAlias
    DisassociateConnectionAlias (DisassociateConnectionAlias'),
    newDisassociateConnectionAlias,
    DisassociateConnectionAliasResponse (DisassociateConnectionAliasResponse'),
    newDisassociateConnectionAliasResponse,

    -- ** ModifyWorkspaceCreationProperties
    ModifyWorkspaceCreationProperties (ModifyWorkspaceCreationProperties'),
    newModifyWorkspaceCreationProperties,
    ModifyWorkspaceCreationPropertiesResponse (ModifyWorkspaceCreationPropertiesResponse'),
    newModifyWorkspaceCreationPropertiesResponse,

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

    -- ** DescribeConnectionAliasPermissions
    DescribeConnectionAliasPermissions (DescribeConnectionAliasPermissions'),
    newDescribeConnectionAliasPermissions,
    DescribeConnectionAliasPermissionsResponse (DescribeConnectionAliasPermissionsResponse'),
    newDescribeConnectionAliasPermissionsResponse,

    -- ** CreateTags
    CreateTags (CreateTags'),
    newCreateTags,
    CreateTagsResponse (CreateTagsResponse'),
    newCreateTagsResponse,

    -- ** CreateWorkspaceBundle
    CreateWorkspaceBundle (CreateWorkspaceBundle'),
    newCreateWorkspaceBundle,
    CreateWorkspaceBundleResponse (CreateWorkspaceBundleResponse'),
    newCreateWorkspaceBundleResponse,

    -- ** DeleteTags
    DeleteTags (DeleteTags'),
    newDeleteTags,
    DeleteTagsResponse (DeleteTagsResponse'),
    newDeleteTagsResponse,

    -- ** ModifyWorkspaceAccessProperties
    ModifyWorkspaceAccessProperties (ModifyWorkspaceAccessProperties'),
    newModifyWorkspaceAccessProperties,
    ModifyWorkspaceAccessPropertiesResponse (ModifyWorkspaceAccessPropertiesResponse'),
    newModifyWorkspaceAccessPropertiesResponse,

    -- ** UpdateRulesOfIpGroup
    UpdateRulesOfIpGroup (UpdateRulesOfIpGroup'),
    newUpdateRulesOfIpGroup,
    UpdateRulesOfIpGroupResponse (UpdateRulesOfIpGroupResponse'),
    newUpdateRulesOfIpGroupResponse,

    -- ** DeleteWorkspaceImage
    DeleteWorkspaceImage (DeleteWorkspaceImage'),
    newDeleteWorkspaceImage,
    DeleteWorkspaceImageResponse (DeleteWorkspaceImageResponse'),
    newDeleteWorkspaceImageResponse,

    -- ** StopWorkspaces
    StopWorkspaces (StopWorkspaces'),
    newStopWorkspaces,
    StopWorkspacesResponse (StopWorkspacesResponse'),
    newStopWorkspacesResponse,

    -- ** AssociateIpGroups
    AssociateIpGroups (AssociateIpGroups'),
    newAssociateIpGroups,
    AssociateIpGroupsResponse (AssociateIpGroupsResponse'),
    newAssociateIpGroupsResponse,

    -- ** ModifySelfservicePermissions
    ModifySelfservicePermissions (ModifySelfservicePermissions'),
    newModifySelfservicePermissions,
    ModifySelfservicePermissionsResponse (ModifySelfservicePermissionsResponse'),
    newModifySelfservicePermissionsResponse,

    -- ** DeleteConnectionAlias
    DeleteConnectionAlias (DeleteConnectionAlias'),
    newDeleteConnectionAlias,
    DeleteConnectionAliasResponse (DeleteConnectionAliasResponse'),
    newDeleteConnectionAliasResponse,

    -- ** DescribeWorkspacesConnectionStatus (Paginated)
    DescribeWorkspacesConnectionStatus (DescribeWorkspacesConnectionStatus'),
    newDescribeWorkspacesConnectionStatus,
    DescribeWorkspacesConnectionStatusResponse (DescribeWorkspacesConnectionStatusResponse'),
    newDescribeWorkspacesConnectionStatusResponse,

    -- ** CreateConnectionAlias
    CreateConnectionAlias (CreateConnectionAlias'),
    newCreateConnectionAlias,
    CreateConnectionAliasResponse (CreateConnectionAliasResponse'),
    newCreateConnectionAliasResponse,

    -- ** RebootWorkspaces
    RebootWorkspaces (RebootWorkspaces'),
    newRebootWorkspaces,
    RebootWorkspacesResponse (RebootWorkspacesResponse'),
    newRebootWorkspacesResponse,

    -- ** DeleteIpGroup
    DeleteIpGroup (DeleteIpGroup'),
    newDeleteIpGroup,
    DeleteIpGroupResponse (DeleteIpGroupResponse'),
    newDeleteIpGroupResponse,

    -- ** CopyWorkspaceImage
    CopyWorkspaceImage (CopyWorkspaceImage'),
    newCopyWorkspaceImage,
    CopyWorkspaceImageResponse (CopyWorkspaceImageResponse'),
    newCopyWorkspaceImageResponse,

    -- ** DescribeWorkspaceSnapshots
    DescribeWorkspaceSnapshots (DescribeWorkspaceSnapshots'),
    newDescribeWorkspaceSnapshots,
    DescribeWorkspaceSnapshotsResponse (DescribeWorkspaceSnapshotsResponse'),
    newDescribeWorkspaceSnapshotsResponse,

    -- ** TerminateWorkspaces
    TerminateWorkspaces (TerminateWorkspaces'),
    newTerminateWorkspaces,
    TerminateWorkspacesResponse (TerminateWorkspacesResponse'),
    newTerminateWorkspacesResponse,

    -- ** UpdateConnectionAliasPermission
    UpdateConnectionAliasPermission (UpdateConnectionAliasPermission'),
    newUpdateConnectionAliasPermission,
    UpdateConnectionAliasPermissionResponse (UpdateConnectionAliasPermissionResponse'),
    newUpdateConnectionAliasPermissionResponse,

    -- ** CreateWorkspaces
    CreateWorkspaces (CreateWorkspaces'),
    newCreateWorkspaces,
    CreateWorkspacesResponse (CreateWorkspacesResponse'),
    newCreateWorkspacesResponse,

    -- ** DescribeClientProperties
    DescribeClientProperties (DescribeClientProperties'),
    newDescribeClientProperties,
    DescribeClientPropertiesResponse (DescribeClientPropertiesResponse'),
    newDescribeClientPropertiesResponse,

    -- ** ModifyClientProperties
    ModifyClientProperties (ModifyClientProperties'),
    newModifyClientProperties,
    ModifyClientPropertiesResponse (ModifyClientPropertiesResponse'),
    newModifyClientPropertiesResponse,

    -- ** DescribeIpGroups (Paginated)
    DescribeIpGroups (DescribeIpGroups'),
    newDescribeIpGroups,
    DescribeIpGroupsResponse (DescribeIpGroupsResponse'),
    newDescribeIpGroupsResponse,

    -- ** DeleteWorkspaceBundle
    DeleteWorkspaceBundle (DeleteWorkspaceBundle'),
    newDeleteWorkspaceBundle,
    DeleteWorkspaceBundleResponse (DeleteWorkspaceBundleResponse'),
    newDeleteWorkspaceBundleResponse,

    -- ** UpdateWorkspaceBundle
    UpdateWorkspaceBundle (UpdateWorkspaceBundle'),
    newUpdateWorkspaceBundle,
    UpdateWorkspaceBundleResponse (UpdateWorkspaceBundleResponse'),
    newUpdateWorkspaceBundleResponse,

    -- ** ListAvailableManagementCidrRanges (Paginated)
    ListAvailableManagementCidrRanges (ListAvailableManagementCidrRanges'),
    newListAvailableManagementCidrRanges,
    ListAvailableManagementCidrRangesResponse (ListAvailableManagementCidrRangesResponse'),
    newListAvailableManagementCidrRangesResponse,

    -- ** UpdateWorkspaceImagePermission
    UpdateWorkspaceImagePermission (UpdateWorkspaceImagePermission'),
    newUpdateWorkspaceImagePermission,
    UpdateWorkspaceImagePermissionResponse (UpdateWorkspaceImagePermissionResponse'),
    newUpdateWorkspaceImagePermissionResponse,

    -- ** CreateUpdatedWorkspaceImage
    CreateUpdatedWorkspaceImage (CreateUpdatedWorkspaceImage'),
    newCreateUpdatedWorkspaceImage,
    CreateUpdatedWorkspaceImageResponse (CreateUpdatedWorkspaceImageResponse'),
    newCreateUpdatedWorkspaceImageResponse,

    -- ** DescribeWorkspaces (Paginated)
    DescribeWorkspaces (DescribeWorkspaces'),
    newDescribeWorkspaces,
    DescribeWorkspacesResponse (DescribeWorkspacesResponse'),
    newDescribeWorkspacesResponse,

    -- ** DescribeConnectionAliases
    DescribeConnectionAliases (DescribeConnectionAliases'),
    newDescribeConnectionAliases,
    DescribeConnectionAliasesResponse (DescribeConnectionAliasesResponse'),
    newDescribeConnectionAliasesResponse,

    -- ** StartWorkspaces
    StartWorkspaces (StartWorkspaces'),
    newStartWorkspaces,
    StartWorkspacesResponse (StartWorkspacesResponse'),
    newStartWorkspacesResponse,

    -- ** DescribeAccountModifications (Paginated)
    DescribeAccountModifications (DescribeAccountModifications'),
    newDescribeAccountModifications,
    DescribeAccountModificationsResponse (DescribeAccountModificationsResponse'),
    newDescribeAccountModificationsResponse,

    -- ** ModifyAccount
    ModifyAccount (ModifyAccount'),
    newModifyAccount,
    ModifyAccountResponse (ModifyAccountResponse'),
    newModifyAccountResponse,

    -- * Types

    -- ** AccessPropertyValue
    AccessPropertyValue (..),

    -- ** Application
    Application (..),

    -- ** AssociationStatus
    AssociationStatus (..),

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

    -- ** ImageType
    ImageType (..),

    -- ** ModificationResourceEnum
    ModificationResourceEnum (..),

    -- ** ModificationStateEnum
    ModificationStateEnum (..),

    -- ** OperatingSystemType
    OperatingSystemType (..),

    -- ** ReconnectEnum
    ReconnectEnum (..),

    -- ** RunningMode
    RunningMode (..),

    -- ** TargetWorkspaceState
    TargetWorkspaceState (..),

    -- ** Tenancy
    Tenancy (..),

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

    -- ** ClientProperties
    ClientProperties (ClientProperties'),
    newClientProperties,

    -- ** ClientPropertiesResult
    ClientPropertiesResult (ClientPropertiesResult'),
    newClientPropertiesResult,

    -- ** ComputeType
    ComputeType (ComputeType'),
    newComputeType,

    -- ** ConnectionAlias
    ConnectionAlias (ConnectionAlias'),
    newConnectionAlias,

    -- ** ConnectionAliasAssociation
    ConnectionAliasAssociation (ConnectionAliasAssociation'),
    newConnectionAliasAssociation,

    -- ** ConnectionAliasPermission
    ConnectionAliasPermission (ConnectionAliasPermission'),
    newConnectionAliasPermission,

    -- ** DefaultWorkspaceCreationProperties
    DefaultWorkspaceCreationProperties (DefaultWorkspaceCreationProperties'),
    newDefaultWorkspaceCreationProperties,

    -- ** FailedCreateWorkspaceRequest
    FailedCreateWorkspaceRequest (FailedCreateWorkspaceRequest'),
    newFailedCreateWorkspaceRequest,

    -- ** FailedWorkspaceChangeRequest
    FailedWorkspaceChangeRequest (FailedWorkspaceChangeRequest'),
    newFailedWorkspaceChangeRequest,

    -- ** ImagePermission
    ImagePermission (ImagePermission'),
    newImagePermission,

    -- ** IpRuleItem
    IpRuleItem (IpRuleItem'),
    newIpRuleItem,

    -- ** ModificationState
    ModificationState (ModificationState'),
    newModificationState,

    -- ** OperatingSystem
    OperatingSystem (OperatingSystem'),
    newOperatingSystem,

    -- ** RebootRequest
    RebootRequest (RebootRequest'),
    newRebootRequest,

    -- ** RebuildRequest
    RebuildRequest (RebuildRequest'),
    newRebuildRequest,

    -- ** RootStorage
    RootStorage (RootStorage'),
    newRootStorage,

    -- ** SelfservicePermissions
    SelfservicePermissions (SelfservicePermissions'),
    newSelfservicePermissions,

    -- ** Snapshot
    Snapshot (Snapshot'),
    newSnapshot,

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

import Network.AWS.WorkSpaces.AssociateConnectionAlias
import Network.AWS.WorkSpaces.AssociateIpGroups
import Network.AWS.WorkSpaces.AuthorizeIpRules
import Network.AWS.WorkSpaces.CopyWorkspaceImage
import Network.AWS.WorkSpaces.CreateConnectionAlias
import Network.AWS.WorkSpaces.CreateIpGroup
import Network.AWS.WorkSpaces.CreateTags
import Network.AWS.WorkSpaces.CreateUpdatedWorkspaceImage
import Network.AWS.WorkSpaces.CreateWorkspaceBundle
import Network.AWS.WorkSpaces.CreateWorkspaces
import Network.AWS.WorkSpaces.DeleteConnectionAlias
import Network.AWS.WorkSpaces.DeleteIpGroup
import Network.AWS.WorkSpaces.DeleteTags
import Network.AWS.WorkSpaces.DeleteWorkspaceBundle
import Network.AWS.WorkSpaces.DeleteWorkspaceImage
import Network.AWS.WorkSpaces.DeregisterWorkspaceDirectory
import Network.AWS.WorkSpaces.DescribeAccount
import Network.AWS.WorkSpaces.DescribeAccountModifications
import Network.AWS.WorkSpaces.DescribeClientProperties
import Network.AWS.WorkSpaces.DescribeConnectionAliasPermissions
import Network.AWS.WorkSpaces.DescribeConnectionAliases
import Network.AWS.WorkSpaces.DescribeIpGroups
import Network.AWS.WorkSpaces.DescribeTags
import Network.AWS.WorkSpaces.DescribeWorkspaceBundles
import Network.AWS.WorkSpaces.DescribeWorkspaceDirectories
import Network.AWS.WorkSpaces.DescribeWorkspaceImagePermissions
import Network.AWS.WorkSpaces.DescribeWorkspaceImages
import Network.AWS.WorkSpaces.DescribeWorkspaceSnapshots
import Network.AWS.WorkSpaces.DescribeWorkspaces
import Network.AWS.WorkSpaces.DescribeWorkspacesConnectionStatus
import Network.AWS.WorkSpaces.DisassociateConnectionAlias
import Network.AWS.WorkSpaces.DisassociateIpGroups
import Network.AWS.WorkSpaces.ImportWorkspaceImage
import Network.AWS.WorkSpaces.Lens
import Network.AWS.WorkSpaces.ListAvailableManagementCidrRanges
import Network.AWS.WorkSpaces.MigrateWorkspace
import Network.AWS.WorkSpaces.ModifyAccount
import Network.AWS.WorkSpaces.ModifyClientProperties
import Network.AWS.WorkSpaces.ModifySelfservicePermissions
import Network.AWS.WorkSpaces.ModifyWorkspaceAccessProperties
import Network.AWS.WorkSpaces.ModifyWorkspaceCreationProperties
import Network.AWS.WorkSpaces.ModifyWorkspaceProperties
import Network.AWS.WorkSpaces.ModifyWorkspaceState
import Network.AWS.WorkSpaces.RebootWorkspaces
import Network.AWS.WorkSpaces.RebuildWorkspaces
import Network.AWS.WorkSpaces.RegisterWorkspaceDirectory
import Network.AWS.WorkSpaces.RestoreWorkspace
import Network.AWS.WorkSpaces.RevokeIpRules
import Network.AWS.WorkSpaces.StartWorkspaces
import Network.AWS.WorkSpaces.StopWorkspaces
import Network.AWS.WorkSpaces.TerminateWorkspaces
import Network.AWS.WorkSpaces.Types
import Network.AWS.WorkSpaces.UpdateConnectionAliasPermission
import Network.AWS.WorkSpaces.UpdateRulesOfIpGroup
import Network.AWS.WorkSpaces.UpdateWorkspaceBundle
import Network.AWS.WorkSpaces.UpdateWorkspaceImagePermission
import Network.AWS.WorkSpaces.Waiters

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
