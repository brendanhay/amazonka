{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
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

    -- ** ResourceUnavailableException
    _ResourceUnavailableException,

    -- ** UnsupportedNetworkConfigurationException
    _UnsupportedNetworkConfigurationException,

    -- ** OperationNotSupportedException
    _OperationNotSupportedException,

    -- ** ResourceAlreadyExistsException
    _ResourceAlreadyExistsException,

    -- ** ResourceLimitExceededException
    _ResourceLimitExceededException,

    -- ** OperationInProgressException
    _OperationInProgressException,

    -- ** WorkspacesDefaultRoleNotFoundException
    _WorkspacesDefaultRoleNotFoundException,

    -- ** ResourceAssociatedException
    _ResourceAssociatedException,

    -- ** InvalidParameterValuesException
    _InvalidParameterValuesException,

    -- ** ResourceCreationFailedException
    _ResourceCreationFailedException,

    -- ** UnsupportedWorkspaceConfigurationException
    _UnsupportedWorkspaceConfigurationException,

    -- ** AccessDeniedException
    _AccessDeniedException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- ** InvalidResourceStateException
    _InvalidResourceStateException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** DescribeWorkspaceDirectories (Paginated)
    DescribeWorkspaceDirectories (DescribeWorkspaceDirectories'),
    newDescribeWorkspaceDirectories,
    DescribeWorkspaceDirectoriesResponse (DescribeWorkspaceDirectoriesResponse'),
    newDescribeWorkspaceDirectoriesResponse,

    -- ** TerminateWorkspaces
    TerminateWorkspaces (TerminateWorkspaces'),
    newTerminateWorkspaces,
    TerminateWorkspacesResponse (TerminateWorkspacesResponse'),
    newTerminateWorkspacesResponse,

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

    -- ** ImportWorkspaceImage
    ImportWorkspaceImage (ImportWorkspaceImage'),
    newImportWorkspaceImage,
    ImportWorkspaceImageResponse (ImportWorkspaceImageResponse'),
    newImportWorkspaceImageResponse,

    -- ** DeleteIpGroup
    DeleteIpGroup (DeleteIpGroup'),
    newDeleteIpGroup,
    DeleteIpGroupResponse (DeleteIpGroupResponse'),
    newDeleteIpGroupResponse,

    -- ** DeregisterWorkspaceDirectory
    DeregisterWorkspaceDirectory (DeregisterWorkspaceDirectory'),
    newDeregisterWorkspaceDirectory,
    DeregisterWorkspaceDirectoryResponse (DeregisterWorkspaceDirectoryResponse'),
    newDeregisterWorkspaceDirectoryResponse,

    -- ** AssociateConnectionAlias
    AssociateConnectionAlias (AssociateConnectionAlias'),
    newAssociateConnectionAlias,
    AssociateConnectionAliasResponse (AssociateConnectionAliasResponse'),
    newAssociateConnectionAliasResponse,

    -- ** DescribeTags
    DescribeTags (DescribeTags'),
    newDescribeTags,
    DescribeTagsResponse (DescribeTagsResponse'),
    newDescribeTagsResponse,

    -- ** CreateConnectionAlias
    CreateConnectionAlias (CreateConnectionAlias'),
    newCreateConnectionAlias,
    CreateConnectionAliasResponse (CreateConnectionAliasResponse'),
    newCreateConnectionAliasResponse,

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

    -- ** DescribeWorkspacesConnectionStatus (Paginated)
    DescribeWorkspacesConnectionStatus (DescribeWorkspacesConnectionStatus'),
    newDescribeWorkspacesConnectionStatus,
    DescribeWorkspacesConnectionStatusResponse (DescribeWorkspacesConnectionStatusResponse'),
    newDescribeWorkspacesConnectionStatusResponse,

    -- ** ModifySelfservicePermissions
    ModifySelfservicePermissions (ModifySelfservicePermissions'),
    newModifySelfservicePermissions,
    ModifySelfservicePermissionsResponse (ModifySelfservicePermissionsResponse'),
    newModifySelfservicePermissionsResponse,

    -- ** UpdateRulesOfIpGroup
    UpdateRulesOfIpGroup (UpdateRulesOfIpGroup'),
    newUpdateRulesOfIpGroup,
    UpdateRulesOfIpGroupResponse (UpdateRulesOfIpGroupResponse'),
    newUpdateRulesOfIpGroupResponse,

    -- ** DescribeConnectionAliases
    DescribeConnectionAliases (DescribeConnectionAliases'),
    newDescribeConnectionAliases,
    DescribeConnectionAliasesResponse (DescribeConnectionAliasesResponse'),
    newDescribeConnectionAliasesResponse,

    -- ** DeleteWorkspaceImage
    DeleteWorkspaceImage (DeleteWorkspaceImage'),
    newDeleteWorkspaceImage,
    DeleteWorkspaceImageResponse (DeleteWorkspaceImageResponse'),
    newDeleteWorkspaceImageResponse,

    -- ** DeleteTags
    DeleteTags (DeleteTags'),
    newDeleteTags,
    DeleteTagsResponse (DeleteTagsResponse'),
    newDeleteTagsResponse,

    -- ** ListAvailableManagementCidrRanges (Paginated)
    ListAvailableManagementCidrRanges (ListAvailableManagementCidrRanges'),
    newListAvailableManagementCidrRanges,
    ListAvailableManagementCidrRangesResponse (ListAvailableManagementCidrRangesResponse'),
    newListAvailableManagementCidrRangesResponse,

    -- ** ModifyWorkspaceCreationProperties
    ModifyWorkspaceCreationProperties (ModifyWorkspaceCreationProperties'),
    newModifyWorkspaceCreationProperties,
    ModifyWorkspaceCreationPropertiesResponse (ModifyWorkspaceCreationPropertiesResponse'),
    newModifyWorkspaceCreationPropertiesResponse,

    -- ** DescribeClientProperties
    DescribeClientProperties (DescribeClientProperties'),
    newDescribeClientProperties,
    DescribeClientPropertiesResponse (DescribeClientPropertiesResponse'),
    newDescribeClientPropertiesResponse,

    -- ** ModifyWorkspaceState
    ModifyWorkspaceState (ModifyWorkspaceState'),
    newModifyWorkspaceState,
    ModifyWorkspaceStateResponse (ModifyWorkspaceStateResponse'),
    newModifyWorkspaceStateResponse,

    -- ** UpdateConnectionAliasPermission
    UpdateConnectionAliasPermission (UpdateConnectionAliasPermission'),
    newUpdateConnectionAliasPermission,
    UpdateConnectionAliasPermissionResponse (UpdateConnectionAliasPermissionResponse'),
    newUpdateConnectionAliasPermissionResponse,

    -- ** CopyWorkspaceImage
    CopyWorkspaceImage (CopyWorkspaceImage'),
    newCopyWorkspaceImage,
    CopyWorkspaceImageResponse (CopyWorkspaceImageResponse'),
    newCopyWorkspaceImageResponse,

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

    -- ** RebootWorkspaces
    RebootWorkspaces (RebootWorkspaces'),
    newRebootWorkspaces,
    RebootWorkspacesResponse (RebootWorkspacesResponse'),
    newRebootWorkspacesResponse,

    -- ** DescribeWorkspaceSnapshots
    DescribeWorkspaceSnapshots (DescribeWorkspaceSnapshots'),
    newDescribeWorkspaceSnapshots,
    DescribeWorkspaceSnapshotsResponse (DescribeWorkspaceSnapshotsResponse'),
    newDescribeWorkspaceSnapshotsResponse,

    -- ** DescribeAccount
    DescribeAccount (DescribeAccount'),
    newDescribeAccount,
    DescribeAccountResponse (DescribeAccountResponse'),
    newDescribeAccountResponse,

    -- ** ModifyWorkspaceProperties
    ModifyWorkspaceProperties (ModifyWorkspaceProperties'),
    newModifyWorkspaceProperties,
    ModifyWorkspacePropertiesResponse (ModifyWorkspacePropertiesResponse'),
    newModifyWorkspacePropertiesResponse,

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

    -- ** DescribeAccountModifications (Paginated)
    DescribeAccountModifications (DescribeAccountModifications'),
    newDescribeAccountModifications,
    DescribeAccountModificationsResponse (DescribeAccountModificationsResponse'),
    newDescribeAccountModificationsResponse,

    -- ** DeleteConnectionAlias
    DeleteConnectionAlias (DeleteConnectionAlias'),
    newDeleteConnectionAlias,
    DeleteConnectionAliasResponse (DeleteConnectionAliasResponse'),
    newDeleteConnectionAliasResponse,

    -- ** AssociateIpGroups
    AssociateIpGroups (AssociateIpGroups'),
    newAssociateIpGroups,
    AssociateIpGroupsResponse (AssociateIpGroupsResponse'),
    newAssociateIpGroupsResponse,

    -- ** StopWorkspaces
    StopWorkspaces (StopWorkspaces'),
    newStopWorkspaces,
    StopWorkspacesResponse (StopWorkspacesResponse'),
    newStopWorkspacesResponse,

    -- ** StartWorkspaces
    StartWorkspaces (StartWorkspaces'),
    newStartWorkspaces,
    StartWorkspacesResponse (StartWorkspacesResponse'),
    newStartWorkspacesResponse,

    -- ** DescribeWorkspaces (Paginated)
    DescribeWorkspaces (DescribeWorkspaces'),
    newDescribeWorkspaces,
    DescribeWorkspacesResponse (DescribeWorkspacesResponse'),
    newDescribeWorkspacesResponse,

    -- ** UpdateWorkspaceImagePermission
    UpdateWorkspaceImagePermission (UpdateWorkspaceImagePermission'),
    newUpdateWorkspaceImagePermission,
    UpdateWorkspaceImagePermissionResponse (UpdateWorkspaceImagePermissionResponse'),
    newUpdateWorkspaceImagePermissionResponse,

    -- ** ModifyClientProperties
    ModifyClientProperties (ModifyClientProperties'),
    newModifyClientProperties,
    ModifyClientPropertiesResponse (ModifyClientPropertiesResponse'),
    newModifyClientPropertiesResponse,

    -- ** ModifyWorkspaceAccessProperties
    ModifyWorkspaceAccessProperties (ModifyWorkspaceAccessProperties'),
    newModifyWorkspaceAccessProperties,
    ModifyWorkspaceAccessPropertiesResponse (ModifyWorkspaceAccessPropertiesResponse'),
    newModifyWorkspaceAccessPropertiesResponse,

    -- ** DescribeIpGroups (Paginated)
    DescribeIpGroups (DescribeIpGroups'),
    newDescribeIpGroups,
    DescribeIpGroupsResponse (DescribeIpGroupsResponse'),
    newDescribeIpGroupsResponse,

    -- ** RestoreWorkspace
    RestoreWorkspace (RestoreWorkspace'),
    newRestoreWorkspace,
    RestoreWorkspaceResponse (RestoreWorkspaceResponse'),
    newRestoreWorkspaceResponse,

    -- ** CreateTags
    CreateTags (CreateTags'),
    newCreateTags,
    CreateTagsResponse (CreateTagsResponse'),
    newCreateTagsResponse,

    -- ** DescribeConnectionAliasPermissions
    DescribeConnectionAliasPermissions (DescribeConnectionAliasPermissions'),
    newDescribeConnectionAliasPermissions,
    DescribeConnectionAliasPermissionsResponse (DescribeConnectionAliasPermissionsResponse'),
    newDescribeConnectionAliasPermissionsResponse,

    -- ** RegisterWorkspaceDirectory
    RegisterWorkspaceDirectory (RegisterWorkspaceDirectory'),
    newRegisterWorkspaceDirectory,
    RegisterWorkspaceDirectoryResponse (RegisterWorkspaceDirectoryResponse'),
    newRegisterWorkspaceDirectoryResponse,

    -- ** CreateWorkspaces
    CreateWorkspaces (CreateWorkspaces'),
    newCreateWorkspaces,
    CreateWorkspacesResponse (CreateWorkspacesResponse'),
    newCreateWorkspacesResponse,

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
import Network.AWS.WorkSpaces.CreateWorkspaces
import Network.AWS.WorkSpaces.DeleteConnectionAlias
import Network.AWS.WorkSpaces.DeleteIpGroup
import Network.AWS.WorkSpaces.DeleteTags
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
