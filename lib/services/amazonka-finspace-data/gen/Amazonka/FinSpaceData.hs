{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.FinSpaceData
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2020-07-13@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- The FinSpace APIs let you take actions inside the FinSpace.
module Amazonka.FinSpaceData
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** AccessDeniedException
    _AccessDeniedException,

    -- ** ConflictException
    _ConflictException,

    -- ** InternalServerException
    _InternalServerException,

    -- ** LimitExceededException
    _LimitExceededException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- ** ThrottlingException
    _ThrottlingException,

    -- ** ValidationException
    _ValidationException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** AssociateUserToPermissionGroup
    AssociateUserToPermissionGroup (AssociateUserToPermissionGroup'),
    newAssociateUserToPermissionGroup,
    AssociateUserToPermissionGroupResponse (AssociateUserToPermissionGroupResponse'),
    newAssociateUserToPermissionGroupResponse,

    -- ** CreateChangeset
    CreateChangeset (CreateChangeset'),
    newCreateChangeset,
    CreateChangesetResponse (CreateChangesetResponse'),
    newCreateChangesetResponse,

    -- ** CreateDataView
    CreateDataView (CreateDataView'),
    newCreateDataView,
    CreateDataViewResponse (CreateDataViewResponse'),
    newCreateDataViewResponse,

    -- ** CreateDataset
    CreateDataset (CreateDataset'),
    newCreateDataset,
    CreateDatasetResponse (CreateDatasetResponse'),
    newCreateDatasetResponse,

    -- ** CreatePermissionGroup
    CreatePermissionGroup (CreatePermissionGroup'),
    newCreatePermissionGroup,
    CreatePermissionGroupResponse (CreatePermissionGroupResponse'),
    newCreatePermissionGroupResponse,

    -- ** CreateUser
    CreateUser (CreateUser'),
    newCreateUser,
    CreateUserResponse (CreateUserResponse'),
    newCreateUserResponse,

    -- ** DeleteDataset
    DeleteDataset (DeleteDataset'),
    newDeleteDataset,
    DeleteDatasetResponse (DeleteDatasetResponse'),
    newDeleteDatasetResponse,

    -- ** DeletePermissionGroup
    DeletePermissionGroup (DeletePermissionGroup'),
    newDeletePermissionGroup,
    DeletePermissionGroupResponse (DeletePermissionGroupResponse'),
    newDeletePermissionGroupResponse,

    -- ** DisableUser
    DisableUser (DisableUser'),
    newDisableUser,
    DisableUserResponse (DisableUserResponse'),
    newDisableUserResponse,

    -- ** DisassociateUserFromPermissionGroup
    DisassociateUserFromPermissionGroup (DisassociateUserFromPermissionGroup'),
    newDisassociateUserFromPermissionGroup,
    DisassociateUserFromPermissionGroupResponse (DisassociateUserFromPermissionGroupResponse'),
    newDisassociateUserFromPermissionGroupResponse,

    -- ** EnableUser
    EnableUser (EnableUser'),
    newEnableUser,
    EnableUserResponse (EnableUserResponse'),
    newEnableUserResponse,

    -- ** GetChangeset
    GetChangeset (GetChangeset'),
    newGetChangeset,
    GetChangesetResponse (GetChangesetResponse'),
    newGetChangesetResponse,

    -- ** GetDataView
    GetDataView (GetDataView'),
    newGetDataView,
    GetDataViewResponse (GetDataViewResponse'),
    newGetDataViewResponse,

    -- ** GetDataset
    GetDataset (GetDataset'),
    newGetDataset,
    GetDatasetResponse (GetDatasetResponse'),
    newGetDatasetResponse,

    -- ** GetExternalDataViewAccessDetails
    GetExternalDataViewAccessDetails (GetExternalDataViewAccessDetails'),
    newGetExternalDataViewAccessDetails,
    GetExternalDataViewAccessDetailsResponse (GetExternalDataViewAccessDetailsResponse'),
    newGetExternalDataViewAccessDetailsResponse,

    -- ** GetPermissionGroup
    GetPermissionGroup (GetPermissionGroup'),
    newGetPermissionGroup,
    GetPermissionGroupResponse (GetPermissionGroupResponse'),
    newGetPermissionGroupResponse,

    -- ** GetProgrammaticAccessCredentials
    GetProgrammaticAccessCredentials (GetProgrammaticAccessCredentials'),
    newGetProgrammaticAccessCredentials,
    GetProgrammaticAccessCredentialsResponse (GetProgrammaticAccessCredentialsResponse'),
    newGetProgrammaticAccessCredentialsResponse,

    -- ** GetUser
    GetUser (GetUser'),
    newGetUser,
    GetUserResponse (GetUserResponse'),
    newGetUserResponse,

    -- ** GetWorkingLocation
    GetWorkingLocation (GetWorkingLocation'),
    newGetWorkingLocation,
    GetWorkingLocationResponse (GetWorkingLocationResponse'),
    newGetWorkingLocationResponse,

    -- ** ListChangesets (Paginated)
    ListChangesets (ListChangesets'),
    newListChangesets,
    ListChangesetsResponse (ListChangesetsResponse'),
    newListChangesetsResponse,

    -- ** ListDataViews (Paginated)
    ListDataViews (ListDataViews'),
    newListDataViews,
    ListDataViewsResponse (ListDataViewsResponse'),
    newListDataViewsResponse,

    -- ** ListDatasets (Paginated)
    ListDatasets (ListDatasets'),
    newListDatasets,
    ListDatasetsResponse (ListDatasetsResponse'),
    newListDatasetsResponse,

    -- ** ListPermissionGroups (Paginated)
    ListPermissionGroups (ListPermissionGroups'),
    newListPermissionGroups,
    ListPermissionGroupsResponse (ListPermissionGroupsResponse'),
    newListPermissionGroupsResponse,

    -- ** ListPermissionGroupsByUser
    ListPermissionGroupsByUser (ListPermissionGroupsByUser'),
    newListPermissionGroupsByUser,
    ListPermissionGroupsByUserResponse (ListPermissionGroupsByUserResponse'),
    newListPermissionGroupsByUserResponse,

    -- ** ListUsers (Paginated)
    ListUsers (ListUsers'),
    newListUsers,
    ListUsersResponse (ListUsersResponse'),
    newListUsersResponse,

    -- ** ListUsersByPermissionGroup
    ListUsersByPermissionGroup (ListUsersByPermissionGroup'),
    newListUsersByPermissionGroup,
    ListUsersByPermissionGroupResponse (ListUsersByPermissionGroupResponse'),
    newListUsersByPermissionGroupResponse,

    -- ** ResetUserPassword
    ResetUserPassword (ResetUserPassword'),
    newResetUserPassword,
    ResetUserPasswordResponse (ResetUserPasswordResponse'),
    newResetUserPasswordResponse,

    -- ** UpdateChangeset
    UpdateChangeset (UpdateChangeset'),
    newUpdateChangeset,
    UpdateChangesetResponse (UpdateChangesetResponse'),
    newUpdateChangesetResponse,

    -- ** UpdateDataset
    UpdateDataset (UpdateDataset'),
    newUpdateDataset,
    UpdateDatasetResponse (UpdateDatasetResponse'),
    newUpdateDatasetResponse,

    -- ** UpdatePermissionGroup
    UpdatePermissionGroup (UpdatePermissionGroup'),
    newUpdatePermissionGroup,
    UpdatePermissionGroupResponse (UpdatePermissionGroupResponse'),
    newUpdatePermissionGroupResponse,

    -- ** UpdateUser
    UpdateUser (UpdateUser'),
    newUpdateUser,
    UpdateUserResponse (UpdateUserResponse'),
    newUpdateUserResponse,

    -- * Types

    -- ** ApiAccess
    ApiAccess (..),

    -- ** ApplicationPermission
    ApplicationPermission (..),

    -- ** ChangeType
    ChangeType (..),

    -- ** ColumnDataType
    ColumnDataType (..),

    -- ** DataViewStatus
    DataViewStatus (..),

    -- ** DatasetKind
    DatasetKind (..),

    -- ** DatasetStatus
    DatasetStatus (..),

    -- ** ErrorCategory
    ErrorCategory (..),

    -- ** ExportFileFormat
    ExportFileFormat (..),

    -- ** IngestionStatus
    IngestionStatus (..),

    -- ** LocationType
    LocationType (..),

    -- ** PermissionGroupMembershipStatus
    PermissionGroupMembershipStatus (..),

    -- ** UserStatus
    UserStatus (..),

    -- ** UserType
    UserType (..),

    -- ** AwsCredentials
    AwsCredentials (AwsCredentials'),
    newAwsCredentials,

    -- ** ChangesetErrorInfo
    ChangesetErrorInfo (ChangesetErrorInfo'),
    newChangesetErrorInfo,

    -- ** ChangesetSummary
    ChangesetSummary (ChangesetSummary'),
    newChangesetSummary,

    -- ** ColumnDefinition
    ColumnDefinition (ColumnDefinition'),
    newColumnDefinition,

    -- ** Credentials
    Credentials (Credentials'),
    newCredentials,

    -- ** DataViewDestinationTypeParams
    DataViewDestinationTypeParams (DataViewDestinationTypeParams'),
    newDataViewDestinationTypeParams,

    -- ** DataViewErrorInfo
    DataViewErrorInfo (DataViewErrorInfo'),
    newDataViewErrorInfo,

    -- ** DataViewSummary
    DataViewSummary (DataViewSummary'),
    newDataViewSummary,

    -- ** Dataset
    Dataset (Dataset'),
    newDataset,

    -- ** DatasetOwnerInfo
    DatasetOwnerInfo (DatasetOwnerInfo'),
    newDatasetOwnerInfo,

    -- ** PermissionGroup
    PermissionGroup (PermissionGroup'),
    newPermissionGroup,

    -- ** PermissionGroupByUser
    PermissionGroupByUser (PermissionGroupByUser'),
    newPermissionGroupByUser,

    -- ** PermissionGroupParams
    PermissionGroupParams (PermissionGroupParams'),
    newPermissionGroupParams,

    -- ** ResourcePermission
    ResourcePermission (ResourcePermission'),
    newResourcePermission,

    -- ** S3Location
    S3Location (S3Location'),
    newS3Location,

    -- ** SchemaDefinition
    SchemaDefinition (SchemaDefinition'),
    newSchemaDefinition,

    -- ** SchemaUnion
    SchemaUnion (SchemaUnion'),
    newSchemaUnion,

    -- ** User
    User (User'),
    newUser,

    -- ** UserByPermissionGroup
    UserByPermissionGroup (UserByPermissionGroup'),
    newUserByPermissionGroup,
  )
where

import Amazonka.FinSpaceData.AssociateUserToPermissionGroup
import Amazonka.FinSpaceData.CreateChangeset
import Amazonka.FinSpaceData.CreateDataView
import Amazonka.FinSpaceData.CreateDataset
import Amazonka.FinSpaceData.CreatePermissionGroup
import Amazonka.FinSpaceData.CreateUser
import Amazonka.FinSpaceData.DeleteDataset
import Amazonka.FinSpaceData.DeletePermissionGroup
import Amazonka.FinSpaceData.DisableUser
import Amazonka.FinSpaceData.DisassociateUserFromPermissionGroup
import Amazonka.FinSpaceData.EnableUser
import Amazonka.FinSpaceData.GetChangeset
import Amazonka.FinSpaceData.GetDataView
import Amazonka.FinSpaceData.GetDataset
import Amazonka.FinSpaceData.GetExternalDataViewAccessDetails
import Amazonka.FinSpaceData.GetPermissionGroup
import Amazonka.FinSpaceData.GetProgrammaticAccessCredentials
import Amazonka.FinSpaceData.GetUser
import Amazonka.FinSpaceData.GetWorkingLocation
import Amazonka.FinSpaceData.Lens
import Amazonka.FinSpaceData.ListChangesets
import Amazonka.FinSpaceData.ListDataViews
import Amazonka.FinSpaceData.ListDatasets
import Amazonka.FinSpaceData.ListPermissionGroups
import Amazonka.FinSpaceData.ListPermissionGroupsByUser
import Amazonka.FinSpaceData.ListUsers
import Amazonka.FinSpaceData.ListUsersByPermissionGroup
import Amazonka.FinSpaceData.ResetUserPassword
import Amazonka.FinSpaceData.Types
import Amazonka.FinSpaceData.UpdateChangeset
import Amazonka.FinSpaceData.UpdateDataset
import Amazonka.FinSpaceData.UpdatePermissionGroup
import Amazonka.FinSpaceData.UpdateUser
import Amazonka.FinSpaceData.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'FinSpaceData'.

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
