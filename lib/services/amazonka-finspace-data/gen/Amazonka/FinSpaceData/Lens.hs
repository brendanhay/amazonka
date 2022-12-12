{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.FinSpaceData.Lens
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FinSpaceData.Lens
  ( -- * Operations

    -- ** AssociateUserToPermissionGroup
    associateUserToPermissionGroup_clientToken,
    associateUserToPermissionGroup_permissionGroupId,
    associateUserToPermissionGroup_userId,
    associateUserToPermissionGroupResponse_statusCode,

    -- ** CreateChangeset
    createChangeset_clientToken,
    createChangeset_datasetId,
    createChangeset_changeType,
    createChangeset_sourceParams,
    createChangeset_formatParams,
    createChangesetResponse_changesetId,
    createChangesetResponse_datasetId,
    createChangesetResponse_httpStatus,

    -- ** CreateDataView
    createDataView_asOfTimestamp,
    createDataView_autoUpdate,
    createDataView_clientToken,
    createDataView_partitionColumns,
    createDataView_sortColumns,
    createDataView_datasetId,
    createDataView_destinationTypeParams,
    createDataViewResponse_dataViewId,
    createDataViewResponse_datasetId,
    createDataViewResponse_httpStatus,

    -- ** CreateDataset
    createDataset_alias,
    createDataset_clientToken,
    createDataset_datasetDescription,
    createDataset_ownerInfo,
    createDataset_schemaDefinition,
    createDataset_datasetTitle,
    createDataset_kind,
    createDataset_permissionGroupParams,
    createDatasetResponse_datasetId,
    createDatasetResponse_httpStatus,

    -- ** CreatePermissionGroup
    createPermissionGroup_clientToken,
    createPermissionGroup_description,
    createPermissionGroup_name,
    createPermissionGroup_applicationPermissions,
    createPermissionGroupResponse_permissionGroupId,
    createPermissionGroupResponse_httpStatus,

    -- ** CreateUser
    createUser_apiAccess,
    createUser_apiAccessPrincipalArn,
    createUser_clientToken,
    createUser_firstName,
    createUser_lastName,
    createUser_emailAddress,
    createUser_type,
    createUserResponse_userId,
    createUserResponse_httpStatus,

    -- ** DeleteDataset
    deleteDataset_clientToken,
    deleteDataset_datasetId,
    deleteDatasetResponse_datasetId,
    deleteDatasetResponse_httpStatus,

    -- ** DeletePermissionGroup
    deletePermissionGroup_clientToken,
    deletePermissionGroup_permissionGroupId,
    deletePermissionGroupResponse_permissionGroupId,
    deletePermissionGroupResponse_httpStatus,

    -- ** DisableUser
    disableUser_clientToken,
    disableUser_userId,
    disableUserResponse_userId,
    disableUserResponse_httpStatus,

    -- ** DisassociateUserFromPermissionGroup
    disassociateUserFromPermissionGroup_clientToken,
    disassociateUserFromPermissionGroup_permissionGroupId,
    disassociateUserFromPermissionGroup_userId,
    disassociateUserFromPermissionGroupResponse_statusCode,

    -- ** EnableUser
    enableUser_clientToken,
    enableUser_userId,
    enableUserResponse_userId,
    enableUserResponse_httpStatus,

    -- ** GetChangeset
    getChangeset_datasetId,
    getChangeset_changesetId,
    getChangesetResponse_activeFromTimestamp,
    getChangesetResponse_activeUntilTimestamp,
    getChangesetResponse_changeType,
    getChangesetResponse_changesetArn,
    getChangesetResponse_changesetId,
    getChangesetResponse_createTime,
    getChangesetResponse_datasetId,
    getChangesetResponse_errorInfo,
    getChangesetResponse_formatParams,
    getChangesetResponse_sourceParams,
    getChangesetResponse_status,
    getChangesetResponse_updatedByChangesetId,
    getChangesetResponse_updatesChangesetId,
    getChangesetResponse_httpStatus,

    -- ** GetDataView
    getDataView_dataViewId,
    getDataView_datasetId,
    getDataViewResponse_asOfTimestamp,
    getDataViewResponse_autoUpdate,
    getDataViewResponse_createTime,
    getDataViewResponse_dataViewArn,
    getDataViewResponse_dataViewId,
    getDataViewResponse_datasetId,
    getDataViewResponse_destinationTypeParams,
    getDataViewResponse_errorInfo,
    getDataViewResponse_lastModifiedTime,
    getDataViewResponse_partitionColumns,
    getDataViewResponse_sortColumns,
    getDataViewResponse_status,
    getDataViewResponse_httpStatus,

    -- ** GetDataset
    getDataset_datasetId,
    getDatasetResponse_alias,
    getDatasetResponse_createTime,
    getDatasetResponse_datasetArn,
    getDatasetResponse_datasetDescription,
    getDatasetResponse_datasetId,
    getDatasetResponse_datasetTitle,
    getDatasetResponse_kind,
    getDatasetResponse_lastModifiedTime,
    getDatasetResponse_schemaDefinition,
    getDatasetResponse_status,
    getDatasetResponse_httpStatus,

    -- ** GetExternalDataViewAccessDetails
    getExternalDataViewAccessDetails_dataViewId,
    getExternalDataViewAccessDetails_datasetId,
    getExternalDataViewAccessDetailsResponse_credentials,
    getExternalDataViewAccessDetailsResponse_s3Location,
    getExternalDataViewAccessDetailsResponse_httpStatus,

    -- ** GetPermissionGroup
    getPermissionGroup_permissionGroupId,
    getPermissionGroupResponse_permissionGroup,
    getPermissionGroupResponse_httpStatus,

    -- ** GetProgrammaticAccessCredentials
    getProgrammaticAccessCredentials_durationInMinutes,
    getProgrammaticAccessCredentials_environmentId,
    getProgrammaticAccessCredentialsResponse_credentials,
    getProgrammaticAccessCredentialsResponse_durationInMinutes,
    getProgrammaticAccessCredentialsResponse_httpStatus,

    -- ** GetUser
    getUser_userId,
    getUserResponse_apiAccess,
    getUserResponse_apiAccessPrincipalArn,
    getUserResponse_createTime,
    getUserResponse_emailAddress,
    getUserResponse_firstName,
    getUserResponse_lastDisabledTime,
    getUserResponse_lastEnabledTime,
    getUserResponse_lastLoginTime,
    getUserResponse_lastModifiedTime,
    getUserResponse_lastName,
    getUserResponse_status,
    getUserResponse_type,
    getUserResponse_userId,
    getUserResponse_httpStatus,

    -- ** GetWorkingLocation
    getWorkingLocation_locationType,
    getWorkingLocationResponse_s3Bucket,
    getWorkingLocationResponse_s3Path,
    getWorkingLocationResponse_s3Uri,
    getWorkingLocationResponse_httpStatus,

    -- ** ListChangesets
    listChangesets_maxResults,
    listChangesets_nextToken,
    listChangesets_datasetId,
    listChangesetsResponse_changesets,
    listChangesetsResponse_nextToken,
    listChangesetsResponse_httpStatus,

    -- ** ListDataViews
    listDataViews_maxResults,
    listDataViews_nextToken,
    listDataViews_datasetId,
    listDataViewsResponse_dataViews,
    listDataViewsResponse_nextToken,
    listDataViewsResponse_httpStatus,

    -- ** ListDatasets
    listDatasets_maxResults,
    listDatasets_nextToken,
    listDatasetsResponse_datasets,
    listDatasetsResponse_nextToken,
    listDatasetsResponse_httpStatus,

    -- ** ListPermissionGroups
    listPermissionGroups_nextToken,
    listPermissionGroups_maxResults,
    listPermissionGroupsResponse_nextToken,
    listPermissionGroupsResponse_permissionGroups,
    listPermissionGroupsResponse_httpStatus,

    -- ** ListPermissionGroupsByUser
    listPermissionGroupsByUser_nextToken,
    listPermissionGroupsByUser_userId,
    listPermissionGroupsByUser_maxResults,
    listPermissionGroupsByUserResponse_nextToken,
    listPermissionGroupsByUserResponse_permissionGroups,
    listPermissionGroupsByUserResponse_httpStatus,

    -- ** ListUsers
    listUsers_nextToken,
    listUsers_maxResults,
    listUsersResponse_nextToken,
    listUsersResponse_users,
    listUsersResponse_httpStatus,

    -- ** ListUsersByPermissionGroup
    listUsersByPermissionGroup_nextToken,
    listUsersByPermissionGroup_permissionGroupId,
    listUsersByPermissionGroup_maxResults,
    listUsersByPermissionGroupResponse_nextToken,
    listUsersByPermissionGroupResponse_users,
    listUsersByPermissionGroupResponse_httpStatus,

    -- ** ResetUserPassword
    resetUserPassword_clientToken,
    resetUserPassword_userId,
    resetUserPasswordResponse_temporaryPassword,
    resetUserPasswordResponse_userId,
    resetUserPasswordResponse_httpStatus,

    -- ** UpdateChangeset
    updateChangeset_clientToken,
    updateChangeset_datasetId,
    updateChangeset_changesetId,
    updateChangeset_sourceParams,
    updateChangeset_formatParams,
    updateChangesetResponse_changesetId,
    updateChangesetResponse_datasetId,
    updateChangesetResponse_httpStatus,

    -- ** UpdateDataset
    updateDataset_alias,
    updateDataset_clientToken,
    updateDataset_datasetDescription,
    updateDataset_schemaDefinition,
    updateDataset_datasetId,
    updateDataset_datasetTitle,
    updateDataset_kind,
    updateDatasetResponse_datasetId,
    updateDatasetResponse_httpStatus,

    -- ** UpdatePermissionGroup
    updatePermissionGroup_applicationPermissions,
    updatePermissionGroup_clientToken,
    updatePermissionGroup_description,
    updatePermissionGroup_name,
    updatePermissionGroup_permissionGroupId,
    updatePermissionGroupResponse_permissionGroupId,
    updatePermissionGroupResponse_httpStatus,

    -- ** UpdateUser
    updateUser_apiAccess,
    updateUser_apiAccessPrincipalArn,
    updateUser_clientToken,
    updateUser_firstName,
    updateUser_lastName,
    updateUser_type,
    updateUser_userId,
    updateUserResponse_userId,
    updateUserResponse_httpStatus,

    -- * Types

    -- ** AwsCredentials
    awsCredentials_accessKeyId,
    awsCredentials_expiration,
    awsCredentials_secretAccessKey,
    awsCredentials_sessionToken,

    -- ** ChangesetErrorInfo
    changesetErrorInfo_errorCategory,
    changesetErrorInfo_errorMessage,

    -- ** ChangesetSummary
    changesetSummary_activeFromTimestamp,
    changesetSummary_activeUntilTimestamp,
    changesetSummary_changeType,
    changesetSummary_changesetArn,
    changesetSummary_changesetId,
    changesetSummary_createTime,
    changesetSummary_datasetId,
    changesetSummary_errorInfo,
    changesetSummary_formatParams,
    changesetSummary_sourceParams,
    changesetSummary_status,
    changesetSummary_updatedByChangesetId,
    changesetSummary_updatesChangesetId,

    -- ** ColumnDefinition
    columnDefinition_columnDescription,
    columnDefinition_columnName,
    columnDefinition_dataType,

    -- ** Credentials
    credentials_accessKeyId,
    credentials_secretAccessKey,
    credentials_sessionToken,

    -- ** DataViewDestinationTypeParams
    dataViewDestinationTypeParams_s3DestinationExportFileFormat,
    dataViewDestinationTypeParams_s3DestinationExportFileFormatOptions,
    dataViewDestinationTypeParams_destinationType,

    -- ** DataViewErrorInfo
    dataViewErrorInfo_errorCategory,
    dataViewErrorInfo_errorMessage,

    -- ** DataViewSummary
    dataViewSummary_asOfTimestamp,
    dataViewSummary_autoUpdate,
    dataViewSummary_createTime,
    dataViewSummary_dataViewArn,
    dataViewSummary_dataViewId,
    dataViewSummary_datasetId,
    dataViewSummary_destinationTypeProperties,
    dataViewSummary_errorInfo,
    dataViewSummary_lastModifiedTime,
    dataViewSummary_partitionColumns,
    dataViewSummary_sortColumns,
    dataViewSummary_status,

    -- ** Dataset
    dataset_alias,
    dataset_createTime,
    dataset_datasetArn,
    dataset_datasetDescription,
    dataset_datasetId,
    dataset_datasetTitle,
    dataset_kind,
    dataset_lastModifiedTime,
    dataset_ownerInfo,
    dataset_schemaDefinition,

    -- ** DatasetOwnerInfo
    datasetOwnerInfo_email,
    datasetOwnerInfo_name,
    datasetOwnerInfo_phoneNumber,

    -- ** PermissionGroup
    permissionGroup_applicationPermissions,
    permissionGroup_createTime,
    permissionGroup_description,
    permissionGroup_lastModifiedTime,
    permissionGroup_membershipStatus,
    permissionGroup_name,
    permissionGroup_permissionGroupId,

    -- ** PermissionGroupByUser
    permissionGroupByUser_membershipStatus,
    permissionGroupByUser_name,
    permissionGroupByUser_permissionGroupId,

    -- ** PermissionGroupParams
    permissionGroupParams_datasetPermissions,
    permissionGroupParams_permissionGroupId,

    -- ** ResourcePermission
    resourcePermission_permission,

    -- ** S3Location
    s3Location_bucket,
    s3Location_key,

    -- ** SchemaDefinition
    schemaDefinition_columns,
    schemaDefinition_primaryKeyColumns,

    -- ** SchemaUnion
    schemaUnion_tabularSchemaConfig,

    -- ** User
    user_apiAccess,
    user_apiAccessPrincipalArn,
    user_createTime,
    user_emailAddress,
    user_firstName,
    user_lastDisabledTime,
    user_lastEnabledTime,
    user_lastLoginTime,
    user_lastModifiedTime,
    user_lastName,
    user_status,
    user_type,
    user_userId,

    -- ** UserByPermissionGroup
    userByPermissionGroup_apiAccess,
    userByPermissionGroup_apiAccessPrincipalArn,
    userByPermissionGroup_emailAddress,
    userByPermissionGroup_firstName,
    userByPermissionGroup_lastName,
    userByPermissionGroup_membershipStatus,
    userByPermissionGroup_status,
    userByPermissionGroup_type,
    userByPermissionGroup_userId,
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
import Amazonka.FinSpaceData.ListChangesets
import Amazonka.FinSpaceData.ListDataViews
import Amazonka.FinSpaceData.ListDatasets
import Amazonka.FinSpaceData.ListPermissionGroups
import Amazonka.FinSpaceData.ListPermissionGroupsByUser
import Amazonka.FinSpaceData.ListUsers
import Amazonka.FinSpaceData.ListUsersByPermissionGroup
import Amazonka.FinSpaceData.ResetUserPassword
import Amazonka.FinSpaceData.Types.AwsCredentials
import Amazonka.FinSpaceData.Types.ChangesetErrorInfo
import Amazonka.FinSpaceData.Types.ChangesetSummary
import Amazonka.FinSpaceData.Types.ColumnDefinition
import Amazonka.FinSpaceData.Types.Credentials
import Amazonka.FinSpaceData.Types.DataViewDestinationTypeParams
import Amazonka.FinSpaceData.Types.DataViewErrorInfo
import Amazonka.FinSpaceData.Types.DataViewSummary
import Amazonka.FinSpaceData.Types.Dataset
import Amazonka.FinSpaceData.Types.DatasetOwnerInfo
import Amazonka.FinSpaceData.Types.PermissionGroup
import Amazonka.FinSpaceData.Types.PermissionGroupByUser
import Amazonka.FinSpaceData.Types.PermissionGroupParams
import Amazonka.FinSpaceData.Types.ResourcePermission
import Amazonka.FinSpaceData.Types.S3Location
import Amazonka.FinSpaceData.Types.SchemaDefinition
import Amazonka.FinSpaceData.Types.SchemaUnion
import Amazonka.FinSpaceData.Types.User
import Amazonka.FinSpaceData.Types.UserByPermissionGroup
import Amazonka.FinSpaceData.UpdateChangeset
import Amazonka.FinSpaceData.UpdateDataset
import Amazonka.FinSpaceData.UpdatePermissionGroup
import Amazonka.FinSpaceData.UpdateUser
