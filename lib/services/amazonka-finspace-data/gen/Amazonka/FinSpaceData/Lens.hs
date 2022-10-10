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
    createDataView_sortColumns,
    createDataView_clientToken,
    createDataView_autoUpdate,
    createDataView_partitionColumns,
    createDataView_asOfTimestamp,
    createDataView_datasetId,
    createDataView_destinationTypeParams,
    createDataViewResponse_dataViewId,
    createDataViewResponse_datasetId,
    createDataViewResponse_httpStatus,

    -- ** CreateDataset
    createDataset_alias,
    createDataset_clientToken,
    createDataset_datasetDescription,
    createDataset_schemaDefinition,
    createDataset_ownerInfo,
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
    createUser_clientToken,
    createUser_firstName,
    createUser_lastName,
    createUser_apiAccess,
    createUser_apiAccessPrincipalArn,
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
    getChangesetResponse_sourceParams,
    getChangesetResponse_updatedByChangesetId,
    getChangesetResponse_changeType,
    getChangesetResponse_changesetId,
    getChangesetResponse_changesetArn,
    getChangesetResponse_formatParams,
    getChangesetResponse_activeUntilTimestamp,
    getChangesetResponse_status,
    getChangesetResponse_updatesChangesetId,
    getChangesetResponse_datasetId,
    getChangesetResponse_activeFromTimestamp,
    getChangesetResponse_createTime,
    getChangesetResponse_errorInfo,
    getChangesetResponse_httpStatus,

    -- ** GetDataView
    getDataView_dataViewId,
    getDataView_datasetId,
    getDataViewResponse_sortColumns,
    getDataViewResponse_autoUpdate,
    getDataViewResponse_status,
    getDataViewResponse_lastModifiedTime,
    getDataViewResponse_dataViewArn,
    getDataViewResponse_dataViewId,
    getDataViewResponse_partitionColumns,
    getDataViewResponse_datasetId,
    getDataViewResponse_asOfTimestamp,
    getDataViewResponse_destinationTypeParams,
    getDataViewResponse_createTime,
    getDataViewResponse_errorInfo,
    getDataViewResponse_httpStatus,

    -- ** GetDataset
    getDataset_datasetId,
    getDatasetResponse_alias,
    getDatasetResponse_datasetDescription,
    getDatasetResponse_datasetTitle,
    getDatasetResponse_kind,
    getDatasetResponse_status,
    getDatasetResponse_datasetArn,
    getDatasetResponse_lastModifiedTime,
    getDatasetResponse_datasetId,
    getDatasetResponse_createTime,
    getDatasetResponse_schemaDefinition,
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
    getUserResponse_type,
    getUserResponse_firstName,
    getUserResponse_status,
    getUserResponse_lastDisabledTime,
    getUserResponse_lastLoginTime,
    getUserResponse_lastName,
    getUserResponse_lastModifiedTime,
    getUserResponse_apiAccess,
    getUserResponse_userId,
    getUserResponse_lastEnabledTime,
    getUserResponse_emailAddress,
    getUserResponse_createTime,
    getUserResponse_apiAccessPrincipalArn,
    getUserResponse_httpStatus,

    -- ** GetWorkingLocation
    getWorkingLocation_locationType,
    getWorkingLocationResponse_s3Bucket,
    getWorkingLocationResponse_s3Path,
    getWorkingLocationResponse_s3Uri,
    getWorkingLocationResponse_httpStatus,

    -- ** ListChangesets
    listChangesets_nextToken,
    listChangesets_maxResults,
    listChangesets_datasetId,
    listChangesetsResponse_nextToken,
    listChangesetsResponse_changesets,
    listChangesetsResponse_httpStatus,

    -- ** ListDataViews
    listDataViews_nextToken,
    listDataViews_maxResults,
    listDataViews_datasetId,
    listDataViewsResponse_nextToken,
    listDataViewsResponse_dataViews,
    listDataViewsResponse_httpStatus,

    -- ** ListDatasets
    listDatasets_nextToken,
    listDatasets_maxResults,
    listDatasetsResponse_nextToken,
    listDatasetsResponse_datasets,
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
    updatePermissionGroup_name,
    updatePermissionGroup_clientToken,
    updatePermissionGroup_description,
    updatePermissionGroup_applicationPermissions,
    updatePermissionGroup_permissionGroupId,
    updatePermissionGroupResponse_permissionGroupId,
    updatePermissionGroupResponse_httpStatus,

    -- ** UpdateUser
    updateUser_clientToken,
    updateUser_type,
    updateUser_firstName,
    updateUser_lastName,
    updateUser_apiAccess,
    updateUser_apiAccessPrincipalArn,
    updateUser_userId,
    updateUserResponse_userId,
    updateUserResponse_httpStatus,

    -- * Types

    -- ** AwsCredentials
    awsCredentials_expiration,
    awsCredentials_sessionToken,
    awsCredentials_secretAccessKey,
    awsCredentials_accessKeyId,

    -- ** ChangesetErrorInfo
    changesetErrorInfo_errorCategory,
    changesetErrorInfo_errorMessage,

    -- ** ChangesetSummary
    changesetSummary_sourceParams,
    changesetSummary_updatedByChangesetId,
    changesetSummary_changeType,
    changesetSummary_changesetId,
    changesetSummary_changesetArn,
    changesetSummary_formatParams,
    changesetSummary_activeUntilTimestamp,
    changesetSummary_status,
    changesetSummary_updatesChangesetId,
    changesetSummary_datasetId,
    changesetSummary_activeFromTimestamp,
    changesetSummary_createTime,
    changesetSummary_errorInfo,

    -- ** ColumnDefinition
    columnDefinition_columnName,
    columnDefinition_columnDescription,
    columnDefinition_dataType,

    -- ** Credentials
    credentials_sessionToken,
    credentials_secretAccessKey,
    credentials_accessKeyId,

    -- ** DataViewDestinationTypeParams
    dataViewDestinationTypeParams_s3DestinationExportFileFormat,
    dataViewDestinationTypeParams_s3DestinationExportFileFormatOptions,
    dataViewDestinationTypeParams_destinationType,

    -- ** DataViewErrorInfo
    dataViewErrorInfo_errorCategory,
    dataViewErrorInfo_errorMessage,

    -- ** DataViewSummary
    dataViewSummary_sortColumns,
    dataViewSummary_autoUpdate,
    dataViewSummary_status,
    dataViewSummary_lastModifiedTime,
    dataViewSummary_dataViewArn,
    dataViewSummary_dataViewId,
    dataViewSummary_partitionColumns,
    dataViewSummary_datasetId,
    dataViewSummary_asOfTimestamp,
    dataViewSummary_createTime,
    dataViewSummary_errorInfo,
    dataViewSummary_destinationTypeProperties,

    -- ** Dataset
    dataset_alias,
    dataset_datasetDescription,
    dataset_datasetTitle,
    dataset_kind,
    dataset_datasetArn,
    dataset_lastModifiedTime,
    dataset_datasetId,
    dataset_createTime,
    dataset_schemaDefinition,
    dataset_ownerInfo,

    -- ** DatasetOwnerInfo
    datasetOwnerInfo_name,
    datasetOwnerInfo_email,
    datasetOwnerInfo_phoneNumber,

    -- ** PermissionGroup
    permissionGroup_name,
    permissionGroup_description,
    permissionGroup_lastModifiedTime,
    permissionGroup_applicationPermissions,
    permissionGroup_permissionGroupId,
    permissionGroup_membershipStatus,
    permissionGroup_createTime,

    -- ** PermissionGroupByUser
    permissionGroupByUser_name,
    permissionGroupByUser_permissionGroupId,
    permissionGroupByUser_membershipStatus,

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
    user_type,
    user_firstName,
    user_status,
    user_lastDisabledTime,
    user_lastLoginTime,
    user_lastName,
    user_lastModifiedTime,
    user_apiAccess,
    user_userId,
    user_lastEnabledTime,
    user_emailAddress,
    user_createTime,
    user_apiAccessPrincipalArn,

    -- ** UserByPermissionGroup
    userByPermissionGroup_type,
    userByPermissionGroup_firstName,
    userByPermissionGroup_status,
    userByPermissionGroup_lastName,
    userByPermissionGroup_apiAccess,
    userByPermissionGroup_userId,
    userByPermissionGroup_membershipStatus,
    userByPermissionGroup_emailAddress,
    userByPermissionGroup_apiAccessPrincipalArn,
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
