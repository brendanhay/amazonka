{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.FinSpaceData.Types
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FinSpaceData.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _AccessDeniedException,
    _ConflictException,
    _InternalServerException,
    _LimitExceededException,
    _ResourceNotFoundException,
    _ThrottlingException,
    _ValidationException,

    -- * ApiAccess
    ApiAccess (..),

    -- * ApplicationPermission
    ApplicationPermission (..),

    -- * ChangeType
    ChangeType (..),

    -- * ColumnDataType
    ColumnDataType (..),

    -- * DataViewStatus
    DataViewStatus (..),

    -- * DatasetKind
    DatasetKind (..),

    -- * DatasetStatus
    DatasetStatus (..),

    -- * ErrorCategory
    ErrorCategory (..),

    -- * ExportFileFormat
    ExportFileFormat (..),

    -- * IngestionStatus
    IngestionStatus (..),

    -- * LocationType
    LocationType (..),

    -- * PermissionGroupMembershipStatus
    PermissionGroupMembershipStatus (..),

    -- * UserStatus
    UserStatus (..),

    -- * UserType
    UserType (..),

    -- * AwsCredentials
    AwsCredentials (..),
    newAwsCredentials,
    awsCredentials_accessKeyId,
    awsCredentials_expiration,
    awsCredentials_secretAccessKey,
    awsCredentials_sessionToken,

    -- * ChangesetErrorInfo
    ChangesetErrorInfo (..),
    newChangesetErrorInfo,
    changesetErrorInfo_errorCategory,
    changesetErrorInfo_errorMessage,

    -- * ChangesetSummary
    ChangesetSummary (..),
    newChangesetSummary,
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

    -- * ColumnDefinition
    ColumnDefinition (..),
    newColumnDefinition,
    columnDefinition_columnDescription,
    columnDefinition_columnName,
    columnDefinition_dataType,

    -- * Credentials
    Credentials (..),
    newCredentials,
    credentials_accessKeyId,
    credentials_secretAccessKey,
    credentials_sessionToken,

    -- * DataViewDestinationTypeParams
    DataViewDestinationTypeParams (..),
    newDataViewDestinationTypeParams,
    dataViewDestinationTypeParams_s3DestinationExportFileFormat,
    dataViewDestinationTypeParams_s3DestinationExportFileFormatOptions,
    dataViewDestinationTypeParams_destinationType,

    -- * DataViewErrorInfo
    DataViewErrorInfo (..),
    newDataViewErrorInfo,
    dataViewErrorInfo_errorCategory,
    dataViewErrorInfo_errorMessage,

    -- * DataViewSummary
    DataViewSummary (..),
    newDataViewSummary,
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

    -- * Dataset
    Dataset (..),
    newDataset,
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

    -- * DatasetOwnerInfo
    DatasetOwnerInfo (..),
    newDatasetOwnerInfo,
    datasetOwnerInfo_email,
    datasetOwnerInfo_name,
    datasetOwnerInfo_phoneNumber,

    -- * PermissionGroup
    PermissionGroup (..),
    newPermissionGroup,
    permissionGroup_applicationPermissions,
    permissionGroup_createTime,
    permissionGroup_description,
    permissionGroup_lastModifiedTime,
    permissionGroup_membershipStatus,
    permissionGroup_name,
    permissionGroup_permissionGroupId,

    -- * PermissionGroupByUser
    PermissionGroupByUser (..),
    newPermissionGroupByUser,
    permissionGroupByUser_membershipStatus,
    permissionGroupByUser_name,
    permissionGroupByUser_permissionGroupId,

    -- * PermissionGroupParams
    PermissionGroupParams (..),
    newPermissionGroupParams,
    permissionGroupParams_datasetPermissions,
    permissionGroupParams_permissionGroupId,

    -- * ResourcePermission
    ResourcePermission (..),
    newResourcePermission,
    resourcePermission_permission,

    -- * S3Location
    S3Location (..),
    newS3Location,
    s3Location_bucket,
    s3Location_key,

    -- * SchemaDefinition
    SchemaDefinition (..),
    newSchemaDefinition,
    schemaDefinition_columns,
    schemaDefinition_primaryKeyColumns,

    -- * SchemaUnion
    SchemaUnion (..),
    newSchemaUnion,
    schemaUnion_tabularSchemaConfig,

    -- * User
    User (..),
    newUser,
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

    -- * UserByPermissionGroup
    UserByPermissionGroup (..),
    newUserByPermissionGroup,
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.FinSpaceData.Types.ApiAccess
import Amazonka.FinSpaceData.Types.ApplicationPermission
import Amazonka.FinSpaceData.Types.AwsCredentials
import Amazonka.FinSpaceData.Types.ChangeType
import Amazonka.FinSpaceData.Types.ChangesetErrorInfo
import Amazonka.FinSpaceData.Types.ChangesetSummary
import Amazonka.FinSpaceData.Types.ColumnDataType
import Amazonka.FinSpaceData.Types.ColumnDefinition
import Amazonka.FinSpaceData.Types.Credentials
import Amazonka.FinSpaceData.Types.DataViewDestinationTypeParams
import Amazonka.FinSpaceData.Types.DataViewErrorInfo
import Amazonka.FinSpaceData.Types.DataViewStatus
import Amazonka.FinSpaceData.Types.DataViewSummary
import Amazonka.FinSpaceData.Types.Dataset
import Amazonka.FinSpaceData.Types.DatasetKind
import Amazonka.FinSpaceData.Types.DatasetOwnerInfo
import Amazonka.FinSpaceData.Types.DatasetStatus
import Amazonka.FinSpaceData.Types.ErrorCategory
import Amazonka.FinSpaceData.Types.ExportFileFormat
import Amazonka.FinSpaceData.Types.IngestionStatus
import Amazonka.FinSpaceData.Types.LocationType
import Amazonka.FinSpaceData.Types.PermissionGroup
import Amazonka.FinSpaceData.Types.PermissionGroupByUser
import Amazonka.FinSpaceData.Types.PermissionGroupMembershipStatus
import Amazonka.FinSpaceData.Types.PermissionGroupParams
import Amazonka.FinSpaceData.Types.ResourcePermission
import Amazonka.FinSpaceData.Types.S3Location
import Amazonka.FinSpaceData.Types.SchemaDefinition
import Amazonka.FinSpaceData.Types.SchemaUnion
import Amazonka.FinSpaceData.Types.User
import Amazonka.FinSpaceData.Types.UserByPermissionGroup
import Amazonka.FinSpaceData.Types.UserStatus
import Amazonka.FinSpaceData.Types.UserType
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2020-07-13@ of the Amazon FinSpace Public API SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "FinSpaceData",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "finspace-api",
      Core.signingName = "finspace-api",
      Core.version = "2020-07-13",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error = Core.parseJSONError "FinSpaceData",
      Core.retry = retry
    }
  where
    retry =
      Core.Exponential
        { Core.base = 5.0e-2,
          Core.growth = 2,
          Core.attempts = 5,
          Core.check = check
        }
    check e
      | Lens.has (Core.hasStatus 502) e =
          Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 504) e =
          Prelude.Just "gateway_timeout"
      | Lens.has (Core.hasStatus 500) e =
          Prelude.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e =
          Prelude.Just "limit_exceeded"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 503) e =
          Prelude.Just "service_unavailable"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throttled_exception"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throttling"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throttling_exception"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 429) e =
          Prelude.Just "too_many_requests"
      | Prelude.otherwise = Prelude.Nothing

-- | You do not have sufficient access to perform this action.
_AccessDeniedException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_AccessDeniedException =
  Core._MatchServiceError
    defaultService
    "AccessDeniedException"
    Prelude.. Core.hasStatus 403

-- | The request conflicts with an existing resource.
_ConflictException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ConflictException =
  Core._MatchServiceError
    defaultService
    "ConflictException"
    Prelude.. Core.hasStatus 409

-- | The request processing has failed because of an unknown error, exception
-- or failure.
_InternalServerException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InternalServerException =
  Core._MatchServiceError
    defaultService
    "InternalServerException"
    Prelude.. Core.hasStatus 500

-- | A limit has exceeded.
_LimitExceededException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_LimitExceededException =
  Core._MatchServiceError
    defaultService
    "LimitExceededException"
    Prelude.. Core.hasStatus 400

-- | One or more resources can\'t be found.
_ResourceNotFoundException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"
    Prelude.. Core.hasStatus 404

-- | The request was denied due to request throttling.
_ThrottlingException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ThrottlingException =
  Core._MatchServiceError
    defaultService
    "ThrottlingException"
    Prelude.. Core.hasStatus 429

-- | The input fails to satisfy the constraints specified by an AWS service.
_ValidationException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ValidationException =
  Core._MatchServiceError
    defaultService
    "ValidationException"
    Prelude.. Core.hasStatus 400
