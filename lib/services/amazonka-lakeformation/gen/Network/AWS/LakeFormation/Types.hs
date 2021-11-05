{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LakeFormation.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LakeFormation.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _AccessDeniedException,
    _OperationTimeoutException,
    _EntityNotFoundException,
    _ConcurrentModificationException,
    _InternalServiceException,
    _InvalidInputException,
    _ResourceNumberLimitExceededException,
    _GlueEncryptionException,
    _AlreadyExistsException,

    -- * ComparisonOperator
    ComparisonOperator (..),

    -- * DataLakeResourceType
    DataLakeResourceType (..),

    -- * FieldNameString
    FieldNameString (..),

    -- * Permission
    Permission (..),

    -- * ResourceShareType
    ResourceShareType (..),

    -- * ResourceType
    ResourceType (..),

    -- * BatchPermissionsFailureEntry
    BatchPermissionsFailureEntry (..),
    newBatchPermissionsFailureEntry,
    batchPermissionsFailureEntry_error,
    batchPermissionsFailureEntry_requestEntry,

    -- * BatchPermissionsRequestEntry
    BatchPermissionsRequestEntry (..),
    newBatchPermissionsRequestEntry,
    batchPermissionsRequestEntry_permissionsWithGrantOption,
    batchPermissionsRequestEntry_principal,
    batchPermissionsRequestEntry_resource,
    batchPermissionsRequestEntry_permissions,
    batchPermissionsRequestEntry_id,

    -- * CatalogResource
    CatalogResource (..),
    newCatalogResource,

    -- * ColumnLFTag
    ColumnLFTag (..),
    newColumnLFTag,
    columnLFTag_name,
    columnLFTag_lFTags,

    -- * ColumnWildcard
    ColumnWildcard (..),
    newColumnWildcard,
    columnWildcard_excludedColumnNames,

    -- * DataLakePrincipal
    DataLakePrincipal (..),
    newDataLakePrincipal,
    dataLakePrincipal_dataLakePrincipalIdentifier,

    -- * DataLakeSettings
    DataLakeSettings (..),
    newDataLakeSettings,
    dataLakeSettings_dataLakeAdmins,
    dataLakeSettings_trustedResourceOwners,
    dataLakeSettings_createDatabaseDefaultPermissions,
    dataLakeSettings_createTableDefaultPermissions,

    -- * DataLocationResource
    DataLocationResource (..),
    newDataLocationResource,
    dataLocationResource_catalogId,
    dataLocationResource_resourceArn,

    -- * DatabaseResource
    DatabaseResource (..),
    newDatabaseResource,
    databaseResource_catalogId,
    databaseResource_name,

    -- * DetailsMap
    DetailsMap (..),
    newDetailsMap,
    detailsMap_resourceShare,

    -- * ErrorDetail
    ErrorDetail (..),
    newErrorDetail,
    errorDetail_errorCode,
    errorDetail_errorMessage,

    -- * FilterCondition
    FilterCondition (..),
    newFilterCondition,
    filterCondition_field,
    filterCondition_comparisonOperator,
    filterCondition_stringValueList,

    -- * LFTag
    LFTag (..),
    newLFTag,
    lFTag_tagKey,
    lFTag_tagValues,

    -- * LFTagError
    LFTagError (..),
    newLFTagError,
    lFTagError_lFTag,
    lFTagError_error,

    -- * LFTagKeyResource
    LFTagKeyResource (..),
    newLFTagKeyResource,
    lFTagKeyResource_catalogId,
    lFTagKeyResource_tagKey,
    lFTagKeyResource_tagValues,

    -- * LFTagPair
    LFTagPair (..),
    newLFTagPair,
    lFTagPair_catalogId,
    lFTagPair_tagKey,
    lFTagPair_tagValues,

    -- * LFTagPolicyResource
    LFTagPolicyResource (..),
    newLFTagPolicyResource,
    lFTagPolicyResource_catalogId,
    lFTagPolicyResource_resourceType,
    lFTagPolicyResource_expression,

    -- * PrincipalPermissions
    PrincipalPermissions (..),
    newPrincipalPermissions,
    principalPermissions_principal,
    principalPermissions_permissions,

    -- * PrincipalResourcePermissions
    PrincipalResourcePermissions (..),
    newPrincipalResourcePermissions,
    principalResourcePermissions_additionalDetails,
    principalResourcePermissions_permissionsWithGrantOption,
    principalResourcePermissions_principal,
    principalResourcePermissions_resource,
    principalResourcePermissions_permissions,

    -- * Resource
    Resource (..),
    newResource,
    resource_dataLocation,
    resource_database,
    resource_lFTag,
    resource_catalog,
    resource_lFTagPolicy,
    resource_table,
    resource_tableWithColumns,

    -- * ResourceInfo
    ResourceInfo (..),
    newResourceInfo,
    resourceInfo_resourceArn,
    resourceInfo_lastModified,
    resourceInfo_roleArn,

    -- * TableResource
    TableResource (..),
    newTableResource,
    tableResource_catalogId,
    tableResource_tableWildcard,
    tableResource_name,
    tableResource_databaseName,

    -- * TableWildcard
    TableWildcard (..),
    newTableWildcard,

    -- * TableWithColumnsResource
    TableWithColumnsResource (..),
    newTableWithColumnsResource,
    tableWithColumnsResource_catalogId,
    tableWithColumnsResource_columnWildcard,
    tableWithColumnsResource_columnNames,
    tableWithColumnsResource_databaseName,
    tableWithColumnsResource_name,

    -- * TaggedDatabase
    TaggedDatabase (..),
    newTaggedDatabase,
    taggedDatabase_database,
    taggedDatabase_lFTags,

    -- * TaggedTable
    TaggedTable (..),
    newTaggedTable,
    taggedTable_lFTagsOnTable,
    taggedTable_lFTagOnDatabase,
    taggedTable_lFTagsOnColumns,
    taggedTable_table,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.LakeFormation.Types.BatchPermissionsFailureEntry
import Network.AWS.LakeFormation.Types.BatchPermissionsRequestEntry
import Network.AWS.LakeFormation.Types.CatalogResource
import Network.AWS.LakeFormation.Types.ColumnLFTag
import Network.AWS.LakeFormation.Types.ColumnWildcard
import Network.AWS.LakeFormation.Types.ComparisonOperator
import Network.AWS.LakeFormation.Types.DataLakePrincipal
import Network.AWS.LakeFormation.Types.DataLakeResourceType
import Network.AWS.LakeFormation.Types.DataLakeSettings
import Network.AWS.LakeFormation.Types.DataLocationResource
import Network.AWS.LakeFormation.Types.DatabaseResource
import Network.AWS.LakeFormation.Types.DetailsMap
import Network.AWS.LakeFormation.Types.ErrorDetail
import Network.AWS.LakeFormation.Types.FieldNameString
import Network.AWS.LakeFormation.Types.FilterCondition
import Network.AWS.LakeFormation.Types.LFTag
import Network.AWS.LakeFormation.Types.LFTagError
import Network.AWS.LakeFormation.Types.LFTagKeyResource
import Network.AWS.LakeFormation.Types.LFTagPair
import Network.AWS.LakeFormation.Types.LFTagPolicyResource
import Network.AWS.LakeFormation.Types.Permission
import Network.AWS.LakeFormation.Types.PrincipalPermissions
import Network.AWS.LakeFormation.Types.PrincipalResourcePermissions
import Network.AWS.LakeFormation.Types.Resource
import Network.AWS.LakeFormation.Types.ResourceInfo
import Network.AWS.LakeFormation.Types.ResourceShareType
import Network.AWS.LakeFormation.Types.ResourceType
import Network.AWS.LakeFormation.Types.TableResource
import Network.AWS.LakeFormation.Types.TableWildcard
import Network.AWS.LakeFormation.Types.TableWithColumnsResource
import Network.AWS.LakeFormation.Types.TaggedDatabase
import Network.AWS.LakeFormation.Types.TaggedTable
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2017-03-31@ of the Amazon Lake Formation SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core._serviceAbbrev = "LakeFormation",
      Core._serviceSigner = Sign.v4,
      Core._serviceEndpointPrefix = "lakeformation",
      Core._serviceSigningName = "lakeformation",
      Core._serviceVersion = "2017-03-31",
      Core._serviceEndpoint =
        Core.defaultEndpoint defaultService,
      Core._serviceTimeout = Prelude.Just 70,
      Core._serviceCheck = Core.statusSuccess,
      Core._serviceError =
        Core.parseJSONError "LakeFormation",
      Core._serviceRetry = retry
    }
  where
    retry =
      Core.Exponential
        { Core._retryBase = 5.0e-2,
          Core._retryGrowth = 2,
          Core._retryAttempts = 5,
          Core._retryCheck = check
        }
    check e
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has (Core.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Core.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Prelude.otherwise = Prelude.Nothing

-- | Access to a resource was denied.
_AccessDeniedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AccessDeniedException =
  Core._MatchServiceError
    defaultService
    "AccessDeniedException"

-- | The operation timed out.
_OperationTimeoutException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_OperationTimeoutException =
  Core._MatchServiceError
    defaultService
    "OperationTimeoutException"

-- | A specified entity does not exist
_EntityNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_EntityNotFoundException =
  Core._MatchServiceError
    defaultService
    "EntityNotFoundException"

-- | Two processes are trying to modify a resource simultaneously.
_ConcurrentModificationException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ConcurrentModificationException =
  Core._MatchServiceError
    defaultService
    "ConcurrentModificationException"

-- | An internal service error occurred.
_InternalServiceException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InternalServiceException =
  Core._MatchServiceError
    defaultService
    "InternalServiceException"

-- | The input provided was not valid.
_InvalidInputException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidInputException =
  Core._MatchServiceError
    defaultService
    "InvalidInputException"

-- | A resource numerical limit was exceeded.
_ResourceNumberLimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceNumberLimitExceededException =
  Core._MatchServiceError
    defaultService
    "ResourceNumberLimitExceededException"

-- | An encryption operation failed.
_GlueEncryptionException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_GlueEncryptionException =
  Core._MatchServiceError
    defaultService
    "GlueEncryptionException"

-- | A resource to be created or added already exists.
_AlreadyExistsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AlreadyExistsException =
  Core._MatchServiceError
    defaultService
    "AlreadyExistsException"
