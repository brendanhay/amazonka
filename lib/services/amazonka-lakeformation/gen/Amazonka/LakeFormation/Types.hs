{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.LakeFormation.Types
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LakeFormation.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _AccessDeniedException,
    _AlreadyExistsException,
    _ConcurrentModificationException,
    _EntityNotFoundException,
    _ExpiredException,
    _GlueEncryptionException,
    _InternalServiceException,
    _InvalidInputException,
    _OperationTimeoutException,
    _PermissionTypeMismatchException,
    _ResourceNotReadyException,
    _ResourceNumberLimitExceededException,
    _StatisticsNotReadyYetException,
    _ThrottledException,
    _TransactionCanceledException,
    _TransactionCommitInProgressException,
    _TransactionCommittedException,
    _WorkUnitsNotReadyYetException,

    -- * ComparisonOperator
    ComparisonOperator (..),

    -- * DataLakeResourceType
    DataLakeResourceType (..),

    -- * FieldNameString
    FieldNameString (..),

    -- * OptimizerType
    OptimizerType (..),

    -- * Permission
    Permission (..),

    -- * PermissionType
    PermissionType (..),

    -- * QueryStateString
    QueryStateString (..),

    -- * ResourceShareType
    ResourceShareType (..),

    -- * ResourceType
    ResourceType (..),

    -- * TransactionStatus
    TransactionStatus (..),

    -- * TransactionStatusFilter
    TransactionStatusFilter (..),

    -- * TransactionType
    TransactionType (..),

    -- * AddObjectInput
    AddObjectInput (..),
    newAddObjectInput,
    addObjectInput_partitionValues,
    addObjectInput_uri,
    addObjectInput_eTag,
    addObjectInput_size,

    -- * AllRowsWildcard
    AllRowsWildcard (..),
    newAllRowsWildcard,

    -- * AuditContext
    AuditContext (..),
    newAuditContext,
    auditContext_additionalAuditContext,

    -- * BatchPermissionsFailureEntry
    BatchPermissionsFailureEntry (..),
    newBatchPermissionsFailureEntry,
    batchPermissionsFailureEntry_error,
    batchPermissionsFailureEntry_requestEntry,

    -- * BatchPermissionsRequestEntry
    BatchPermissionsRequestEntry (..),
    newBatchPermissionsRequestEntry,
    batchPermissionsRequestEntry_permissions,
    batchPermissionsRequestEntry_permissionsWithGrantOption,
    batchPermissionsRequestEntry_principal,
    batchPermissionsRequestEntry_resource,
    batchPermissionsRequestEntry_id,

    -- * CatalogResource
    CatalogResource (..),
    newCatalogResource,

    -- * ColumnLFTag
    ColumnLFTag (..),
    newColumnLFTag,
    columnLFTag_lFTags,
    columnLFTag_name,

    -- * ColumnWildcard
    ColumnWildcard (..),
    newColumnWildcard,
    columnWildcard_excludedColumnNames,

    -- * DataCellsFilter
    DataCellsFilter (..),
    newDataCellsFilter,
    dataCellsFilter_columnNames,
    dataCellsFilter_columnWildcard,
    dataCellsFilter_rowFilter,
    dataCellsFilter_tableCatalogId,
    dataCellsFilter_databaseName,
    dataCellsFilter_tableName,
    dataCellsFilter_name,

    -- * DataCellsFilterResource
    DataCellsFilterResource (..),
    newDataCellsFilterResource,
    dataCellsFilterResource_databaseName,
    dataCellsFilterResource_name,
    dataCellsFilterResource_tableCatalogId,
    dataCellsFilterResource_tableName,

    -- * DataLakePrincipal
    DataLakePrincipal (..),
    newDataLakePrincipal,
    dataLakePrincipal_dataLakePrincipalIdentifier,

    -- * DataLakeSettings
    DataLakeSettings (..),
    newDataLakeSettings,
    dataLakeSettings_allowExternalDataFiltering,
    dataLakeSettings_authorizedSessionTagValueList,
    dataLakeSettings_createDatabaseDefaultPermissions,
    dataLakeSettings_createTableDefaultPermissions,
    dataLakeSettings_dataLakeAdmins,
    dataLakeSettings_externalDataFilteringAllowList,
    dataLakeSettings_parameters,
    dataLakeSettings_trustedResourceOwners,

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

    -- * DeleteObjectInput
    DeleteObjectInput (..),
    newDeleteObjectInput,
    deleteObjectInput_eTag,
    deleteObjectInput_partitionValues,
    deleteObjectInput_uri,

    -- * DetailsMap
    DetailsMap (..),
    newDetailsMap,
    detailsMap_resourceShare,

    -- * ErrorDetail
    ErrorDetail (..),
    newErrorDetail,
    errorDetail_errorCode,
    errorDetail_errorMessage,

    -- * ExecutionStatistics
    ExecutionStatistics (..),
    newExecutionStatistics,
    executionStatistics_averageExecutionTimeMillis,
    executionStatistics_dataScannedBytes,
    executionStatistics_workUnitsExecutedCount,

    -- * FilterCondition
    FilterCondition (..),
    newFilterCondition,
    filterCondition_comparisonOperator,
    filterCondition_field,
    filterCondition_stringValueList,

    -- * LFTag
    LFTag (..),
    newLFTag,
    lFTag_tagKey,
    lFTag_tagValues,

    -- * LFTagError
    LFTagError (..),
    newLFTagError,
    lFTagError_error,
    lFTagError_lFTag,

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

    -- * PartitionObjects
    PartitionObjects (..),
    newPartitionObjects,
    partitionObjects_objects,
    partitionObjects_partitionValues,

    -- * PartitionValueList
    PartitionValueList (..),
    newPartitionValueList,
    partitionValueList_values,

    -- * PlanningStatistics
    PlanningStatistics (..),
    newPlanningStatistics,
    planningStatistics_estimatedDataToScanBytes,
    planningStatistics_planningTimeMillis,
    planningStatistics_queueTimeMillis,
    planningStatistics_workUnitsGeneratedCount,

    -- * PrincipalPermissions
    PrincipalPermissions (..),
    newPrincipalPermissions,
    principalPermissions_permissions,
    principalPermissions_principal,

    -- * PrincipalResourcePermissions
    PrincipalResourcePermissions (..),
    newPrincipalResourcePermissions,
    principalResourcePermissions_additionalDetails,
    principalResourcePermissions_permissions,
    principalResourcePermissions_permissionsWithGrantOption,
    principalResourcePermissions_principal,
    principalResourcePermissions_resource,

    -- * QueryPlanningContext
    QueryPlanningContext (..),
    newQueryPlanningContext,
    queryPlanningContext_catalogId,
    queryPlanningContext_queryAsOfTime,
    queryPlanningContext_queryParameters,
    queryPlanningContext_transactionId,
    queryPlanningContext_databaseName,

    -- * Resource
    Resource (..),
    newResource,
    resource_catalog,
    resource_dataCellsFilter,
    resource_dataLocation,
    resource_database,
    resource_lFTag,
    resource_lFTagPolicy,
    resource_table,
    resource_tableWithColumns,

    -- * ResourceInfo
    ResourceInfo (..),
    newResourceInfo,
    resourceInfo_lastModified,
    resourceInfo_resourceArn,
    resourceInfo_roleArn,

    -- * RowFilter
    RowFilter (..),
    newRowFilter,
    rowFilter_allRowsWildcard,
    rowFilter_filterExpression,

    -- * StorageOptimizer
    StorageOptimizer (..),
    newStorageOptimizer,
    storageOptimizer_config,
    storageOptimizer_errorMessage,
    storageOptimizer_lastRunDetails,
    storageOptimizer_storageOptimizerType,
    storageOptimizer_warnings,

    -- * TableObject
    TableObject (..),
    newTableObject,
    tableObject_eTag,
    tableObject_size,
    tableObject_uri,

    -- * TableResource
    TableResource (..),
    newTableResource,
    tableResource_catalogId,
    tableResource_name,
    tableResource_tableWildcard,
    tableResource_databaseName,

    -- * TableWildcard
    TableWildcard (..),
    newTableWildcard,

    -- * TableWithColumnsResource
    TableWithColumnsResource (..),
    newTableWithColumnsResource,
    tableWithColumnsResource_catalogId,
    tableWithColumnsResource_columnNames,
    tableWithColumnsResource_columnWildcard,
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
    taggedTable_lFTagOnDatabase,
    taggedTable_lFTagsOnColumns,
    taggedTable_lFTagsOnTable,
    taggedTable_table,

    -- * TransactionDescription
    TransactionDescription (..),
    newTransactionDescription,
    transactionDescription_transactionEndTime,
    transactionDescription_transactionId,
    transactionDescription_transactionStartTime,
    transactionDescription_transactionStatus,

    -- * VirtualObject
    VirtualObject (..),
    newVirtualObject,
    virtualObject_eTag,
    virtualObject_uri,

    -- * WorkUnitRange
    WorkUnitRange (..),
    newWorkUnitRange,
    workUnitRange_workUnitIdMax,
    workUnitRange_workUnitIdMin,
    workUnitRange_workUnitToken,

    -- * WriteOperation
    WriteOperation (..),
    newWriteOperation,
    writeOperation_addObject,
    writeOperation_deleteObject,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.LakeFormation.Types.AddObjectInput
import Amazonka.LakeFormation.Types.AllRowsWildcard
import Amazonka.LakeFormation.Types.AuditContext
import Amazonka.LakeFormation.Types.BatchPermissionsFailureEntry
import Amazonka.LakeFormation.Types.BatchPermissionsRequestEntry
import Amazonka.LakeFormation.Types.CatalogResource
import Amazonka.LakeFormation.Types.ColumnLFTag
import Amazonka.LakeFormation.Types.ColumnWildcard
import Amazonka.LakeFormation.Types.ComparisonOperator
import Amazonka.LakeFormation.Types.DataCellsFilter
import Amazonka.LakeFormation.Types.DataCellsFilterResource
import Amazonka.LakeFormation.Types.DataLakePrincipal
import Amazonka.LakeFormation.Types.DataLakeResourceType
import Amazonka.LakeFormation.Types.DataLakeSettings
import Amazonka.LakeFormation.Types.DataLocationResource
import Amazonka.LakeFormation.Types.DatabaseResource
import Amazonka.LakeFormation.Types.DeleteObjectInput
import Amazonka.LakeFormation.Types.DetailsMap
import Amazonka.LakeFormation.Types.ErrorDetail
import Amazonka.LakeFormation.Types.ExecutionStatistics
import Amazonka.LakeFormation.Types.FieldNameString
import Amazonka.LakeFormation.Types.FilterCondition
import Amazonka.LakeFormation.Types.LFTag
import Amazonka.LakeFormation.Types.LFTagError
import Amazonka.LakeFormation.Types.LFTagKeyResource
import Amazonka.LakeFormation.Types.LFTagPair
import Amazonka.LakeFormation.Types.LFTagPolicyResource
import Amazonka.LakeFormation.Types.OptimizerType
import Amazonka.LakeFormation.Types.PartitionObjects
import Amazonka.LakeFormation.Types.PartitionValueList
import Amazonka.LakeFormation.Types.Permission
import Amazonka.LakeFormation.Types.PermissionType
import Amazonka.LakeFormation.Types.PlanningStatistics
import Amazonka.LakeFormation.Types.PrincipalPermissions
import Amazonka.LakeFormation.Types.PrincipalResourcePermissions
import Amazonka.LakeFormation.Types.QueryPlanningContext
import Amazonka.LakeFormation.Types.QueryStateString
import Amazonka.LakeFormation.Types.Resource
import Amazonka.LakeFormation.Types.ResourceInfo
import Amazonka.LakeFormation.Types.ResourceShareType
import Amazonka.LakeFormation.Types.ResourceType
import Amazonka.LakeFormation.Types.RowFilter
import Amazonka.LakeFormation.Types.StorageOptimizer
import Amazonka.LakeFormation.Types.TableObject
import Amazonka.LakeFormation.Types.TableResource
import Amazonka.LakeFormation.Types.TableWildcard
import Amazonka.LakeFormation.Types.TableWithColumnsResource
import Amazonka.LakeFormation.Types.TaggedDatabase
import Amazonka.LakeFormation.Types.TaggedTable
import Amazonka.LakeFormation.Types.TransactionDescription
import Amazonka.LakeFormation.Types.TransactionStatus
import Amazonka.LakeFormation.Types.TransactionStatusFilter
import Amazonka.LakeFormation.Types.TransactionType
import Amazonka.LakeFormation.Types.VirtualObject
import Amazonka.LakeFormation.Types.WorkUnitRange
import Amazonka.LakeFormation.Types.WriteOperation
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2017-03-31@ of the Amazon Lake Formation SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "LakeFormation",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "lakeformation",
      Core.signingName = "lakeformation",
      Core.version = "2017-03-31",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error = Core.parseJSONError "LakeFormation",
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

-- | Access to a resource was denied.
_AccessDeniedException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_AccessDeniedException =
  Core._MatchServiceError
    defaultService
    "AccessDeniedException"
    Prelude.. Core.hasStatus 403

-- | A resource to be created or added already exists.
_AlreadyExistsException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_AlreadyExistsException =
  Core._MatchServiceError
    defaultService
    "AlreadyExistsException"

-- | Two processes are trying to modify a resource simultaneously.
_ConcurrentModificationException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ConcurrentModificationException =
  Core._MatchServiceError
    defaultService
    "ConcurrentModificationException"

-- | A specified entity does not exist
_EntityNotFoundException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_EntityNotFoundException =
  Core._MatchServiceError
    defaultService
    "EntityNotFoundException"

-- | Contains details about an error where the query request expired.
_ExpiredException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ExpiredException =
  Core._MatchServiceError
    defaultService
    "ExpiredException"
    Prelude.. Core.hasStatus 410

-- | An encryption operation failed.
_GlueEncryptionException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_GlueEncryptionException =
  Core._MatchServiceError
    defaultService
    "GlueEncryptionException"

-- | An internal service error occurred.
_InternalServiceException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InternalServiceException =
  Core._MatchServiceError
    defaultService
    "InternalServiceException"
    Prelude.. Core.hasStatus 500

-- | The input provided was not valid.
_InvalidInputException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidInputException =
  Core._MatchServiceError
    defaultService
    "InvalidInputException"
    Prelude.. Core.hasStatus 400

-- | The operation timed out.
_OperationTimeoutException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_OperationTimeoutException =
  Core._MatchServiceError
    defaultService
    "OperationTimeoutException"

-- | The engine does not support filtering data based on the enforced
-- permissions. For example, if you call the
-- @GetTemporaryGlueTableCredentials@ operation with
-- @SupportedPermissionType@ equal to @ColumnPermission@, but cell-level
-- permissions exist on the table, this exception is thrown.
_PermissionTypeMismatchException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_PermissionTypeMismatchException =
  Core._MatchServiceError
    defaultService
    "PermissionTypeMismatchException"

-- | Contains details about an error related to a resource which is not ready
-- for a transaction.
_ResourceNotReadyException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ResourceNotReadyException =
  Core._MatchServiceError
    defaultService
    "ResourceNotReadyException"
    Prelude.. Core.hasStatus 400

-- | A resource numerical limit was exceeded.
_ResourceNumberLimitExceededException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ResourceNumberLimitExceededException =
  Core._MatchServiceError
    defaultService
    "ResourceNumberLimitExceededException"

-- | Contains details about an error related to statistics not being ready.
_StatisticsNotReadyYetException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_StatisticsNotReadyYetException =
  Core._MatchServiceError
    defaultService
    "StatisticsNotReadyYetException"
    Prelude.. Core.hasStatus 420

-- | Contains details about an error where the query request was throttled.
_ThrottledException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ThrottledException =
  Core._MatchServiceError
    defaultService
    "ThrottledException"
    Prelude.. Core.hasStatus 429

-- | Contains details about an error related to a transaction that was
-- cancelled.
_TransactionCanceledException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_TransactionCanceledException =
  Core._MatchServiceError
    defaultService
    "TransactionCanceledException"
    Prelude.. Core.hasStatus 400

-- | Contains details about an error related to a transaction commit that was
-- in progress.
_TransactionCommitInProgressException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_TransactionCommitInProgressException =
  Core._MatchServiceError
    defaultService
    "TransactionCommitInProgressException"
    Prelude.. Core.hasStatus 400

-- | Contains details about an error where the specified transaction has
-- already been committed and cannot be used for @UpdateTableObjects@.
_TransactionCommittedException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_TransactionCommittedException =
  Core._MatchServiceError
    defaultService
    "TransactionCommittedException"
    Prelude.. Core.hasStatus 400

-- | Contains details about an error related to work units not being ready.
_WorkUnitsNotReadyYetException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_WorkUnitsNotReadyYetException =
  Core._MatchServiceError
    defaultService
    "WorkUnitsNotReadyYetException"
    Prelude.. Core.hasStatus 420
