{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.LakeFormation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2017-03-31@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- Lake Formation
--
-- Defines the public endpoint for the Lake Formation service.
module Amazonka.LakeFormation
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** AccessDeniedException
    _AccessDeniedException,

    -- ** AlreadyExistsException
    _AlreadyExistsException,

    -- ** ConcurrentModificationException
    _ConcurrentModificationException,

    -- ** EntityNotFoundException
    _EntityNotFoundException,

    -- ** ExpiredException
    _ExpiredException,

    -- ** GlueEncryptionException
    _GlueEncryptionException,

    -- ** InternalServiceException
    _InternalServiceException,

    -- ** InvalidInputException
    _InvalidInputException,

    -- ** OperationTimeoutException
    _OperationTimeoutException,

    -- ** PermissionTypeMismatchException
    _PermissionTypeMismatchException,

    -- ** ResourceNotReadyException
    _ResourceNotReadyException,

    -- ** ResourceNumberLimitExceededException
    _ResourceNumberLimitExceededException,

    -- ** StatisticsNotReadyYetException
    _StatisticsNotReadyYetException,

    -- ** ThrottledException
    _ThrottledException,

    -- ** TransactionCanceledException
    _TransactionCanceledException,

    -- ** TransactionCommitInProgressException
    _TransactionCommitInProgressException,

    -- ** TransactionCommittedException
    _TransactionCommittedException,

    -- ** WorkUnitsNotReadyYetException
    _WorkUnitsNotReadyYetException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** AddLFTagsToResource
    AddLFTagsToResource (AddLFTagsToResource'),
    newAddLFTagsToResource,
    AddLFTagsToResourceResponse (AddLFTagsToResourceResponse'),
    newAddLFTagsToResourceResponse,

    -- ** AssumeDecoratedRoleWithSAML
    AssumeDecoratedRoleWithSAML (AssumeDecoratedRoleWithSAML'),
    newAssumeDecoratedRoleWithSAML,
    AssumeDecoratedRoleWithSAMLResponse (AssumeDecoratedRoleWithSAMLResponse'),
    newAssumeDecoratedRoleWithSAMLResponse,

    -- ** BatchGrantPermissions
    BatchGrantPermissions (BatchGrantPermissions'),
    newBatchGrantPermissions,
    BatchGrantPermissionsResponse (BatchGrantPermissionsResponse'),
    newBatchGrantPermissionsResponse,

    -- ** BatchRevokePermissions
    BatchRevokePermissions (BatchRevokePermissions'),
    newBatchRevokePermissions,
    BatchRevokePermissionsResponse (BatchRevokePermissionsResponse'),
    newBatchRevokePermissionsResponse,

    -- ** CancelTransaction
    CancelTransaction (CancelTransaction'),
    newCancelTransaction,
    CancelTransactionResponse (CancelTransactionResponse'),
    newCancelTransactionResponse,

    -- ** CommitTransaction
    CommitTransaction (CommitTransaction'),
    newCommitTransaction,
    CommitTransactionResponse (CommitTransactionResponse'),
    newCommitTransactionResponse,

    -- ** CreateDataCellsFilter
    CreateDataCellsFilter (CreateDataCellsFilter'),
    newCreateDataCellsFilter,
    CreateDataCellsFilterResponse (CreateDataCellsFilterResponse'),
    newCreateDataCellsFilterResponse,

    -- ** CreateLFTag
    CreateLFTag (CreateLFTag'),
    newCreateLFTag,
    CreateLFTagResponse (CreateLFTagResponse'),
    newCreateLFTagResponse,

    -- ** DeleteDataCellsFilter
    DeleteDataCellsFilter (DeleteDataCellsFilter'),
    newDeleteDataCellsFilter,
    DeleteDataCellsFilterResponse (DeleteDataCellsFilterResponse'),
    newDeleteDataCellsFilterResponse,

    -- ** DeleteLFTag
    DeleteLFTag (DeleteLFTag'),
    newDeleteLFTag,
    DeleteLFTagResponse (DeleteLFTagResponse'),
    newDeleteLFTagResponse,

    -- ** DeleteObjectsOnCancel
    DeleteObjectsOnCancel (DeleteObjectsOnCancel'),
    newDeleteObjectsOnCancel,
    DeleteObjectsOnCancelResponse (DeleteObjectsOnCancelResponse'),
    newDeleteObjectsOnCancelResponse,

    -- ** DeregisterResource
    DeregisterResource (DeregisterResource'),
    newDeregisterResource,
    DeregisterResourceResponse (DeregisterResourceResponse'),
    newDeregisterResourceResponse,

    -- ** DescribeResource
    DescribeResource (DescribeResource'),
    newDescribeResource,
    DescribeResourceResponse (DescribeResourceResponse'),
    newDescribeResourceResponse,

    -- ** DescribeTransaction
    DescribeTransaction (DescribeTransaction'),
    newDescribeTransaction,
    DescribeTransactionResponse (DescribeTransactionResponse'),
    newDescribeTransactionResponse,

    -- ** ExtendTransaction
    ExtendTransaction (ExtendTransaction'),
    newExtendTransaction,
    ExtendTransactionResponse (ExtendTransactionResponse'),
    newExtendTransactionResponse,

    -- ** GetDataLakeSettings
    GetDataLakeSettings (GetDataLakeSettings'),
    newGetDataLakeSettings,
    GetDataLakeSettingsResponse (GetDataLakeSettingsResponse'),
    newGetDataLakeSettingsResponse,

    -- ** GetEffectivePermissionsForPath
    GetEffectivePermissionsForPath (GetEffectivePermissionsForPath'),
    newGetEffectivePermissionsForPath,
    GetEffectivePermissionsForPathResponse (GetEffectivePermissionsForPathResponse'),
    newGetEffectivePermissionsForPathResponse,

    -- ** GetLFTag
    GetLFTag (GetLFTag'),
    newGetLFTag,
    GetLFTagResponse (GetLFTagResponse'),
    newGetLFTagResponse,

    -- ** GetQueryState
    GetQueryState (GetQueryState'),
    newGetQueryState,
    GetQueryStateResponse (GetQueryStateResponse'),
    newGetQueryStateResponse,

    -- ** GetQueryStatistics
    GetQueryStatistics (GetQueryStatistics'),
    newGetQueryStatistics,
    GetQueryStatisticsResponse (GetQueryStatisticsResponse'),
    newGetQueryStatisticsResponse,

    -- ** GetResourceLFTags
    GetResourceLFTags (GetResourceLFTags'),
    newGetResourceLFTags,
    GetResourceLFTagsResponse (GetResourceLFTagsResponse'),
    newGetResourceLFTagsResponse,

    -- ** GetTableObjects
    GetTableObjects (GetTableObjects'),
    newGetTableObjects,
    GetTableObjectsResponse (GetTableObjectsResponse'),
    newGetTableObjectsResponse,

    -- ** GetTemporaryGluePartitionCredentials
    GetTemporaryGluePartitionCredentials (GetTemporaryGluePartitionCredentials'),
    newGetTemporaryGluePartitionCredentials,
    GetTemporaryGluePartitionCredentialsResponse (GetTemporaryGluePartitionCredentialsResponse'),
    newGetTemporaryGluePartitionCredentialsResponse,

    -- ** GetTemporaryGlueTableCredentials
    GetTemporaryGlueTableCredentials (GetTemporaryGlueTableCredentials'),
    newGetTemporaryGlueTableCredentials,
    GetTemporaryGlueTableCredentialsResponse (GetTemporaryGlueTableCredentialsResponse'),
    newGetTemporaryGlueTableCredentialsResponse,

    -- ** GetWorkUnitResults
    GetWorkUnitResults (GetWorkUnitResults'),
    newGetWorkUnitResults,
    GetWorkUnitResultsResponse (GetWorkUnitResultsResponse'),
    newGetWorkUnitResultsResponse,

    -- ** GetWorkUnits (Paginated)
    GetWorkUnits (GetWorkUnits'),
    newGetWorkUnits,
    GetWorkUnitsResponse (GetWorkUnitsResponse'),
    newGetWorkUnitsResponse,

    -- ** GrantPermissions
    GrantPermissions (GrantPermissions'),
    newGrantPermissions,
    GrantPermissionsResponse (GrantPermissionsResponse'),
    newGrantPermissionsResponse,

    -- ** ListDataCellsFilter (Paginated)
    ListDataCellsFilter (ListDataCellsFilter'),
    newListDataCellsFilter,
    ListDataCellsFilterResponse (ListDataCellsFilterResponse'),
    newListDataCellsFilterResponse,

    -- ** ListLFTags (Paginated)
    ListLFTags (ListLFTags'),
    newListLFTags,
    ListLFTagsResponse (ListLFTagsResponse'),
    newListLFTagsResponse,

    -- ** ListPermissions
    ListPermissions (ListPermissions'),
    newListPermissions,
    ListPermissionsResponse (ListPermissionsResponse'),
    newListPermissionsResponse,

    -- ** ListResources
    ListResources (ListResources'),
    newListResources,
    ListResourcesResponse (ListResourcesResponse'),
    newListResourcesResponse,

    -- ** ListTableStorageOptimizers
    ListTableStorageOptimizers (ListTableStorageOptimizers'),
    newListTableStorageOptimizers,
    ListTableStorageOptimizersResponse (ListTableStorageOptimizersResponse'),
    newListTableStorageOptimizersResponse,

    -- ** ListTransactions
    ListTransactions (ListTransactions'),
    newListTransactions,
    ListTransactionsResponse (ListTransactionsResponse'),
    newListTransactionsResponse,

    -- ** PutDataLakeSettings
    PutDataLakeSettings (PutDataLakeSettings'),
    newPutDataLakeSettings,
    PutDataLakeSettingsResponse (PutDataLakeSettingsResponse'),
    newPutDataLakeSettingsResponse,

    -- ** RegisterResource
    RegisterResource (RegisterResource'),
    newRegisterResource,
    RegisterResourceResponse (RegisterResourceResponse'),
    newRegisterResourceResponse,

    -- ** RemoveLFTagsFromResource
    RemoveLFTagsFromResource (RemoveLFTagsFromResource'),
    newRemoveLFTagsFromResource,
    RemoveLFTagsFromResourceResponse (RemoveLFTagsFromResourceResponse'),
    newRemoveLFTagsFromResourceResponse,

    -- ** RevokePermissions
    RevokePermissions (RevokePermissions'),
    newRevokePermissions,
    RevokePermissionsResponse (RevokePermissionsResponse'),
    newRevokePermissionsResponse,

    -- ** SearchDatabasesByLFTags (Paginated)
    SearchDatabasesByLFTags (SearchDatabasesByLFTags'),
    newSearchDatabasesByLFTags,
    SearchDatabasesByLFTagsResponse (SearchDatabasesByLFTagsResponse'),
    newSearchDatabasesByLFTagsResponse,

    -- ** SearchTablesByLFTags (Paginated)
    SearchTablesByLFTags (SearchTablesByLFTags'),
    newSearchTablesByLFTags,
    SearchTablesByLFTagsResponse (SearchTablesByLFTagsResponse'),
    newSearchTablesByLFTagsResponse,

    -- ** StartQueryPlanning
    StartQueryPlanning (StartQueryPlanning'),
    newStartQueryPlanning,
    StartQueryPlanningResponse (StartQueryPlanningResponse'),
    newStartQueryPlanningResponse,

    -- ** StartTransaction
    StartTransaction (StartTransaction'),
    newStartTransaction,
    StartTransactionResponse (StartTransactionResponse'),
    newStartTransactionResponse,

    -- ** UpdateLFTag
    UpdateLFTag (UpdateLFTag'),
    newUpdateLFTag,
    UpdateLFTagResponse (UpdateLFTagResponse'),
    newUpdateLFTagResponse,

    -- ** UpdateResource
    UpdateResource (UpdateResource'),
    newUpdateResource,
    UpdateResourceResponse (UpdateResourceResponse'),
    newUpdateResourceResponse,

    -- ** UpdateTableObjects
    UpdateTableObjects (UpdateTableObjects'),
    newUpdateTableObjects,
    UpdateTableObjectsResponse (UpdateTableObjectsResponse'),
    newUpdateTableObjectsResponse,

    -- ** UpdateTableStorageOptimizer
    UpdateTableStorageOptimizer (UpdateTableStorageOptimizer'),
    newUpdateTableStorageOptimizer,
    UpdateTableStorageOptimizerResponse (UpdateTableStorageOptimizerResponse'),
    newUpdateTableStorageOptimizerResponse,

    -- * Types

    -- ** ComparisonOperator
    ComparisonOperator (..),

    -- ** DataLakeResourceType
    DataLakeResourceType (..),

    -- ** FieldNameString
    FieldNameString (..),

    -- ** OptimizerType
    OptimizerType (..),

    -- ** Permission
    Permission (..),

    -- ** PermissionType
    PermissionType (..),

    -- ** QueryStateString
    QueryStateString (..),

    -- ** ResourceShareType
    ResourceShareType (..),

    -- ** ResourceType
    ResourceType (..),

    -- ** TransactionStatus
    TransactionStatus (..),

    -- ** TransactionStatusFilter
    TransactionStatusFilter (..),

    -- ** TransactionType
    TransactionType (..),

    -- ** AddObjectInput
    AddObjectInput (AddObjectInput'),
    newAddObjectInput,

    -- ** AllRowsWildcard
    AllRowsWildcard (AllRowsWildcard'),
    newAllRowsWildcard,

    -- ** AuditContext
    AuditContext (AuditContext'),
    newAuditContext,

    -- ** BatchPermissionsFailureEntry
    BatchPermissionsFailureEntry (BatchPermissionsFailureEntry'),
    newBatchPermissionsFailureEntry,

    -- ** BatchPermissionsRequestEntry
    BatchPermissionsRequestEntry (BatchPermissionsRequestEntry'),
    newBatchPermissionsRequestEntry,

    -- ** CatalogResource
    CatalogResource (CatalogResource'),
    newCatalogResource,

    -- ** ColumnLFTag
    ColumnLFTag (ColumnLFTag'),
    newColumnLFTag,

    -- ** ColumnWildcard
    ColumnWildcard (ColumnWildcard'),
    newColumnWildcard,

    -- ** DataCellsFilter
    DataCellsFilter (DataCellsFilter'),
    newDataCellsFilter,

    -- ** DataCellsFilterResource
    DataCellsFilterResource (DataCellsFilterResource'),
    newDataCellsFilterResource,

    -- ** DataLakePrincipal
    DataLakePrincipal (DataLakePrincipal'),
    newDataLakePrincipal,

    -- ** DataLakeSettings
    DataLakeSettings (DataLakeSettings'),
    newDataLakeSettings,

    -- ** DataLocationResource
    DataLocationResource (DataLocationResource'),
    newDataLocationResource,

    -- ** DatabaseResource
    DatabaseResource (DatabaseResource'),
    newDatabaseResource,

    -- ** DeleteObjectInput
    DeleteObjectInput (DeleteObjectInput'),
    newDeleteObjectInput,

    -- ** DetailsMap
    DetailsMap (DetailsMap'),
    newDetailsMap,

    -- ** ErrorDetail
    ErrorDetail (ErrorDetail'),
    newErrorDetail,

    -- ** ExecutionStatistics
    ExecutionStatistics (ExecutionStatistics'),
    newExecutionStatistics,

    -- ** FilterCondition
    FilterCondition (FilterCondition'),
    newFilterCondition,

    -- ** LFTag
    LFTag (LFTag'),
    newLFTag,

    -- ** LFTagError
    LFTagError (LFTagError'),
    newLFTagError,

    -- ** LFTagKeyResource
    LFTagKeyResource (LFTagKeyResource'),
    newLFTagKeyResource,

    -- ** LFTagPair
    LFTagPair (LFTagPair'),
    newLFTagPair,

    -- ** LFTagPolicyResource
    LFTagPolicyResource (LFTagPolicyResource'),
    newLFTagPolicyResource,

    -- ** PartitionObjects
    PartitionObjects (PartitionObjects'),
    newPartitionObjects,

    -- ** PartitionValueList
    PartitionValueList (PartitionValueList'),
    newPartitionValueList,

    -- ** PlanningStatistics
    PlanningStatistics (PlanningStatistics'),
    newPlanningStatistics,

    -- ** PrincipalPermissions
    PrincipalPermissions (PrincipalPermissions'),
    newPrincipalPermissions,

    -- ** PrincipalResourcePermissions
    PrincipalResourcePermissions (PrincipalResourcePermissions'),
    newPrincipalResourcePermissions,

    -- ** QueryPlanningContext
    QueryPlanningContext (QueryPlanningContext'),
    newQueryPlanningContext,

    -- ** Resource
    Resource (Resource'),
    newResource,

    -- ** ResourceInfo
    ResourceInfo (ResourceInfo'),
    newResourceInfo,

    -- ** RowFilter
    RowFilter (RowFilter'),
    newRowFilter,

    -- ** StorageOptimizer
    StorageOptimizer (StorageOptimizer'),
    newStorageOptimizer,

    -- ** TableObject
    TableObject (TableObject'),
    newTableObject,

    -- ** TableResource
    TableResource (TableResource'),
    newTableResource,

    -- ** TableWildcard
    TableWildcard (TableWildcard'),
    newTableWildcard,

    -- ** TableWithColumnsResource
    TableWithColumnsResource (TableWithColumnsResource'),
    newTableWithColumnsResource,

    -- ** TaggedDatabase
    TaggedDatabase (TaggedDatabase'),
    newTaggedDatabase,

    -- ** TaggedTable
    TaggedTable (TaggedTable'),
    newTaggedTable,

    -- ** TransactionDescription
    TransactionDescription (TransactionDescription'),
    newTransactionDescription,

    -- ** VirtualObject
    VirtualObject (VirtualObject'),
    newVirtualObject,

    -- ** WorkUnitRange
    WorkUnitRange (WorkUnitRange'),
    newWorkUnitRange,

    -- ** WriteOperation
    WriteOperation (WriteOperation'),
    newWriteOperation,
  )
where

import Amazonka.LakeFormation.AddLFTagsToResource
import Amazonka.LakeFormation.AssumeDecoratedRoleWithSAML
import Amazonka.LakeFormation.BatchGrantPermissions
import Amazonka.LakeFormation.BatchRevokePermissions
import Amazonka.LakeFormation.CancelTransaction
import Amazonka.LakeFormation.CommitTransaction
import Amazonka.LakeFormation.CreateDataCellsFilter
import Amazonka.LakeFormation.CreateLFTag
import Amazonka.LakeFormation.DeleteDataCellsFilter
import Amazonka.LakeFormation.DeleteLFTag
import Amazonka.LakeFormation.DeleteObjectsOnCancel
import Amazonka.LakeFormation.DeregisterResource
import Amazonka.LakeFormation.DescribeResource
import Amazonka.LakeFormation.DescribeTransaction
import Amazonka.LakeFormation.ExtendTransaction
import Amazonka.LakeFormation.GetDataLakeSettings
import Amazonka.LakeFormation.GetEffectivePermissionsForPath
import Amazonka.LakeFormation.GetLFTag
import Amazonka.LakeFormation.GetQueryState
import Amazonka.LakeFormation.GetQueryStatistics
import Amazonka.LakeFormation.GetResourceLFTags
import Amazonka.LakeFormation.GetTableObjects
import Amazonka.LakeFormation.GetTemporaryGluePartitionCredentials
import Amazonka.LakeFormation.GetTemporaryGlueTableCredentials
import Amazonka.LakeFormation.GetWorkUnitResults
import Amazonka.LakeFormation.GetWorkUnits
import Amazonka.LakeFormation.GrantPermissions
import Amazonka.LakeFormation.Lens
import Amazonka.LakeFormation.ListDataCellsFilter
import Amazonka.LakeFormation.ListLFTags
import Amazonka.LakeFormation.ListPermissions
import Amazonka.LakeFormation.ListResources
import Amazonka.LakeFormation.ListTableStorageOptimizers
import Amazonka.LakeFormation.ListTransactions
import Amazonka.LakeFormation.PutDataLakeSettings
import Amazonka.LakeFormation.RegisterResource
import Amazonka.LakeFormation.RemoveLFTagsFromResource
import Amazonka.LakeFormation.RevokePermissions
import Amazonka.LakeFormation.SearchDatabasesByLFTags
import Amazonka.LakeFormation.SearchTablesByLFTags
import Amazonka.LakeFormation.StartQueryPlanning
import Amazonka.LakeFormation.StartTransaction
import Amazonka.LakeFormation.Types
import Amazonka.LakeFormation.UpdateLFTag
import Amazonka.LakeFormation.UpdateResource
import Amazonka.LakeFormation.UpdateTableObjects
import Amazonka.LakeFormation.UpdateTableStorageOptimizer
import Amazonka.LakeFormation.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'LakeFormation'.

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
