{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.IoTSiteWise
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2019-12-02@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- Welcome to the IoT SiteWise API Reference. IoT SiteWise is an Amazon Web
-- Services service that connects
-- <https://en.wikipedia.org/wiki/Internet_of_things#Industrial_applications Industrial Internet of Things (IIoT)>
-- devices to the power of the Amazon Web Services Cloud. For more
-- information, see the
-- <https://docs.aws.amazon.com/iot-sitewise/latest/userguide/ IoT SiteWise User Guide>.
-- For information about IoT SiteWise quotas, see
-- <https://docs.aws.amazon.com/iot-sitewise/latest/userguide/quotas.html Quotas>
-- in the /IoT SiteWise User Guide/.
module Amazonka.IoTSiteWise
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** ConflictingOperationException
    _ConflictingOperationException,

    -- ** InternalFailureException
    _InternalFailureException,

    -- ** InvalidRequestException
    _InvalidRequestException,

    -- ** LimitExceededException
    _LimitExceededException,

    -- ** ResourceAlreadyExistsException
    _ResourceAlreadyExistsException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- ** ServiceUnavailableException
    _ServiceUnavailableException,

    -- ** ThrottlingException
    _ThrottlingException,

    -- ** TooManyTagsException
    _TooManyTagsException,

    -- ** UnauthorizedException
    _UnauthorizedException,

    -- * Waiters
    -- $waiters

    -- ** AssetActive
    newAssetActive,

    -- ** AssetModelActive
    newAssetModelActive,

    -- ** AssetModelNotExists
    newAssetModelNotExists,

    -- ** AssetNotExists
    newAssetNotExists,

    -- ** PortalActive
    newPortalActive,

    -- ** PortalNotExists
    newPortalNotExists,

    -- * Operations
    -- $operations

    -- ** AssociateAssets
    AssociateAssets (AssociateAssets'),
    newAssociateAssets,
    AssociateAssetsResponse (AssociateAssetsResponse'),
    newAssociateAssetsResponse,

    -- ** AssociateTimeSeriesToAssetProperty
    AssociateTimeSeriesToAssetProperty (AssociateTimeSeriesToAssetProperty'),
    newAssociateTimeSeriesToAssetProperty,
    AssociateTimeSeriesToAssetPropertyResponse (AssociateTimeSeriesToAssetPropertyResponse'),
    newAssociateTimeSeriesToAssetPropertyResponse,

    -- ** BatchAssociateProjectAssets
    BatchAssociateProjectAssets (BatchAssociateProjectAssets'),
    newBatchAssociateProjectAssets,
    BatchAssociateProjectAssetsResponse (BatchAssociateProjectAssetsResponse'),
    newBatchAssociateProjectAssetsResponse,

    -- ** BatchDisassociateProjectAssets
    BatchDisassociateProjectAssets (BatchDisassociateProjectAssets'),
    newBatchDisassociateProjectAssets,
    BatchDisassociateProjectAssetsResponse (BatchDisassociateProjectAssetsResponse'),
    newBatchDisassociateProjectAssetsResponse,

    -- ** BatchGetAssetPropertyAggregates
    BatchGetAssetPropertyAggregates (BatchGetAssetPropertyAggregates'),
    newBatchGetAssetPropertyAggregates,
    BatchGetAssetPropertyAggregatesResponse (BatchGetAssetPropertyAggregatesResponse'),
    newBatchGetAssetPropertyAggregatesResponse,

    -- ** BatchGetAssetPropertyValue
    BatchGetAssetPropertyValue (BatchGetAssetPropertyValue'),
    newBatchGetAssetPropertyValue,
    BatchGetAssetPropertyValueResponse (BatchGetAssetPropertyValueResponse'),
    newBatchGetAssetPropertyValueResponse,

    -- ** BatchGetAssetPropertyValueHistory
    BatchGetAssetPropertyValueHistory (BatchGetAssetPropertyValueHistory'),
    newBatchGetAssetPropertyValueHistory,
    BatchGetAssetPropertyValueHistoryResponse (BatchGetAssetPropertyValueHistoryResponse'),
    newBatchGetAssetPropertyValueHistoryResponse,

    -- ** BatchPutAssetPropertyValue
    BatchPutAssetPropertyValue (BatchPutAssetPropertyValue'),
    newBatchPutAssetPropertyValue,
    BatchPutAssetPropertyValueResponse (BatchPutAssetPropertyValueResponse'),
    newBatchPutAssetPropertyValueResponse,

    -- ** CreateAccessPolicy
    CreateAccessPolicy (CreateAccessPolicy'),
    newCreateAccessPolicy,
    CreateAccessPolicyResponse (CreateAccessPolicyResponse'),
    newCreateAccessPolicyResponse,

    -- ** CreateAsset
    CreateAsset (CreateAsset'),
    newCreateAsset,
    CreateAssetResponse (CreateAssetResponse'),
    newCreateAssetResponse,

    -- ** CreateAssetModel
    CreateAssetModel (CreateAssetModel'),
    newCreateAssetModel,
    CreateAssetModelResponse (CreateAssetModelResponse'),
    newCreateAssetModelResponse,

    -- ** CreateBulkImportJob
    CreateBulkImportJob (CreateBulkImportJob'),
    newCreateBulkImportJob,
    CreateBulkImportJobResponse (CreateBulkImportJobResponse'),
    newCreateBulkImportJobResponse,

    -- ** CreateDashboard
    CreateDashboard (CreateDashboard'),
    newCreateDashboard,
    CreateDashboardResponse (CreateDashboardResponse'),
    newCreateDashboardResponse,

    -- ** CreateGateway
    CreateGateway (CreateGateway'),
    newCreateGateway,
    CreateGatewayResponse (CreateGatewayResponse'),
    newCreateGatewayResponse,

    -- ** CreatePortal
    CreatePortal (CreatePortal'),
    newCreatePortal,
    CreatePortalResponse (CreatePortalResponse'),
    newCreatePortalResponse,

    -- ** CreateProject
    CreateProject (CreateProject'),
    newCreateProject,
    CreateProjectResponse (CreateProjectResponse'),
    newCreateProjectResponse,

    -- ** DeleteAccessPolicy
    DeleteAccessPolicy (DeleteAccessPolicy'),
    newDeleteAccessPolicy,
    DeleteAccessPolicyResponse (DeleteAccessPolicyResponse'),
    newDeleteAccessPolicyResponse,

    -- ** DeleteAsset
    DeleteAsset (DeleteAsset'),
    newDeleteAsset,
    DeleteAssetResponse (DeleteAssetResponse'),
    newDeleteAssetResponse,

    -- ** DeleteAssetModel
    DeleteAssetModel (DeleteAssetModel'),
    newDeleteAssetModel,
    DeleteAssetModelResponse (DeleteAssetModelResponse'),
    newDeleteAssetModelResponse,

    -- ** DeleteDashboard
    DeleteDashboard (DeleteDashboard'),
    newDeleteDashboard,
    DeleteDashboardResponse (DeleteDashboardResponse'),
    newDeleteDashboardResponse,

    -- ** DeleteGateway
    DeleteGateway (DeleteGateway'),
    newDeleteGateway,
    DeleteGatewayResponse (DeleteGatewayResponse'),
    newDeleteGatewayResponse,

    -- ** DeletePortal
    DeletePortal (DeletePortal'),
    newDeletePortal,
    DeletePortalResponse (DeletePortalResponse'),
    newDeletePortalResponse,

    -- ** DeleteProject
    DeleteProject (DeleteProject'),
    newDeleteProject,
    DeleteProjectResponse (DeleteProjectResponse'),
    newDeleteProjectResponse,

    -- ** DeleteTimeSeries
    DeleteTimeSeries (DeleteTimeSeries'),
    newDeleteTimeSeries,
    DeleteTimeSeriesResponse (DeleteTimeSeriesResponse'),
    newDeleteTimeSeriesResponse,

    -- ** DescribeAccessPolicy
    DescribeAccessPolicy (DescribeAccessPolicy'),
    newDescribeAccessPolicy,
    DescribeAccessPolicyResponse (DescribeAccessPolicyResponse'),
    newDescribeAccessPolicyResponse,

    -- ** DescribeAsset
    DescribeAsset (DescribeAsset'),
    newDescribeAsset,
    DescribeAssetResponse (DescribeAssetResponse'),
    newDescribeAssetResponse,

    -- ** DescribeAssetModel
    DescribeAssetModel (DescribeAssetModel'),
    newDescribeAssetModel,
    DescribeAssetModelResponse (DescribeAssetModelResponse'),
    newDescribeAssetModelResponse,

    -- ** DescribeAssetProperty
    DescribeAssetProperty (DescribeAssetProperty'),
    newDescribeAssetProperty,
    DescribeAssetPropertyResponse (DescribeAssetPropertyResponse'),
    newDescribeAssetPropertyResponse,

    -- ** DescribeBulkImportJob
    DescribeBulkImportJob (DescribeBulkImportJob'),
    newDescribeBulkImportJob,
    DescribeBulkImportJobResponse (DescribeBulkImportJobResponse'),
    newDescribeBulkImportJobResponse,

    -- ** DescribeDashboard
    DescribeDashboard (DescribeDashboard'),
    newDescribeDashboard,
    DescribeDashboardResponse (DescribeDashboardResponse'),
    newDescribeDashboardResponse,

    -- ** DescribeDefaultEncryptionConfiguration
    DescribeDefaultEncryptionConfiguration (DescribeDefaultEncryptionConfiguration'),
    newDescribeDefaultEncryptionConfiguration,
    DescribeDefaultEncryptionConfigurationResponse (DescribeDefaultEncryptionConfigurationResponse'),
    newDescribeDefaultEncryptionConfigurationResponse,

    -- ** DescribeGateway
    DescribeGateway (DescribeGateway'),
    newDescribeGateway,
    DescribeGatewayResponse (DescribeGatewayResponse'),
    newDescribeGatewayResponse,

    -- ** DescribeGatewayCapabilityConfiguration
    DescribeGatewayCapabilityConfiguration (DescribeGatewayCapabilityConfiguration'),
    newDescribeGatewayCapabilityConfiguration,
    DescribeGatewayCapabilityConfigurationResponse (DescribeGatewayCapabilityConfigurationResponse'),
    newDescribeGatewayCapabilityConfigurationResponse,

    -- ** DescribeLoggingOptions
    DescribeLoggingOptions (DescribeLoggingOptions'),
    newDescribeLoggingOptions,
    DescribeLoggingOptionsResponse (DescribeLoggingOptionsResponse'),
    newDescribeLoggingOptionsResponse,

    -- ** DescribePortal
    DescribePortal (DescribePortal'),
    newDescribePortal,
    DescribePortalResponse (DescribePortalResponse'),
    newDescribePortalResponse,

    -- ** DescribeProject
    DescribeProject (DescribeProject'),
    newDescribeProject,
    DescribeProjectResponse (DescribeProjectResponse'),
    newDescribeProjectResponse,

    -- ** DescribeStorageConfiguration
    DescribeStorageConfiguration (DescribeStorageConfiguration'),
    newDescribeStorageConfiguration,
    DescribeStorageConfigurationResponse (DescribeStorageConfigurationResponse'),
    newDescribeStorageConfigurationResponse,

    -- ** DescribeTimeSeries
    DescribeTimeSeries (DescribeTimeSeries'),
    newDescribeTimeSeries,
    DescribeTimeSeriesResponse (DescribeTimeSeriesResponse'),
    newDescribeTimeSeriesResponse,

    -- ** DisassociateAssets
    DisassociateAssets (DisassociateAssets'),
    newDisassociateAssets,
    DisassociateAssetsResponse (DisassociateAssetsResponse'),
    newDisassociateAssetsResponse,

    -- ** DisassociateTimeSeriesFromAssetProperty
    DisassociateTimeSeriesFromAssetProperty (DisassociateTimeSeriesFromAssetProperty'),
    newDisassociateTimeSeriesFromAssetProperty,
    DisassociateTimeSeriesFromAssetPropertyResponse (DisassociateTimeSeriesFromAssetPropertyResponse'),
    newDisassociateTimeSeriesFromAssetPropertyResponse,

    -- ** GetAssetPropertyAggregates (Paginated)
    GetAssetPropertyAggregates (GetAssetPropertyAggregates'),
    newGetAssetPropertyAggregates,
    GetAssetPropertyAggregatesResponse (GetAssetPropertyAggregatesResponse'),
    newGetAssetPropertyAggregatesResponse,

    -- ** GetAssetPropertyValue
    GetAssetPropertyValue (GetAssetPropertyValue'),
    newGetAssetPropertyValue,
    GetAssetPropertyValueResponse (GetAssetPropertyValueResponse'),
    newGetAssetPropertyValueResponse,

    -- ** GetAssetPropertyValueHistory (Paginated)
    GetAssetPropertyValueHistory (GetAssetPropertyValueHistory'),
    newGetAssetPropertyValueHistory,
    GetAssetPropertyValueHistoryResponse (GetAssetPropertyValueHistoryResponse'),
    newGetAssetPropertyValueHistoryResponse,

    -- ** GetInterpolatedAssetPropertyValues (Paginated)
    GetInterpolatedAssetPropertyValues (GetInterpolatedAssetPropertyValues'),
    newGetInterpolatedAssetPropertyValues,
    GetInterpolatedAssetPropertyValuesResponse (GetInterpolatedAssetPropertyValuesResponse'),
    newGetInterpolatedAssetPropertyValuesResponse,

    -- ** ListAccessPolicies (Paginated)
    ListAccessPolicies (ListAccessPolicies'),
    newListAccessPolicies,
    ListAccessPoliciesResponse (ListAccessPoliciesResponse'),
    newListAccessPoliciesResponse,

    -- ** ListAssetModelProperties (Paginated)
    ListAssetModelProperties (ListAssetModelProperties'),
    newListAssetModelProperties,
    ListAssetModelPropertiesResponse (ListAssetModelPropertiesResponse'),
    newListAssetModelPropertiesResponse,

    -- ** ListAssetModels (Paginated)
    ListAssetModels (ListAssetModels'),
    newListAssetModels,
    ListAssetModelsResponse (ListAssetModelsResponse'),
    newListAssetModelsResponse,

    -- ** ListAssetProperties (Paginated)
    ListAssetProperties (ListAssetProperties'),
    newListAssetProperties,
    ListAssetPropertiesResponse (ListAssetPropertiesResponse'),
    newListAssetPropertiesResponse,

    -- ** ListAssetRelationships (Paginated)
    ListAssetRelationships (ListAssetRelationships'),
    newListAssetRelationships,
    ListAssetRelationshipsResponse (ListAssetRelationshipsResponse'),
    newListAssetRelationshipsResponse,

    -- ** ListAssets (Paginated)
    ListAssets (ListAssets'),
    newListAssets,
    ListAssetsResponse (ListAssetsResponse'),
    newListAssetsResponse,

    -- ** ListAssociatedAssets (Paginated)
    ListAssociatedAssets (ListAssociatedAssets'),
    newListAssociatedAssets,
    ListAssociatedAssetsResponse (ListAssociatedAssetsResponse'),
    newListAssociatedAssetsResponse,

    -- ** ListBulkImportJobs (Paginated)
    ListBulkImportJobs (ListBulkImportJobs'),
    newListBulkImportJobs,
    ListBulkImportJobsResponse (ListBulkImportJobsResponse'),
    newListBulkImportJobsResponse,

    -- ** ListDashboards (Paginated)
    ListDashboards (ListDashboards'),
    newListDashboards,
    ListDashboardsResponse (ListDashboardsResponse'),
    newListDashboardsResponse,

    -- ** ListGateways (Paginated)
    ListGateways (ListGateways'),
    newListGateways,
    ListGatewaysResponse (ListGatewaysResponse'),
    newListGatewaysResponse,

    -- ** ListPortals (Paginated)
    ListPortals (ListPortals'),
    newListPortals,
    ListPortalsResponse (ListPortalsResponse'),
    newListPortalsResponse,

    -- ** ListProjectAssets (Paginated)
    ListProjectAssets (ListProjectAssets'),
    newListProjectAssets,
    ListProjectAssetsResponse (ListProjectAssetsResponse'),
    newListProjectAssetsResponse,

    -- ** ListProjects (Paginated)
    ListProjects (ListProjects'),
    newListProjects,
    ListProjectsResponse (ListProjectsResponse'),
    newListProjectsResponse,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** ListTimeSeries (Paginated)
    ListTimeSeries (ListTimeSeries'),
    newListTimeSeries,
    ListTimeSeriesResponse (ListTimeSeriesResponse'),
    newListTimeSeriesResponse,

    -- ** PutDefaultEncryptionConfiguration
    PutDefaultEncryptionConfiguration (PutDefaultEncryptionConfiguration'),
    newPutDefaultEncryptionConfiguration,
    PutDefaultEncryptionConfigurationResponse (PutDefaultEncryptionConfigurationResponse'),
    newPutDefaultEncryptionConfigurationResponse,

    -- ** PutLoggingOptions
    PutLoggingOptions (PutLoggingOptions'),
    newPutLoggingOptions,
    PutLoggingOptionsResponse (PutLoggingOptionsResponse'),
    newPutLoggingOptionsResponse,

    -- ** PutStorageConfiguration
    PutStorageConfiguration (PutStorageConfiguration'),
    newPutStorageConfiguration,
    PutStorageConfigurationResponse (PutStorageConfigurationResponse'),
    newPutStorageConfigurationResponse,

    -- ** TagResource
    TagResource (TagResource'),
    newTagResource,
    TagResourceResponse (TagResourceResponse'),
    newTagResourceResponse,

    -- ** UntagResource
    UntagResource (UntagResource'),
    newUntagResource,
    UntagResourceResponse (UntagResourceResponse'),
    newUntagResourceResponse,

    -- ** UpdateAccessPolicy
    UpdateAccessPolicy (UpdateAccessPolicy'),
    newUpdateAccessPolicy,
    UpdateAccessPolicyResponse (UpdateAccessPolicyResponse'),
    newUpdateAccessPolicyResponse,

    -- ** UpdateAsset
    UpdateAsset (UpdateAsset'),
    newUpdateAsset,
    UpdateAssetResponse (UpdateAssetResponse'),
    newUpdateAssetResponse,

    -- ** UpdateAssetModel
    UpdateAssetModel (UpdateAssetModel'),
    newUpdateAssetModel,
    UpdateAssetModelResponse (UpdateAssetModelResponse'),
    newUpdateAssetModelResponse,

    -- ** UpdateAssetProperty
    UpdateAssetProperty (UpdateAssetProperty'),
    newUpdateAssetProperty,
    UpdateAssetPropertyResponse (UpdateAssetPropertyResponse'),
    newUpdateAssetPropertyResponse,

    -- ** UpdateDashboard
    UpdateDashboard (UpdateDashboard'),
    newUpdateDashboard,
    UpdateDashboardResponse (UpdateDashboardResponse'),
    newUpdateDashboardResponse,

    -- ** UpdateGateway
    UpdateGateway (UpdateGateway'),
    newUpdateGateway,
    UpdateGatewayResponse (UpdateGatewayResponse'),
    newUpdateGatewayResponse,

    -- ** UpdateGatewayCapabilityConfiguration
    UpdateGatewayCapabilityConfiguration (UpdateGatewayCapabilityConfiguration'),
    newUpdateGatewayCapabilityConfiguration,
    UpdateGatewayCapabilityConfigurationResponse (UpdateGatewayCapabilityConfigurationResponse'),
    newUpdateGatewayCapabilityConfigurationResponse,

    -- ** UpdatePortal
    UpdatePortal (UpdatePortal'),
    newUpdatePortal,
    UpdatePortalResponse (UpdatePortalResponse'),
    newUpdatePortalResponse,

    -- ** UpdateProject
    UpdateProject (UpdateProject'),
    newUpdateProject,
    UpdateProjectResponse (UpdateProjectResponse'),
    newUpdateProjectResponse,

    -- * Types

    -- ** AggregateType
    AggregateType (..),

    -- ** AssetErrorCode
    AssetErrorCode (..),

    -- ** AssetModelState
    AssetModelState (..),

    -- ** AssetRelationshipType
    AssetRelationshipType (..),

    -- ** AssetState
    AssetState (..),

    -- ** AuthMode
    AuthMode (..),

    -- ** BatchEntryCompletionStatus
    BatchEntryCompletionStatus (..),

    -- ** BatchGetAssetPropertyAggregatesErrorCode
    BatchGetAssetPropertyAggregatesErrorCode (..),

    -- ** BatchGetAssetPropertyValueErrorCode
    BatchGetAssetPropertyValueErrorCode (..),

    -- ** BatchGetAssetPropertyValueHistoryErrorCode
    BatchGetAssetPropertyValueHistoryErrorCode (..),

    -- ** BatchPutAssetPropertyValueErrorCode
    BatchPutAssetPropertyValueErrorCode (..),

    -- ** CapabilitySyncStatus
    CapabilitySyncStatus (..),

    -- ** ColumnName
    ColumnName (..),

    -- ** ComputeLocation
    ComputeLocation (..),

    -- ** ConfigurationState
    ConfigurationState (..),

    -- ** DetailedErrorCode
    DetailedErrorCode (..),

    -- ** DisassociatedDataStorageState
    DisassociatedDataStorageState (..),

    -- ** EncryptionType
    EncryptionType (..),

    -- ** ErrorCode
    ErrorCode (..),

    -- ** ForwardingConfigState
    ForwardingConfigState (..),

    -- ** IdentityType
    IdentityType (..),

    -- ** ImageFileType
    ImageFileType (..),

    -- ** JobStatus
    JobStatus (..),

    -- ** ListAssetModelPropertiesFilter
    ListAssetModelPropertiesFilter (..),

    -- ** ListAssetPropertiesFilter
    ListAssetPropertiesFilter (..),

    -- ** ListAssetsFilter
    ListAssetsFilter (..),

    -- ** ListBulkImportJobsFilter
    ListBulkImportJobsFilter (..),

    -- ** ListTimeSeriesType
    ListTimeSeriesType (..),

    -- ** LoggingLevel
    LoggingLevel (..),

    -- ** MonitorErrorCode
    MonitorErrorCode (..),

    -- ** Permission
    Permission (..),

    -- ** PortalState
    PortalState (..),

    -- ** PropertyDataType
    PropertyDataType (..),

    -- ** PropertyNotificationState
    PropertyNotificationState (..),

    -- ** Quality
    Quality (..),

    -- ** ResourceType
    ResourceType (..),

    -- ** StorageType
    StorageType (..),

    -- ** TimeOrdering
    TimeOrdering (..),

    -- ** TraversalDirection
    TraversalDirection (..),

    -- ** TraversalType
    TraversalType (..),

    -- ** AccessPolicySummary
    AccessPolicySummary (AccessPolicySummary'),
    newAccessPolicySummary,

    -- ** AggregatedValue
    AggregatedValue (AggregatedValue'),
    newAggregatedValue,

    -- ** Aggregates
    Aggregates (Aggregates'),
    newAggregates,

    -- ** Alarms
    Alarms (Alarms'),
    newAlarms,

    -- ** AssetCompositeModel
    AssetCompositeModel (AssetCompositeModel'),
    newAssetCompositeModel,

    -- ** AssetErrorDetails
    AssetErrorDetails (AssetErrorDetails'),
    newAssetErrorDetails,

    -- ** AssetHierarchy
    AssetHierarchy (AssetHierarchy'),
    newAssetHierarchy,

    -- ** AssetHierarchyInfo
    AssetHierarchyInfo (AssetHierarchyInfo'),
    newAssetHierarchyInfo,

    -- ** AssetModelCompositeModel
    AssetModelCompositeModel (AssetModelCompositeModel'),
    newAssetModelCompositeModel,

    -- ** AssetModelCompositeModelDefinition
    AssetModelCompositeModelDefinition (AssetModelCompositeModelDefinition'),
    newAssetModelCompositeModelDefinition,

    -- ** AssetModelHierarchy
    AssetModelHierarchy (AssetModelHierarchy'),
    newAssetModelHierarchy,

    -- ** AssetModelHierarchyDefinition
    AssetModelHierarchyDefinition (AssetModelHierarchyDefinition'),
    newAssetModelHierarchyDefinition,

    -- ** AssetModelProperty
    AssetModelProperty (AssetModelProperty'),
    newAssetModelProperty,

    -- ** AssetModelPropertyDefinition
    AssetModelPropertyDefinition (AssetModelPropertyDefinition'),
    newAssetModelPropertyDefinition,

    -- ** AssetModelPropertySummary
    AssetModelPropertySummary (AssetModelPropertySummary'),
    newAssetModelPropertySummary,

    -- ** AssetModelStatus
    AssetModelStatus (AssetModelStatus'),
    newAssetModelStatus,

    -- ** AssetModelSummary
    AssetModelSummary (AssetModelSummary'),
    newAssetModelSummary,

    -- ** AssetProperty
    AssetProperty (AssetProperty'),
    newAssetProperty,

    -- ** AssetPropertySummary
    AssetPropertySummary (AssetPropertySummary'),
    newAssetPropertySummary,

    -- ** AssetPropertyValue
    AssetPropertyValue (AssetPropertyValue'),
    newAssetPropertyValue,

    -- ** AssetRelationshipSummary
    AssetRelationshipSummary (AssetRelationshipSummary'),
    newAssetRelationshipSummary,

    -- ** AssetStatus
    AssetStatus (AssetStatus'),
    newAssetStatus,

    -- ** AssetSummary
    AssetSummary (AssetSummary'),
    newAssetSummary,

    -- ** AssociatedAssetsSummary
    AssociatedAssetsSummary (AssociatedAssetsSummary'),
    newAssociatedAssetsSummary,

    -- ** Attribute
    Attribute (Attribute'),
    newAttribute,

    -- ** BatchGetAssetPropertyAggregatesEntry
    BatchGetAssetPropertyAggregatesEntry (BatchGetAssetPropertyAggregatesEntry'),
    newBatchGetAssetPropertyAggregatesEntry,

    -- ** BatchGetAssetPropertyAggregatesErrorEntry
    BatchGetAssetPropertyAggregatesErrorEntry (BatchGetAssetPropertyAggregatesErrorEntry'),
    newBatchGetAssetPropertyAggregatesErrorEntry,

    -- ** BatchGetAssetPropertyAggregatesErrorInfo
    BatchGetAssetPropertyAggregatesErrorInfo (BatchGetAssetPropertyAggregatesErrorInfo'),
    newBatchGetAssetPropertyAggregatesErrorInfo,

    -- ** BatchGetAssetPropertyAggregatesSkippedEntry
    BatchGetAssetPropertyAggregatesSkippedEntry (BatchGetAssetPropertyAggregatesSkippedEntry'),
    newBatchGetAssetPropertyAggregatesSkippedEntry,

    -- ** BatchGetAssetPropertyAggregatesSuccessEntry
    BatchGetAssetPropertyAggregatesSuccessEntry (BatchGetAssetPropertyAggregatesSuccessEntry'),
    newBatchGetAssetPropertyAggregatesSuccessEntry,

    -- ** BatchGetAssetPropertyValueEntry
    BatchGetAssetPropertyValueEntry (BatchGetAssetPropertyValueEntry'),
    newBatchGetAssetPropertyValueEntry,

    -- ** BatchGetAssetPropertyValueErrorEntry
    BatchGetAssetPropertyValueErrorEntry (BatchGetAssetPropertyValueErrorEntry'),
    newBatchGetAssetPropertyValueErrorEntry,

    -- ** BatchGetAssetPropertyValueErrorInfo
    BatchGetAssetPropertyValueErrorInfo (BatchGetAssetPropertyValueErrorInfo'),
    newBatchGetAssetPropertyValueErrorInfo,

    -- ** BatchGetAssetPropertyValueHistoryEntry
    BatchGetAssetPropertyValueHistoryEntry (BatchGetAssetPropertyValueHistoryEntry'),
    newBatchGetAssetPropertyValueHistoryEntry,

    -- ** BatchGetAssetPropertyValueHistoryErrorEntry
    BatchGetAssetPropertyValueHistoryErrorEntry (BatchGetAssetPropertyValueHistoryErrorEntry'),
    newBatchGetAssetPropertyValueHistoryErrorEntry,

    -- ** BatchGetAssetPropertyValueHistoryErrorInfo
    BatchGetAssetPropertyValueHistoryErrorInfo (BatchGetAssetPropertyValueHistoryErrorInfo'),
    newBatchGetAssetPropertyValueHistoryErrorInfo,

    -- ** BatchGetAssetPropertyValueHistorySkippedEntry
    BatchGetAssetPropertyValueHistorySkippedEntry (BatchGetAssetPropertyValueHistorySkippedEntry'),
    newBatchGetAssetPropertyValueHistorySkippedEntry,

    -- ** BatchGetAssetPropertyValueHistorySuccessEntry
    BatchGetAssetPropertyValueHistorySuccessEntry (BatchGetAssetPropertyValueHistorySuccessEntry'),
    newBatchGetAssetPropertyValueHistorySuccessEntry,

    -- ** BatchGetAssetPropertyValueSkippedEntry
    BatchGetAssetPropertyValueSkippedEntry (BatchGetAssetPropertyValueSkippedEntry'),
    newBatchGetAssetPropertyValueSkippedEntry,

    -- ** BatchGetAssetPropertyValueSuccessEntry
    BatchGetAssetPropertyValueSuccessEntry (BatchGetAssetPropertyValueSuccessEntry'),
    newBatchGetAssetPropertyValueSuccessEntry,

    -- ** BatchPutAssetPropertyError
    BatchPutAssetPropertyError (BatchPutAssetPropertyError'),
    newBatchPutAssetPropertyError,

    -- ** BatchPutAssetPropertyErrorEntry
    BatchPutAssetPropertyErrorEntry (BatchPutAssetPropertyErrorEntry'),
    newBatchPutAssetPropertyErrorEntry,

    -- ** CompositeModelProperty
    CompositeModelProperty (CompositeModelProperty'),
    newCompositeModelProperty,

    -- ** ConfigurationErrorDetails
    ConfigurationErrorDetails (ConfigurationErrorDetails'),
    newConfigurationErrorDetails,

    -- ** ConfigurationStatus
    ConfigurationStatus (ConfigurationStatus'),
    newConfigurationStatus,

    -- ** Csv
    Csv (Csv'),
    newCsv,

    -- ** CustomerManagedS3Storage
    CustomerManagedS3Storage (CustomerManagedS3Storage'),
    newCustomerManagedS3Storage,

    -- ** DashboardSummary
    DashboardSummary (DashboardSummary'),
    newDashboardSummary,

    -- ** DetailedError
    DetailedError (DetailedError'),
    newDetailedError,

    -- ** ErrorDetails
    ErrorDetails (ErrorDetails'),
    newErrorDetails,

    -- ** ErrorReportLocation
    ErrorReportLocation (ErrorReportLocation'),
    newErrorReportLocation,

    -- ** ExpressionVariable
    ExpressionVariable (ExpressionVariable'),
    newExpressionVariable,

    -- ** File
    File (File'),
    newFile,

    -- ** FileFormat
    FileFormat (FileFormat'),
    newFileFormat,

    -- ** ForwardingConfig
    ForwardingConfig (ForwardingConfig'),
    newForwardingConfig,

    -- ** GatewayCapabilitySummary
    GatewayCapabilitySummary (GatewayCapabilitySummary'),
    newGatewayCapabilitySummary,

    -- ** GatewayPlatform
    GatewayPlatform (GatewayPlatform'),
    newGatewayPlatform,

    -- ** GatewaySummary
    GatewaySummary (GatewaySummary'),
    newGatewaySummary,

    -- ** Greengrass
    Greengrass (Greengrass'),
    newGreengrass,

    -- ** GreengrassV2
    GreengrassV2 (GreengrassV2'),
    newGreengrassV2,

    -- ** GroupIdentity
    GroupIdentity (GroupIdentity'),
    newGroupIdentity,

    -- ** IAMRoleIdentity
    IAMRoleIdentity (IAMRoleIdentity'),
    newIAMRoleIdentity,

    -- ** IAMUserIdentity
    IAMUserIdentity (IAMUserIdentity'),
    newIAMUserIdentity,

    -- ** Identity
    Identity (Identity'),
    newIdentity,

    -- ** Image
    Image (Image'),
    newImage,

    -- ** ImageFile
    ImageFile (ImageFile'),
    newImageFile,

    -- ** ImageLocation
    ImageLocation (ImageLocation'),
    newImageLocation,

    -- ** InterpolatedAssetPropertyValue
    InterpolatedAssetPropertyValue (InterpolatedAssetPropertyValue'),
    newInterpolatedAssetPropertyValue,

    -- ** JobConfiguration
    JobConfiguration (JobConfiguration'),
    newJobConfiguration,

    -- ** JobSummary
    JobSummary (JobSummary'),
    newJobSummary,

    -- ** LoggingOptions
    LoggingOptions (LoggingOptions'),
    newLoggingOptions,

    -- ** Measurement
    Measurement (Measurement'),
    newMeasurement,

    -- ** MeasurementProcessingConfig
    MeasurementProcessingConfig (MeasurementProcessingConfig'),
    newMeasurementProcessingConfig,

    -- ** Metric
    Metric (Metric'),
    newMetric,

    -- ** MetricProcessingConfig
    MetricProcessingConfig (MetricProcessingConfig'),
    newMetricProcessingConfig,

    -- ** MetricWindow
    MetricWindow (MetricWindow'),
    newMetricWindow,

    -- ** MonitorErrorDetails
    MonitorErrorDetails (MonitorErrorDetails'),
    newMonitorErrorDetails,

    -- ** MultiLayerStorage
    MultiLayerStorage (MultiLayerStorage'),
    newMultiLayerStorage,

    -- ** PortalResource
    PortalResource (PortalResource'),
    newPortalResource,

    -- ** PortalStatus
    PortalStatus (PortalStatus'),
    newPortalStatus,

    -- ** PortalSummary
    PortalSummary (PortalSummary'),
    newPortalSummary,

    -- ** ProjectResource
    ProjectResource (ProjectResource'),
    newProjectResource,

    -- ** ProjectSummary
    ProjectSummary (ProjectSummary'),
    newProjectSummary,

    -- ** Property
    Property (Property'),
    newProperty,

    -- ** PropertyNotification
    PropertyNotification (PropertyNotification'),
    newPropertyNotification,

    -- ** PropertyType
    PropertyType (PropertyType'),
    newPropertyType,

    -- ** PutAssetPropertyValueEntry
    PutAssetPropertyValueEntry (PutAssetPropertyValueEntry'),
    newPutAssetPropertyValueEntry,

    -- ** Resource
    Resource (Resource'),
    newResource,

    -- ** RetentionPeriod
    RetentionPeriod (RetentionPeriod'),
    newRetentionPeriod,

    -- ** TimeInNanos
    TimeInNanos (TimeInNanos'),
    newTimeInNanos,

    -- ** TimeSeriesSummary
    TimeSeriesSummary (TimeSeriesSummary'),
    newTimeSeriesSummary,

    -- ** Transform
    Transform (Transform'),
    newTransform,

    -- ** TransformProcessingConfig
    TransformProcessingConfig (TransformProcessingConfig'),
    newTransformProcessingConfig,

    -- ** TumblingWindow
    TumblingWindow (TumblingWindow'),
    newTumblingWindow,

    -- ** UserIdentity
    UserIdentity (UserIdentity'),
    newUserIdentity,

    -- ** VariableValue
    VariableValue (VariableValue'),
    newVariableValue,

    -- ** Variant
    Variant (Variant'),
    newVariant,
  )
where

import Amazonka.IoTSiteWise.AssociateAssets
import Amazonka.IoTSiteWise.AssociateTimeSeriesToAssetProperty
import Amazonka.IoTSiteWise.BatchAssociateProjectAssets
import Amazonka.IoTSiteWise.BatchDisassociateProjectAssets
import Amazonka.IoTSiteWise.BatchGetAssetPropertyAggregates
import Amazonka.IoTSiteWise.BatchGetAssetPropertyValue
import Amazonka.IoTSiteWise.BatchGetAssetPropertyValueHistory
import Amazonka.IoTSiteWise.BatchPutAssetPropertyValue
import Amazonka.IoTSiteWise.CreateAccessPolicy
import Amazonka.IoTSiteWise.CreateAsset
import Amazonka.IoTSiteWise.CreateAssetModel
import Amazonka.IoTSiteWise.CreateBulkImportJob
import Amazonka.IoTSiteWise.CreateDashboard
import Amazonka.IoTSiteWise.CreateGateway
import Amazonka.IoTSiteWise.CreatePortal
import Amazonka.IoTSiteWise.CreateProject
import Amazonka.IoTSiteWise.DeleteAccessPolicy
import Amazonka.IoTSiteWise.DeleteAsset
import Amazonka.IoTSiteWise.DeleteAssetModel
import Amazonka.IoTSiteWise.DeleteDashboard
import Amazonka.IoTSiteWise.DeleteGateway
import Amazonka.IoTSiteWise.DeletePortal
import Amazonka.IoTSiteWise.DeleteProject
import Amazonka.IoTSiteWise.DeleteTimeSeries
import Amazonka.IoTSiteWise.DescribeAccessPolicy
import Amazonka.IoTSiteWise.DescribeAsset
import Amazonka.IoTSiteWise.DescribeAssetModel
import Amazonka.IoTSiteWise.DescribeAssetProperty
import Amazonka.IoTSiteWise.DescribeBulkImportJob
import Amazonka.IoTSiteWise.DescribeDashboard
import Amazonka.IoTSiteWise.DescribeDefaultEncryptionConfiguration
import Amazonka.IoTSiteWise.DescribeGateway
import Amazonka.IoTSiteWise.DescribeGatewayCapabilityConfiguration
import Amazonka.IoTSiteWise.DescribeLoggingOptions
import Amazonka.IoTSiteWise.DescribePortal
import Amazonka.IoTSiteWise.DescribeProject
import Amazonka.IoTSiteWise.DescribeStorageConfiguration
import Amazonka.IoTSiteWise.DescribeTimeSeries
import Amazonka.IoTSiteWise.DisassociateAssets
import Amazonka.IoTSiteWise.DisassociateTimeSeriesFromAssetProperty
import Amazonka.IoTSiteWise.GetAssetPropertyAggregates
import Amazonka.IoTSiteWise.GetAssetPropertyValue
import Amazonka.IoTSiteWise.GetAssetPropertyValueHistory
import Amazonka.IoTSiteWise.GetInterpolatedAssetPropertyValues
import Amazonka.IoTSiteWise.Lens
import Amazonka.IoTSiteWise.ListAccessPolicies
import Amazonka.IoTSiteWise.ListAssetModelProperties
import Amazonka.IoTSiteWise.ListAssetModels
import Amazonka.IoTSiteWise.ListAssetProperties
import Amazonka.IoTSiteWise.ListAssetRelationships
import Amazonka.IoTSiteWise.ListAssets
import Amazonka.IoTSiteWise.ListAssociatedAssets
import Amazonka.IoTSiteWise.ListBulkImportJobs
import Amazonka.IoTSiteWise.ListDashboards
import Amazonka.IoTSiteWise.ListGateways
import Amazonka.IoTSiteWise.ListPortals
import Amazonka.IoTSiteWise.ListProjectAssets
import Amazonka.IoTSiteWise.ListProjects
import Amazonka.IoTSiteWise.ListTagsForResource
import Amazonka.IoTSiteWise.ListTimeSeries
import Amazonka.IoTSiteWise.PutDefaultEncryptionConfiguration
import Amazonka.IoTSiteWise.PutLoggingOptions
import Amazonka.IoTSiteWise.PutStorageConfiguration
import Amazonka.IoTSiteWise.TagResource
import Amazonka.IoTSiteWise.Types
import Amazonka.IoTSiteWise.UntagResource
import Amazonka.IoTSiteWise.UpdateAccessPolicy
import Amazonka.IoTSiteWise.UpdateAsset
import Amazonka.IoTSiteWise.UpdateAssetModel
import Amazonka.IoTSiteWise.UpdateAssetProperty
import Amazonka.IoTSiteWise.UpdateDashboard
import Amazonka.IoTSiteWise.UpdateGateway
import Amazonka.IoTSiteWise.UpdateGatewayCapabilityConfiguration
import Amazonka.IoTSiteWise.UpdatePortal
import Amazonka.IoTSiteWise.UpdateProject
import Amazonka.IoTSiteWise.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'IoTSiteWise'.

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
