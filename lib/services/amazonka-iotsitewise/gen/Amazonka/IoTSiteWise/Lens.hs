{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.IoTSiteWise.Lens
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTSiteWise.Lens
  ( -- * Operations

    -- ** AssociateAssets
    associateAssets_clientToken,
    associateAssets_assetId,
    associateAssets_hierarchyId,
    associateAssets_childAssetId,

    -- ** AssociateTimeSeriesToAssetProperty
    associateTimeSeriesToAssetProperty_clientToken,
    associateTimeSeriesToAssetProperty_alias,
    associateTimeSeriesToAssetProperty_assetId,
    associateTimeSeriesToAssetProperty_propertyId,

    -- ** BatchAssociateProjectAssets
    batchAssociateProjectAssets_clientToken,
    batchAssociateProjectAssets_projectId,
    batchAssociateProjectAssets_assetIds,
    batchAssociateProjectAssetsResponse_errors,
    batchAssociateProjectAssetsResponse_httpStatus,

    -- ** BatchDisassociateProjectAssets
    batchDisassociateProjectAssets_clientToken,
    batchDisassociateProjectAssets_projectId,
    batchDisassociateProjectAssets_assetIds,
    batchDisassociateProjectAssetsResponse_errors,
    batchDisassociateProjectAssetsResponse_httpStatus,

    -- ** BatchGetAssetPropertyAggregates
    batchGetAssetPropertyAggregates_maxResults,
    batchGetAssetPropertyAggregates_nextToken,
    batchGetAssetPropertyAggregates_entries,
    batchGetAssetPropertyAggregatesResponse_nextToken,
    batchGetAssetPropertyAggregatesResponse_httpStatus,
    batchGetAssetPropertyAggregatesResponse_errorEntries,
    batchGetAssetPropertyAggregatesResponse_successEntries,
    batchGetAssetPropertyAggregatesResponse_skippedEntries,

    -- ** BatchGetAssetPropertyValue
    batchGetAssetPropertyValue_nextToken,
    batchGetAssetPropertyValue_entries,
    batchGetAssetPropertyValueResponse_nextToken,
    batchGetAssetPropertyValueResponse_httpStatus,
    batchGetAssetPropertyValueResponse_errorEntries,
    batchGetAssetPropertyValueResponse_successEntries,
    batchGetAssetPropertyValueResponse_skippedEntries,

    -- ** BatchGetAssetPropertyValueHistory
    batchGetAssetPropertyValueHistory_maxResults,
    batchGetAssetPropertyValueHistory_nextToken,
    batchGetAssetPropertyValueHistory_entries,
    batchGetAssetPropertyValueHistoryResponse_nextToken,
    batchGetAssetPropertyValueHistoryResponse_httpStatus,
    batchGetAssetPropertyValueHistoryResponse_errorEntries,
    batchGetAssetPropertyValueHistoryResponse_successEntries,
    batchGetAssetPropertyValueHistoryResponse_skippedEntries,

    -- ** BatchPutAssetPropertyValue
    batchPutAssetPropertyValue_entries,
    batchPutAssetPropertyValueResponse_httpStatus,
    batchPutAssetPropertyValueResponse_errorEntries,

    -- ** CreateAccessPolicy
    createAccessPolicy_clientToken,
    createAccessPolicy_tags,
    createAccessPolicy_accessPolicyIdentity,
    createAccessPolicy_accessPolicyResource,
    createAccessPolicy_accessPolicyPermission,
    createAccessPolicyResponse_httpStatus,
    createAccessPolicyResponse_accessPolicyId,
    createAccessPolicyResponse_accessPolicyArn,

    -- ** CreateAsset
    createAsset_assetDescription,
    createAsset_clientToken,
    createAsset_tags,
    createAsset_assetName,
    createAsset_assetModelId,
    createAssetResponse_httpStatus,
    createAssetResponse_assetId,
    createAssetResponse_assetArn,
    createAssetResponse_assetStatus,

    -- ** CreateAssetModel
    createAssetModel_assetModelCompositeModels,
    createAssetModel_assetModelDescription,
    createAssetModel_assetModelHierarchies,
    createAssetModel_assetModelProperties,
    createAssetModel_clientToken,
    createAssetModel_tags,
    createAssetModel_assetModelName,
    createAssetModelResponse_httpStatus,
    createAssetModelResponse_assetModelId,
    createAssetModelResponse_assetModelArn,
    createAssetModelResponse_assetModelStatus,

    -- ** CreateBulkImportJob
    createBulkImportJob_jobName,
    createBulkImportJob_jobRoleArn,
    createBulkImportJob_files,
    createBulkImportJob_errorReportLocation,
    createBulkImportJob_jobConfiguration,
    createBulkImportJobResponse_httpStatus,
    createBulkImportJobResponse_jobId,
    createBulkImportJobResponse_jobName,
    createBulkImportJobResponse_jobStatus,

    -- ** CreateDashboard
    createDashboard_clientToken,
    createDashboard_dashboardDescription,
    createDashboard_tags,
    createDashboard_projectId,
    createDashboard_dashboardName,
    createDashboard_dashboardDefinition,
    createDashboardResponse_httpStatus,
    createDashboardResponse_dashboardId,
    createDashboardResponse_dashboardArn,

    -- ** CreateGateway
    createGateway_tags,
    createGateway_gatewayName,
    createGateway_gatewayPlatform,
    createGatewayResponse_httpStatus,
    createGatewayResponse_gatewayId,
    createGatewayResponse_gatewayArn,

    -- ** CreatePortal
    createPortal_alarms,
    createPortal_clientToken,
    createPortal_notificationSenderEmail,
    createPortal_portalAuthMode,
    createPortal_portalDescription,
    createPortal_portalLogoImageFile,
    createPortal_tags,
    createPortal_portalName,
    createPortal_portalContactEmail,
    createPortal_roleArn,
    createPortalResponse_httpStatus,
    createPortalResponse_portalId,
    createPortalResponse_portalArn,
    createPortalResponse_portalStartUrl,
    createPortalResponse_portalStatus,
    createPortalResponse_ssoApplicationId,

    -- ** CreateProject
    createProject_clientToken,
    createProject_projectDescription,
    createProject_tags,
    createProject_portalId,
    createProject_projectName,
    createProjectResponse_httpStatus,
    createProjectResponse_projectId,
    createProjectResponse_projectArn,

    -- ** DeleteAccessPolicy
    deleteAccessPolicy_clientToken,
    deleteAccessPolicy_accessPolicyId,
    deleteAccessPolicyResponse_httpStatus,

    -- ** DeleteAsset
    deleteAsset_clientToken,
    deleteAsset_assetId,
    deleteAssetResponse_httpStatus,
    deleteAssetResponse_assetStatus,

    -- ** DeleteAssetModel
    deleteAssetModel_clientToken,
    deleteAssetModel_assetModelId,
    deleteAssetModelResponse_httpStatus,
    deleteAssetModelResponse_assetModelStatus,

    -- ** DeleteDashboard
    deleteDashboard_clientToken,
    deleteDashboard_dashboardId,
    deleteDashboardResponse_httpStatus,

    -- ** DeleteGateway
    deleteGateway_gatewayId,

    -- ** DeletePortal
    deletePortal_clientToken,
    deletePortal_portalId,
    deletePortalResponse_httpStatus,
    deletePortalResponse_portalStatus,

    -- ** DeleteProject
    deleteProject_clientToken,
    deleteProject_projectId,
    deleteProjectResponse_httpStatus,

    -- ** DeleteTimeSeries
    deleteTimeSeries_alias,
    deleteTimeSeries_assetId,
    deleteTimeSeries_clientToken,
    deleteTimeSeries_propertyId,

    -- ** DescribeAccessPolicy
    describeAccessPolicy_accessPolicyId,
    describeAccessPolicyResponse_httpStatus,
    describeAccessPolicyResponse_accessPolicyId,
    describeAccessPolicyResponse_accessPolicyArn,
    describeAccessPolicyResponse_accessPolicyIdentity,
    describeAccessPolicyResponse_accessPolicyResource,
    describeAccessPolicyResponse_accessPolicyPermission,
    describeAccessPolicyResponse_accessPolicyCreationDate,
    describeAccessPolicyResponse_accessPolicyLastUpdateDate,

    -- ** DescribeAsset
    describeAsset_excludeProperties,
    describeAsset_assetId,
    describeAssetResponse_assetCompositeModels,
    describeAssetResponse_assetDescription,
    describeAssetResponse_httpStatus,
    describeAssetResponse_assetId,
    describeAssetResponse_assetArn,
    describeAssetResponse_assetName,
    describeAssetResponse_assetModelId,
    describeAssetResponse_assetProperties,
    describeAssetResponse_assetHierarchies,
    describeAssetResponse_assetCreationDate,
    describeAssetResponse_assetLastUpdateDate,
    describeAssetResponse_assetStatus,

    -- ** DescribeAssetModel
    describeAssetModel_excludeProperties,
    describeAssetModel_assetModelId,
    describeAssetModelResponse_assetModelCompositeModels,
    describeAssetModelResponse_httpStatus,
    describeAssetModelResponse_assetModelId,
    describeAssetModelResponse_assetModelArn,
    describeAssetModelResponse_assetModelName,
    describeAssetModelResponse_assetModelDescription,
    describeAssetModelResponse_assetModelProperties,
    describeAssetModelResponse_assetModelHierarchies,
    describeAssetModelResponse_assetModelCreationDate,
    describeAssetModelResponse_assetModelLastUpdateDate,
    describeAssetModelResponse_assetModelStatus,

    -- ** DescribeAssetProperty
    describeAssetProperty_assetId,
    describeAssetProperty_propertyId,
    describeAssetPropertyResponse_assetProperty,
    describeAssetPropertyResponse_compositeModel,
    describeAssetPropertyResponse_httpStatus,
    describeAssetPropertyResponse_assetId,
    describeAssetPropertyResponse_assetName,
    describeAssetPropertyResponse_assetModelId,

    -- ** DescribeBulkImportJob
    describeBulkImportJob_jobId,
    describeBulkImportJobResponse_httpStatus,
    describeBulkImportJobResponse_jobId,
    describeBulkImportJobResponse_jobName,
    describeBulkImportJobResponse_jobStatus,
    describeBulkImportJobResponse_jobRoleArn,
    describeBulkImportJobResponse_files,
    describeBulkImportJobResponse_errorReportLocation,
    describeBulkImportJobResponse_jobConfiguration,
    describeBulkImportJobResponse_jobCreationDate,
    describeBulkImportJobResponse_jobLastUpdateDate,

    -- ** DescribeDashboard
    describeDashboard_dashboardId,
    describeDashboardResponse_dashboardDescription,
    describeDashboardResponse_httpStatus,
    describeDashboardResponse_dashboardId,
    describeDashboardResponse_dashboardArn,
    describeDashboardResponse_dashboardName,
    describeDashboardResponse_projectId,
    describeDashboardResponse_dashboardDefinition,
    describeDashboardResponse_dashboardCreationDate,
    describeDashboardResponse_dashboardLastUpdateDate,

    -- ** DescribeDefaultEncryptionConfiguration
    describeDefaultEncryptionConfigurationResponse_kmsKeyArn,
    describeDefaultEncryptionConfigurationResponse_httpStatus,
    describeDefaultEncryptionConfigurationResponse_encryptionType,
    describeDefaultEncryptionConfigurationResponse_configurationStatus,

    -- ** DescribeGateway
    describeGateway_gatewayId,
    describeGatewayResponse_gatewayPlatform,
    describeGatewayResponse_httpStatus,
    describeGatewayResponse_gatewayId,
    describeGatewayResponse_gatewayName,
    describeGatewayResponse_gatewayArn,
    describeGatewayResponse_gatewayCapabilitySummaries,
    describeGatewayResponse_creationDate,
    describeGatewayResponse_lastUpdateDate,

    -- ** DescribeGatewayCapabilityConfiguration
    describeGatewayCapabilityConfiguration_gatewayId,
    describeGatewayCapabilityConfiguration_capabilityNamespace,
    describeGatewayCapabilityConfigurationResponse_httpStatus,
    describeGatewayCapabilityConfigurationResponse_gatewayId,
    describeGatewayCapabilityConfigurationResponse_capabilityNamespace,
    describeGatewayCapabilityConfigurationResponse_capabilityConfiguration,
    describeGatewayCapabilityConfigurationResponse_capabilitySyncStatus,

    -- ** DescribeLoggingOptions
    describeLoggingOptionsResponse_httpStatus,
    describeLoggingOptionsResponse_loggingOptions,

    -- ** DescribePortal
    describePortal_portalId,
    describePortalResponse_alarms,
    describePortalResponse_notificationSenderEmail,
    describePortalResponse_portalAuthMode,
    describePortalResponse_portalDescription,
    describePortalResponse_portalLogoImageLocation,
    describePortalResponse_roleArn,
    describePortalResponse_httpStatus,
    describePortalResponse_portalId,
    describePortalResponse_portalArn,
    describePortalResponse_portalName,
    describePortalResponse_portalClientId,
    describePortalResponse_portalStartUrl,
    describePortalResponse_portalContactEmail,
    describePortalResponse_portalStatus,
    describePortalResponse_portalCreationDate,
    describePortalResponse_portalLastUpdateDate,

    -- ** DescribeProject
    describeProject_projectId,
    describeProjectResponse_projectDescription,
    describeProjectResponse_httpStatus,
    describeProjectResponse_projectId,
    describeProjectResponse_projectArn,
    describeProjectResponse_projectName,
    describeProjectResponse_portalId,
    describeProjectResponse_projectCreationDate,
    describeProjectResponse_projectLastUpdateDate,

    -- ** DescribeStorageConfiguration
    describeStorageConfigurationResponse_disassociatedDataStorage,
    describeStorageConfigurationResponse_lastUpdateDate,
    describeStorageConfigurationResponse_multiLayerStorage,
    describeStorageConfigurationResponse_retentionPeriod,
    describeStorageConfigurationResponse_httpStatus,
    describeStorageConfigurationResponse_storageType,
    describeStorageConfigurationResponse_configurationStatus,

    -- ** DescribeTimeSeries
    describeTimeSeries_alias,
    describeTimeSeries_assetId,
    describeTimeSeries_propertyId,
    describeTimeSeriesResponse_alias,
    describeTimeSeriesResponse_assetId,
    describeTimeSeriesResponse_dataTypeSpec,
    describeTimeSeriesResponse_propertyId,
    describeTimeSeriesResponse_httpStatus,
    describeTimeSeriesResponse_timeSeriesId,
    describeTimeSeriesResponse_dataType,
    describeTimeSeriesResponse_timeSeriesCreationDate,
    describeTimeSeriesResponse_timeSeriesLastUpdateDate,

    -- ** DisassociateAssets
    disassociateAssets_clientToken,
    disassociateAssets_assetId,
    disassociateAssets_hierarchyId,
    disassociateAssets_childAssetId,

    -- ** DisassociateTimeSeriesFromAssetProperty
    disassociateTimeSeriesFromAssetProperty_clientToken,
    disassociateTimeSeriesFromAssetProperty_alias,
    disassociateTimeSeriesFromAssetProperty_assetId,
    disassociateTimeSeriesFromAssetProperty_propertyId,

    -- ** GetAssetPropertyAggregates
    getAssetPropertyAggregates_assetId,
    getAssetPropertyAggregates_maxResults,
    getAssetPropertyAggregates_nextToken,
    getAssetPropertyAggregates_propertyAlias,
    getAssetPropertyAggregates_propertyId,
    getAssetPropertyAggregates_qualities,
    getAssetPropertyAggregates_timeOrdering,
    getAssetPropertyAggregates_aggregateTypes,
    getAssetPropertyAggregates_resolution,
    getAssetPropertyAggregates_startDate,
    getAssetPropertyAggregates_endDate,
    getAssetPropertyAggregatesResponse_nextToken,
    getAssetPropertyAggregatesResponse_httpStatus,
    getAssetPropertyAggregatesResponse_aggregatedValues,

    -- ** GetAssetPropertyValue
    getAssetPropertyValue_assetId,
    getAssetPropertyValue_propertyAlias,
    getAssetPropertyValue_propertyId,
    getAssetPropertyValueResponse_propertyValue,
    getAssetPropertyValueResponse_httpStatus,

    -- ** GetAssetPropertyValueHistory
    getAssetPropertyValueHistory_assetId,
    getAssetPropertyValueHistory_endDate,
    getAssetPropertyValueHistory_maxResults,
    getAssetPropertyValueHistory_nextToken,
    getAssetPropertyValueHistory_propertyAlias,
    getAssetPropertyValueHistory_propertyId,
    getAssetPropertyValueHistory_qualities,
    getAssetPropertyValueHistory_startDate,
    getAssetPropertyValueHistory_timeOrdering,
    getAssetPropertyValueHistoryResponse_nextToken,
    getAssetPropertyValueHistoryResponse_httpStatus,
    getAssetPropertyValueHistoryResponse_assetPropertyValueHistory,

    -- ** GetInterpolatedAssetPropertyValues
    getInterpolatedAssetPropertyValues_assetId,
    getInterpolatedAssetPropertyValues_endTimeOffsetInNanos,
    getInterpolatedAssetPropertyValues_intervalWindowInSeconds,
    getInterpolatedAssetPropertyValues_maxResults,
    getInterpolatedAssetPropertyValues_nextToken,
    getInterpolatedAssetPropertyValues_propertyAlias,
    getInterpolatedAssetPropertyValues_propertyId,
    getInterpolatedAssetPropertyValues_startTimeOffsetInNanos,
    getInterpolatedAssetPropertyValues_startTimeInSeconds,
    getInterpolatedAssetPropertyValues_endTimeInSeconds,
    getInterpolatedAssetPropertyValues_quality,
    getInterpolatedAssetPropertyValues_intervalInSeconds,
    getInterpolatedAssetPropertyValues_type,
    getInterpolatedAssetPropertyValuesResponse_nextToken,
    getInterpolatedAssetPropertyValuesResponse_httpStatus,
    getInterpolatedAssetPropertyValuesResponse_interpolatedAssetPropertyValues,

    -- ** ListAccessPolicies
    listAccessPolicies_iamArn,
    listAccessPolicies_identityId,
    listAccessPolicies_identityType,
    listAccessPolicies_maxResults,
    listAccessPolicies_nextToken,
    listAccessPolicies_resourceId,
    listAccessPolicies_resourceType,
    listAccessPoliciesResponse_nextToken,
    listAccessPoliciesResponse_httpStatus,
    listAccessPoliciesResponse_accessPolicySummaries,

    -- ** ListAssetModelProperties
    listAssetModelProperties_filter,
    listAssetModelProperties_maxResults,
    listAssetModelProperties_nextToken,
    listAssetModelProperties_assetModelId,
    listAssetModelPropertiesResponse_nextToken,
    listAssetModelPropertiesResponse_httpStatus,
    listAssetModelPropertiesResponse_assetModelPropertySummaries,

    -- ** ListAssetModels
    listAssetModels_maxResults,
    listAssetModels_nextToken,
    listAssetModelsResponse_nextToken,
    listAssetModelsResponse_httpStatus,
    listAssetModelsResponse_assetModelSummaries,

    -- ** ListAssetProperties
    listAssetProperties_filter,
    listAssetProperties_maxResults,
    listAssetProperties_nextToken,
    listAssetProperties_assetId,
    listAssetPropertiesResponse_nextToken,
    listAssetPropertiesResponse_httpStatus,
    listAssetPropertiesResponse_assetPropertySummaries,

    -- ** ListAssetRelationships
    listAssetRelationships_maxResults,
    listAssetRelationships_nextToken,
    listAssetRelationships_assetId,
    listAssetRelationships_traversalType,
    listAssetRelationshipsResponse_nextToken,
    listAssetRelationshipsResponse_httpStatus,
    listAssetRelationshipsResponse_assetRelationshipSummaries,

    -- ** ListAssets
    listAssets_assetModelId,
    listAssets_filter,
    listAssets_maxResults,
    listAssets_nextToken,
    listAssetsResponse_nextToken,
    listAssetsResponse_httpStatus,
    listAssetsResponse_assetSummaries,

    -- ** ListAssociatedAssets
    listAssociatedAssets_hierarchyId,
    listAssociatedAssets_maxResults,
    listAssociatedAssets_nextToken,
    listAssociatedAssets_traversalDirection,
    listAssociatedAssets_assetId,
    listAssociatedAssetsResponse_nextToken,
    listAssociatedAssetsResponse_httpStatus,
    listAssociatedAssetsResponse_assetSummaries,

    -- ** ListBulkImportJobs
    listBulkImportJobs_filter,
    listBulkImportJobs_maxResults,
    listBulkImportJobs_nextToken,
    listBulkImportJobsResponse_nextToken,
    listBulkImportJobsResponse_httpStatus,
    listBulkImportJobsResponse_jobSummaries,

    -- ** ListDashboards
    listDashboards_maxResults,
    listDashboards_nextToken,
    listDashboards_projectId,
    listDashboardsResponse_nextToken,
    listDashboardsResponse_httpStatus,
    listDashboardsResponse_dashboardSummaries,

    -- ** ListGateways
    listGateways_maxResults,
    listGateways_nextToken,
    listGatewaysResponse_nextToken,
    listGatewaysResponse_httpStatus,
    listGatewaysResponse_gatewaySummaries,

    -- ** ListPortals
    listPortals_maxResults,
    listPortals_nextToken,
    listPortalsResponse_nextToken,
    listPortalsResponse_portalSummaries,
    listPortalsResponse_httpStatus,

    -- ** ListProjectAssets
    listProjectAssets_maxResults,
    listProjectAssets_nextToken,
    listProjectAssets_projectId,
    listProjectAssetsResponse_nextToken,
    listProjectAssetsResponse_httpStatus,
    listProjectAssetsResponse_assetIds,

    -- ** ListProjects
    listProjects_maxResults,
    listProjects_nextToken,
    listProjects_portalId,
    listProjectsResponse_nextToken,
    listProjectsResponse_httpStatus,
    listProjectsResponse_projectSummaries,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** ListTimeSeries
    listTimeSeries_aliasPrefix,
    listTimeSeries_assetId,
    listTimeSeries_maxResults,
    listTimeSeries_nextToken,
    listTimeSeries_timeSeriesType,
    listTimeSeriesResponse_nextToken,
    listTimeSeriesResponse_httpStatus,
    listTimeSeriesResponse_timeSeriesSummaries,

    -- ** PutDefaultEncryptionConfiguration
    putDefaultEncryptionConfiguration_kmsKeyId,
    putDefaultEncryptionConfiguration_encryptionType,
    putDefaultEncryptionConfigurationResponse_kmsKeyArn,
    putDefaultEncryptionConfigurationResponse_httpStatus,
    putDefaultEncryptionConfigurationResponse_encryptionType,
    putDefaultEncryptionConfigurationResponse_configurationStatus,

    -- ** PutLoggingOptions
    putLoggingOptions_loggingOptions,
    putLoggingOptionsResponse_httpStatus,

    -- ** PutStorageConfiguration
    putStorageConfiguration_disassociatedDataStorage,
    putStorageConfiguration_multiLayerStorage,
    putStorageConfiguration_retentionPeriod,
    putStorageConfiguration_storageType,
    putStorageConfigurationResponse_disassociatedDataStorage,
    putStorageConfigurationResponse_multiLayerStorage,
    putStorageConfigurationResponse_retentionPeriod,
    putStorageConfigurationResponse_httpStatus,
    putStorageConfigurationResponse_storageType,
    putStorageConfigurationResponse_configurationStatus,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** UpdateAccessPolicy
    updateAccessPolicy_clientToken,
    updateAccessPolicy_accessPolicyId,
    updateAccessPolicy_accessPolicyIdentity,
    updateAccessPolicy_accessPolicyResource,
    updateAccessPolicy_accessPolicyPermission,
    updateAccessPolicyResponse_httpStatus,

    -- ** UpdateAsset
    updateAsset_assetDescription,
    updateAsset_clientToken,
    updateAsset_assetId,
    updateAsset_assetName,
    updateAssetResponse_httpStatus,
    updateAssetResponse_assetStatus,

    -- ** UpdateAssetModel
    updateAssetModel_assetModelCompositeModels,
    updateAssetModel_assetModelDescription,
    updateAssetModel_assetModelHierarchies,
    updateAssetModel_assetModelProperties,
    updateAssetModel_clientToken,
    updateAssetModel_assetModelId,
    updateAssetModel_assetModelName,
    updateAssetModelResponse_httpStatus,
    updateAssetModelResponse_assetModelStatus,

    -- ** UpdateAssetProperty
    updateAssetProperty_clientToken,
    updateAssetProperty_propertyAlias,
    updateAssetProperty_propertyNotificationState,
    updateAssetProperty_propertyUnit,
    updateAssetProperty_assetId,
    updateAssetProperty_propertyId,

    -- ** UpdateDashboard
    updateDashboard_clientToken,
    updateDashboard_dashboardDescription,
    updateDashboard_dashboardId,
    updateDashboard_dashboardName,
    updateDashboard_dashboardDefinition,
    updateDashboardResponse_httpStatus,

    -- ** UpdateGateway
    updateGateway_gatewayId,
    updateGateway_gatewayName,

    -- ** UpdateGatewayCapabilityConfiguration
    updateGatewayCapabilityConfiguration_gatewayId,
    updateGatewayCapabilityConfiguration_capabilityNamespace,
    updateGatewayCapabilityConfiguration_capabilityConfiguration,
    updateGatewayCapabilityConfigurationResponse_httpStatus,
    updateGatewayCapabilityConfigurationResponse_capabilityNamespace,
    updateGatewayCapabilityConfigurationResponse_capabilitySyncStatus,

    -- ** UpdatePortal
    updatePortal_alarms,
    updatePortal_clientToken,
    updatePortal_notificationSenderEmail,
    updatePortal_portalDescription,
    updatePortal_portalLogoImage,
    updatePortal_portalId,
    updatePortal_portalName,
    updatePortal_portalContactEmail,
    updatePortal_roleArn,
    updatePortalResponse_httpStatus,
    updatePortalResponse_portalStatus,

    -- ** UpdateProject
    updateProject_clientToken,
    updateProject_projectDescription,
    updateProject_projectId,
    updateProject_projectName,
    updateProjectResponse_httpStatus,

    -- * Types

    -- ** AccessPolicySummary
    accessPolicySummary_creationDate,
    accessPolicySummary_lastUpdateDate,
    accessPolicySummary_id,
    accessPolicySummary_identity,
    accessPolicySummary_resource,
    accessPolicySummary_permission,

    -- ** AggregatedValue
    aggregatedValue_quality,
    aggregatedValue_timestamp,
    aggregatedValue_value,

    -- ** Aggregates
    aggregates_average,
    aggregates_count,
    aggregates_maximum,
    aggregates_minimum,
    aggregates_standardDeviation,
    aggregates_sum,

    -- ** Alarms
    alarms_notificationLambdaArn,
    alarms_alarmRoleArn,

    -- ** AssetCompositeModel
    assetCompositeModel_description,
    assetCompositeModel_id,
    assetCompositeModel_name,
    assetCompositeModel_type,
    assetCompositeModel_properties,

    -- ** AssetErrorDetails
    assetErrorDetails_assetId,
    assetErrorDetails_code,
    assetErrorDetails_message,

    -- ** AssetHierarchy
    assetHierarchy_id,
    assetHierarchy_name,

    -- ** AssetHierarchyInfo
    assetHierarchyInfo_childAssetId,
    assetHierarchyInfo_parentAssetId,

    -- ** AssetModelCompositeModel
    assetModelCompositeModel_description,
    assetModelCompositeModel_id,
    assetModelCompositeModel_properties,
    assetModelCompositeModel_name,
    assetModelCompositeModel_type,

    -- ** AssetModelCompositeModelDefinition
    assetModelCompositeModelDefinition_description,
    assetModelCompositeModelDefinition_properties,
    assetModelCompositeModelDefinition_name,
    assetModelCompositeModelDefinition_type,

    -- ** AssetModelHierarchy
    assetModelHierarchy_id,
    assetModelHierarchy_name,
    assetModelHierarchy_childAssetModelId,

    -- ** AssetModelHierarchyDefinition
    assetModelHierarchyDefinition_name,
    assetModelHierarchyDefinition_childAssetModelId,

    -- ** AssetModelProperty
    assetModelProperty_dataTypeSpec,
    assetModelProperty_id,
    assetModelProperty_unit,
    assetModelProperty_name,
    assetModelProperty_dataType,
    assetModelProperty_type,

    -- ** AssetModelPropertyDefinition
    assetModelPropertyDefinition_dataTypeSpec,
    assetModelPropertyDefinition_unit,
    assetModelPropertyDefinition_name,
    assetModelPropertyDefinition_dataType,
    assetModelPropertyDefinition_type,

    -- ** AssetModelPropertySummary
    assetModelPropertySummary_assetModelCompositeModelId,
    assetModelPropertySummary_dataTypeSpec,
    assetModelPropertySummary_id,
    assetModelPropertySummary_unit,
    assetModelPropertySummary_name,
    assetModelPropertySummary_dataType,
    assetModelPropertySummary_type,

    -- ** AssetModelStatus
    assetModelStatus_error,
    assetModelStatus_state,

    -- ** AssetModelSummary
    assetModelSummary_id,
    assetModelSummary_arn,
    assetModelSummary_name,
    assetModelSummary_description,
    assetModelSummary_creationDate,
    assetModelSummary_lastUpdateDate,
    assetModelSummary_status,

    -- ** AssetProperty
    assetProperty_alias,
    assetProperty_dataTypeSpec,
    assetProperty_notification,
    assetProperty_unit,
    assetProperty_id,
    assetProperty_name,
    assetProperty_dataType,

    -- ** AssetPropertySummary
    assetPropertySummary_alias,
    assetPropertySummary_assetCompositeModelId,
    assetPropertySummary_id,
    assetPropertySummary_notification,
    assetPropertySummary_unit,

    -- ** AssetPropertyValue
    assetPropertyValue_quality,
    assetPropertyValue_value,
    assetPropertyValue_timestamp,

    -- ** AssetRelationshipSummary
    assetRelationshipSummary_hierarchyInfo,
    assetRelationshipSummary_relationshipType,

    -- ** AssetStatus
    assetStatus_error,
    assetStatus_state,

    -- ** AssetSummary
    assetSummary_description,
    assetSummary_id,
    assetSummary_arn,
    assetSummary_name,
    assetSummary_assetModelId,
    assetSummary_creationDate,
    assetSummary_lastUpdateDate,
    assetSummary_status,
    assetSummary_hierarchies,

    -- ** AssociatedAssetsSummary
    associatedAssetsSummary_description,
    associatedAssetsSummary_id,
    associatedAssetsSummary_arn,
    associatedAssetsSummary_name,
    associatedAssetsSummary_assetModelId,
    associatedAssetsSummary_creationDate,
    associatedAssetsSummary_lastUpdateDate,
    associatedAssetsSummary_status,
    associatedAssetsSummary_hierarchies,

    -- ** Attribute
    attribute_defaultValue,

    -- ** BatchGetAssetPropertyAggregatesEntry
    batchGetAssetPropertyAggregatesEntry_assetId,
    batchGetAssetPropertyAggregatesEntry_propertyAlias,
    batchGetAssetPropertyAggregatesEntry_propertyId,
    batchGetAssetPropertyAggregatesEntry_qualities,
    batchGetAssetPropertyAggregatesEntry_timeOrdering,
    batchGetAssetPropertyAggregatesEntry_entryId,
    batchGetAssetPropertyAggregatesEntry_aggregateTypes,
    batchGetAssetPropertyAggregatesEntry_resolution,
    batchGetAssetPropertyAggregatesEntry_startDate,
    batchGetAssetPropertyAggregatesEntry_endDate,

    -- ** BatchGetAssetPropertyAggregatesErrorEntry
    batchGetAssetPropertyAggregatesErrorEntry_errorCode,
    batchGetAssetPropertyAggregatesErrorEntry_errorMessage,
    batchGetAssetPropertyAggregatesErrorEntry_entryId,

    -- ** BatchGetAssetPropertyAggregatesErrorInfo
    batchGetAssetPropertyAggregatesErrorInfo_errorCode,
    batchGetAssetPropertyAggregatesErrorInfo_errorTimestamp,

    -- ** BatchGetAssetPropertyAggregatesSkippedEntry
    batchGetAssetPropertyAggregatesSkippedEntry_errorInfo,
    batchGetAssetPropertyAggregatesSkippedEntry_entryId,
    batchGetAssetPropertyAggregatesSkippedEntry_completionStatus,

    -- ** BatchGetAssetPropertyAggregatesSuccessEntry
    batchGetAssetPropertyAggregatesSuccessEntry_entryId,
    batchGetAssetPropertyAggregatesSuccessEntry_aggregatedValues,

    -- ** BatchGetAssetPropertyValueEntry
    batchGetAssetPropertyValueEntry_assetId,
    batchGetAssetPropertyValueEntry_propertyAlias,
    batchGetAssetPropertyValueEntry_propertyId,
    batchGetAssetPropertyValueEntry_entryId,

    -- ** BatchGetAssetPropertyValueErrorEntry
    batchGetAssetPropertyValueErrorEntry_errorCode,
    batchGetAssetPropertyValueErrorEntry_errorMessage,
    batchGetAssetPropertyValueErrorEntry_entryId,

    -- ** BatchGetAssetPropertyValueErrorInfo
    batchGetAssetPropertyValueErrorInfo_errorCode,
    batchGetAssetPropertyValueErrorInfo_errorTimestamp,

    -- ** BatchGetAssetPropertyValueHistoryEntry
    batchGetAssetPropertyValueHistoryEntry_assetId,
    batchGetAssetPropertyValueHistoryEntry_endDate,
    batchGetAssetPropertyValueHistoryEntry_propertyAlias,
    batchGetAssetPropertyValueHistoryEntry_propertyId,
    batchGetAssetPropertyValueHistoryEntry_qualities,
    batchGetAssetPropertyValueHistoryEntry_startDate,
    batchGetAssetPropertyValueHistoryEntry_timeOrdering,
    batchGetAssetPropertyValueHistoryEntry_entryId,

    -- ** BatchGetAssetPropertyValueHistoryErrorEntry
    batchGetAssetPropertyValueHistoryErrorEntry_errorCode,
    batchGetAssetPropertyValueHistoryErrorEntry_errorMessage,
    batchGetAssetPropertyValueHistoryErrorEntry_entryId,

    -- ** BatchGetAssetPropertyValueHistoryErrorInfo
    batchGetAssetPropertyValueHistoryErrorInfo_errorCode,
    batchGetAssetPropertyValueHistoryErrorInfo_errorTimestamp,

    -- ** BatchGetAssetPropertyValueHistorySkippedEntry
    batchGetAssetPropertyValueHistorySkippedEntry_errorInfo,
    batchGetAssetPropertyValueHistorySkippedEntry_entryId,
    batchGetAssetPropertyValueHistorySkippedEntry_completionStatus,

    -- ** BatchGetAssetPropertyValueHistorySuccessEntry
    batchGetAssetPropertyValueHistorySuccessEntry_entryId,
    batchGetAssetPropertyValueHistorySuccessEntry_assetPropertyValueHistory,

    -- ** BatchGetAssetPropertyValueSkippedEntry
    batchGetAssetPropertyValueSkippedEntry_errorInfo,
    batchGetAssetPropertyValueSkippedEntry_entryId,
    batchGetAssetPropertyValueSkippedEntry_completionStatus,

    -- ** BatchGetAssetPropertyValueSuccessEntry
    batchGetAssetPropertyValueSuccessEntry_assetPropertyValue,
    batchGetAssetPropertyValueSuccessEntry_entryId,

    -- ** BatchPutAssetPropertyError
    batchPutAssetPropertyError_errorCode,
    batchPutAssetPropertyError_errorMessage,
    batchPutAssetPropertyError_timestamps,

    -- ** BatchPutAssetPropertyErrorEntry
    batchPutAssetPropertyErrorEntry_entryId,
    batchPutAssetPropertyErrorEntry_errors,

    -- ** CompositeModelProperty
    compositeModelProperty_id,
    compositeModelProperty_name,
    compositeModelProperty_type,
    compositeModelProperty_assetProperty,

    -- ** ConfigurationErrorDetails
    configurationErrorDetails_code,
    configurationErrorDetails_message,

    -- ** ConfigurationStatus
    configurationStatus_error,
    configurationStatus_state,

    -- ** Csv
    csv_columnNames,

    -- ** CustomerManagedS3Storage
    customerManagedS3Storage_s3ResourceArn,
    customerManagedS3Storage_roleArn,

    -- ** DashboardSummary
    dashboardSummary_creationDate,
    dashboardSummary_description,
    dashboardSummary_lastUpdateDate,
    dashboardSummary_id,
    dashboardSummary_name,

    -- ** DetailedError
    detailedError_code,
    detailedError_message,

    -- ** ErrorDetails
    errorDetails_details,
    errorDetails_code,
    errorDetails_message,

    -- ** ErrorReportLocation
    errorReportLocation_bucket,
    errorReportLocation_prefix,

    -- ** ExpressionVariable
    expressionVariable_name,
    expressionVariable_value,

    -- ** File
    file_versionId,
    file_bucket,
    file_key,

    -- ** FileFormat
    fileFormat_csv,

    -- ** ForwardingConfig
    forwardingConfig_state,

    -- ** GatewayCapabilitySummary
    gatewayCapabilitySummary_capabilityNamespace,
    gatewayCapabilitySummary_capabilitySyncStatus,

    -- ** GatewayPlatform
    gatewayPlatform_greengrass,
    gatewayPlatform_greengrassV2,

    -- ** GatewaySummary
    gatewaySummary_gatewayCapabilitySummaries,
    gatewaySummary_gatewayPlatform,
    gatewaySummary_gatewayId,
    gatewaySummary_gatewayName,
    gatewaySummary_creationDate,
    gatewaySummary_lastUpdateDate,

    -- ** Greengrass
    greengrass_groupArn,

    -- ** GreengrassV2
    greengrassV2_coreDeviceThingName,

    -- ** GroupIdentity
    groupIdentity_id,

    -- ** IAMRoleIdentity
    iAMRoleIdentity_arn,

    -- ** IAMUserIdentity
    iAMUserIdentity_arn,

    -- ** Identity
    identity_group,
    identity_iamRole,
    identity_iamUser,
    identity_user,

    -- ** Image
    image_file,
    image_id,

    -- ** ImageFile
    imageFile_data,
    imageFile_type,

    -- ** ImageLocation
    imageLocation_id,
    imageLocation_url,

    -- ** InterpolatedAssetPropertyValue
    interpolatedAssetPropertyValue_timestamp,
    interpolatedAssetPropertyValue_value,

    -- ** JobConfiguration
    jobConfiguration_fileFormat,

    -- ** JobSummary
    jobSummary_id,
    jobSummary_name,
    jobSummary_status,

    -- ** LoggingOptions
    loggingOptions_level,

    -- ** Measurement
    measurement_processingConfig,

    -- ** MeasurementProcessingConfig
    measurementProcessingConfig_forwardingConfig,

    -- ** Metric
    metric_processingConfig,
    metric_expression,
    metric_variables,
    metric_window,

    -- ** MetricProcessingConfig
    metricProcessingConfig_computeLocation,

    -- ** MetricWindow
    metricWindow_tumbling,

    -- ** MonitorErrorDetails
    monitorErrorDetails_code,
    monitorErrorDetails_message,

    -- ** MultiLayerStorage
    multiLayerStorage_customerManagedS3Storage,

    -- ** PortalResource
    portalResource_id,

    -- ** PortalStatus
    portalStatus_error,
    portalStatus_state,

    -- ** PortalSummary
    portalSummary_creationDate,
    portalSummary_description,
    portalSummary_lastUpdateDate,
    portalSummary_roleArn,
    portalSummary_id,
    portalSummary_name,
    portalSummary_startUrl,
    portalSummary_status,

    -- ** ProjectResource
    projectResource_id,

    -- ** ProjectSummary
    projectSummary_creationDate,
    projectSummary_description,
    projectSummary_lastUpdateDate,
    projectSummary_id,
    projectSummary_name,

    -- ** Property
    property_alias,
    property_notification,
    property_type,
    property_unit,
    property_id,
    property_name,
    property_dataType,

    -- ** PropertyNotification
    propertyNotification_topic,
    propertyNotification_state,

    -- ** PropertyType
    propertyType_attribute,
    propertyType_measurement,
    propertyType_metric,
    propertyType_transform,

    -- ** PutAssetPropertyValueEntry
    putAssetPropertyValueEntry_assetId,
    putAssetPropertyValueEntry_propertyAlias,
    putAssetPropertyValueEntry_propertyId,
    putAssetPropertyValueEntry_entryId,
    putAssetPropertyValueEntry_propertyValues,

    -- ** Resource
    resource_portal,
    resource_project,

    -- ** RetentionPeriod
    retentionPeriod_numberOfDays,
    retentionPeriod_unlimited,

    -- ** TimeInNanos
    timeInNanos_offsetInNanos,
    timeInNanos_timeInSeconds,

    -- ** TimeSeriesSummary
    timeSeriesSummary_alias,
    timeSeriesSummary_assetId,
    timeSeriesSummary_dataTypeSpec,
    timeSeriesSummary_propertyId,
    timeSeriesSummary_timeSeriesId,
    timeSeriesSummary_dataType,
    timeSeriesSummary_timeSeriesCreationDate,
    timeSeriesSummary_timeSeriesLastUpdateDate,

    -- ** Transform
    transform_processingConfig,
    transform_expression,
    transform_variables,

    -- ** TransformProcessingConfig
    transformProcessingConfig_forwardingConfig,
    transformProcessingConfig_computeLocation,

    -- ** TumblingWindow
    tumblingWindow_offset,
    tumblingWindow_interval,

    -- ** UserIdentity
    userIdentity_id,

    -- ** VariableValue
    variableValue_hierarchyId,
    variableValue_propertyId,

    -- ** Variant
    variant_booleanValue,
    variant_doubleValue,
    variant_integerValue,
    variant_stringValue,
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
import Amazonka.IoTSiteWise.Types.AccessPolicySummary
import Amazonka.IoTSiteWise.Types.AggregatedValue
import Amazonka.IoTSiteWise.Types.Aggregates
import Amazonka.IoTSiteWise.Types.Alarms
import Amazonka.IoTSiteWise.Types.AssetCompositeModel
import Amazonka.IoTSiteWise.Types.AssetErrorDetails
import Amazonka.IoTSiteWise.Types.AssetHierarchy
import Amazonka.IoTSiteWise.Types.AssetHierarchyInfo
import Amazonka.IoTSiteWise.Types.AssetModelCompositeModel
import Amazonka.IoTSiteWise.Types.AssetModelCompositeModelDefinition
import Amazonka.IoTSiteWise.Types.AssetModelHierarchy
import Amazonka.IoTSiteWise.Types.AssetModelHierarchyDefinition
import Amazonka.IoTSiteWise.Types.AssetModelProperty
import Amazonka.IoTSiteWise.Types.AssetModelPropertyDefinition
import Amazonka.IoTSiteWise.Types.AssetModelPropertySummary
import Amazonka.IoTSiteWise.Types.AssetModelStatus
import Amazonka.IoTSiteWise.Types.AssetModelSummary
import Amazonka.IoTSiteWise.Types.AssetProperty
import Amazonka.IoTSiteWise.Types.AssetPropertySummary
import Amazonka.IoTSiteWise.Types.AssetPropertyValue
import Amazonka.IoTSiteWise.Types.AssetRelationshipSummary
import Amazonka.IoTSiteWise.Types.AssetStatus
import Amazonka.IoTSiteWise.Types.AssetSummary
import Amazonka.IoTSiteWise.Types.AssociatedAssetsSummary
import Amazonka.IoTSiteWise.Types.Attribute
import Amazonka.IoTSiteWise.Types.BatchGetAssetPropertyAggregatesEntry
import Amazonka.IoTSiteWise.Types.BatchGetAssetPropertyAggregatesErrorEntry
import Amazonka.IoTSiteWise.Types.BatchGetAssetPropertyAggregatesErrorInfo
import Amazonka.IoTSiteWise.Types.BatchGetAssetPropertyAggregatesSkippedEntry
import Amazonka.IoTSiteWise.Types.BatchGetAssetPropertyAggregatesSuccessEntry
import Amazonka.IoTSiteWise.Types.BatchGetAssetPropertyValueEntry
import Amazonka.IoTSiteWise.Types.BatchGetAssetPropertyValueErrorEntry
import Amazonka.IoTSiteWise.Types.BatchGetAssetPropertyValueErrorInfo
import Amazonka.IoTSiteWise.Types.BatchGetAssetPropertyValueHistoryEntry
import Amazonka.IoTSiteWise.Types.BatchGetAssetPropertyValueHistoryErrorEntry
import Amazonka.IoTSiteWise.Types.BatchGetAssetPropertyValueHistoryErrorInfo
import Amazonka.IoTSiteWise.Types.BatchGetAssetPropertyValueHistorySkippedEntry
import Amazonka.IoTSiteWise.Types.BatchGetAssetPropertyValueHistorySuccessEntry
import Amazonka.IoTSiteWise.Types.BatchGetAssetPropertyValueSkippedEntry
import Amazonka.IoTSiteWise.Types.BatchGetAssetPropertyValueSuccessEntry
import Amazonka.IoTSiteWise.Types.BatchPutAssetPropertyError
import Amazonka.IoTSiteWise.Types.BatchPutAssetPropertyErrorEntry
import Amazonka.IoTSiteWise.Types.CompositeModelProperty
import Amazonka.IoTSiteWise.Types.ConfigurationErrorDetails
import Amazonka.IoTSiteWise.Types.ConfigurationStatus
import Amazonka.IoTSiteWise.Types.Csv
import Amazonka.IoTSiteWise.Types.CustomerManagedS3Storage
import Amazonka.IoTSiteWise.Types.DashboardSummary
import Amazonka.IoTSiteWise.Types.DetailedError
import Amazonka.IoTSiteWise.Types.ErrorDetails
import Amazonka.IoTSiteWise.Types.ErrorReportLocation
import Amazonka.IoTSiteWise.Types.ExpressionVariable
import Amazonka.IoTSiteWise.Types.File
import Amazonka.IoTSiteWise.Types.FileFormat
import Amazonka.IoTSiteWise.Types.ForwardingConfig
import Amazonka.IoTSiteWise.Types.GatewayCapabilitySummary
import Amazonka.IoTSiteWise.Types.GatewayPlatform
import Amazonka.IoTSiteWise.Types.GatewaySummary
import Amazonka.IoTSiteWise.Types.Greengrass
import Amazonka.IoTSiteWise.Types.GreengrassV2
import Amazonka.IoTSiteWise.Types.GroupIdentity
import Amazonka.IoTSiteWise.Types.IAMRoleIdentity
import Amazonka.IoTSiteWise.Types.IAMUserIdentity
import Amazonka.IoTSiteWise.Types.Identity
import Amazonka.IoTSiteWise.Types.Image
import Amazonka.IoTSiteWise.Types.ImageFile
import Amazonka.IoTSiteWise.Types.ImageLocation
import Amazonka.IoTSiteWise.Types.InterpolatedAssetPropertyValue
import Amazonka.IoTSiteWise.Types.JobConfiguration
import Amazonka.IoTSiteWise.Types.JobSummary
import Amazonka.IoTSiteWise.Types.LoggingOptions
import Amazonka.IoTSiteWise.Types.Measurement
import Amazonka.IoTSiteWise.Types.MeasurementProcessingConfig
import Amazonka.IoTSiteWise.Types.Metric
import Amazonka.IoTSiteWise.Types.MetricProcessingConfig
import Amazonka.IoTSiteWise.Types.MetricWindow
import Amazonka.IoTSiteWise.Types.MonitorErrorDetails
import Amazonka.IoTSiteWise.Types.MultiLayerStorage
import Amazonka.IoTSiteWise.Types.PortalResource
import Amazonka.IoTSiteWise.Types.PortalStatus
import Amazonka.IoTSiteWise.Types.PortalSummary
import Amazonka.IoTSiteWise.Types.ProjectResource
import Amazonka.IoTSiteWise.Types.ProjectSummary
import Amazonka.IoTSiteWise.Types.Property
import Amazonka.IoTSiteWise.Types.PropertyNotification
import Amazonka.IoTSiteWise.Types.PropertyType
import Amazonka.IoTSiteWise.Types.PutAssetPropertyValueEntry
import Amazonka.IoTSiteWise.Types.Resource
import Amazonka.IoTSiteWise.Types.RetentionPeriod
import Amazonka.IoTSiteWise.Types.TimeInNanos
import Amazonka.IoTSiteWise.Types.TimeSeriesSummary
import Amazonka.IoTSiteWise.Types.Transform
import Amazonka.IoTSiteWise.Types.TransformProcessingConfig
import Amazonka.IoTSiteWise.Types.TumblingWindow
import Amazonka.IoTSiteWise.Types.UserIdentity
import Amazonka.IoTSiteWise.Types.VariableValue
import Amazonka.IoTSiteWise.Types.Variant
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
