{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.IoTSiteWise.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTSiteWise.Lens
  ( -- * Operations

    -- ** ListProjects
    listProjects_nextToken,
    listProjects_maxResults,
    listProjects_portalId,
    listProjectsResponse_nextToken,
    listProjectsResponse_httpStatus,
    listProjectsResponse_projectSummaries,

    -- ** DeleteProject
    deleteProject_clientToken,
    deleteProject_projectId,
    deleteProjectResponse_httpStatus,

    -- ** UpdateProject
    updateProject_clientToken,
    updateProject_projectDescription,
    updateProject_projectId,
    updateProject_projectName,
    updateProjectResponse_httpStatus,

    -- ** PutLoggingOptions
    putLoggingOptions_loggingOptions,
    putLoggingOptionsResponse_httpStatus,

    -- ** DescribeAssetModel
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

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** GetAssetPropertyValue
    getAssetPropertyValue_propertyAlias,
    getAssetPropertyValue_propertyId,
    getAssetPropertyValue_assetId,
    getAssetPropertyValueResponse_propertyValue,
    getAssetPropertyValueResponse_httpStatus,

    -- ** DeleteAccessPolicy
    deleteAccessPolicy_clientToken,
    deleteAccessPolicy_accessPolicyId,
    deleteAccessPolicyResponse_httpStatus,

    -- ** UpdateAccessPolicy
    updateAccessPolicy_clientToken,
    updateAccessPolicy_accessPolicyId,
    updateAccessPolicy_accessPolicyIdentity,
    updateAccessPolicy_accessPolicyResource,
    updateAccessPolicy_accessPolicyPermission,
    updateAccessPolicyResponse_httpStatus,

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

    -- ** DescribeAsset
    describeAsset_assetId,
    describeAssetResponse_assetCompositeModels,
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

    -- ** ListDashboards
    listDashboards_nextToken,
    listDashboards_maxResults,
    listDashboards_projectId,
    listDashboardsResponse_nextToken,
    listDashboardsResponse_httpStatus,
    listDashboardsResponse_dashboardSummaries,

    -- ** ListAccessPolicies
    listAccessPolicies_resourceId,
    listAccessPolicies_resourceType,
    listAccessPolicies_identityType,
    listAccessPolicies_nextToken,
    listAccessPolicies_iamArn,
    listAccessPolicies_identityId,
    listAccessPolicies_maxResults,
    listAccessPoliciesResponse_nextToken,
    listAccessPoliciesResponse_httpStatus,
    listAccessPoliciesResponse_accessPolicySummaries,

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

    -- ** GetAssetPropertyValueHistory
    getAssetPropertyValueHistory_endDate,
    getAssetPropertyValueHistory_qualities,
    getAssetPropertyValueHistory_timeOrdering,
    getAssetPropertyValueHistory_propertyAlias,
    getAssetPropertyValueHistory_startDate,
    getAssetPropertyValueHistory_nextToken,
    getAssetPropertyValueHistory_propertyId,
    getAssetPropertyValueHistory_assetId,
    getAssetPropertyValueHistory_maxResults,
    getAssetPropertyValueHistoryResponse_nextToken,
    getAssetPropertyValueHistoryResponse_httpStatus,
    getAssetPropertyValueHistoryResponse_assetPropertyValueHistory,

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

    -- ** CreateAccessPolicy
    createAccessPolicy_clientToken,
    createAccessPolicy_tags,
    createAccessPolicy_accessPolicyIdentity,
    createAccessPolicy_accessPolicyResource,
    createAccessPolicy_accessPolicyPermission,
    createAccessPolicyResponse_httpStatus,
    createAccessPolicyResponse_accessPolicyId,
    createAccessPolicyResponse_accessPolicyArn,

    -- ** CreateAssetModel
    createAssetModel_clientToken,
    createAssetModel_assetModelDescription,
    createAssetModel_assetModelProperties,
    createAssetModel_assetModelCompositeModels,
    createAssetModel_assetModelHierarchies,
    createAssetModel_tags,
    createAssetModel_assetModelName,
    createAssetModelResponse_httpStatus,
    createAssetModelResponse_assetModelId,
    createAssetModelResponse_assetModelArn,
    createAssetModelResponse_assetModelStatus,

    -- ** BatchAssociateProjectAssets
    batchAssociateProjectAssets_clientToken,
    batchAssociateProjectAssets_projectId,
    batchAssociateProjectAssets_assetIds,
    batchAssociateProjectAssetsResponse_errors,
    batchAssociateProjectAssetsResponse_httpStatus,

    -- ** ListAssetModels
    listAssetModels_nextToken,
    listAssetModels_maxResults,
    listAssetModelsResponse_nextToken,
    listAssetModelsResponse_httpStatus,
    listAssetModelsResponse_assetModelSummaries,

    -- ** ListAssociatedAssets
    listAssociatedAssets_hierarchyId,
    listAssociatedAssets_traversalDirection,
    listAssociatedAssets_nextToken,
    listAssociatedAssets_maxResults,
    listAssociatedAssets_assetId,
    listAssociatedAssetsResponse_nextToken,
    listAssociatedAssetsResponse_httpStatus,
    listAssociatedAssetsResponse_assetSummaries,

    -- ** BatchPutAssetPropertyValue
    batchPutAssetPropertyValue_entries,
    batchPutAssetPropertyValueResponse_httpStatus,
    batchPutAssetPropertyValueResponse_errorEntries,

    -- ** DeleteAsset
    deleteAsset_clientToken,
    deleteAsset_assetId,
    deleteAssetResponse_httpStatus,
    deleteAssetResponse_assetStatus,

    -- ** UpdateAsset
    updateAsset_clientToken,
    updateAsset_assetId,
    updateAsset_assetName,
    updateAssetResponse_httpStatus,
    updateAssetResponse_assetStatus,

    -- ** DeleteGateway
    deleteGateway_gatewayId,

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

    -- ** UpdateGateway
    updateGateway_gatewayId,
    updateGateway_gatewayName,

    -- ** ListProjectAssets
    listProjectAssets_nextToken,
    listProjectAssets_maxResults,
    listProjectAssets_projectId,
    listProjectAssetsResponse_nextToken,
    listProjectAssetsResponse_httpStatus,
    listProjectAssetsResponse_assetIds,

    -- ** CreateGateway
    createGateway_tags,
    createGateway_gatewayName,
    createGateway_gatewayPlatform,
    createGatewayResponse_httpStatus,
    createGatewayResponse_gatewayId,
    createGatewayResponse_gatewayArn,

    -- ** DescribeStorageConfiguration
    describeStorageConfigurationResponse_multiLayerStorage,
    describeStorageConfigurationResponse_lastUpdateDate,
    describeStorageConfigurationResponse_httpStatus,
    describeStorageConfigurationResponse_storageType,
    describeStorageConfigurationResponse_configurationStatus,

    -- ** CreateAsset
    createAsset_clientToken,
    createAsset_tags,
    createAsset_assetName,
    createAsset_assetModelId,
    createAssetResponse_httpStatus,
    createAssetResponse_assetId,
    createAssetResponse_assetArn,
    createAssetResponse_assetStatus,

    -- ** AssociateAssets
    associateAssets_clientToken,
    associateAssets_assetId,
    associateAssets_hierarchyId,
    associateAssets_childAssetId,

    -- ** GetInterpolatedAssetPropertyValues
    getInterpolatedAssetPropertyValues_startTimeOffsetInNanos,
    getInterpolatedAssetPropertyValues_endTimeOffsetInNanos,
    getInterpolatedAssetPropertyValues_propertyAlias,
    getInterpolatedAssetPropertyValues_nextToken,
    getInterpolatedAssetPropertyValues_intervalWindowInSeconds,
    getInterpolatedAssetPropertyValues_propertyId,
    getInterpolatedAssetPropertyValues_assetId,
    getInterpolatedAssetPropertyValues_maxResults,
    getInterpolatedAssetPropertyValues_startTimeInSeconds,
    getInterpolatedAssetPropertyValues_endTimeInSeconds,
    getInterpolatedAssetPropertyValues_quality,
    getInterpolatedAssetPropertyValues_intervalInSeconds,
    getInterpolatedAssetPropertyValues_type,
    getInterpolatedAssetPropertyValuesResponse_nextToken,
    getInterpolatedAssetPropertyValuesResponse_httpStatus,
    getInterpolatedAssetPropertyValuesResponse_interpolatedAssetPropertyValues,

    -- ** DescribeGatewayCapabilityConfiguration
    describeGatewayCapabilityConfiguration_gatewayId,
    describeGatewayCapabilityConfiguration_capabilityNamespace,
    describeGatewayCapabilityConfigurationResponse_httpStatus,
    describeGatewayCapabilityConfigurationResponse_gatewayId,
    describeGatewayCapabilityConfigurationResponse_capabilityNamespace,
    describeGatewayCapabilityConfigurationResponse_capabilityConfiguration,
    describeGatewayCapabilityConfigurationResponse_capabilitySyncStatus,

    -- ** PutDefaultEncryptionConfiguration
    putDefaultEncryptionConfiguration_kmsKeyId,
    putDefaultEncryptionConfiguration_encryptionType,
    putDefaultEncryptionConfigurationResponse_kmsKeyArn,
    putDefaultEncryptionConfigurationResponse_httpStatus,
    putDefaultEncryptionConfigurationResponse_encryptionType,
    putDefaultEncryptionConfigurationResponse_configurationStatus,

    -- ** DeletePortal
    deletePortal_clientToken,
    deletePortal_portalId,
    deletePortalResponse_httpStatus,
    deletePortalResponse_portalStatus,

    -- ** ListAssetRelationships
    listAssetRelationships_nextToken,
    listAssetRelationships_maxResults,
    listAssetRelationships_assetId,
    listAssetRelationships_traversalType,
    listAssetRelationshipsResponse_nextToken,
    listAssetRelationshipsResponse_httpStatus,
    listAssetRelationshipsResponse_assetRelationshipSummaries,

    -- ** UpdatePortal
    updatePortal_clientToken,
    updatePortal_portalDescription,
    updatePortal_notificationSenderEmail,
    updatePortal_portalLogoImage,
    updatePortal_alarms,
    updatePortal_portalId,
    updatePortal_portalName,
    updatePortal_portalContactEmail,
    updatePortal_roleArn,
    updatePortalResponse_httpStatus,
    updatePortalResponse_portalStatus,

    -- ** ListPortals
    listPortals_nextToken,
    listPortals_maxResults,
    listPortalsResponse_portalSummaries,
    listPortalsResponse_nextToken,
    listPortalsResponse_httpStatus,

    -- ** DeleteDashboard
    deleteDashboard_clientToken,
    deleteDashboard_dashboardId,
    deleteDashboardResponse_httpStatus,

    -- ** UpdateDashboard
    updateDashboard_clientToken,
    updateDashboard_dashboardDescription,
    updateDashboard_dashboardId,
    updateDashboard_dashboardName,
    updateDashboard_dashboardDefinition,
    updateDashboardResponse_httpStatus,

    -- ** PutStorageConfiguration
    putStorageConfiguration_multiLayerStorage,
    putStorageConfiguration_storageType,
    putStorageConfigurationResponse_multiLayerStorage,
    putStorageConfigurationResponse_httpStatus,
    putStorageConfigurationResponse_storageType,
    putStorageConfigurationResponse_configurationStatus,

    -- ** CreatePortal
    createPortal_portalAuthMode,
    createPortal_clientToken,
    createPortal_portalDescription,
    createPortal_notificationSenderEmail,
    createPortal_alarms,
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

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** BatchDisassociateProjectAssets
    batchDisassociateProjectAssets_clientToken,
    batchDisassociateProjectAssets_projectId,
    batchDisassociateProjectAssets_assetIds,
    batchDisassociateProjectAssetsResponse_errors,
    batchDisassociateProjectAssetsResponse_httpStatus,

    -- ** GetAssetPropertyAggregates
    getAssetPropertyAggregates_qualities,
    getAssetPropertyAggregates_timeOrdering,
    getAssetPropertyAggregates_propertyAlias,
    getAssetPropertyAggregates_nextToken,
    getAssetPropertyAggregates_propertyId,
    getAssetPropertyAggregates_assetId,
    getAssetPropertyAggregates_maxResults,
    getAssetPropertyAggregates_aggregateTypes,
    getAssetPropertyAggregates_resolution,
    getAssetPropertyAggregates_startDate,
    getAssetPropertyAggregates_endDate,
    getAssetPropertyAggregatesResponse_nextToken,
    getAssetPropertyAggregatesResponse_httpStatus,
    getAssetPropertyAggregatesResponse_aggregatedValues,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** DeleteAssetModel
    deleteAssetModel_clientToken,
    deleteAssetModel_assetModelId,
    deleteAssetModelResponse_httpStatus,
    deleteAssetModelResponse_assetModelStatus,

    -- ** UpdateAssetModel
    updateAssetModel_clientToken,
    updateAssetModel_assetModelDescription,
    updateAssetModel_assetModelProperties,
    updateAssetModel_assetModelCompositeModels,
    updateAssetModel_assetModelHierarchies,
    updateAssetModel_assetModelId,
    updateAssetModel_assetModelName,
    updateAssetModelResponse_httpStatus,
    updateAssetModelResponse_assetModelStatus,

    -- ** UpdateAssetProperty
    updateAssetProperty_clientToken,
    updateAssetProperty_propertyNotificationState,
    updateAssetProperty_propertyAlias,
    updateAssetProperty_assetId,
    updateAssetProperty_propertyId,

    -- ** DescribeLoggingOptions
    describeLoggingOptionsResponse_httpStatus,
    describeLoggingOptionsResponse_loggingOptions,

    -- ** ListGateways
    listGateways_nextToken,
    listGateways_maxResults,
    listGatewaysResponse_nextToken,
    listGatewaysResponse_httpStatus,
    listGatewaysResponse_gatewaySummaries,

    -- ** UpdateGatewayCapabilityConfiguration
    updateGatewayCapabilityConfiguration_gatewayId,
    updateGatewayCapabilityConfiguration_capabilityNamespace,
    updateGatewayCapabilityConfiguration_capabilityConfiguration,
    updateGatewayCapabilityConfigurationResponse_httpStatus,
    updateGatewayCapabilityConfigurationResponse_capabilityNamespace,
    updateGatewayCapabilityConfigurationResponse_capabilitySyncStatus,

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

    -- ** DescribePortal
    describePortal_portalId,
    describePortalResponse_portalAuthMode,
    describePortalResponse_portalDescription,
    describePortalResponse_notificationSenderEmail,
    describePortalResponse_portalLogoImageLocation,
    describePortalResponse_alarms,
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

    -- ** CreateProject
    createProject_clientToken,
    createProject_projectDescription,
    createProject_tags,
    createProject_portalId,
    createProject_projectName,
    createProjectResponse_httpStatus,
    createProjectResponse_projectId,
    createProjectResponse_projectArn,

    -- ** DescribeDefaultEncryptionConfiguration
    describeDefaultEncryptionConfigurationResponse_kmsKeyArn,
    describeDefaultEncryptionConfigurationResponse_httpStatus,
    describeDefaultEncryptionConfigurationResponse_encryptionType,
    describeDefaultEncryptionConfigurationResponse_configurationStatus,

    -- ** ListAssets
    listAssets_assetModelId,
    listAssets_nextToken,
    listAssets_filter,
    listAssets_maxResults,
    listAssetsResponse_nextToken,
    listAssetsResponse_httpStatus,
    listAssetsResponse_assetSummaries,

    -- ** DisassociateAssets
    disassociateAssets_clientToken,
    disassociateAssets_assetId,
    disassociateAssets_hierarchyId,
    disassociateAssets_childAssetId,

    -- * Types

    -- ** AccessPolicySummary
    accessPolicySummary_lastUpdateDate,
    accessPolicySummary_creationDate,
    accessPolicySummary_id,
    accessPolicySummary_identity,
    accessPolicySummary_resource,
    accessPolicySummary_permission,

    -- ** AggregatedValue
    aggregatedValue_quality,
    aggregatedValue_timestamp,
    aggregatedValue_value,

    -- ** Aggregates
    aggregates_maximum,
    aggregates_average,
    aggregates_count,
    aggregates_minimum,
    aggregates_standardDeviation,
    aggregates_sum,

    -- ** Alarms
    alarms_notificationLambdaArn,
    alarms_alarmRoleArn,

    -- ** AssetCompositeModel
    assetCompositeModel_description,
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
    assetProperty_dataTypeSpec,
    assetProperty_notification,
    assetProperty_alias,
    assetProperty_unit,
    assetProperty_id,
    assetProperty_name,
    assetProperty_dataType,

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
    assetSummary_id,
    assetSummary_arn,
    assetSummary_name,
    assetSummary_assetModelId,
    assetSummary_creationDate,
    assetSummary_lastUpdateDate,
    assetSummary_status,
    assetSummary_hierarchies,

    -- ** AssociatedAssetsSummary
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

    -- ** BatchPutAssetPropertyError
    batchPutAssetPropertyError_errorCode,
    batchPutAssetPropertyError_errorMessage,
    batchPutAssetPropertyError_timestamps,

    -- ** BatchPutAssetPropertyErrorEntry
    batchPutAssetPropertyErrorEntry_entryId,
    batchPutAssetPropertyErrorEntry_errors,

    -- ** CompositeModelProperty
    compositeModelProperty_name,
    compositeModelProperty_type,
    compositeModelProperty_assetProperty,

    -- ** ConfigurationErrorDetails
    configurationErrorDetails_code,
    configurationErrorDetails_message,

    -- ** ConfigurationStatus
    configurationStatus_error,
    configurationStatus_state,

    -- ** CustomerManagedS3Storage
    customerManagedS3Storage_s3ResourceArn,
    customerManagedS3Storage_roleArn,

    -- ** DashboardSummary
    dashboardSummary_lastUpdateDate,
    dashboardSummary_creationDate,
    dashboardSummary_description,
    dashboardSummary_id,
    dashboardSummary_name,

    -- ** DetailedError
    detailedError_code,
    detailedError_message,

    -- ** ErrorDetails
    errorDetails_details,
    errorDetails_code,
    errorDetails_message,

    -- ** ExpressionVariable
    expressionVariable_name,
    expressionVariable_value,

    -- ** ForwardingConfig
    forwardingConfig_state,

    -- ** GatewayCapabilitySummary
    gatewayCapabilitySummary_capabilityNamespace,
    gatewayCapabilitySummary_capabilitySyncStatus,

    -- ** GatewayPlatform
    gatewayPlatform_greengrass,
    gatewayPlatform_greengrassV2,

    -- ** GatewaySummary
    gatewaySummary_gatewayPlatform,
    gatewaySummary_gatewayCapabilitySummaries,
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
    identity_iamUser,
    identity_group,
    identity_user,
    identity_iamRole,

    -- ** Image
    image_id,
    image_file,

    -- ** ImageFile
    imageFile_data,
    imageFile_type,

    -- ** ImageLocation
    imageLocation_id,
    imageLocation_url,

    -- ** InterpolatedAssetPropertyValue
    interpolatedAssetPropertyValue_timestamp,
    interpolatedAssetPropertyValue_value,

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
    portalSummary_lastUpdateDate,
    portalSummary_creationDate,
    portalSummary_description,
    portalSummary_roleArn,
    portalSummary_id,
    portalSummary_name,
    portalSummary_startUrl,
    portalSummary_status,

    -- ** ProjectResource
    projectResource_id,

    -- ** ProjectSummary
    projectSummary_lastUpdateDate,
    projectSummary_creationDate,
    projectSummary_description,
    projectSummary_id,
    projectSummary_name,

    -- ** Property
    property_notification,
    property_alias,
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
    propertyType_transform,
    propertyType_metric,
    propertyType_measurement,

    -- ** PutAssetPropertyValueEntry
    putAssetPropertyValueEntry_propertyAlias,
    putAssetPropertyValueEntry_propertyId,
    putAssetPropertyValueEntry_assetId,
    putAssetPropertyValueEntry_entryId,
    putAssetPropertyValueEntry_propertyValues,

    -- ** Resource
    resource_portal,
    resource_project,

    -- ** TimeInNanos
    timeInNanos_offsetInNanos,
    timeInNanos_timeInSeconds,

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
    variant_integerValue,
    variant_doubleValue,
    variant_stringValue,
    variant_booleanValue,
  )
where

import Amazonka.IoTSiteWise.AssociateAssets
import Amazonka.IoTSiteWise.BatchAssociateProjectAssets
import Amazonka.IoTSiteWise.BatchDisassociateProjectAssets
import Amazonka.IoTSiteWise.BatchPutAssetPropertyValue
import Amazonka.IoTSiteWise.CreateAccessPolicy
import Amazonka.IoTSiteWise.CreateAsset
import Amazonka.IoTSiteWise.CreateAssetModel
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
import Amazonka.IoTSiteWise.DescribeAccessPolicy
import Amazonka.IoTSiteWise.DescribeAsset
import Amazonka.IoTSiteWise.DescribeAssetModel
import Amazonka.IoTSiteWise.DescribeAssetProperty
import Amazonka.IoTSiteWise.DescribeDashboard
import Amazonka.IoTSiteWise.DescribeDefaultEncryptionConfiguration
import Amazonka.IoTSiteWise.DescribeGateway
import Amazonka.IoTSiteWise.DescribeGatewayCapabilityConfiguration
import Amazonka.IoTSiteWise.DescribeLoggingOptions
import Amazonka.IoTSiteWise.DescribePortal
import Amazonka.IoTSiteWise.DescribeProject
import Amazonka.IoTSiteWise.DescribeStorageConfiguration
import Amazonka.IoTSiteWise.DisassociateAssets
import Amazonka.IoTSiteWise.GetAssetPropertyAggregates
import Amazonka.IoTSiteWise.GetAssetPropertyValue
import Amazonka.IoTSiteWise.GetAssetPropertyValueHistory
import Amazonka.IoTSiteWise.GetInterpolatedAssetPropertyValues
import Amazonka.IoTSiteWise.ListAccessPolicies
import Amazonka.IoTSiteWise.ListAssetModels
import Amazonka.IoTSiteWise.ListAssetRelationships
import Amazonka.IoTSiteWise.ListAssets
import Amazonka.IoTSiteWise.ListAssociatedAssets
import Amazonka.IoTSiteWise.ListDashboards
import Amazonka.IoTSiteWise.ListGateways
import Amazonka.IoTSiteWise.ListPortals
import Amazonka.IoTSiteWise.ListProjectAssets
import Amazonka.IoTSiteWise.ListProjects
import Amazonka.IoTSiteWise.ListTagsForResource
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
import Amazonka.IoTSiteWise.Types.AssetModelStatus
import Amazonka.IoTSiteWise.Types.AssetModelSummary
import Amazonka.IoTSiteWise.Types.AssetProperty
import Amazonka.IoTSiteWise.Types.AssetPropertyValue
import Amazonka.IoTSiteWise.Types.AssetRelationshipSummary
import Amazonka.IoTSiteWise.Types.AssetStatus
import Amazonka.IoTSiteWise.Types.AssetSummary
import Amazonka.IoTSiteWise.Types.AssociatedAssetsSummary
import Amazonka.IoTSiteWise.Types.Attribute
import Amazonka.IoTSiteWise.Types.BatchPutAssetPropertyError
import Amazonka.IoTSiteWise.Types.BatchPutAssetPropertyErrorEntry
import Amazonka.IoTSiteWise.Types.CompositeModelProperty
import Amazonka.IoTSiteWise.Types.ConfigurationErrorDetails
import Amazonka.IoTSiteWise.Types.ConfigurationStatus
import Amazonka.IoTSiteWise.Types.CustomerManagedS3Storage
import Amazonka.IoTSiteWise.Types.DashboardSummary
import Amazonka.IoTSiteWise.Types.DetailedError
import Amazonka.IoTSiteWise.Types.ErrorDetails
import Amazonka.IoTSiteWise.Types.ExpressionVariable
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
import Amazonka.IoTSiteWise.Types.TimeInNanos
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
