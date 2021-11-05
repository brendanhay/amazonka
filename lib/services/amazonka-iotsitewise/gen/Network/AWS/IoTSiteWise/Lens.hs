{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTSiteWise.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTSiteWise.Lens
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

import Network.AWS.IoTSiteWise.AssociateAssets
import Network.AWS.IoTSiteWise.BatchAssociateProjectAssets
import Network.AWS.IoTSiteWise.BatchDisassociateProjectAssets
import Network.AWS.IoTSiteWise.BatchPutAssetPropertyValue
import Network.AWS.IoTSiteWise.CreateAccessPolicy
import Network.AWS.IoTSiteWise.CreateAsset
import Network.AWS.IoTSiteWise.CreateAssetModel
import Network.AWS.IoTSiteWise.CreateDashboard
import Network.AWS.IoTSiteWise.CreateGateway
import Network.AWS.IoTSiteWise.CreatePortal
import Network.AWS.IoTSiteWise.CreateProject
import Network.AWS.IoTSiteWise.DeleteAccessPolicy
import Network.AWS.IoTSiteWise.DeleteAsset
import Network.AWS.IoTSiteWise.DeleteAssetModel
import Network.AWS.IoTSiteWise.DeleteDashboard
import Network.AWS.IoTSiteWise.DeleteGateway
import Network.AWS.IoTSiteWise.DeletePortal
import Network.AWS.IoTSiteWise.DeleteProject
import Network.AWS.IoTSiteWise.DescribeAccessPolicy
import Network.AWS.IoTSiteWise.DescribeAsset
import Network.AWS.IoTSiteWise.DescribeAssetModel
import Network.AWS.IoTSiteWise.DescribeAssetProperty
import Network.AWS.IoTSiteWise.DescribeDashboard
import Network.AWS.IoTSiteWise.DescribeDefaultEncryptionConfiguration
import Network.AWS.IoTSiteWise.DescribeGateway
import Network.AWS.IoTSiteWise.DescribeGatewayCapabilityConfiguration
import Network.AWS.IoTSiteWise.DescribeLoggingOptions
import Network.AWS.IoTSiteWise.DescribePortal
import Network.AWS.IoTSiteWise.DescribeProject
import Network.AWS.IoTSiteWise.DescribeStorageConfiguration
import Network.AWS.IoTSiteWise.DisassociateAssets
import Network.AWS.IoTSiteWise.GetAssetPropertyAggregates
import Network.AWS.IoTSiteWise.GetAssetPropertyValue
import Network.AWS.IoTSiteWise.GetAssetPropertyValueHistory
import Network.AWS.IoTSiteWise.GetInterpolatedAssetPropertyValues
import Network.AWS.IoTSiteWise.ListAccessPolicies
import Network.AWS.IoTSiteWise.ListAssetModels
import Network.AWS.IoTSiteWise.ListAssetRelationships
import Network.AWS.IoTSiteWise.ListAssets
import Network.AWS.IoTSiteWise.ListAssociatedAssets
import Network.AWS.IoTSiteWise.ListDashboards
import Network.AWS.IoTSiteWise.ListGateways
import Network.AWS.IoTSiteWise.ListPortals
import Network.AWS.IoTSiteWise.ListProjectAssets
import Network.AWS.IoTSiteWise.ListProjects
import Network.AWS.IoTSiteWise.ListTagsForResource
import Network.AWS.IoTSiteWise.PutDefaultEncryptionConfiguration
import Network.AWS.IoTSiteWise.PutLoggingOptions
import Network.AWS.IoTSiteWise.PutStorageConfiguration
import Network.AWS.IoTSiteWise.TagResource
import Network.AWS.IoTSiteWise.Types.AccessPolicySummary
import Network.AWS.IoTSiteWise.Types.AggregatedValue
import Network.AWS.IoTSiteWise.Types.Aggregates
import Network.AWS.IoTSiteWise.Types.Alarms
import Network.AWS.IoTSiteWise.Types.AssetCompositeModel
import Network.AWS.IoTSiteWise.Types.AssetErrorDetails
import Network.AWS.IoTSiteWise.Types.AssetHierarchy
import Network.AWS.IoTSiteWise.Types.AssetHierarchyInfo
import Network.AWS.IoTSiteWise.Types.AssetModelCompositeModel
import Network.AWS.IoTSiteWise.Types.AssetModelCompositeModelDefinition
import Network.AWS.IoTSiteWise.Types.AssetModelHierarchy
import Network.AWS.IoTSiteWise.Types.AssetModelHierarchyDefinition
import Network.AWS.IoTSiteWise.Types.AssetModelProperty
import Network.AWS.IoTSiteWise.Types.AssetModelPropertyDefinition
import Network.AWS.IoTSiteWise.Types.AssetModelStatus
import Network.AWS.IoTSiteWise.Types.AssetModelSummary
import Network.AWS.IoTSiteWise.Types.AssetProperty
import Network.AWS.IoTSiteWise.Types.AssetPropertyValue
import Network.AWS.IoTSiteWise.Types.AssetRelationshipSummary
import Network.AWS.IoTSiteWise.Types.AssetStatus
import Network.AWS.IoTSiteWise.Types.AssetSummary
import Network.AWS.IoTSiteWise.Types.AssociatedAssetsSummary
import Network.AWS.IoTSiteWise.Types.Attribute
import Network.AWS.IoTSiteWise.Types.BatchPutAssetPropertyError
import Network.AWS.IoTSiteWise.Types.BatchPutAssetPropertyErrorEntry
import Network.AWS.IoTSiteWise.Types.CompositeModelProperty
import Network.AWS.IoTSiteWise.Types.ConfigurationErrorDetails
import Network.AWS.IoTSiteWise.Types.ConfigurationStatus
import Network.AWS.IoTSiteWise.Types.CustomerManagedS3Storage
import Network.AWS.IoTSiteWise.Types.DashboardSummary
import Network.AWS.IoTSiteWise.Types.DetailedError
import Network.AWS.IoTSiteWise.Types.ErrorDetails
import Network.AWS.IoTSiteWise.Types.ExpressionVariable
import Network.AWS.IoTSiteWise.Types.ForwardingConfig
import Network.AWS.IoTSiteWise.Types.GatewayCapabilitySummary
import Network.AWS.IoTSiteWise.Types.GatewayPlatform
import Network.AWS.IoTSiteWise.Types.GatewaySummary
import Network.AWS.IoTSiteWise.Types.Greengrass
import Network.AWS.IoTSiteWise.Types.GreengrassV2
import Network.AWS.IoTSiteWise.Types.GroupIdentity
import Network.AWS.IoTSiteWise.Types.IAMRoleIdentity
import Network.AWS.IoTSiteWise.Types.IAMUserIdentity
import Network.AWS.IoTSiteWise.Types.Identity
import Network.AWS.IoTSiteWise.Types.Image
import Network.AWS.IoTSiteWise.Types.ImageFile
import Network.AWS.IoTSiteWise.Types.ImageLocation
import Network.AWS.IoTSiteWise.Types.InterpolatedAssetPropertyValue
import Network.AWS.IoTSiteWise.Types.LoggingOptions
import Network.AWS.IoTSiteWise.Types.Measurement
import Network.AWS.IoTSiteWise.Types.MeasurementProcessingConfig
import Network.AWS.IoTSiteWise.Types.Metric
import Network.AWS.IoTSiteWise.Types.MetricProcessingConfig
import Network.AWS.IoTSiteWise.Types.MetricWindow
import Network.AWS.IoTSiteWise.Types.MonitorErrorDetails
import Network.AWS.IoTSiteWise.Types.MultiLayerStorage
import Network.AWS.IoTSiteWise.Types.PortalResource
import Network.AWS.IoTSiteWise.Types.PortalStatus
import Network.AWS.IoTSiteWise.Types.PortalSummary
import Network.AWS.IoTSiteWise.Types.ProjectResource
import Network.AWS.IoTSiteWise.Types.ProjectSummary
import Network.AWS.IoTSiteWise.Types.Property
import Network.AWS.IoTSiteWise.Types.PropertyNotification
import Network.AWS.IoTSiteWise.Types.PropertyType
import Network.AWS.IoTSiteWise.Types.PutAssetPropertyValueEntry
import Network.AWS.IoTSiteWise.Types.Resource
import Network.AWS.IoTSiteWise.Types.TimeInNanos
import Network.AWS.IoTSiteWise.Types.Transform
import Network.AWS.IoTSiteWise.Types.TransformProcessingConfig
import Network.AWS.IoTSiteWise.Types.TumblingWindow
import Network.AWS.IoTSiteWise.Types.UserIdentity
import Network.AWS.IoTSiteWise.Types.VariableValue
import Network.AWS.IoTSiteWise.Types.Variant
import Network.AWS.IoTSiteWise.UntagResource
import Network.AWS.IoTSiteWise.UpdateAccessPolicy
import Network.AWS.IoTSiteWise.UpdateAsset
import Network.AWS.IoTSiteWise.UpdateAssetModel
import Network.AWS.IoTSiteWise.UpdateAssetProperty
import Network.AWS.IoTSiteWise.UpdateDashboard
import Network.AWS.IoTSiteWise.UpdateGateway
import Network.AWS.IoTSiteWise.UpdateGatewayCapabilityConfiguration
import Network.AWS.IoTSiteWise.UpdatePortal
import Network.AWS.IoTSiteWise.UpdateProject
