{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTSiteWise.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTSiteWise.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _InvalidRequestException,
    _ResourceAlreadyExistsException,
    _TooManyTagsException,
    _ConflictingOperationException,
    _ThrottlingException,
    _InternalFailureException,
    _ServiceUnavailableException,
    _UnauthorizedException,
    _ResourceNotFoundException,
    _LimitExceededException,

    -- * AggregateType
    AggregateType (..),

    -- * AssetErrorCode
    AssetErrorCode (..),

    -- * AssetModelState
    AssetModelState (..),

    -- * AssetRelationshipType
    AssetRelationshipType (..),

    -- * AssetState
    AssetState (..),

    -- * AuthMode
    AuthMode (..),

    -- * BatchPutAssetPropertyValueErrorCode
    BatchPutAssetPropertyValueErrorCode (..),

    -- * CapabilitySyncStatus
    CapabilitySyncStatus (..),

    -- * ComputeLocation
    ComputeLocation (..),

    -- * ConfigurationState
    ConfigurationState (..),

    -- * DetailedErrorCode
    DetailedErrorCode (..),

    -- * EncryptionType
    EncryptionType (..),

    -- * ErrorCode
    ErrorCode (..),

    -- * ForwardingConfigState
    ForwardingConfigState (..),

    -- * IdentityType
    IdentityType (..),

    -- * ImageFileType
    ImageFileType (..),

    -- * ListAssetsFilter
    ListAssetsFilter (..),

    -- * LoggingLevel
    LoggingLevel (..),

    -- * MonitorErrorCode
    MonitorErrorCode (..),

    -- * Permission
    Permission (..),

    -- * PortalState
    PortalState (..),

    -- * PropertyDataType
    PropertyDataType (..),

    -- * PropertyNotificationState
    PropertyNotificationState (..),

    -- * Quality
    Quality (..),

    -- * ResourceType
    ResourceType (..),

    -- * StorageType
    StorageType (..),

    -- * TimeOrdering
    TimeOrdering (..),

    -- * TraversalDirection
    TraversalDirection (..),

    -- * TraversalType
    TraversalType (..),

    -- * AccessPolicySummary
    AccessPolicySummary (..),
    newAccessPolicySummary,
    accessPolicySummary_lastUpdateDate,
    accessPolicySummary_creationDate,
    accessPolicySummary_id,
    accessPolicySummary_identity,
    accessPolicySummary_resource,
    accessPolicySummary_permission,

    -- * AggregatedValue
    AggregatedValue (..),
    newAggregatedValue,
    aggregatedValue_quality,
    aggregatedValue_timestamp,
    aggregatedValue_value,

    -- * Aggregates
    Aggregates (..),
    newAggregates,
    aggregates_maximum,
    aggregates_average,
    aggregates_count,
    aggregates_minimum,
    aggregates_standardDeviation,
    aggregates_sum,

    -- * Alarms
    Alarms (..),
    newAlarms,
    alarms_notificationLambdaArn,
    alarms_alarmRoleArn,

    -- * AssetCompositeModel
    AssetCompositeModel (..),
    newAssetCompositeModel,
    assetCompositeModel_description,
    assetCompositeModel_name,
    assetCompositeModel_type,
    assetCompositeModel_properties,

    -- * AssetErrorDetails
    AssetErrorDetails (..),
    newAssetErrorDetails,
    assetErrorDetails_assetId,
    assetErrorDetails_code,
    assetErrorDetails_message,

    -- * AssetHierarchy
    AssetHierarchy (..),
    newAssetHierarchy,
    assetHierarchy_id,
    assetHierarchy_name,

    -- * AssetHierarchyInfo
    AssetHierarchyInfo (..),
    newAssetHierarchyInfo,
    assetHierarchyInfo_childAssetId,
    assetHierarchyInfo_parentAssetId,

    -- * AssetModelCompositeModel
    AssetModelCompositeModel (..),
    newAssetModelCompositeModel,
    assetModelCompositeModel_description,
    assetModelCompositeModel_properties,
    assetModelCompositeModel_name,
    assetModelCompositeModel_type,

    -- * AssetModelCompositeModelDefinition
    AssetModelCompositeModelDefinition (..),
    newAssetModelCompositeModelDefinition,
    assetModelCompositeModelDefinition_description,
    assetModelCompositeModelDefinition_properties,
    assetModelCompositeModelDefinition_name,
    assetModelCompositeModelDefinition_type,

    -- * AssetModelHierarchy
    AssetModelHierarchy (..),
    newAssetModelHierarchy,
    assetModelHierarchy_id,
    assetModelHierarchy_name,
    assetModelHierarchy_childAssetModelId,

    -- * AssetModelHierarchyDefinition
    AssetModelHierarchyDefinition (..),
    newAssetModelHierarchyDefinition,
    assetModelHierarchyDefinition_name,
    assetModelHierarchyDefinition_childAssetModelId,

    -- * AssetModelProperty
    AssetModelProperty (..),
    newAssetModelProperty,
    assetModelProperty_dataTypeSpec,
    assetModelProperty_id,
    assetModelProperty_unit,
    assetModelProperty_name,
    assetModelProperty_dataType,
    assetModelProperty_type,

    -- * AssetModelPropertyDefinition
    AssetModelPropertyDefinition (..),
    newAssetModelPropertyDefinition,
    assetModelPropertyDefinition_dataTypeSpec,
    assetModelPropertyDefinition_unit,
    assetModelPropertyDefinition_name,
    assetModelPropertyDefinition_dataType,
    assetModelPropertyDefinition_type,

    -- * AssetModelStatus
    AssetModelStatus (..),
    newAssetModelStatus,
    assetModelStatus_error,
    assetModelStatus_state,

    -- * AssetModelSummary
    AssetModelSummary (..),
    newAssetModelSummary,
    assetModelSummary_id,
    assetModelSummary_arn,
    assetModelSummary_name,
    assetModelSummary_description,
    assetModelSummary_creationDate,
    assetModelSummary_lastUpdateDate,
    assetModelSummary_status,

    -- * AssetProperty
    AssetProperty (..),
    newAssetProperty,
    assetProperty_dataTypeSpec,
    assetProperty_notification,
    assetProperty_alias,
    assetProperty_unit,
    assetProperty_id,
    assetProperty_name,
    assetProperty_dataType,

    -- * AssetPropertyValue
    AssetPropertyValue (..),
    newAssetPropertyValue,
    assetPropertyValue_quality,
    assetPropertyValue_value,
    assetPropertyValue_timestamp,

    -- * AssetRelationshipSummary
    AssetRelationshipSummary (..),
    newAssetRelationshipSummary,
    assetRelationshipSummary_hierarchyInfo,
    assetRelationshipSummary_relationshipType,

    -- * AssetStatus
    AssetStatus (..),
    newAssetStatus,
    assetStatus_error,
    assetStatus_state,

    -- * AssetSummary
    AssetSummary (..),
    newAssetSummary,
    assetSummary_id,
    assetSummary_arn,
    assetSummary_name,
    assetSummary_assetModelId,
    assetSummary_creationDate,
    assetSummary_lastUpdateDate,
    assetSummary_status,
    assetSummary_hierarchies,

    -- * AssociatedAssetsSummary
    AssociatedAssetsSummary (..),
    newAssociatedAssetsSummary,
    associatedAssetsSummary_id,
    associatedAssetsSummary_arn,
    associatedAssetsSummary_name,
    associatedAssetsSummary_assetModelId,
    associatedAssetsSummary_creationDate,
    associatedAssetsSummary_lastUpdateDate,
    associatedAssetsSummary_status,
    associatedAssetsSummary_hierarchies,

    -- * Attribute
    Attribute (..),
    newAttribute,
    attribute_defaultValue,

    -- * BatchPutAssetPropertyError
    BatchPutAssetPropertyError (..),
    newBatchPutAssetPropertyError,
    batchPutAssetPropertyError_errorCode,
    batchPutAssetPropertyError_errorMessage,
    batchPutAssetPropertyError_timestamps,

    -- * BatchPutAssetPropertyErrorEntry
    BatchPutAssetPropertyErrorEntry (..),
    newBatchPutAssetPropertyErrorEntry,
    batchPutAssetPropertyErrorEntry_entryId,
    batchPutAssetPropertyErrorEntry_errors,

    -- * CompositeModelProperty
    CompositeModelProperty (..),
    newCompositeModelProperty,
    compositeModelProperty_name,
    compositeModelProperty_type,
    compositeModelProperty_assetProperty,

    -- * ConfigurationErrorDetails
    ConfigurationErrorDetails (..),
    newConfigurationErrorDetails,
    configurationErrorDetails_code,
    configurationErrorDetails_message,

    -- * ConfigurationStatus
    ConfigurationStatus (..),
    newConfigurationStatus,
    configurationStatus_error,
    configurationStatus_state,

    -- * CustomerManagedS3Storage
    CustomerManagedS3Storage (..),
    newCustomerManagedS3Storage,
    customerManagedS3Storage_s3ResourceArn,
    customerManagedS3Storage_roleArn,

    -- * DashboardSummary
    DashboardSummary (..),
    newDashboardSummary,
    dashboardSummary_lastUpdateDate,
    dashboardSummary_creationDate,
    dashboardSummary_description,
    dashboardSummary_id,
    dashboardSummary_name,

    -- * DetailedError
    DetailedError (..),
    newDetailedError,
    detailedError_code,
    detailedError_message,

    -- * ErrorDetails
    ErrorDetails (..),
    newErrorDetails,
    errorDetails_details,
    errorDetails_code,
    errorDetails_message,

    -- * ExpressionVariable
    ExpressionVariable (..),
    newExpressionVariable,
    expressionVariable_name,
    expressionVariable_value,

    -- * ForwardingConfig
    ForwardingConfig (..),
    newForwardingConfig,
    forwardingConfig_state,

    -- * GatewayCapabilitySummary
    GatewayCapabilitySummary (..),
    newGatewayCapabilitySummary,
    gatewayCapabilitySummary_capabilityNamespace,
    gatewayCapabilitySummary_capabilitySyncStatus,

    -- * GatewayPlatform
    GatewayPlatform (..),
    newGatewayPlatform,
    gatewayPlatform_greengrass,
    gatewayPlatform_greengrassV2,

    -- * GatewaySummary
    GatewaySummary (..),
    newGatewaySummary,
    gatewaySummary_gatewayPlatform,
    gatewaySummary_gatewayCapabilitySummaries,
    gatewaySummary_gatewayId,
    gatewaySummary_gatewayName,
    gatewaySummary_creationDate,
    gatewaySummary_lastUpdateDate,

    -- * Greengrass
    Greengrass (..),
    newGreengrass,
    greengrass_groupArn,

    -- * GreengrassV2
    GreengrassV2 (..),
    newGreengrassV2,
    greengrassV2_coreDeviceThingName,

    -- * GroupIdentity
    GroupIdentity (..),
    newGroupIdentity,
    groupIdentity_id,

    -- * IAMRoleIdentity
    IAMRoleIdentity (..),
    newIAMRoleIdentity,
    iAMRoleIdentity_arn,

    -- * IAMUserIdentity
    IAMUserIdentity (..),
    newIAMUserIdentity,
    iAMUserIdentity_arn,

    -- * Identity
    Identity (..),
    newIdentity,
    identity_iamUser,
    identity_group,
    identity_user,
    identity_iamRole,

    -- * Image
    Image (..),
    newImage,
    image_id,
    image_file,

    -- * ImageFile
    ImageFile (..),
    newImageFile,
    imageFile_data,
    imageFile_type,

    -- * ImageLocation
    ImageLocation (..),
    newImageLocation,
    imageLocation_id,
    imageLocation_url,

    -- * InterpolatedAssetPropertyValue
    InterpolatedAssetPropertyValue (..),
    newInterpolatedAssetPropertyValue,
    interpolatedAssetPropertyValue_timestamp,
    interpolatedAssetPropertyValue_value,

    -- * LoggingOptions
    LoggingOptions (..),
    newLoggingOptions,
    loggingOptions_level,

    -- * Measurement
    Measurement (..),
    newMeasurement,
    measurement_processingConfig,

    -- * MeasurementProcessingConfig
    MeasurementProcessingConfig (..),
    newMeasurementProcessingConfig,
    measurementProcessingConfig_forwardingConfig,

    -- * Metric
    Metric (..),
    newMetric,
    metric_processingConfig,
    metric_expression,
    metric_variables,
    metric_window,

    -- * MetricProcessingConfig
    MetricProcessingConfig (..),
    newMetricProcessingConfig,
    metricProcessingConfig_computeLocation,

    -- * MetricWindow
    MetricWindow (..),
    newMetricWindow,
    metricWindow_tumbling,

    -- * MonitorErrorDetails
    MonitorErrorDetails (..),
    newMonitorErrorDetails,
    monitorErrorDetails_code,
    monitorErrorDetails_message,

    -- * MultiLayerStorage
    MultiLayerStorage (..),
    newMultiLayerStorage,
    multiLayerStorage_customerManagedS3Storage,

    -- * PortalResource
    PortalResource (..),
    newPortalResource,
    portalResource_id,

    -- * PortalStatus
    PortalStatus (..),
    newPortalStatus,
    portalStatus_error,
    portalStatus_state,

    -- * PortalSummary
    PortalSummary (..),
    newPortalSummary,
    portalSummary_lastUpdateDate,
    portalSummary_creationDate,
    portalSummary_description,
    portalSummary_roleArn,
    portalSummary_id,
    portalSummary_name,
    portalSummary_startUrl,
    portalSummary_status,

    -- * ProjectResource
    ProjectResource (..),
    newProjectResource,
    projectResource_id,

    -- * ProjectSummary
    ProjectSummary (..),
    newProjectSummary,
    projectSummary_lastUpdateDate,
    projectSummary_creationDate,
    projectSummary_description,
    projectSummary_id,
    projectSummary_name,

    -- * Property
    Property (..),
    newProperty,
    property_notification,
    property_alias,
    property_type,
    property_unit,
    property_id,
    property_name,
    property_dataType,

    -- * PropertyNotification
    PropertyNotification (..),
    newPropertyNotification,
    propertyNotification_topic,
    propertyNotification_state,

    -- * PropertyType
    PropertyType (..),
    newPropertyType,
    propertyType_attribute,
    propertyType_transform,
    propertyType_metric,
    propertyType_measurement,

    -- * PutAssetPropertyValueEntry
    PutAssetPropertyValueEntry (..),
    newPutAssetPropertyValueEntry,
    putAssetPropertyValueEntry_propertyAlias,
    putAssetPropertyValueEntry_propertyId,
    putAssetPropertyValueEntry_assetId,
    putAssetPropertyValueEntry_entryId,
    putAssetPropertyValueEntry_propertyValues,

    -- * Resource
    Resource (..),
    newResource,
    resource_portal,
    resource_project,

    -- * TimeInNanos
    TimeInNanos (..),
    newTimeInNanos,
    timeInNanos_offsetInNanos,
    timeInNanos_timeInSeconds,

    -- * Transform
    Transform (..),
    newTransform,
    transform_processingConfig,
    transform_expression,
    transform_variables,

    -- * TransformProcessingConfig
    TransformProcessingConfig (..),
    newTransformProcessingConfig,
    transformProcessingConfig_forwardingConfig,
    transformProcessingConfig_computeLocation,

    -- * TumblingWindow
    TumblingWindow (..),
    newTumblingWindow,
    tumblingWindow_offset,
    tumblingWindow_interval,

    -- * UserIdentity
    UserIdentity (..),
    newUserIdentity,
    userIdentity_id,

    -- * VariableValue
    VariableValue (..),
    newVariableValue,
    variableValue_hierarchyId,
    variableValue_propertyId,

    -- * Variant
    Variant (..),
    newVariant,
    variant_integerValue,
    variant_doubleValue,
    variant_stringValue,
    variant_booleanValue,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.IoTSiteWise.Types.AccessPolicySummary
import Network.AWS.IoTSiteWise.Types.AggregateType
import Network.AWS.IoTSiteWise.Types.AggregatedValue
import Network.AWS.IoTSiteWise.Types.Aggregates
import Network.AWS.IoTSiteWise.Types.Alarms
import Network.AWS.IoTSiteWise.Types.AssetCompositeModel
import Network.AWS.IoTSiteWise.Types.AssetErrorCode
import Network.AWS.IoTSiteWise.Types.AssetErrorDetails
import Network.AWS.IoTSiteWise.Types.AssetHierarchy
import Network.AWS.IoTSiteWise.Types.AssetHierarchyInfo
import Network.AWS.IoTSiteWise.Types.AssetModelCompositeModel
import Network.AWS.IoTSiteWise.Types.AssetModelCompositeModelDefinition
import Network.AWS.IoTSiteWise.Types.AssetModelHierarchy
import Network.AWS.IoTSiteWise.Types.AssetModelHierarchyDefinition
import Network.AWS.IoTSiteWise.Types.AssetModelProperty
import Network.AWS.IoTSiteWise.Types.AssetModelPropertyDefinition
import Network.AWS.IoTSiteWise.Types.AssetModelState
import Network.AWS.IoTSiteWise.Types.AssetModelStatus
import Network.AWS.IoTSiteWise.Types.AssetModelSummary
import Network.AWS.IoTSiteWise.Types.AssetProperty
import Network.AWS.IoTSiteWise.Types.AssetPropertyValue
import Network.AWS.IoTSiteWise.Types.AssetRelationshipSummary
import Network.AWS.IoTSiteWise.Types.AssetRelationshipType
import Network.AWS.IoTSiteWise.Types.AssetState
import Network.AWS.IoTSiteWise.Types.AssetStatus
import Network.AWS.IoTSiteWise.Types.AssetSummary
import Network.AWS.IoTSiteWise.Types.AssociatedAssetsSummary
import Network.AWS.IoTSiteWise.Types.Attribute
import Network.AWS.IoTSiteWise.Types.AuthMode
import Network.AWS.IoTSiteWise.Types.BatchPutAssetPropertyError
import Network.AWS.IoTSiteWise.Types.BatchPutAssetPropertyErrorEntry
import Network.AWS.IoTSiteWise.Types.BatchPutAssetPropertyValueErrorCode
import Network.AWS.IoTSiteWise.Types.CapabilitySyncStatus
import Network.AWS.IoTSiteWise.Types.CompositeModelProperty
import Network.AWS.IoTSiteWise.Types.ComputeLocation
import Network.AWS.IoTSiteWise.Types.ConfigurationErrorDetails
import Network.AWS.IoTSiteWise.Types.ConfigurationState
import Network.AWS.IoTSiteWise.Types.ConfigurationStatus
import Network.AWS.IoTSiteWise.Types.CustomerManagedS3Storage
import Network.AWS.IoTSiteWise.Types.DashboardSummary
import Network.AWS.IoTSiteWise.Types.DetailedError
import Network.AWS.IoTSiteWise.Types.DetailedErrorCode
import Network.AWS.IoTSiteWise.Types.EncryptionType
import Network.AWS.IoTSiteWise.Types.ErrorCode
import Network.AWS.IoTSiteWise.Types.ErrorDetails
import Network.AWS.IoTSiteWise.Types.ExpressionVariable
import Network.AWS.IoTSiteWise.Types.ForwardingConfig
import Network.AWS.IoTSiteWise.Types.ForwardingConfigState
import Network.AWS.IoTSiteWise.Types.GatewayCapabilitySummary
import Network.AWS.IoTSiteWise.Types.GatewayPlatform
import Network.AWS.IoTSiteWise.Types.GatewaySummary
import Network.AWS.IoTSiteWise.Types.Greengrass
import Network.AWS.IoTSiteWise.Types.GreengrassV2
import Network.AWS.IoTSiteWise.Types.GroupIdentity
import Network.AWS.IoTSiteWise.Types.IAMRoleIdentity
import Network.AWS.IoTSiteWise.Types.IAMUserIdentity
import Network.AWS.IoTSiteWise.Types.Identity
import Network.AWS.IoTSiteWise.Types.IdentityType
import Network.AWS.IoTSiteWise.Types.Image
import Network.AWS.IoTSiteWise.Types.ImageFile
import Network.AWS.IoTSiteWise.Types.ImageFileType
import Network.AWS.IoTSiteWise.Types.ImageLocation
import Network.AWS.IoTSiteWise.Types.InterpolatedAssetPropertyValue
import Network.AWS.IoTSiteWise.Types.ListAssetsFilter
import Network.AWS.IoTSiteWise.Types.LoggingLevel
import Network.AWS.IoTSiteWise.Types.LoggingOptions
import Network.AWS.IoTSiteWise.Types.Measurement
import Network.AWS.IoTSiteWise.Types.MeasurementProcessingConfig
import Network.AWS.IoTSiteWise.Types.Metric
import Network.AWS.IoTSiteWise.Types.MetricProcessingConfig
import Network.AWS.IoTSiteWise.Types.MetricWindow
import Network.AWS.IoTSiteWise.Types.MonitorErrorCode
import Network.AWS.IoTSiteWise.Types.MonitorErrorDetails
import Network.AWS.IoTSiteWise.Types.MultiLayerStorage
import Network.AWS.IoTSiteWise.Types.Permission
import Network.AWS.IoTSiteWise.Types.PortalResource
import Network.AWS.IoTSiteWise.Types.PortalState
import Network.AWS.IoTSiteWise.Types.PortalStatus
import Network.AWS.IoTSiteWise.Types.PortalSummary
import Network.AWS.IoTSiteWise.Types.ProjectResource
import Network.AWS.IoTSiteWise.Types.ProjectSummary
import Network.AWS.IoTSiteWise.Types.Property
import Network.AWS.IoTSiteWise.Types.PropertyDataType
import Network.AWS.IoTSiteWise.Types.PropertyNotification
import Network.AWS.IoTSiteWise.Types.PropertyNotificationState
import Network.AWS.IoTSiteWise.Types.PropertyType
import Network.AWS.IoTSiteWise.Types.PutAssetPropertyValueEntry
import Network.AWS.IoTSiteWise.Types.Quality
import Network.AWS.IoTSiteWise.Types.Resource
import Network.AWS.IoTSiteWise.Types.ResourceType
import Network.AWS.IoTSiteWise.Types.StorageType
import Network.AWS.IoTSiteWise.Types.TimeInNanos
import Network.AWS.IoTSiteWise.Types.TimeOrdering
import Network.AWS.IoTSiteWise.Types.Transform
import Network.AWS.IoTSiteWise.Types.TransformProcessingConfig
import Network.AWS.IoTSiteWise.Types.TraversalDirection
import Network.AWS.IoTSiteWise.Types.TraversalType
import Network.AWS.IoTSiteWise.Types.TumblingWindow
import Network.AWS.IoTSiteWise.Types.UserIdentity
import Network.AWS.IoTSiteWise.Types.VariableValue
import Network.AWS.IoTSiteWise.Types.Variant
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2019-12-02@ of the Amazon IoT SiteWise SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core._serviceAbbrev = "IoTSiteWise",
      Core._serviceSigner = Sign.v4,
      Core._serviceEndpointPrefix = "iotsitewise",
      Core._serviceSigningName = "iotsitewise",
      Core._serviceVersion = "2019-12-02",
      Core._serviceEndpoint =
        Core.defaultEndpoint defaultService,
      Core._serviceTimeout = Prelude.Just 70,
      Core._serviceCheck = Core.statusSuccess,
      Core._serviceError =
        Core.parseJSONError "IoTSiteWise",
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

-- | The request isn\'t valid. This can occur if your request contains
-- malformed JSON or unsupported characters. Check your request and try
-- again.
_InvalidRequestException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidRequestException =
  Core._MatchServiceError
    defaultService
    "InvalidRequestException"
    Prelude.. Core.hasStatus 400

-- | The resource already exists.
_ResourceAlreadyExistsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceAlreadyExistsException =
  Core._MatchServiceError
    defaultService
    "ResourceAlreadyExistsException"
    Prelude.. Core.hasStatus 409

-- | You\'ve reached the limit for the number of tags allowed for a resource.
-- For more information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html#tag-conventions Tag naming limits and requirements>
-- in the /Amazon Web Services General Reference/.
_TooManyTagsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TooManyTagsException =
  Core._MatchServiceError
    defaultService
    "TooManyTagsException"
    Prelude.. Core.hasStatus 400

-- | Your request has conflicting operations. This can occur if you\'re
-- trying to perform more than one operation on the same resource at the
-- same time.
_ConflictingOperationException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ConflictingOperationException =
  Core._MatchServiceError
    defaultService
    "ConflictingOperationException"
    Prelude.. Core.hasStatus 409

-- | Your request exceeded a rate limit. For example, you might have exceeded
-- the number of IoT SiteWise assets that can be created per second, the
-- allowed number of messages per second, and so on.
--
-- For more information, see
-- <https://docs.aws.amazon.com/iot-sitewise/latest/userguide/quotas.html Quotas>
-- in the /IoT SiteWise User Guide/.
_ThrottlingException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ThrottlingException =
  Core._MatchServiceError
    defaultService
    "ThrottlingException"
    Prelude.. Core.hasStatus 429

-- | IoT SiteWise can\'t process your request right now. Try again later.
_InternalFailureException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InternalFailureException =
  Core._MatchServiceError
    defaultService
    "InternalFailureException"
    Prelude.. Core.hasStatus 500

-- | The requested service is unavailable.
_ServiceUnavailableException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ServiceUnavailableException =
  Core._MatchServiceError
    defaultService
    "ServiceUnavailableException"
    Prelude.. Core.hasStatus 503

-- | You are not authorized.
_UnauthorizedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_UnauthorizedException =
  Core._MatchServiceError
    defaultService
    "UnauthorizedException"
    Prelude.. Core.hasStatus 401

-- | The requested resource can\'t be found.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"
    Prelude.. Core.hasStatus 404

-- | You\'ve reached the limit for a resource. For example, this can occur if
-- you\'re trying to associate more than the allowed number of child assets
-- or attempting to create more than the allowed number of properties for
-- an asset model.
--
-- For more information, see
-- <https://docs.aws.amazon.com/iot-sitewise/latest/userguide/quotas.html Quotas>
-- in the /IoT SiteWise User Guide/.
_LimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_LimitExceededException =
  Core._MatchServiceError
    defaultService
    "LimitExceededException"
    Prelude.. Core.hasStatus 410
