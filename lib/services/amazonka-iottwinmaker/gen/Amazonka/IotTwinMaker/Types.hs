{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.IotTwinMaker.Types
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IotTwinMaker.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _AccessDeniedException,
    _InternalServerException,
    _TooManyTagsException,
    _ServiceQuotaExceededException,
    _ResourceNotFoundException,
    _QueryTimeoutException,
    _ConflictException,
    _ConnectorFailureException,
    _ThrottlingException,
    _ValidationException,
    _ConnectorTimeoutException,

    -- * ColumnType
    ColumnType (..),

    -- * ComponentUpdateType
    ComponentUpdateType (..),

    -- * ErrorCode
    ErrorCode (..),

    -- * GroupType
    GroupType (..),

    -- * InterpolationType
    InterpolationType (..),

    -- * Order
    Order (..),

    -- * OrderByTime
    OrderByTime (..),

    -- * ParentEntityUpdateType
    ParentEntityUpdateType (..),

    -- * PricingMode
    PricingMode (..),

    -- * PricingTier
    PricingTier (..),

    -- * PropertyGroupUpdateType
    PropertyGroupUpdateType (..),

    -- * PropertyUpdateType
    PropertyUpdateType (..),

    -- * Scope
    Scope (..),

    -- * State
    State (..),

    -- * Type
    Type (..),

    -- * UpdateReason
    UpdateReason (..),

    -- * BatchPutPropertyError
    BatchPutPropertyError (..),
    newBatchPutPropertyError,
    batchPutPropertyError_errorCode,
    batchPutPropertyError_errorMessage,
    batchPutPropertyError_entry,

    -- * BatchPutPropertyErrorEntry
    BatchPutPropertyErrorEntry (..),
    newBatchPutPropertyErrorEntry,
    batchPutPropertyErrorEntry_errors,

    -- * BundleInformation
    BundleInformation (..),
    newBundleInformation,
    bundleInformation_pricingTier,
    bundleInformation_bundleNames,

    -- * ColumnDescription
    ColumnDescription (..),
    newColumnDescription,
    columnDescription_name,
    columnDescription_type,

    -- * ComponentPropertyGroupRequest
    ComponentPropertyGroupRequest (..),
    newComponentPropertyGroupRequest,
    componentPropertyGroupRequest_updateType,
    componentPropertyGroupRequest_propertyNames,
    componentPropertyGroupRequest_groupType,

    -- * ComponentPropertyGroupResponse
    ComponentPropertyGroupResponse (..),
    newComponentPropertyGroupResponse,
    componentPropertyGroupResponse_groupType,
    componentPropertyGroupResponse_propertyNames,
    componentPropertyGroupResponse_isInherited,

    -- * ComponentRequest
    ComponentRequest (..),
    newComponentRequest,
    componentRequest_properties,
    componentRequest_description,
    componentRequest_propertyGroups,
    componentRequest_componentTypeId,

    -- * ComponentResponse
    ComponentResponse (..),
    newComponentResponse,
    componentResponse_componentName,
    componentResponse_properties,
    componentResponse_definedIn,
    componentResponse_status,
    componentResponse_description,
    componentResponse_propertyGroups,
    componentResponse_componentTypeId,

    -- * ComponentTypeSummary
    ComponentTypeSummary (..),
    newComponentTypeSummary,
    componentTypeSummary_status,
    componentTypeSummary_description,
    componentTypeSummary_arn,
    componentTypeSummary_componentTypeId,
    componentTypeSummary_creationDateTime,
    componentTypeSummary_updateDateTime,

    -- * ComponentUpdateRequest
    ComponentUpdateRequest (..),
    newComponentUpdateRequest,
    componentUpdateRequest_updateType,
    componentUpdateRequest_propertyUpdates,
    componentUpdateRequest_propertyGroupUpdates,
    componentUpdateRequest_description,
    componentUpdateRequest_componentTypeId,

    -- * DataConnector
    DataConnector (..),
    newDataConnector,
    dataConnector_lambda,
    dataConnector_isNative,

    -- * DataType
    DataType (..),
    newDataType,
    dataType_nestedType,
    dataType_relationship,
    dataType_unitOfMeasure,
    dataType_allowedValues,
    dataType_type,

    -- * DataValue
    DataValue (..),
    newDataValue,
    dataValue_integerValue,
    dataValue_doubleValue,
    dataValue_booleanValue,
    dataValue_expression,
    dataValue_mapValue,
    dataValue_stringValue,
    dataValue_relationshipValue,
    dataValue_listValue,
    dataValue_longValue,

    -- * EntityPropertyReference
    EntityPropertyReference (..),
    newEntityPropertyReference,
    entityPropertyReference_entityId,
    entityPropertyReference_componentName,
    entityPropertyReference_externalIdProperty,
    entityPropertyReference_propertyName,

    -- * EntitySummary
    EntitySummary (..),
    newEntitySummary,
    entitySummary_hasChildEntities,
    entitySummary_parentEntityId,
    entitySummary_description,
    entitySummary_entityId,
    entitySummary_entityName,
    entitySummary_arn,
    entitySummary_status,
    entitySummary_creationDateTime,
    entitySummary_updateDateTime,

    -- * ErrorDetails
    ErrorDetails (..),
    newErrorDetails,
    errorDetails_message,
    errorDetails_code,

    -- * FunctionRequest
    FunctionRequest (..),
    newFunctionRequest,
    functionRequest_requiredProperties,
    functionRequest_implementedBy,
    functionRequest_scope,

    -- * FunctionResponse
    FunctionResponse (..),
    newFunctionResponse,
    functionResponse_requiredProperties,
    functionResponse_implementedBy,
    functionResponse_scope,
    functionResponse_isInherited,

    -- * InterpolationParameters
    InterpolationParameters (..),
    newInterpolationParameters,
    interpolationParameters_interpolationType,
    interpolationParameters_intervalInSeconds,

    -- * LambdaFunction
    LambdaFunction (..),
    newLambdaFunction,
    lambdaFunction_arn,

    -- * ListComponentTypesFilter
    ListComponentTypesFilter (..),
    newListComponentTypesFilter,
    listComponentTypesFilter_isAbstract,
    listComponentTypesFilter_namespace,
    listComponentTypesFilter_extendsFrom,

    -- * ListEntitiesFilter
    ListEntitiesFilter (..),
    newListEntitiesFilter,
    listEntitiesFilter_parentEntityId,
    listEntitiesFilter_externalId,
    listEntitiesFilter_componentTypeId,

    -- * OrderBy
    OrderBy (..),
    newOrderBy,
    orderBy_order,
    orderBy_propertyName,

    -- * ParentEntityUpdateRequest
    ParentEntityUpdateRequest (..),
    newParentEntityUpdateRequest,
    parentEntityUpdateRequest_parentEntityId,
    parentEntityUpdateRequest_updateType,

    -- * PricingPlan
    PricingPlan (..),
    newPricingPlan,
    pricingPlan_bundleInformation,
    pricingPlan_billableEntityCount,
    pricingPlan_effectiveDateTime,
    pricingPlan_pricingMode,
    pricingPlan_updateDateTime,
    pricingPlan_updateReason,

    -- * PropertyDefinitionRequest
    PropertyDefinitionRequest (..),
    newPropertyDefinitionRequest,
    propertyDefinitionRequest_isExternalId,
    propertyDefinitionRequest_isStoredExternally,
    propertyDefinitionRequest_configuration,
    propertyDefinitionRequest_defaultValue,
    propertyDefinitionRequest_isRequiredInEntity,
    propertyDefinitionRequest_dataType,
    propertyDefinitionRequest_isTimeSeries,

    -- * PropertyDefinitionResponse
    PropertyDefinitionResponse (..),
    newPropertyDefinitionResponse,
    propertyDefinitionResponse_configuration,
    propertyDefinitionResponse_defaultValue,
    propertyDefinitionResponse_dataType,
    propertyDefinitionResponse_isTimeSeries,
    propertyDefinitionResponse_isRequiredInEntity,
    propertyDefinitionResponse_isExternalId,
    propertyDefinitionResponse_isStoredExternally,
    propertyDefinitionResponse_isImported,
    propertyDefinitionResponse_isFinal,
    propertyDefinitionResponse_isInherited,

    -- * PropertyFilter
    PropertyFilter (..),
    newPropertyFilter,
    propertyFilter_operator,
    propertyFilter_propertyName,
    propertyFilter_value,

    -- * PropertyGroupRequest
    PropertyGroupRequest (..),
    newPropertyGroupRequest,
    propertyGroupRequest_propertyNames,
    propertyGroupRequest_groupType,

    -- * PropertyGroupResponse
    PropertyGroupResponse (..),
    newPropertyGroupResponse,
    propertyGroupResponse_groupType,
    propertyGroupResponse_propertyNames,
    propertyGroupResponse_isInherited,

    -- * PropertyLatestValue
    PropertyLatestValue (..),
    newPropertyLatestValue,
    propertyLatestValue_propertyValue,
    propertyLatestValue_propertyReference,

    -- * PropertyRequest
    PropertyRequest (..),
    newPropertyRequest,
    propertyRequest_updateType,
    propertyRequest_definition,
    propertyRequest_value,

    -- * PropertyResponse
    PropertyResponse (..),
    newPropertyResponse,
    propertyResponse_definition,
    propertyResponse_value,

    -- * PropertyValue
    PropertyValue (..),
    newPropertyValue,
    propertyValue_time,
    propertyValue_timestamp,
    propertyValue_value,

    -- * PropertyValueEntry
    PropertyValueEntry (..),
    newPropertyValueEntry,
    propertyValueEntry_propertyValues,
    propertyValueEntry_entityPropertyReference,

    -- * PropertyValueHistory
    PropertyValueHistory (..),
    newPropertyValueHistory,
    propertyValueHistory_values,
    propertyValueHistory_entityPropertyReference,

    -- * QueryResultValue
    QueryResultValue (..),
    newQueryResultValue,

    -- * Relationship
    Relationship (..),
    newRelationship,
    relationship_targetComponentTypeId,
    relationship_relationshipType,

    -- * RelationshipValue
    RelationshipValue (..),
    newRelationshipValue,
    relationshipValue_targetEntityId,
    relationshipValue_targetComponentName,

    -- * Row
    Row (..),
    newRow,
    row_rowData,

    -- * SceneSummary
    SceneSummary (..),
    newSceneSummary,
    sceneSummary_description,
    sceneSummary_sceneId,
    sceneSummary_contentLocation,
    sceneSummary_arn,
    sceneSummary_creationDateTime,
    sceneSummary_updateDateTime,

    -- * Status
    Status (..),
    newStatus,
    status_state,
    status_error,

    -- * TabularConditions
    TabularConditions (..),
    newTabularConditions,
    tabularConditions_propertyFilters,
    tabularConditions_orderBy,

    -- * WorkspaceSummary
    WorkspaceSummary (..),
    newWorkspaceSummary,
    workspaceSummary_description,
    workspaceSummary_workspaceId,
    workspaceSummary_arn,
    workspaceSummary_creationDateTime,
    workspaceSummary_updateDateTime,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.IotTwinMaker.Types.BatchPutPropertyError
import Amazonka.IotTwinMaker.Types.BatchPutPropertyErrorEntry
import Amazonka.IotTwinMaker.Types.BundleInformation
import Amazonka.IotTwinMaker.Types.ColumnDescription
import Amazonka.IotTwinMaker.Types.ColumnType
import Amazonka.IotTwinMaker.Types.ComponentPropertyGroupRequest
import Amazonka.IotTwinMaker.Types.ComponentPropertyGroupResponse
import Amazonka.IotTwinMaker.Types.ComponentRequest
import Amazonka.IotTwinMaker.Types.ComponentResponse
import Amazonka.IotTwinMaker.Types.ComponentTypeSummary
import Amazonka.IotTwinMaker.Types.ComponentUpdateRequest
import Amazonka.IotTwinMaker.Types.ComponentUpdateType
import Amazonka.IotTwinMaker.Types.DataConnector
import Amazonka.IotTwinMaker.Types.DataType
import Amazonka.IotTwinMaker.Types.DataValue
import Amazonka.IotTwinMaker.Types.EntityPropertyReference
import Amazonka.IotTwinMaker.Types.EntitySummary
import Amazonka.IotTwinMaker.Types.ErrorCode
import Amazonka.IotTwinMaker.Types.ErrorDetails
import Amazonka.IotTwinMaker.Types.FunctionRequest
import Amazonka.IotTwinMaker.Types.FunctionResponse
import Amazonka.IotTwinMaker.Types.GroupType
import Amazonka.IotTwinMaker.Types.InterpolationParameters
import Amazonka.IotTwinMaker.Types.InterpolationType
import Amazonka.IotTwinMaker.Types.LambdaFunction
import Amazonka.IotTwinMaker.Types.ListComponentTypesFilter
import Amazonka.IotTwinMaker.Types.ListEntitiesFilter
import Amazonka.IotTwinMaker.Types.Order
import Amazonka.IotTwinMaker.Types.OrderBy
import Amazonka.IotTwinMaker.Types.OrderByTime
import Amazonka.IotTwinMaker.Types.ParentEntityUpdateRequest
import Amazonka.IotTwinMaker.Types.ParentEntityUpdateType
import Amazonka.IotTwinMaker.Types.PricingMode
import Amazonka.IotTwinMaker.Types.PricingPlan
import Amazonka.IotTwinMaker.Types.PricingTier
import Amazonka.IotTwinMaker.Types.PropertyDefinitionRequest
import Amazonka.IotTwinMaker.Types.PropertyDefinitionResponse
import Amazonka.IotTwinMaker.Types.PropertyFilter
import Amazonka.IotTwinMaker.Types.PropertyGroupRequest
import Amazonka.IotTwinMaker.Types.PropertyGroupResponse
import Amazonka.IotTwinMaker.Types.PropertyGroupUpdateType
import Amazonka.IotTwinMaker.Types.PropertyLatestValue
import Amazonka.IotTwinMaker.Types.PropertyRequest
import Amazonka.IotTwinMaker.Types.PropertyResponse
import Amazonka.IotTwinMaker.Types.PropertyUpdateType
import Amazonka.IotTwinMaker.Types.PropertyValue
import Amazonka.IotTwinMaker.Types.PropertyValueEntry
import Amazonka.IotTwinMaker.Types.PropertyValueHistory
import Amazonka.IotTwinMaker.Types.QueryResultValue
import Amazonka.IotTwinMaker.Types.Relationship
import Amazonka.IotTwinMaker.Types.RelationshipValue
import Amazonka.IotTwinMaker.Types.Row
import Amazonka.IotTwinMaker.Types.SceneSummary
import Amazonka.IotTwinMaker.Types.Scope
import Amazonka.IotTwinMaker.Types.State
import Amazonka.IotTwinMaker.Types.Status
import Amazonka.IotTwinMaker.Types.TabularConditions
import Amazonka.IotTwinMaker.Types.Type
import Amazonka.IotTwinMaker.Types.UpdateReason
import Amazonka.IotTwinMaker.Types.WorkspaceSummary
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2021-11-29@ of the Amazon IoT TwinMaker SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "IotTwinMaker",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "iottwinmaker",
      Core.signingName = "iottwinmaker",
      Core.version = "2021-11-29",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error = Core.parseJSONError "IotTwinMaker",
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
      | Lens.has (Core.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Lens.has (Core.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Core.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has (Core.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Prelude.otherwise = Prelude.Nothing

-- | Access is denied.
_AccessDeniedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AccessDeniedException =
  Core._MatchServiceError
    defaultService
    "AccessDeniedException"
    Prelude.. Core.hasStatus 403

-- | An unexpected error has occurred.
_InternalServerException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InternalServerException =
  Core._MatchServiceError
    defaultService
    "InternalServerException"
    Prelude.. Core.hasStatus 500

-- | The number of tags exceeds the limit.
_TooManyTagsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TooManyTagsException =
  Core._MatchServiceError
    defaultService
    "TooManyTagsException"
    Prelude.. Core.hasStatus 400

-- | The service quota was exceeded.
_ServiceQuotaExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ServiceQuotaExceededException =
  Core._MatchServiceError
    defaultService
    "ServiceQuotaExceededException"
    Prelude.. Core.hasStatus 402

-- | The resource wasn\'t found.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"
    Prelude.. Core.hasStatus 404

-- | The query timeout exception.
_QueryTimeoutException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_QueryTimeoutException =
  Core._MatchServiceError
    defaultService
    "QueryTimeoutException"
    Prelude.. Core.hasStatus 400

-- | A conflict occurred.
_ConflictException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ConflictException =
  Core._MatchServiceError
    defaultService
    "ConflictException"
    Prelude.. Core.hasStatus 409

-- | The connector failed.
_ConnectorFailureException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ConnectorFailureException =
  Core._MatchServiceError
    defaultService
    "ConnectorFailureException"
    Prelude.. Core.hasStatus 424

-- | The rate exceeds the limit.
_ThrottlingException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ThrottlingException =
  Core._MatchServiceError
    defaultService
    "ThrottlingException"
    Prelude.. Core.hasStatus 429

-- | Failed
_ValidationException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ValidationException =
  Core._MatchServiceError
    defaultService
    "ValidationException"
    Prelude.. Core.hasStatus 400

-- | The connector timed out.
_ConnectorTimeoutException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ConnectorTimeoutException =
  Core._MatchServiceError
    defaultService
    "ConnectorTimeoutException"
    Prelude.. Core.hasStatus 424
