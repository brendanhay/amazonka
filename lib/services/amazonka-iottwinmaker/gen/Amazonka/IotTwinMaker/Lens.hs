{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.IotTwinMaker.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IotTwinMaker.Lens
  ( -- * Operations

    -- ** BatchPutPropertyValues
    batchPutPropertyValues_entries,
    batchPutPropertyValues_workspaceId,
    batchPutPropertyValuesResponse_httpStatus,
    batchPutPropertyValuesResponse_errorEntries,

    -- ** CreateComponentType
    createComponentType_tags,
    createComponentType_functions,
    createComponentType_propertyDefinitions,
    createComponentType_description,
    createComponentType_isSingleton,
    createComponentType_extendsFrom,
    createComponentType_componentTypeId,
    createComponentType_workspaceId,
    createComponentTypeResponse_httpStatus,
    createComponentTypeResponse_arn,
    createComponentTypeResponse_creationDateTime,
    createComponentTypeResponse_state,

    -- ** CreateEntity
    createEntity_tags,
    createEntity_entityId,
    createEntity_parentEntityId,
    createEntity_description,
    createEntity_components,
    createEntity_entityName,
    createEntity_workspaceId,
    createEntityResponse_httpStatus,
    createEntityResponse_arn,
    createEntityResponse_creationDateTime,
    createEntityResponse_entityId,
    createEntityResponse_state,

    -- ** CreateScene
    createScene_tags,
    createScene_description,
    createScene_capabilities,
    createScene_contentLocation,
    createScene_sceneId,
    createScene_workspaceId,
    createSceneResponse_httpStatus,
    createSceneResponse_arn,
    createSceneResponse_creationDateTime,

    -- ** CreateWorkspace
    createWorkspace_tags,
    createWorkspace_description,
    createWorkspace_role,
    createWorkspace_s3Location,
    createWorkspace_workspaceId,
    createWorkspaceResponse_httpStatus,
    createWorkspaceResponse_arn,
    createWorkspaceResponse_creationDateTime,

    -- ** DeleteComponentType
    deleteComponentType_componentTypeId,
    deleteComponentType_workspaceId,
    deleteComponentTypeResponse_httpStatus,
    deleteComponentTypeResponse_state,

    -- ** DeleteEntity
    deleteEntity_isRecursive,
    deleteEntity_entityId,
    deleteEntity_workspaceId,
    deleteEntityResponse_httpStatus,
    deleteEntityResponse_state,

    -- ** DeleteScene
    deleteScene_sceneId,
    deleteScene_workspaceId,
    deleteSceneResponse_httpStatus,

    -- ** DeleteWorkspace
    deleteWorkspace_workspaceId,
    deleteWorkspaceResponse_httpStatus,

    -- ** GetComponentType
    getComponentType_componentTypeId,
    getComponentType_workspaceId,
    getComponentTypeResponse_isSchemaInitialized,
    getComponentTypeResponse_functions,
    getComponentTypeResponse_isAbstract,
    getComponentTypeResponse_propertyDefinitions,
    getComponentTypeResponse_status,
    getComponentTypeResponse_description,
    getComponentTypeResponse_isSingleton,
    getComponentTypeResponse_extendsFrom,
    getComponentTypeResponse_httpStatus,
    getComponentTypeResponse_arn,
    getComponentTypeResponse_componentTypeId,
    getComponentTypeResponse_creationDateTime,
    getComponentTypeResponse_updateDateTime,
    getComponentTypeResponse_workspaceId,

    -- ** GetEntity
    getEntity_entityId,
    getEntity_workspaceId,
    getEntityResponse_description,
    getEntityResponse_components,
    getEntityResponse_httpStatus,
    getEntityResponse_arn,
    getEntityResponse_creationDateTime,
    getEntityResponse_entityId,
    getEntityResponse_entityName,
    getEntityResponse_hasChildEntities,
    getEntityResponse_parentEntityId,
    getEntityResponse_status,
    getEntityResponse_updateDateTime,
    getEntityResponse_workspaceId,

    -- ** GetPropertyValue
    getPropertyValue_entityId,
    getPropertyValue_componentName,
    getPropertyValue_componentTypeId,
    getPropertyValue_selectedProperties,
    getPropertyValue_workspaceId,
    getPropertyValueResponse_httpStatus,
    getPropertyValueResponse_propertyValues,

    -- ** GetPropertyValueHistory
    getPropertyValueHistory_entityId,
    getPropertyValueHistory_nextToken,
    getPropertyValueHistory_componentName,
    getPropertyValueHistory_propertyFilters,
    getPropertyValueHistory_startDateTime,
    getPropertyValueHistory_endTime,
    getPropertyValueHistory_maxResults,
    getPropertyValueHistory_interpolation,
    getPropertyValueHistory_orderByTime,
    getPropertyValueHistory_startTime,
    getPropertyValueHistory_endDateTime,
    getPropertyValueHistory_componentTypeId,
    getPropertyValueHistory_selectedProperties,
    getPropertyValueHistory_workspaceId,
    getPropertyValueHistoryResponse_nextToken,
    getPropertyValueHistoryResponse_httpStatus,
    getPropertyValueHistoryResponse_propertyValues,

    -- ** GetScene
    getScene_sceneId,
    getScene_workspaceId,
    getSceneResponse_description,
    getSceneResponse_capabilities,
    getSceneResponse_httpStatus,
    getSceneResponse_arn,
    getSceneResponse_contentLocation,
    getSceneResponse_creationDateTime,
    getSceneResponse_sceneId,
    getSceneResponse_updateDateTime,
    getSceneResponse_workspaceId,

    -- ** GetWorkspace
    getWorkspace_workspaceId,
    getWorkspaceResponse_description,
    getWorkspaceResponse_httpStatus,
    getWorkspaceResponse_arn,
    getWorkspaceResponse_creationDateTime,
    getWorkspaceResponse_role,
    getWorkspaceResponse_s3Location,
    getWorkspaceResponse_updateDateTime,
    getWorkspaceResponse_workspaceId,

    -- ** ListComponentTypes
    listComponentTypes_nextToken,
    listComponentTypes_filters,
    listComponentTypes_maxResults,
    listComponentTypes_workspaceId,
    listComponentTypesResponse_nextToken,
    listComponentTypesResponse_maxResults,
    listComponentTypesResponse_httpStatus,
    listComponentTypesResponse_componentTypeSummaries,
    listComponentTypesResponse_workspaceId,

    -- ** ListEntities
    listEntities_nextToken,
    listEntities_filters,
    listEntities_maxResults,
    listEntities_workspaceId,
    listEntitiesResponse_nextToken,
    listEntitiesResponse_entitySummaries,
    listEntitiesResponse_httpStatus,

    -- ** ListScenes
    listScenes_nextToken,
    listScenes_maxResults,
    listScenes_workspaceId,
    listScenesResponse_nextToken,
    listScenesResponse_sceneSummaries,
    listScenesResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_nextToken,
    listTagsForResource_maxResults,
    listTagsForResource_resourceARN,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_nextToken,
    listTagsForResourceResponse_httpStatus,

    -- ** ListWorkspaces
    listWorkspaces_nextToken,
    listWorkspaces_maxResults,
    listWorkspacesResponse_nextToken,
    listWorkspacesResponse_workspaceSummaries,
    listWorkspacesResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceARN,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceARN,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** UpdateComponentType
    updateComponentType_functions,
    updateComponentType_propertyDefinitions,
    updateComponentType_description,
    updateComponentType_isSingleton,
    updateComponentType_extendsFrom,
    updateComponentType_componentTypeId,
    updateComponentType_workspaceId,
    updateComponentTypeResponse_httpStatus,
    updateComponentTypeResponse_arn,
    updateComponentTypeResponse_componentTypeId,
    updateComponentTypeResponse_state,
    updateComponentTypeResponse_workspaceId,

    -- ** UpdateEntity
    updateEntity_entityName,
    updateEntity_componentUpdates,
    updateEntity_description,
    updateEntity_parentEntityUpdate,
    updateEntity_entityId,
    updateEntity_workspaceId,
    updateEntityResponse_httpStatus,
    updateEntityResponse_state,
    updateEntityResponse_updateDateTime,

    -- ** UpdateScene
    updateScene_contentLocation,
    updateScene_description,
    updateScene_capabilities,
    updateScene_sceneId,
    updateScene_workspaceId,
    updateSceneResponse_httpStatus,
    updateSceneResponse_updateDateTime,

    -- ** UpdateWorkspace
    updateWorkspace_description,
    updateWorkspace_role,
    updateWorkspace_workspaceId,
    updateWorkspaceResponse_httpStatus,
    updateWorkspaceResponse_updateDateTime,

    -- * Types

    -- ** BatchPutPropertyError
    batchPutPropertyError_entry,
    batchPutPropertyError_errorCode,
    batchPutPropertyError_errorMessage,

    -- ** BatchPutPropertyErrorEntry
    batchPutPropertyErrorEntry_errors,

    -- ** ComponentRequest
    componentRequest_properties,
    componentRequest_description,
    componentRequest_componentTypeId,

    -- ** ComponentResponse
    componentResponse_componentName,
    componentResponse_properties,
    componentResponse_definedIn,
    componentResponse_status,
    componentResponse_description,
    componentResponse_componentTypeId,

    -- ** ComponentTypeSummary
    componentTypeSummary_status,
    componentTypeSummary_description,
    componentTypeSummary_arn,
    componentTypeSummary_componentTypeId,
    componentTypeSummary_creationDateTime,
    componentTypeSummary_updateDateTime,

    -- ** ComponentUpdateRequest
    componentUpdateRequest_updateType,
    componentUpdateRequest_propertyUpdates,
    componentUpdateRequest_description,
    componentUpdateRequest_componentTypeId,

    -- ** DataConnector
    dataConnector_lambda,
    dataConnector_isNative,

    -- ** DataType
    dataType_nestedType,
    dataType_relationship,
    dataType_unitOfMeasure,
    dataType_allowedValues,
    dataType_type,

    -- ** DataValue
    dataValue_integerValue,
    dataValue_doubleValue,
    dataValue_booleanValue,
    dataValue_expression,
    dataValue_mapValue,
    dataValue_stringValue,
    dataValue_relationshipValue,
    dataValue_listValue,
    dataValue_longValue,

    -- ** EntityPropertyReference
    entityPropertyReference_entityId,
    entityPropertyReference_componentName,
    entityPropertyReference_externalIdProperty,
    entityPropertyReference_propertyName,

    -- ** EntitySummary
    entitySummary_hasChildEntities,
    entitySummary_parentEntityId,
    entitySummary_description,
    entitySummary_arn,
    entitySummary_creationDateTime,
    entitySummary_entityId,
    entitySummary_entityName,
    entitySummary_status,
    entitySummary_updateDateTime,

    -- ** ErrorDetails
    errorDetails_message,
    errorDetails_code,

    -- ** FunctionRequest
    functionRequest_requiredProperties,
    functionRequest_implementedBy,
    functionRequest_scope,

    -- ** FunctionResponse
    functionResponse_requiredProperties,
    functionResponse_implementedBy,
    functionResponse_scope,
    functionResponse_isInherited,

    -- ** InterpolationParameters
    interpolationParameters_interpolationType,
    interpolationParameters_intervalInSeconds,

    -- ** LambdaFunction
    lambdaFunction_arn,

    -- ** ListComponentTypesFilter
    listComponentTypesFilter_isAbstract,
    listComponentTypesFilter_namespace,
    listComponentTypesFilter_extendsFrom,

    -- ** ListEntitiesFilter
    listEntitiesFilter_parentEntityId,
    listEntitiesFilter_externalId,
    listEntitiesFilter_componentTypeId,

    -- ** ParentEntityUpdateRequest
    parentEntityUpdateRequest_parentEntityId,
    parentEntityUpdateRequest_updateType,

    -- ** PropertyDefinitionRequest
    propertyDefinitionRequest_isExternalId,
    propertyDefinitionRequest_isStoredExternally,
    propertyDefinitionRequest_configuration,
    propertyDefinitionRequest_defaultValue,
    propertyDefinitionRequest_isRequiredInEntity,
    propertyDefinitionRequest_dataType,
    propertyDefinitionRequest_isTimeSeries,

    -- ** PropertyDefinitionResponse
    propertyDefinitionResponse_configuration,
    propertyDefinitionResponse_defaultValue,
    propertyDefinitionResponse_dataType,
    propertyDefinitionResponse_isExternalId,
    propertyDefinitionResponse_isFinal,
    propertyDefinitionResponse_isImported,
    propertyDefinitionResponse_isInherited,
    propertyDefinitionResponse_isRequiredInEntity,
    propertyDefinitionResponse_isStoredExternally,
    propertyDefinitionResponse_isTimeSeries,

    -- ** PropertyFilter
    propertyFilter_operator,
    propertyFilter_propertyName,
    propertyFilter_value,

    -- ** PropertyLatestValue
    propertyLatestValue_propertyValue,
    propertyLatestValue_propertyReference,

    -- ** PropertyRequest
    propertyRequest_updateType,
    propertyRequest_definition,
    propertyRequest_value,

    -- ** PropertyResponse
    propertyResponse_definition,
    propertyResponse_value,

    -- ** PropertyValue
    propertyValue_time,
    propertyValue_timestamp,
    propertyValue_value,

    -- ** PropertyValueEntry
    propertyValueEntry_propertyValues,
    propertyValueEntry_entityPropertyReference,

    -- ** PropertyValueHistory
    propertyValueHistory_values,
    propertyValueHistory_entityPropertyReference,

    -- ** Relationship
    relationship_targetComponentTypeId,
    relationship_relationshipType,

    -- ** RelationshipValue
    relationshipValue_targetEntityId,
    relationshipValue_targetComponentName,

    -- ** SceneSummary
    sceneSummary_description,
    sceneSummary_arn,
    sceneSummary_contentLocation,
    sceneSummary_creationDateTime,
    sceneSummary_sceneId,
    sceneSummary_updateDateTime,

    -- ** Status
    status_state,
    status_error,

    -- ** WorkspaceSummary
    workspaceSummary_description,
    workspaceSummary_arn,
    workspaceSummary_creationDateTime,
    workspaceSummary_updateDateTime,
    workspaceSummary_workspaceId,
  )
where

import Amazonka.IotTwinMaker.BatchPutPropertyValues
import Amazonka.IotTwinMaker.CreateComponentType
import Amazonka.IotTwinMaker.CreateEntity
import Amazonka.IotTwinMaker.CreateScene
import Amazonka.IotTwinMaker.CreateWorkspace
import Amazonka.IotTwinMaker.DeleteComponentType
import Amazonka.IotTwinMaker.DeleteEntity
import Amazonka.IotTwinMaker.DeleteScene
import Amazonka.IotTwinMaker.DeleteWorkspace
import Amazonka.IotTwinMaker.GetComponentType
import Amazonka.IotTwinMaker.GetEntity
import Amazonka.IotTwinMaker.GetPropertyValue
import Amazonka.IotTwinMaker.GetPropertyValueHistory
import Amazonka.IotTwinMaker.GetScene
import Amazonka.IotTwinMaker.GetWorkspace
import Amazonka.IotTwinMaker.ListComponentTypes
import Amazonka.IotTwinMaker.ListEntities
import Amazonka.IotTwinMaker.ListScenes
import Amazonka.IotTwinMaker.ListTagsForResource
import Amazonka.IotTwinMaker.ListWorkspaces
import Amazonka.IotTwinMaker.TagResource
import Amazonka.IotTwinMaker.Types.BatchPutPropertyError
import Amazonka.IotTwinMaker.Types.BatchPutPropertyErrorEntry
import Amazonka.IotTwinMaker.Types.ComponentRequest
import Amazonka.IotTwinMaker.Types.ComponentResponse
import Amazonka.IotTwinMaker.Types.ComponentTypeSummary
import Amazonka.IotTwinMaker.Types.ComponentUpdateRequest
import Amazonka.IotTwinMaker.Types.DataConnector
import Amazonka.IotTwinMaker.Types.DataType
import Amazonka.IotTwinMaker.Types.DataValue
import Amazonka.IotTwinMaker.Types.EntityPropertyReference
import Amazonka.IotTwinMaker.Types.EntitySummary
import Amazonka.IotTwinMaker.Types.ErrorDetails
import Amazonka.IotTwinMaker.Types.FunctionRequest
import Amazonka.IotTwinMaker.Types.FunctionResponse
import Amazonka.IotTwinMaker.Types.InterpolationParameters
import Amazonka.IotTwinMaker.Types.LambdaFunction
import Amazonka.IotTwinMaker.Types.ListComponentTypesFilter
import Amazonka.IotTwinMaker.Types.ListEntitiesFilter
import Amazonka.IotTwinMaker.Types.ParentEntityUpdateRequest
import Amazonka.IotTwinMaker.Types.PropertyDefinitionRequest
import Amazonka.IotTwinMaker.Types.PropertyDefinitionResponse
import Amazonka.IotTwinMaker.Types.PropertyFilter
import Amazonka.IotTwinMaker.Types.PropertyLatestValue
import Amazonka.IotTwinMaker.Types.PropertyRequest
import Amazonka.IotTwinMaker.Types.PropertyResponse
import Amazonka.IotTwinMaker.Types.PropertyValue
import Amazonka.IotTwinMaker.Types.PropertyValueEntry
import Amazonka.IotTwinMaker.Types.PropertyValueHistory
import Amazonka.IotTwinMaker.Types.Relationship
import Amazonka.IotTwinMaker.Types.RelationshipValue
import Amazonka.IotTwinMaker.Types.SceneSummary
import Amazonka.IotTwinMaker.Types.Status
import Amazonka.IotTwinMaker.Types.WorkspaceSummary
import Amazonka.IotTwinMaker.UntagResource
import Amazonka.IotTwinMaker.UpdateComponentType
import Amazonka.IotTwinMaker.UpdateEntity
import Amazonka.IotTwinMaker.UpdateScene
import Amazonka.IotTwinMaker.UpdateWorkspace
