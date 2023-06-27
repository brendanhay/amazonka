{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.IotTwinMaker
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2021-11-29@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- IoT TwinMaker is a service with which you can build operational digital
-- twins of physical systems. IoT TwinMaker overlays measurements and
-- analysis from real-world sensors, cameras, and enterprise applications
-- so you can create data visualizations to monitor your physical factory,
-- building, or industrial plant. You can use this real-world data to
-- monitor operations and diagnose and repair errors.
module Amazonka.IotTwinMaker
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** AccessDeniedException
    _AccessDeniedException,

    -- ** ConflictException
    _ConflictException,

    -- ** ConnectorFailureException
    _ConnectorFailureException,

    -- ** ConnectorTimeoutException
    _ConnectorTimeoutException,

    -- ** InternalServerException
    _InternalServerException,

    -- ** QueryTimeoutException
    _QueryTimeoutException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- ** ServiceQuotaExceededException
    _ServiceQuotaExceededException,

    -- ** ThrottlingException
    _ThrottlingException,

    -- ** TooManyTagsException
    _TooManyTagsException,

    -- ** ValidationException
    _ValidationException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** BatchPutPropertyValues
    BatchPutPropertyValues (BatchPutPropertyValues'),
    newBatchPutPropertyValues,
    BatchPutPropertyValuesResponse (BatchPutPropertyValuesResponse'),
    newBatchPutPropertyValuesResponse,

    -- ** CreateComponentType
    CreateComponentType (CreateComponentType'),
    newCreateComponentType,
    CreateComponentTypeResponse (CreateComponentTypeResponse'),
    newCreateComponentTypeResponse,

    -- ** CreateEntity
    CreateEntity (CreateEntity'),
    newCreateEntity,
    CreateEntityResponse (CreateEntityResponse'),
    newCreateEntityResponse,

    -- ** CreateScene
    CreateScene (CreateScene'),
    newCreateScene,
    CreateSceneResponse (CreateSceneResponse'),
    newCreateSceneResponse,

    -- ** CreateSyncJob
    CreateSyncJob (CreateSyncJob'),
    newCreateSyncJob,
    CreateSyncJobResponse (CreateSyncJobResponse'),
    newCreateSyncJobResponse,

    -- ** CreateWorkspace
    CreateWorkspace (CreateWorkspace'),
    newCreateWorkspace,
    CreateWorkspaceResponse (CreateWorkspaceResponse'),
    newCreateWorkspaceResponse,

    -- ** DeleteComponentType
    DeleteComponentType (DeleteComponentType'),
    newDeleteComponentType,
    DeleteComponentTypeResponse (DeleteComponentTypeResponse'),
    newDeleteComponentTypeResponse,

    -- ** DeleteEntity
    DeleteEntity (DeleteEntity'),
    newDeleteEntity,
    DeleteEntityResponse (DeleteEntityResponse'),
    newDeleteEntityResponse,

    -- ** DeleteScene
    DeleteScene (DeleteScene'),
    newDeleteScene,
    DeleteSceneResponse (DeleteSceneResponse'),
    newDeleteSceneResponse,

    -- ** DeleteSyncJob
    DeleteSyncJob (DeleteSyncJob'),
    newDeleteSyncJob,
    DeleteSyncJobResponse (DeleteSyncJobResponse'),
    newDeleteSyncJobResponse,

    -- ** DeleteWorkspace
    DeleteWorkspace (DeleteWorkspace'),
    newDeleteWorkspace,
    DeleteWorkspaceResponse (DeleteWorkspaceResponse'),
    newDeleteWorkspaceResponse,

    -- ** ExecuteQuery
    ExecuteQuery (ExecuteQuery'),
    newExecuteQuery,
    ExecuteQueryResponse (ExecuteQueryResponse'),
    newExecuteQueryResponse,

    -- ** GetComponentType
    GetComponentType (GetComponentType'),
    newGetComponentType,
    GetComponentTypeResponse (GetComponentTypeResponse'),
    newGetComponentTypeResponse,

    -- ** GetEntity
    GetEntity (GetEntity'),
    newGetEntity,
    GetEntityResponse (GetEntityResponse'),
    newGetEntityResponse,

    -- ** GetPricingPlan
    GetPricingPlan (GetPricingPlan'),
    newGetPricingPlan,
    GetPricingPlanResponse (GetPricingPlanResponse'),
    newGetPricingPlanResponse,

    -- ** GetPropertyValue
    GetPropertyValue (GetPropertyValue'),
    newGetPropertyValue,
    GetPropertyValueResponse (GetPropertyValueResponse'),
    newGetPropertyValueResponse,

    -- ** GetPropertyValueHistory
    GetPropertyValueHistory (GetPropertyValueHistory'),
    newGetPropertyValueHistory,
    GetPropertyValueHistoryResponse (GetPropertyValueHistoryResponse'),
    newGetPropertyValueHistoryResponse,

    -- ** GetScene
    GetScene (GetScene'),
    newGetScene,
    GetSceneResponse (GetSceneResponse'),
    newGetSceneResponse,

    -- ** GetSyncJob
    GetSyncJob (GetSyncJob'),
    newGetSyncJob,
    GetSyncJobResponse (GetSyncJobResponse'),
    newGetSyncJobResponse,

    -- ** GetWorkspace
    GetWorkspace (GetWorkspace'),
    newGetWorkspace,
    GetWorkspaceResponse (GetWorkspaceResponse'),
    newGetWorkspaceResponse,

    -- ** ListComponentTypes
    ListComponentTypes (ListComponentTypes'),
    newListComponentTypes,
    ListComponentTypesResponse (ListComponentTypesResponse'),
    newListComponentTypesResponse,

    -- ** ListEntities
    ListEntities (ListEntities'),
    newListEntities,
    ListEntitiesResponse (ListEntitiesResponse'),
    newListEntitiesResponse,

    -- ** ListScenes
    ListScenes (ListScenes'),
    newListScenes,
    ListScenesResponse (ListScenesResponse'),
    newListScenesResponse,

    -- ** ListSyncJobs
    ListSyncJobs (ListSyncJobs'),
    newListSyncJobs,
    ListSyncJobsResponse (ListSyncJobsResponse'),
    newListSyncJobsResponse,

    -- ** ListSyncResources
    ListSyncResources (ListSyncResources'),
    newListSyncResources,
    ListSyncResourcesResponse (ListSyncResourcesResponse'),
    newListSyncResourcesResponse,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** ListWorkspaces
    ListWorkspaces (ListWorkspaces'),
    newListWorkspaces,
    ListWorkspacesResponse (ListWorkspacesResponse'),
    newListWorkspacesResponse,

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

    -- ** UpdateComponentType
    UpdateComponentType (UpdateComponentType'),
    newUpdateComponentType,
    UpdateComponentTypeResponse (UpdateComponentTypeResponse'),
    newUpdateComponentTypeResponse,

    -- ** UpdateEntity
    UpdateEntity (UpdateEntity'),
    newUpdateEntity,
    UpdateEntityResponse (UpdateEntityResponse'),
    newUpdateEntityResponse,

    -- ** UpdatePricingPlan
    UpdatePricingPlan (UpdatePricingPlan'),
    newUpdatePricingPlan,
    UpdatePricingPlanResponse (UpdatePricingPlanResponse'),
    newUpdatePricingPlanResponse,

    -- ** UpdateScene
    UpdateScene (UpdateScene'),
    newUpdateScene,
    UpdateSceneResponse (UpdateSceneResponse'),
    newUpdateSceneResponse,

    -- ** UpdateWorkspace
    UpdateWorkspace (UpdateWorkspace'),
    newUpdateWorkspace,
    UpdateWorkspaceResponse (UpdateWorkspaceResponse'),
    newUpdateWorkspaceResponse,

    -- * Types

    -- ** ColumnType
    ColumnType (..),

    -- ** ComponentUpdateType
    ComponentUpdateType (..),

    -- ** ErrorCode
    ErrorCode (..),

    -- ** GroupType
    GroupType (..),

    -- ** InterpolationType
    InterpolationType (..),

    -- ** Order
    Order (..),

    -- ** OrderByTime
    OrderByTime (..),

    -- ** ParentEntityUpdateType
    ParentEntityUpdateType (..),

    -- ** PricingMode
    PricingMode (..),

    -- ** PricingTier
    PricingTier (..),

    -- ** PropertyGroupUpdateType
    PropertyGroupUpdateType (..),

    -- ** PropertyUpdateType
    PropertyUpdateType (..),

    -- ** SceneErrorCode
    SceneErrorCode (..),

    -- ** Scope
    Scope (..),

    -- ** State
    State (..),

    -- ** SyncJobState
    SyncJobState (..),

    -- ** SyncResourceState
    SyncResourceState (..),

    -- ** SyncResourceType
    SyncResourceType (..),

    -- ** Type
    Type (..),

    -- ** UpdateReason
    UpdateReason (..),

    -- ** BatchPutPropertyError
    BatchPutPropertyError (BatchPutPropertyError'),
    newBatchPutPropertyError,

    -- ** BatchPutPropertyErrorEntry
    BatchPutPropertyErrorEntry (BatchPutPropertyErrorEntry'),
    newBatchPutPropertyErrorEntry,

    -- ** BundleInformation
    BundleInformation (BundleInformation'),
    newBundleInformation,

    -- ** ColumnDescription
    ColumnDescription (ColumnDescription'),
    newColumnDescription,

    -- ** ComponentPropertyGroupRequest
    ComponentPropertyGroupRequest (ComponentPropertyGroupRequest'),
    newComponentPropertyGroupRequest,

    -- ** ComponentPropertyGroupResponse
    ComponentPropertyGroupResponse (ComponentPropertyGroupResponse'),
    newComponentPropertyGroupResponse,

    -- ** ComponentRequest
    ComponentRequest (ComponentRequest'),
    newComponentRequest,

    -- ** ComponentResponse
    ComponentResponse (ComponentResponse'),
    newComponentResponse,

    -- ** ComponentTypeSummary
    ComponentTypeSummary (ComponentTypeSummary'),
    newComponentTypeSummary,

    -- ** ComponentUpdateRequest
    ComponentUpdateRequest (ComponentUpdateRequest'),
    newComponentUpdateRequest,

    -- ** DataConnector
    DataConnector (DataConnector'),
    newDataConnector,

    -- ** DataType
    DataType (DataType'),
    newDataType,

    -- ** DataValue
    DataValue (DataValue'),
    newDataValue,

    -- ** EntityPropertyReference
    EntityPropertyReference (EntityPropertyReference'),
    newEntityPropertyReference,

    -- ** EntitySummary
    EntitySummary (EntitySummary'),
    newEntitySummary,

    -- ** ErrorDetails
    ErrorDetails (ErrorDetails'),
    newErrorDetails,

    -- ** FunctionRequest
    FunctionRequest (FunctionRequest'),
    newFunctionRequest,

    -- ** FunctionResponse
    FunctionResponse (FunctionResponse'),
    newFunctionResponse,

    -- ** InterpolationParameters
    InterpolationParameters (InterpolationParameters'),
    newInterpolationParameters,

    -- ** LambdaFunction
    LambdaFunction (LambdaFunction'),
    newLambdaFunction,

    -- ** ListComponentTypesFilter
    ListComponentTypesFilter (ListComponentTypesFilter'),
    newListComponentTypesFilter,

    -- ** ListEntitiesFilter
    ListEntitiesFilter (ListEntitiesFilter'),
    newListEntitiesFilter,

    -- ** OrderBy
    OrderBy (OrderBy'),
    newOrderBy,

    -- ** ParentEntityUpdateRequest
    ParentEntityUpdateRequest (ParentEntityUpdateRequest'),
    newParentEntityUpdateRequest,

    -- ** PricingPlan
    PricingPlan (PricingPlan'),
    newPricingPlan,

    -- ** PropertyDefinitionRequest
    PropertyDefinitionRequest (PropertyDefinitionRequest'),
    newPropertyDefinitionRequest,

    -- ** PropertyDefinitionResponse
    PropertyDefinitionResponse (PropertyDefinitionResponse'),
    newPropertyDefinitionResponse,

    -- ** PropertyFilter
    PropertyFilter (PropertyFilter'),
    newPropertyFilter,

    -- ** PropertyGroupRequest
    PropertyGroupRequest (PropertyGroupRequest'),
    newPropertyGroupRequest,

    -- ** PropertyGroupResponse
    PropertyGroupResponse (PropertyGroupResponse'),
    newPropertyGroupResponse,

    -- ** PropertyLatestValue
    PropertyLatestValue (PropertyLatestValue'),
    newPropertyLatestValue,

    -- ** PropertyRequest
    PropertyRequest (PropertyRequest'),
    newPropertyRequest,

    -- ** PropertyResponse
    PropertyResponse (PropertyResponse'),
    newPropertyResponse,

    -- ** PropertyValue
    PropertyValue (PropertyValue'),
    newPropertyValue,

    -- ** PropertyValueEntry
    PropertyValueEntry (PropertyValueEntry'),
    newPropertyValueEntry,

    -- ** PropertyValueHistory
    PropertyValueHistory (PropertyValueHistory'),
    newPropertyValueHistory,

    -- ** QueryResultValue
    QueryResultValue (QueryResultValue'),
    newQueryResultValue,

    -- ** Relationship
    Relationship (Relationship'),
    newRelationship,

    -- ** RelationshipValue
    RelationshipValue (RelationshipValue'),
    newRelationshipValue,

    -- ** Row
    Row (Row'),
    newRow,

    -- ** SceneError
    SceneError (SceneError'),
    newSceneError,

    -- ** SceneSummary
    SceneSummary (SceneSummary'),
    newSceneSummary,

    -- ** Status
    Status (Status'),
    newStatus,

    -- ** SyncJobStatus
    SyncJobStatus (SyncJobStatus'),
    newSyncJobStatus,

    -- ** SyncJobSummary
    SyncJobSummary (SyncJobSummary'),
    newSyncJobSummary,

    -- ** SyncResourceFilter
    SyncResourceFilter (SyncResourceFilter'),
    newSyncResourceFilter,

    -- ** SyncResourceStatus
    SyncResourceStatus (SyncResourceStatus'),
    newSyncResourceStatus,

    -- ** SyncResourceSummary
    SyncResourceSummary (SyncResourceSummary'),
    newSyncResourceSummary,

    -- ** TabularConditions
    TabularConditions (TabularConditions'),
    newTabularConditions,

    -- ** WorkspaceSummary
    WorkspaceSummary (WorkspaceSummary'),
    newWorkspaceSummary,
  )
where

import Amazonka.IotTwinMaker.BatchPutPropertyValues
import Amazonka.IotTwinMaker.CreateComponentType
import Amazonka.IotTwinMaker.CreateEntity
import Amazonka.IotTwinMaker.CreateScene
import Amazonka.IotTwinMaker.CreateSyncJob
import Amazonka.IotTwinMaker.CreateWorkspace
import Amazonka.IotTwinMaker.DeleteComponentType
import Amazonka.IotTwinMaker.DeleteEntity
import Amazonka.IotTwinMaker.DeleteScene
import Amazonka.IotTwinMaker.DeleteSyncJob
import Amazonka.IotTwinMaker.DeleteWorkspace
import Amazonka.IotTwinMaker.ExecuteQuery
import Amazonka.IotTwinMaker.GetComponentType
import Amazonka.IotTwinMaker.GetEntity
import Amazonka.IotTwinMaker.GetPricingPlan
import Amazonka.IotTwinMaker.GetPropertyValue
import Amazonka.IotTwinMaker.GetPropertyValueHistory
import Amazonka.IotTwinMaker.GetScene
import Amazonka.IotTwinMaker.GetSyncJob
import Amazonka.IotTwinMaker.GetWorkspace
import Amazonka.IotTwinMaker.Lens
import Amazonka.IotTwinMaker.ListComponentTypes
import Amazonka.IotTwinMaker.ListEntities
import Amazonka.IotTwinMaker.ListScenes
import Amazonka.IotTwinMaker.ListSyncJobs
import Amazonka.IotTwinMaker.ListSyncResources
import Amazonka.IotTwinMaker.ListTagsForResource
import Amazonka.IotTwinMaker.ListWorkspaces
import Amazonka.IotTwinMaker.TagResource
import Amazonka.IotTwinMaker.Types
import Amazonka.IotTwinMaker.UntagResource
import Amazonka.IotTwinMaker.UpdateComponentType
import Amazonka.IotTwinMaker.UpdateEntity
import Amazonka.IotTwinMaker.UpdatePricingPlan
import Amazonka.IotTwinMaker.UpdateScene
import Amazonka.IotTwinMaker.UpdateWorkspace
import Amazonka.IotTwinMaker.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'IotTwinMaker'.

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
