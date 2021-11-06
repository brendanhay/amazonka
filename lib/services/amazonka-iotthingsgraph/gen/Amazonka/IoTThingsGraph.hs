{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.IoTThingsGraph
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2018-09-06@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- AWS IoT Things Graph
--
-- AWS IoT Things Graph provides an integrated set of tools that enable
-- developers to connect devices and services that use different standards,
-- such as units of measure and communication protocols. AWS IoT Things
-- Graph makes it possible to build IoT applications with little to no code
-- by connecting devices and services and defining how they interact at an
-- abstract level.
--
-- For more information about how AWS IoT Things Graph works, see the
-- <https://docs.aws.amazon.com/thingsgraph/latest/ug/iot-tg-whatis.html User Guide>.
module Amazonka.IoTThingsGraph
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** InvalidRequestException
    _InvalidRequestException,

    -- ** ResourceAlreadyExistsException
    _ResourceAlreadyExistsException,

    -- ** ThrottlingException
    _ThrottlingException,

    -- ** InternalFailureException
    _InternalFailureException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- ** LimitExceededException
    _LimitExceededException,

    -- ** ResourceInUseException
    _ResourceInUseException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** GetFlowTemplate
    GetFlowTemplate (GetFlowTemplate'),
    newGetFlowTemplate,
    GetFlowTemplateResponse (GetFlowTemplateResponse'),
    newGetFlowTemplateResponse,

    -- ** UpdateSystemTemplate
    UpdateSystemTemplate (UpdateSystemTemplate'),
    newUpdateSystemTemplate,
    UpdateSystemTemplateResponse (UpdateSystemTemplateResponse'),
    newUpdateSystemTemplateResponse,

    -- ** DeleteSystemTemplate
    DeleteSystemTemplate (DeleteSystemTemplate'),
    newDeleteSystemTemplate,
    DeleteSystemTemplateResponse (DeleteSystemTemplateResponse'),
    newDeleteSystemTemplateResponse,

    -- ** DeprecateFlowTemplate
    DeprecateFlowTemplate (DeprecateFlowTemplate'),
    newDeprecateFlowTemplate,
    DeprecateFlowTemplateResponse (DeprecateFlowTemplateResponse'),
    newDeprecateFlowTemplateResponse,

    -- ** DeploySystemInstance
    DeploySystemInstance (DeploySystemInstance'),
    newDeploySystemInstance,
    DeploySystemInstanceResponse (DeploySystemInstanceResponse'),
    newDeploySystemInstanceResponse,

    -- ** SearchFlowTemplates (Paginated)
    SearchFlowTemplates (SearchFlowTemplates'),
    newSearchFlowTemplates,
    SearchFlowTemplatesResponse (SearchFlowTemplatesResponse'),
    newSearchFlowTemplatesResponse,

    -- ** DeleteNamespace
    DeleteNamespace (DeleteNamespace'),
    newDeleteNamespace,
    DeleteNamespaceResponse (DeleteNamespaceResponse'),
    newDeleteNamespaceResponse,

    -- ** GetSystemInstance
    GetSystemInstance (GetSystemInstance'),
    newGetSystemInstance,
    GetSystemInstanceResponse (GetSystemInstanceResponse'),
    newGetSystemInstanceResponse,

    -- ** ListFlowExecutionMessages (Paginated)
    ListFlowExecutionMessages (ListFlowExecutionMessages'),
    newListFlowExecutionMessages,
    ListFlowExecutionMessagesResponse (ListFlowExecutionMessagesResponse'),
    newListFlowExecutionMessagesResponse,

    -- ** ListTagsForResource (Paginated)
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** SearchFlowExecutions (Paginated)
    SearchFlowExecutions (SearchFlowExecutions'),
    newSearchFlowExecutions,
    SearchFlowExecutionsResponse (SearchFlowExecutionsResponse'),
    newSearchFlowExecutionsResponse,

    -- ** DeleteSystemInstance
    DeleteSystemInstance (DeleteSystemInstance'),
    newDeleteSystemInstance,
    DeleteSystemInstanceResponse (DeleteSystemInstanceResponse'),
    newDeleteSystemInstanceResponse,

    -- ** CreateSystemInstance
    CreateSystemInstance (CreateSystemInstance'),
    newCreateSystemInstance,
    CreateSystemInstanceResponse (CreateSystemInstanceResponse'),
    newCreateSystemInstanceResponse,

    -- ** DeprecateSystemTemplate
    DeprecateSystemTemplate (DeprecateSystemTemplate'),
    newDeprecateSystemTemplate,
    DeprecateSystemTemplateResponse (DeprecateSystemTemplateResponse'),
    newDeprecateSystemTemplateResponse,

    -- ** GetSystemTemplateRevisions (Paginated)
    GetSystemTemplateRevisions (GetSystemTemplateRevisions'),
    newGetSystemTemplateRevisions,
    GetSystemTemplateRevisionsResponse (GetSystemTemplateRevisionsResponse'),
    newGetSystemTemplateRevisionsResponse,

    -- ** SearchEntities (Paginated)
    SearchEntities (SearchEntities'),
    newSearchEntities,
    SearchEntitiesResponse (SearchEntitiesResponse'),
    newSearchEntitiesResponse,

    -- ** DeleteFlowTemplate
    DeleteFlowTemplate (DeleteFlowTemplate'),
    newDeleteFlowTemplate,
    DeleteFlowTemplateResponse (DeleteFlowTemplateResponse'),
    newDeleteFlowTemplateResponse,

    -- ** UpdateFlowTemplate
    UpdateFlowTemplate (UpdateFlowTemplate'),
    newUpdateFlowTemplate,
    UpdateFlowTemplateResponse (UpdateFlowTemplateResponse'),
    newUpdateFlowTemplateResponse,

    -- ** GetSystemTemplate
    GetSystemTemplate (GetSystemTemplate'),
    newGetSystemTemplate,
    GetSystemTemplateResponse (GetSystemTemplateResponse'),
    newGetSystemTemplateResponse,

    -- ** SearchSystemInstances (Paginated)
    SearchSystemInstances (SearchSystemInstances'),
    newSearchSystemInstances,
    SearchSystemInstancesResponse (SearchSystemInstancesResponse'),
    newSearchSystemInstancesResponse,

    -- ** GetUploadStatus
    GetUploadStatus (GetUploadStatus'),
    newGetUploadStatus,
    GetUploadStatusResponse (GetUploadStatusResponse'),
    newGetUploadStatusResponse,

    -- ** CreateSystemTemplate
    CreateSystemTemplate (CreateSystemTemplate'),
    newCreateSystemTemplate,
    CreateSystemTemplateResponse (CreateSystemTemplateResponse'),
    newCreateSystemTemplateResponse,

    -- ** UndeploySystemInstance
    UndeploySystemInstance (UndeploySystemInstance'),
    newUndeploySystemInstance,
    UndeploySystemInstanceResponse (UndeploySystemInstanceResponse'),
    newUndeploySystemInstanceResponse,

    -- ** GetFlowTemplateRevisions (Paginated)
    GetFlowTemplateRevisions (GetFlowTemplateRevisions'),
    newGetFlowTemplateRevisions,
    GetFlowTemplateRevisionsResponse (GetFlowTemplateRevisionsResponse'),
    newGetFlowTemplateRevisionsResponse,

    -- ** GetNamespaceDeletionStatus
    GetNamespaceDeletionStatus (GetNamespaceDeletionStatus'),
    newGetNamespaceDeletionStatus,
    GetNamespaceDeletionStatusResponse (GetNamespaceDeletionStatusResponse'),
    newGetNamespaceDeletionStatusResponse,

    -- ** AssociateEntityToThing
    AssociateEntityToThing (AssociateEntityToThing'),
    newAssociateEntityToThing,
    AssociateEntityToThingResponse (AssociateEntityToThingResponse'),
    newAssociateEntityToThingResponse,

    -- ** SearchSystemTemplates (Paginated)
    SearchSystemTemplates (SearchSystemTemplates'),
    newSearchSystemTemplates,
    SearchSystemTemplatesResponse (SearchSystemTemplatesResponse'),
    newSearchSystemTemplatesResponse,

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

    -- ** GetEntities
    GetEntities (GetEntities'),
    newGetEntities,
    GetEntitiesResponse (GetEntitiesResponse'),
    newGetEntitiesResponse,

    -- ** DescribeNamespace
    DescribeNamespace (DescribeNamespace'),
    newDescribeNamespace,
    DescribeNamespaceResponse (DescribeNamespaceResponse'),
    newDescribeNamespaceResponse,

    -- ** CreateFlowTemplate
    CreateFlowTemplate (CreateFlowTemplate'),
    newCreateFlowTemplate,
    CreateFlowTemplateResponse (CreateFlowTemplateResponse'),
    newCreateFlowTemplateResponse,

    -- ** UploadEntityDefinitions
    UploadEntityDefinitions (UploadEntityDefinitions'),
    newUploadEntityDefinitions,
    UploadEntityDefinitionsResponse (UploadEntityDefinitionsResponse'),
    newUploadEntityDefinitionsResponse,

    -- ** DissociateEntityFromThing
    DissociateEntityFromThing (DissociateEntityFromThing'),
    newDissociateEntityFromThing,
    DissociateEntityFromThingResponse (DissociateEntityFromThingResponse'),
    newDissociateEntityFromThingResponse,

    -- ** SearchThings (Paginated)
    SearchThings (SearchThings'),
    newSearchThings,
    SearchThingsResponse (SearchThingsResponse'),
    newSearchThingsResponse,

    -- * Types

    -- ** DefinitionLanguage
    DefinitionLanguage (..),

    -- ** DeploymentTarget
    DeploymentTarget (..),

    -- ** EntityFilterName
    EntityFilterName (..),

    -- ** EntityType
    EntityType (..),

    -- ** FlowExecutionEventType
    FlowExecutionEventType (..),

    -- ** FlowExecutionStatus
    FlowExecutionStatus (..),

    -- ** FlowTemplateFilterName
    FlowTemplateFilterName (..),

    -- ** NamespaceDeletionStatus
    NamespaceDeletionStatus (..),

    -- ** NamespaceDeletionStatusErrorCodes
    NamespaceDeletionStatusErrorCodes (..),

    -- ** SystemInstanceDeploymentStatus
    SystemInstanceDeploymentStatus (..),

    -- ** SystemInstanceFilterName
    SystemInstanceFilterName (..),

    -- ** SystemTemplateFilterName
    SystemTemplateFilterName (..),

    -- ** UploadStatus
    UploadStatus (..),

    -- ** DefinitionDocument
    DefinitionDocument (DefinitionDocument'),
    newDefinitionDocument,

    -- ** DependencyRevision
    DependencyRevision (DependencyRevision'),
    newDependencyRevision,

    -- ** EntityDescription
    EntityDescription (EntityDescription'),
    newEntityDescription,

    -- ** EntityFilter
    EntityFilter (EntityFilter'),
    newEntityFilter,

    -- ** FlowExecutionMessage
    FlowExecutionMessage (FlowExecutionMessage'),
    newFlowExecutionMessage,

    -- ** FlowExecutionSummary
    FlowExecutionSummary (FlowExecutionSummary'),
    newFlowExecutionSummary,

    -- ** FlowTemplateDescription
    FlowTemplateDescription (FlowTemplateDescription'),
    newFlowTemplateDescription,

    -- ** FlowTemplateFilter
    FlowTemplateFilter (FlowTemplateFilter'),
    newFlowTemplateFilter,

    -- ** FlowTemplateSummary
    FlowTemplateSummary (FlowTemplateSummary'),
    newFlowTemplateSummary,

    -- ** MetricsConfiguration
    MetricsConfiguration (MetricsConfiguration'),
    newMetricsConfiguration,

    -- ** SystemInstanceDescription
    SystemInstanceDescription (SystemInstanceDescription'),
    newSystemInstanceDescription,

    -- ** SystemInstanceFilter
    SystemInstanceFilter (SystemInstanceFilter'),
    newSystemInstanceFilter,

    -- ** SystemInstanceSummary
    SystemInstanceSummary (SystemInstanceSummary'),
    newSystemInstanceSummary,

    -- ** SystemTemplateDescription
    SystemTemplateDescription (SystemTemplateDescription'),
    newSystemTemplateDescription,

    -- ** SystemTemplateFilter
    SystemTemplateFilter (SystemTemplateFilter'),
    newSystemTemplateFilter,

    -- ** SystemTemplateSummary
    SystemTemplateSummary (SystemTemplateSummary'),
    newSystemTemplateSummary,

    -- ** Tag
    Tag (Tag'),
    newTag,

    -- ** Thing
    Thing (Thing'),
    newThing,
  )
where

import Amazonka.IoTThingsGraph.AssociateEntityToThing
import Amazonka.IoTThingsGraph.CreateFlowTemplate
import Amazonka.IoTThingsGraph.CreateSystemInstance
import Amazonka.IoTThingsGraph.CreateSystemTemplate
import Amazonka.IoTThingsGraph.DeleteFlowTemplate
import Amazonka.IoTThingsGraph.DeleteNamespace
import Amazonka.IoTThingsGraph.DeleteSystemInstance
import Amazonka.IoTThingsGraph.DeleteSystemTemplate
import Amazonka.IoTThingsGraph.DeploySystemInstance
import Amazonka.IoTThingsGraph.DeprecateFlowTemplate
import Amazonka.IoTThingsGraph.DeprecateSystemTemplate
import Amazonka.IoTThingsGraph.DescribeNamespace
import Amazonka.IoTThingsGraph.DissociateEntityFromThing
import Amazonka.IoTThingsGraph.GetEntities
import Amazonka.IoTThingsGraph.GetFlowTemplate
import Amazonka.IoTThingsGraph.GetFlowTemplateRevisions
import Amazonka.IoTThingsGraph.GetNamespaceDeletionStatus
import Amazonka.IoTThingsGraph.GetSystemInstance
import Amazonka.IoTThingsGraph.GetSystemTemplate
import Amazonka.IoTThingsGraph.GetSystemTemplateRevisions
import Amazonka.IoTThingsGraph.GetUploadStatus
import Amazonka.IoTThingsGraph.Lens
import Amazonka.IoTThingsGraph.ListFlowExecutionMessages
import Amazonka.IoTThingsGraph.ListTagsForResource
import Amazonka.IoTThingsGraph.SearchEntities
import Amazonka.IoTThingsGraph.SearchFlowExecutions
import Amazonka.IoTThingsGraph.SearchFlowTemplates
import Amazonka.IoTThingsGraph.SearchSystemInstances
import Amazonka.IoTThingsGraph.SearchSystemTemplates
import Amazonka.IoTThingsGraph.SearchThings
import Amazonka.IoTThingsGraph.TagResource
import Amazonka.IoTThingsGraph.Types
import Amazonka.IoTThingsGraph.UndeploySystemInstance
import Amazonka.IoTThingsGraph.UntagResource
import Amazonka.IoTThingsGraph.UpdateFlowTemplate
import Amazonka.IoTThingsGraph.UpdateSystemTemplate
import Amazonka.IoTThingsGraph.UploadEntityDefinitions
import Amazonka.IoTThingsGraph.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'IoTThingsGraph'.

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
