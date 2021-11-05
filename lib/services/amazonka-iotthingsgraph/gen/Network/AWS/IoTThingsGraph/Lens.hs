{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.IoTThingsGraph.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTThingsGraph.Lens
  ( -- * Operations

    -- ** GetFlowTemplate
    getFlowTemplate_revisionNumber,
    getFlowTemplate_id,
    getFlowTemplateResponse_description,
    getFlowTemplateResponse_httpStatus,

    -- ** UpdateSystemTemplate
    updateSystemTemplate_compatibleNamespaceVersion,
    updateSystemTemplate_id,
    updateSystemTemplate_definition,
    updateSystemTemplateResponse_summary,
    updateSystemTemplateResponse_httpStatus,

    -- ** DeleteSystemTemplate
    deleteSystemTemplate_id,
    deleteSystemTemplateResponse_httpStatus,

    -- ** DeprecateFlowTemplate
    deprecateFlowTemplate_id,
    deprecateFlowTemplateResponse_httpStatus,

    -- ** DeploySystemInstance
    deploySystemInstance_id,
    deploySystemInstanceResponse_greengrassDeploymentId,
    deploySystemInstanceResponse_httpStatus,
    deploySystemInstanceResponse_summary,

    -- ** SearchFlowTemplates
    searchFlowTemplates_filters,
    searchFlowTemplates_nextToken,
    searchFlowTemplates_maxResults,
    searchFlowTemplatesResponse_nextToken,
    searchFlowTemplatesResponse_summaries,
    searchFlowTemplatesResponse_httpStatus,

    -- ** DeleteNamespace
    deleteNamespaceResponse_namespaceArn,
    deleteNamespaceResponse_namespaceName,
    deleteNamespaceResponse_httpStatus,

    -- ** GetSystemInstance
    getSystemInstance_id,
    getSystemInstanceResponse_description,
    getSystemInstanceResponse_httpStatus,

    -- ** ListFlowExecutionMessages
    listFlowExecutionMessages_nextToken,
    listFlowExecutionMessages_maxResults,
    listFlowExecutionMessages_flowExecutionId,
    listFlowExecutionMessagesResponse_nextToken,
    listFlowExecutionMessagesResponse_messages,
    listFlowExecutionMessagesResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_nextToken,
    listTagsForResource_maxResults,
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_nextToken,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** SearchFlowExecutions
    searchFlowExecutions_startTime,
    searchFlowExecutions_flowExecutionId,
    searchFlowExecutions_nextToken,
    searchFlowExecutions_endTime,
    searchFlowExecutions_maxResults,
    searchFlowExecutions_systemInstanceId,
    searchFlowExecutionsResponse_nextToken,
    searchFlowExecutionsResponse_summaries,
    searchFlowExecutionsResponse_httpStatus,

    -- ** DeleteSystemInstance
    deleteSystemInstance_id,
    deleteSystemInstanceResponse_httpStatus,

    -- ** CreateSystemInstance
    createSystemInstance_metricsConfiguration,
    createSystemInstance_greengrassGroupName,
    createSystemInstance_flowActionsRoleArn,
    createSystemInstance_s3BucketName,
    createSystemInstance_tags,
    createSystemInstance_definition,
    createSystemInstance_target,
    createSystemInstanceResponse_summary,
    createSystemInstanceResponse_httpStatus,

    -- ** DeprecateSystemTemplate
    deprecateSystemTemplate_id,
    deprecateSystemTemplateResponse_httpStatus,

    -- ** GetSystemTemplateRevisions
    getSystemTemplateRevisions_nextToken,
    getSystemTemplateRevisions_maxResults,
    getSystemTemplateRevisions_id,
    getSystemTemplateRevisionsResponse_nextToken,
    getSystemTemplateRevisionsResponse_summaries,
    getSystemTemplateRevisionsResponse_httpStatus,

    -- ** SearchEntities
    searchEntities_filters,
    searchEntities_namespaceVersion,
    searchEntities_nextToken,
    searchEntities_maxResults,
    searchEntities_entityTypes,
    searchEntitiesResponse_nextToken,
    searchEntitiesResponse_descriptions,
    searchEntitiesResponse_httpStatus,

    -- ** DeleteFlowTemplate
    deleteFlowTemplate_id,
    deleteFlowTemplateResponse_httpStatus,

    -- ** UpdateFlowTemplate
    updateFlowTemplate_compatibleNamespaceVersion,
    updateFlowTemplate_id,
    updateFlowTemplate_definition,
    updateFlowTemplateResponse_summary,
    updateFlowTemplateResponse_httpStatus,

    -- ** GetSystemTemplate
    getSystemTemplate_revisionNumber,
    getSystemTemplate_id,
    getSystemTemplateResponse_description,
    getSystemTemplateResponse_httpStatus,

    -- ** SearchSystemInstances
    searchSystemInstances_filters,
    searchSystemInstances_nextToken,
    searchSystemInstances_maxResults,
    searchSystemInstancesResponse_nextToken,
    searchSystemInstancesResponse_summaries,
    searchSystemInstancesResponse_httpStatus,

    -- ** GetUploadStatus
    getUploadStatus_uploadId,
    getUploadStatusResponse_failureReason,
    getUploadStatusResponse_namespaceArn,
    getUploadStatusResponse_namespaceVersion,
    getUploadStatusResponse_namespaceName,
    getUploadStatusResponse_httpStatus,
    getUploadStatusResponse_uploadId,
    getUploadStatusResponse_uploadStatus,
    getUploadStatusResponse_createdDate,

    -- ** CreateSystemTemplate
    createSystemTemplate_compatibleNamespaceVersion,
    createSystemTemplate_definition,
    createSystemTemplateResponse_summary,
    createSystemTemplateResponse_httpStatus,

    -- ** UndeploySystemInstance
    undeploySystemInstance_id,
    undeploySystemInstanceResponse_summary,
    undeploySystemInstanceResponse_httpStatus,

    -- ** GetFlowTemplateRevisions
    getFlowTemplateRevisions_nextToken,
    getFlowTemplateRevisions_maxResults,
    getFlowTemplateRevisions_id,
    getFlowTemplateRevisionsResponse_nextToken,
    getFlowTemplateRevisionsResponse_summaries,
    getFlowTemplateRevisionsResponse_httpStatus,

    -- ** GetNamespaceDeletionStatus
    getNamespaceDeletionStatusResponse_status,
    getNamespaceDeletionStatusResponse_namespaceArn,
    getNamespaceDeletionStatusResponse_namespaceName,
    getNamespaceDeletionStatusResponse_errorCode,
    getNamespaceDeletionStatusResponse_errorMessage,
    getNamespaceDeletionStatusResponse_httpStatus,

    -- ** AssociateEntityToThing
    associateEntityToThing_namespaceVersion,
    associateEntityToThing_thingName,
    associateEntityToThing_entityId,
    associateEntityToThingResponse_httpStatus,

    -- ** SearchSystemTemplates
    searchSystemTemplates_filters,
    searchSystemTemplates_nextToken,
    searchSystemTemplates_maxResults,
    searchSystemTemplatesResponse_nextToken,
    searchSystemTemplatesResponse_summaries,
    searchSystemTemplatesResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** GetEntities
    getEntities_namespaceVersion,
    getEntities_ids,
    getEntitiesResponse_descriptions,
    getEntitiesResponse_httpStatus,

    -- ** DescribeNamespace
    describeNamespace_namespaceName,
    describeNamespaceResponse_namespaceArn,
    describeNamespaceResponse_trackingNamespaceVersion,
    describeNamespaceResponse_namespaceVersion,
    describeNamespaceResponse_namespaceName,
    describeNamespaceResponse_trackingNamespaceName,
    describeNamespaceResponse_httpStatus,

    -- ** CreateFlowTemplate
    createFlowTemplate_compatibleNamespaceVersion,
    createFlowTemplate_definition,
    createFlowTemplateResponse_summary,
    createFlowTemplateResponse_httpStatus,

    -- ** UploadEntityDefinitions
    uploadEntityDefinitions_syncWithPublicNamespace,
    uploadEntityDefinitions_deprecateExistingEntities,
    uploadEntityDefinitions_document,
    uploadEntityDefinitionsResponse_httpStatus,
    uploadEntityDefinitionsResponse_uploadId,

    -- ** DissociateEntityFromThing
    dissociateEntityFromThing_thingName,
    dissociateEntityFromThing_entityType,
    dissociateEntityFromThingResponse_httpStatus,

    -- ** SearchThings
    searchThings_namespaceVersion,
    searchThings_nextToken,
    searchThings_maxResults,
    searchThings_entityId,
    searchThingsResponse_nextToken,
    searchThingsResponse_things,
    searchThingsResponse_httpStatus,

    -- * Types

    -- ** DefinitionDocument
    definitionDocument_language,
    definitionDocument_text,

    -- ** DependencyRevision
    dependencyRevision_revisionNumber,
    dependencyRevision_id,

    -- ** EntityDescription
    entityDescription_arn,
    entityDescription_createdAt,
    entityDescription_definition,
    entityDescription_id,
    entityDescription_type,

    -- ** EntityFilter
    entityFilter_value,
    entityFilter_name,

    -- ** FlowExecutionMessage
    flowExecutionMessage_payload,
    flowExecutionMessage_eventType,
    flowExecutionMessage_timestamp,
    flowExecutionMessage_messageId,

    -- ** FlowExecutionSummary
    flowExecutionSummary_status,
    flowExecutionSummary_flowTemplateId,
    flowExecutionSummary_createdAt,
    flowExecutionSummary_flowExecutionId,
    flowExecutionSummary_systemInstanceId,
    flowExecutionSummary_updatedAt,

    -- ** FlowTemplateDescription
    flowTemplateDescription_summary,
    flowTemplateDescription_definition,
    flowTemplateDescription_validatedNamespaceVersion,

    -- ** FlowTemplateFilter
    flowTemplateFilter_name,
    flowTemplateFilter_value,

    -- ** FlowTemplateSummary
    flowTemplateSummary_arn,
    flowTemplateSummary_createdAt,
    flowTemplateSummary_revisionNumber,
    flowTemplateSummary_id,

    -- ** MetricsConfiguration
    metricsConfiguration_cloudMetricEnabled,
    metricsConfiguration_metricRuleRoleArn,

    -- ** SystemInstanceDescription
    systemInstanceDescription_summary,
    systemInstanceDescription_metricsConfiguration,
    systemInstanceDescription_validatedDependencyRevisions,
    systemInstanceDescription_definition,
    systemInstanceDescription_validatedNamespaceVersion,
    systemInstanceDescription_flowActionsRoleArn,
    systemInstanceDescription_s3BucketName,

    -- ** SystemInstanceFilter
    systemInstanceFilter_value,
    systemInstanceFilter_name,

    -- ** SystemInstanceSummary
    systemInstanceSummary_status,
    systemInstanceSummary_greengrassGroupName,
    systemInstanceSummary_arn,
    systemInstanceSummary_createdAt,
    systemInstanceSummary_greengrassGroupId,
    systemInstanceSummary_greengrassGroupVersionId,
    systemInstanceSummary_id,
    systemInstanceSummary_updatedAt,
    systemInstanceSummary_target,

    -- ** SystemTemplateDescription
    systemTemplateDescription_summary,
    systemTemplateDescription_definition,
    systemTemplateDescription_validatedNamespaceVersion,

    -- ** SystemTemplateFilter
    systemTemplateFilter_name,
    systemTemplateFilter_value,

    -- ** SystemTemplateSummary
    systemTemplateSummary_arn,
    systemTemplateSummary_createdAt,
    systemTemplateSummary_revisionNumber,
    systemTemplateSummary_id,

    -- ** Tag
    tag_key,
    tag_value,

    -- ** Thing
    thing_thingArn,
    thing_thingName,
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
import Amazonka.IoTThingsGraph.ListFlowExecutionMessages
import Amazonka.IoTThingsGraph.ListTagsForResource
import Amazonka.IoTThingsGraph.SearchEntities
import Amazonka.IoTThingsGraph.SearchFlowExecutions
import Amazonka.IoTThingsGraph.SearchFlowTemplates
import Amazonka.IoTThingsGraph.SearchSystemInstances
import Amazonka.IoTThingsGraph.SearchSystemTemplates
import Amazonka.IoTThingsGraph.SearchThings
import Amazonka.IoTThingsGraph.TagResource
import Amazonka.IoTThingsGraph.Types.DefinitionDocument
import Amazonka.IoTThingsGraph.Types.DependencyRevision
import Amazonka.IoTThingsGraph.Types.EntityDescription
import Amazonka.IoTThingsGraph.Types.EntityFilter
import Amazonka.IoTThingsGraph.Types.FlowExecutionMessage
import Amazonka.IoTThingsGraph.Types.FlowExecutionSummary
import Amazonka.IoTThingsGraph.Types.FlowTemplateDescription
import Amazonka.IoTThingsGraph.Types.FlowTemplateFilter
import Amazonka.IoTThingsGraph.Types.FlowTemplateSummary
import Amazonka.IoTThingsGraph.Types.MetricsConfiguration
import Amazonka.IoTThingsGraph.Types.SystemInstanceDescription
import Amazonka.IoTThingsGraph.Types.SystemInstanceFilter
import Amazonka.IoTThingsGraph.Types.SystemInstanceSummary
import Amazonka.IoTThingsGraph.Types.SystemTemplateDescription
import Amazonka.IoTThingsGraph.Types.SystemTemplateFilter
import Amazonka.IoTThingsGraph.Types.SystemTemplateSummary
import Amazonka.IoTThingsGraph.Types.Tag
import Amazonka.IoTThingsGraph.Types.Thing
import Amazonka.IoTThingsGraph.UndeploySystemInstance
import Amazonka.IoTThingsGraph.UntagResource
import Amazonka.IoTThingsGraph.UpdateFlowTemplate
import Amazonka.IoTThingsGraph.UpdateSystemTemplate
import Amazonka.IoTThingsGraph.UploadEntityDefinitions
