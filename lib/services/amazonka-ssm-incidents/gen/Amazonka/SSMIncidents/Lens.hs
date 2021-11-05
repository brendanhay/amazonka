{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.SSMIncidents.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSMIncidents.Lens
  ( -- * Operations

    -- ** DeleteReplicationSet
    deleteReplicationSet_arn,
    deleteReplicationSetResponse_httpStatus,

    -- ** UpdateReplicationSet
    updateReplicationSet_clientToken,
    updateReplicationSet_actions,
    updateReplicationSet_arn,
    updateReplicationSetResponse_httpStatus,

    -- ** ListReplicationSets
    listReplicationSets_nextToken,
    listReplicationSets_maxResults,
    listReplicationSetsResponse_nextToken,
    listReplicationSetsResponse_httpStatus,
    listReplicationSetsResponse_replicationSetArns,

    -- ** UpdateIncidentRecord
    updateIncidentRecord_summary,
    updateIncidentRecord_status,
    updateIncidentRecord_notificationTargets,
    updateIncidentRecord_clientToken,
    updateIncidentRecord_impact,
    updateIncidentRecord_chatChannel,
    updateIncidentRecord_title,
    updateIncidentRecord_arn,
    updateIncidentRecordResponse_httpStatus,

    -- ** DeleteIncidentRecord
    deleteIncidentRecord_arn,
    deleteIncidentRecordResponse_httpStatus,

    -- ** CreateReplicationSet
    createReplicationSet_clientToken,
    createReplicationSet_regions,
    createReplicationSetResponse_httpStatus,
    createReplicationSetResponse_arn,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_httpStatus,
    listTagsForResourceResponse_tags,

    -- ** GetResourcePolicies
    getResourcePolicies_nextToken,
    getResourcePolicies_maxResults,
    getResourcePolicies_resourceArn,
    getResourcePoliciesResponse_nextToken,
    getResourcePoliciesResponse_httpStatus,
    getResourcePoliciesResponse_resourcePolicies,

    -- ** GetIncidentRecord
    getIncidentRecord_arn,
    getIncidentRecordResponse_httpStatus,
    getIncidentRecordResponse_incidentRecord,

    -- ** GetReplicationSet
    getReplicationSet_arn,
    getReplicationSetResponse_httpStatus,
    getReplicationSetResponse_replicationSet,

    -- ** ListRelatedItems
    listRelatedItems_nextToken,
    listRelatedItems_maxResults,
    listRelatedItems_incidentRecordArn,
    listRelatedItemsResponse_nextToken,
    listRelatedItemsResponse_httpStatus,
    listRelatedItemsResponse_relatedItems,

    -- ** UpdateDeletionProtection
    updateDeletionProtection_clientToken,
    updateDeletionProtection_arn,
    updateDeletionProtection_deletionProtected,
    updateDeletionProtectionResponse_httpStatus,

    -- ** GetResponsePlan
    getResponsePlan_arn,
    getResponsePlanResponse_actions,
    getResponsePlanResponse_displayName,
    getResponsePlanResponse_chatChannel,
    getResponsePlanResponse_engagements,
    getResponsePlanResponse_httpStatus,
    getResponsePlanResponse_arn,
    getResponsePlanResponse_incidentTemplate,
    getResponsePlanResponse_name,

    -- ** CreateResponsePlan
    createResponsePlan_clientToken,
    createResponsePlan_actions,
    createResponsePlan_displayName,
    createResponsePlan_chatChannel,
    createResponsePlan_engagements,
    createResponsePlan_tags,
    createResponsePlan_incidentTemplate,
    createResponsePlan_name,
    createResponsePlanResponse_httpStatus,
    createResponsePlanResponse_arn,

    -- ** ListIncidentRecords
    listIncidentRecords_filters,
    listIncidentRecords_nextToken,
    listIncidentRecords_maxResults,
    listIncidentRecordsResponse_nextToken,
    listIncidentRecordsResponse_httpStatus,
    listIncidentRecordsResponse_incidentRecordSummaries,

    -- ** UpdateRelatedItems
    updateRelatedItems_clientToken,
    updateRelatedItems_incidentRecordArn,
    updateRelatedItems_relatedItemsUpdate,
    updateRelatedItemsResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** PutResourcePolicy
    putResourcePolicy_policy,
    putResourcePolicy_resourceArn,
    putResourcePolicyResponse_httpStatus,
    putResourcePolicyResponse_policyId,

    -- ** DeleteResourcePolicy
    deleteResourcePolicy_policyId,
    deleteResourcePolicy_resourceArn,
    deleteResourcePolicyResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** CreateTimelineEvent
    createTimelineEvent_clientToken,
    createTimelineEvent_eventData,
    createTimelineEvent_eventTime,
    createTimelineEvent_eventType,
    createTimelineEvent_incidentRecordArn,
    createTimelineEventResponse_httpStatus,
    createTimelineEventResponse_eventId,
    createTimelineEventResponse_incidentRecordArn,

    -- ** ListTimelineEvents
    listTimelineEvents_filters,
    listTimelineEvents_sortOrder,
    listTimelineEvents_nextToken,
    listTimelineEvents_maxResults,
    listTimelineEvents_sortBy,
    listTimelineEvents_incidentRecordArn,
    listTimelineEventsResponse_nextToken,
    listTimelineEventsResponse_httpStatus,
    listTimelineEventsResponse_eventSummaries,

    -- ** StartIncident
    startIncident_clientToken,
    startIncident_triggerDetails,
    startIncident_relatedItems,
    startIncident_impact,
    startIncident_title,
    startIncident_responsePlanArn,
    startIncidentResponse_httpStatus,
    startIncidentResponse_incidentRecordArn,

    -- ** DeleteTimelineEvent
    deleteTimelineEvent_eventId,
    deleteTimelineEvent_incidentRecordArn,
    deleteTimelineEventResponse_httpStatus,

    -- ** UpdateTimelineEvent
    updateTimelineEvent_eventData,
    updateTimelineEvent_clientToken,
    updateTimelineEvent_eventTime,
    updateTimelineEvent_eventType,
    updateTimelineEvent_eventId,
    updateTimelineEvent_incidentRecordArn,
    updateTimelineEventResponse_httpStatus,

    -- ** ListResponsePlans
    listResponsePlans_nextToken,
    listResponsePlans_maxResults,
    listResponsePlansResponse_nextToken,
    listResponsePlansResponse_httpStatus,
    listResponsePlansResponse_responsePlanSummaries,

    -- ** GetTimelineEvent
    getTimelineEvent_eventId,
    getTimelineEvent_incidentRecordArn,
    getTimelineEventResponse_httpStatus,
    getTimelineEventResponse_event,

    -- ** UpdateResponsePlan
    updateResponsePlan_incidentTemplateImpact,
    updateResponsePlan_clientToken,
    updateResponsePlan_actions,
    updateResponsePlan_incidentTemplateSummary,
    updateResponsePlan_displayName,
    updateResponsePlan_chatChannel,
    updateResponsePlan_incidentTemplateDedupeString,
    updateResponsePlan_incidentTemplateTitle,
    updateResponsePlan_engagements,
    updateResponsePlan_incidentTemplateNotificationTargets,
    updateResponsePlan_arn,
    updateResponsePlanResponse_httpStatus,

    -- ** DeleteResponsePlan
    deleteResponsePlan_arn,
    deleteResponsePlanResponse_httpStatus,

    -- * Types

    -- ** Action
    action_ssmAutomation,

    -- ** AddRegionAction
    addRegionAction_sseKmsKeyId,
    addRegionAction_regionName,

    -- ** AttributeValueList
    attributeValueList_stringValues,
    attributeValueList_integerValues,

    -- ** AutomationExecution
    automationExecution_ssmExecutionArn,

    -- ** ChatChannel
    chatChannel_empty,
    chatChannel_chatbotSns,

    -- ** Condition
    condition_after,
    condition_equals,
    condition_before,

    -- ** DeleteRegionAction
    deleteRegionAction_regionName,

    -- ** EmptyChatChannel

    -- ** EventSummary
    eventSummary_eventId,
    eventSummary_eventTime,
    eventSummary_eventType,
    eventSummary_eventUpdatedTime,
    eventSummary_incidentRecordArn,

    -- ** Filter
    filter_condition,
    filter_key,

    -- ** IncidentRecord
    incidentRecord_summary,
    incidentRecord_notificationTargets,
    incidentRecord_resolvedTime,
    incidentRecord_chatChannel,
    incidentRecord_automationExecutions,
    incidentRecord_arn,
    incidentRecord_creationTime,
    incidentRecord_dedupeString,
    incidentRecord_impact,
    incidentRecord_incidentRecordSource,
    incidentRecord_lastModifiedBy,
    incidentRecord_lastModifiedTime,
    incidentRecord_status,
    incidentRecord_title,

    -- ** IncidentRecordSource
    incidentRecordSource_invokedBy,
    incidentRecordSource_resourceArn,
    incidentRecordSource_createdBy,
    incidentRecordSource_source,

    -- ** IncidentRecordSummary
    incidentRecordSummary_resolvedTime,
    incidentRecordSummary_arn,
    incidentRecordSummary_creationTime,
    incidentRecordSummary_impact,
    incidentRecordSummary_incidentRecordSource,
    incidentRecordSummary_status,
    incidentRecordSummary_title,

    -- ** IncidentTemplate
    incidentTemplate_summary,
    incidentTemplate_notificationTargets,
    incidentTemplate_dedupeString,
    incidentTemplate_impact,
    incidentTemplate_title,

    -- ** ItemIdentifier
    itemIdentifier_type,
    itemIdentifier_value,

    -- ** ItemValue
    itemValue_arn,
    itemValue_url,
    itemValue_metricDefinition,

    -- ** NotificationTargetItem
    notificationTargetItem_snsTopicArn,

    -- ** RegionInfo
    regionInfo_statusMessage,
    regionInfo_sseKmsKeyId,
    regionInfo_status,
    regionInfo_statusUpdateDateTime,

    -- ** RegionMapInputValue
    regionMapInputValue_sseKmsKeyId,

    -- ** RelatedItem
    relatedItem_title,
    relatedItem_identifier,

    -- ** RelatedItemsUpdate
    relatedItemsUpdate_itemToRemove,
    relatedItemsUpdate_itemToAdd,

    -- ** ReplicationSet
    replicationSet_arn,
    replicationSet_createdBy,
    replicationSet_createdTime,
    replicationSet_deletionProtected,
    replicationSet_lastModifiedBy,
    replicationSet_lastModifiedTime,
    replicationSet_regionMap,
    replicationSet_status,

    -- ** ResourcePolicy
    resourcePolicy_policyDocument,
    resourcePolicy_policyId,
    resourcePolicy_ramResourceShareRegion,

    -- ** ResponsePlanSummary
    responsePlanSummary_displayName,
    responsePlanSummary_arn,
    responsePlanSummary_name,

    -- ** SsmAutomation
    ssmAutomation_targetAccount,
    ssmAutomation_parameters,
    ssmAutomation_documentVersion,
    ssmAutomation_documentName,
    ssmAutomation_roleArn,

    -- ** TimelineEvent
    timelineEvent_eventData,
    timelineEvent_eventId,
    timelineEvent_eventTime,
    timelineEvent_eventType,
    timelineEvent_eventUpdatedTime,
    timelineEvent_incidentRecordArn,

    -- ** TriggerDetails
    triggerDetails_rawData,
    triggerDetails_triggerArn,
    triggerDetails_source,
    triggerDetails_timestamp,

    -- ** UpdateReplicationSetAction
    updateReplicationSetAction_addRegionAction,
    updateReplicationSetAction_deleteRegionAction,
  )
where

import Amazonka.SSMIncidents.CreateReplicationSet
import Amazonka.SSMIncidents.CreateResponsePlan
import Amazonka.SSMIncidents.CreateTimelineEvent
import Amazonka.SSMIncidents.DeleteIncidentRecord
import Amazonka.SSMIncidents.DeleteReplicationSet
import Amazonka.SSMIncidents.DeleteResourcePolicy
import Amazonka.SSMIncidents.DeleteResponsePlan
import Amazonka.SSMIncidents.DeleteTimelineEvent
import Amazonka.SSMIncidents.GetIncidentRecord
import Amazonka.SSMIncidents.GetReplicationSet
import Amazonka.SSMIncidents.GetResourcePolicies
import Amazonka.SSMIncidents.GetResponsePlan
import Amazonka.SSMIncidents.GetTimelineEvent
import Amazonka.SSMIncidents.ListIncidentRecords
import Amazonka.SSMIncidents.ListRelatedItems
import Amazonka.SSMIncidents.ListReplicationSets
import Amazonka.SSMIncidents.ListResponsePlans
import Amazonka.SSMIncidents.ListTagsForResource
import Amazonka.SSMIncidents.ListTimelineEvents
import Amazonka.SSMIncidents.PutResourcePolicy
import Amazonka.SSMIncidents.StartIncident
import Amazonka.SSMIncidents.TagResource
import Amazonka.SSMIncidents.Types.Action
import Amazonka.SSMIncidents.Types.AddRegionAction
import Amazonka.SSMIncidents.Types.AttributeValueList
import Amazonka.SSMIncidents.Types.AutomationExecution
import Amazonka.SSMIncidents.Types.ChatChannel
import Amazonka.SSMIncidents.Types.Condition
import Amazonka.SSMIncidents.Types.DeleteRegionAction
import Amazonka.SSMIncidents.Types.EmptyChatChannel
import Amazonka.SSMIncidents.Types.EventSummary
import Amazonka.SSMIncidents.Types.Filter
import Amazonka.SSMIncidents.Types.IncidentRecord
import Amazonka.SSMIncidents.Types.IncidentRecordSource
import Amazonka.SSMIncidents.Types.IncidentRecordSummary
import Amazonka.SSMIncidents.Types.IncidentTemplate
import Amazonka.SSMIncidents.Types.ItemIdentifier
import Amazonka.SSMIncidents.Types.ItemValue
import Amazonka.SSMIncidents.Types.NotificationTargetItem
import Amazonka.SSMIncidents.Types.RegionInfo
import Amazonka.SSMIncidents.Types.RegionMapInputValue
import Amazonka.SSMIncidents.Types.RelatedItem
import Amazonka.SSMIncidents.Types.RelatedItemsUpdate
import Amazonka.SSMIncidents.Types.ReplicationSet
import Amazonka.SSMIncidents.Types.ResourcePolicy
import Amazonka.SSMIncidents.Types.ResponsePlanSummary
import Amazonka.SSMIncidents.Types.SsmAutomation
import Amazonka.SSMIncidents.Types.TimelineEvent
import Amazonka.SSMIncidents.Types.TriggerDetails
import Amazonka.SSMIncidents.Types.UpdateReplicationSetAction
import Amazonka.SSMIncidents.UntagResource
import Amazonka.SSMIncidents.UpdateDeletionProtection
import Amazonka.SSMIncidents.UpdateIncidentRecord
import Amazonka.SSMIncidents.UpdateRelatedItems
import Amazonka.SSMIncidents.UpdateReplicationSet
import Amazonka.SSMIncidents.UpdateResponsePlan
import Amazonka.SSMIncidents.UpdateTimelineEvent
