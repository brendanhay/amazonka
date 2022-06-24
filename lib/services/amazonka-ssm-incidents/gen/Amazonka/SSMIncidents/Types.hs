{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.SSMIncidents.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSMIncidents.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _AccessDeniedException,
    _InternalServerException,
    _ServiceQuotaExceededException,
    _ResourceNotFoundException,
    _ConflictException,
    _ThrottlingException,
    _ValidationException,

    -- * IncidentRecordStatus
    IncidentRecordStatus (..),

    -- * ItemType
    ItemType (..),

    -- * RegionStatus
    RegionStatus (..),

    -- * ReplicationSetStatus
    ReplicationSetStatus (..),

    -- * SortOrder
    SortOrder (..),

    -- * SsmTargetAccount
    SsmTargetAccount (..),

    -- * TimelineEventSort
    TimelineEventSort (..),

    -- * Action
    Action (..),
    newAction,
    action_ssmAutomation,

    -- * AddRegionAction
    AddRegionAction (..),
    newAddRegionAction,
    addRegionAction_sseKmsKeyId,
    addRegionAction_regionName,

    -- * AttributeValueList
    AttributeValueList (..),
    newAttributeValueList,
    attributeValueList_integerValues,
    attributeValueList_stringValues,

    -- * AutomationExecution
    AutomationExecution (..),
    newAutomationExecution,
    automationExecution_ssmExecutionArn,

    -- * ChatChannel
    ChatChannel (..),
    newChatChannel,
    chatChannel_empty,
    chatChannel_chatbotSns,

    -- * Condition
    Condition (..),
    newCondition,
    condition_equals,
    condition_after,
    condition_before,

    -- * DeleteRegionAction
    DeleteRegionAction (..),
    newDeleteRegionAction,
    deleteRegionAction_regionName,

    -- * EmptyChatChannel
    EmptyChatChannel (..),
    newEmptyChatChannel,

    -- * EventSummary
    EventSummary (..),
    newEventSummary,
    eventSummary_eventId,
    eventSummary_eventTime,
    eventSummary_eventType,
    eventSummary_eventUpdatedTime,
    eventSummary_incidentRecordArn,

    -- * Filter
    Filter (..),
    newFilter,
    filter_condition,
    filter_key,

    -- * IncidentRecord
    IncidentRecord (..),
    newIncidentRecord,
    incidentRecord_chatChannel,
    incidentRecord_summary,
    incidentRecord_automationExecutions,
    incidentRecord_resolvedTime,
    incidentRecord_notificationTargets,
    incidentRecord_arn,
    incidentRecord_creationTime,
    incidentRecord_dedupeString,
    incidentRecord_impact,
    incidentRecord_incidentRecordSource,
    incidentRecord_lastModifiedBy,
    incidentRecord_lastModifiedTime,
    incidentRecord_status,
    incidentRecord_title,

    -- * IncidentRecordSource
    IncidentRecordSource (..),
    newIncidentRecordSource,
    incidentRecordSource_invokedBy,
    incidentRecordSource_resourceArn,
    incidentRecordSource_createdBy,
    incidentRecordSource_source,

    -- * IncidentRecordSummary
    IncidentRecordSummary (..),
    newIncidentRecordSummary,
    incidentRecordSummary_resolvedTime,
    incidentRecordSummary_arn,
    incidentRecordSummary_creationTime,
    incidentRecordSummary_impact,
    incidentRecordSummary_incidentRecordSource,
    incidentRecordSummary_status,
    incidentRecordSummary_title,

    -- * IncidentTemplate
    IncidentTemplate (..),
    newIncidentTemplate,
    incidentTemplate_summary,
    incidentTemplate_notificationTargets,
    incidentTemplate_dedupeString,
    incidentTemplate_impact,
    incidentTemplate_title,

    -- * ItemIdentifier
    ItemIdentifier (..),
    newItemIdentifier,
    itemIdentifier_type,
    itemIdentifier_value,

    -- * ItemValue
    ItemValue (..),
    newItemValue,
    itemValue_arn,
    itemValue_url,
    itemValue_metricDefinition,

    -- * NotificationTargetItem
    NotificationTargetItem (..),
    newNotificationTargetItem,
    notificationTargetItem_snsTopicArn,

    -- * RegionInfo
    RegionInfo (..),
    newRegionInfo,
    regionInfo_sseKmsKeyId,
    regionInfo_statusMessage,
    regionInfo_status,
    regionInfo_statusUpdateDateTime,

    -- * RegionMapInputValue
    RegionMapInputValue (..),
    newRegionMapInputValue,
    regionMapInputValue_sseKmsKeyId,

    -- * RelatedItem
    RelatedItem (..),
    newRelatedItem,
    relatedItem_title,
    relatedItem_identifier,

    -- * RelatedItemsUpdate
    RelatedItemsUpdate (..),
    newRelatedItemsUpdate,
    relatedItemsUpdate_itemToRemove,
    relatedItemsUpdate_itemToAdd,

    -- * ReplicationSet
    ReplicationSet (..),
    newReplicationSet,
    replicationSet_arn,
    replicationSet_createdBy,
    replicationSet_createdTime,
    replicationSet_deletionProtected,
    replicationSet_lastModifiedBy,
    replicationSet_lastModifiedTime,
    replicationSet_regionMap,
    replicationSet_status,

    -- * ResourcePolicy
    ResourcePolicy (..),
    newResourcePolicy,
    resourcePolicy_policyDocument,
    resourcePolicy_policyId,
    resourcePolicy_ramResourceShareRegion,

    -- * ResponsePlanSummary
    ResponsePlanSummary (..),
    newResponsePlanSummary,
    responsePlanSummary_displayName,
    responsePlanSummary_arn,
    responsePlanSummary_name,

    -- * SsmAutomation
    SsmAutomation (..),
    newSsmAutomation,
    ssmAutomation_targetAccount,
    ssmAutomation_parameters,
    ssmAutomation_documentVersion,
    ssmAutomation_documentName,
    ssmAutomation_roleArn,

    -- * TimelineEvent
    TimelineEvent (..),
    newTimelineEvent,
    timelineEvent_eventData,
    timelineEvent_eventId,
    timelineEvent_eventTime,
    timelineEvent_eventType,
    timelineEvent_eventUpdatedTime,
    timelineEvent_incidentRecordArn,

    -- * TriggerDetails
    TriggerDetails (..),
    newTriggerDetails,
    triggerDetails_triggerArn,
    triggerDetails_rawData,
    triggerDetails_source,
    triggerDetails_timestamp,

    -- * UpdateReplicationSetAction
    UpdateReplicationSetAction (..),
    newUpdateReplicationSetAction,
    updateReplicationSetAction_deleteRegionAction,
    updateReplicationSetAction_addRegionAction,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
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
import Amazonka.SSMIncidents.Types.IncidentRecordStatus
import Amazonka.SSMIncidents.Types.IncidentRecordSummary
import Amazonka.SSMIncidents.Types.IncidentTemplate
import Amazonka.SSMIncidents.Types.ItemIdentifier
import Amazonka.SSMIncidents.Types.ItemType
import Amazonka.SSMIncidents.Types.ItemValue
import Amazonka.SSMIncidents.Types.NotificationTargetItem
import Amazonka.SSMIncidents.Types.RegionInfo
import Amazonka.SSMIncidents.Types.RegionMapInputValue
import Amazonka.SSMIncidents.Types.RegionStatus
import Amazonka.SSMIncidents.Types.RelatedItem
import Amazonka.SSMIncidents.Types.RelatedItemsUpdate
import Amazonka.SSMIncidents.Types.ReplicationSet
import Amazonka.SSMIncidents.Types.ReplicationSetStatus
import Amazonka.SSMIncidents.Types.ResourcePolicy
import Amazonka.SSMIncidents.Types.ResponsePlanSummary
import Amazonka.SSMIncidents.Types.SortOrder
import Amazonka.SSMIncidents.Types.SsmAutomation
import Amazonka.SSMIncidents.Types.SsmTargetAccount
import Amazonka.SSMIncidents.Types.TimelineEvent
import Amazonka.SSMIncidents.Types.TimelineEventSort
import Amazonka.SSMIncidents.Types.TriggerDetails
import Amazonka.SSMIncidents.Types.UpdateReplicationSetAction
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2018-05-10@ of the Amazon Systems Manager Incident Manager SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core._serviceAbbrev = "SSMIncidents",
      Core._serviceSigner = Sign.v4,
      Core._serviceEndpointPrefix = "ssm-incidents",
      Core._serviceSigningName = "ssm-incidents",
      Core._serviceVersion = "2018-05-10",
      Core._serviceEndpoint =
        Core.defaultEndpoint defaultService,
      Core._serviceTimeout = Prelude.Just 70,
      Core._serviceCheck = Core.statusSuccess,
      Core._serviceError =
        Core.parseJSONError "SSMIncidents",
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

-- | You don\'t have sufficient access to perform this action.
_AccessDeniedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AccessDeniedException =
  Core._MatchServiceError
    defaultService
    "AccessDeniedException"
    Prelude.. Core.hasStatus 403

-- | The request processing has failed because of an unknown error, exception
-- or failure.
_InternalServerException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InternalServerException =
  Core._MatchServiceError
    defaultService
    "InternalServerException"
    Prelude.. Core.hasStatus 500

-- | Request would cause a service quota to be exceeded.
_ServiceQuotaExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ServiceQuotaExceededException =
  Core._MatchServiceError
    defaultService
    "ServiceQuotaExceededException"
    Prelude.. Core.hasStatus 402

-- | Request references a resource which does not exist.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"
    Prelude.. Core.hasStatus 404

-- | Updating or deleting a resource causes an inconsistent state.
_ConflictException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ConflictException =
  Core._MatchServiceError
    defaultService
    "ConflictException"
    Prelude.. Core.hasStatus 409

-- | The request was denied due to request throttling.
_ThrottlingException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ThrottlingException =
  Core._MatchServiceError
    defaultService
    "ThrottlingException"
    Prelude.. Core.hasStatus 429

-- | The input fails to satisfy the constraints specified by an AWS service.
_ValidationException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ValidationException =
  Core._MatchServiceError
    defaultService
    "ValidationException"
    Prelude.. Core.hasStatus 400
