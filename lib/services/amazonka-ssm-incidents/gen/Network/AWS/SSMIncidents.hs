{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.SSMIncidents
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2018-05-10@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- AWS Systems Manager Incident Manager is an incident management console
-- designed to help users mitigate and recover from incidents affecting
-- their AWS-hosted applications. An incident is any unplanned interruption
-- or reduction in quality of services.
--
-- Incident Manager increases incident resolution by notifying responders
-- of impact, highlighting relevant troubleshooting data, and providing
-- collaboration tools to get services back up and running. To achieve the
-- primary goal of reducing the time-to-resolution of critical incidents,
-- Incident Manager automates response plans and enables responder team
-- escalation.
module Amazonka.SSMIncidents
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** ValidationException
    _ValidationException,

    -- ** AccessDeniedException
    _AccessDeniedException,

    -- ** ConflictException
    _ConflictException,

    -- ** ServiceQuotaExceededException
    _ServiceQuotaExceededException,

    -- ** ThrottlingException
    _ThrottlingException,

    -- ** InternalServerException
    _InternalServerException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- * Waiters
    -- $waiters

    -- ** WaitForReplicationSetActive
    newWaitForReplicationSetActive,

    -- ** WaitForReplicationSetDeleted
    newWaitForReplicationSetDeleted,

    -- * Operations
    -- $operations

    -- ** DeleteReplicationSet
    DeleteReplicationSet (DeleteReplicationSet'),
    newDeleteReplicationSet,
    DeleteReplicationSetResponse (DeleteReplicationSetResponse'),
    newDeleteReplicationSetResponse,

    -- ** UpdateReplicationSet
    UpdateReplicationSet (UpdateReplicationSet'),
    newUpdateReplicationSet,
    UpdateReplicationSetResponse (UpdateReplicationSetResponse'),
    newUpdateReplicationSetResponse,

    -- ** ListReplicationSets (Paginated)
    ListReplicationSets (ListReplicationSets'),
    newListReplicationSets,
    ListReplicationSetsResponse (ListReplicationSetsResponse'),
    newListReplicationSetsResponse,

    -- ** UpdateIncidentRecord
    UpdateIncidentRecord (UpdateIncidentRecord'),
    newUpdateIncidentRecord,
    UpdateIncidentRecordResponse (UpdateIncidentRecordResponse'),
    newUpdateIncidentRecordResponse,

    -- ** DeleteIncidentRecord
    DeleteIncidentRecord (DeleteIncidentRecord'),
    newDeleteIncidentRecord,
    DeleteIncidentRecordResponse (DeleteIncidentRecordResponse'),
    newDeleteIncidentRecordResponse,

    -- ** CreateReplicationSet
    CreateReplicationSet (CreateReplicationSet'),
    newCreateReplicationSet,
    CreateReplicationSetResponse (CreateReplicationSetResponse'),
    newCreateReplicationSetResponse,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** GetResourcePolicies (Paginated)
    GetResourcePolicies (GetResourcePolicies'),
    newGetResourcePolicies,
    GetResourcePoliciesResponse (GetResourcePoliciesResponse'),
    newGetResourcePoliciesResponse,

    -- ** GetIncidentRecord
    GetIncidentRecord (GetIncidentRecord'),
    newGetIncidentRecord,
    GetIncidentRecordResponse (GetIncidentRecordResponse'),
    newGetIncidentRecordResponse,

    -- ** GetReplicationSet
    GetReplicationSet (GetReplicationSet'),
    newGetReplicationSet,
    GetReplicationSetResponse (GetReplicationSetResponse'),
    newGetReplicationSetResponse,

    -- ** ListRelatedItems (Paginated)
    ListRelatedItems (ListRelatedItems'),
    newListRelatedItems,
    ListRelatedItemsResponse (ListRelatedItemsResponse'),
    newListRelatedItemsResponse,

    -- ** UpdateDeletionProtection
    UpdateDeletionProtection (UpdateDeletionProtection'),
    newUpdateDeletionProtection,
    UpdateDeletionProtectionResponse (UpdateDeletionProtectionResponse'),
    newUpdateDeletionProtectionResponse,

    -- ** GetResponsePlan
    GetResponsePlan (GetResponsePlan'),
    newGetResponsePlan,
    GetResponsePlanResponse (GetResponsePlanResponse'),
    newGetResponsePlanResponse,

    -- ** CreateResponsePlan
    CreateResponsePlan (CreateResponsePlan'),
    newCreateResponsePlan,
    CreateResponsePlanResponse (CreateResponsePlanResponse'),
    newCreateResponsePlanResponse,

    -- ** ListIncidentRecords (Paginated)
    ListIncidentRecords (ListIncidentRecords'),
    newListIncidentRecords,
    ListIncidentRecordsResponse (ListIncidentRecordsResponse'),
    newListIncidentRecordsResponse,

    -- ** UpdateRelatedItems
    UpdateRelatedItems (UpdateRelatedItems'),
    newUpdateRelatedItems,
    UpdateRelatedItemsResponse (UpdateRelatedItemsResponse'),
    newUpdateRelatedItemsResponse,

    -- ** TagResource
    TagResource (TagResource'),
    newTagResource,
    TagResourceResponse (TagResourceResponse'),
    newTagResourceResponse,

    -- ** PutResourcePolicy
    PutResourcePolicy (PutResourcePolicy'),
    newPutResourcePolicy,
    PutResourcePolicyResponse (PutResourcePolicyResponse'),
    newPutResourcePolicyResponse,

    -- ** DeleteResourcePolicy
    DeleteResourcePolicy (DeleteResourcePolicy'),
    newDeleteResourcePolicy,
    DeleteResourcePolicyResponse (DeleteResourcePolicyResponse'),
    newDeleteResourcePolicyResponse,

    -- ** UntagResource
    UntagResource (UntagResource'),
    newUntagResource,
    UntagResourceResponse (UntagResourceResponse'),
    newUntagResourceResponse,

    -- ** CreateTimelineEvent
    CreateTimelineEvent (CreateTimelineEvent'),
    newCreateTimelineEvent,
    CreateTimelineEventResponse (CreateTimelineEventResponse'),
    newCreateTimelineEventResponse,

    -- ** ListTimelineEvents (Paginated)
    ListTimelineEvents (ListTimelineEvents'),
    newListTimelineEvents,
    ListTimelineEventsResponse (ListTimelineEventsResponse'),
    newListTimelineEventsResponse,

    -- ** StartIncident
    StartIncident (StartIncident'),
    newStartIncident,
    StartIncidentResponse (StartIncidentResponse'),
    newStartIncidentResponse,

    -- ** DeleteTimelineEvent
    DeleteTimelineEvent (DeleteTimelineEvent'),
    newDeleteTimelineEvent,
    DeleteTimelineEventResponse (DeleteTimelineEventResponse'),
    newDeleteTimelineEventResponse,

    -- ** UpdateTimelineEvent
    UpdateTimelineEvent (UpdateTimelineEvent'),
    newUpdateTimelineEvent,
    UpdateTimelineEventResponse (UpdateTimelineEventResponse'),
    newUpdateTimelineEventResponse,

    -- ** ListResponsePlans (Paginated)
    ListResponsePlans (ListResponsePlans'),
    newListResponsePlans,
    ListResponsePlansResponse (ListResponsePlansResponse'),
    newListResponsePlansResponse,

    -- ** GetTimelineEvent
    GetTimelineEvent (GetTimelineEvent'),
    newGetTimelineEvent,
    GetTimelineEventResponse (GetTimelineEventResponse'),
    newGetTimelineEventResponse,

    -- ** UpdateResponsePlan
    UpdateResponsePlan (UpdateResponsePlan'),
    newUpdateResponsePlan,
    UpdateResponsePlanResponse (UpdateResponsePlanResponse'),
    newUpdateResponsePlanResponse,

    -- ** DeleteResponsePlan
    DeleteResponsePlan (DeleteResponsePlan'),
    newDeleteResponsePlan,
    DeleteResponsePlanResponse (DeleteResponsePlanResponse'),
    newDeleteResponsePlanResponse,

    -- * Types

    -- ** IncidentRecordStatus
    IncidentRecordStatus (..),

    -- ** ItemType
    ItemType (..),

    -- ** RegionStatus
    RegionStatus (..),

    -- ** ReplicationSetStatus
    ReplicationSetStatus (..),

    -- ** SortOrder
    SortOrder (..),

    -- ** SsmTargetAccount
    SsmTargetAccount (..),

    -- ** TimelineEventSort
    TimelineEventSort (..),

    -- ** Action
    Action (Action'),
    newAction,

    -- ** AddRegionAction
    AddRegionAction (AddRegionAction'),
    newAddRegionAction,

    -- ** AttributeValueList
    AttributeValueList (AttributeValueList'),
    newAttributeValueList,

    -- ** AutomationExecution
    AutomationExecution (AutomationExecution'),
    newAutomationExecution,

    -- ** ChatChannel
    ChatChannel (ChatChannel'),
    newChatChannel,

    -- ** Condition
    Condition (Condition'),
    newCondition,

    -- ** DeleteRegionAction
    DeleteRegionAction (DeleteRegionAction'),
    newDeleteRegionAction,

    -- ** EmptyChatChannel
    EmptyChatChannel (EmptyChatChannel'),
    newEmptyChatChannel,

    -- ** EventSummary
    EventSummary (EventSummary'),
    newEventSummary,

    -- ** Filter
    Filter (Filter'),
    newFilter,

    -- ** IncidentRecord
    IncidentRecord (IncidentRecord'),
    newIncidentRecord,

    -- ** IncidentRecordSource
    IncidentRecordSource (IncidentRecordSource'),
    newIncidentRecordSource,

    -- ** IncidentRecordSummary
    IncidentRecordSummary (IncidentRecordSummary'),
    newIncidentRecordSummary,

    -- ** IncidentTemplate
    IncidentTemplate (IncidentTemplate'),
    newIncidentTemplate,

    -- ** ItemIdentifier
    ItemIdentifier (ItemIdentifier'),
    newItemIdentifier,

    -- ** ItemValue
    ItemValue (ItemValue'),
    newItemValue,

    -- ** NotificationTargetItem
    NotificationTargetItem (NotificationTargetItem'),
    newNotificationTargetItem,

    -- ** RegionInfo
    RegionInfo (RegionInfo'),
    newRegionInfo,

    -- ** RegionMapInputValue
    RegionMapInputValue (RegionMapInputValue'),
    newRegionMapInputValue,

    -- ** RelatedItem
    RelatedItem (RelatedItem'),
    newRelatedItem,

    -- ** RelatedItemsUpdate
    RelatedItemsUpdate (RelatedItemsUpdate'),
    newRelatedItemsUpdate,

    -- ** ReplicationSet
    ReplicationSet (ReplicationSet'),
    newReplicationSet,

    -- ** ResourcePolicy
    ResourcePolicy (ResourcePolicy'),
    newResourcePolicy,

    -- ** ResponsePlanSummary
    ResponsePlanSummary (ResponsePlanSummary'),
    newResponsePlanSummary,

    -- ** SsmAutomation
    SsmAutomation (SsmAutomation'),
    newSsmAutomation,

    -- ** TimelineEvent
    TimelineEvent (TimelineEvent'),
    newTimelineEvent,

    -- ** TriggerDetails
    TriggerDetails (TriggerDetails'),
    newTriggerDetails,

    -- ** UpdateReplicationSetAction
    UpdateReplicationSetAction (UpdateReplicationSetAction'),
    newUpdateReplicationSetAction,
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
import Amazonka.SSMIncidents.Lens
import Amazonka.SSMIncidents.ListIncidentRecords
import Amazonka.SSMIncidents.ListRelatedItems
import Amazonka.SSMIncidents.ListReplicationSets
import Amazonka.SSMIncidents.ListResponsePlans
import Amazonka.SSMIncidents.ListTagsForResource
import Amazonka.SSMIncidents.ListTimelineEvents
import Amazonka.SSMIncidents.PutResourcePolicy
import Amazonka.SSMIncidents.StartIncident
import Amazonka.SSMIncidents.TagResource
import Amazonka.SSMIncidents.Types
import Amazonka.SSMIncidents.UntagResource
import Amazonka.SSMIncidents.UpdateDeletionProtection
import Amazonka.SSMIncidents.UpdateIncidentRecord
import Amazonka.SSMIncidents.UpdateRelatedItems
import Amazonka.SSMIncidents.UpdateReplicationSet
import Amazonka.SSMIncidents.UpdateResponsePlan
import Amazonka.SSMIncidents.UpdateTimelineEvent
import Amazonka.SSMIncidents.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'SSMIncidents'.

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
