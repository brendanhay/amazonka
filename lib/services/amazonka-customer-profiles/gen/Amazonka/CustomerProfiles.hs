{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.CustomerProfiles
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2020-08-15@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- Amazon Connect Customer Profiles
--
-- Amazon Connect Customer Profiles is a unified customer profile for your
-- contact center that has pre-built connectors powered by AppFlow that
-- make it easy to combine customer information from third party
-- applications, such as Salesforce (CRM), ServiceNow (ITSM), and your
-- enterprise resource planning (ERP), with contact history from your
-- Amazon Connect contact center. If you\'re new to Amazon Connect, you
-- might find it helpful to review the
-- <https://docs.aws.amazon.com/connect/latest/adminguide/ Amazon Connect Administrator Guide>.
module Amazonka.CustomerProfiles
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** AccessDeniedException
    _AccessDeniedException,

    -- ** BadRequestException
    _BadRequestException,

    -- ** InternalServerException
    _InternalServerException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- ** ThrottlingException
    _ThrottlingException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** AddProfileKey
    AddProfileKey (AddProfileKey'),
    newAddProfileKey,
    AddProfileKeyResponse (AddProfileKeyResponse'),
    newAddProfileKeyResponse,

    -- ** CreateCalculatedAttributeDefinition
    CreateCalculatedAttributeDefinition (CreateCalculatedAttributeDefinition'),
    newCreateCalculatedAttributeDefinition,
    CreateCalculatedAttributeDefinitionResponse (CreateCalculatedAttributeDefinitionResponse'),
    newCreateCalculatedAttributeDefinitionResponse,

    -- ** CreateDomain
    CreateDomain (CreateDomain'),
    newCreateDomain,
    CreateDomainResponse (CreateDomainResponse'),
    newCreateDomainResponse,

    -- ** CreateEventStream
    CreateEventStream (CreateEventStream'),
    newCreateEventStream,
    CreateEventStreamResponse (CreateEventStreamResponse'),
    newCreateEventStreamResponse,

    -- ** CreateIntegrationWorkflow
    CreateIntegrationWorkflow (CreateIntegrationWorkflow'),
    newCreateIntegrationWorkflow,
    CreateIntegrationWorkflowResponse (CreateIntegrationWorkflowResponse'),
    newCreateIntegrationWorkflowResponse,

    -- ** CreateProfile
    CreateProfile (CreateProfile'),
    newCreateProfile,
    CreateProfileResponse (CreateProfileResponse'),
    newCreateProfileResponse,

    -- ** DeleteCalculatedAttributeDefinition
    DeleteCalculatedAttributeDefinition (DeleteCalculatedAttributeDefinition'),
    newDeleteCalculatedAttributeDefinition,
    DeleteCalculatedAttributeDefinitionResponse (DeleteCalculatedAttributeDefinitionResponse'),
    newDeleteCalculatedAttributeDefinitionResponse,

    -- ** DeleteDomain
    DeleteDomain (DeleteDomain'),
    newDeleteDomain,
    DeleteDomainResponse (DeleteDomainResponse'),
    newDeleteDomainResponse,

    -- ** DeleteEventStream
    DeleteEventStream (DeleteEventStream'),
    newDeleteEventStream,
    DeleteEventStreamResponse (DeleteEventStreamResponse'),
    newDeleteEventStreamResponse,

    -- ** DeleteIntegration
    DeleteIntegration (DeleteIntegration'),
    newDeleteIntegration,
    DeleteIntegrationResponse (DeleteIntegrationResponse'),
    newDeleteIntegrationResponse,

    -- ** DeleteProfile
    DeleteProfile (DeleteProfile'),
    newDeleteProfile,
    DeleteProfileResponse (DeleteProfileResponse'),
    newDeleteProfileResponse,

    -- ** DeleteProfileKey
    DeleteProfileKey (DeleteProfileKey'),
    newDeleteProfileKey,
    DeleteProfileKeyResponse (DeleteProfileKeyResponse'),
    newDeleteProfileKeyResponse,

    -- ** DeleteProfileObject
    DeleteProfileObject (DeleteProfileObject'),
    newDeleteProfileObject,
    DeleteProfileObjectResponse (DeleteProfileObjectResponse'),
    newDeleteProfileObjectResponse,

    -- ** DeleteProfileObjectType
    DeleteProfileObjectType (DeleteProfileObjectType'),
    newDeleteProfileObjectType,
    DeleteProfileObjectTypeResponse (DeleteProfileObjectTypeResponse'),
    newDeleteProfileObjectTypeResponse,

    -- ** DeleteWorkflow
    DeleteWorkflow (DeleteWorkflow'),
    newDeleteWorkflow,
    DeleteWorkflowResponse (DeleteWorkflowResponse'),
    newDeleteWorkflowResponse,

    -- ** GetAutoMergingPreview
    GetAutoMergingPreview (GetAutoMergingPreview'),
    newGetAutoMergingPreview,
    GetAutoMergingPreviewResponse (GetAutoMergingPreviewResponse'),
    newGetAutoMergingPreviewResponse,

    -- ** GetCalculatedAttributeDefinition
    GetCalculatedAttributeDefinition (GetCalculatedAttributeDefinition'),
    newGetCalculatedAttributeDefinition,
    GetCalculatedAttributeDefinitionResponse (GetCalculatedAttributeDefinitionResponse'),
    newGetCalculatedAttributeDefinitionResponse,

    -- ** GetCalculatedAttributeForProfile
    GetCalculatedAttributeForProfile (GetCalculatedAttributeForProfile'),
    newGetCalculatedAttributeForProfile,
    GetCalculatedAttributeForProfileResponse (GetCalculatedAttributeForProfileResponse'),
    newGetCalculatedAttributeForProfileResponse,

    -- ** GetDomain
    GetDomain (GetDomain'),
    newGetDomain,
    GetDomainResponse (GetDomainResponse'),
    newGetDomainResponse,

    -- ** GetEventStream
    GetEventStream (GetEventStream'),
    newGetEventStream,
    GetEventStreamResponse (GetEventStreamResponse'),
    newGetEventStreamResponse,

    -- ** GetIdentityResolutionJob
    GetIdentityResolutionJob (GetIdentityResolutionJob'),
    newGetIdentityResolutionJob,
    GetIdentityResolutionJobResponse (GetIdentityResolutionJobResponse'),
    newGetIdentityResolutionJobResponse,

    -- ** GetIntegration
    GetIntegration (GetIntegration'),
    newGetIntegration,
    GetIntegrationResponse (GetIntegrationResponse'),
    newGetIntegrationResponse,

    -- ** GetMatches
    GetMatches (GetMatches'),
    newGetMatches,
    GetMatchesResponse (GetMatchesResponse'),
    newGetMatchesResponse,

    -- ** GetProfileObjectType
    GetProfileObjectType (GetProfileObjectType'),
    newGetProfileObjectType,
    GetProfileObjectTypeResponse (GetProfileObjectTypeResponse'),
    newGetProfileObjectTypeResponse,

    -- ** GetProfileObjectTypeTemplate
    GetProfileObjectTypeTemplate (GetProfileObjectTypeTemplate'),
    newGetProfileObjectTypeTemplate,
    GetProfileObjectTypeTemplateResponse (GetProfileObjectTypeTemplateResponse'),
    newGetProfileObjectTypeTemplateResponse,

    -- ** GetWorkflow
    GetWorkflow (GetWorkflow'),
    newGetWorkflow,
    GetWorkflowResponse (GetWorkflowResponse'),
    newGetWorkflowResponse,

    -- ** GetWorkflowSteps
    GetWorkflowSteps (GetWorkflowSteps'),
    newGetWorkflowSteps,
    GetWorkflowStepsResponse (GetWorkflowStepsResponse'),
    newGetWorkflowStepsResponse,

    -- ** ListAccountIntegrations
    ListAccountIntegrations (ListAccountIntegrations'),
    newListAccountIntegrations,
    ListAccountIntegrationsResponse (ListAccountIntegrationsResponse'),
    newListAccountIntegrationsResponse,

    -- ** ListCalculatedAttributeDefinitions
    ListCalculatedAttributeDefinitions (ListCalculatedAttributeDefinitions'),
    newListCalculatedAttributeDefinitions,
    ListCalculatedAttributeDefinitionsResponse (ListCalculatedAttributeDefinitionsResponse'),
    newListCalculatedAttributeDefinitionsResponse,

    -- ** ListCalculatedAttributesForProfile
    ListCalculatedAttributesForProfile (ListCalculatedAttributesForProfile'),
    newListCalculatedAttributesForProfile,
    ListCalculatedAttributesForProfileResponse (ListCalculatedAttributesForProfileResponse'),
    newListCalculatedAttributesForProfileResponse,

    -- ** ListDomains
    ListDomains (ListDomains'),
    newListDomains,
    ListDomainsResponse (ListDomainsResponse'),
    newListDomainsResponse,

    -- ** ListEventStreams (Paginated)
    ListEventStreams (ListEventStreams'),
    newListEventStreams,
    ListEventStreamsResponse (ListEventStreamsResponse'),
    newListEventStreamsResponse,

    -- ** ListIdentityResolutionJobs
    ListIdentityResolutionJobs (ListIdentityResolutionJobs'),
    newListIdentityResolutionJobs,
    ListIdentityResolutionJobsResponse (ListIdentityResolutionJobsResponse'),
    newListIdentityResolutionJobsResponse,

    -- ** ListIntegrations
    ListIntegrations (ListIntegrations'),
    newListIntegrations,
    ListIntegrationsResponse (ListIntegrationsResponse'),
    newListIntegrationsResponse,

    -- ** ListProfileObjectTypeTemplates
    ListProfileObjectTypeTemplates (ListProfileObjectTypeTemplates'),
    newListProfileObjectTypeTemplates,
    ListProfileObjectTypeTemplatesResponse (ListProfileObjectTypeTemplatesResponse'),
    newListProfileObjectTypeTemplatesResponse,

    -- ** ListProfileObjectTypes
    ListProfileObjectTypes (ListProfileObjectTypes'),
    newListProfileObjectTypes,
    ListProfileObjectTypesResponse (ListProfileObjectTypesResponse'),
    newListProfileObjectTypesResponse,

    -- ** ListProfileObjects
    ListProfileObjects (ListProfileObjects'),
    newListProfileObjects,
    ListProfileObjectsResponse (ListProfileObjectsResponse'),
    newListProfileObjectsResponse,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** ListWorkflows
    ListWorkflows (ListWorkflows'),
    newListWorkflows,
    ListWorkflowsResponse (ListWorkflowsResponse'),
    newListWorkflowsResponse,

    -- ** MergeProfiles
    MergeProfiles (MergeProfiles'),
    newMergeProfiles,
    MergeProfilesResponse (MergeProfilesResponse'),
    newMergeProfilesResponse,

    -- ** PutIntegration
    PutIntegration (PutIntegration'),
    newPutIntegration,
    PutIntegrationResponse (PutIntegrationResponse'),
    newPutIntegrationResponse,

    -- ** PutProfileObject
    PutProfileObject (PutProfileObject'),
    newPutProfileObject,
    PutProfileObjectResponse (PutProfileObjectResponse'),
    newPutProfileObjectResponse,

    -- ** PutProfileObjectType
    PutProfileObjectType (PutProfileObjectType'),
    newPutProfileObjectType,
    PutProfileObjectTypeResponse (PutProfileObjectTypeResponse'),
    newPutProfileObjectTypeResponse,

    -- ** SearchProfiles
    SearchProfiles (SearchProfiles'),
    newSearchProfiles,
    SearchProfilesResponse (SearchProfilesResponse'),
    newSearchProfilesResponse,

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

    -- ** UpdateCalculatedAttributeDefinition
    UpdateCalculatedAttributeDefinition (UpdateCalculatedAttributeDefinition'),
    newUpdateCalculatedAttributeDefinition,
    UpdateCalculatedAttributeDefinitionResponse (UpdateCalculatedAttributeDefinitionResponse'),
    newUpdateCalculatedAttributeDefinitionResponse,

    -- ** UpdateDomain
    UpdateDomain (UpdateDomain'),
    newUpdateDomain,
    UpdateDomainResponse (UpdateDomainResponse'),
    newUpdateDomainResponse,

    -- ** UpdateProfile
    UpdateProfile (UpdateProfile'),
    newUpdateProfile,
    UpdateProfileResponse (UpdateProfileResponse'),
    newUpdateProfileResponse,

    -- * Types

    -- ** ConflictResolvingModel
    ConflictResolvingModel (..),

    -- ** DataPullMode
    DataPullMode (..),

    -- ** EventStreamDestinationStatus
    EventStreamDestinationStatus (..),

    -- ** EventStreamState
    EventStreamState (..),

    -- ** FieldContentType
    FieldContentType (..),

    -- ** Gender
    Gender (..),

    -- ** IdentityResolutionJobStatus
    IdentityResolutionJobStatus (..),

    -- ** JobScheduleDayOfTheWeek
    JobScheduleDayOfTheWeek (..),

    -- ** LogicalOperator
    LogicalOperator (..),

    -- ** MarketoConnectorOperator
    MarketoConnectorOperator (..),

    -- ** Operator
    Operator (..),

    -- ** OperatorPropertiesKeys
    OperatorPropertiesKeys (..),

    -- ** PartyType
    PartyType (..),

    -- ** S3ConnectorOperator
    S3ConnectorOperator (..),

    -- ** SalesforceConnectorOperator
    SalesforceConnectorOperator (..),

    -- ** ServiceNowConnectorOperator
    ServiceNowConnectorOperator (..),

    -- ** SourceConnectorType
    SourceConnectorType (..),

    -- ** StandardIdentifier
    StandardIdentifier (..),

    -- ** Statistic
    Statistic (..),

    -- ** Status
    Status (..),

    -- ** TaskType
    TaskType (..),

    -- ** TriggerType
    TriggerType (..),

    -- ** Unit
    Unit (..),

    -- ** WorkflowType
    WorkflowType (..),

    -- ** ZendeskConnectorOperator
    ZendeskConnectorOperator (..),

    -- ** AdditionalSearchKey
    AdditionalSearchKey (AdditionalSearchKey'),
    newAdditionalSearchKey,

    -- ** Address
    Address (Address'),
    newAddress,

    -- ** AppflowIntegration
    AppflowIntegration (AppflowIntegration'),
    newAppflowIntegration,

    -- ** AppflowIntegrationWorkflowAttributes
    AppflowIntegrationWorkflowAttributes (AppflowIntegrationWorkflowAttributes'),
    newAppflowIntegrationWorkflowAttributes,

    -- ** AppflowIntegrationWorkflowMetrics
    AppflowIntegrationWorkflowMetrics (AppflowIntegrationWorkflowMetrics'),
    newAppflowIntegrationWorkflowMetrics,

    -- ** AppflowIntegrationWorkflowStep
    AppflowIntegrationWorkflowStep (AppflowIntegrationWorkflowStep'),
    newAppflowIntegrationWorkflowStep,

    -- ** AttributeDetails
    AttributeDetails (AttributeDetails'),
    newAttributeDetails,

    -- ** AttributeItem
    AttributeItem (AttributeItem'),
    newAttributeItem,

    -- ** AutoMerging
    AutoMerging (AutoMerging'),
    newAutoMerging,

    -- ** Batch
    Batch (Batch'),
    newBatch,

    -- ** Conditions
    Conditions (Conditions'),
    newConditions,

    -- ** ConflictResolution
    ConflictResolution (ConflictResolution'),
    newConflictResolution,

    -- ** ConnectorOperator
    ConnectorOperator (ConnectorOperator'),
    newConnectorOperator,

    -- ** Consolidation
    Consolidation (Consolidation'),
    newConsolidation,

    -- ** DestinationSummary
    DestinationSummary (DestinationSummary'),
    newDestinationSummary,

    -- ** DomainStats
    DomainStats (DomainStats'),
    newDomainStats,

    -- ** EventStreamDestinationDetails
    EventStreamDestinationDetails (EventStreamDestinationDetails'),
    newEventStreamDestinationDetails,

    -- ** EventStreamSummary
    EventStreamSummary (EventStreamSummary'),
    newEventStreamSummary,

    -- ** ExportingConfig
    ExportingConfig (ExportingConfig'),
    newExportingConfig,

    -- ** ExportingLocation
    ExportingLocation (ExportingLocation'),
    newExportingLocation,

    -- ** FieldSourceProfileIds
    FieldSourceProfileIds (FieldSourceProfileIds'),
    newFieldSourceProfileIds,

    -- ** FlowDefinition
    FlowDefinition (FlowDefinition'),
    newFlowDefinition,

    -- ** FoundByKeyValue
    FoundByKeyValue (FoundByKeyValue'),
    newFoundByKeyValue,

    -- ** IdentityResolutionJob
    IdentityResolutionJob (IdentityResolutionJob'),
    newIdentityResolutionJob,

    -- ** IncrementalPullConfig
    IncrementalPullConfig (IncrementalPullConfig'),
    newIncrementalPullConfig,

    -- ** IntegrationConfig
    IntegrationConfig (IntegrationConfig'),
    newIntegrationConfig,

    -- ** JobSchedule
    JobSchedule (JobSchedule'),
    newJobSchedule,

    -- ** JobStats
    JobStats (JobStats'),
    newJobStats,

    -- ** ListCalculatedAttributeDefinitionItem
    ListCalculatedAttributeDefinitionItem (ListCalculatedAttributeDefinitionItem'),
    newListCalculatedAttributeDefinitionItem,

    -- ** ListCalculatedAttributeForProfileItem
    ListCalculatedAttributeForProfileItem (ListCalculatedAttributeForProfileItem'),
    newListCalculatedAttributeForProfileItem,

    -- ** ListDomainItem
    ListDomainItem (ListDomainItem'),
    newListDomainItem,

    -- ** ListIntegrationItem
    ListIntegrationItem (ListIntegrationItem'),
    newListIntegrationItem,

    -- ** ListProfileObjectTypeItem
    ListProfileObjectTypeItem (ListProfileObjectTypeItem'),
    newListProfileObjectTypeItem,

    -- ** ListProfileObjectTypeTemplateItem
    ListProfileObjectTypeTemplateItem (ListProfileObjectTypeTemplateItem'),
    newListProfileObjectTypeTemplateItem,

    -- ** ListProfileObjectsItem
    ListProfileObjectsItem (ListProfileObjectsItem'),
    newListProfileObjectsItem,

    -- ** ListWorkflowsItem
    ListWorkflowsItem (ListWorkflowsItem'),
    newListWorkflowsItem,

    -- ** MarketoSourceProperties
    MarketoSourceProperties (MarketoSourceProperties'),
    newMarketoSourceProperties,

    -- ** MatchItem
    MatchItem (MatchItem'),
    newMatchItem,

    -- ** MatchingRequest
    MatchingRequest (MatchingRequest'),
    newMatchingRequest,

    -- ** MatchingResponse
    MatchingResponse (MatchingResponse'),
    newMatchingResponse,

    -- ** ObjectFilter
    ObjectFilter (ObjectFilter'),
    newObjectFilter,

    -- ** ObjectTypeField
    ObjectTypeField (ObjectTypeField'),
    newObjectTypeField,

    -- ** ObjectTypeKey
    ObjectTypeKey (ObjectTypeKey'),
    newObjectTypeKey,

    -- ** Profile
    Profile (Profile'),
    newProfile,

    -- ** Range
    Range (Range'),
    newRange,

    -- ** S3ExportingConfig
    S3ExportingConfig (S3ExportingConfig'),
    newS3ExportingConfig,

    -- ** S3ExportingLocation
    S3ExportingLocation (S3ExportingLocation'),
    newS3ExportingLocation,

    -- ** S3SourceProperties
    S3SourceProperties (S3SourceProperties'),
    newS3SourceProperties,

    -- ** SalesforceSourceProperties
    SalesforceSourceProperties (SalesforceSourceProperties'),
    newSalesforceSourceProperties,

    -- ** ScheduledTriggerProperties
    ScheduledTriggerProperties (ScheduledTriggerProperties'),
    newScheduledTriggerProperties,

    -- ** ServiceNowSourceProperties
    ServiceNowSourceProperties (ServiceNowSourceProperties'),
    newServiceNowSourceProperties,

    -- ** SourceConnectorProperties
    SourceConnectorProperties (SourceConnectorProperties'),
    newSourceConnectorProperties,

    -- ** SourceFlowConfig
    SourceFlowConfig (SourceFlowConfig'),
    newSourceFlowConfig,

    -- ** Task
    Task (Task'),
    newTask,

    -- ** Threshold
    Threshold (Threshold'),
    newThreshold,

    -- ** TriggerConfig
    TriggerConfig (TriggerConfig'),
    newTriggerConfig,

    -- ** TriggerProperties
    TriggerProperties (TriggerProperties'),
    newTriggerProperties,

    -- ** UpdateAddress
    UpdateAddress (UpdateAddress'),
    newUpdateAddress,

    -- ** WorkflowAttributes
    WorkflowAttributes (WorkflowAttributes'),
    newWorkflowAttributes,

    -- ** WorkflowMetrics
    WorkflowMetrics (WorkflowMetrics'),
    newWorkflowMetrics,

    -- ** WorkflowStepItem
    WorkflowStepItem (WorkflowStepItem'),
    newWorkflowStepItem,

    -- ** ZendeskSourceProperties
    ZendeskSourceProperties (ZendeskSourceProperties'),
    newZendeskSourceProperties,
  )
where

import Amazonka.CustomerProfiles.AddProfileKey
import Amazonka.CustomerProfiles.CreateCalculatedAttributeDefinition
import Amazonka.CustomerProfiles.CreateDomain
import Amazonka.CustomerProfiles.CreateEventStream
import Amazonka.CustomerProfiles.CreateIntegrationWorkflow
import Amazonka.CustomerProfiles.CreateProfile
import Amazonka.CustomerProfiles.DeleteCalculatedAttributeDefinition
import Amazonka.CustomerProfiles.DeleteDomain
import Amazonka.CustomerProfiles.DeleteEventStream
import Amazonka.CustomerProfiles.DeleteIntegration
import Amazonka.CustomerProfiles.DeleteProfile
import Amazonka.CustomerProfiles.DeleteProfileKey
import Amazonka.CustomerProfiles.DeleteProfileObject
import Amazonka.CustomerProfiles.DeleteProfileObjectType
import Amazonka.CustomerProfiles.DeleteWorkflow
import Amazonka.CustomerProfiles.GetAutoMergingPreview
import Amazonka.CustomerProfiles.GetCalculatedAttributeDefinition
import Amazonka.CustomerProfiles.GetCalculatedAttributeForProfile
import Amazonka.CustomerProfiles.GetDomain
import Amazonka.CustomerProfiles.GetEventStream
import Amazonka.CustomerProfiles.GetIdentityResolutionJob
import Amazonka.CustomerProfiles.GetIntegration
import Amazonka.CustomerProfiles.GetMatches
import Amazonka.CustomerProfiles.GetProfileObjectType
import Amazonka.CustomerProfiles.GetProfileObjectTypeTemplate
import Amazonka.CustomerProfiles.GetWorkflow
import Amazonka.CustomerProfiles.GetWorkflowSteps
import Amazonka.CustomerProfiles.Lens
import Amazonka.CustomerProfiles.ListAccountIntegrations
import Amazonka.CustomerProfiles.ListCalculatedAttributeDefinitions
import Amazonka.CustomerProfiles.ListCalculatedAttributesForProfile
import Amazonka.CustomerProfiles.ListDomains
import Amazonka.CustomerProfiles.ListEventStreams
import Amazonka.CustomerProfiles.ListIdentityResolutionJobs
import Amazonka.CustomerProfiles.ListIntegrations
import Amazonka.CustomerProfiles.ListProfileObjectTypeTemplates
import Amazonka.CustomerProfiles.ListProfileObjectTypes
import Amazonka.CustomerProfiles.ListProfileObjects
import Amazonka.CustomerProfiles.ListTagsForResource
import Amazonka.CustomerProfiles.ListWorkflows
import Amazonka.CustomerProfiles.MergeProfiles
import Amazonka.CustomerProfiles.PutIntegration
import Amazonka.CustomerProfiles.PutProfileObject
import Amazonka.CustomerProfiles.PutProfileObjectType
import Amazonka.CustomerProfiles.SearchProfiles
import Amazonka.CustomerProfiles.TagResource
import Amazonka.CustomerProfiles.Types
import Amazonka.CustomerProfiles.UntagResource
import Amazonka.CustomerProfiles.UpdateCalculatedAttributeDefinition
import Amazonka.CustomerProfiles.UpdateDomain
import Amazonka.CustomerProfiles.UpdateProfile
import Amazonka.CustomerProfiles.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'CustomerProfiles'.

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
