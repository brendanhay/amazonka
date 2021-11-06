{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.Connect
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2017-08-08@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- Amazon Connect is a cloud-based contact center solution that you use to
-- set up and manage a customer contact center and provide reliable
-- customer engagement at any scale.
--
-- Amazon Connect provides metrics and real-time reporting that enable you
-- to optimize contact routing. You can also resolve customer issues more
-- efficiently by getting customers in touch with the appropriate agents.
--
-- There are limits to the number of Amazon Connect resources that you can
-- create. There are also limits to the number of requests that you can
-- make per second. For more information, see
-- <https://docs.aws.amazon.com/connect/latest/adminguide/amazon-connect-service-limits.html Amazon Connect Service Quotas>
-- in the /Amazon Connect Administrator Guide/.
--
-- You can connect programmatically to an AWS service by using an endpoint.
-- For a list of Amazon Connect endpoints, see
-- <https://docs.aws.amazon.com/general/latest/gr/connect_region.html Amazon Connect Endpoints>.
--
-- Working with contact flows? Check out the
-- <https://docs.aws.amazon.com/connect/latest/adminguide/flow-language.html Amazon Connect Flow language>.
module Amazonka.Connect
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** InvalidContactFlowException
    _InvalidContactFlowException,

    -- ** OutboundContactNotPermittedException
    _OutboundContactNotPermittedException,

    -- ** InvalidParameterException
    _InvalidParameterException,

    -- ** InvalidRequestException
    _InvalidRequestException,

    -- ** DuplicateResourceException
    _DuplicateResourceException,

    -- ** UserNotFoundException
    _UserNotFoundException,

    -- ** ContactFlowNotPublishedException
    _ContactFlowNotPublishedException,

    -- ** DestinationNotAllowedException
    _DestinationNotAllowedException,

    -- ** ContactNotFoundException
    _ContactNotFoundException,

    -- ** ServiceQuotaExceededException
    _ServiceQuotaExceededException,

    -- ** ThrottlingException
    _ThrottlingException,

    -- ** InternalServiceException
    _InternalServiceException,

    -- ** ResourceConflictException
    _ResourceConflictException,

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

    -- ** DescribeInstance
    DescribeInstance (DescribeInstance'),
    newDescribeInstance,
    DescribeInstanceResponse (DescribeInstanceResponse'),
    newDescribeInstanceResponse,

    -- ** ListSecurityProfiles (Paginated)
    ListSecurityProfiles (ListSecurityProfiles'),
    newListSecurityProfiles,
    ListSecurityProfilesResponse (ListSecurityProfilesResponse'),
    newListSecurityProfilesResponse,

    -- ** AssociateLexBot
    AssociateLexBot (AssociateLexBot'),
    newAssociateLexBot,
    AssociateLexBotResponse (AssociateLexBotResponse'),
    newAssociateLexBotResponse,

    -- ** UpdateInstanceAttribute
    UpdateInstanceAttribute (UpdateInstanceAttribute'),
    newUpdateInstanceAttribute,
    UpdateInstanceAttributeResponse (UpdateInstanceAttributeResponse'),
    newUpdateInstanceAttributeResponse,

    -- ** UpdateQueueStatus
    UpdateQueueStatus (UpdateQueueStatus'),
    newUpdateQueueStatus,
    UpdateQueueStatusResponse (UpdateQueueStatusResponse'),
    newUpdateQueueStatusResponse,

    -- ** UpdateRoutingProfileQueues
    UpdateRoutingProfileQueues (UpdateRoutingProfileQueues'),
    newUpdateRoutingProfileQueues,
    UpdateRoutingProfileQueuesResponse (UpdateRoutingProfileQueuesResponse'),
    newUpdateRoutingProfileQueuesResponse,

    -- ** DescribeQueue
    DescribeQueue (DescribeQueue'),
    newDescribeQueue,
    DescribeQueueResponse (DescribeQueueResponse'),
    newDescribeQueueResponse,

    -- ** ListInstanceAttributes (Paginated)
    ListInstanceAttributes (ListInstanceAttributes'),
    newListInstanceAttributes,
    ListInstanceAttributesResponse (ListInstanceAttributesResponse'),
    newListInstanceAttributesResponse,

    -- ** UpdateAgentStatus
    UpdateAgentStatus (UpdateAgentStatus'),
    newUpdateAgentStatus,
    UpdateAgentStatusResponse (UpdateAgentStatusResponse'),
    newUpdateAgentStatusResponse,

    -- ** DescribeInstanceStorageConfig
    DescribeInstanceStorageConfig (DescribeInstanceStorageConfig'),
    newDescribeInstanceStorageConfig,
    DescribeInstanceStorageConfigResponse (DescribeInstanceStorageConfigResponse'),
    newDescribeInstanceStorageConfigResponse,

    -- ** CreateQuickConnect
    CreateQuickConnect (CreateQuickConnect'),
    newCreateQuickConnect,
    CreateQuickConnectResponse (CreateQuickConnectResponse'),
    newCreateQuickConnectResponse,

    -- ** DescribeContactFlow
    DescribeContactFlow (DescribeContactFlow'),
    newDescribeContactFlow,
    DescribeContactFlowResponse (DescribeContactFlowResponse'),
    newDescribeContactFlowResponse,

    -- ** UpdateUserHierarchy
    UpdateUserHierarchy (UpdateUserHierarchy'),
    newUpdateUserHierarchy,
    UpdateUserHierarchyResponse (UpdateUserHierarchyResponse'),
    newUpdateUserHierarchyResponse,

    -- ** UpdateUserRoutingProfile
    UpdateUserRoutingProfile (UpdateUserRoutingProfile'),
    newUpdateUserRoutingProfile,
    UpdateUserRoutingProfileResponse (UpdateUserRoutingProfileResponse'),
    newUpdateUserRoutingProfileResponse,

    -- ** UpdateUserHierarchyGroupName
    UpdateUserHierarchyGroupName (UpdateUserHierarchyGroupName'),
    newUpdateUserHierarchyGroupName,
    UpdateUserHierarchyGroupNameResponse (UpdateUserHierarchyGroupNameResponse'),
    newUpdateUserHierarchyGroupNameResponse,

    -- ** UpdateQueueHoursOfOperation
    UpdateQueueHoursOfOperation (UpdateQueueHoursOfOperation'),
    newUpdateQueueHoursOfOperation,
    UpdateQueueHoursOfOperationResponse (UpdateQueueHoursOfOperationResponse'),
    newUpdateQueueHoursOfOperationResponse,

    -- ** DescribeRoutingProfile
    DescribeRoutingProfile (DescribeRoutingProfile'),
    newDescribeRoutingProfile,
    DescribeRoutingProfileResponse (DescribeRoutingProfileResponse'),
    newDescribeRoutingProfileResponse,

    -- ** DisassociateLexBot
    DisassociateLexBot (DisassociateLexBot'),
    newDisassociateLexBot,
    DisassociateLexBotResponse (DisassociateLexBotResponse'),
    newDisassociateLexBotResponse,

    -- ** DeleteQuickConnect
    DeleteQuickConnect (DeleteQuickConnect'),
    newDeleteQuickConnect,
    DeleteQuickConnectResponse (DeleteQuickConnectResponse'),
    newDeleteQuickConnectResponse,

    -- ** StartOutboundVoiceContact
    StartOutboundVoiceContact (StartOutboundVoiceContact'),
    newStartOutboundVoiceContact,
    StartOutboundVoiceContactResponse (StartOutboundVoiceContactResponse'),
    newStartOutboundVoiceContactResponse,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** GetMetricData (Paginated)
    GetMetricData (GetMetricData'),
    newGetMetricData,
    GetMetricDataResponse (GetMetricDataResponse'),
    newGetMetricDataResponse,

    -- ** StartContactRecording
    StartContactRecording (StartContactRecording'),
    newStartContactRecording,
    StartContactRecordingResponse (StartContactRecordingResponse'),
    newStartContactRecordingResponse,

    -- ** CreateInstance
    CreateInstance (CreateInstance'),
    newCreateInstance,
    CreateInstanceResponse (CreateInstanceResponse'),
    newCreateInstanceResponse,

    -- ** AssociateBot
    AssociateBot (AssociateBot'),
    newAssociateBot,
    AssociateBotResponse (AssociateBotResponse'),
    newAssociateBotResponse,

    -- ** AssociateQueueQuickConnects
    AssociateQueueQuickConnects (AssociateQueueQuickConnects'),
    newAssociateQueueQuickConnects,
    AssociateQueueQuickConnectsResponse (AssociateQueueQuickConnectsResponse'),
    newAssociateQueueQuickConnectsResponse,

    -- ** StartTaskContact
    StartTaskContact (StartTaskContact'),
    newStartTaskContact,
    StartTaskContactResponse (StartTaskContactResponse'),
    newStartTaskContactResponse,

    -- ** ListUsers (Paginated)
    ListUsers (ListUsers'),
    newListUsers,
    ListUsersResponse (ListUsersResponse'),
    newListUsersResponse,

    -- ** ListUserHierarchyGroups (Paginated)
    ListUserHierarchyGroups (ListUserHierarchyGroups'),
    newListUserHierarchyGroups,
    ListUserHierarchyGroupsResponse (ListUserHierarchyGroupsResponse'),
    newListUserHierarchyGroupsResponse,

    -- ** ListQueues (Paginated)
    ListQueues (ListQueues'),
    newListQueues,
    ListQueuesResponse (ListQueuesResponse'),
    newListQueuesResponse,

    -- ** DescribeInstanceAttribute
    DescribeInstanceAttribute (DescribeInstanceAttribute'),
    newDescribeInstanceAttribute,
    DescribeInstanceAttributeResponse (DescribeInstanceAttributeResponse'),
    newDescribeInstanceAttributeResponse,

    -- ** ListBots (Paginated)
    ListBots (ListBots'),
    newListBots,
    ListBotsResponse (ListBotsResponse'),
    newListBotsResponse,

    -- ** UpdateQuickConnectConfig
    UpdateQuickConnectConfig (UpdateQuickConnectConfig'),
    newUpdateQuickConnectConfig,
    UpdateQuickConnectConfigResponse (UpdateQuickConnectConfigResponse'),
    newUpdateQuickConnectConfigResponse,

    -- ** DescribeAgentStatus
    DescribeAgentStatus (DescribeAgentStatus'),
    newDescribeAgentStatus,
    DescribeAgentStatusResponse (DescribeAgentStatusResponse'),
    newDescribeAgentStatusResponse,

    -- ** DeleteInstance
    DeleteInstance (DeleteInstance'),
    newDeleteInstance,
    DeleteInstanceResponse (DeleteInstanceResponse'),
    newDeleteInstanceResponse,

    -- ** DisassociateInstanceStorageConfig
    DisassociateInstanceStorageConfig (DisassociateInstanceStorageConfig'),
    newDisassociateInstanceStorageConfig,
    DisassociateInstanceStorageConfigResponse (DisassociateInstanceStorageConfigResponse'),
    newDisassociateInstanceStorageConfigResponse,

    -- ** CreateRoutingProfile
    CreateRoutingProfile (CreateRoutingProfile'),
    newCreateRoutingProfile,
    CreateRoutingProfileResponse (CreateRoutingProfileResponse'),
    newCreateRoutingProfileResponse,

    -- ** UpdateInstanceStorageConfig
    UpdateInstanceStorageConfig (UpdateInstanceStorageConfig'),
    newUpdateInstanceStorageConfig,
    UpdateInstanceStorageConfigResponse (UpdateInstanceStorageConfigResponse'),
    newUpdateInstanceStorageConfigResponse,

    -- ** DisassociateQueueQuickConnects
    DisassociateQueueQuickConnects (DisassociateQueueQuickConnects'),
    newDisassociateQueueQuickConnects,
    DisassociateQueueQuickConnectsResponse (DisassociateQueueQuickConnectsResponse'),
    newDisassociateQueueQuickConnectsResponse,

    -- ** CreateUseCase
    CreateUseCase (CreateUseCase'),
    newCreateUseCase,
    CreateUseCaseResponse (CreateUseCaseResponse'),
    newCreateUseCaseResponse,

    -- ** DisassociateBot
    DisassociateBot (DisassociateBot'),
    newDisassociateBot,
    DisassociateBotResponse (DisassociateBotResponse'),
    newDisassociateBotResponse,

    -- ** ListQueueQuickConnects (Paginated)
    ListQueueQuickConnects (ListQueueQuickConnects'),
    newListQueueQuickConnects,
    ListQueueQuickConnectsResponse (ListQueueQuickConnectsResponse'),
    newListQueueQuickConnectsResponse,

    -- ** GetCurrentMetricData
    GetCurrentMetricData (GetCurrentMetricData'),
    newGetCurrentMetricData,
    GetCurrentMetricDataResponse (GetCurrentMetricDataResponse'),
    newGetCurrentMetricDataResponse,

    -- ** CreateContactFlow
    CreateContactFlow (CreateContactFlow'),
    newCreateContactFlow,
    CreateContactFlowResponse (CreateContactFlowResponse'),
    newCreateContactFlowResponse,

    -- ** ListRoutingProfiles (Paginated)
    ListRoutingProfiles (ListRoutingProfiles'),
    newListRoutingProfiles,
    ListRoutingProfilesResponse (ListRoutingProfilesResponse'),
    newListRoutingProfilesResponse,

    -- ** DeleteIntegrationAssociation
    DeleteIntegrationAssociation (DeleteIntegrationAssociation'),
    newDeleteIntegrationAssociation,
    DeleteIntegrationAssociationResponse (DeleteIntegrationAssociationResponse'),
    newDeleteIntegrationAssociationResponse,

    -- ** DeleteHoursOfOperation
    DeleteHoursOfOperation (DeleteHoursOfOperation'),
    newDeleteHoursOfOperation,
    DeleteHoursOfOperationResponse (DeleteHoursOfOperationResponse'),
    newDeleteHoursOfOperationResponse,

    -- ** UpdateUserPhoneConfig
    UpdateUserPhoneConfig (UpdateUserPhoneConfig'),
    newUpdateUserPhoneConfig,
    UpdateUserPhoneConfigResponse (UpdateUserPhoneConfigResponse'),
    newUpdateUserPhoneConfigResponse,

    -- ** UpdateHoursOfOperation
    UpdateHoursOfOperation (UpdateHoursOfOperation'),
    newUpdateHoursOfOperation,
    UpdateHoursOfOperationResponse (UpdateHoursOfOperationResponse'),
    newUpdateHoursOfOperationResponse,

    -- ** ListApprovedOrigins (Paginated)
    ListApprovedOrigins (ListApprovedOrigins'),
    newListApprovedOrigins,
    ListApprovedOriginsResponse (ListApprovedOriginsResponse'),
    newListApprovedOriginsResponse,

    -- ** DescribeUserHierarchyStructure
    DescribeUserHierarchyStructure (DescribeUserHierarchyStructure'),
    newDescribeUserHierarchyStructure,
    DescribeUserHierarchyStructureResponse (DescribeUserHierarchyStructureResponse'),
    newDescribeUserHierarchyStructureResponse,

    -- ** ListPhoneNumbers (Paginated)
    ListPhoneNumbers (ListPhoneNumbers'),
    newListPhoneNumbers,
    ListPhoneNumbersResponse (ListPhoneNumbersResponse'),
    newListPhoneNumbersResponse,

    -- ** UpdateContactAttributes
    UpdateContactAttributes (UpdateContactAttributes'),
    newUpdateContactAttributes,
    UpdateContactAttributesResponse (UpdateContactAttributesResponse'),
    newUpdateContactAttributesResponse,

    -- ** ListUseCases (Paginated)
    ListUseCases (ListUseCases'),
    newListUseCases,
    ListUseCasesResponse (ListUseCasesResponse'),
    newListUseCasesResponse,

    -- ** StartChatContact
    StartChatContact (StartChatContact'),
    newStartChatContact,
    StartChatContactResponse (StartChatContactResponse'),
    newStartChatContactResponse,

    -- ** DeleteUseCase
    DeleteUseCase (DeleteUseCase'),
    newDeleteUseCase,
    DeleteUseCaseResponse (DeleteUseCaseResponse'),
    newDeleteUseCaseResponse,

    -- ** UpdateUserSecurityProfiles
    UpdateUserSecurityProfiles (UpdateUserSecurityProfiles'),
    newUpdateUserSecurityProfiles,
    UpdateUserSecurityProfilesResponse (UpdateUserSecurityProfilesResponse'),
    newUpdateUserSecurityProfilesResponse,

    -- ** GetContactAttributes
    GetContactAttributes (GetContactAttributes'),
    newGetContactAttributes,
    GetContactAttributesResponse (GetContactAttributesResponse'),
    newGetContactAttributesResponse,

    -- ** ListLambdaFunctions (Paginated)
    ListLambdaFunctions (ListLambdaFunctions'),
    newListLambdaFunctions,
    ListLambdaFunctionsResponse (ListLambdaFunctionsResponse'),
    newListLambdaFunctionsResponse,

    -- ** DescribeUserHierarchyGroup
    DescribeUserHierarchyGroup (DescribeUserHierarchyGroup'),
    newDescribeUserHierarchyGroup,
    DescribeUserHierarchyGroupResponse (DescribeUserHierarchyGroupResponse'),
    newDescribeUserHierarchyGroupResponse,

    -- ** DescribeUser
    DescribeUser (DescribeUser'),
    newDescribeUser,
    DescribeUserResponse (DescribeUserResponse'),
    newDescribeUserResponse,

    -- ** ResumeContactRecording
    ResumeContactRecording (ResumeContactRecording'),
    newResumeContactRecording,
    ResumeContactRecordingResponse (ResumeContactRecordingResponse'),
    newResumeContactRecordingResponse,

    -- ** UpdateContactFlowName
    UpdateContactFlowName (UpdateContactFlowName'),
    newUpdateContactFlowName,
    UpdateContactFlowNameResponse (UpdateContactFlowNameResponse'),
    newUpdateContactFlowNameResponse,

    -- ** SuspendContactRecording
    SuspendContactRecording (SuspendContactRecording'),
    newSuspendContactRecording,
    SuspendContactRecordingResponse (SuspendContactRecordingResponse'),
    newSuspendContactRecordingResponse,

    -- ** UpdateQueueName
    UpdateQueueName (UpdateQueueName'),
    newUpdateQueueName,
    UpdateQueueNameResponse (UpdateQueueNameResponse'),
    newUpdateQueueNameResponse,

    -- ** UpdateQueueMaxContacts
    UpdateQueueMaxContacts (UpdateQueueMaxContacts'),
    newUpdateQueueMaxContacts,
    UpdateQueueMaxContactsResponse (UpdateQueueMaxContactsResponse'),
    newUpdateQueueMaxContactsResponse,

    -- ** ListRoutingProfileQueues (Paginated)
    ListRoutingProfileQueues (ListRoutingProfileQueues'),
    newListRoutingProfileQueues,
    ListRoutingProfileQueuesResponse (ListRoutingProfileQueuesResponse'),
    newListRoutingProfileQueuesResponse,

    -- ** DisassociateRoutingProfileQueues
    DisassociateRoutingProfileQueues (DisassociateRoutingProfileQueues'),
    newDisassociateRoutingProfileQueues,
    DisassociateRoutingProfileQueuesResponse (DisassociateRoutingProfileQueuesResponse'),
    newDisassociateRoutingProfileQueuesResponse,

    -- ** DisassociateLambdaFunction
    DisassociateLambdaFunction (DisassociateLambdaFunction'),
    newDisassociateLambdaFunction,
    DisassociateLambdaFunctionResponse (DisassociateLambdaFunctionResponse'),
    newDisassociateLambdaFunctionResponse,

    -- ** UpdateContactFlowContent
    UpdateContactFlowContent (UpdateContactFlowContent'),
    newUpdateContactFlowContent,
    UpdateContactFlowContentResponse (UpdateContactFlowContentResponse'),
    newUpdateContactFlowContentResponse,

    -- ** UpdateUserHierarchyStructure
    UpdateUserHierarchyStructure (UpdateUserHierarchyStructure'),
    newUpdateUserHierarchyStructure,
    UpdateUserHierarchyStructureResponse (UpdateUserHierarchyStructureResponse'),
    newUpdateUserHierarchyStructureResponse,

    -- ** DescribeHoursOfOperation
    DescribeHoursOfOperation (DescribeHoursOfOperation'),
    newDescribeHoursOfOperation,
    DescribeHoursOfOperationResponse (DescribeHoursOfOperationResponse'),
    newDescribeHoursOfOperationResponse,

    -- ** ListQuickConnects (Paginated)
    ListQuickConnects (ListQuickConnects'),
    newListQuickConnects,
    ListQuickConnectsResponse (ListQuickConnectsResponse'),
    newListQuickConnectsResponse,

    -- ** CreateUserHierarchyGroup
    CreateUserHierarchyGroup (CreateUserHierarchyGroup'),
    newCreateUserHierarchyGroup,
    CreateUserHierarchyGroupResponse (CreateUserHierarchyGroupResponse'),
    newCreateUserHierarchyGroupResponse,

    -- ** CreateUser
    CreateUser (CreateUser'),
    newCreateUser,
    CreateUserResponse (CreateUserResponse'),
    newCreateUserResponse,

    -- ** CreateQueue
    CreateQueue (CreateQueue'),
    newCreateQueue,
    CreateQueueResponse (CreateQueueResponse'),
    newCreateQueueResponse,

    -- ** UpdateQuickConnectName
    UpdateQuickConnectName (UpdateQuickConnectName'),
    newUpdateQuickConnectName,
    UpdateQuickConnectNameResponse (UpdateQuickConnectNameResponse'),
    newUpdateQuickConnectNameResponse,

    -- ** ListPrompts (Paginated)
    ListPrompts (ListPrompts'),
    newListPrompts,
    ListPromptsResponse (ListPromptsResponse'),
    newListPromptsResponse,

    -- ** AssociateSecurityKey
    AssociateSecurityKey (AssociateSecurityKey'),
    newAssociateSecurityKey,
    AssociateSecurityKeyResponse (AssociateSecurityKeyResponse'),
    newAssociateSecurityKeyResponse,

    -- ** StopContactRecording
    StopContactRecording (StopContactRecording'),
    newStopContactRecording,
    StopContactRecordingResponse (StopContactRecordingResponse'),
    newStopContactRecordingResponse,

    -- ** DisassociateApprovedOrigin
    DisassociateApprovedOrigin (DisassociateApprovedOrigin'),
    newDisassociateApprovedOrigin,
    DisassociateApprovedOriginResponse (DisassociateApprovedOriginResponse'),
    newDisassociateApprovedOriginResponse,

    -- ** ListSecurityKeys (Paginated)
    ListSecurityKeys (ListSecurityKeys'),
    newListSecurityKeys,
    ListSecurityKeysResponse (ListSecurityKeysResponse'),
    newListSecurityKeysResponse,

    -- ** GetFederationToken
    GetFederationToken (GetFederationToken'),
    newGetFederationToken,
    GetFederationTokenResponse (GetFederationTokenResponse'),
    newGetFederationTokenResponse,

    -- ** StopContact
    StopContact (StopContact'),
    newStopContact,
    StopContactResponse (StopContactResponse'),
    newStopContactResponse,

    -- ** DeleteUser
    DeleteUser (DeleteUser'),
    newDeleteUser,
    DeleteUserResponse (DeleteUserResponse'),
    newDeleteUserResponse,

    -- ** TagResource
    TagResource (TagResource'),
    newTagResource,
    TagResourceResponse (TagResourceResponse'),
    newTagResourceResponse,

    -- ** UpdateUserIdentityInfo
    UpdateUserIdentityInfo (UpdateUserIdentityInfo'),
    newUpdateUserIdentityInfo,
    UpdateUserIdentityInfoResponse (UpdateUserIdentityInfoResponse'),
    newUpdateUserIdentityInfoResponse,

    -- ** ListInstances (Paginated)
    ListInstances (ListInstances'),
    newListInstances,
    ListInstancesResponse (ListInstancesResponse'),
    newListInstancesResponse,

    -- ** DeleteUserHierarchyGroup
    DeleteUserHierarchyGroup (DeleteUserHierarchyGroup'),
    newDeleteUserHierarchyGroup,
    DeleteUserHierarchyGroupResponse (DeleteUserHierarchyGroupResponse'),
    newDeleteUserHierarchyGroupResponse,

    -- ** UpdateRoutingProfileDefaultOutboundQueue
    UpdateRoutingProfileDefaultOutboundQueue (UpdateRoutingProfileDefaultOutboundQueue'),
    newUpdateRoutingProfileDefaultOutboundQueue,
    UpdateRoutingProfileDefaultOutboundQueueResponse (UpdateRoutingProfileDefaultOutboundQueueResponse'),
    newUpdateRoutingProfileDefaultOutboundQueueResponse,

    -- ** UpdateQueueOutboundCallerConfig
    UpdateQueueOutboundCallerConfig (UpdateQueueOutboundCallerConfig'),
    newUpdateQueueOutboundCallerConfig,
    UpdateQueueOutboundCallerConfigResponse (UpdateQueueOutboundCallerConfigResponse'),
    newUpdateQueueOutboundCallerConfigResponse,

    -- ** ListContactFlows (Paginated)
    ListContactFlows (ListContactFlows'),
    newListContactFlows,
    ListContactFlowsResponse (ListContactFlowsResponse'),
    newListContactFlowsResponse,

    -- ** CreateIntegrationAssociation
    CreateIntegrationAssociation (CreateIntegrationAssociation'),
    newCreateIntegrationAssociation,
    CreateIntegrationAssociationResponse (CreateIntegrationAssociationResponse'),
    newCreateIntegrationAssociationResponse,

    -- ** UntagResource
    UntagResource (UntagResource'),
    newUntagResource,
    UntagResourceResponse (UntagResourceResponse'),
    newUntagResourceResponse,

    -- ** AssociateApprovedOrigin
    AssociateApprovedOrigin (AssociateApprovedOrigin'),
    newAssociateApprovedOrigin,
    AssociateApprovedOriginResponse (AssociateApprovedOriginResponse'),
    newAssociateApprovedOriginResponse,

    -- ** CreateHoursOfOperation
    CreateHoursOfOperation (CreateHoursOfOperation'),
    newCreateHoursOfOperation,
    CreateHoursOfOperationResponse (CreateHoursOfOperationResponse'),
    newCreateHoursOfOperationResponse,

    -- ** DisassociateSecurityKey
    DisassociateSecurityKey (DisassociateSecurityKey'),
    newDisassociateSecurityKey,
    DisassociateSecurityKeyResponse (DisassociateSecurityKeyResponse'),
    newDisassociateSecurityKeyResponse,

    -- ** UpdateRoutingProfileConcurrency
    UpdateRoutingProfileConcurrency (UpdateRoutingProfileConcurrency'),
    newUpdateRoutingProfileConcurrency,
    UpdateRoutingProfileConcurrencyResponse (UpdateRoutingProfileConcurrencyResponse'),
    newUpdateRoutingProfileConcurrencyResponse,

    -- ** ListInstanceStorageConfigs (Paginated)
    ListInstanceStorageConfigs (ListInstanceStorageConfigs'),
    newListInstanceStorageConfigs,
    ListInstanceStorageConfigsResponse (ListInstanceStorageConfigsResponse'),
    newListInstanceStorageConfigsResponse,

    -- ** DescribeQuickConnect
    DescribeQuickConnect (DescribeQuickConnect'),
    newDescribeQuickConnect,
    DescribeQuickConnectResponse (DescribeQuickConnectResponse'),
    newDescribeQuickConnectResponse,

    -- ** AssociateInstanceStorageConfig
    AssociateInstanceStorageConfig (AssociateInstanceStorageConfig'),
    newAssociateInstanceStorageConfig,
    AssociateInstanceStorageConfigResponse (AssociateInstanceStorageConfigResponse'),
    newAssociateInstanceStorageConfigResponse,

    -- ** ListHoursOfOperations (Paginated)
    ListHoursOfOperations (ListHoursOfOperations'),
    newListHoursOfOperations,
    ListHoursOfOperationsResponse (ListHoursOfOperationsResponse'),
    newListHoursOfOperationsResponse,

    -- ** ListIntegrationAssociations (Paginated)
    ListIntegrationAssociations (ListIntegrationAssociations'),
    newListIntegrationAssociations,
    ListIntegrationAssociationsResponse (ListIntegrationAssociationsResponse'),
    newListIntegrationAssociationsResponse,

    -- ** CreateAgentStatus
    CreateAgentStatus (CreateAgentStatus'),
    newCreateAgentStatus,
    CreateAgentStatusResponse (CreateAgentStatusResponse'),
    newCreateAgentStatusResponse,

    -- ** UpdateRoutingProfileName
    UpdateRoutingProfileName (UpdateRoutingProfileName'),
    newUpdateRoutingProfileName,
    UpdateRoutingProfileNameResponse (UpdateRoutingProfileNameResponse'),
    newUpdateRoutingProfileNameResponse,

    -- ** ListLexBots (Paginated)
    ListLexBots (ListLexBots'),
    newListLexBots,
    ListLexBotsResponse (ListLexBotsResponse'),
    newListLexBotsResponse,

    -- ** ListAgentStatuses (Paginated)
    ListAgentStatuses (ListAgentStatuses'),
    newListAgentStatuses,
    ListAgentStatusesResponse (ListAgentStatusesResponse'),
    newListAgentStatusesResponse,

    -- ** AssociateLambdaFunction
    AssociateLambdaFunction (AssociateLambdaFunction'),
    newAssociateLambdaFunction,
    AssociateLambdaFunctionResponse (AssociateLambdaFunctionResponse'),
    newAssociateLambdaFunctionResponse,

    -- ** AssociateRoutingProfileQueues
    AssociateRoutingProfileQueues (AssociateRoutingProfileQueues'),
    newAssociateRoutingProfileQueues,
    AssociateRoutingProfileQueuesResponse (AssociateRoutingProfileQueuesResponse'),
    newAssociateRoutingProfileQueuesResponse,

    -- * Types

    -- ** AgentStatusState
    AgentStatusState (..),

    -- ** AgentStatusType
    AgentStatusType (..),

    -- ** Channel
    Channel (..),

    -- ** Comparison
    Comparison (..),

    -- ** ContactFlowType
    ContactFlowType (..),

    -- ** CurrentMetricName
    CurrentMetricName (..),

    -- ** DirectoryType
    DirectoryType (..),

    -- ** EncryptionType
    EncryptionType (..),

    -- ** Grouping
    Grouping (..),

    -- ** HistoricalMetricName
    HistoricalMetricName (..),

    -- ** HoursOfOperationDays
    HoursOfOperationDays (..),

    -- ** InstanceAttributeType
    InstanceAttributeType (..),

    -- ** InstanceStatus
    InstanceStatus (..),

    -- ** InstanceStorageResourceType
    InstanceStorageResourceType (..),

    -- ** IntegrationType
    IntegrationType (..),

    -- ** LexVersion
    LexVersion (..),

    -- ** PhoneNumberCountryCode
    PhoneNumberCountryCode (..),

    -- ** PhoneNumberType
    PhoneNumberType (..),

    -- ** PhoneType
    PhoneType (..),

    -- ** QueueStatus
    QueueStatus (..),

    -- ** QueueType
    QueueType (..),

    -- ** QuickConnectType
    QuickConnectType (..),

    -- ** ReferenceType
    ReferenceType (..),

    -- ** SourceType
    SourceType (..),

    -- ** Statistic
    Statistic (..),

    -- ** StorageType
    StorageType (..),

    -- ** TrafficType
    TrafficType (..),

    -- ** Unit
    Unit (..),

    -- ** UseCaseType
    UseCaseType (..),

    -- ** VoiceRecordingTrack
    VoiceRecordingTrack (..),

    -- ** AgentStatus
    AgentStatus (AgentStatus'),
    newAgentStatus,

    -- ** AgentStatusSummary
    AgentStatusSummary (AgentStatusSummary'),
    newAgentStatusSummary,

    -- ** AnswerMachineDetectionConfig
    AnswerMachineDetectionConfig (AnswerMachineDetectionConfig'),
    newAnswerMachineDetectionConfig,

    -- ** Attribute
    Attribute (Attribute'),
    newAttribute,

    -- ** ChatMessage
    ChatMessage (ChatMessage'),
    newChatMessage,

    -- ** ContactFlow
    ContactFlow (ContactFlow'),
    newContactFlow,

    -- ** ContactFlowSummary
    ContactFlowSummary (ContactFlowSummary'),
    newContactFlowSummary,

    -- ** Credentials
    Credentials (Credentials'),
    newCredentials,

    -- ** CurrentMetric
    CurrentMetric (CurrentMetric'),
    newCurrentMetric,

    -- ** CurrentMetricData
    CurrentMetricData (CurrentMetricData'),
    newCurrentMetricData,

    -- ** CurrentMetricResult
    CurrentMetricResult (CurrentMetricResult'),
    newCurrentMetricResult,

    -- ** Dimensions
    Dimensions (Dimensions'),
    newDimensions,

    -- ** EncryptionConfig
    EncryptionConfig (EncryptionConfig'),
    newEncryptionConfig,

    -- ** Filters
    Filters (Filters'),
    newFilters,

    -- ** HierarchyGroup
    HierarchyGroup (HierarchyGroup'),
    newHierarchyGroup,

    -- ** HierarchyGroupSummary
    HierarchyGroupSummary (HierarchyGroupSummary'),
    newHierarchyGroupSummary,

    -- ** HierarchyLevel
    HierarchyLevel (HierarchyLevel'),
    newHierarchyLevel,

    -- ** HierarchyLevelUpdate
    HierarchyLevelUpdate (HierarchyLevelUpdate'),
    newHierarchyLevelUpdate,

    -- ** HierarchyPath
    HierarchyPath (HierarchyPath'),
    newHierarchyPath,

    -- ** HierarchyStructure
    HierarchyStructure (HierarchyStructure'),
    newHierarchyStructure,

    -- ** HierarchyStructureUpdate
    HierarchyStructureUpdate (HierarchyStructureUpdate'),
    newHierarchyStructureUpdate,

    -- ** HistoricalMetric
    HistoricalMetric (HistoricalMetric'),
    newHistoricalMetric,

    -- ** HistoricalMetricData
    HistoricalMetricData (HistoricalMetricData'),
    newHistoricalMetricData,

    -- ** HistoricalMetricResult
    HistoricalMetricResult (HistoricalMetricResult'),
    newHistoricalMetricResult,

    -- ** HoursOfOperation
    HoursOfOperation (HoursOfOperation'),
    newHoursOfOperation,

    -- ** HoursOfOperationConfig
    HoursOfOperationConfig (HoursOfOperationConfig'),
    newHoursOfOperationConfig,

    -- ** HoursOfOperationSummary
    HoursOfOperationSummary (HoursOfOperationSummary'),
    newHoursOfOperationSummary,

    -- ** HoursOfOperationTimeSlice
    HoursOfOperationTimeSlice (HoursOfOperationTimeSlice'),
    newHoursOfOperationTimeSlice,

    -- ** Instance
    Instance (Instance'),
    newInstance,

    -- ** InstanceStatusReason
    InstanceStatusReason (InstanceStatusReason'),
    newInstanceStatusReason,

    -- ** InstanceStorageConfig
    InstanceStorageConfig (InstanceStorageConfig'),
    newInstanceStorageConfig,

    -- ** InstanceSummary
    InstanceSummary (InstanceSummary'),
    newInstanceSummary,

    -- ** IntegrationAssociationSummary
    IntegrationAssociationSummary (IntegrationAssociationSummary'),
    newIntegrationAssociationSummary,

    -- ** KinesisFirehoseConfig
    KinesisFirehoseConfig (KinesisFirehoseConfig'),
    newKinesisFirehoseConfig,

    -- ** KinesisStreamConfig
    KinesisStreamConfig (KinesisStreamConfig'),
    newKinesisStreamConfig,

    -- ** KinesisVideoStreamConfig
    KinesisVideoStreamConfig (KinesisVideoStreamConfig'),
    newKinesisVideoStreamConfig,

    -- ** LexBot
    LexBot (LexBot'),
    newLexBot,

    -- ** LexBotConfig
    LexBotConfig (LexBotConfig'),
    newLexBotConfig,

    -- ** LexV2Bot
    LexV2Bot (LexV2Bot'),
    newLexV2Bot,

    -- ** MediaConcurrency
    MediaConcurrency (MediaConcurrency'),
    newMediaConcurrency,

    -- ** OutboundCallerConfig
    OutboundCallerConfig (OutboundCallerConfig'),
    newOutboundCallerConfig,

    -- ** ParticipantDetails
    ParticipantDetails (ParticipantDetails'),
    newParticipantDetails,

    -- ** PhoneNumberQuickConnectConfig
    PhoneNumberQuickConnectConfig (PhoneNumberQuickConnectConfig'),
    newPhoneNumberQuickConnectConfig,

    -- ** PhoneNumberSummary
    PhoneNumberSummary (PhoneNumberSummary'),
    newPhoneNumberSummary,

    -- ** PromptSummary
    PromptSummary (PromptSummary'),
    newPromptSummary,

    -- ** Queue
    Queue (Queue'),
    newQueue,

    -- ** QueueQuickConnectConfig
    QueueQuickConnectConfig (QueueQuickConnectConfig'),
    newQueueQuickConnectConfig,

    -- ** QueueReference
    QueueReference (QueueReference'),
    newQueueReference,

    -- ** QueueSummary
    QueueSummary (QueueSummary'),
    newQueueSummary,

    -- ** QuickConnect
    QuickConnect (QuickConnect'),
    newQuickConnect,

    -- ** QuickConnectConfig
    QuickConnectConfig (QuickConnectConfig'),
    newQuickConnectConfig,

    -- ** QuickConnectSummary
    QuickConnectSummary (QuickConnectSummary'),
    newQuickConnectSummary,

    -- ** Reference
    Reference (Reference'),
    newReference,

    -- ** RoutingProfile
    RoutingProfile (RoutingProfile'),
    newRoutingProfile,

    -- ** RoutingProfileQueueConfig
    RoutingProfileQueueConfig (RoutingProfileQueueConfig'),
    newRoutingProfileQueueConfig,

    -- ** RoutingProfileQueueConfigSummary
    RoutingProfileQueueConfigSummary (RoutingProfileQueueConfigSummary'),
    newRoutingProfileQueueConfigSummary,

    -- ** RoutingProfileQueueReference
    RoutingProfileQueueReference (RoutingProfileQueueReference'),
    newRoutingProfileQueueReference,

    -- ** RoutingProfileSummary
    RoutingProfileSummary (RoutingProfileSummary'),
    newRoutingProfileSummary,

    -- ** S3Config
    S3Config (S3Config'),
    newS3Config,

    -- ** SecurityKey
    SecurityKey (SecurityKey'),
    newSecurityKey,

    -- ** SecurityProfileSummary
    SecurityProfileSummary (SecurityProfileSummary'),
    newSecurityProfileSummary,

    -- ** Threshold
    Threshold (Threshold'),
    newThreshold,

    -- ** UseCase
    UseCase (UseCase'),
    newUseCase,

    -- ** User
    User (User'),
    newUser,

    -- ** UserIdentityInfo
    UserIdentityInfo (UserIdentityInfo'),
    newUserIdentityInfo,

    -- ** UserPhoneConfig
    UserPhoneConfig (UserPhoneConfig'),
    newUserPhoneConfig,

    -- ** UserQuickConnectConfig
    UserQuickConnectConfig (UserQuickConnectConfig'),
    newUserQuickConnectConfig,

    -- ** UserSummary
    UserSummary (UserSummary'),
    newUserSummary,

    -- ** VoiceRecordingConfiguration
    VoiceRecordingConfiguration (VoiceRecordingConfiguration'),
    newVoiceRecordingConfiguration,
  )
where

import Amazonka.Connect.AssociateApprovedOrigin
import Amazonka.Connect.AssociateBot
import Amazonka.Connect.AssociateInstanceStorageConfig
import Amazonka.Connect.AssociateLambdaFunction
import Amazonka.Connect.AssociateLexBot
import Amazonka.Connect.AssociateQueueQuickConnects
import Amazonka.Connect.AssociateRoutingProfileQueues
import Amazonka.Connect.AssociateSecurityKey
import Amazonka.Connect.CreateAgentStatus
import Amazonka.Connect.CreateContactFlow
import Amazonka.Connect.CreateHoursOfOperation
import Amazonka.Connect.CreateInstance
import Amazonka.Connect.CreateIntegrationAssociation
import Amazonka.Connect.CreateQueue
import Amazonka.Connect.CreateQuickConnect
import Amazonka.Connect.CreateRoutingProfile
import Amazonka.Connect.CreateUseCase
import Amazonka.Connect.CreateUser
import Amazonka.Connect.CreateUserHierarchyGroup
import Amazonka.Connect.DeleteHoursOfOperation
import Amazonka.Connect.DeleteInstance
import Amazonka.Connect.DeleteIntegrationAssociation
import Amazonka.Connect.DeleteQuickConnect
import Amazonka.Connect.DeleteUseCase
import Amazonka.Connect.DeleteUser
import Amazonka.Connect.DeleteUserHierarchyGroup
import Amazonka.Connect.DescribeAgentStatus
import Amazonka.Connect.DescribeContactFlow
import Amazonka.Connect.DescribeHoursOfOperation
import Amazonka.Connect.DescribeInstance
import Amazonka.Connect.DescribeInstanceAttribute
import Amazonka.Connect.DescribeInstanceStorageConfig
import Amazonka.Connect.DescribeQueue
import Amazonka.Connect.DescribeQuickConnect
import Amazonka.Connect.DescribeRoutingProfile
import Amazonka.Connect.DescribeUser
import Amazonka.Connect.DescribeUserHierarchyGroup
import Amazonka.Connect.DescribeUserHierarchyStructure
import Amazonka.Connect.DisassociateApprovedOrigin
import Amazonka.Connect.DisassociateBot
import Amazonka.Connect.DisassociateInstanceStorageConfig
import Amazonka.Connect.DisassociateLambdaFunction
import Amazonka.Connect.DisassociateLexBot
import Amazonka.Connect.DisassociateQueueQuickConnects
import Amazonka.Connect.DisassociateRoutingProfileQueues
import Amazonka.Connect.DisassociateSecurityKey
import Amazonka.Connect.GetContactAttributes
import Amazonka.Connect.GetCurrentMetricData
import Amazonka.Connect.GetFederationToken
import Amazonka.Connect.GetMetricData
import Amazonka.Connect.Lens
import Amazonka.Connect.ListAgentStatuses
import Amazonka.Connect.ListApprovedOrigins
import Amazonka.Connect.ListBots
import Amazonka.Connect.ListContactFlows
import Amazonka.Connect.ListHoursOfOperations
import Amazonka.Connect.ListInstanceAttributes
import Amazonka.Connect.ListInstanceStorageConfigs
import Amazonka.Connect.ListInstances
import Amazonka.Connect.ListIntegrationAssociations
import Amazonka.Connect.ListLambdaFunctions
import Amazonka.Connect.ListLexBots
import Amazonka.Connect.ListPhoneNumbers
import Amazonka.Connect.ListPrompts
import Amazonka.Connect.ListQueueQuickConnects
import Amazonka.Connect.ListQueues
import Amazonka.Connect.ListQuickConnects
import Amazonka.Connect.ListRoutingProfileQueues
import Amazonka.Connect.ListRoutingProfiles
import Amazonka.Connect.ListSecurityKeys
import Amazonka.Connect.ListSecurityProfiles
import Amazonka.Connect.ListTagsForResource
import Amazonka.Connect.ListUseCases
import Amazonka.Connect.ListUserHierarchyGroups
import Amazonka.Connect.ListUsers
import Amazonka.Connect.ResumeContactRecording
import Amazonka.Connect.StartChatContact
import Amazonka.Connect.StartContactRecording
import Amazonka.Connect.StartOutboundVoiceContact
import Amazonka.Connect.StartTaskContact
import Amazonka.Connect.StopContact
import Amazonka.Connect.StopContactRecording
import Amazonka.Connect.SuspendContactRecording
import Amazonka.Connect.TagResource
import Amazonka.Connect.Types
import Amazonka.Connect.UntagResource
import Amazonka.Connect.UpdateAgentStatus
import Amazonka.Connect.UpdateContactAttributes
import Amazonka.Connect.UpdateContactFlowContent
import Amazonka.Connect.UpdateContactFlowName
import Amazonka.Connect.UpdateHoursOfOperation
import Amazonka.Connect.UpdateInstanceAttribute
import Amazonka.Connect.UpdateInstanceStorageConfig
import Amazonka.Connect.UpdateQueueHoursOfOperation
import Amazonka.Connect.UpdateQueueMaxContacts
import Amazonka.Connect.UpdateQueueName
import Amazonka.Connect.UpdateQueueOutboundCallerConfig
import Amazonka.Connect.UpdateQueueStatus
import Amazonka.Connect.UpdateQuickConnectConfig
import Amazonka.Connect.UpdateQuickConnectName
import Amazonka.Connect.UpdateRoutingProfileConcurrency
import Amazonka.Connect.UpdateRoutingProfileDefaultOutboundQueue
import Amazonka.Connect.UpdateRoutingProfileName
import Amazonka.Connect.UpdateRoutingProfileQueues
import Amazonka.Connect.UpdateUserHierarchy
import Amazonka.Connect.UpdateUserHierarchyGroupName
import Amazonka.Connect.UpdateUserHierarchyStructure
import Amazonka.Connect.UpdateUserIdentityInfo
import Amazonka.Connect.UpdateUserPhoneConfig
import Amazonka.Connect.UpdateUserRoutingProfile
import Amazonka.Connect.UpdateUserSecurityProfiles
import Amazonka.Connect.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'Connect'.

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
