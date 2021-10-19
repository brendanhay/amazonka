{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Network.AWS.Connect
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
module Network.AWS.Connect
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

import Network.AWS.Connect.AssociateApprovedOrigin
import Network.AWS.Connect.AssociateBot
import Network.AWS.Connect.AssociateInstanceStorageConfig
import Network.AWS.Connect.AssociateLambdaFunction
import Network.AWS.Connect.AssociateLexBot
import Network.AWS.Connect.AssociateQueueQuickConnects
import Network.AWS.Connect.AssociateRoutingProfileQueues
import Network.AWS.Connect.AssociateSecurityKey
import Network.AWS.Connect.CreateAgentStatus
import Network.AWS.Connect.CreateContactFlow
import Network.AWS.Connect.CreateHoursOfOperation
import Network.AWS.Connect.CreateInstance
import Network.AWS.Connect.CreateIntegrationAssociation
import Network.AWS.Connect.CreateQueue
import Network.AWS.Connect.CreateQuickConnect
import Network.AWS.Connect.CreateRoutingProfile
import Network.AWS.Connect.CreateUseCase
import Network.AWS.Connect.CreateUser
import Network.AWS.Connect.CreateUserHierarchyGroup
import Network.AWS.Connect.DeleteHoursOfOperation
import Network.AWS.Connect.DeleteInstance
import Network.AWS.Connect.DeleteIntegrationAssociation
import Network.AWS.Connect.DeleteQuickConnect
import Network.AWS.Connect.DeleteUseCase
import Network.AWS.Connect.DeleteUser
import Network.AWS.Connect.DeleteUserHierarchyGroup
import Network.AWS.Connect.DescribeAgentStatus
import Network.AWS.Connect.DescribeContactFlow
import Network.AWS.Connect.DescribeHoursOfOperation
import Network.AWS.Connect.DescribeInstance
import Network.AWS.Connect.DescribeInstanceAttribute
import Network.AWS.Connect.DescribeInstanceStorageConfig
import Network.AWS.Connect.DescribeQueue
import Network.AWS.Connect.DescribeQuickConnect
import Network.AWS.Connect.DescribeRoutingProfile
import Network.AWS.Connect.DescribeUser
import Network.AWS.Connect.DescribeUserHierarchyGroup
import Network.AWS.Connect.DescribeUserHierarchyStructure
import Network.AWS.Connect.DisassociateApprovedOrigin
import Network.AWS.Connect.DisassociateBot
import Network.AWS.Connect.DisassociateInstanceStorageConfig
import Network.AWS.Connect.DisassociateLambdaFunction
import Network.AWS.Connect.DisassociateLexBot
import Network.AWS.Connect.DisassociateQueueQuickConnects
import Network.AWS.Connect.DisassociateRoutingProfileQueues
import Network.AWS.Connect.DisassociateSecurityKey
import Network.AWS.Connect.GetContactAttributes
import Network.AWS.Connect.GetCurrentMetricData
import Network.AWS.Connect.GetFederationToken
import Network.AWS.Connect.GetMetricData
import Network.AWS.Connect.Lens
import Network.AWS.Connect.ListAgentStatuses
import Network.AWS.Connect.ListApprovedOrigins
import Network.AWS.Connect.ListBots
import Network.AWS.Connect.ListContactFlows
import Network.AWS.Connect.ListHoursOfOperations
import Network.AWS.Connect.ListInstanceAttributes
import Network.AWS.Connect.ListInstanceStorageConfigs
import Network.AWS.Connect.ListInstances
import Network.AWS.Connect.ListIntegrationAssociations
import Network.AWS.Connect.ListLambdaFunctions
import Network.AWS.Connect.ListLexBots
import Network.AWS.Connect.ListPhoneNumbers
import Network.AWS.Connect.ListPrompts
import Network.AWS.Connect.ListQueueQuickConnects
import Network.AWS.Connect.ListQueues
import Network.AWS.Connect.ListQuickConnects
import Network.AWS.Connect.ListRoutingProfileQueues
import Network.AWS.Connect.ListRoutingProfiles
import Network.AWS.Connect.ListSecurityKeys
import Network.AWS.Connect.ListSecurityProfiles
import Network.AWS.Connect.ListTagsForResource
import Network.AWS.Connect.ListUseCases
import Network.AWS.Connect.ListUserHierarchyGroups
import Network.AWS.Connect.ListUsers
import Network.AWS.Connect.ResumeContactRecording
import Network.AWS.Connect.StartChatContact
import Network.AWS.Connect.StartContactRecording
import Network.AWS.Connect.StartOutboundVoiceContact
import Network.AWS.Connect.StartTaskContact
import Network.AWS.Connect.StopContact
import Network.AWS.Connect.StopContactRecording
import Network.AWS.Connect.SuspendContactRecording
import Network.AWS.Connect.TagResource
import Network.AWS.Connect.Types
import Network.AWS.Connect.UntagResource
import Network.AWS.Connect.UpdateAgentStatus
import Network.AWS.Connect.UpdateContactAttributes
import Network.AWS.Connect.UpdateContactFlowContent
import Network.AWS.Connect.UpdateContactFlowName
import Network.AWS.Connect.UpdateHoursOfOperation
import Network.AWS.Connect.UpdateInstanceAttribute
import Network.AWS.Connect.UpdateInstanceStorageConfig
import Network.AWS.Connect.UpdateQueueHoursOfOperation
import Network.AWS.Connect.UpdateQueueMaxContacts
import Network.AWS.Connect.UpdateQueueName
import Network.AWS.Connect.UpdateQueueOutboundCallerConfig
import Network.AWS.Connect.UpdateQueueStatus
import Network.AWS.Connect.UpdateQuickConnectConfig
import Network.AWS.Connect.UpdateQuickConnectName
import Network.AWS.Connect.UpdateRoutingProfileConcurrency
import Network.AWS.Connect.UpdateRoutingProfileDefaultOutboundQueue
import Network.AWS.Connect.UpdateRoutingProfileName
import Network.AWS.Connect.UpdateRoutingProfileQueues
import Network.AWS.Connect.UpdateUserHierarchy
import Network.AWS.Connect.UpdateUserHierarchyGroupName
import Network.AWS.Connect.UpdateUserHierarchyStructure
import Network.AWS.Connect.UpdateUserIdentityInfo
import Network.AWS.Connect.UpdateUserPhoneConfig
import Network.AWS.Connect.UpdateUserRoutingProfile
import Network.AWS.Connect.UpdateUserSecurityProfiles
import Network.AWS.Connect.Waiters

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
